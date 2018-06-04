package scala.meta.internal.scalacp

import java.nio.file._
import scala.collection.mutable
import scala.meta.internal.metacp._
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb3.{Language => l}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.semanticdb3.Scala._
import scala.meta.internal.semanticdb3.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb3.Scala.{Names => n}
import scala.meta.internal.semanticdb3.SingletonType.{Tag => st}
import scala.meta.internal.semanticdb3.Type.{Tag => t}
import scala.reflect.NameTransformer
import scala.tools.scalap._
import scala.tools.scalap.scalax.rules.ScalaSigParserError
import scala.tools.scalap.scalax.rules.scalasig._

object Scalacp {
  def parse(classfile: ToplevelClassfile): Option[ToplevelInfos] = {
    val bytes = Files.readAllBytes(classfile.path.toNIO)
    val scalapClassfile = ClassFileParser.parse(ByteCode(bytes))
    ScalaSigParser.parse(scalapClassfile).map { scalaSig =>
      val toplevels = scalaSig.topLevelClasses ++ scalaSig.topLevelObjects
      val others = {
        scalaSig.symbols.toList.flatMap {
          case sym: SymbolInfoSymbol if !toplevels.contains(sym) => Some(sym)
          case _ => None
        }
      }
      val stoplevels = toplevels.flatMap(sinfos)
      val sothers = toplevels.flatMap(spackages).distinct ++ others.flatMap(sinfos)
      ToplevelInfos(classfile, stoplevels, sothers)
    }
  }

  private def spackages(toplevelSym: SymbolInfoSymbol): List[s.SymbolInformation] = {
    val enclosingPackages = ssymbol(toplevelSym).ownerChain.init
    enclosingPackages.map { enclosingPackage =>
      s.SymbolInformation(
        symbol = enclosingPackage,
        language = l.SCALA,
        kind = k.PACKAGE,
        name = enclosingPackage.desc.name)
    }
  }

  private def sinfos(sym: SymbolInfoSymbol): List[s.SymbolInformation] = {
    if (sym.parent.get == NoSymbol) return Nil
    if (sym.isUseless) return Nil
    val ssym = ssymbol(sym)
    if (ssym.contains("$extension")) return Nil
    val sinfo = s.SymbolInformation(
      symbol = ssym,
      language = l.SCALA,
      kind = skind(sym),
      properties = sproperties(sym),
      name = sname(sym),
      tpe = stpe(sym),
      annotations = sanns(sym),
      accessibility = Some(sacc(sym))
    )
    if (sym.isUsefulField && sym.isMutable) {
      List(sinfo) ++ Synthetics.setterInfos(sinfo)
    } else {
      List(sinfo)
    }
  }

  private def ssymbol(sym: Symbol): String = {
    Symbols.Global(sowner(sym), sym.descriptor)
  }

  // NOTE: Cases in the pattern match are ordered
  // similarly to SymbolInformationOps.kind in semanticdb/scalac.
  private def skind(sym: Symbol): s.SymbolInformation.Kind = {
    sym match {
      case sym: MethodSymbol if sym.isMethod =>
        if (sym.isConstructor) k.CONSTRUCTOR
        else if (sym.isMacro) k.MACRO
        else k.METHOD
      case _: ObjectSymbol | _: ClassSymbol if sym.isModule =>
        if (sym.name == "package") k.PACKAGE_OBJECT
        else k.OBJECT
      case sym: MethodSymbol =>
        // NOTE: This is craziness. In scalap, parameters, val and vars
        // are also modelled with method symbols.
        if (sym.isParam) k.PARAMETER
        else {
          // NOTE: More craziness. Useful fields are persisted as methods
          // to accommodate the disparity between the Scalac symbol table
          // and the SemanticDB spec.
          k.METHOD
        }
      case sym: ClassSymbol if !sym.isModule =>
        if (sym.isTrait) k.TRAIT
        else k.CLASS
      case _: TypeSymbol | _: AliasSymbol =>
        if (sym.isParam) k.TYPE_PARAMETER
        else k.TYPE
      case sym: ExternalSymbol =>
        // NOTE: Object and package external symbols
        // are indistinguishable from each other.
        // This means that metacp never sets k.PACKAGE.
        val hasTermName = {
          val idx = sym.entry.index + 1
          if (sym.entry.scalaSig.hasEntry(idx)) {
            val nameEntryType = sym.entry.scalaSig.table(idx)._1
            nameEntryType == 1
          } else {
            false
          }
        }
        val isModuleClass = sym.entry.entryType == 10
        if (hasTermName || isModuleClass) k.OBJECT
        else k.CLASS
      case NoSymbol =>
        k.UNKNOWN_KIND
      case _ =>
        sys.error(s"unsupported symbol $sym")
    }
  }

  private val primaryCtors = mutable.Map[String, Int]()
  private def sproperties(sym: SymbolInfoSymbol): Int = {
    def isAbstractClass = sym.isClass && sym.isAbstract && !sym.isTrait
    def isAbstractMethod = sym.isMethod && sym.isDeferred
    def isAbstractType = sym.isType && !sym.isParam && sym.isDeferred
    var sprops = 0
    def sflip(sprop: s.SymbolInformation.Property) = sprops ^= sprop.value
    if (isAbstractClass || isAbstractMethod || isAbstractType) sflip(p.ABSTRACT)
    if (sym.isFinal || sym.isModule) sflip(p.FINAL)
    if (sym.isSealed) sflip(p.SEALED)
    if (sym.isImplicit) sflip(p.IMPLICIT)
    if (sym.isLazy) sflip(p.LAZY)
    if (sym.isCase && (sym.isClass || sym.isModule)) sflip(p.CASE)
    if (sym.isType && sym.isCovariant) sflip(p.COVARIANT)
    if (sym.isType && sym.isContravariant) sflip(p.CONTRAVARIANT)
    if (sym.isScalacField) {
      if (sym.isMutable) sflip(p.VAR)
      else sflip(p.VAL)
    }
    if (sym.isAccessor) {
      if (sym.isStable) sflip(p.VAL)
      else sflip(p.VAR)
    }
    if (sym.isParam) {
      val methodSym = sym.parent.get.asInstanceOf[SymbolInfoSymbol]
      if ((sproperties(methodSym) & p.PRIMARY.value) != 0) {
        val classMembers = methodSym.parent.get.children
        val accessor = classMembers.find(m => m.isParamAccessor && m.name == sym.name)
        accessor.foreach { accessor =>
          val isStable = if (accessor.isMethod) accessor.isStable else !accessor.isMutable
          if (!isStable) sflip(p.VAR)
          else if (accessor.isMethod) sflip(p.VAL)
          else ()
        }
      }
    }
    if (sym.isConstructor) {
      val primaryIndex = primaryCtors.getOrElseUpdate(sym.path, sym.entry.index)
      if (sym.entry.index == primaryIndex) sflip(p.PRIMARY)
    }
    sprops
  }

  private def sname(sym: Symbol): String = {
    val ssym = ssymbol(sym)
    if (skind(sym) == k.PACKAGE_OBJECT) ssym.owner.desc.name
    else ssym.desc.name
  }

  private def stpe(sym: SymbolInfoSymbol): Option[s.Type] = {
    def loop(tpe: Type): Option[s.Type] = {
      tpe match {
        case ByNameType(tpe) =>
          val stag = t.BY_NAME_TYPE
          val stpe = loop(tpe)
          Some(s.Type(tag = stag, byNameType = Some(s.ByNameType(stpe))))
        case RepeatedType(tpe) =>
          val stag = t.REPEATED_TYPE
          val stpe = loop(tpe)
          Some(s.Type(tag = stag, repeatedType = Some(s.RepeatedType(stpe))))
        case TypeRefType(pre, sym, args) =>
          val stag = t.TYPE_REF
          val spre = if (tpe.hasNontrivialPrefix) loop(pre) else None
          val ssym = ssymbol(sym)
          val sargs = args.flatMap(loop)
          Some(s.Type(tag = stag, typeRef = Some(s.TypeRef(spre, ssym, sargs))))
        case SingleType(pre, sym) =>
          val stag = t.SINGLETON_TYPE
          val stpe = {
            val stag = st.SYMBOL
            val spre = if (tpe.hasNontrivialPrefix) loop(pre) else None
            val ssym = {
              // NOTE: Due to some unclear reason, Scalac sometimes saves
              // (or Scalap sometimes loads) single types that point to
              // companion classes, not module classes (see #1392).
              // We assume that it's a mistake and work around accordingly.
              val raw = ssymbol(sym)
              if (raw.endsWith("#")) raw.stripSuffix("#") + "."
              else raw
            }
            s.SingletonType(stag, spre, ssym, 0, "")
          }
          Some(s.Type(tag = stag, singletonType = Some(stpe)))
        case ThisType(sym) =>
          val stag = t.SINGLETON_TYPE
          val stpe = {
            val stag = st.THIS
            val ssym = ssymbol(sym)
            s.SingletonType(stag, None, ssym, 0, "")
          }
          Some(s.Type(tag = stag, singletonType = Some(stpe)))
        case ConstantType(underlying: Type) =>
          loop(underlying).map { sarg =>
            val stag = t.TYPE_REF
            val ssym = "java.lang.Class#"
            val sargs = sarg :: Nil
            s.Type(tag = stag, typeRef = Some(s.TypeRef(None, ssym, sargs)))
          }
        case ConstantType(const) =>
          val stag = t.SINGLETON_TYPE
          val stpe = {
            def floatBits(x: Float) = java.lang.Float.floatToRawIntBits(x).toLong
            def doubleBits(x: Double) = java.lang.Double.doubleToRawLongBits(x)
            const match {
              case () => s.SingletonType(st.UNIT, None, Symbols.None, 0, "")
              case false => s.SingletonType(st.BOOLEAN, None, Symbols.None, 0, "")
              case true => s.SingletonType(st.BOOLEAN, None, Symbols.None, 1, "")
              case x: Byte => s.SingletonType(st.BYTE, None, Symbols.None, x.toLong, "")
              case x: Short => s.SingletonType(st.SHORT, None, Symbols.None, x.toLong, "")
              case x: Char => s.SingletonType(st.CHAR, None, Symbols.None, x.toLong, "")
              case x: Int => s.SingletonType(st.INT, None, Symbols.None, x.toLong, "")
              case x: Long => s.SingletonType(st.LONG, None, Symbols.None, x, "")
              case x: Float => s.SingletonType(st.FLOAT, None, Symbols.None, floatBits(x), "")
              case x: Double => s.SingletonType(st.DOUBLE, None, Symbols.None, doubleBits(x), "")
              case x: String => s.SingletonType(st.STRING, None, Symbols.None, 0, x)
              case null => s.SingletonType(st.NULL, None, Symbols.None, 0, "")
              case other => sys.error(s"unsupported const $other")
            }
          }
          Some(s.Type(tag = stag, singletonType = Some(stpe)))
        case RefinedType(sym, parents) =>
          val stag = t.STRUCTURAL_TYPE
          val stpe = {
            val sparents = parents.flatMap(loop)
            Some(s.Type(tag = t.WITH_TYPE, withType = Some(s.WithType(sparents))))
          }
          val sdecls = Some(s.Scope(sym.children.map(ssymbol)))
          Some(s.Type(tag = stag, structuralType = Some(s.StructuralType(stpe, sdecls))))
        case AnnotatedType(tpe, anns) =>
          val stag = t.ANNOTATED_TYPE
          // FIXME: https://github.com/scalameta/scalameta/issues/1292
          val sanns = Nil
          val stpe = loop(tpe)
          Some(s.Type(tag = stag, annotatedType = Some(s.AnnotatedType(sanns, stpe))))
        case ExistentialType(tpe, tparams) =>
          val stag = t.EXISTENTIAL_TYPE
          val stpe = loop(tpe)
          val sdecls = Some(s.Scope(tparams.map(ssymbol)))
          Some(s.Type(tag = stag, existentialType = Some(s.ExistentialType(stpe, sdecls))))
        case ClassInfoType(sym, parents) =>
          val stag = t.CLASS_INFO_TYPE
          val sparents = parents.flatMap(loop)
          val sdecls = Some(sym.semanticdbDecls.sscope)
          Some(s.Type(tag = stag, classInfoType = Some(s.ClassInfoType(None, sparents, sdecls))))
        case _: NullaryMethodType | _: MethodType =>
          val stag = t.METHOD_TYPE
          val sparamss = tpe.paramss.map(params => s.Scope(params.map(ssymbol)))
          val sret = loop(tpe.ret)
          Some(s.Type(tag = stag, methodType = Some(s.MethodType(None, sparamss, sret))))
        case TypeBoundsType(lo, hi) =>
          val stag = t.TYPE_TYPE
          val slo = loop(lo)
          val shi = loop(hi)
          Some(s.Type(tag = stag, typeType = Some(s.TypeType(None, slo, shi))))
        case PolyType(tpe, tparams) =>
          val stparams = s.Scope(tparams.map(ssymbol))
          loop(tpe).map { stpe =>
            if (stpe.tag == t.CLASS_INFO_TYPE) {
              stpe.update(_.classInfoType.typeParameters := stparams)
            } else if (stpe.tag == t.METHOD_TYPE) {
              stpe.update(_.methodType.typeParameters := stparams)
            } else if (stpe.tag == t.TYPE_TYPE) {
              stpe.update(_.typeType.typeParameters := stparams)
            } else {
              val stag = t.UNIVERSAL_TYPE
              s.Type(tag = stag, universalType = Some(s.UniversalType(Some(stparams), Some(stpe))))
            }
          }
        case NoType =>
          None
        case NoPrefixType =>
          None
        case other =>
          sys.error(s"unsupported type $other")
      }
    }

    try {
      if (sym.isAlias) {
        def preprocess(info: Type): Type = {
          info match {
            case PolyType(tpe, tparams) => PolyType(preprocess(tpe), tparams)
            case tpe => TypeBoundsType(tpe, tpe)
          }
        }
        loop(preprocess(sym.infoType))
      } else if (sym.isObject) {
        sym.infoType match {
          case TypeRefType(_, moduleClassSym: SymbolInfoSymbol, _) =>
            loop(moduleClassSym.infoType)
          case other =>
            sys.error(s"unsupported type $other")
        }
      } else if (sym.isConstructor) {
        val tpe = loop(sym.infoType)
        tpe.map(_.update(_.methodType.optionalReturnType := None))
      } else if (sym.isScalacField) {
        val stag = t.METHOD_TYPE
        val sparamss = Nil
        val sret = loop(sym.infoType)
        Some(s.Type(tag = stag, methodType = Some(s.MethodType(None, sparamss, sret))))
      } else {
        loop(sym.infoType)
      }
    } catch {
      case ScalaSigParserError("Unexpected failure") =>
        // FIXME: https://github.com/scalameta/scalameta/issues/1494
        None
    }
  }

  def sanns(sym: SymbolInfoSymbol): List[s.Annotation] = {
    // FIXME: https://github.com/scalameta/scalameta/issues/1315
    Nil
  }

  // FIXME: https://github.com/scalameta/scalameta/issues/1325
  def sacc(sym: SymbolInfoSymbol): s.Accessibility = {
    sym.symbolInfo.privateWithin match {
      case Some(privateWithin: Symbol) =>
        val sprivateWithin = ssymbol(privateWithin)
        if (sym.isProtected) s.Accessibility(a.PROTECTED_WITHIN, sprivateWithin)
        else s.Accessibility(a.PRIVATE_WITHIN, sprivateWithin)
      case Some(other) =>
        sys.error(s"unsupported privateWithin: ${other.getClass} $other")
      case None =>
        if (sym.isPrivate && sym.isLocal) s.Accessibility(a.PRIVATE_THIS)
        else if (sym.isPrivate) s.Accessibility(a.PRIVATE)
        else if (sym.isProtected && sym.isLocal) s.Accessibility(a.PROTECTED_THIS)
        else if (sym.isProtected) s.Accessibility(a.PROTECTED)
        else s.Accessibility(a.PUBLIC)
    }
  }

  private def sowner(sym: Symbol): String = {
    if (sym.isRootPackage) Symbols.None
    else if (sym.isEmptyPackage) Symbols.RootPackage
    else if (sym.isToplevelPackage) Symbols.RootPackage
    else {
      sym.parent match {
        case Some(NoSymbol) => ""
        case Some(parent) => ssymbol(parent)
        case None => sys.error(s"unsupported symbol $sym")
      }
    }
  }

  private object ByNameType {
    def unapply(tpe: Type): Option[Type] = {
      tpe match {
        case TypeRefType(_, sym, List(tpe)) if sym.name == "<byname>" => Some(tpe)
        case _ => None
      }
    }
  }

  private object RepeatedType {
    def unapply(tpe: Type): Option[Type] = {
      tpe match {
        case TypeRefType(_, sym, List(tpe)) if sym.name == "<repeated>" => Some(tpe)
        case _ => None
      }
    }
  }

  private implicit class SymbolOps(sym: Symbol) {
    def isRootPackage: Boolean = sym.path == "<root>"
    def isEmptyPackage: Boolean = sym.path == "<empty>"
    def isToplevelPackage: Boolean = sym.parent.isEmpty
    def isModuleClass: Boolean = sym.isInstanceOf[ClassSymbol] && sym.isModule
    def moduleClass: Symbol = sym match {
      case sym: SymbolInfoSymbol if sym.isModule =>
        sym.infoType match {
          case TypeRefType(_, moduleClass, _) => moduleClass
          case _ => NoSymbol
        }
      case _ =>
        NoSymbol
    }
    def isClass: Boolean = sym.isInstanceOf[ClassSymbol] && !sym.isModule
    def isObject: Boolean = sym.isInstanceOf[ObjectSymbol]
    def isType: Boolean = sym.isInstanceOf[TypeSymbol]
    def isAlias: Boolean = sym.isInstanceOf[AliasSymbol]
    def isMacro: Boolean = sym.isMethod && sym.hasFlag(0x00008000)
    def isConstructor: Boolean = sym.isMethod && (sym.name == "<init>" || sym.name == "$init$")
    def isTypeParam = sym.isType && sym.isParam
    def isSyntheticConstructor = sym match {
      case sym: SymbolInfoSymbol =>
        val owner = sym.symbolInfo.owner
        sym.isConstructor && (owner.isModuleClass || owner.isTrait)
      case _ =>
        false
    }
    def isLocalChild: Boolean = sym.name == "<local child>"
    def isExtensionMethod: Boolean = sym.name.contains("$extension")
    def isSyntheticValueClassCompanion: Boolean = {
      sym match {
        case sym: SymbolInfoSymbol =>
          if (sym.isModuleClass) {
            sym.infoType match {
              case ClassInfoType(_, List(TypeRefType(_, anyRef, _))) =>
                sym.isSynthetic && sym.semanticdbDecls.syms.isEmpty
              case _ =>
                false
            }
          } else if (sym.isModule) {
            sym.moduleClass.isSyntheticValueClassCompanion
          } else {
            false
          }
        case _ =>
          false
      }
    }
    def isScalacField: Boolean = {
      val isField = sym.isInstanceOf[MethodSymbol] && !sym.isMethod && !sym.isParam
      val isJavaDefined = sym.isJava
      isField && !isJavaDefined
    }
    def isUselessField: Boolean = {
      val peers = sym.parent.map(_.children.toList).getOrElse(Nil)
      val getter = peers.find(m => m.isAccessor && m.name == sym.name.stripSuffix(" "))
      sym.isScalacField && getter.nonEmpty
    }
    def isUsefulField: Boolean = sym.isScalacField && !sym.isUselessField
    def isSyntheticCaseAccessor: Boolean = {
      sym.isCaseAccessor && sym.name.contains("$")
    }
    def isRefinementDummy: Boolean = {
      sym.name == "<refinement>"
    }
    def isUseless: Boolean = {
      sym.isSyntheticConstructor ||
      sym.isModuleClass ||
      sym.isLocalChild ||
      sym.isExtensionMethod ||
      sym.isSyntheticValueClassCompanion ||
      sym.isUselessField ||
      sym.isSyntheticCaseAccessor ||
      sym.isRefinementDummy
    }
    def isUseful: Boolean = !sym.isUseless
    def descriptor: Descriptor = {
      val name = {
        def loop(name: String): String = {
          val i = name.lastIndexOf("$$")
          if (i > 0) loop(name.substring(i + 2))
          else if (name.endsWith(" ")) loop(name.substring(0, name.length - 1))
          else if (name == "<root>") n.RootPackage
          else if (name == "<empty>") n.EmptyPackage
          else if (name == "<init>") n.Constructor
          else if (name == "<refinement>") "$anon"
          else NameTransformer.decode(name)
        }
        loop(sym.name)
      }
      skind(sym) match {
        case k.LOCAL | k.OBJECT | k.PACKAGE | k.PACKAGE_OBJECT =>
          d.Term(name)
        case k.METHOD | k.CONSTRUCTOR | k.MACRO =>
          val overloads = {
            val peers = sym.parent.get.semanticdbDecls.syms
            peers.filter {
              case peer: ObjectSymbol => peer.name == sym.name
              case peer: MethodSymbol => peer.name == sym.name
              case _ => false
            }
          }
          val disambiguator = {
            if (overloads.lengthCompare(1) == 0) "()"
            else {
              val index = overloads.indexOf(sym)
              if (index <= 0) "()"
              else s"(+${index})"
            }
          }
          d.Method(name, disambiguator)
        case k.TYPE | k.CLASS | k.TRAIT =>
          d.Type(name)
        case k.PARAMETER =>
          d.Parameter(name)
        case k.TYPE_PARAMETER =>
          d.TypeParameter(name)
        case skind =>
          sys.error(s"unsupported kind $skind for symbol $sym")
      }
    }
    def semanticdbDecls: SemanticdbDecls = {
      val decls = sym.children.filter(decl => decl.isUseful && !decl.isTypeParam)
      SemanticdbDecls(decls.toList)
    }
  }

  case class SemanticdbDecls(syms: List[Symbol]) {
    lazy val sscope: s.Scope = {
      val sbuf = List.newBuilder[String]
      syms.foreach { sym =>
        val ssym = ssymbol(sym)
        sbuf += ssym
        if (sym.isUsefulField && sym.isMutable) {
          val setterName = ssym.desc.name + "_="
          val setterSym = Symbols.Global(ssym.owner, d.Method(setterName, "()"))
          sbuf += setterSym
        }
      }
      s.Scope(sbuf.result)
    }
  }

  private implicit class TypeOps(tpe: Type) {
    def prefix: Type = {
      tpe match {
        case TypeRefType(pre, _, _) => pre
        case SingleType(pre, _) => pre
        case _ => NoType
      }
    }
    def symbol: Symbol = {
      tpe match {
        case TypeRefType(_, sym, _) => sym
        case SingleType(_, sym) => sym
        case ThisType(sym) => sym
        case _ => NoSymbol
      }
    }
    // FIXME: https://github.com/scalameta/scalameta/issues/1343
    def hasNontrivialPrefix: Boolean = {
      val kind = skind(tpe.prefix.symbol)
      kind != k.OBJECT && kind != k.PACKAGE && kind != k.PACKAGE_OBJECT
    }
    def paramss: List[List[SymbolInfoSymbol]] = {
      tpe match {
        case NullaryMethodType(_) =>
          Nil
        case MethodType(tpe, params) =>
          val symbolInfoParams = params.map(_.asInstanceOf[SymbolInfoSymbol])
          symbolInfoParams.toList +: tpe.paramss
        case _ =>
          Nil
      }
    }
    def ret: Type = {
      tpe match {
        case NullaryMethodType(tpe) => tpe.ret
        case MethodType(tpe, _) => tpe.ret
        case _ => tpe
      }
    }
  }
}

package scala.meta.cli.metacp

import java.io._
import java.nio.file._
import scala.collection.mutable
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.LiteralType.{Tag => l}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.semanticdb3.Type.{Tag => t}
import scala.reflect.NameTransformer
import scala.tools.scalap.scalax.rules.scalasig._
import scala.util.control.NonFatal
import org.langmeta.io._

object Main {
  def main(args: Array[String]): Unit = {
    Settings.parse(args.toList) match {
      case Some(settings) => sys.exit(process(settings))
      case None => sys.exit(1)
    }
  }

  def process(settings: Settings): Int = {
    var failed = false
    val classpath = Classpath(settings.cps.mkString(File.pathSeparator))
    val fragments = classpath.deep.filter(_.uri.toString.endsWith(".class"))
    fragments.sortBy(_.uri.toString).foreach { fragment =>
      try {
        val bytecode = {
          val is = fragment.uri.toURL.openStream()
          val os = new ByteArrayOutputStream()
          var b = is.read()
          while (b != -1) {
            os.write(b)
            b = is.read()
          }
          is.close()
          ByteCode(os.toByteArray())
        }
        val classfile = ClassFileParser.parse(bytecode)
        ScalaSigParser.parse(classfile) match {
          case Some(scalaSig) =>
            val semanticdbInfos = scalaSig.symbols.flatMap {
              case sym: SymbolInfoSymbol => sinfo(sym)
              case _ => None
            }
            val semanticdbRoot = Paths.get(settings.d, "META-INF", "semanticdb")
            val className = NameTransformer.decode(fragment.name.toString.replace("\\", "/"))
            val semanticdbName = className.replace(".class", ".semanticdb")
            val semanticdbFile = semanticdbRoot.resolve(semanticdbName)
            semanticdbFile.getParent.toFile.mkdirs()
            val semanticdbDocument = s.TextDocument(
              schema = s.Schema.SEMANTICDB3,
              uri = className.replace(".class", ".scala"),
              language = "Scala",
              symbols = semanticdbInfos)
            val semanticdbDocuments = s.TextDocuments(List(semanticdbDocument))
            val semanticdbStream = Files.newOutputStream(semanticdbFile)
            semanticdbDocuments.writeTo(semanticdbStream)
            semanticdbStream.close()
          case None =>
            ()
        }
      } catch {
        case NonFatal(ex) =>
          println(s"error: can't convert $fragment")
          ex.printStackTrace()
          failed = true
      }
    }
    if (failed) 1 else 0
  }

  private def sinfo(sym: SymbolInfoSymbol): Option[s.SymbolInformation] = {
    if (sym.isSynthetic) return None
    if (sym.isModuleClass) return None
    if (sym.name.endsWith(" ")) return None
    if (sym.name.endsWith("_$eq")) return None
    if (sym.name == "<init>" && !sym.isClassConstructor) return None
    Some(s.SymbolInformation(
      symbol = ssymbol(sym),
      kind = skind(sym),
      properties = sproperties(sym),
      name = sname(sym),
      tpe = stpe(sym)))
  }

  private def ssymbol(sym: Symbol): String = {
    val prefix = {
      sym match {
        case sym: SymbolInfoSymbol => ssymbol(sym.parent.get)
        case sym: ExternalSymbol => sym.parent.map(_.path + ".").getOrElse("")
        case _ => sys.error(s"unsupported symbol $sym")
      }
    }
    val encodedName = {
      val name = sname(sym)
      if (name.isEmpty) sys.error(s"unsupported symbol $sym")
      else {
        val (start, parts) = (name.head, name.tail)
        val isStartOk = Character.isJavaIdentifierStart(start)
        val isPartsOk = parts.forall(Character.isJavaIdentifierPart)
        if (isStartOk && isPartsOk) name
        else "`" + name + "`"
      }
    }
    skind(sym) match {
      case k.VAL | k.VAR | k.OBJECT | k.PACKAGE | k.PACKAGE_OBJECT =>
        prefix + encodedName + "."
      case k.DEF | k.PRIMARY_CONSTRUCTOR | k.SECONDARY_CONSTRUCTOR | k.MACRO =>
        val descriptor = {
          // TODO: We're pretty much fatally blocked here.
          // Looks like we'll have to change the specification of SemanticDB.
          def loop(tpe: Type): String = {
            tpe match {
              case PolyType(tpe, _) => loop(tpe)
              case MethodType(_, params) => "(" + params.length + ")"
              case _ => "(0)"
            }
          }
          sym match {
            case sym: SymbolInfoSymbol => loop(sym.infoType)
            case sym => sys.error(s"unsupported symbol $sym")
          }
        }
        prefix + encodedName + descriptor + "."
      case k.TYPE | k.CLASS | k.TRAIT =>
        prefix + encodedName + "#"
      case k.PARAMETER =>
        prefix + "(" + encodedName + ")"
      case k.TYPE_PARAMETER =>
        prefix + "[" + encodedName + "]"
      case skind =>
        sys.error(s"unsupported kind $skind for symbol $sym")
    }
  }

  private val primaryCtors = mutable.Map[String, Int]()
  private def skind(sym: Symbol): s.SymbolInformation.Kind = {
    sym match {
      case sym: MethodSymbol if sym.isAccessor && !sym.isParamAccessor =>
        // TODO: Implement k.VAR.
        // Currently, we filter out methods that end in `_=`,
        // which means that we never get symbols that are mutable.
        if (sym.isMutable) k.VAR
        else k.VAL
      case sym: MethodSymbol if sym.isParamAccessor || sym.isParam =>
        // NOTE: This is some craziness - parameters are modelled as methods.
        // Not just class parameters, but also normal method parameters.
        k.PARAMETER
      case sym: MethodSymbol if sym.name == "<init>" =>
        val primaryIndex = primaryCtors.getOrElseUpdate(sym.path, sym.entry.index)
        if (sym.entry.index == primaryIndex) k.PRIMARY_CONSTRUCTOR
        else k.SECONDARY_CONSTRUCTOR
      case sym: MethodSymbol =>
        // NOTE: More craziness - 0x8000 used to mean DEPRECATED back then.
        // Since then, deprecated became an annotation, and 0x8000 got used by MACRO,
        // but Scalap hasn't been updated.
        if (sym.isDeprecated) k.MACRO
        else k.DEF
      case sym: TypeSymbol if sym.isParam =>
        k.TYPE_PARAMETER
      case _: TypeSymbol | _: AliasSymbol =>
        k.TYPE
      case sym: ClassSymbol if !sym.isModule =>
        if (sym.isTrait) k.TRAIT
        else k.CLASS
      case _: ObjectSymbol | _: ClassSymbol if sym.isModule =>
        if (sym.name == "package") k.PACKAGE_OBJECT
        else k.OBJECT
      case sym: ExternalSymbol =>
        // NOTE: Object and package external symbols
        // are indistinguishable from each other.
        val nameEntryType = sym.entry.scalaSig.table(sym.entry.index + 1)._1
        val hasTermName = nameEntryType == 1
        val isModuleClass = sym.entry.entryType == 10
        if (hasTermName || isModuleClass) k.OBJECT
        else k.CLASS
      case NoSymbol =>
        k.UNKNOWN_KIND
      case _ =>
        sys.error(s"unsupported symbol $sym")
    }
  }

  private def sproperties(sym: SymbolInfoSymbol): Int = {
    def isAbstractClass = sym.isAbstract && !sym.isTrait
    def isAbstractMethod = sym.isMethod && sym.isDeferred
    def isAbstractType = sym.isType && !sym.isParam && sym.isDeferred
    var sproperties = 0
    def sflip(sbit: Int) = sproperties ^= sbit
    if (!sym.isParamAccessor && (sym.isPrivate || sym.isLocal)) sflip(p.PRIVATE.value)
    if (sym.isProtected) sflip(p.PROTECTED.value)
    if (isAbstractClass || isAbstractMethod || isAbstractType) sflip(p.ABSTRACT.value)
    if (sym.isFinal || sym.isModule) sflip(p.FINAL.value)
    if (sym.isSealed) sflip(p.SEALED.value)
    if (sym.isImplicit) sflip(p.IMPLICIT.value)
    if (sym.isLazy) sflip(p.LAZY.value)
    if (sym.isCase) sflip(p.CASE.value)
    if (sym.isType && sym.isCovariant) sflip(p.COVARIANT.value)
    if (sym.isType && sym.isContravariant) sflip(p.CONTRAVARIANT.value)
    if (sym.isAccessor && sym.isParamAccessor && !sym.isMutable) sflip(p.VALPARAM.value)
    if (sym.isAccessor && sym.isParamAccessor && sym.isMutable) sflip(p.VARPARAM.value)
    sproperties
  }

  private def sname(sym: Symbol): String = {
    if (sym.name == "<no symbol>") ""
    else if (sym.name == "<root>") "_root_"
    else if (sym.name == "<empty>") "_empty_"
    else if (sym.name == "<init>") "<init>"
    else NameTransformer.decode(sym.name)
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
          val stag = t.SINGLE_TYPE
          val spre = if (tpe.hasNontrivialPrefix) loop(pre) else None
          val ssym = ssymbol(sym)
          Some(s.Type(tag = stag, singleType = Some(s.SingleType(spre, ssym))))
        case ThisType(sym) =>
          val stag = t.THIS_TYPE
          val ssym = ssymbol(sym)
          Some(s.Type(tag = stag, thisType = Some(s.ThisType(ssym))))
        // TODO: Not supported by scalap.
        // case SuperType(pre, gmix) =>
        //   val stag = t.SUPER_TYPE
        //   val spre = loop(pre)
        //   val smix = loop(gmix)
        //   Some(s.Type(tag = stag, superType = Some(s.SuperType(spre, smix))))
        case ConstantType(const) =>
          def floatBits(x: Float) = java.lang.Float.floatToRawIntBits(x).toLong
          def doubleBits(x: Double) = java.lang.Double.doubleToRawLongBits(x)
          val stag = t.LITERAL_TYPE
          val sconst = const match {
            case () => s.LiteralType(l.UNIT, 0, "")
            case false => s.LiteralType(l.BOOLEAN, 0, "")
            case true => s.LiteralType(l.BOOLEAN, 1, "")
            case x: Byte => s.LiteralType(l.BYTE, x.toLong, "")
            case x: Short => s.LiteralType(l.SHORT, x.toLong, "")
            case x: Char => s.LiteralType(l.CHAR, x.toLong, "")
            case x: Int => s.LiteralType(l.INT, x.toLong, "")
            case x: Long => s.LiteralType(l.LONG, x, "")
            case x: Float => s.LiteralType(l.FLOAT, floatBits(x), "")
            case x: Double => s.LiteralType(l.DOUBLE, doubleBits(x), "")
            case x: String => s.LiteralType(l.STRING, 0, x)
            case null => s.LiteralType(l.NULL, 0, "")
            case other => sys.error(s"unsupported const $other")
          }
          Some(s.Type(tag = stag, literalType = Some(sconst)))
        case RefinedType(sym, parents) =>
          val stag = t.COMPOUND_TYPE
          val sparents = parents.flatMap(loop)
          val sdecls = sym.children.map(ssymbol)
          Some(s.Type(tag = stag, compoundType = Some(s.CompoundType(sparents, sdecls))))
        case AnnotatedType(tpe, raw) =>
          val stag = t.ANNOTATED_TYPE
          val stpe = loop(tpe)
          val sanns = {
            // TODO: Not supported by scalap.
            // anns.reverse.flatMap(gann => loop(gann.atp))
            Nil
          }
          Some(s.Type(tag = stag, annotatedType = Some(s.AnnotatedType(stpe, sanns))))
        case ExistentialType(tpe, quants) =>
          val stag = t.EXISTENTIAL_TYPE
          val stpe = loop(tpe)
          val ssyms = quants.map(ssymbol)
          Some(s.Type(tag = stag, existentialType = Some(s.ExistentialType(stpe, ssyms))))
        case ClassInfoType(sym, parents) =>
          val stag = t.CLASS_INFO_TYPE
          val sparents = parents.flatMap(loop)
          val sdecls = sym.children.map(ssymbol)
          Some(s.Type(tag = stag, classInfoType = Some(s.ClassInfoType(Nil, sparents, sdecls))))
        case NullaryMethodType(tpe) =>
          val stag = t.METHOD_TYPE
          val stpe = loop(tpe)
          Some(s.Type(tag = stag, methodType = Some(s.MethodType(Nil, Nil, stpe))))
        case tpe: MethodType =>
          def flatten(tpe: Type): (List[List[Symbol]], Type) = {
            tpe match {
              case MethodType(tpe, head) =>
                val (tail, ret) = flatten(tpe)
                (head.toList +: tail, ret)
              case other =>
                (Nil, other)
            }
          }
          val (paramss, ret) = flatten(tpe)
          val stag = t.METHOD_TYPE
          val sparamss = paramss.map { params =>
            val sparams = params.map(ssymbol)
            s.MethodType.ParameterList(sparams)
          }
          val sret = loop(ret)
          Some(s.Type(tag = stag, methodType = Some(s.MethodType(Nil, sparamss, sret))))
        case TypeBoundsType(lo, hi) =>
          val stag = t.TYPE_TYPE
          val slo = loop(lo)
          val shi = loop(hi)
          Some(s.Type(tag = stag, typeType = Some(s.TypeType(Nil, slo, shi))))
        case PolyType(tpe, tparams) =>
          val stparams = tparams.map(ssymbol)
          val stpe = loop(tpe)
          stpe.map { stpe =>
            if (stpe.tag == t.CLASS_INFO_TYPE) {
              stpe.update(_.classInfoType.typeParameters := stparams)
            } else if (stpe.tag == t.METHOD_TYPE) {
              stpe.update(_.methodType.typeParameters := stparams)
            } else if (stpe.tag == t.TYPE_TYPE) {
              stpe.update(_.typeType.typeParameters := stparams)
            } else {
              val stag = t.TYPE_LAMBDA
              s.Type(tag = stag, typeLambda = Some(s.TypeLambda(stparams, Some(stpe))))
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
    loop(sym.infoType)
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

  private implicit class SymbolOps(sym: SymbolInfoSymbol) {
    def isModuleClass = sym.isInstanceOf[ClassSymbol] && sym.isModule
    def isType = sym.isInstanceOf[TypeSymbol]
    def isClassConstructor = {
      sym.parent match {
        case Some(parent: ClassSymbol) if !parent.isTrait && !parent.isModule =>
          sym.name == "<init>"
        case _ =>
          false
      }
    }
  }

  private implicit class TypeOps(tpe: Type) {
    def prefix = {
      tpe match {
        case TypeRefType(pre, _, _) => pre
        case SingleType(pre, _) => pre
        case _ => NoType
      }
    }
    def symbol = {
      tpe match {
        case TypeRefType(_, sym, _) => sym
        case SingleType(_, sym) => sym
        case ThisType(sym) => sym
        case _ => NoSymbol
      }
    }
    def hasNontrivialPrefix: Boolean = {
      val kind = skind(tpe.prefix.symbol)
      kind != k.OBJECT && kind != k.PACKAGE && kind != k.PACKAGE_OBJECT
    }
  }
}

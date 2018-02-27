package scala.meta.internal.metacp

import java.io._
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util
import java.util.Comparator
import scala.collection.mutable
import scala.meta.internal.javacp._
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.semanticdb3.SingletonType.{Tag => st}
import scala.meta.internal.semanticdb3.Type.{Tag => t}
import scala.reflect.NameTransformer
import scala.tools.asm.ClassReader
import scala.tools.asm.ClassVisitor
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.ClassNode
import scala.tools.scalap.ByteArrayReader
import scala.tools.scalap
import scala.tools.scalap.Classfile
import scala.tools.scalap.JavaWriter
import scala.tools.scalap.scalax.rules.ScalaSigParserError
import scala.tools.scalap.scalax.rules.scalasig._
import scala.util.control.NonFatal
import org.langmeta.internal.io._
import org.langmeta.io._

class Main(settings: Settings, out: PrintStream, err: PrintStream) {
  def process(): Int = {
    val metaInfRoot = AbsolutePath(settings.d).resolve("META-INF")
    val semanticdbRoot = metaInfRoot.resolve("semanticdb")
    var failed = false
    def fail(file: Path, ex: Throwable): Unit = {
      out.println(s"error: can't convert $file")
      ex.printStackTrace(out)
      failed = true
    }
    val packageIndex = mutable.Map[String, mutable.Set[String]]()
    packageIndex("_root_.") = mutable.Set[String]()
    packageIndex("_empty_.") = mutable.Set[String]()
    val toplevelIndex = mutable.Map[String, String]()
    def indexToplevel(info: s.SymbolInformation, uri: String): Unit = {
      toplevelIndex(info.symbol) = uri
      if (info.symbol.stripSuffix("#").contains("#")) return
      val ownerChain = info.owner.split("\\.")
      ownerChain.scanLeft("") { (ancestorSym, name) =>
        val sym = ancestorSym + name + "."
        val decls = packageIndex.getOrElse(sym, mutable.Set[String]())
        packageIndex(sym) = decls
        if (ancestorSym != "") packageIndex(ancestorSym) += sym
        sym
      }
      packageIndex(info.owner) += info.symbol
    }
    val isVisited = mutable.Set.empty[Path]
    val classpath = Classpath(settings.cps.mkString(File.pathSeparator))
    classpath.visit { root =>
      new FileVisitor[Path] {
        // Convert a .class file to a .class.semanticdb file with symbols only.
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          FileVisitResult.CONTINUE
        }

        def handleFile(file: Path): FileVisitResult = {
          if (PathIO.extension(file) != "class") return FileVisitResult.CONTINUE
          try {
            val relpath = AbsolutePath(file).toRelative(root).toString
            val bytes = Files.readAllBytes(file)
            val bytecode = ByteCode(bytes)
            val classfile = ClassFileParser.parse(bytecode)

            val isScalaFile =
              classfile.attribute("ScalaSig").isDefined ||
                classfile.attribute("Scala").isDefined
            val language = if (isScalaFile) "Scala" else "Java"
            val semanticdbInfos: Option[Seq[s.SymbolInformation]] = if (isScalaFile) {
              ScalaSigParser.parse(classfile).map { scalaSig =>
                val toplevelSyms = scalaSig.topLevelClasses ++ scalaSig.topLevelObjects
                val toplevelInfos = toplevelSyms.map { toplevelSym =>
                  s.SymbolInformation(symbol = ssymbol(toplevelSym), owner = sowner(toplevelSym))
                }
                toplevelInfos.foreach(indexToplevel(_, relpath + ".semanticdb"))
                scalaSigPackages(scalaSig) ++ scalaSigSymbols(scalaSig)
              }
            } else {
              val infos = Javacp.sinfos(root.toNIO, file, isVisited)
              if (infos.nonEmpty) indexToplevel(infos.last, relpath + ".semanticdb")
              Some(infos)
            }
            semanticdbInfos.foreach { infos =>
              val className = NameTransformer.decode(PathIO.toUnix(relpath))
              val semanticdbRelpath = relpath + ".semanticdb"
              val semanticdbAbspath = semanticdbRoot.resolve(semanticdbRelpath)
              val semanticdbDocument = s.TextDocument(
                schema = s.Schema.SEMANTICDB3,
                uri = className,
                language = Some(s.Language(language)),
                symbols = infos)
              val semanticdbDocuments = s.TextDocuments(List(semanticdbDocument))
              FileIO.write(semanticdbAbspath, semanticdbDocuments)
            }
          } catch {
            case NonFatal(ex) =>
              fail(file, ex)
          }
          FileVisitResult.CONTINUE
        }

        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
          // Sort files in reverse order so that we process class files in the following order:
          // 1. Outer.class
          // 2. Outer$Inner.class
          // This ordering is necessary so that we enter type parameters in
          // Outer.class before processing Outer$Inner.class
          val files = Files.list(dir).sorted(Comparator.reverseOrder())
          import scala.collection.JavaConverters._
          files
            .iterator()
            .asScala
            .filter { f =>
              !isVisited(f) &&
              Files.isRegularFile(f)
            }
            .foreach(handleFile)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, e: IOException): FileVisitResult = {
          if (e != null) fail(dir, e)
          FileVisitResult.CONTINUE
        }

        override def visitFileFailed(file: Path, e: IOException): FileVisitResult = {
          fail(file, e)
          FileVisitResult.CONTINUE
        }
      }
    }
    locally {
      def synthesizeBuiltin(name: String): s.SymbolInformation = {
        val parent = s.TypeRef(None, "_root_.scala.Any#", Nil)
        val tpe = s.ClassInfoType(Nil, List(s.Type(tag = t.TYPE_REF, typeRef = Some(parent))), Nil)
        s.SymbolInformation(
          symbol = "_root_.scala." + name + "#",
          language = Some(s.Language("Scala")),
          kind = k.CLASS,
          name = name,
          tpe = Some(s.Type(tag = t.CLASS_INFO_TYPE, classInfoType = Some(tpe))),
          owner = "_root_.scala."
        )
      }
      def synthesizeAny(): List[s.SymbolInformation] = {
        val decls = {
          // TODO: Implement me.
          // lazy val Any_==       = enterNewMethod(AnyClass, nme.EQ, AnyTpe :: Nil, BooleanTpe, FINAL)
          // lazy val Any_!=       = enterNewMethod(AnyClass, nme.NE, AnyTpe :: Nil, BooleanTpe, FINAL)
          // lazy val Any_equals   = enterNewMethod(AnyClass, nme.equals_, AnyTpe :: Nil, BooleanTpe)
          // lazy val Any_hashCode = enterNewMethod(AnyClass, nme.hashCode_, Nil, IntTpe)
          // lazy val Any_toString = enterNewMethod(AnyClass, nme.toString_, Nil, StringTpe)
          // lazy val Any_##       = enterNewMethod(AnyClass, nme.HASHHASH, Nil, IntTpe, FINAL)
          // lazy val Any_getClass     = enterNewMethod(AnyClass, nme.getClass_, Nil, getMemberMethod(ObjectClass, nme.getClass_).tpe.resultType, DEFERRED)
          // lazy val Any_isInstanceOf = newT1NullaryMethod(AnyClass, nme.isInstanceOf_, FINAL)(_ => BooleanTpe)
          // lazy val Any_asInstanceOf = newT1NullaryMethod(AnyClass, nme.asInstanceOf_, FINAL)(_.typeConstructor)
          List[s.SymbolInformation]()
        }
        val any0 = synthesizeBuiltin("Any")
        val any1 = any0.update(_.tpe.classInfoType.parents := Nil)
        val any = any1.update(_.tpe.classInfoType.declarations := decls.map(_.symbol))
        any +: decls
      }
      def synthesizeAnyVal(): List[s.SymbolInformation] = {
        List(synthesizeBuiltin("AnyVal"))
      }
      def synthesizeAnyRef(): List[s.SymbolInformation] = {
        List(synthesizeBuiltin("AnyRef"))
      }
      def synthesizeNothing(): List[s.SymbolInformation] = {
        List(synthesizeBuiltin("Nothing"))
      }
      val map = Map(
        "Any" -> synthesizeAny(),
        "AnyVal" -> synthesizeAnyVal(),
        "AnyRef" -> synthesizeAnyRef(),
        "Nothing" -> synthesizeNothing()
      )
      map.foreach {
        case (name, infos) =>
          val relpath = "scala/" + name + ".class"
          val className = NameTransformer.decode(PathIO.toUnix(relpath))
          val semanticdbRelpath = relpath + ".semanticdb"
          val semanticdbAbspath = semanticdbRoot.resolve(semanticdbRelpath)
          val semanticdbDocument = s.TextDocument(
            schema = s.Schema.SEMANTICDB3,
            uri = relpath,
            language = Some(s.Language("Scala")),
            symbols = infos)
          val semanticdbDocuments = s.TextDocuments(List(semanticdbDocument))
          FileIO.write(semanticdbAbspath, semanticdbDocuments)
          indexToplevel(infos.head, semanticdbRelpath)
      }
    }
    val index = {
      val packages = packageIndex.map(kv => s.PackageEntry(symbol = kv._1, members = kv._2.toList))
      val toplevels = toplevelIndex.map(kv => s.ToplevelEntry(symbol = kv._1, uri = kv._2))
      s.Index(packages = packages.toList, toplevels = toplevels.toList)
    }
    val indexAbspath = metaInfRoot.resolve("semanticdb.semanticidx")
    FileIO.write(indexAbspath, index)
    if (failed) 1 else 0
  }

  private def scalaSigPackages(scalaSig: ScalaSig): List[s.SymbolInformation] = {
    val topLevelSymbols = scalaSig.topLevelClasses ++ scalaSig.topLevelObjects
    val directPackagePaths = topLevelSymbols.map { topLevelSymbol =>
      val topLevelPath = topLevelSymbol.symbolInfo.owner.path.replace("<empty>", "_empty_")
      if (topLevelPath.startsWith("_empty_")) topLevelPath
      else "_root_." + topLevelPath
    }
    val transitivePackagePaths = directPackagePaths.flatMap { directPackagePath =>
      val directPackageSteps = directPackagePath.split("\\.")
      directPackageSteps
        .scanLeft("") { (path, step) =>
          if (path.nonEmpty) path + "." + step
          else step
        }
        .tail
    }.distinct
    transitivePackagePaths.map { transitivePackagePath =>
      val (owner, name) = {
        transitivePackagePath.split("\\.").toList match {
          case List(name) if name.nonEmpty => ("", name)
          case ownerSteps :+ name if name.nonEmpty => (ownerSteps.mkString(".") + ".", name)
          case _ => sys.error(s"unsupported top-level symbols: $topLevelSymbols")
        }
      }
      s.SymbolInformation(
        symbol = transitivePackagePath + ".",
        language = Some(s.Language("Scala")),
        kind = k.PACKAGE,
        name = name,
        owner = owner)
    }
  }

  private def scalaSigSymbols(scalaSig: ScalaSig): List[s.SymbolInformation] = {
    scalaSig.symbols.toList.flatMap {
      case sym: SymbolInfoSymbol => sinfo(sym)
      case _ => None
    }
  }

  private def sinfo(sym: SymbolInfoSymbol): Option[s.SymbolInformation] = {
    if (sym.parent.get == NoSymbol) return None
    if (sym.isModuleClass) return None
    if (sym.name == "<init>" && !sym.isClassConstructor) return None
    Some(
      s.SymbolInformation(
        symbol = ssymbol(sym),
        language = Some(s.Language("Scala")),
        kind = skind(sym),
        properties = sproperties(sym),
        name = sname(sym),
        tpe = stpe(sym),
        annotations = sanns(sym),
        accessibility = Some(sacc(sym)),
        owner = sowner(sym)
      ))
  }

  private def ssymbol(sym: Symbol): String = {
    val prefix = {
      sym match {
        case sym: SymbolInfoSymbol =>
          ssymbol(sym.parent.get)
        case sym: ExternalSymbol =>
          if (sym.name == "<root>") ""
          else if (sym.name == "<empty>") ""
          else {
            val path = sym.parent.map(_.path + ".").getOrElse("")
            if (path.startsWith("<empty>")) "_empty_" + path.stripPrefix("<empty>")
            else "_root_." + path
          }
        case _ =>
          sys.error(s"unsupported symbol $sym")
      }
    }
    val encodedName = sname(sym).encoded
    skind(sym) match {
      case k.VAL | k.VAR | k.OBJECT | k.PACKAGE | k.PACKAGE_OBJECT =>
        prefix + encodedName + "."
      case k.DEF | k.GETTER | k.SETTER | k.PRIMARY_CONSTRUCTOR | k.SECONDARY_CONSTRUCTOR |
          k.MACRO =>
        prefix + encodedName + sym.disambiguator + "."
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

  // NOTE: Cases in the pattern match are ordered
  // similarly to DenotationOps.kindFlags in semanticdb/scalac.
  private val primaryCtors = mutable.Map[String, Int]()
  private def skind(sym: Symbol): s.SymbolInformation.Kind = {
    sym match {
      case sym: MethodSymbol if sym.isMethod =>
        if (sym.name == "<init>") {
          val primaryIndex = primaryCtors.getOrElseUpdate(sym.path, sym.entry.index)
          if (sym.entry.index == primaryIndex) k.PRIMARY_CONSTRUCTOR
          else k.SECONDARY_CONSTRUCTOR
        } else {
          if (sym.isAccessor && sym.name.endsWith("_$eq")) k.SETTER
          else if (sym.isAccessor) k.GETTER
          else if (sym.hasFlag(0x00008000)) k.MACRO
          else k.DEF
        }
      case _: ObjectSymbol | _: ClassSymbol if sym.isModule =>
        if (sym.name == "package") k.PACKAGE_OBJECT
        else k.OBJECT
      case sym: MethodSymbol =>
        // NOTE: This is craziness. In scalap, parameters, val and vars
        // are also modelled with method symbols.
        if (sym.isParam) k.PARAMETER
        else if (sym.isMutable) k.VAR
        else k.VAL
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

  private def sproperties(sym: SymbolInfoSymbol): Int = {
    def isAbstractClass = sym.isClass && sym.isAbstract && !sym.isTrait
    def isAbstractMethod = sym.isMethod && sym.isDeferred
    def isAbstractType = sym.isType && !sym.isParam && sym.isDeferred
    var sproperties = 0
    def sflip(sbit: Int) = sproperties ^= sbit
    if (isAbstractClass || isAbstractMethod || isAbstractType) sflip(p.ABSTRACT.value)
    if (sym.isFinal || sym.isModule) sflip(p.FINAL.value)
    if (sym.isSealed) sflip(p.SEALED.value)
    if (sym.isImplicit) sflip(p.IMPLICIT.value)
    if (sym.isLazy) sflip(p.LAZY.value)
    if (sym.isCase) sflip(p.CASE.value)
    if (sym.isType && sym.isCovariant) sflip(p.COVARIANT.value)
    if (sym.isType && sym.isContravariant) sflip(p.CONTRAVARIANT.value)
    if (sym.isParam && skind(sym.parent.get) == k.PRIMARY_CONSTRUCTOR) {
      val members = sym.parent.get.parent.get.children
      val getter = members.find(m => skind(m) == k.GETTER && m.name == sym.name)
      val setter = members.find(m => skind(m) == k.SETTER && m.name == sym.name + "_$eq")
      if (setter.nonEmpty) sflip(p.VARPARAM.value)
      else if (getter.nonEmpty) sflip(p.VALPARAM.value)
      else ()
    }
    sproperties
  }

  private def sname(sym: Symbol): String = {
    def loop(name: String): String = {
      val i = name.lastIndexOf("$$")
      if (i > 0) loop(name.substring(i + 2))
      else if (name.endsWith(" ")) loop(name.substring(0, name.length - 1))
      else if (name == "<no symbol>") ""
      else if (name == "<root>") "_root_"
      else if (name == "<empty>") "_empty_"
      else if (name == "<init>") "<init>"
      else NameTransformer.decode(name)
    }
    loop(sym.name)
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
            val ssym = ssymbol(sym)
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
            val ssym = "_root_.java.lang.Class#"
            val sargs = sarg :: Nil
            // TODO: Implement me.
            s.Type(tag = stag, typeRef = Some(s.TypeRef(None, ssym, sargs)))
          }
        case ConstantType(const) =>
          val stag = t.SINGLETON_TYPE
          val stpe = {
            def floatBits(x: Float) = java.lang.Float.floatToRawIntBits(x).toLong
            def doubleBits(x: Double) = java.lang.Double.doubleToRawLongBits(x)
            const match {
              case () => s.SingletonType(st.UNIT, None, "", 0, "")
              case false => s.SingletonType(st.BOOLEAN, None, "", 0, "")
              case true => s.SingletonType(st.BOOLEAN, None, "", 1, "")
              case x: Byte => s.SingletonType(st.BYTE, None, "", x.toLong, "")
              case x: Short => s.SingletonType(st.SHORT, None, "", x.toLong, "")
              case x: Char => s.SingletonType(st.CHAR, None, "", x.toLong, "")
              case x: Int => s.SingletonType(st.INT, None, "", x.toLong, "")
              case x: Long => s.SingletonType(st.LONG, None, "", x, "")
              case x: Float => s.SingletonType(st.FLOAT, None, "", floatBits(x), "")
              case x: Double => s.SingletonType(st.DOUBLE, None, "", doubleBits(x), "")
              case x: String => s.SingletonType(st.STRING, None, "", 0, x)
              case null => s.SingletonType(st.NULL, None, "", 0, "")
              case other => sys.error(s"unsupported const $other")
            }
          }
          Some(s.Type(tag = stag, singletonType = Some(stpe)))
        case RefinedType(sym, parents) =>
          val stag = t.STRUCTURAL_TYPE
          val sparents = parents.flatMap(loop)
          val sdecls = sym.children.map(ssymbol)
          Some(s.Type(tag = stag, structuralType = Some(s.StructuralType(Nil, sparents, sdecls))))
        case AnnotatedType(tpe, anns) =>
          val stag = t.ANNOTATED_TYPE
          // TODO: Not supported by scalap.
          val sanns = Nil
          val stpe = loop(tpe)
          Some(s.Type(tag = stag, annotatedType = Some(s.AnnotatedType(sanns, stpe))))
        case ExistentialType(tpe, tparams) =>
          val stag = t.EXISTENTIAL_TYPE
          val stparams = tparams.map(ssymbol)
          val stpe = loop(tpe)
          Some(s.Type(tag = stag, existentialType = Some(s.ExistentialType(stparams, stpe))))
        case ClassInfoType(sym, parents) =>
          val stag = t.CLASS_INFO_TYPE
          val sparents = parents.flatMap(loop)
          val sdecls = sym.children.map(ssymbol)
          Some(s.Type(tag = stag, classInfoType = Some(s.ClassInfoType(Nil, sparents, sdecls))))
        case _: NullaryMethodType | _: MethodType =>
          val stag = t.METHOD_TYPE
          val sparamss = tpe.paramss.map { params =>
            val sparams = params.map(ssymbol)
            s.MethodType.ParameterList(sparams)
          }
          val sret = loop(tpe.ret)
          Some(s.Type(tag = stag, methodType = Some(s.MethodType(Nil, sparamss, sret))))
        case TypeBoundsType(lo, hi) =>
          val stag = t.TYPE_TYPE
          val slo = loop(lo)
          val shi = loop(hi)
          Some(s.Type(tag = stag, typeType = Some(s.TypeType(Nil, slo, shi))))
        case PolyType(tpe, tparams) =>
          val stparams = tparams.map(ssymbol)
          loop(tpe).map { stpe =>
            if (stpe.tag == t.STRUCTURAL_TYPE) {
              stpe.update(_.structuralType.typeParameters := stparams)
            } else if (stpe.tag == t.CLASS_INFO_TYPE) {
              stpe.update(_.classInfoType.typeParameters := stparams)
            } else if (stpe.tag == t.METHOD_TYPE) {
              stpe.update(_.methodType.typeParameters := stparams)
            } else if (stpe.tag == t.TYPE_TYPE) {
              stpe.update(_.typeType.typeParameters := stparams)
            } else {
              val stag = t.UNIVERSAL_TYPE
              s.Type(tag = stag, universalType = Some(s.UniversalType(stparams, Some(stpe))))
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
      } else {
        loop(sym.infoType)
      }
    } catch {
      case ScalaSigParserError("Unexpected failure") =>
        // TODO: See https://github.com/scalameta/scalameta/issues/1283
        // when this can happen.
        None
    }
  }

  def sanns(sym: SymbolInfoSymbol): List[s.Annotation] = {
    // TODO: Not supported by scalap.
    Nil
  }

  // TODO: I'm not completely happy with the implementation of this method.
  // See https://github.com/scalameta/scalameta/issues/1325 for details.
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

  def sowner(sym: SymbolInfoSymbol): String = {
    if (sym.symbolInfo.owner == NoSymbol) return ""
    ssymbol(sym.symbolInfo.owner)
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

  private implicit class NameOps(name: String) {
    def encoded: String = {
      if (name.isEmpty) {
        sys.error(s"unsupported name")
      } else {
        val (start, parts) = (name.head, name.tail)
        val isStartOk = Character.isJavaIdentifierStart(start)
        val isPartsOk = parts.forall(Character.isJavaIdentifierPart)
        if (isStartOk && isPartsOk) name
        else "`" + name + "`"
      }
    }
  }

  private implicit class SymbolOps(sym: Symbol) {
    def isModuleClass: Boolean = sym.isInstanceOf[ClassSymbol] && sym.isModule
    def isClass: Boolean = sym.isInstanceOf[ClassSymbol] && !sym.isModule
    def isObject: Boolean = sym.isInstanceOf[ObjectSymbol]
    def isType: Boolean = sym.isInstanceOf[TypeSymbol]
    def isAlias: Boolean = sym.isInstanceOf[AliasSymbol]
    def isClassConstructor: Boolean = {
      sym.parent match {
        case Some(parent: ClassSymbol) if !parent.isTrait && !parent.isModule =>
          sym.name == "<init>"
        case _ =>
          false
      }
    }
    def descriptor: String = {
      try {
        sym match {
          case sym: SymbolInfoSymbol => sym.infoType.descriptor
          case sym => sys.error(s"unsupported symbol $sym")
        }
      } catch {
        case ScalaSigParserError("Unexpected failure") =>
          // TODO: MethodSymbol(javaEnum, owner=414, flags=8400202, info=486 ,None)
          // aka "accessor stable method final javaEnum".
          // Looks like the same problem as the one in
          // https://github.com/scalameta/scalameta/issues/1283.
          // It seems that Scalap doesn't support all the signatures that
          // Scalac can emit.
          "<?>"
      }
    }
    def disambiguator: String = {
      val kindred = sym.parent.get.children.filter(other => skind(other) == skind(sym))
      val siblings = kindred.filter(_.name == sym.name)
      val synonyms = siblings.filter(_.descriptor == sym.descriptor)
      val suffix = {
        if (synonyms.length == 1) ""
        else "+" + (synonyms.indexOf(sym) + 1)
      }
      "(" + descriptor + suffix + ")"
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
    // TODO: Implement me.
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
    def descriptor: String = {
      def unsupported = sys.error(s"unsupported type $tpe")
      def paramDescriptors = tpe.paramss.flatten.map(_.infoType.descriptor)
      tpe match {
        case ByNameType(tpe) => "=>" + tpe.descriptor
        case RepeatedType(tpe) => tpe.descriptor + "*"
        case TypeRefType(_, sym, _) => sname(sym).encoded
        case SingleType(_, _) => ".type"
        case ThisType(_) => ".type"
        case ConstantType(_: Type) => "Class"
        case ConstantType(_) => ".type"
        case RefinedType(_, _) => "{}"
        case AnnotatedType(tpe, _) => tpe.descriptor
        case ExistentialType(tpe, _) => tpe.descriptor
        case ClassInfoType(_, _) => unsupported
        case _: NullaryMethodType | _: MethodType => paramDescriptors.mkString(",")
        case TypeBoundsType(_, _) => unsupported
        case PolyType(tpe, _) => tpe.descriptor
        case NoType => "<?>" // TODO: fixme
        case NoPrefixType => unsupported
        case other => unsupported
      }
    }
  }
}

package scala.meta.internal.metacp

import java.io._
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.Comparator
import scala.collection.mutable
import scala.meta.internal.javacp._
import scala.meta.internal.scalacp._
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
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
                  s.SymbolInformation(
                    symbol = Scalacp.ssymbol(toplevelSym),
                    owner = Scalacp.sowner(toplevelSym))
                }
                toplevelInfos.foreach(indexToplevel(_, relpath + ".semanticdb"))
                Scalacp.sinfos(scalaSig)
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

}

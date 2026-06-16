package scala.meta.tests.metacp

import scala.meta.cli._
import scala.meta.internal.io._
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.{semanticdb => s}
import scala.meta.io._
import scala.meta.metacp.{Settings => MetacpSettings}

import java.nio.file.{Files, Path}

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{Directory, PlainDirectory}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings => CompilerSettings}

import munit.FunSuite

// Regression test for https://github.com/scalameta/scalameta/issues/1494:
// metacp (the classpath -> SemanticDB path, i.e. scalacp) must emit a type for
// `final val` constants. A literal constant becomes a ConstantType, and a `final val`
// aliasing a Java enum constant (the originally reported `DAYS = TimeUnit.DAYS` shape)
// becomes a SingleType to that enum symbol.
//
// This is intentionally assertion-based and compiles its own fresh symbols, so it is
// independent of the `types.Test.Literal` example-corpus golden checks (MetacpExpect).
class FinalValConstantSuite extends FunSuite {

  private def compileToClassDir(code: String): Path = {
    val out = Files.createTempDirectory("metacp-1494-classes")
    out.toFile.deleteOnExit()
    val classpath = sys.props("sbt.paths.tests.test.classes")
    assert(classpath != null, "classpath not set. broken build?")
    val settings = new CompilerSettings(msg => fail(s"compiler settings error: $msg"))
    settings.classpath.value = classpath
    settings.outputDirs.setSingleOutput(new PlainDirectory(new Directory(out.toFile)))
    val reporter = new StoreReporter()
    val global = new Global(settings, reporter)
    val run = new global.Run
    run.compileSources(List(new BatchSourceFile("issue1494.scala", code)))
    val errors = reporter.infos.filter(_.severity == reporter.ERROR).map(_.msg)
    assert(errors.isEmpty, s"compilation failed:\n${errors.mkString("\n")}")
    out
  }

  private def metacpSymbols(classDir: Path): Map[String, s.SymbolInformation] = {
    val target = Files.createTempDirectory("metacp-1494-out")
    target.toFile.deleteOnExit()
    val settings = MetacpSettings().withOut(AbsolutePath(target))
      .withClasspath(Classpath(AbsolutePath(classDir))).withUsejavacp(true)
      .withScalaLibrarySynthetics(false)
    val reporter = Reporter().withSilentOut().withErr(System.err)
    assert(Metacp.process(settings, reporter).isSuccess, "metacp failed")
    (for {
      file <- FileIO.listAllFilesRecursively(AbsolutePath(target)).iterator
      if PathIO.extension(file.toNIO) == "semanticdb"
      doc <- TextDocuments.parseFrom(file.readAllBytes).documents
      sym <- doc.symbols
    } yield sym.symbol -> sym).toMap
  }

  // The result type of a val accessor's signature (a nullary MethodSignature in scalacp).
  private def signatureType(info: s.SymbolInformation): s.Type = info.signature match {
    case s.MethodSignature(_, _, ret, _) => ret
    case s.ValueSignature(tpe) => tpe
    case other => fail(s"unexpected signature for ${info.symbol}: $other")
  }

  private lazy val symtab: Map[String, s.SymbolInformation] = metacpSymbols(compileToClassDir(
    """|package issue1494
       |object Constants {
       |  final val litInt = 1
       |  final val litString = "s"
       |  final val javaEnum = java.nio.file.LinkOption.NOFOLLOW_LINKS
       |  val plainInt = 1
       |}
       |""".stripMargin,
  ))

  test("metacp emits ConstantType for literal final vals (#1494)") {
    val int = symtab("issue1494/Constants.litInt.")
    assert(signatureType(int).isInstanceOf[s.ConstantType], s"got ${int.signature}")
    val string = symtab("issue1494/Constants.litString.")
    assert(signatureType(string).isInstanceOf[s.ConstantType], s"got ${string.signature}")
  }

  test("metacp emits SingleType for a final val aliasing a Java enum constant (#1494)") {
    val javaEnum = symtab("issue1494/Constants.javaEnum.")
    val tpe = signatureType(javaEnum)
    assert(tpe.isInstanceOf[s.SingleType], s"got ${javaEnum.signature}")
    assert(
      tpe.asInstanceOf[s.SingleType].symbol.contains("NOFOLLOW_LINKS"),
      s"got ${tpe.asInstanceOf[s.SingleType].symbol}",
    )
  }

  test("a non-final val is widened, not a ConstantType (#1494 negative case)") {
    val plain = symtab("issue1494/Constants.plainInt.")
    val tpe = signatureType(plain)
    assert(!tpe.isInstanceOf[s.ConstantType], s"got ${plain.signature}")
    assert(tpe.isInstanceOf[s.TypeRef], s"expected scala.Int TypeRef, got $tpe")
  }
}

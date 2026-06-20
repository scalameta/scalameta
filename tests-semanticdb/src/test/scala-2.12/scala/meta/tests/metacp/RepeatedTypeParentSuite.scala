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

// Regression test for https://github.com/scalameta/scalameta/issues/1497:
// a repeated type (`T*`) is only valid as a parameter signature in SemanticDB, but scalac leaves
// it un-widened in the synthetic `AbstractFunction1[T*, C]` parent of a case class companion.
// metacp (the classpath -> SemanticDB path, i.e. scalacp) must widen the nested repeated to
// `scala.Seq[T]` there, while keeping the genuine `apply` parameter a RepeatedType.
//
// This is intentionally assertion-based and compiles its own fresh symbols, so it is
// independent of the `types.Test.C.RepeatedType` example-corpus golden checks (MetacpExpect).
class RepeatedTypeParentSuite extends FunSuite {

  private def compileToClassDir(code: String): Path = {
    val out = Files.createTempDirectory("metacp-1497-classes")
    out.toFile.deleteOnExit()
    val classpath = sys.props("sbt.paths.tests.test.classes")
    assert(classpath != null, "classpath not set. broken build?")
    val settings = new CompilerSettings(msg => fail(s"compiler settings error: $msg"))
    settings.classpath.value = classpath
    settings.outputDirs.setSingleOutput(new PlainDirectory(new Directory(out.toFile)))
    val reporter = new StoreReporter()
    val global = new Global(settings, reporter)
    val run = new global.Run
    run.compileSources(List(new BatchSourceFile("issue1497.scala", code)))
    val errors = reporter.infos.filter(_.severity == reporter.ERROR).map(_.msg)
    assert(errors.isEmpty, s"compilation failed:\n${errors.mkString("\n")}")
    out
  }

  private def metacpSymbols(classDir: Path): Map[String, s.SymbolInformation] = {
    val target = Files.createTempDirectory("metacp-1497-out")
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

  private lazy val symtab: Map[String, s.SymbolInformation] = metacpSymbols(compileToClassDir(
    """|package issue1497
       |case class C(s: String*)
       |""".stripMargin,
  ))

  // The single type argument of the companion's synthetic `AbstractFunction1[_, C]` parent.
  private def abstractFunctionParamType: s.Type = {
    val companion = symtab("issue1497/C.")
    val parents = companion.signature match {
      case s.ClassSignature(_, ps, _, _) => ps
      case other => fail(s"expected a ClassSignature for the companion, got $other")
    }
    val parent = parents
      .collectFirst { case tr: s.TypeRef if tr.symbol.contains("AbstractFunction1") => tr }
      .getOrElse(fail(s"no AbstractFunction1 parent in $parents"))
    parent.typeArguments.headOption
      .getOrElse(fail(s"AbstractFunction1 parent has no type args: $parent"))
  }

  test("metacp widens a repeated type to Seq in the AbstractFunction1 parent (#1497)") {
    val arg = abstractFunctionParamType
    assert(!arg.isInstanceOf[s.RepeatedType], s"repeated type leaked into the parent: $arg")
    arg match {
      case s.TypeRef(_, sym, _) => assertEquals(sym, "scala/package.Seq#", s"got $arg")
      case other => fail(s"expected a Seq TypeRef, got $other")
    }
  }

  test("metacp keeps the apply parameter a repeated type (#1497 positive case)") {
    val param = symtab("issue1497/C.apply().(s)")
    val tpe = param.signature match {
      case s.ValueSignature(t) => t
      case other => fail(s"expected a ValueSignature for the param, got $other")
    }
    assert(tpe.isInstanceOf[s.RepeatedType], s"the apply parameter should stay repeated, got $tpe")
  }
}

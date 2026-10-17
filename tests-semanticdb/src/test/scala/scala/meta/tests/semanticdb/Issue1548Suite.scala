package scala.meta.tests.semanticdb

import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.tests.metacp.Library

import java.nio.file.Files

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

import munit.FunSuite

// https://github.com/scalameta/scalameta/issues/1548
class Issue1548Suite extends FunSuite {

  private val symtab = {
    val out = Files.createTempDirectory("issue1548_")
    out.toFile.deleteOnExit()
    val settings = new Settings(msg => fail(msg))
    settings.classpath.value = Library.scalaLibrary.classpath().syntax
    settings.outputDirs.setSingleOutput(out.toString)
    val reporter = new StoreReporter()
    val g = new Global(settings, reporter)
    val code =
      """|package p1 {
         |  trait X
         |  object X {
         |    class A
         |    class B extends X
         |  }
         |}
         |package p2 {
         |  sealed trait X
         |  object X {
         |    class A
         |    class B extends X
         |  }
         |}
         |""".stripMargin
    new g.Run().compileSources(List(new BatchSourceFile("Issue1548.scala", code)))
    assertEquals(reporter.infos.toList.map(_.msg), Nil)
    GlobalSymbolTable(Library.scalaLibrary.classpath() ++ Classpath(AbsolutePath(out)))
  }

  private def declarations(symbol: String): List[String] =
    symtab.info(symbol).map(_.signature) match {
      case Some(sig: s.ClassSignature) => sig.declarations.toList.flatMap(_.symlinks)
      case sig => fail(s"expected ClassSignature for $symbol, obtained $sig")
    }

  test("object of a trait")(assertEquals(declarations("p1/X."), List("p1/X.A#", "p1/X.B#")))

  test("object of a sealed trait")(assertEquals(declarations("p2/X."), List("p2/X.A#", "p2/X.B#")))

}

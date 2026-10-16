package scala.meta.tests.symtab

import scala.meta.internal.classpath.MissingSymbolException
import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.semanticdb.{Print, SymbolInformation}
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.metap.Format
import scala.meta.tests.BuildInfo
import scala.meta.tests.metacp.Library

import java.nio.file.{Files, Paths}

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

import munit.FunSuite

// https://github.com/scalameta/scalameta/issues/1675
class Issue1675Suite extends FunSuite {

  /* Compile the classes, then delete `p/gone/` and `p/Lost.class` as a shaded jar would.
   * Copy `ClassSuffix.class` without its nested classes. */
  private val classdir = {
    val out = Files.createTempDirectory("issue1675_")
    out.toFile.deleteOnExit()
    val settings = new Settings(msg => fail(msg))
    settings.classpath.value = sys.props("sbt.paths.tests.test.classes")
    settings.outputDirs.setSingleOutput(out.toString)
    val reporter = new StoreReporter()
    val g = new Global(settings, reporter)
    val code =
      """|package p {
         |  package gone {
         |    class M
         |    class Ann extends scala.annotation.StaticAnnotation
         |  }
         |  class Lost
         |  class U {
         |    def m: gone.M = ???
         |    def n: Int = 1
         |  }
         |  class L {
         |    def m: Lost = ???
         |  }
         |  class V extends gone.M
         |  class W {
         |    @gone.Ann def a: Int = 1
         |  }
         |}
         |""".stripMargin
    new g.Run().compileSources(List(new BatchSourceFile("Issue1675.scala", code)))
    assertEquals(reporter.infos.toList.map(_.msg), Nil)
    List("p/gone/M.class", "p/gone/Ann.class", "p/gone", "p/Lost.class")
      .foreach(x => Files.delete(out.resolve(x)))
    val javaClass = "com/javacp/ClassSuffix.class"
    Files.createDirectories(out.resolve(javaClass).getParent)
    Files.copy(Paths.get(BuildInfo.databaseClasspath).resolve(javaClass), out.resolve(javaClass))
    out
  }

  private val symtab = GlobalSymbolTable(
    Library.scalaLibrary.classpath() ++ Classpath(AbsolutePath(classdir)),
    includeJdk = true,
  )

  private val printerSymtab = new PrinterSymtab {
    def info(symbol: String): Option[SymbolInformation] = symtab.info(symbol)
  }

  private def load(symbol: String): Option[String] = symtab.info(symbol)
    .map(Print.info(Format.Compact, _, printerSymtab))

  private def missing(symbol: String): String = intercept[MissingSymbolException](load(symbol))
    .getMessage

  test("method returning a class from a missing package") {
    assertEquals(load("p/U#"), Some("p/U# => class U extends AnyRef { +3 decls }"))
    assertEquals(load("p/U#m()."), Some("p/U#m(). => method m"))
    assertEquals(load("p/U#n()."), Some("p/U#n(). => method n: Int"))
  }

  test("method returning a missing class")(
    assertEquals(load("p/L#m()."), Some("p/L#m(). => method m")),
  )

  test("class extending a class from a missing package")(
    assertEquals(load("p/V#"), Some("p/V# => class V")),
  )

  test("method annotated with a class from a missing package")(
    assertEquals(load("p/W#a()."), Some("p/W#a(). => method a: Int")),
  )

  test("java class without its nested classfiles")(assertEquals(
    missing("com/javacp/ClassSuffix#suffix."),
    "missing symbol: com/javacp/ClassSuffix#Inner#",
  ))

}

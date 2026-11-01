package scala.meta.tests.semanticdb

import scala.meta.internal.io.FileIO
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath
import scala.meta.metap.Format

import java.io.File
import java.nio.file.Files

import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

class DiagnosticSuite extends SemanticdbSuite {

  private lazy val ops: SemanticdbOps { val global: DiagnosticSuite.this.g.type } =
    new SemanticdbOps {
      val global: DiagnosticSuite.this.g.type = DiagnosticSuite.this.g
    }
  import ops._

  // `ops` caches the input of a file by its path, so each test uses its own path
  private def toMtrees(name: String, code: String) =
    new g.CompilationUnit(g.newSourceFile(code, name)).toMtrees

  test("toMtrees: a source that parses")(
    assertEquals(toMtrees("parses.scala", "class A")(6).syntax, "A"),
  )

  test("toMtrees: a source that does not parse")(
    assertEquals(toMtrees("noparse.scala", "class {")(0), null),
  )

  // a non-exhaustive match, which scalac reports after the typer document
  private def lateWarning(name: String, value: String) =
    s"""|object $name {
        |  val x = $value
        |  def f(o: Option[Int]) = o.map(_ + 1) match { case Some(y) => y }
        |}
        |""".stripMargin

  // `src` is the sourceroot
  // scalac 2.13 compiles the literal `08`, but metac does not parse it
  private val sources = Map(
    "src/Parses.scala" -> lateWarning("Parses", "8"),
    "other/Outside.scala" -> lateWarning("Outside", "8"),
  ) ++ (if (isScala213) Map("src/NoParse.scala" -> lateWarning("NoParse", "08")) else Map.empty)

  private val reporter = new StoreReporter()

  // the uri and the diagnostics of each document of a run that compiles `sources` with the plugin
  private lazy val documents: List[String] = {
    val root = Files.createTempDirectory("diagnostic_")
    val targetroot = Files.createDirectories(root.resolve("target"))
    val paths = sources.map { case (name, code) =>
      val path = root.resolve(name)
      Files.createDirectories(path.getParent)
      Files.write(path, code.getBytes("UTF-8")).toString
    }
    val settings = new Settings(msg => fail(msg))
    settings.classpath.value = sys.props("sbt.paths.tests.test.classes")
    settings.outputDirs.setSingleOutput(targetroot.toString)
    val plugin = sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar")
    settings.processArgumentString(
      s"-Yrangepos -Xplugin:$plugin -Xplugin-require:semanticdb -P:semanticdb:failures:warning " +
        s"-P:semanticdb:sourceroot:${root.resolve("src")}",
    )
    val compiler = new Global(settings, reporter)
    new compiler.Run().compile(paths.toList.sorted)
    assertEquals(reporter.infos.filter(_.severity == reporter.ERROR).map(_.msg).toList, Nil)
    FileIO.listAllFilesRecursively(AbsolutePath(targetroot)).iterator
      .filter(_.toNIO.toString.endsWith(".semanticdb")).toList.sortBy(_.toNIO.toString)
      .flatMap(file => TextDocuments.parseFrom(file.readAllBytes).documents).map { doc =>
        val printed = s.Print.document(Format.Detailed, doc).linesIterator
          .dropWhile(_ != "Diagnostics:").drop(1).takeWhile(_.nonEmpty)
        (doc.uri +: printed.toList).mkString("\n")
      }
  }

  test("a late warning in a file that parses") {
    val late =
      """|Parses.scala
         |[2:31..2:38) [warning] match may not be exhaustive.
         |It would fail on the following input: None""".stripMargin
    assertEquals(documents.filter(_.contains("Parses")), List("Parses.scala", late))
  }

  test("a late warning in a file that does not parse") {
    assume(isScala213, "scalac 2.12 does not compile the literal `08`")
    val late =
      """|NoParse.scala
         |[2:26..2:38) [warning] match may not be exhaustive.
         |It would fail on the following input: None""".stripMargin
    assertEquals(documents.filter(_.contains("NoParse")), List(late))
  }

  test("a late warning in a file outside the sourceroot") {
    assertEquals(documents.filter(_.contains("Outside")), Nil)
    // the file of each "failed to generate semanticdb for <path>:" of the run
    val failed = reporter.infos.toList.map(_.msg.linesIterator.next())
      .filter(_.startsWith("failed to generate semanticdb"))
      .map(msg => msg.substring(msg.lastIndexOf(File.separatorChar) + 1).stripSuffix(":"))
    assertEquals(failed, if (isScala213) List("NoParse.scala") else Nil)
  }

}

package scala.meta.tests.semanticdb

import scala.meta.internal.io.FileIO
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.semanticdb.scalac.{SemanticdbOps, SemanticdbReporter}
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath
import scala.meta.metap.Format

import java.io.File
import java.nio.file.Files

import scala.reflect.internal.util.NoPosition
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

  private val sources = {
    // `src` is the sourceroot
    val res = Map.newBuilder[String, String]
    res += "src/Parses.scala" -> lateWarning("Parses", "8")
    res += "other/Outside.scala" -> lateWarning("Outside", "8")
    if (isScala213) { // scalac 2.13 compiles the literal `08`, but metac does not parse it
      res += "src/NoParse.scala" -> lateWarning("NoParse", "08")
      res += "src/Broken.scala" ->
        s"""|import scala.collection.mutable
            |${lateWarning("Broken", "08")}""".stripMargin
    }
    res.result()
  }

  private val settings = new Settings(msg => fail(msg))
  private val reporter = new StoreReporter(settings)

  /** the targetroot of a run that compiles `files` with the plugin and `options` */
  private def compile(files: Map[String, String], store: StoreReporter, options: String) = {
    val root = Files.createTempDirectory("diagnostic_")
    val targetroot = Files.createDirectories(root.resolve("target"))
    val paths = files.map { case (name, code) =>
      val path = root.resolve(name)
      Files.createDirectories(path.getParent)
      Files.write(path, code.getBytes("UTF-8")).toString
    }
    store.settings.classpath.value = sys.props("sbt.paths.tests.test.classes")
    store.settings.outputDirs.setSingleOutput(targetroot.toString)
    val plugin = sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar")
    store.settings.processArgumentString(
      s"-Yrangepos -Xplugin:$plugin -Xplugin-require:semanticdb $options " +
        s"-P:semanticdb:sourceroot:${root.resolve("src")}",
    )
    val compiler = new Global(store.settings, store)
    new compiler.Run().compile(paths.toList.sorted)
    targetroot
  }

  /** each document's uri and diagnostics, from compiling `sources` with the plugin */
  private lazy val documents: List[String] = {
    val targetroot = compile(sources, reporter, s"-P:semanticdb:failures:warning $warnUnusedImports")
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

  test("a typer warning and a late warning in a file that does not parse") {
    assume(isScala213, "scalac 2.12 does not compile the literal `08`")
    // scalac 2.13.15 puts the warning at the whole import
    val start = if (ScalaVersion.atLeast213_16) "0:24" else "0:0"
    val both =
      s"""|Broken.scala
          |[$start..0:31) [warning] Unused import
          |[3:26..3:38) [warning] match may not be exhaustive.
          |It would fail on the following input: None""".stripMargin
    assertEquals(documents.filter(_.contains("Broken")), List(both))
  }

  test("a late warning in a file outside the sourceroot") {
    assertEquals(documents.filter(_.contains("Outside")), Nil)
    // the file of each "failed to generate semanticdb for <path>:" of the run
    val failed = reporter.infos.toList.map(_.msg.linesIterator.next())
      .filter(_.startsWith("failed to generate semanticdb"))
      .map(msg => msg.substring(msg.lastIndexOf(File.separatorChar) + 1).stripSuffix(":"))
    assertEquals(failed, if (isScala213) List("Broken.scala", "NoParse.scala") else Nil)
  }

  test("failures:info: a targetroot that is a file") {
    val store = new StoreReporter(new Settings(msg => fail(msg)))
    val file = Files.createTempFile("targetroot_", "")
    compile(
      Map("src/Ok.scala" -> "object Ok"),
      store,
      s"-P:semanticdb:failures:info -P:semanticdb:targetroot:$file",
    )
    // the position, the severity and the first line of each message, with the file name only
    val infos = store.infos.toList.map { info =>
      val line = info.msg.linesIterator.next()
      (info.pos, info.severity, line.substring(line.lastIndexOf(File.separatorChar) + 1))
    }
    assertEquals(infos, List((NoPosition, store.INFO, "Ok.scala:")))
    assertEquals((store.errorCount, store.warningCount), (0, 0))
  }

  private class Wrapped extends StoreReporter(new Settings) {
    val calls = List.newBuilder[String]
    override def flush(): Unit = calls += "flush"
    override def finish(): Unit = calls += "finish"
  }

  test("SemanticdbReporter: a warning and an error, then reset") {
    val wrapped = new Wrapped
    val reporter = new SemanticdbReporter(wrapped)
    reporter.warning(NoPosition, "careful")
    reporter.error(NoPosition, "boom")
    reporter.reset()
    assertEquals((reporter.errorCount, reporter.warningCount), (0, 0))
    assertEquals((wrapped.errorCount, wrapped.warningCount, wrapped.infos.size), (0, 0, 0))
  }

  test("SemanticdbReporter: cancel, then reset") {
    val wrapped = new Wrapped
    val reporter = new SemanticdbReporter(wrapped)
    reporter.cancelled = true
    assertEquals(wrapped.cancelled, true)
    reporter.reset()
    assertEquals(wrapped.cancelled, false)
  }

  test("SemanticdbReporter: flush and finish") {
    val wrapped = new Wrapped
    val reporter = new SemanticdbReporter(wrapped)
    reporter.flush()
    reporter.finish()
    assertEquals(wrapped.calls.result(), List("flush", "finish"))
  }

}

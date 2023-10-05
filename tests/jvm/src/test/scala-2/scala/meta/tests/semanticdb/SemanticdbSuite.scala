package scala.meta.tests
package semanticdb

import org.scalameta.internal.ScalaCompat.EOL
import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.semanticdb.scalac.CommandLineParser
import scala.meta.internal.semanticdb.scalac._
import scala.meta.internal.{semanticdb => s}
import scala.meta.io._
import scala.meta.tests.SkipWindows

import java.io.File
import java.io.PrintWriter

import scala.reflect.io._
import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter
import scala.{meta => m}

import munit._

abstract class SemanticdbSuite extends FunSuite {
  self =>
  private def test(code: String)(fn: => Unit): Unit = {
    var name = code.trim.replace(EOL, " ")
    if (name.length > 50) name = name.take(50) + "..."
    super.test(name.tag(SkipWindows))(fn)
  }

  def yMacroAnnotations: String =
    if (isScala213) "-Ymacro-annotations"
    else {
      val paradiseJar = sys.props("sbt.paths.tests.test.options").split(" ")
        .find(_.contains("paradise")).orNull
      if (paradiseJar == null) fail("Missing scalamacros/paradise from scalacOptions")
      paradiseJar + " -Xplugin-require:macro-paradise-plugin"
    }

  def isScala213: Boolean = scala.util.Properties.versionNumberString.startsWith("2.13")

  lazy val g: Global = {
    def fail(msg: String) = sys.error(s"SemanticdbSuite initialization failed: $msg")
    val classpath = sys.props("sbt.paths.tests.test.classes")
    if (classpath == null) fail("classpath not set. broken build?")
    val pluginjar = sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar")
    if (pluginjar == null) fail("pluginjar not set. broken build?")
    val warnUnusedImports = if (isScala213) "-Wunused:imports" else "-Ywarn-unused-import"
    val options = s"-Yrangepos $warnUnusedImports -cp " + classpath + " -Xplugin:" + pluginjar +
      " -Xplugin-require:semanticdb " + yMacroAnnotations
    val args = CommandLineParser.tokenize(options)
    val emptySettings = new Settings(error => fail(s"couldn't apply settings because $error"))
    val reporter = new StoreReporter()
    val command = new CompilerCommand(args, emptySettings)
    val settings = command.settings
    val g = new Global(settings, reporter)
    val run = new g.Run
    g.phase = run.parserPhase
    g.globalPhase = run.parserPhase
    g
  }
  private lazy val databaseOps: SemanticdbOps { val global: self.g.type } = new SemanticdbOps {
    val global: self.g.type = self.g
    config = config
      .copy(failures = FailureMode.Error, text = BinaryMode.On, synthetics = BinaryMode.On)
    config = customizeConfig(config)
  }
  def customizeConfig(config: SemanticdbConfig): SemanticdbConfig = config
  import databaseOps._

  private def assertNoReporterErrors(): Unit = {
    val reporter = g.reporter.asInstanceOf[StoreReporter]
    val errors = reporter.infos.filter(_.severity == reporter.ERROR)
    val diagnostics = errors.map { error =>
      s"""|<input>:${error.pos.line}:${error.pos.column}: error ${error.msg}
          |${error.pos.lineContent}
          |${error.pos.lineCaret}""".stripMargin
    }
    if (errors.nonEmpty) fail(diagnostics.mkString("\n"))
  }

  private def computeDatabaseFromSnippet(code: String): s.TextDocument = {
    val javaFile = File.createTempFile("paradise", ".scala")
    val writer = new PrintWriter(javaFile)
    try writer.write(code)
    finally writer.close()
    config = config.copy(sourceroot = AbsolutePath(javaFile.getParentFile))
    val run = new g.Run
    val abstractFile = AbstractFile.getFile(javaFile)
    val sourceFile = g.getSourceFile(abstractFile)
    val unit = new g.CompilationUnit(sourceFile)
    run.compileUnits(List(unit), run.phaseNamed("terminal"))

    g.phase = run.parserPhase
    g.globalPhase = run.parserPhase
    val reporter = new StoreReporter()
    g.reporter = reporter
    unit.body = g.newUnitParser(unit).parse()
    assertNoReporterErrors()

    val packageobjectsPhase = run.phaseNamed("packageobjects")
    val basePhases = List(run.parserPhase, run.namerPhase, packageobjectsPhase)
    val phases = if (unit.isJava) basePhases else basePhases :+ run.typerPhase // can't run typer for Java units in 2.11
    reporter.reset()

    /* note(@tgodzik)
     * Since 2.13.7 it seems we need to force loading the scala package objects
     * probably due to https://github.com/scala/scala/pull/9661, but it doesn't
     * break the normal compilation, just here where we force a compiler run until
     * the type.
     */
    g.openPackageModule(g.definitions.ScalaPackage)
    phases.foreach { phase =>
      g.phase = phase
      g.globalPhase = phase
      phase.asInstanceOf[g.GlobalPhase].apply(unit)
      assertNoReporterErrors()
    }
    g.phase = run.phaseNamed("patmat")
    g.globalPhase = run.phaseNamed("patmat")

    unit.toTextDocument
  }

  protected def computePayloadFromSnippet(code: String): String = {
    val document = computeDatabaseFromSnippet(code)
    val format = scala.meta.metap.Format.Detailed
    s.Print.document(format, document)
  }

  protected def computeSectionFromPayload(payload: String, sectionName: String): String = {
    val sectionLine = sectionName + ":"
    payload.linesIterator.dropWhile(_ != sectionLine).drop(1).takeWhile(_ != "")
      .mkString("", EOL, EOL)
  }

  protected def computeDatabaseSectionFromSnippet(code: String, sectionName: String): String =
    computeSectionFromPayload(computePayloadFromSnippet(code), sectionName)

  def checkSection(code: String, expected: String, section: String)(implicit
      loc: munit.Location
  ): Unit = checkSection(code, code, expected, section)
  def checkSection(name: TestOptions, code: String, expected: String, section: String)(implicit
      loc: munit.Location
  ): Unit = test(name) {
    val obtained = computeDatabaseSectionFromSnippet(code, section)
    assertNoDiff(obtained, expected)
  }

  def occurrences(code: String, expected: String)(implicit loc: munit.Location): Unit =
    checkSection(code, expected, "Occurrences")

  def diagnostics(code: String, expected: String)(implicit loc: munit.Location): Unit =
    throw new UnsupportedOperationException(
      "SemanticdbSuite is not able to test against diagnostics. Use ExpectSuite instead."
    )

  def symbols(code: String, expected: String)(implicit loc: munit.Location): Unit =
    checkSection(code, expected, "Symbols")

  def synthetics(code: String, expected: String)(implicit loc: munit.Location): Unit =
    checkSection(code, expected, "Synthetics")

  private def computeDatabaseAndOccurrencesFromMarkup(
      markup: String
  ): (s.TextDocument, List[String]) = {
    val chevrons = "<<(.*?)>>".r
    val ps0 = chevrons.findAllIn(markup).matchData.map(m => (m.start, m.end)).toList
    val ps = ps0.zipWithIndex.map { case ((s, e), i) => (s - 4 * i, e - 4 * i - 4) }
    val code = chevrons.replaceAllIn(markup, "$1")
    val database = computeDatabaseFromSnippet(code)
    val unit = g.currentRun.units.toList.last
    val source = unit.toSource
    val symbols = ps.map { case (s, e) =>
      val symbols = source.collect {
        case name: m.Name if name.pos.start == s && name.pos.end == e =>
          database.occurrences.find(_.range.contains(name.pos.toRange)).map(_.symbol)
      }
      val chevron = "<<" + code.substring(s, e) + ">>"
      symbols match {
        case Nil => sys.error(chevron + " does not wrap a name")
        case List(Some(symbol)) => symbol
        case _ => sys.error("fatal error processing " + chevron)
      }
    }
    (database, symbols)
  }

  trait OverloadHack1; implicit object OverloadHack1 extends OverloadHack1
  trait OverloadHack2; implicit object OverloadHack2 extends OverloadHack2
  trait OverloadHack3; implicit object OverloadHack3 extends OverloadHack3
  trait OverloadHack4; implicit object OverloadHack4 extends OverloadHack4
  trait OverloadHack5; implicit object OverloadHack5 extends OverloadHack5

  def targeted(markup: String, fn: s.TextDocument => Unit)(implicit hack: OverloadHack1): Unit =
    test(markup) {
      val (database, occurrences) = computeDatabaseAndOccurrencesFromMarkup(markup)
      occurrences match {
        case List() => fn(database)
        case _ => sys.error(s"0 chevrons expected, ${occurrences.length} chevrons found")
      }
    }

  def targeted(markup: String, fn: (s.TextDocument, String) => Unit)(implicit
      hack: OverloadHack2
  ): Unit = test(markup) {
    val (database, occurrences) = computeDatabaseAndOccurrencesFromMarkup(markup)
    occurrences match {
      case List(name1) => fn(database, name1)
      case _ => sys.error(s"1 chevron expected, ${occurrences.length} chevrons found")
    }
  }

  def targeted(markup: String, fn: (s.TextDocument, String, String) => Unit)(implicit
      hack: OverloadHack3
  ): Unit = test(markup) {
    val (database, occurrences) = computeDatabaseAndOccurrencesFromMarkup(markup)
    occurrences match {
      case List(name1, name2) => fn(database, name1, name2)
      case _ => sys.error(s"2 chevrons expected, ${occurrences.length} chevrons found")
    }
  }

  def targeted(markup: String, fn: (s.TextDocument, String, String, String) => Unit)(implicit
      hack: OverloadHack4
  ): Unit = test(markup) {
    val (database, occurrences) = computeDatabaseAndOccurrencesFromMarkup(markup)
    occurrences match {
      case List(name1, name2, name3) => fn(database, name1, name2, name3)
      case _ => sys.error(s"3 chevrons expected, ${occurrences.length} chevrons found")
    }
  }

  def targeted(markup: String, fn: (s.TextDocument, String, String, String, String) => Unit)(
      implicit hack: OverloadHack5
  ): Unit = test(markup) {
    val (database, occurrences) = computeDatabaseAndOccurrencesFromMarkup(markup)
    occurrences match {
      case List(name1, name2, name3, name4) => fn(database, name1, name2, name3, name4)
      case _ => sys.error(s"4 chevrons expected, ${occurrences.length} chevrons found")
    }
  }

  def targeted(markup: String, fn: (s.TextDocument, String, String, String, String, String) => Unit)(
      implicit hack: OverloadHack5
  ): Unit = test(markup) {
    val (database, occurrences) = computeDatabaseAndOccurrencesFromMarkup(markup)
    occurrences match {
      case List(name1, name2, name3, name4, name5) => fn(database, name1, name2, name3, name4, name5)
      case _ => sys.error(s"5 chevrons expected, ${occurrences.length} chevrons found")
    }
  }

  def targeted(
      markup: String,
      fn: (s.TextDocument, String, String, String, String, String, String) => Unit
  )(implicit hack: OverloadHack5): Unit = test(markup) {
    val (database, occurrences) = computeDatabaseAndOccurrencesFromMarkup(markup)
    occurrences match {
      case List(name1, name2, name3, name4, name5, name6) =>
        fn(database, name1, name2, name3, name4, name5, name6)
      case _ => sys.error(s"6 chevrons expected, ${occurrences.length} chevrons found")
    }
  }

}

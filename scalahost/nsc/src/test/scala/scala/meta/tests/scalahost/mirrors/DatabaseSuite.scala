package scala.meta.tests
package scalahost

import org.scalatest._
import java.io.{File, PrintWriter}
import scala.collection.immutable.Seq
import scala.reflect.io._
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{CompilerCommand, Global, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.compat.Platform.EOL
import scala.{meta => m}
import scala.meta.internal.semantic.DatabaseOps

abstract class DatabaseSuite extends FunSuite { self =>
  private def test(code: String)(fn: => Unit): Unit = {
    var name = code.trim.replace(EOL, " ")
    if (name.length > 50) name = name.take(50) + "..."
    super.test(name)(fn)
  }

  lazy val g: Global = {
    def fail(msg: String) = sys.error(s"DatabaseSuite initialization failed: $msg")
    val classpath = System.getProperty("sbt.paths.scalahost.test.classes")
    val pluginpath = System.getProperty("sbt.paths.scalahost.compile.jar")
    val options = "-Yrangepos -Ywarn-unused-import -cp " + classpath + " -Xplugin:" + pluginpath + ":" + classpath + " -Xplugin-require:scalahost"
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
  private lazy val databaseOps: DatabaseOps { val global: self.g.type } = new DatabaseOps {
    val global: self.g.type = self.g
  }
  import databaseOps._

  private def computeDatabaseFromSnippet(code: String): m.Database = {
    val cwd = sys.props("user.dir")
    try {
      val javaFile = File.createTempFile("paradise", ".scala")
      val writer = new PrintWriter(javaFile)
      try writer.write(code)
      finally writer.close()

      sys.props("user.dir") = javaFile.getParentFile.getAbsolutePath
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
      val errors = reporter.infos.filter(_.severity == reporter.ERROR)
      errors.foreach(error => fail(s"scalac parse error: ${error.msg} at ${error.pos}"))

      val packageobjectsPhase = run.phaseNamed("packageobjects")
      val phases = List(run.parserPhase, run.namerPhase, packageobjectsPhase, run.typerPhase)
      reporter.reset()

      phases.foreach(phase => {
        g.phase = phase
        g.globalPhase = phase
        phase.asInstanceOf[g.GlobalPhase].apply(unit)
        val errors = reporter.infos.filter(_.severity == reporter.ERROR)
        errors.foreach(error => fail(s"scalac ${phase.name} error: ${error.msg} at ${error.pos}"))
      })
      g.phase = run.phaseNamed("patmat")
      g.globalPhase = run.phaseNamed("patmat")

      m.Database(List(unit.source.toInput -> unit.toAttributes))
    } finally {
      sys.props("user.dir") = cwd
    }
  }

  private def computeDatabaseSectionFromSnippet(code: String, sectionName: String): String = {
    val database = computeDatabaseFromSnippet(code)
    val path = g.currentRun.units.toList.last.source.file.file.getAbsolutePath
    val payload = database.toString.split(EOL)
    val section = payload.dropWhile(_ != sectionName + ":").drop(1).takeWhile(_ != "")
    section.mkString(EOL).replace(path, "<...>")
  }

  def names(code: String, expected: String): Unit = {
    test(code)(assert(expected === computeDatabaseSectionFromSnippet(code, "Names")))
  }

  def messages(code: String, expected: String): Unit = {
    test(code)(assert(expected === computeDatabaseSectionFromSnippet(code, "Messages")))
  }

  def denotations(code: String, expected: String): Unit = {
    test(code)(assert(expected === computeDatabaseSectionFromSnippet(code, "Denotations")))
  }

  def sugars(code: String, expected: String): Unit = {
    test(code)(assert(expected === computeDatabaseSectionFromSnippet(code, "Sugars")))
  }

  private def computeDatabaseAndNamesFromMarkup(markup: String): (m.Database, List[m.Name]) = {
    val chevrons = "<<(.*?)>>".r
    val ps0 = chevrons.findAllIn(markup).matchData.map(m => (m.start, m.end)).toList
    val ps = ps0.zipWithIndex.map { case ((s, e), i) => (s - 4 * i, e - 4 * i - 4) }
    val code = chevrons.replaceAllIn(markup, "$1")
    val database = computeDatabaseFromSnippet(code)
    val unit = g.currentRun.units.toList.last
    val source = unit.toSource
    val names = ps.map {
      case (s, e) =>
        val names = source.collect {
          case name: m.Name if name.pos.start.offset == s && name.pos.end.offset == e => name
        }
        val chevron = "<<" + code.substring(s, e) + ">>"
        names match {
          case Nil => sys.error(chevron + " does not wrap a name")
          case List(name) => name
          case _ => sys.error("fatal error processing " + chevron)
        }
    }
    (database, names)
  }

  trait OverloadHack1; implicit object OverloadHack1 extends OverloadHack1
  trait OverloadHack2; implicit object OverloadHack2 extends OverloadHack2
  trait OverloadHack3; implicit object OverloadHack3 extends OverloadHack3
  trait OverloadHack4; implicit object OverloadHack4 extends OverloadHack4
  trait OverloadHack5; implicit object OverloadHack5 extends OverloadHack5

  def targeted(markup: String, fn: m.Database => () => Unit)(implicit hack: OverloadHack1): Unit = {
    test(markup) {
      val (database, names) = computeDatabaseAndNamesFromMarkup(markup)
      names match {
        case List() => fn(database)()
        case _ => sys.error(s"0 chevrons expected, ${names.length} chevrons found")
      }
    }
  }

  def targeted(markup: String, fn: m.Database => m.Name => Unit)(
      implicit hack: OverloadHack2): Unit = {
    test(markup) {
      val (database, names) = computeDatabaseAndNamesFromMarkup(markup)
      names match {
        case List(name1) => fn(database)(name1)
        case _ => sys.error(s"1 chevron expected, ${names.length} chevrons found")
      }
    }
  }

  def targeted(markup: String, fn: m.Database => (m.Name, m.Name) => Unit)(
      implicit hack: OverloadHack3): Unit = {
    test(markup) {
      val (database, names) = computeDatabaseAndNamesFromMarkup(markup)
      names match {
        case List(name1, name2) => fn(database)(name1, name2)
        case _ => sys.error(s"2 chevrons expected, ${names.length} chevrons found")
      }
    }
  }

  def targeted(markup: String, fn: m.Database => (m.Name, m.Name, m.Name) => Unit)(
      implicit hack: OverloadHack4): Unit = {
    test(markup) {
      val (database, names) = computeDatabaseAndNamesFromMarkup(markup)
      names match {
        case List(name1, name2, name3) => fn(database)(name1, name2, name3)
        case _ => sys.error(s"3 chevrons expected, ${names.length} chevrons found")
      }
    }
  }

  def targeted(markup: String, fn: m.Database => (m.Name, m.Name, m.Name, m.Name) => Unit)(
      implicit hack: OverloadHack5): Unit = {
    test(markup) {
      val (database, names) = computeDatabaseAndNamesFromMarkup(markup)
      names match {
        case List(name1, name2, name3, name4) => fn(database)(name1, name2, name3, name4)
        case _ => sys.error(s"4 chevrons expected, ${names.length} chevrons found")
      }
    }
  }
}

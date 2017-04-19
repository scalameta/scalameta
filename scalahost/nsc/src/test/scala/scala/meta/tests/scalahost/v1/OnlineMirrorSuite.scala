package scala.meta.tests
package scalahost
package v1

import org.scalatest._
import java.io.{File, PrintWriter}
import scala.collection.immutable.Seq
import scala.{meta => m}
import scala.reflect.io._
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{CompilerCommand, Global, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.compat.Platform.EOL
import scala.meta.semantic.v1.Mirror
import scala.meta.semantic.v1.Database
import scala.meta.internal.scalahost.v1.online.{Mirror => OnlineMirror, _}

abstract class OnlineMirrorSuite extends FunSuite {
  private def test(code: String)(fn: => Unit): Unit = {
    var name = code.trim.replace(EOL, " ")
    if (name.length > 50) name = name.take(50) + "..."
    super.test(name)(fn)
  }

  lazy val g: Global = {
    def fail(msg: String) = sys.error(s"OnlineMirrorSuite initialization failed: $msg")
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

  implicit val mirror: OnlineMirror = new OnlineMirror(g)
  import mirror._

  // checks that parse(binary(database)) == database
  def assertDatabaseSerializationIsBijective(database: Database): Unit = {
    val binary = database.toBinary
    val database2 = Database.fromBinary(binary).get
    assert(database.toString === database2.toString)
  }

  private def computeDatabaseFromSnippet(code: String): Database = {
    val javaFile = File.createTempFile("paradise", ".scala")
    val writer = new PrintWriter(javaFile)
    try writer.write(code)
    finally writer.close()

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

    val database = unit.asInstanceOf[mirror.g.CompilationUnit].toDatabase
    assertDatabaseSerializationIsBijective(database)
    database
  }

  def database(code: String, expected: String): Unit = {
    test(code) {
      val database = computeDatabaseFromSnippet(code)
      val path = g.currentRun.units.toList.last.source.file.file.getAbsolutePath
      val actual = database.toString.split(EOL).drop(1).mkString(EOL).replace(path, "<...>")
      assert(expected === actual)
    }
  }

  def names(code: String, expected: String): Unit = {
    test(code) {
      val database = computeDatabaseFromSnippet(code)
      val path = g.currentRun.units.toList.last.source.file.file.getAbsolutePath
      val actual = database.toString.split(EOL).takeWhile(_ != "Denotations:").drop(1).dropRight(1).mkString(EOL).replace(path, "<...>")
      assert(expected === actual)
    }
  }

  def denotations(code: String, expected: String): Unit = {
    test(code) {
      val database = computeDatabaseFromSnippet(code)
      val actual = database.toString.split(EOL).dropWhile(_ != "Denotations:").drop(1).mkString(EOL)
      assert(expected === actual)
    }
  }

  private def computeDatabaseFromMarkup(markup: String): List[m.Name] = {
    val chevrons = "<<(.*?)>>".r
    val ps0 = chevrons.findAllIn(markup).matchData.map(m => (m.start, m.end)).toList
    val ps = ps0.zipWithIndex.map { case ((s, e), i) => (s - 4 * i, e - 4 * i - 4) }
    val code = chevrons.replaceAllIn(markup, "$1")
    val database = computeDatabaseFromSnippet(code)
    val unit = g.currentRun.units.toList.last.asInstanceOf[mirror.g.CompilationUnit]
    val source = unit.toSource
    ps.map {
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
  }

  def targeted(markup: String, fn: () => Unit): Unit = {
    test(markup) {
      val names = computeDatabaseFromMarkup(markup)
      names match {
        case List() => fn()
        case _ => sys.error(s"0 chevrons expected, ${names.length} chevrons found")
      }
    }
  }

  def targeted(markup: String, fn: m.Name => Unit): Unit = {
    test(markup) {
      val names = computeDatabaseFromMarkup(markup)
      names match {
        case List(name1) => fn(name1)
        case _ => sys.error(s"1 chevron expected, ${names.length} chevrons found")
      }
    }
  }

  def targeted(markup: String, fn: (m.Name, m.Name) => Unit): Unit = {
    test(markup) {
      val names = computeDatabaseFromMarkup(markup)
      names match {
        case List(name1, name2) => fn(name1, name2)
        case _ => sys.error(s"2 chevrons expected, ${names.length} chevrons found")
      }
    }
  }

  def targeted(markup: String, fn: (m.Name, m.Name, m.Name) => Unit): Unit = {
    test(markup) {
      val names = computeDatabaseFromMarkup(markup)
      names match {
        case List(name1, name2, name3) => fn(name1, name2, name3)
        case _ => sys.error(s"3 chevrons expected, ${names.length} chevrons found")
      }
    }
  }

  def targeted(markup: String, fn: (m.Name, m.Name, m.Name, m.Name) => Unit): Unit = {
    test(markup) {
      val names = computeDatabaseFromMarkup(markup)
      names match {
        case List(name1, name2, name3, name4) => fn(name1, name2, name3, name4)
        case _ => sys.error(s"4 chevrons expected, ${names.length} chevrons found")
      }
    }
  }
}

package scala.meta.tests
package scalahost
package v1

import org.scalatest._
import java.io.{File, PrintWriter}
import scala.collection.immutable.Seq
import scala.{meta => m}
import scala.reflect.io._
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.compat.Platform.EOL
import scala.meta.semantic.v1.Mirror
import scala.meta.semantic.v1.Database
import scala.meta.internal.scalahost.v1.OnlineMirror

abstract class OnlineMirrorSuite extends FunSuite {
  lazy val g: Global = {
    def fail(msg: String) = sys.error(s"OnlineMirrorSuite initialization failed: $msg")
    val classpath         = System.getProperty("sbt.paths.scalahost.test.classes")
    val pluginpath        = System.getProperty("sbt.paths.scalahost.compile.jar")
    val options           = "-Yrangepos -cp " + classpath + " -Xplugin:" + pluginpath + ":" + classpath + " -Xplugin-require:scalahost"
    val args              = CommandLineParser.tokenize(options)
    val emptySettings     = new Settings(error => fail(s"couldn't apply settings because $error"))
    val reporter          = new StoreReporter()
    val command           = new CompilerCommand(args, emptySettings)
    val settings          = command.settings
    val g                 = new Global(settings, reporter)
    val run               = new g.Run
    g.phase = run.parserPhase
    g.globalPhase = run.parserPhase
    g
  }

  implicit val mirror: OnlineMirror = new OnlineMirror(g)
  import mirror._

  private def computeDatabaseFromSnippet(code: String): Database = {
    val javaFile = File.createTempFile("paradise", ".scala")
    val writer   = new PrintWriter(javaFile)
    try writer.write(code)
    finally writer.close()

    val run          = new g.Run
    val abstractFile = AbstractFile.getFile(javaFile)
    val sourceFile   = g.getSourceFile(abstractFile)
    val unit         = new g.CompilationUnit(sourceFile)
    run.compileUnits(List(unit), run.phaseNamed("terminal"))

    g.phase = run.parserPhase
    g.globalPhase = run.parserPhase
    val reporter = new StoreReporter()
    g.reporter = reporter
    unit.body = g.newUnitParser(unit).parse()
    val errors = reporter.infos.filter(_.severity == reporter.ERROR)
    errors.foreach(error => fail(s"scalac parse error: ${error.msg} at ${error.pos}"))

    val packageobjectsPhase = run.phaseNamed("packageobjects")
    val phases              = List(run.parserPhase, run.namerPhase, packageobjectsPhase, run.typerPhase)
    reporter.reset()

    phases.foreach(phase => {
      g.phase = phase
      g.globalPhase = phase
      phase.asInstanceOf[g.GlobalPhase].apply(unit)
      val errors = reporter.infos.filter(_.severity == reporter.ERROR)
      errors.foreach(error => fail(s"scalac ${phase.name} error: ${error.msg} at ${error.pos}"))
    })

    unit.asInstanceOf[mirror.g.CompilationUnit].toDatabase
  }

  def database(code: String, dump: String): Unit = {
    var name = code.trim.replace(EOL, " ")
    if (name.length > 50) name = name.take(50) + "..."

    test(name) {
      val database = computeDatabaseFromSnippet(code)
      val lines = database.symbols.keys.toList
        .sortBy(_.start)
        .map(k => {
          val snippet = code.substring(k.start, k.end)
          val symbol  = database.symbols(k)
          s"[${k.start}..${k.end}): $snippet => ${symbol.id}"
        })
      lines.mkString(EOL)
    }
  }
}

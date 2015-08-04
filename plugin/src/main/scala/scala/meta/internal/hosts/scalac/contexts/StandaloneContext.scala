package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.contexts._
import org.scalameta.invariants._
import scala.meta.{ScalahostStandaloneContext => StandaloneContextApi}
import scala.meta.internal.hosts.scalac.converters.{mergeTrees => mmergeTrees}
import scala.compat.Platform.EOL
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.reflect.macros.runtime.AbortMacroException

@context(translateExceptions = false)
class StandaloneContext(options: String) extends GlobalContext(mkGlobal(options)) with StandaloneContextApi {
  def define(code: String): mapi.Source = {
    val reporter: StoreReporter = g.reporter.require[StoreReporter]
    reporter.reset()
    val gunit = g.newCompilationUnit(code, "<scalahost>")
    val gtree = g.newUnitParser(gunit).parse()
    if (reporter.hasErrors) throw new StandaloneException("parse has failed:" + EOL + (reporter.infos map (_.msg) mkString EOL))
    val gtypedtree = {
      import g.{reporter => _, _}
      import analyzer._
      val run = new Run
      phase = run.namerPhase
      globalPhase = run.namerPhase
      val namer = newNamer(rootContext(gunit))
      namer.enterSym(gtree)
      phase = run.typerPhase
      globalPhase = run.typerPhase
      val typer = newTyper(rootContext(gunit))
      val typedpkg = typer.typed(gtree).require[Tree]
      for (workItem <- gunit.toCheck) workItem()
      if (reporter.hasErrors) throw new StandaloneException("typecheck has failed:" + EOL + (reporter.infos map (_.msg) mkString EOL))
      typedpkg.require[PackageDef]
    }
    import scala.meta.dialects.Scala211
    val msyntacticTree = code.parse[mapi.Source].require[m.Source]
    val msemanticTree = gtypedtree.toMtree[m.Source]
    mmergeTrees(msyntacticTree, msemanticTree).require[m.Source]
  }
  val reporter = new StoreReporter()
}

object mkGlobal {
  def apply(options: String): Global = {
    var compilerOptions = options
    if (!compilerOptions.contains("-Xplugin-require:scalahost")) {
      val scalahostJar = {
        try getClass.getProtectionDomain().getCodeSource().getLocation().getFile()
        catch { case ex: Throwable => throw new scala.Exception("failed to auto-load the scalahost plugin", ex) }
      }
      val scalahostOptions = " -Xplugin:" + scalahostJar + " -Xplugin-require:scalahost"
      compilerOptions += scalahostOptions
    }
    val customGlobal = {
      val args = CommandLineParser.tokenize(compilerOptions)
      val emptySettings = new Settings(error => sys.error("compilation has failed: " + error))
      val reporter = new StoreReporter()
      val command = new CompilerCommand(args, emptySettings)
      val settings = command.settings
      val global = new Global(settings, reporter)
      val run = new global.Run
      global.phase = run.parserPhase
      global.globalPhase = run.parserPhase
      global
    }
    customGlobal
  }
}

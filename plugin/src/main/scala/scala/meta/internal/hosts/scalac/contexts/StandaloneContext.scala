package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.contexts._
import org.scalameta.invariants._
import scala.meta.{ScalahostStandaloneContext => StandaloneContextApi}
import scala.meta.internal.hosts.scalac.converters.{mergeTrees => mmergeTrees}
import scala.compat.Platform.EOL
import org.scalameta.internal.mkGlobal
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

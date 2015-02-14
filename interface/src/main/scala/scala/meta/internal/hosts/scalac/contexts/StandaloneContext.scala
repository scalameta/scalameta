package scala.meta
package internal.hosts.scalac
package contexts

import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.hosts.scalac.contexts.{SemanticContext => ScalahostSemanticContext}
import scala.compat.Platform.EOL
import org.scalameta.reflection.mkGlobal
import scala.tools.nsc.reporters.StoreReporter
import scala.{meta => papi}
import scala.meta.internal.{ast => p}

class StandaloneContext(options: String) extends ScalahostSemanticContext(mkGlobal(options)) {
  private val reporter: StoreReporter = g.reporter.asInstanceOf[StoreReporter]
  def define(code: String): p.Tree = {
    val gtree = g.newUnitParser(code, "<scalahost>").parse()
    val gtypedtree = {
      import g.{reporter => _, _}
      import analyzer._
      val run = new Run
      reporter.reset()
      phase = run.namerPhase
      globalPhase = run.namerPhase
      val namer = newNamer(rootContext(NoCompilationUnit))
      namer.enterSym(gtree)
      phase = run.typerPhase
      globalPhase = run.typerPhase
      val typer = newTyper(rootContext(NoCompilationUnit))
      val typedpkg = typer.typed(gtree).asInstanceOf[Tree]
      if (reporter.hasErrors) sys.error("typecheck has failed:" + EOL + EOL + (reporter.infos map (_.msg) mkString EOL))
      typedpkg.asInstanceOf[PackageDef].stats.head
    }
    val _ = toPtree.computeConverters // TODO: necessary because of macro expansion order
    toPtree(gtypedtree, classOf[papi.Stat])
  }
}

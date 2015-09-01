package scala.meta
package internal.hosts.scalac

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.meta.dialects.Scala211
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}
import scala.meta.internal.hosts.scalac.contexts.{Proxy => ProxyImpl}
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.reflect.io.AbstractFile
import org.scalameta.reflection._
import org.scalameta.invariants._

trait ConvertPhase {
  self: ScalahostPlugin =>

  object ConvertComponent extends NscPluginComponent {
    lazy val global: self.global.type = self.global
    import global._

    // TODO: ideally we would like to save everything after the very end of typechecking, which is after refchecks
    // but unfortunately by then a lot of semantic stuff is already desugared to death (patmat, superaccessors, some code in refchecks)
    // therefore we run after typer and hope for the best (i.e. that we don't run into nonsense that we don't know how to convert,
    // and also that we don't encounter residual cyclic reference errors which are the reason why certain typechecks are delayed past typer)
    // btw this isn't such a big problem for persistence, but it definitely is for macro interpretation
    // let's hope that the research into runtime macros, which entails moving the typechecker to scala-reflect.jar will allow us to restructure things
    // so that delayed typechecks come right after typer, not intermingled with other logic
    override val runsAfter = List("typer")
    override val runsRightAfter = None
    override val phaseName = "convert"
    override def description = "convert compiler trees to scala.meta"

    override def newPhase(prev: Phase): Phase = new Phase(prev) {
      override def name = "convert"
      override def run(): Unit = {
        val proxy = new ProxyImpl[global.type](global)
        // NOTE: no need to call this explicitly - the constructor will do that for us
        // proxy.indexCompilationUnits()
      }
    }
  }
}

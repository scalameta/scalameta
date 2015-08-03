package scala.meta
package internal.hosts.scalac

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.meta.dialects.Scala211
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}
import scala.meta.internal.hosts.scalac.converters.mergeTrees
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.reflect.io.AbstractFile
import org.scalameta.reflection._
import org.scalameta.invariants._

trait ConvertPhase {
  self: ScalahostPlugin =>

  object ConvertComponent extends NscPluginComponent {
    val global: self.global.type = self.global
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
    val phaseName = "convert"
    override def description = "convert compiler trees to scala.meta"
    implicit val c = Scalahost.mkGlobalContext[global.type](global)
    import c.decorators._

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        // NOTE: We don't have to persist perfect trees, because tokens are transient anyway.
        // Therefore, if noone uses perfect trees in a compiler plugin, then we can avoid merging altogether.
        // Alternatively, if we hardcode merging into the core of scalameta/scalameta
        // (e.g. by making it lazy, coinciding with the first traversal of the perfect tree),
        // then we can keep mergeTrees and expose its results only to those who need perfectTrees
        // (e.g. to compiler plugins that want to work with scala.meta trees).
        // TODO: For now, I'm going to keep mergeTrees here, but in the 0.1 release,
        // we might want to turn merging off if it turns out being a big performance hit.
        val syntacticTree = unit.source.content.parse[mapi.Source].require[m.Source]
        val semanticTree = unit.body.toMtree[m.Source]
        val perfectTree = mergeTrees(syntacticTree, semanticTree)
        unit.body.appendMetadata("scalameta" -> perfectTree)
      }
    }
  }
}

package scala.meta
package internal.hosts.scalacompiler
package macros

import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
import scala.meta.internal.hosts.scalacompiler.{Plugin => PalladiumPlugin}
import scalahost.Scalahost
import scala.reflect.internal.util.{SourceFile, BatchSourceFile, OffsetPosition}

trait RenumberPhase {
  self: PalladiumPlugin =>

  object RenumberComponent extends NscPluginComponent {
    val global: self.global.type = self.global
    import global._
    import analyzer._
    import Settings._

    override val runsAfter = List("typer")
    override val runsRightAfter = Some("typer")
    val phaseName = "renumber"
    override def description = "renumber positions of macro expansions"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        def fail(msg: String) = if (!reporter.hasErrors) reporter.error(NoPosition, msg)
        (YfictionalLineNumbers.value, YshiftingLineNumbers.value) match {
          case (true, true) => fail("can't have both fictional and shifting line numbers enabled")
          case (true, false) => fictional(unit)
          case (false, true) => fail("shifting line numbers aren't implemented yet")
          case _ => // do nothing
        }
      }
      private def fictional(unit: CompilationUnit): Unit = {
        // TODO: can't assign lines here, we can only operate with offsets...
        // unit.body.foreach(tree => {
        //   if (hasMacroExpansionAttachment(tree)) {
        //     val f = unit.source
        //     def totalLines(f: SourceFile) = f.offsetToLine(f.length - 1) + 1
        //     val factor = Math.pow(10, totalLines(f).toString.length).toInt
        //     // val bag = tree.attachments.get[java.util.HashMap[String, Any]].get
        //     // println(bag.get("expansionString").asInstanceOf[String])
        //     // println((factor, digits))
        //     var currentLine = (tree.pos.line + 1) * factor - 1
        //     def currentPos = new OffsetPosition(f, f.lineToOffset(currentLine))
        //     object renumber extends Traverser {
        //       override def traverse(tree: Tree): Unit = tree match {
        //         case Block(stats, expr) =>
        //           tree.setPos(currentPos)
        //           currentLine += 1 // opening curly brace
        //           (stats :+ expr).foreach(stat => { currentLine += 1; traverse(stat) })
        //           currentLine += 1 // closing curly brace
        //         case _ =>
        //           tree.setPos(currentPos)
        //           super.traverse(tree)
        //       }
        //     }
        //     renumber.traverse(tree)
        //   }
        // })
      }
    }
  }
}
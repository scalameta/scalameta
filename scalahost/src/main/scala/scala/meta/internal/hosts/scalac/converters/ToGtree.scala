package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.compat.Platform.EOL
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.tools.nsc.reporters.StoreReporter
import scala.meta.dialects.Scala211
import scala.meta.internal.{ast => m}
import scala.meta.internal.prettyprinters._
import scala.meta.internal.hosts.scalac.reflect._

// This module exposes a method to convert from scala.meta trees to scala.reflect trees.
// Nothing is implemented yet, but we'll have to at least take a stab at it to enable scala.meta macros.
trait ToGtree extends ReflectToolkit with MetaToolkit {
  self: Api =>

  protected implicit class XtensionMtreeToGtree(mtree: m.Tree) {
    def toGtree: g.Tree = {
      // TODO: LOLOLOL
      def fail(msg: String) = {
        val details = s"$EOL$msg$EOL${mtree.show[Syntax]}$EOL${mtree.show[Structure]}"
        sys.error(s"implementation restriction: error converting from ${mtree.show[Summary]} to g.Tree:$details")
      }
      val scode = {
        import scala.meta.dialects.Scala211
        mtree.show[Syntax]
      }
      val gcode = {
        val newReporter = new StoreReporter()
        val oldReporter = g.reporter
        try {
          g.reporter = newReporter
          val gparser = g.newUnitParser(new g.CompilationUnit(g.newSourceFile(scode, "<toGtree>")))
          val gtree = mtree match {
            case _: m.Source => gparser.compilationUnit()
            case _: m.Term => gparser.expr()
            case _: m.Type => gparser.typ()
            case _ => fail(s"scala.meta trees of type ${mtree.productPrefix} are unsupported")
          }
          newReporter.infos.foreach { case newReporter.Info(pos, msg, newReporter.ERROR) => fail(msg) }
          gtree
        } finally g.reporter = oldReporter
      }
      gcode
    }
  }
}
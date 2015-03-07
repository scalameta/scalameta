package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.compat.Platform.EOL
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.tools.nsc.reporters.StoreReporter
import scala.meta.internal.{ast => m}
import scala.meta.internal.ui.Summary

// This module exposes a method to convert from scala.meta trees to scala.reflect trees.
// Nothing is implemented yet, but we'll have to at least take a stab at it to enable scala.meta macros.
trait ToGtree extends GlobalToolkit with MetaToolkit {
  self: Api =>

  def toGtree(mtree: m.Tree): g.Tree = mtree.toGtree

  protected implicit class RichToGtree(mtree: m.Tree) {
    def toGtree: g.Tree = mtree match {
      case mtree: m.Term =>
        // TODO: HAHAHA
        val scode = {
          import scala.meta.dialects.Scala211
          mtree.show[Code]
        }
        val gcode = {
          val newReporter = new StoreReporter()
          val oldReporter = g.reporter
          try {
            g.reporter = newReporter
            val gparser = g.newUnitParser(new g.CompilationUnit(g.newSourceFile(scode, "<scalahost>")))
            val gtree = gparser.expr()
            def fail(msg: String) = sys.error(s"implementation restriction: error converting from ${mtree.show[Summary]} to g.Tree:$EOL$msg$EOL${mtree.show[Code]}")
            newReporter.infos.foreach { case newReporter.Info(pos, msg, newReporter.ERROR) => fail(msg) }
            gtree
          } finally g.reporter = oldReporter
        }
        gcode
      case _ =>
        ???
    }
  }
}
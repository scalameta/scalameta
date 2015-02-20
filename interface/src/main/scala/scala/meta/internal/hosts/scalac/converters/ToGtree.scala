package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.internal.{ast => m}

// This module exposes a method to convert from scala.meta trees to scala.reflect trees.
// Nothing is implemented yet, but we'll have to at least take a stab at it to enable scala.meta macros.
trait ToGtree extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class RichToGtree(mtree: m.Tree) {
    def toGtree: g.Tree = {
      ???
    }
  }
}
package scala.meta
package internal
package ui

import scala.annotation.implicitNotFound
import org.scalameta.show._
import Show.{sequence => s, repeat => r, indent => i, newline => n}
import scala.meta.internal.{ast => m}
import org.scalameta.invariants._
import org.scalameta.unreachable

@implicitNotFound(msg = "don't know how to show[TopLevel] for ${T}")
private[meta] trait TopLevel[T] extends Show[T]
private[meta] object TopLevel {
  def apply[T](f: T => Show.Result): TopLevel[T] = new TopLevel[T] { def apply(input: T) = f(input) }

  implicit def toplevel[T <: Tree]: TopLevel[T] = TopLevel((x: T) => x match {
    case m.Source(stats) => r(stats, "; ")
    case m.Pkg(ref, stats) => s(s"package $ref { ", r(stats, "; "), " }")
    case m.Defn.Class(_, name, _, _, _) => s(s"class $name...")
    case m.Defn.Trait(_, name, _, _, _) => s(s"trait $name...")
    case m.Defn.Object(_, name, _, _) => s(s"object $name...")
    case m.Pkg.Object(_, name, _, _) => s(s"package object $name...")
    case _ => unreachable(debug(x, x.show[Structure]))
  })
}

package scala.meta
package internal
package prettyprinters

import scala.annotation.implicitNotFound
import org.scalameta._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.meta.prettyprinters._
import Show.{sequence => s, repeat => r, indent => i, newline => n}

@implicitNotFound(msg = "don't know how to show[TopLevel] for ${T}")
private[meta] trait TopLevel[T] extends Show[T]
private[meta] object TopLevel {
  def apply[T](f: T => Show.Result): TopLevel[T] = new TopLevel[T] { def apply(input: T) = f(input) }

  implicit def toplevel[T <: Tree]: TopLevel[T] = TopLevel((x: T) => x match {
    case Source(stats) => r(stats, "; ")
    case Pkg(ref, stats) => s(s"package $ref { ", r(stats, "; "), " }")
    case Defn.Class(_, name, _, _, _) => s(s"class $name...")
    case Defn.Trait(_, name, _, _, _) => s(s"trait $name...")
    case Defn.Object(_, name, _) => s(s"object $name...")
    case Pkg.Object(_, name, _) => s(s"package object $name...")
    case _ => unreachable(debug(x, x.show[Structure]))
  })
}

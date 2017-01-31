package scala.meta.contrib

import scala.language.implicitConversions
import scala.meta.Tree

/** Represents syntactic equality between trees */
class Syntactic[+A <: Tree](val tree: A) {
  private lazy val syntax      = tree.syntax
  override def hashCode(): Int = syntax.hashCode
  override def equals(obj: scala.Any): Boolean = obj match {
    case e2: Syntactic[_] => syntax == e2.tree.syntax
    case _                => false
  }
}

object Syntactic {
  implicit def SyntacticEq[A <: Tree]: Equal[Syntactic[A]] =
    new Equal[Syntactic[A]] {
      override def equal(a: Syntactic[A], b: Syntactic[A]): Boolean = a.equals(b)
    }
  implicit def toSyntactic[A <: Tree](e: A): Syntactic[A] = new Syntactic[A](e)
}

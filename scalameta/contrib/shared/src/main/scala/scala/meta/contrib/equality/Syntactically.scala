package scala.meta.contrib.equality

import scala.language.implicitConversions
import scala.meta.Tree

/** Represents syntactic equality between trees
  * Two trees are syntactically equal if their .syntax is equal.
  **/
class Syntactically[+A <: Tree](val tree: A) extends TreeEquality[A] {
  private lazy val syntax = tree.syntax
  override def hashCode(): Int = syntax.hashCode
  override def equals(obj: scala.Any): Boolean = obj match {
    case e2: Syntactically[_] => syntax == e2.syntax
    case _ => false
  }
}

object Syntactically {
  def apply[A <: Tree](tree: A): Syntactically[A] =
    new Syntactically(tree)

  implicit def SyntacticEq[A <: Tree]: Equal[Syntactically[A]] =
    new Equal[Syntactically[A]] {
      override def isEqual(a: Syntactically[A], b: Syntactically[A]): Boolean = a.equals(b)
    }
  implicit def toSyntactic[A <: Tree](e: A): Syntactically[A] = apply(e)
}

package scala.meta.contrib

import scala.annotation.implicitNotFound
import scala.collection.immutable.Seq

/**
  *
  * The purpose of this typeclass is to replace
  * one list of things with another of that type.
  *
  * eg. replacing the statements of a Defn.Class
  * with a new set of statements
  *
  * @tparam A the object you are replacing the items in
  * @tparam B the type of items you are replacing
  */
trait Replace[A, B] {
  def replace(a: A, bs: Seq[B]): A
}

object Replace {
  def apply[A, B](f: (A, Seq[B]) => A): Replace[A, B] = new Replace[A, B] {
    @inline override def replace(a: A, bs: Seq[B]): A = f(a, bs)
  }
}

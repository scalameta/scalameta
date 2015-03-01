package org.scalameta.algebra

import scala.language.higherKinds
import scala.collection.generic._

/**
 * https://en.wikipedia.org/wiki/Monoid
 * */
// NOTE: I don't like reinventing the wheels, but I honestly don't know where to get Monoid.
// Scalaz, spire, algebra, cats, structures, even shapeless and who knows what else defines monoids...
trait Monoid[A] {
  def zero : A
  def append(a: A, b: A): A
}

/**
 * Different base implementation of several instances of Monoids
 * */
object Monoid {
  //note: http://stackoverflow.com/questions/15623585/why-is-list-a-semigroup-but-seq-is-not
  implicit def listMonoid[A] = new Monoid[List[A]] {
    def zero = Nil
    def append(a: List[A], b: List[A]) = a ::: b
  }

  implicit def setMonoid[A] = new Monoid[Set[A]] {
    def zero = Set.empty[A]
    def append(a: Set[A], b: Set[A]) = a ++ b
  }

  implicit def traversableMonoid[A, B[A] <: Traversable[A]](implicit y: CanBuildFrom[B[A], A, B[A]]) =
    new Monoid[B[A]] {
      def zero = y.apply.result
      def append(a: B[A], b: B[A]): B[A] =
        if (a.size == 0) b
        else if (b.size == 0) a
        else {
          val x = y.apply
          x ++= a
          x ++= b
          x.result
        }
  }

  implicit def mapMonoid[A, C, B[A, C] <: Traversable[(A, C)]](implicit y: CanBuildFrom[B[A, C], (A, C), B[A, C]]) =
    new Monoid[B[A, C]] {
      def zero = y.apply.result
      def append(a: B[A, C], b: B[A, C]): B[A, C] = {
        val x = y.apply
        x ++= a
        x ++= b
        x.result
      }
  }

  implicit object Void extends Monoid[Unit]{
    def zero = ()
    def append(a: Unit, b: Unit) = ()
  }

  implicit def tupleMonoid[A : Monoid, B : Monoid] = new Monoid[(A, B)] {
    def zero = (implicitly[Monoid[A]].zero,  implicitly[Monoid[B]].zero)
    def append(a: (A, B), b: (A, B)) = (a._1 + b._1, a._2 + b._2)
  }

  //State monoid, from https://hackage.haskell.org/package/monoid-transformer-0.0.2/docs/src/Data-Monoid-State.html#T
  case class Cons[S, A](run: S => (A, S))

  def pure[S, A](a: A) = Cons((s: S) => (a, s))
  def evaluate[S, A](s: S, m: Cons[S, A]) = m.run(s)._1
  def execute [S, A](s: S, m: Cons[S, A]) = m.run(s)._2
  def put[S, A: Monoid](s: S) = Cons((_: S) => (implicitly[Monoid[A]].zero, s))
  def modify[S, A: Monoid](f: S => S) = Cons((s: S) => (implicitly[Monoid[A]].zero, f(s)))

  implicit def stateMonoid[S, A : Monoid] = new Monoid[Cons[S, A]] {
    def zero = pure[S, A](implicitly[Monoid[A]].zero)
    def append(x: Cons[S, A], y: Cons[S, A]) = Cons(s0 => {
      val (xr, s1) = x.run(s0)
      val (yr, s2) = y.run(s1)
      (implicitly[Monoid[A]].append(xr, yr), s2)
    })
  }
}
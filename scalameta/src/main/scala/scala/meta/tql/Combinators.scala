package scala.meta
package tql

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.collection.generic.CanBuildFrom
import org.scalameta.algebra._
import scala.meta.internal.tql._

/**
 * This trait contains the base combinators which can be used to write powerful traversers.
 * */
trait Combinators[T] { self: Traverser[T] =>

  /**
   * Simple Identity combinator. a identity = a
   * */
  def identity[A : Monoid] = Matcher[A]{tree => Some(tree, implicitly[Monoid[A]].zero)}

  /**
   * Traverse the direct children of the tree and apply f to them.
   * */
  def children[A : Monoid](f: => Matcher[A]) = Matcher[A]{ tree =>
    traverse(tree, f)
  }

  /*
  * Is successful if at least one of the children is successful
  * TODO currently using a var, change that when stateful transformation are working
  * */
  def oneOfChildren[A: Monoid](m: => Matcher[A]) = Matcher[A] { tree =>
    var oneSuccess = false
    def wrapper = Matcher[A] { t =>
      m(t) match {
        case x @ Some(_) =>
          oneSuccess = true
          x
        case None => identity.apply(t)
      }
    }
    val traverseChildren = children(wrapper).apply(tree)
    if (oneSuccess)
      traverseChildren
    else
      None
  }

  /**
   * Same as TopDown, but does not sop when a transformation/traversal has succeeded
   * */
  def topDown[A : Monoid](m: Matcher[A]): Matcher[A] =
    m + children(topDown[A](m))

  /**
   * Same as bottomUpBreak, but does not sop when a transformation/traversal has succeeded
   * */
  def bottomUp[A : Monoid](m: => Matcher[A]): Matcher[A] =
    children(bottomUp[A](m)) + m

  /**
   * Traverse the tree in a TopDown manner, stop when a transformation/traversal has succeeded
   * */
  def topDownBreak[A : Monoid](m: Matcher[A]): Matcher[A] =
    m | children(topDownBreak[A](m))

  /**
   * Traverse the tree in a BottomUp manner, stop when a transformation/traversal has succeeded
   * */
  def bottomUpBreak[A : Monoid](m: Matcher[A]): Matcher[A] =
    oneOfChildren(bottomUpBreak[A](m)) | m


  /*
  * Traverse the structure and apply m1 until m2 matches. Then return only the result aggregated by m1
  * */
  def until[A : Monoid, B](m1: => Matcher[A], m2: Matcher[B]): Matcher[A] =
    m2 |> (m1 + children(until(m1, m2)))

  /*
  * Traverse the structure and apply m1 until m2 matches. Return a tuple of the result of m1 and m2,
  * TODO find a better name
  * */
  def tupledUntil[A : Monoid, B: Monoid](m1: Matcher[A], m2: Matcher[B]): Matcher[(A, B)] =
    m2.map(x => (implicitly[Monoid[A]].zero, x)) |
    (m1.map(x => (x, implicitly[Monoid[B]].zero)) + children(tupledUntil(m1, m2)))

  /**
   * The infamous fix point combinator
   * */
  def fix[A](f: Matcher[A] => Matcher[A]): Matcher[A] = Matcher[A]{ tree =>
    f(fix(f))(tree)
  }

  /**
   * Succeed if the partial function f applied on the tree is defined and return true
   * */
  def guard[U <: T : ClassTag](f: PartialFunction[U, Boolean]) = Matcher[U]{
    case t: U if f.isDefinedAt(t) && f(t) => Some((t, t))
    case _ => None
  }

  /**
   * Alias for focus{case t: U => some((t,t)}
   * */
  def select[U <: T: ClassTag] = Matcher[U]{
    case t: U => Some((t, t))
    case _ => None
  }

  /**
   * Alias for select
   * */
  def @@[U <: T: ClassTag] = select[U]

  /**
   *  Transform a I into a T where both I and O are subtypes of T and where a transformation from I to O is authorized
   * */
  def transformWithResult[I <: T : ClassTag, O <: T, A](f: PartialFunction[I, (O, A)])(implicit x: AllowedTransformation[I, O]) =
    Matcher[A] {
      case t: I if f.isDefinedAt(t) => Some(f(t))
      case _ => None
    }

  /**
   *  Traverse the data structure and return a result
   * */
  def visit[A](f: PartialFunction[T, A])(implicit x: ClassTag[T]) =
    guard[T]{case t => f.isDefinedAt(t)} map(f(_))


  /**
   * This trait is part of the infrastructure required to make the collect combinator have a
   * default parameter to List[_].
   * C is the type of the collection (ex: List[Int]) given by the User (or inferred to Nothing by the compiler)
   * A is the element inside the collection (in List[Int] it would be Int)
   * R is the 'real' collection that will be used i.e if C = Nothing => R = List[A] else R = C
   *
   * Note: maybe it would be interesting to generate such boilerplaite with a macro annotation. Something like that:
   * @default(C = List[_]) def collect[C[_] ]{..}
   * */
  trait Collector[C, A, R] {
    def builder: mutable.Builder[A, R]
  }
  object Collector {
    implicit def nothingToList[A](implicit y: CanBuildFrom[List[A], A, List[A]], m: Monoid[List[A]]) =
      new Collector[Nothing, A, List[A]] {
        def builder = {y().clear(); y()}
      }

    implicit def otherToCollect[A, C[A]](implicit y: CanBuildFrom[C[A], A, C[A]], m: Monoid[C[A]]) =
      new Collector[C[A], A, C[A]] {
        def builder = {y().clear(); y()}
      }
  }

  /**
   * This is solely use to write collect as  def collect[C[_] ] = new CollectInType[C] {..} instead of
   * def collect[C[_] ] = new {} and so to not use reflective calls. This as not been benchmarked so I don't really know
   * if it has a real performance impact
   * */
  abstract class CollectInType[C[_]] {
    def apply[A, R](f: PartialFunction[T, A])(implicit  x: ClassTag[T], y: Collector[C[A], A, R]): Matcher[R]
  }

  /**
   * Same as visit but puts the results into a collection (List by default)
   * */
  def collect[C[_]] = new CollectInType[C] {
    def apply[A, R](f: PartialFunction[T, A])(implicit  x: ClassTag[T], y: Collector[C[A], A, R]): Matcher[R] =
      Matcher[R] { tree =>
        if (f.isDefinedAt(tree)) Some((tree, (y.builder += f(tree)).result))
        else None
      }
  }

  /**
   * same as collect but put the results into a collection of 2 type parameter e.g a Map[K, V]
   * */
  def collect2[V[_, _]] = new {
    def apply[A, B](f: PartialFunction[T, (A, B)])(implicit  x: ClassTag[T], y: CanBuildFrom[V[A, B], (A, B), V[A, B]]) =
      guard[T]{case t => f.isDefinedAt(t)} map(t => (y() += f(t)).result)
  }


  /**
   * Part of the 'transform' infrastructure.
   * transform ca take either a T only or a (T, A).
   * (t, a) is bit cumbersome to write and that is why CTWithResult exists.
   * This allows to write t andCollect a instead of (t, List(a))
   * */
  implicit class CTWithResult[U <: T](t: U) {
    def withResult[A](a: A): (U, A) = macro CombinatorMacros.TWithResult[U, A]//(t, a)
    def andCollect[C[_]] = new {
        def apply[A, R](a: A)(implicit y: Collector[C[A], A, R]): (U, R) = macro CombinatorMacros.TAndCollect[U, A]
      }
  }

  /**
   * Syntactic sugar for transform combinator so that one doesn't need to type the type parameter
   * */
  def transform(f: PartialFunction[T, Any]): Matcher[Any] = macro CombinatorMacros.transformSugarImpl[T]

  /**
   * Syntactic sugar for guard combinator so that one doesn't need to type the type parameter
   * */
  def focus(f: PartialFunction[T, Boolean]): Matcher[T] = macro CombinatorMacros.filterSugarImpl[T]
}


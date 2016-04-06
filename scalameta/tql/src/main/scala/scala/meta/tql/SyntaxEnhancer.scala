package scala.meta
package tql

import scala.language.implicitConversions
import scala.reflect.ClassTag

trait SyntaxEnhancer[T] { self: Combinators[T] with Traverser[T] =>

  implicit class TEnhancer(t: T){

    def >>[A](a: Matcher[A]) = a(t)

    def resultOf[A : Monoid](a: Matcher[A]) = new MatcherResultEnhancer(a(t)).result
    def treeOf[A : Monoid](a: Matcher[A]) = new MatcherResultEnhancer(a(t)).tree
  }

  /*Required for things inside TreeMapperEnhancer*/
  def topDownBreakAlias[A : Monoid](m: Matcher[A]) = topDownBreak(m)
  def topDownAlias[A : Monoid](m: Matcher[A]) = topDown(m)
  def bottomUpBreakAlias[A : Monoid](m: Matcher[A]) = bottomUpBreak(m)
  def bottomUpAlias[A : Monoid](m: Matcher[A]) = bottomUp(m)
  def childrenAlias[A : Monoid](m: Matcher[A]) = children(m)

  implicit class TreeMapperEnhancer[A](a: Matcher[A]){
    //def >>[B] (f: T => MatchResult[B]) = flatMap(f)
    /*def apply[I <: T : ClassTag, O <: T]
             (f: PartialFunction[I, O])
             (implicit x: ClassTag[T], y: AllowedTransformation[I, O]) =
      transformWithResult[I, O](f)  */

    def collect = a.map(List(_))
    def topDownBreak(implicit x: Monoid[A]) = topDownBreakAlias(a)
    def topDown(implicit x: Monoid[A]) = topDownAlias(a)
    def bottomUpBreak(implicit x: Monoid[A]) = bottomUpBreakAlias(a)
    def bottomUp(implicit x: Monoid[A]) = bottomUpAlias(a)
    def children(implicit x: Monoid[A]) = childrenAlias(a)
  }


  /**
   * Convention : Operators ending with : are right assosiative
   * Moreover a \: b is desugared to b.\:(a)
   */
  implicit class MatcherXPath[A : Monoid](a: Matcher[A]){
    def \: (t: T) = topDownBreakAlias(a).apply(t)
    def \\: (t: T) = topDownAlias(a).apply(t)
    def \:[B] (b: Matcher[B]) = b andThen topDownBreakAlias(a)
    def \\:[B] (b: Matcher[B]) = b andThen topDownAlias(a)
  }

  implicit class MatcherResultEnhancer[A](a: MatchResult[A]){
    def result(implicit x: Monoid[A])  = a.map(_._2).getOrElse(x.zero)
    def tree    = a.map(_._1)
  }

  implicit def MatcherResultToResult[A : Monoid](a: MatchResult[A]): A = a.result
  implicit def MatcherResultToTree(a : MatchResult[_]): Option[T] = a.tree
}

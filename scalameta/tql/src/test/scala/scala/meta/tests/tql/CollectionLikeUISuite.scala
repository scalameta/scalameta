package scala.meta.tests
package tql

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.{ast => impl}
import org.scalatest.FunSuite
import scala.meta.tql.basicApi._

class CollectionLikeUISuite extends FunSuite {
  // TODO: fixme
  // val x = q"""
  //   val a = 5
  //   val c = 3
  //   c = 5
  //   if (3 == 17) {
  //    val c = 1
  //    while (a != c) println(78)
  //    val x = 14
  //    while (a != c) println(85)
  //   } else {
  //     2
  //   }
  //   5
  // """
  lazy val x: Term = ???
  lazy val t1: List[Int] = x.collect{case impl.Lit.Int(a) if a > 10 => a}
  lazy val t2: List[Int] = x.focus({case impl.Term.If(_,_,_) => true}).topDown.collect{case impl.Lit.Int(a) => a}
  lazy val t3: (Tree, List[String]) = {
    import scala.meta.tql._
    x.transform{case impl.Defn.Val(a, b, c, d) => impl.Defn.Var(a,b,c,Some(d)) andCollect(b.toString)}
  }
  lazy val t4: Tree = x.transform{case impl.Lit.Int(x) => impl.Lit.Int(x * 2)}
  lazy val t5: Set[String] = x.bottomUp.collect[Set]{case x: impl.Defn.Val => x.pats.head.toString}
  lazy val t6: List[Int] = {
    import scala.meta.tql._
    x.focus({case impl.Term.If(_,_,_) => true}).combine(topDown(collect{case impl.Lit.Int(a) => a})).result
  }
  lazy val t7: scala.meta.Tree = x.transform {
    case impl.Lit.Int(a) => impl.Lit.Int(a * 3)
    case impl.Defn.Val(a, b, c, d) => impl.Defn.Var(a,b,c,Some(d))
  }
}
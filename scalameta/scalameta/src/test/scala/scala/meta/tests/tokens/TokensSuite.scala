package scala.meta.tests
package tokens

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta._

// NOTE: don't run anything, just make sure that stuff compiles
class TokensSuite {
  def newToken: Token = ???
  val d: Tokens = ???
  val d1: Seq[Token] = d ++ d
  val d2: Seq[Token] = newToken +: d
  val d3: Seq[Token] = d :+ newToken
  val (d41: Token) +: (d42: Seq[Token]) :+ (d43: Token) = d
  val d5a: Seq[Int] = d.map(_ => 42)
  val d5b: Seq[Token] = d5a.map(_ => newToken)
  val d5c: Seq[Token] = d.map(_.toString).flatMap(_ => List(newToken))
  val d5d: Seq[Token] = d.flatMap(_ => d)
  val d6a: Seq[String] = d.zip(List(3, 4, 5)).zipWithIndex.map{ case ((x, y), _) => x.toString + y.toString }
  val d6b: Seq[Token] = d.zip(List(3, 4, 5)).zipWithIndex.map{ case ((x, y), _) => newToken }
  val d6c: Seq[Token] = d.zip(List(3, 4, 5)).zipWithIndex.flatMap{ case ((x, y), _) => d }
  val d7a: Seq[(Token, Int)] = d.zipWithIndex
  val d8: Seq[Token] = d.slice(0, 1)
}
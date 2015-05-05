import scala.meta._

// NOTE: don't run anything, just make sure that stuff compiles
class TokensSuite {
  val c = Tokens.Projected(1, 2, 3)
  val c1: Tokens.Projected[Int] = c ++ c
  val c2: Tokens.Projected[Int] = 0 +: c
  val c3: Tokens.Projected[Int] = c :+ 4
  val (c41: Int) +: (c42: Tokens.Projected[Int]) :+ (c43: Int) = c
  val c5: Tokens.Projected[Int] = c.map(_.toString).flatMap(_ => List(1, 2, 3))
  val c6: Tokens.Projected[String] = c.zip(List(3, 4, 5)).zipWithIndex.map{ case ((x, y), _) => x.toString + y.toString }
  val c7: Tokens.Projected[(Int, Int)] = c.zipWithIndex
  val c8: Tokens.Projected[Int] = c.slice(0, 1)

  def newToken: Token = null
  val d = Tokens(newToken, newToken, newToken)
  val d1: Tokens = d ++ d
  val d2: Tokens = newToken +: d
  val d3: Tokens = d :+ newToken
  val (d41: Token) +: (d42: Tokens) :+ (d43: Token) = d
  val d5a: Tokens.Projected[Int] = d.map(_ => 42)
  val d5b: Tokens = d5a.map(_ => newToken)
  val d5c: Tokens = d.map(_.toString).flatMap(_ => List(newToken))
  val d5d: Tokens = d.flatMap(_ => Tokens())
  val d6a: Tokens.Projected[String] = d.zip(List(3, 4, 5)).zipWithIndex.map{ case ((x, y), _) => x.toString + y.toString }
  val d6b: Tokens = d.zip(List(3, 4, 5)).zipWithIndex.map{ case ((x, y), _) => newToken }
  val d6c: Tokens = d.zip(List(3, 4, 5)).zipWithIndex.flatMap{ case ((x, y), _) => Tokens() }
  val d7a: Tokens.Projected[(Token, Int)] = d.zipWithIndex
  val d8: Tokens = d.slice(0, 1)
}
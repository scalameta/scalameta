package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.prettyprinters._

class PositionSuite extends ParseSuite {
  test("1 + (2 / 3) * 4") {
    val tree = term("1 + (2 / 3) * 4")
    assert(tree.show[Positions] === """
      |Term.ApplyInfix{0..15}(
        |Lit{0..1}(1),
        | Term.Name{2..3}("+"),
        | Nil,
        | List(
          |Term.ApplyInfix{4..15}(
            |Term.ApplyInfix{4..11}(
              |Lit{5..6}(2),
              | Term.Name{7..8}("/"),
              | Nil,
              | List(Lit{9..10}(3))),
            | Term.Name{12..13}("*"),
            | Nil,
            | List(Lit{14..15}(4)))))
    """.trim.stripMargin.split("\n").mkString)
  }
  test("(1 + 2).foo") {
    val tree = term("(1 + 2).foo")
    assert(tree.show[Positions] === """
      |Term.Select{0..11}(
        |Term.ApplyInfix{0..7}(
          |Lit{1..2}(1),
          | Term.Name{3..4}("+"),
          | Nil,
          | List(Lit{5..6}(2))),
        | Term.Name{8..11}("foo"))
    """.trim.stripMargin.split("\n").mkString)
  }
  test("#333") {
    val tree = blockStat(""" def shortInfo: String = s"created=$x" """)
    val interp = tree.children(2)
    val id = interp.children(0)
    assert(id.tokens.show[Structure] == "Tokens(s [25..26))")
  }
  test("#331") {
    val tree = caseClause("foo if bar || baz => ")
    val cond = tree.children(1)
    assert(cond.tokens.show[Structure] == "Tokens(bar [7..10),   [10..11), || [11..13),   [13..14), baz [14..17))")
  }
  test("a + b + c") {
    val tree = term("a + b + c")
    assert(tree.show[Positions] === """
      |Term.ApplyInfix{0..9}(
        |Term.ApplyInfix{0..5}(
          |Term.Name{0..1}("a"),
          | Term.Name{2..3}("+"),
          | Nil,
          | List(Term.Name{4..5}("b"))),
        | Term.Name{6..7}("+"),
        | Nil,
        | List(Term.Name{8..9}("c")))
    """.trim.stripMargin.split("\n").mkString)
  }
}

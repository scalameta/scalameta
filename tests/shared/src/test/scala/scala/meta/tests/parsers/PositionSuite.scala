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
        |Lit.Int{0..1}(1),
        | Term.Name{2..3}("+"),
        | Nil,
        | List(
          |Term.ApplyInfix{4..15}(
            |Term.ApplyInfix{4..11}(
              |Lit.Int{5..6}(2),
              | Term.Name{7..8}("/"),
              | Nil,
              | List(Lit.Int{9..10}(3))),
            | Term.Name{12..13}("*"),
            | Nil,
            | List(Lit.Int{14..15}(4)))))
    """.trim.stripMargin.split("\n").mkString)
  }
  test("(1 + 2).foo") {
    val tree = term("(1 + 2).foo")
    assert(tree.show[Positions] === """
      |Term.Select{0..11}(
        |Term.ApplyInfix{0..7}(
          |Lit.Int{1..2}(1),
          | Term.Name{3..4}("+"),
          | Nil,
          | List(Lit.Int{5..6}(2))),
        | Term.Name{8..11}("foo"))
    """.trim.stripMargin.split("\n").mkString)
  }
  test("#333") {
    val tree = blockStat(""" def shortInfo: String = s"created=$x" """)
    val interp = tree.children(2)
    val id = interp.children(0)
    assert(id.tokens.structure == "Tokens(s [25..26))")
  }
  test("#331") {
    val tree = caseClause("foo if bar || baz => ")
    val cond = tree.children(1)
    assert(cond.tokens.structure == "Tokens(bar [7..10),   [10..11), || [11..13),   [13..14), baz [14..17))")
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

  test("apply(startLine,startColumn,endLine,endColumn)") {
    val input = Input.String(
      """|  val x = 2 // line 0
         |
         |            // line 2""".stripMargin
    )
    val x = Position.Range(input, 0, 2, 0, 11)
    assert(x.text == "val x = 2")
    val x2 = Position.Range(input, 0, 2, 0, Int.MaxValue)
    assert(x2.text == "val x = 2 // line 0")
    val empty = Position.Range(input, 1, 0, 1, Int.MaxValue)
    assert(empty.text == "")
    val last = Position.Range(input, 2, 0, 2, Int.MaxValue)
    assert(last.text == "            // line 2")
  }

}

package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._

class PositionSuite extends ParseSuite {
  test("1 + (2 / 3) * 4") {
    val tree = term("1 + (2 / 3) * 4")
    assert(tree.show[Positions] === """
      |Term.ApplyInfix[0..15](
        |Lit[0..1](1),
        | Term.Name[2..3]("+"),
        | Nil,
        | List(
          |Term.ApplyInfix[4..15](
            |Term.ApplyInfix[4..11](
              |Lit[5..6](2),
              | Term.Name[7..8]("/"),
              | Nil,
              | List(Lit[9..10](3))),
            | Term.Name[12..13]("*"),
            | Nil,
            | List(Lit[14..15](4)))))
    """.trim.stripMargin.split("\n").mkString)
  }
  test("(1 + 2).foo") {
    val tree = term("(1 + 2).foo")
    assert(tree.show[Positions] === """
      |Term.Select[0..11](
        |Term.ApplyInfix[0..7](
          |Lit[1..2](1),
          | Term.Name[3..4]("+"),
          | Nil,
          | List(Lit[5..6](2))),
        | Term.Name[8..11]("foo"))
    """.trim.stripMargin.split("\n").mkString)
  }
}

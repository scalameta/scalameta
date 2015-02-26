import scala.meta._
import scala.meta.dialects.Scala211

class PositionSuite extends ParseSuite {
  test("1 + (2 / 3) * 4") {
    val tree = term("1 + (2 / 3) * 4")
    assert(tree.show[Positions] === """
      |Term.ApplyInfix[0..14](
        |Lit.Int[0..0](1),
        | Term.Name[2..2]("+"),
        | Nil,
        | List(
          |Term.ApplyInfix[4..14](
            |Term.ApplyInfix[4..10](
              |Lit.Int[5..5](2),
              | Term.Name[7..7]("/"),
              | Nil,
              | List(Lit.Int[9..9](3))),
            | Term.Name[12..12]("*"),
            | Nil,
            | List(Lit.Int[14..14](4)))))
    """.trim.stripMargin.split("\n").mkString)
  }
  test("(1 + 2).foo") {
    val tree = term("(1 + 2).foo")
    assert(tree.show[Positions] === """
      |Term.Select[0..10](
        |Term.ApplyInfix[0..6](
          |Lit.Int[1..1](1),
          | Term.Name[3..3]("+"),
          | Nil,
          | List(Lit.Int[5..5](2))),
        | Term.Name[8..10]("foo"))
    """.trim.stripMargin.split("\n").mkString)
  }
}

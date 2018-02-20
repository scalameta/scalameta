package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.prettyprinters._

class PositionSuite extends ParseSuite {
  private implicit class TrimPosOps(in: String) {
    def trimPos: String = in.trim.stripMargin.split("\n").mkString
  }

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
    """.trimPos)
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
    """.trimPos)
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
    """.trimPos)
  }

  test("#1345 - Tree.transform does not preserve parent chain origins") {
    val original = 
      """|object a {
         |  def bar = 4
         |  // comment
         |  def foo = 2
         |}""".stripMargin.parse[Source].get

    val originalPositions = """
      |Source{0..53}(
        |List(
          |Defn.Object{0..53}(
            |Nil,
            | Term.Name{7..8}("a"),
            | Template{9..53}(
              |Nil,
              | Nil,
              | Self{13..13}(
                  |Name.Anonymous{13..13}(),
                  | None),
              | List(
                |Defn.Def{13..24}(
                |Nil,
                | Term.Name{17..20}("bar"),
                | Nil,
                | Nil,
                | None,
                | Lit.Int{23..24}(4)
                |), 
                |Defn.Def{40..51}(
                  |Nil,
                  | Term.Name{44..47}("foo"),
                  | Nil,
                  | Nil,
                  | None,
                  | Lit.Int{50..51}(2)))))))""".trimPos
    
    assert(original.show[Positions] === originalPositions)

    val refactored = original.transform { case q"2" => q"3" }

    val refactoredPositions = """
      |Source{0..53}(
        |List(
          |Defn.Object{0..53}(
            |Nil,
            | Term.Name{7..8}("a"),
            | Template{9..53}(
              |Nil,
              | Nil,
              | Self{13..13}(
                  |Name.Anonymous{13..13}(),
                  | None),
              | List(
                |Defn.Def{13..24}(
                |Nil,
                | Term.Name{17..20}("bar"),
                | Nil,
                | Nil,
                | None,
                | Lit.Int{23..24}(4)
                |), 
                |Defn.Def{40..51}(
                  |Nil,
                  | Term.Name{44..47}("foo"),
                  | Nil,
                  | Nil,
                  | None,
                  | Lit.Int(3)))))))""".trimPos

    assert(refactored.show[Positions] === refactoredPositions)
  }

  test("#1350 - Tree.copy does not preserve the origin") {
    import scala.meta._
    val code ="object A { }".parse[Source].get
    val codePositions = """
      |Source{0..12}(
        |List(Defn.Object{0..12}(
          |Nil, 
          |Term.Name{7..8}("A"), 
          |Template{9..12}(
            |Nil, 
            |Nil, 
            |Self{11..11}(
              |Name.Anonymous{11..11}(), 
              |None
            |), 
            |Nil))))""".trimPos

    assert(code.show[Positions] === codePositions)

    val refactored = code.copy(stats = Nil)
    val refactoredPositions = """Source{0..12}(Nil)"""
    assert(refactored.show[Positions] === refactoredPositions)
  }
}

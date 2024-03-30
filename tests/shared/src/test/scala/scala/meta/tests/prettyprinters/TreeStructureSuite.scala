package scala.meta.tests.prettyprinters

import munit.Location
import org.scalameta.logger

import scala.meta._
import scala.meta.tests.parsers.ParseSuite

class TreeStructureSuite extends ParseSuite {

  def assertStructure(tree: Tree)(expected: String)(implicit loc: Location): Unit = {
    test(logger.revealWhitespace(tree.syntax)) {
      assertNoDiff(
        tree.structure,
        expected.stripMargin.replaceAll("[,]\n *", ", ").replaceAll("\n *", "")
      )
    }
  }

  assertStructure(Lit.Unit())("Lit.Unit()")
  assertStructure(Lit.Float(".0f"))("Lit.Float(0f)")
  assertStructure(Lit.Float("0.00f"))("Lit.Float(0f)")
  assertStructure(Lit.Float("1230f"))("Lit.Float(1230f)")
  assertStructure(Lit.Float("12.30f"))("Lit.Float(12.3f)")
  assertStructure(Lit.Double(".0d"))("Lit.Double(0d)")
  assertStructure(Lit.Double("0.00d"))("Lit.Double(0d)")
  assertStructure(Lit.Double("1230d"))("Lit.Double(1230d)")
  assertStructure(Lit.Double("12.30d"))("Lit.Double(12.3d)")
  assertStructure(Lit.Long(1230))("Lit.Long(1230L)")
  assertStructure(int(1230))("Lit.Int(1230)")
  assertStructure(Lit.Null())("Lit.Null()")
  assertStructure(bool(false))("Lit.Boolean(false)")
  assertStructure(bool(true))("Lit.Boolean(true)")
  assertStructure(str("lit.str"))("""Lit.String("lit.str")""")

  assertStructure(
    Case(
      Pat.Wildcard(),
      None,
      Term.Function(List(tparam("")), bool(false))
    )
  )(
    """|
       |Case(
       |  Pat.Wildcard(),
       |  None,
       |  Term.Function(
       |    Term.ParamClause(List(Term.Param(Nil, Name.Anonymous(), None, None)), None),
       |    Lit.Boolean(false)
       |  ))
       |""".stripMargin
  )

  assertStructure(
    Case(
      Pat.Wildcard(),
      None,
      Term.Function(List(tparam("")), Term.Block(List(bool(false))))
    )
  )(
    """|
       |Case(
       |  Pat.Wildcard(),
       |  None,
       |  Term.Function(
       |    Term.ParamClause(List(Term.Param(Nil, Name.Anonymous(), None, None)), None),
       |    Term.Block(List(Lit.Boolean(false)))
       |  ))
       |""".stripMargin
  )

  assertStructure(
    Case(
      Pat.Wildcard(),
      None,
      Term.Function(List(tparam("")), Term.Block(List(bool(false), tname("a"))))
    )
  )(
    """|
       |Case(
       |  Pat.Wildcard(),
       |  None,
       |  Term.Function(
       |    Term.ParamClause(List(Term.Param(Nil, Name.Anonymous(), None, None)), None),
       |    Term.Block(List(Lit.Boolean(false), Term.Name("a")))
       |  ))""".stripMargin
  )

}

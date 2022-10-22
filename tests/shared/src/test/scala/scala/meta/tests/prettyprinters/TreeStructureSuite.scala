package scala.meta.tests.prettyprinters

import org.scalameta.logger

import scala.meta._
import scala.meta.tests.parsers.ParseSuite

class TreeStructureSuite extends ParseSuite {

  def assertStructure(tree: Tree)(expected: String): Unit = {
    test(logger.revealWhitespace(tree.syntax)) {
      assertNoDiff(
        tree.structure,
        expected.stripMargin.trim.replaceAll("[,]\n *", ", ").replaceAll("\n *", "")
      )
    }
  }

  assertStructure(Lit.Unit())("Lit.Unit()")
  assertStructure(Lit.Float("12.30f"))("Lit.Float(12.30f)")
  assertStructure(Lit.Double("12.30d"))("Lit.Double(12.30d)")
  assertStructure(Lit.Long(1230))("Lit.Long(1230L)")
  assertStructure(Lit.Int(1230))("Lit.Int(1230)")
  assertStructure(Lit.Null())("Lit.Null(null)")
  assertStructure(Lit.Boolean(false))("Lit.Boolean(false)")
  assertStructure(Lit.Boolean(true))("Lit.Boolean(true)")
  assertStructure(Lit.String("lit.str"))("""Lit.String("lit.str")""")

  assertStructure(
    Case(
      Pat.Wildcard(),
      None,
      Term.Function(List(Term.Param(Nil, Name(""), None, None)), Lit.Boolean(false))
    )
  )("""
      |Case(
      |  Pat.Wildcard(),
      |  None,
      |  Term.Function(
      |    List(Term.Param(Nil, Name(""), None, None)),
      |    Lit.Boolean(false)
      |  ))
      |""".stripMargin)

  assertStructure(
    Case(
      Pat.Wildcard(),
      None,
      Term.Function(
        List(Term.Param(Nil, Name(""), None, None)),
        Term.Block(List(Lit.Boolean(false)))
      )
    )
  )("""
      |Case(
      |  Pat.Wildcard(),
      |  None,
      |  Term.Function(
      |    List(Term.Param(Nil, Name(""), None, None)),
      |    Term.Block(List(Lit.Boolean(false)))
      |  ))
      |""".stripMargin)

  assertStructure(
    Case(
      Pat.Wildcard(),
      None,
      Term.Function(
        List(Term.Param(Nil, Name(""), None, None)),
        Term.Block(List(Lit.Boolean(false), Term.Name("a")))
      )
    )
  )(
    """
      |Case(
      |  Pat.Wildcard(),
      |  None,
      |  Term.Function(
      |    List(Term.Param(Nil, Name(""), None, None)),
      |    Term.Block(List(Lit.Boolean(false), Term.Name("a")))
      |  ))""".stripMargin
  )

}

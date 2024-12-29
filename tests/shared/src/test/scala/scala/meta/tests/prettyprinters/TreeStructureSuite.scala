package scala.meta.tests.prettyprinters

import org.scalameta.logger
import scala.meta._
import scala.meta.tests.parsers.ParseSuite

import munit.Location

class TreeStructureSuite extends ParseSuite {

  def assertStructure(tree: Tree)(expected: String)(implicit loc: Location): Unit =
    test(logger.revealWhitespace(tree.syntax))(assertNoDiff(tree.structure, expected))

  assertStructure(Lit.Unit())("Lit.Unit()")
  assertStructure(flt(".0f"))("Lit.Float(0f)")
  assertStructure(flt("0.00f"))("Lit.Float(0f)")
  assertStructure(flt("1230f"))("Lit.Float(1230f)")
  assertStructure(flt("12.30f"))("Lit.Float(12.3f)")
  assertStructure(dbl(".0d"))("Lit.Double(0d)")
  assertStructure(dbl("0.00d"))("Lit.Double(0d)")
  assertStructure(dbl("1230d"))("Lit.Double(1230d)")
  assertStructure(dbl("12.30d"))("Lit.Double(12.3d)")
  assertStructure(lit(1230L))("Lit.Long(1230L)")
  assertStructure(int(1230))("Lit.Int(1230)")
  assertStructure(Lit.Null())("Lit.Null()")
  assertStructure(bool(false))("Lit.Boolean(false)")
  assertStructure(bool(true))("Lit.Boolean(true)")
  assertStructure(str("lit.str"))("""Lit.String("lit.str")""")
  assertStructure(lit('a'))("Lit.Char('a')")
  assertStructure(sym("a"))("""Lit.Symbol(Symbol("a"))""")

  assertStructure(Case(patwildcard, None, tfunc(tparam(""))(bool(false))))(
    """|Case(
       |  Pat.Wildcard(),
       |  None,
       |  Term.Function(
       |    Term.ParamClause(List(
       |      Term.Param(
       |        Nil,
       |        Name.Anonymous(),
       |        None,
       |        None
       |      )
       |    )),
       |    Lit.Boolean(false)
       |  )
       |)
       |""".stripMargin
  )

  assertStructure(Case(patwildcard, None, tfunc(tparam(""))(blk(bool(false)))))(
    """|Case(
       |  Pat.Wildcard(),
       |  None,
       |  Term.Function(
       |    Term.ParamClause(List(
       |      Term.Param(
       |        Nil,
       |        Name.Anonymous(),
       |        None,
       |        None
       |      )
       |    )),
       |    Term.Block(List(
       |      Lit.Boolean(false)
       |    ))
       |  )
       |)
       |""".stripMargin
  )

  assertStructure(Case(patwildcard, None, tfunc(tparam(""))(blk(bool(false), tname("a")))))(
    """|Case(
       |  Pat.Wildcard(),
       |  None,
       |  Term.Function(
       |    Term.ParamClause(List(
       |      Term.Param(
       |        Nil,
       |        Name.Anonymous(),
       |        None,
       |        None
       |      )
       |    )),
       |    Term.Block(List(
       |      Lit.Boolean(false),
       |      Term.Name("a")
       |    ))
       |  )
       |)
       |""".stripMargin
  )

}

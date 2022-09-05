package scala.meta.tests.parsers

import scala.compat.Platform.EOL
import scala.meta._

import munit._

import scala.meta.tests.TreeSuiteBase

class ToplevelTermSuite extends TreeSuiteBase {
  implicit val dialect: Dialect = dialects.Scala3.withAllowToplevelTerms(true)
  test("allowToplevelTerms simple") {

    val sourceString = """def foo(x: Int): Int = x
                          |foo(x)
                          |""".trim.stripMargin

    val tree = sourceString.parse[Source].get

    assertEquals(tree.syntax, sourceString)
    val expected =
      Source(
        List(
          Defn.Def(
            Nil,
            Term.Name("foo"),
            List(Clause.TermClause(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)))),
            Some(Type.Name("Int")),
            Term.Name("x")
          ),
          Term.Apply(Term.Name("foo"), List(Term.Name("x")))
        )
      )
    assertTree(tree)(expected)
  }

  test("allowToplevelTerms and allowPackageStatementsWithToplevelTerms with package") {

    val sourceString = """package bar
                         |def foo(x: Int): Int = x
                         |foo(x)
                         |""".trim.stripMargin

    val tree = sourceString.parse[Source].get

    assertEquals(tree.syntax, sourceString)
    val expected =
      Source(
        List(
          Pkg(
            Term.Name("bar"),
            List(
              Defn.Def(
                Nil,
                Term.Name("foo"),
                List(Clause.TermClause(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)))),
                Some(Type.Name("Int")),
                Term.Name("x")
              ),
              Term.Apply(Term.Name("foo"), List(Term.Name("x")))
            )
          )
        )
      )
    assertTree(tree)(expected)
  }

  test("allowToplevelTerms and allowPackageStatementsWithToplevelTerms no package") {
    val sourceString = """def foo(x: Int): Int = x
                         |foo(x)
                         |""".trim.stripMargin

    val tree = sourceString.parse[Source].get

    assertEquals(tree.syntax, sourceString)
    val expected = Source(
      List(
        Defn.Def(
          Nil,
          Term.Name("foo"),
          List(Clause.TermClause(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)))),
          Some(Type.Name("Int")),
          Term.Name("x")
        ),
        Term.Apply(Term.Name("foo"), List(Term.Name("x")))
      )
    )
    assertTree(tree)(expected)
  }

}

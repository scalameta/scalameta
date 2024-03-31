package scala.meta.tests.parsers

import scala.meta._
import scala.meta.tests.TreeSuiteBase

class ToplevelTermSuite extends TreeSuiteBase {
  implicit val dialect: Dialect = dialects.Scala3.withAllowToplevelTerms(true)
  test("allowToplevelTerms simple") {

    val sourceString = """def foo(x: Int): Int = x
                         |foo(x)
                         |""".stripMargin

    val tree = sourceString.parse[Source].get

    assertEquals(tree.syntax, sourceString)
    val expected = Source(List(
      Defn
        .Def(Nil, tname("foo"), Nil, List(List(tparam("x", "Int"))), Some(pname("Int")), tname("x")),
      Term.Apply(tname("foo"), List(tname("x")))
    ))
    assertTree(tree)(expected)
  }

  test("allowToplevelTerms and allowPackageStatementsWithToplevelTerms with package") {

    val sourceString = """package bar
                         |def foo(x: Int): Int = x
                         |foo(x)
                         |""".stripMargin

    val tree = sourceString.parse[Source].get

    assertEquals(tree.syntax, sourceString)
    val expected = Source(List(Pkg(
      tname("bar"),
      List(
        Defn
          .Def(Nil, tname("foo"), Nil, List(List(tparam("x", "Int"))), Some(pname("Int")), tname("x")),
        Term.Apply(tname("foo"), List(tname("x")))
      )
    )))
    assertTree(tree)(expected)
  }

  test("allowToplevelTerms and allowPackageStatementsWithToplevelTerms no package") {
    val sourceString = """def foo(x: Int): Int = x
                         |foo(x)
                         |""".stripMargin

    val tree = sourceString.parse[Source].get

    assertEquals(tree.syntax, sourceString)
    val expected = Source(List(
      Defn
        .Def(Nil, tname("foo"), Nil, List(List(tparam("x", "Int"))), Some(pname("Int")), tname("x")),
      Term.Apply(tname("foo"), List(tname("x")))
    ))
    assertTree(tree)(expected)
  }

}

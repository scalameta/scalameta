package scala.meta.tests
package parsers

import scala.meta._

class QuasiquoteSuite extends ParseSuite {
  locally {
    implicit val dialect = dialects.Scala211.unquoteTerm(multiline = false)

    test("single-line disallow normal escaping")(assertTree(term("\\n"))(tpostfix("\\", "n")))

    test("single-line allow unicode escaping")(assertTree(term("\\u0061"))(tname("a")))

    test("single-line disallow line breaks")(
      interceptMessage[ParseException](
        """|<input>:1: error: line breaks are not allowed in single-line quasiquotes
           |foo + 
           |      ^""".stripMargin.lf2nl,
      )(term("foo + \n bar")),
    )

    test("single-line disallow double quote strings")(
      interceptMessage[ParseException](
        """|<input>:1: error: double quotes are not allowed in single-line quasiquotes
           |"a"
           |^""".stripMargin.lf2nl,
      )(term("\"a\"")),
    )

    test("single-line disallow double quote interpolations")(
      interceptMessage[ParseException](
        """|<input>:1: error: double quotes are not allowed in single-line quasiquotes
           |s"a"
           | ^""".stripMargin.lf2nl,
      )(term("s\"a\"")),
    )

    test("single-line disallow char literal unquote")(
      interceptMessage[ParseException](
        """|<input>:1: error: can't unquote into character literals
           | '$x' 
           |  ^""".stripMargin.lf2nl,
      )(term(" '$x' ")),
    )
  }

  locally {
    implicit val dialect = dialects.Scala211.unquoteTerm(multiline = true)

    test("multi-line disallow do normal escaping")(assertTree(term("\\n"))(tpostfix("\\", "n")))

    test("multi-line allow unicode escaping")(assertTree(term("\\u0061"))(tname("a")))

    test("multi-line allow line breaks")(
      assertTree(term("foo + \n bar"))(tinfix(tname("foo"), "+", tname("bar"))),
    )

    test("multi-line allow double quotes")(assertTree(term("\"a\""))(str("a")))

    test("multi-line disallow single-line unquote")(
      interceptMessage[ParseException](
        """|<input>:2: error: can't unquote into string literals
           |" $x "
           |  ^""".stripMargin.lf2nl,
      )(term(
        """|
           |" $x "
           |""".stripMargin,
      )),
    )

    test("multi-line disallow multi-line unquote")(
      interceptMessage[ParseException](
        """|<input>:2: error: can't unquote into multi-line string literals
           |QQQ $x QQQ
           |    ^""".stripMargin.tq().lf2nl,
      )(term(
        """|
           |QQQ $x QQQ
           |""".stripMargin.tq(),
      )),
    )
  }

  locally {
    implicit val dialect = dialects.Scala3.unquoteTerm(multiline = true)

    test("unquote colon, lambda on the same line")(assertTree(term("$f: y => y"))(
      tapply(Term.Quasi(0, tname("f")), blk(tfunc(tparam("y"))(tname("y")))),
    ))

    test("refinement: type alias, unquote colon, declaration on the next line") {
      val code =
        """|type T = $e:
           |  def f: Int
           |""".stripMargin
      val tree = Defn.Type(
        Nil,
        pname("T"),
        Nil,
        Type.Refine(Some(Type.Quasi(0, tname("e"))), List(Decl.Def(Nil, tname("f"), Nil, pname("Int")))),
        noBounds,
      )
      assertTree(stat(code))(tree)
    }

    test("case guard, unquote colon, name on the next line") {
      val code =
        """|x match {
           |  case y if $p:
           |      z
           |    => 1
           |}
           |""".stripMargin
      val guard = tapply(Term.Quasi(0, tname("p")), blk(tname("z")))
      assertTree(term(code))(tmatch(tname("x"), Case(patvar("y"), Some(guard), int(1))))
    }
  }
}

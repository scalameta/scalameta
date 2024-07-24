package scala.meta.tests
package parsers

import scala.meta._

class QuasiquoteSuite extends ParseSuite {
  locally {
    implicit val dialect = dialects.Scala211.unquoteTerm(multiline = false)

    test("single-line disallow normal escaping") {
      assertTree(term("\\n"))(Term.Select(tname("\\"), tname("n")))
    }

    test("single-line allow unicode escaping")(assertTree(term("\\u0061"))(tname("a")))

    test("single-line disallow line breaks")(
      interceptMessage[ParseException](
        """|<input>:1: error: line breaks are not allowed in single-line quasiquotes
           |foo + 
           |      ^""".stripMargin.lf2nl
      )(term("foo + \n bar"))
    )

    test("single-line disallow double quote strings")(
      interceptMessage[ParseException](
        """|<input>:1: error: double quotes are not allowed in single-line quasiquotes
           |"a"
           |^""".stripMargin.lf2nl
      )(term("\"a\""))
    )

    test("single-line disallow double quote interpolations")(
      interceptMessage[ParseException](
        """|<input>:1: error: double quotes are not allowed in single-line quasiquotes
           |s"a"
           | ^""".stripMargin.lf2nl
      )(term("s\"a\""))
    )

    test("single-line disallow char literal unquote")(
      interceptMessage[ParseException](
        """|<input>:1: error: can't unquote into character literals
           | '$x' 
           |  ^""".stripMargin.lf2nl
      )(term(" '$x' "))
    )
  }

  locally {
    implicit val dialect = dialects.Scala211.unquoteTerm(multiline = true)

    test("multi-line disallow do normal escaping") {
      assertTree(term("\\n"))(Term.Select(tname("\\"), tname("n")))
    }

    test("multi-line allow unicode escaping")(assertTree(term("\\u0061"))(tname("a")))

    test("multi-line allow line breaks") {
      assertTree(term("foo + \n bar"))(
        Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar")))
      )
    }

    test("multi-line allow double quotes")(assertTree(term("\"a\""))(str("a")))

    test("multi-line disallow single-line unquote") {
      interceptMessage[ParseException](
        """|<input>:2: error: can't unquote into string literals
           |" $x "
           |  ^""".stripMargin.lf2nl
      )(term(
        """|
           |" $x "
           |""".stripMargin
      ))
    }

    test("multi-line disallow multi-line unquote") {
      interceptMessage[ParseException](
        """|<input>:2: error: can't unquote into multi-line string literals
           |QQQ $x QQQ
           |    ^""".stripMargin.tq().lf2nl
      )(term(
        """|
           |QQQ $x QQQ
           |""".stripMargin.tq()
      ))
    }
  }
}

package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.parsers.ParseException

class UnclosedTokenSuite extends ParseSuite {

  implicit val dialect: Dialect = dialects.Scala211

  test("unclosed-string-1") {
    interceptMessage[ParseException](
      """|<input>:1: error: unclosed single-line string interpolation
         | s"start   
         |           ^""".stripMargin.lf2nl
    )(stat(""" s"start   """))
  }

  test("unclosed-string-2") {
    interceptMessage[ParseException](
      """|<input>:1: error: unclosed string literal
         | x"${1 + " 
         |         ^""".stripMargin.lf2nl
    )(stat(""" x"${1 + " """))
  }

  test("unclosed-escape") {
    interceptMessage[ParseException](
      """|<input>:1: error: unclosed string literal
         | "start \" 
         | ^""".stripMargin.lf2nl
    )(stat(""" "start \" """))
  }

  test("unclosed-interpolation") {
    interceptMessage[ParseException](
      """|<input>:1: error: `}` expected but `end of file` found
         | s"${1+ 
         |        ^""".stripMargin.lf2nl
    )(stat(""" s"${1+ """))
  }

  test("unclosed-char") {
    interceptMessage[ParseException](
      """|<input>:1: error: unclosed character literal
         | '.,
         |   ^""".stripMargin.lf2nl
    )(stat(
      """| '.,
         |""".stripMargin
    ))
  }

  test("unclosed-char-with-NL") {
    interceptMessage[ParseException](
      """|<input>:1: error: can't use unescaped LF in character literals
         | '
         |  ^""".stripMargin.lf2nl
    )(stat(
      """| '
         |abc
         |""".stripMargin
    ))
  }

  test("unclosed-multi-string-literal") {
    interceptMessage[ParseException](
      s"""|<input>:4: error: unclosed multi-line string literal
          |
          |^""".stripMargin.lf2nl
    )(stat(
      s"""|""\"
          |foo
          |""
          |""".stripMargin
    ))
  }

  test("unclosed-comment") {
    interceptMessage[ParseException](
      """|<input>:2: error: unclosed comment
         | * foo
         |      ^""".stripMargin.lf2nl
    )(stat(
      """|/*
         | * foo
         |""".stripMargin
    ))
  }

}

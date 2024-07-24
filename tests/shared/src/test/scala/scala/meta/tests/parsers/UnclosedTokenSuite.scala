package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.parsers.ParseException

class UnclosedTokenSuite extends ParseSuite {
  test("unclosed-string-1") {
    interceptMessage[TokenizeException](
      """|<input>:1: error: unclosed string interpolation
         | s"start   
         |   ^""".stripMargin.lf2nl
    )(stat(""" s"start   """))
  }

  test("unclosed-string-2") {
    interceptMessage[TokenizeException](
      """|<input>:1: error: unclosed string literal
         | x"${1 + " 
         |         ^""".stripMargin.lf2nl
    )(stat(""" x"${1 + " """))
  }

  test("unclosed-escape") {
    interceptMessage[TokenizeException](
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
    interceptMessage[TokenizeException](
      """|<input>:1: error: unclosed character literal
         | '.,
         | ^""".stripMargin.lf2nl
    )(stat(
      """| '.,
         |""".stripMargin
    ))
  }

  test("unclosed-char-with-NL") {
    interceptMessage[TokenizeException](
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
    interceptMessage[TokenizeException](
      s"""|<input>:1: error: unclosed multi-line string literal
          |""\"
          |^""".stripMargin.lf2nl
    )(stat(
      s"""|""\"
          |foo
          |""
          |""".stripMargin
    ))
  }

  test("unclosed-comment") {
    interceptMessage[TokenizeException](
      """|<input>:1: error: unclosed comment
         |/*
         |^""".stripMargin.lf2nl
    )(stat(
      """|/*
         | * foo
         |""".stripMargin
    ))
  }

}

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
         |   ^""".stripMargin.replace("\n", EOL)
    )(stat(""" s"start   """))
  }

  test("unclosed-string-2") {
    interceptMessage[TokenizeException](
      """|<input>:1: error: unclosed string literal
         | x"${1 + " 
         |         ^""".stripMargin.replace("\n", EOL)
    )(stat(""" x"${1 + " """))
  }

  test("unclosed-escape") {
    interceptMessage[TokenizeException](
      """|<input>:1: error: unclosed string literal
         | "start \" 
         | ^""".stripMargin.replace("\n", EOL)
    )(stat(""" "start \" """))
  }

  test("unclosed-interpolation") {
    interceptMessage[ParseException](
      """|<input>:1: error: `}` expected but `end of file` found
         | s"${1+ 
         |        ^""".stripMargin.replace("\n", EOL)
    )(stat(""" s"${1+ """))
  }

}

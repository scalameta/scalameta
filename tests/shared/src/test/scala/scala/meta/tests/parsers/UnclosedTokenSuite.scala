package scala.meta.tests
package parsers

import scala.meta._
import Term.{Super, Name => TermName}
import Type.{Name => TypeName, _}
import Name.Anonymous
import scala.meta.dialects.Scala211
import scala.meta.parsers.ParseException
import scala.meta.internal.tokenizers.ScalametaTokenizer

class UnclosedTokenSuite extends ParseSuite {
  test("unclosed-string-1") {
    val e = intercept[TokenizeException] {
      stat(""" s"start   """)
    }
    assert(e.getMessage.contains("unclosed string interpolation"))
  }

  test("unclosed-string-2") {
    val e = intercept[TokenizeException] {
      stat(""" x"${1 + " """)
    }
    assert(e.getMessage.contains("unclosed string literal"))
  }

  test("unclosed-escape") {
    val e = intercept[TokenizeException] {
      stat(""" "start \" """)
    }
  }

  test("unclosed-interpolation") {
    val e = intercept[ParseException] {
      stat(""" s"${1+ """)
    }
    assert(e.getMessage.contains("expected but end of file found"))
  }

}

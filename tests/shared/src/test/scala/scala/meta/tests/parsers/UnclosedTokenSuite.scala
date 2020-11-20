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
  test("unclosed-string") {
    val e = intercept[TokenizeException] {
      stat(""" s"start   """)
    }
    assert(e.getMessage().contains("unclosed string interpolation"))
  }

  test("unclosed-interp") {
    val e = intercept[TokenizeException] {
      stat(""" s"${1+ """)
    }
    assert(e.getMessage().contains("unclosed slice interpolation"))
  }

}

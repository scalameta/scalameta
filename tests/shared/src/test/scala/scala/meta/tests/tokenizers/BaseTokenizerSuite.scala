package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.dialects.Scala211
import org.scalatest.FunSuite

abstract class BaseTokenizerSuite extends FunSuite {

  def tokenize(code: String): Tokens = {
    val convert = scala.meta.inputs.Input.stringToInput
    val tokenize = scala.meta.tokenizers.Tokenize.scalametaTokenize
    val dialect = Scala211
    code.tokenize(convert, tokenize, dialect).get
  }

}

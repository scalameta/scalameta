import org.scalatest.FunSuite

abstract class TokenQuasiquoteSuite extends FunSuite {

  import scala.meta.{ Token, Tokens }
  import scala.meta.Token._

  implicit class CheckToken(t: Token) {
    def isIdentNamed(name: String): Boolean = t match {
      case x: Ident => x.code == name
      case _        => false
    }

    def isMinus: Boolean = t isIdentNamed "-"

    def isIntLit(expected: Int): Boolean = t match {
      case t: Literal.Int => t.value.toInt == expected
      case _              => false
    }

    def isWhitespace: Boolean = t.isInstanceOf[Whitespace]

    def isComma: Boolean = t.isInstanceOf[`,`]
  }

  implicit class checkSingleTokenQuasiquote(token: Token) extends CheckTokenQuasiquote(Tokens(token))

  implicit class CheckTokenQuasiquote(tokens: Tokens) {

    def stripWhitespaces = tokens filterNot (_.isWhitespace)

    def shouldConformTo(predicates: (Token => Boolean)*) {
      assert(tokens.length == predicates.length, s"Received ${tokens.length} tokens, but only ${predicates.length} predicates.")

      tokens zip predicates foreach {
        case (t, p) => assert(p(t), s"Token $t didn't satisfy its predicate!")
      }
    }

  }

}

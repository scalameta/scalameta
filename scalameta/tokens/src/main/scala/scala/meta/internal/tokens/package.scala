package scala.meta
package internal

import scala.meta.inputs.Content
import scala.meta.tokens.Token

package object tokens {
  implicit class XtensionAdjust(token: Token) {
    def adjust(content: Content = token.content, dialect: Dialect = token.dialect, delta: Int = 0): Token = {
      // TODO: This is very ugly. We need to find way to enable GP for tokens and trees.
      val copy = token.getClass.getDeclaredMethods().find(_.getName == "copy").get
      val params = copy.getParameterTypes

      val boilerplate = scala.collection.mutable.ListBuffer[Any]()
      val payload = scala.collection.mutable.ListBuffer[Any]()
      def args = boilerplate ++ payload
      boilerplate += content
      boilerplate += dialect
      token match {
        case Token.Ident(value) => payload += value
        case Token.Literal(constant) => payload += constant
        case Token.Interpolation.Part(value) => payload += value
        case Token.Xml.Part(value) => payload += value
        case Token.Ellipsis(rank) => payload += rank
        case Token.Unquote(tree) => payload += tree
        case _ => // no token-specific payload
      }
      if (args.length < params.length) boilerplate += (token.start + delta)
      if (args.length < params.length) boilerplate += (token.end + delta)

      def informativeFail() = sys.error(s"params = ${params.toList}, args = ${args.toList}")
      try {
        if (args.length != params.length) informativeFail()
        copy.invoke(token, args.toList.asInstanceOf[List[AnyRef]].toArray: _*).asInstanceOf[Token]
      } catch {
        case ex: IllegalArgumentException if ex.getMessage == "argument type mismatch" =>
          informativeFail()
      }
    }
  }
}
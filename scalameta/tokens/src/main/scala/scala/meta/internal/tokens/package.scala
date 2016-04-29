package scala.meta
package internal

import scala.meta.inputs.Input
import scala.meta.tokens.Token

package object tokens {
  implicit class XtensionTokenAdjust(token: Token) {
    def adjust(input: Input = token.input, dialect: Dialect = token.dialect, delta: Int = 0): Token = {
      // TODO: This is very ugly. We need to find way to enable GP for tokens and trees.
      // Luckily, this ugliness is only used internally - inside quasiquoting macros.
      val copy = token.getClass.getDeclaredMethods().find(_.getName == "copy").get
      val params = copy.getParameterTypes

      val boilerplate = scala.collection.mutable.ListBuffer[Any]()
      val payload = scala.collection.mutable.ListBuffer[Any]()
      def args = boilerplate ++ payload
      boilerplate += input
      boilerplate += dialect
      token match {
        case Token.Ident(value) => payload += value
        case Token.Constant.Int(value) => payload += value
        case Token.Constant.Long(value) => payload += value
        case Token.Constant.Float(value) => payload += value
        case Token.Constant.Double(value) => payload += value
        case Token.Constant.Char(value) => payload += value
        case Token.Constant.Symbol(value) => payload += value
        case Token.Constant.String(value) => payload += value
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

  implicit class XtensionTokenName(token: Token) {
    def name: String = {
      import scala.reflect.runtime.{universe => ru}
      import scala.reflect.runtime.universe._
      val mirror = ru.runtimeMirror(classOf[Token].getClassLoader)
      val sym = mirror.classSymbol(token.getClass)
      val ann = sym.annotations.map(_.tree).find(_.tpe.typeSymbol == symbolOf[Metadata.tokenClass]).get
      val q"new $_(${name: String})" = ann
      name
    }
  }
}

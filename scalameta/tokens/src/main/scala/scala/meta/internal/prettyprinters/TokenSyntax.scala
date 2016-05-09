package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.tokens._
import scala.meta.tokens.Token._

object TokenSyntax {
  def apply[T <: Token](dialect: Dialect, options: Options): Syntax[T] = Syntax { x =>
    def failXml() = throw new UnsupportedOperationException(s"$dialect doesn't support XML literals")
    def failQuasiquote() = throw new UnsupportedOperationException(s"$dialect doesn't support unquoting")
    x match {
      case Xml.Start() if !dialect.allowXmlLiterals => failXml()
      case Xml.Part(_) if !dialect.allowXmlLiterals => failXml()
      case Xml.SpliceStart() if !dialect.allowXmlLiterals => failXml()
      case Xml.SpliceEnd() if !dialect.allowXmlLiterals => failXml()
      case Xml.End() if !dialect.allowXmlLiterals => failXml()
      case Unquote(_) if !dialect.allowUnquotes => failQuasiquote()
      case Ellipsis(_) if !dialect.allowEllipses => failQuasiquote()
      case _ => // do nothing, check passed
    }

    s(new String(x.input.chars, x.start, x.end - x.start))
  }
}
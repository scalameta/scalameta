package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{ sequence => s }
import scala.meta.tokens._
import scala.meta.tokens.Token._

object TokenSyntax {
  def apply[T <: Token](dialect: Dialect): Syntax[T] = Syntax { x =>
    def failXml() = throw new UnsupportedOperationException(s"$dialect doesn't support xml literals")
    def failQuasiquote() = throw new UnsupportedOperationException(s"$dialect doesn't support unquoting")
    def failViewBound() = throw new UnsupportedOperationException(s"$dialect doesn't support view bounds")
    x match {
      case Xml.Start() if !dialect.allowXmlLiterals => failXml()
      case Xml.Part(_) if !dialect.allowXmlLiterals => failXml()
      case Xml.SpliceStart() if !dialect.allowXmlLiterals => failXml()
      case Xml.SpliceEnd() if !dialect.allowXmlLiterals => failXml()
      case Xml.End() if !dialect.allowXmlLiterals => failXml()
      case Unquote() if !dialect.allowUnquotes => failQuasiquote()
      case Ellipsis(_) if !dialect.allowUnquotes => failQuasiquote()
      case Viewbound() if !dialect.allowViewBounds => failViewBound()
      case _ => // do nothing, check passed
    }
    s(x.text)
  }
}

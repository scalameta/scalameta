package scala.reflect
package syntactic

import scala.reflect.core._
import org.scalareflect.convert._

package object parsers {
  val keywords = Set(
    "abstract", "case", "do", "else", "finally", "for", "import", "lazy",
    "object", "override", "return", "sealed", "trait", "try", "var", "while",
    "catch", "class", "extends", "false", "forSome", "if", "match", "new",
    "package", "private", "super", "this", "true", "type", "with", "yield",
    "def", "final", "implicit", "null", "protected", "throw", "val", "_",
    ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
  )

  implicit val stringToSource: Convert[String, Source] = Convert.apply(Source.String(_))
  implicit val fileToSource: Convert[java.io.File, Source] = Convert.apply(Source.File(_))

  trait Parse[T] extends Convert[Source, T] { def apply(source: Source): T }
  object Parse {
    def apply[T](f: Source => T): Parse[T] = new Parse[T] { def apply(source: Source): T = f(source) }
    implicit val parseCompUnit: Parse[Aux.CompUnit] = apply(source => new Parser(source).parseTopLevel())
    implicit val parseTerm: Parse[Term] = apply(source => new Parser(source).parseTerm())
    implicit val parseType: Parse[Type] = apply(source => new Parser(source).parseType())
    implicit val parsePat: Parse[Pat] = apply(source => new Parser(source).parsePat())
    implicit val parseStats: Parse[List[Stmt.Template]] = apply(source => new Parser(source).parseStats())
  }

  implicit class RichSource[T](val sourceLike: T)(implicit ev: Convert[T, Source]) {
    private val source: Source = ev(sourceLike)
    def parse[T](implicit ev: Parse[T]): T = ev(source)
  }
}

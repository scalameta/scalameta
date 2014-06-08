package scala.reflect
package syntactic

import scala.reflect.core._

package object parsers {
  val keywords = Set(
    "abstract", "case", "do", "else", "finally", "for", "import", "lazy",
    "object", "override", "return", "sealed", "trait", "try", "var", "while",
    "catch", "class", "extends", "false", "forSome", "if", "match", "new",
    "package", "private", "super", "this", "true", "type", "with", "yield",
    "def", "final", "implicit", "null", "protected", "throw", "val", "_",
    ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
  )

  implicit class RichSource[T <% Source](val sourceLike: T) {
    private val source: Source = sourceLike
    def parse: Aux.CompUnit = new Parser(source).parseTopLevel()
    def parseTerm: Term = new Parser(source).parseTerm()
    def parseType: Type = new Parser(source).parseType()
    def parsePat: Pat = new Parser(source).parsePat()
    def parseStats: List[Stmt.Template] = new Parser(source).parseStats()
  }
}

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

  def parse(source: Source): Aux.CompUnit = new Parser(source).parseTopLevel()
  def parseTerm(source: Source): Term = new Parser(source).parseTerm()
  def parseType(source: Source): Type = new Parser(source).parseType()
  def parsePat(source: Source): Pat = new Parser(source).parsePat()
  def parseStats(source: Source): List[Stmt.Template] = new Parser(source).parseStats()
}

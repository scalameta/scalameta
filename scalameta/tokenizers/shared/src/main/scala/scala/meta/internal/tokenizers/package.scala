package scala.meta
package internal

package object tokenizers {
  type Offset = Int
  type LegacyToken = Int

  class UnexpectedInputEndException(val ltd: LegacyTokenData) extends Exception

  private val baseKeywords = Set(
    "abstract",
    "case",
    "do",
    "else",
    "finally",
    "for",
    "import",
    "lazy",
    "object",
    "override",
    "return",
    "sealed",
    "trait",
    "try",
    "var",
    "while",
    "catch",
    "class",
    "extends",
    "false",
    "forSome",
    "if",
    "macro",
    "match",
    "new",
    "package",
    "private",
    "super",
    "this",
    "true",
    "type",
    "with",
    "yield",
    "def",
    "final",
    "implicit",
    "null",
    "protected",
    "throw",
    "val",
    "_",
    ":",
    "=",
    "=>",
    "<-",
    "<:",
    "<%",
    "=>>",
    ">:",
    "#",
    "@",
    "\u21D2",
    "\u2190"
  )

  def keywords(dialect: Dialect) = {
    val dialectKeywords = Set.newBuilder[String]

    if (dialect.allowEnums) dialectKeywords += "enum"
    if (dialect.allowExportClause) dialectKeywords += "export"
    if (dialect.allowGivenUsing) {
      dialectKeywords += "given"
      dialectKeywords += "?=>"
    }
    if (dialect.allowQuietSyntax) dialectKeywords += "then"

    baseKeywords ++ dialectKeywords.result()
  }
}

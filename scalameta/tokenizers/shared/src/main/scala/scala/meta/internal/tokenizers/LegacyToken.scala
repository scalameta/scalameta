package scala.meta
package internal
package tokenizers

// NOTE: moved to the package object
// type LegacyToken = Int

object LegacyToken {
  def isIdentifier(code: LegacyToken) = code == IDENTIFIER || code == BACKQUOTED_IDENT // used by ide
  def isLiteral(code: LegacyToken) = code >= CHARLIT && code <= INTERPOLATIONID

  /** special tokens */
  final val EMPTY = -3
  final val UNDEF = -2
  final val ERROR = -1
  final val EOF = 0

  /** literals */
  final val CHARLIT = 1
  final val INTLIT = 2
  final val LONGLIT = 3
  final val FLOATLIT = 4
  final val DOUBLELIT = 5
  final val STRINGLIT = 6
  final val STRINGPART = 7 // a part of an interpolated string
  final val SYMBOLLIT = 8
  final val INTERPOLATIONID = 9 // the lead identifier of an interpolated string
  final val XMLLIT = 1000

  /** identifiers */
  final val IDENTIFIER = 10
  final val BACKQUOTED_IDENT = 11

  /** keywords */
  final val NEW = 20
  final val THIS = 21
  final val SUPER = 23
  final val NULL = 24
  final val TRUE = 25
  final val FALSE = 26

  /** modifiers */
  final val IMPLICIT = 40
  final val OVERRIDE = 41
  final val PROTECTED = 43
  final val PRIVATE = 44
  final val ABSTRACT = 46
  final val FINAL = 49
  final val SEALED = 45
  final val LAZY = 55
  final val MACRO = 57

  /** templates */
  final val PACKAGE = 60
  final val IMPORT = 61
  final val CLASS = 62
  final val CASECLASS = 63
  final val OBJECT = 64
  final val CASEOBJECT = 65
  final val TRAIT = 66
  final val EXTENDS = 68
  final val WITH = 69
  final val TYPE = 70
  final val FORSOME = 71
  final val DEF = 72
  final val VAL = 73
  final val VAR = 74

  /** control structures */
  final val IF = 80
  final val THEN = 81
  final val ELSE = 82
  final val WHILE = 83
  final val DO = 84
  final val FOR = 85
  final val YIELD = 86
  final val THROW = 90
  final val TRY = 91
  final val CATCH = 92
  final val FINALLY = 93
  final val CASE = 96
  final val RETURN = 97
  final val MATCH = 95

  /** parenthesis */
  final val LPAREN = 100
  final val RPAREN = 101
  final val LBRACKET = 102
  final val RBRACKET = 103
  final val LBRACE = 104
  final val RBRACE = 105

  /** special symbols */
  final val COMMA = 120
  final val SEMI = 121
  final val DOT = 122
  final val COLON = 123
  final val EQUALS = 124
  final val AT = 125

  /** special symbols */
  final val HASH = 130
  final val USCORE = 131
  final val ARROW = 132
  final val LARROW = 133
  final val SUBTYPE = 134
  final val SUPERTYPE = 135
  final val VIEWBOUND = 136

  final val WHITESPACE = 201
  final val COMMENT = 300

  final val UNQUOTE = 400
  final val ELLIPSIS = 401

  val kw2legacytoken = Map[String, LegacyToken](
    "abstract"  -> ABSTRACT,
    "case"      -> CASE,
    "catch"     -> CATCH,
    "class"     -> CLASS,
    "def"       -> DEF,
    "do"        -> DO,
    "else"      -> ELSE,
    "extends"   -> EXTENDS,
    "false"     -> FALSE,
    "final"     -> FINAL,
    "finally"   -> FINALLY,
    "for"       -> FOR,
    "forSome"   -> FORSOME,
    "if"        -> IF,
    "implicit"  -> IMPLICIT,
    "import"    -> IMPORT,
    "lazy"      -> LAZY,
    "match"     -> MATCH,
    "new"       -> NEW,
    "null"      -> NULL,
    "object"    -> OBJECT,
    "override"  -> OVERRIDE,
    "package"   -> PACKAGE,
    "private"   -> PRIVATE,
    "protected" -> PROTECTED,
    "return"    -> RETURN,
    "sealed"    -> SEALED,
    "super"     -> SUPER,
    "this"      -> THIS,
    "throw"     -> THROW,
    "trait"     -> TRAIT,
    "true"      -> TRUE,
    "try"       -> TRY,
    "type"      -> TYPE,
    "val"       -> VAL,
    "var"       -> VAR,
    "while"     -> WHILE,
    "with"      -> WITH,
    "yield"     -> YIELD,
    "."         -> DOT,
    "_"         -> USCORE,
    ":"         -> COLON,
    "="         -> EQUALS,
    "=>"        -> ARROW,
    "<-"        -> LARROW,
    "<:"        -> SUBTYPE,
    "<%"        -> VIEWBOUND,
    ">:"        -> SUPERTYPE,
    "#"         -> HASH,
    "@"         -> AT,
    "macro"     -> MACRO,
    "then"      -> IDENTIFIER
  )
}

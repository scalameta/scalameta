package scala.meta
package tokens

private[meta] trait Api {
  type TokenClassifier[T] = classifiers.Classifier[Token, T]
}

private[meta] trait Aliases {
  type Token = scala.meta.tokens.Token
  @deprecated("Use scala.meta.tokens.Token, this alias contains only a subset", "4.10.2")
  object Token {
    type Ident = scala.meta.tokens.Token.Ident
    val Ident = scala.meta.tokens.Token.Ident
    type KwAbstract = scala.meta.tokens.Token.KwAbstract
    val KwAbstract = scala.meta.tokens.Token.KwAbstract
    type KwCase = scala.meta.tokens.Token.KwCase
    val KwCase = scala.meta.tokens.Token.KwCase
    type KwCatch = scala.meta.tokens.Token.KwCatch
    val KwCatch = scala.meta.tokens.Token.KwCatch
    type KwClass = scala.meta.tokens.Token.KwClass
    val KwClass = scala.meta.tokens.Token.KwClass
    type KwDef = scala.meta.tokens.Token.KwDef
    val KwDef = scala.meta.tokens.Token.KwDef
    type KwDo = scala.meta.tokens.Token.KwDo
    val KwDo = scala.meta.tokens.Token.KwDo
    type KwElse = scala.meta.tokens.Token.KwElse
    val KwElse = scala.meta.tokens.Token.KwElse
    type KwEnum = scala.meta.tokens.Token.KwEnum
    val KwEnum = scala.meta.tokens.Token.KwEnum
    type KwGiven = scala.meta.tokens.Token.KwGiven
    val KwGiven = scala.meta.tokens.Token.KwGiven
    type KwExtends = scala.meta.tokens.Token.KwExtends
    val KwExtends = scala.meta.tokens.Token.KwExtends
    type KwFalse = scala.meta.tokens.Token.KwFalse
    val KwFalse = scala.meta.tokens.Token.KwFalse
    type KwFinal = scala.meta.tokens.Token.KwFinal
    val KwFinal = scala.meta.tokens.Token.KwFinal
    type KwFinally = scala.meta.tokens.Token.KwFinally
    val KwFinally = scala.meta.tokens.Token.KwFinally
    type KwFor = scala.meta.tokens.Token.KwFor
    val KwFor = scala.meta.tokens.Token.KwFor
    type KwForsome = scala.meta.tokens.Token.KwForsome
    val KwForsome = scala.meta.tokens.Token.KwForsome
    type KwIf = scala.meta.tokens.Token.KwIf
    val KwIf = scala.meta.tokens.Token.KwIf
    type KwImplicit = scala.meta.tokens.Token.KwImplicit
    val KwImplicit = scala.meta.tokens.Token.KwImplicit
    type KwImport = scala.meta.tokens.Token.KwImport
    val KwImport = scala.meta.tokens.Token.KwImport
    type KwLazy = scala.meta.tokens.Token.KwLazy
    val KwLazy = scala.meta.tokens.Token.KwLazy
    type KwMatch = scala.meta.tokens.Token.KwMatch
    val KwMatch = scala.meta.tokens.Token.KwMatch
    type KwMacro = scala.meta.tokens.Token.KwMacro
    val KwMacro = scala.meta.tokens.Token.KwMacro
    type KwNew = scala.meta.tokens.Token.KwNew
    val KwNew = scala.meta.tokens.Token.KwNew
    type KwNull = scala.meta.tokens.Token.KwNull
    val KwNull = scala.meta.tokens.Token.KwNull
    type KwObject = scala.meta.tokens.Token.KwObject
    val KwObject = scala.meta.tokens.Token.KwObject
    type KwOverride = scala.meta.tokens.Token.KwOverride
    val KwOverride = scala.meta.tokens.Token.KwOverride
    type KwPackage = scala.meta.tokens.Token.KwPackage
    val KwPackage = scala.meta.tokens.Token.KwPackage
    type KwPrivate = scala.meta.tokens.Token.KwPrivate
    val KwPrivate = scala.meta.tokens.Token.KwPrivate
    type KwProtected = scala.meta.tokens.Token.KwProtected
    val KwProtected = scala.meta.tokens.Token.KwProtected
    type KwReturn = scala.meta.tokens.Token.KwReturn
    val KwReturn = scala.meta.tokens.Token.KwReturn
    type KwSealed = scala.meta.tokens.Token.KwSealed
    val KwSealed = scala.meta.tokens.Token.KwSealed
    type KwSuper = scala.meta.tokens.Token.KwSuper
    val KwSuper = scala.meta.tokens.Token.KwSuper
    type KwThis = scala.meta.tokens.Token.KwThis
    val KwThis = scala.meta.tokens.Token.KwThis
    type KwThrow = scala.meta.tokens.Token.KwThrow
    val KwThrow = scala.meta.tokens.Token.KwThrow
    type KwTrait = scala.meta.tokens.Token.KwTrait
    val KwTrait = scala.meta.tokens.Token.KwTrait
    type KwTrue = scala.meta.tokens.Token.KwTrue
    val KwTrue = scala.meta.tokens.Token.KwTrue
    type KwTry = scala.meta.tokens.Token.KwTry
    val KwTry = scala.meta.tokens.Token.KwTry
    type KwType = scala.meta.tokens.Token.KwType
    val KwType = scala.meta.tokens.Token.KwType
    type KwVal = scala.meta.tokens.Token.KwVal
    val KwVal = scala.meta.tokens.Token.KwVal
    type KwVar = scala.meta.tokens.Token.KwVar
    val KwVar = scala.meta.tokens.Token.KwVar
    type KwWhile = scala.meta.tokens.Token.KwWhile
    val KwWhile = scala.meta.tokens.Token.KwWhile
    type KwWith = scala.meta.tokens.Token.KwWith
    val KwWith = scala.meta.tokens.Token.KwWith
    type KwYield = scala.meta.tokens.Token.KwYield
    val KwYield = scala.meta.tokens.Token.KwYield
    type KwThen = scala.meta.tokens.Token.KwThen
    val KwThen = scala.meta.tokens.Token.KwThen
    type Hash = scala.meta.tokens.Token.Hash
    val Hash = scala.meta.tokens.Token.Hash
    type Colon = scala.meta.tokens.Token.Colon
    val Colon = scala.meta.tokens.Token.Colon
    type Viewbound = scala.meta.tokens.Token.Viewbound
    val Viewbound = scala.meta.tokens.Token.Viewbound
    type TypeLambdaArrow = scala.meta.tokens.Token.TypeLambdaArrow
    val TypeLambdaArrow = scala.meta.tokens.Token.TypeLambdaArrow
    type LeftArrow = scala.meta.tokens.Token.LeftArrow
    val LeftArrow = scala.meta.tokens.Token.LeftArrow
    type Subtype = scala.meta.tokens.Token.Subtype
    val Subtype = scala.meta.tokens.Token.Subtype
    type Equals = scala.meta.tokens.Token.Equals
    val Equals = scala.meta.tokens.Token.Equals
    type RightArrow = scala.meta.tokens.Token.RightArrow
    val RightArrow = scala.meta.tokens.Token.RightArrow
    type Supertype = scala.meta.tokens.Token.Supertype
    val Supertype = scala.meta.tokens.Token.Supertype
    type At = scala.meta.tokens.Token.At
    val At = scala.meta.tokens.Token.At
    type Underscore = scala.meta.tokens.Token.Underscore
    val Underscore = scala.meta.tokens.Token.Underscore
    type LeftParen = scala.meta.tokens.Token.LeftParen
    val LeftParen = scala.meta.tokens.Token.LeftParen
    type RightParen = scala.meta.tokens.Token.RightParen
    val RightParen = scala.meta.tokens.Token.RightParen
    type Comma = scala.meta.tokens.Token.Comma
    val Comma = scala.meta.tokens.Token.Comma
    type Dot = scala.meta.tokens.Token.Dot
    val Dot = scala.meta.tokens.Token.Dot
    type Semicolon = scala.meta.tokens.Token.Semicolon
    val Semicolon = scala.meta.tokens.Token.Semicolon
    type LeftBracket = scala.meta.tokens.Token.LeftBracket
    val LeftBracket = scala.meta.tokens.Token.LeftBracket
    type RightBracket = scala.meta.tokens.Token.RightBracket
    val RightBracket = scala.meta.tokens.Token.RightBracket
    type LeftBrace = scala.meta.tokens.Token.LeftBrace
    val LeftBrace = scala.meta.tokens.Token.LeftBrace
    type RightBrace = scala.meta.tokens.Token.RightBrace
    val RightBrace = scala.meta.tokens.Token.RightBrace
    object Constant {
      type Int = scala.meta.tokens.Token.Constant.Int
      val Int = scala.meta.tokens.Token.Constant.Int
      type Long = scala.meta.tokens.Token.Constant.Long
      val Long = scala.meta.tokens.Token.Constant.Long
      type Float = scala.meta.tokens.Token.Constant.Float
      val Float = scala.meta.tokens.Token.Constant.Float
      type Double = scala.meta.tokens.Token.Constant.Double
      val Double = scala.meta.tokens.Token.Constant.Double
      type Char = scala.meta.tokens.Token.Constant.Char
      val Char = scala.meta.tokens.Token.Constant.Char
      type Symbol = scala.meta.tokens.Token.Constant.Symbol
      val Symbol = scala.meta.tokens.Token.Constant.Symbol
      type String = scala.meta.tokens.Token.Constant.String
      val String = scala.meta.tokens.Token.Constant.String
    }
    object Interpolation {
      type Id = scala.meta.tokens.Token.Interpolation.Id
      val Id = scala.meta.tokens.Token.Interpolation.Id
      type Start = scala.meta.tokens.Token.Interpolation.Start
      val Start = scala.meta.tokens.Token.Interpolation.Start
      type Part = scala.meta.tokens.Token.Interpolation.Part
      val Part = scala.meta.tokens.Token.Interpolation.Part
      type SpliceStart = scala.meta.tokens.Token.Interpolation.SpliceStart
      val SpliceStart = scala.meta.tokens.Token.Interpolation.SpliceStart
      type SpliceEnd = scala.meta.tokens.Token.Interpolation.SpliceEnd
      val SpliceEnd = scala.meta.tokens.Token.Interpolation.SpliceEnd
      type End = scala.meta.tokens.Token.Interpolation.End
      val End = scala.meta.tokens.Token.Interpolation.End
    }
    object Xml {
      type Start = scala.meta.tokens.Token.Xml.Start
      val Start = scala.meta.tokens.Token.Xml.Start
      type Part = scala.meta.tokens.Token.Xml.Part
      val Part = scala.meta.tokens.Token.Xml.Part
      type SpliceStart = scala.meta.tokens.Token.Xml.SpliceStart
      val SpliceStart = scala.meta.tokens.Token.Xml.SpliceStart
      type SpliceEnd = scala.meta.tokens.Token.Xml.SpliceEnd
      val SpliceEnd = scala.meta.tokens.Token.Xml.SpliceEnd
      type End = scala.meta.tokens.Token.Xml.End
      val End = scala.meta.tokens.Token.Xml.End
    }

    // horizontal space
    type MultiHS = scala.meta.tokens.Token.MultiHS
    val MultiHS = scala.meta.tokens.Token.MultiHS
    type Space = scala.meta.tokens.Token.Space
    val Space = scala.meta.tokens.Token.Space
    type Tab = scala.meta.tokens.Token.Tab
    val Tab = scala.meta.tokens.Token.Tab

    // vertical space
    type MultiNL = scala.meta.tokens.Token.MultiNL
    val MultiNL = scala.meta.tokens.Token.MultiNL
    type CR = scala.meta.tokens.Token.CR
    val CR = scala.meta.tokens.Token.CR
    type LF = scala.meta.tokens.Token.LF
    val LF = scala.meta.tokens.Token.LF
    type FF = scala.meta.tokens.Token.FF
    val FF = scala.meta.tokens.Token.FF
    type LFLF = scala.meta.tokens.Token.LFLF
    val LFLF = scala.meta.tokens.Token.LFLF

    type Comment = scala.meta.tokens.Token.Comment
    val Comment = scala.meta.tokens.Token.Comment
    type BOF = scala.meta.tokens.Token.BOF
    val BOF = scala.meta.tokens.Token.BOF
    type EOF = scala.meta.tokens.Token.EOF
    val EOF = scala.meta.tokens.Token.EOF
    type Ellipsis = scala.meta.tokens.Token.Ellipsis
    val Ellipsis = scala.meta.tokens.Token.Ellipsis
    type Unquote = scala.meta.tokens.Token.Unquote
    val Unquote = scala.meta.tokens.Token.Unquote
  }

  type Tokens = scala.meta.tokens.Tokens
  lazy val Tokens = scala.meta.tokens.Tokens
}

package scala.meta
package syntactic.parsers

import scala.reflect.ClassTag

sealed trait Tok {
  def is[T <: Tok: ClassTag]: Boolean  = this match { case _: T => true; case _ => false }
  def isNot[T <: Tok: ClassTag]: Boolean = !is[T]
  def offset: Int
}
object Tok {
  sealed trait TypeIntro extends Tok
  sealed trait ExprIntro extends Tok
  sealed trait DefIntro extends Tok
  sealed trait TemplateIntro extends DefIntro
  sealed trait DclIntro extends DefIntro
  sealed trait StatSeqEnd extends Tok
  sealed trait CaseDefEnd extends Tok

  final case class Ident(value: Predef.String, isBackquoted: Boolean, offset: Int) extends ExprIntro with TypeIntro

  object Interpolation {
    final case class Id(value: Predef.String, offset: Int) extends ExprIntro
    final case class Part(value: Predef.String, offset: Int) extends Tok    
  }  

  sealed trait Literal extends ExprIntro
  sealed trait NumericLiteral extends Literal
  object Literal {    
    final case class Int(value: scala.Int, offset: scala.Int) extends NumericLiteral
    final case class Long(value: scala.Long, offset: scala.Int) extends NumericLiteral
    final case class Float(value: scala.Float, offset: scala.Int) extends NumericLiteral
    final case class Double(value: scala.Double, offset: scala.Int) extends NumericLiteral
    final case class Char(value: scala.Char, offset: scala.Int) extends Literal
    final case class Symbol(value: scala.Symbol, offset: scala.Int) extends Literal
    final case class String(value: Predef.String, offset: scala.Int) extends Literal
  }  
  case class `null`(offset: Int) extends Keyword with Literal
  case class `true`(offset: Int) extends Keyword with Literal
  case class `false`(offset: Int) extends Keyword with Literal

  sealed trait Keyword extends Tok
  case class `case`(offset: Int) extends Keyword with CaseDefEnd
  case class `case class`(offset: Int) extends Keyword with TemplateIntro
  case class `case object`(offset: Int) extends Keyword with TemplateIntro
  case class `catch`(offset: Int) extends Keyword
  case class `class `(offset: Int) extends Keyword with TemplateIntro
  case class `def`(offset: Int) extends Keyword with DclIntro
  case class `do`(offset: Int) extends Keyword with ExprIntro
  case class `else`(offset: Int) extends Keyword
  case class `extends`(offset: Int) extends Keyword
  case class `finally`(offset: Int) extends Keyword
  case class `for`(offset: Int) extends Keyword with ExprIntro
  case class `forSome`(offset: Int) extends Keyword
  case class `if`(offset: Int) extends Keyword with ExprIntro
  case class `import`(offset: Int) extends Keyword
  case class `match`(offset: Int) extends Keyword
  case class `macro`(offset: Int) extends Keyword
  case class `new`(offset: Int) extends Keyword with ExprIntro
  case class `object`(offset: Int) extends Keyword with TemplateIntro
  case class `package `(offset: Int) extends Keyword
  case class `return`(offset: Int) extends Keyword with ExprIntro
  case class `super`(offset: Int) extends Keyword with ExprIntro with TypeIntro
  case class `this`(offset: Int) extends Keyword with ExprIntro with TypeIntro
  case class `throw`(offset: Int) extends Keyword with ExprIntro
  case class `trait`(offset: Int) extends Keyword with TemplateIntro
  case class `try`(offset: Int) extends Keyword with ExprIntro
  case class `type`(offset: Int) extends Keyword with DclIntro
  case class `val`(offset: Int) extends Keyword with DclIntro
  case class `var`(offset: Int) extends Keyword with DclIntro
  case class `while`(offset: Int) extends Keyword with ExprIntro
  case class `with`(offset: Int) extends Keyword
  case class `yield`(offset: Int) extends Keyword

  sealed trait Modifier extends Keyword
  sealed trait LocalModifier extends Modifier
  case class `abstract`(offset: Int) extends LocalModifier
  case class `final`(offset: Int) extends LocalModifier
  case class `sealed`(offset: Int) extends LocalModifier
  case class `implicit`(offset: Int) extends LocalModifier
  case class `lazy`(offset: Int) extends LocalModifier
  case class `private`(offset: Int) extends Modifier
  case class `protected`(offset: Int) extends Modifier
  case class `override`(offset: Int) extends Modifier

  sealed trait Delim extends Tok
  sealed trait StatSep extends Delim
  case class `(`(offset: Int) extends Delim with ExprIntro with TypeIntro
  case class `)`(offset: Int) extends Delim
  case class `[`(offset: Int) extends Delim
  case class `]`(offset: Int) extends Delim
  case class `{`(offset: Int) extends Delim with ExprIntro
  case class `}`(offset: Int) extends Delim with StatSeqEnd with CaseDefEnd
  case class `,`(offset: Int) extends Delim
  case class `;`(offset: Int) extends StatSep
  case class `:`(offset: Int) extends Delim
  case class `.`(offset: Int) extends Delim
  case class `=`(offset: Int) extends Delim
  case class `@`(offset: Int) extends Delim with TypeIntro
  case class `#`(offset: Int) extends Delim
  case class `_ `(offset: Int) extends Delim with ExprIntro with TypeIntro
  case class `=>`(offset: Int) extends Delim
  case class `<-`(offset: Int) extends Delim
  case class `<:`(offset: Int) extends Delim
  case class `>:`(offset: Int) extends Delim
  case class `<%`(offset: Int) extends Delim
  case class `\n`(offset: Int) extends StatSep
  case class `\n\n`(offset: Int) extends StatSep
  
  case class EOF(offset: Int) extends StatSep with StatSeqEnd with CaseDefEnd
  case class XMLStart(offset: Int) extends Tok with ExprIntro
}
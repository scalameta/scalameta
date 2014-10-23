package scala.meta
package syntactic.parsers

import scala.reflect.ClassTag

sealed trait Tok {
  def is[T <: Tok: ClassTag]: Boolean  = this match { case _: T => true; case _ => false }
  def not[T <: Tok: ClassTag]: Boolean = !is[T]
}
object Tok {
  sealed trait TypeIntro extends Tok
  sealed trait ExprIntro extends Tok
  sealed trait DefIntro extends Tok
  sealed trait TemplateIntro extends DefIntro
  sealed trait DclIntro extends DefIntro
  sealed trait StatSeqEnd extends Tok
  sealed trait CaseDefEnd extends Tok

  final case class Ident(value: Predef.String, isBackquoted: Boolean) extends ExprIntro with TypeIntro

  object Interpolation {
    final case class Id(value: Predef.String) extends ExprIntro
    final case class Part(value: Predef.String) extends Tok    
  }  

  sealed trait Literal extends ExprIntro
  sealed trait NumericLiteral extends Literal
  object Literal {    
    final case class Int(value: scala.Int) extends NumericLiteral
    final case class Long(value: scala.Long) extends NumericLiteral
    final case class Float(value: scala.Float) extends NumericLiteral
    final case class Double(value: scala.Double) extends NumericLiteral
    final case class Char(value: scala.Char) extends Literal
    final case class Symbol(value: scala.Symbol) extends Literal
    final case class String(value: Predef.String) extends Literal
  }  
  case class `null`() extends Keyword with Literal
  case class `true`() extends Keyword with Literal
  case class `false`() extends Keyword with Literal

  sealed trait Keyword extends Tok
  case class `case`() extends Keyword with CaseDefEnd
  case class `case class`() extends Keyword with TemplateIntro
  case class `case object`() extends Keyword with TemplateIntro
  case class `catch`() extends Keyword
  case class `class `() extends Keyword with TemplateIntro
  case class `def`() extends Keyword with DclIntro
  case class `do`() extends Keyword with ExprIntro
  case class `else`() extends Keyword
  case class `extends`() extends Keyword
  case class `finally`() extends Keyword
  case class `for`() extends Keyword with ExprIntro
  case class `forSome`() extends Keyword
  case class `if`() extends Keyword with ExprIntro
  case class `import`() extends Keyword
  case class `match`() extends Keyword
  case class `macro`() extends Keyword
  case class `new`() extends Keyword with ExprIntro
  case class `object`() extends Keyword with TemplateIntro
  case class `package `() extends Keyword
  case class `return`() extends Keyword with ExprIntro
  case class `super`() extends Keyword with ExprIntro with TypeIntro
  case class `this`() extends Keyword with ExprIntro with TypeIntro
  case class `throw`() extends Keyword with ExprIntro
  case class `trait`() extends Keyword with TemplateIntro
  case class `try`() extends Keyword with ExprIntro
  case class `type`() extends Keyword with DclIntro
  case class `val`() extends Keyword with DclIntro
  case class `var`() extends Keyword with DclIntro
  case class `while`() extends Keyword with ExprIntro
  case class `with`() extends Keyword
  case class `yield`() extends Keyword

  sealed trait Modifier extends Keyword
  sealed trait LocalModifier extends Modifier
  case class `abstract`() extends LocalModifier
  case class `final`() extends LocalModifier
  case class `sealed`() extends LocalModifier
  case class `implicit`() extends LocalModifier
  case class `lazy`() extends LocalModifier
  case class `private`() extends Modifier
  case class `protected`() extends Modifier
  case class `override`() extends Modifier

  sealed trait Delim extends Tok
  sealed trait StatSep extends Delim
  case class `(`() extends Delim with ExprIntro with TypeIntro
  case class `)`() extends Delim
  case class `[`() extends Delim
  case class `]`() extends Delim
  case class `{`() extends Delim with ExprIntro
  case class `}`() extends Delim with StatSeqEnd with CaseDefEnd
  case class `,`() extends Delim
  case class `;`() extends StatSep
  case class `:`() extends Delim
  case class `.`() extends Delim
  case class `=`() extends Delim
  case class `@`() extends Delim with TypeIntro
  case class `#`() extends Delim
  case class `_ `() extends Delim with ExprIntro with TypeIntro
  case class `=>`() extends Delim
  case class `<-`() extends Delim
  case class `<:`() extends Delim
  case class `>:`() extends Delim
  case class `<%`() extends Delim
  case class `\n`() extends StatSep
  case class `\n\n`() extends StatSep
  
  case class EOF() extends StatSep with StatSeqEnd with CaseDefEnd
  case class XMLStart() extends Tok with ExprIntro
}
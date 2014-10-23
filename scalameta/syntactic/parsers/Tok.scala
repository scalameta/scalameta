package scala.meta
package syntactic.parsers

import org.scalameta.adt._
import scala.reflect.ClassTag

@root trait Tok {
  def is[T <: Tok: ClassTag]: Boolean  = this match { case _: T => true; case _ => false }
  def isNot[T <: Tok: ClassTag]: Boolean = !is[T]
  def offset: Int
}
object Tok {
  @branch trait TypeIntro extends Tok
  @branch trait ExprIntro extends Tok
  @branch trait DefIntro extends Tok
  @branch trait TemplateIntro extends DefIntro
  @branch trait DclIntro extends DefIntro
  @branch trait StatSeqEnd extends Tok
  @branch trait CaseDefEnd extends Tok

  @leaf class Ident(value: Predef.String, isBackquoted: Boolean, offset: Int) extends ExprIntro with TypeIntro

  object Interpolation {
    @leaf class Id(value: Predef.String, offset: Int) extends ExprIntro
    @leaf class Part(value: Predef.String, offset: Int) extends Tok    
  }  

  @branch trait Literal extends ExprIntro
  @branch trait NumericLiteral extends Literal
  object Literal {    
    @leaf class Int(value: scala.Int, offset: scala.Int) extends NumericLiteral
    @leaf class Long(value: scala.Long, offset: scala.Int) extends NumericLiteral
    @leaf class Float(value: scala.Float, offset: scala.Int) extends NumericLiteral
    @leaf class Double(value: scala.Double, offset: scala.Int) extends NumericLiteral
    @leaf class Char(value: scala.Char, offset: scala.Int) extends Literal
    @leaf class Symbol(value: scala.Symbol, offset: scala.Int) extends Literal
    @leaf class String(value: Predef.String, offset: scala.Int) extends Literal
  }  
  @leaf class `null`(offset: Int) extends Keyword with Literal
  @leaf class `true`(offset: Int) extends Keyword with Literal
  @leaf class `false`(offset: Int) extends Keyword with Literal

  @branch trait Keyword extends Tok
  @leaf class `case`(offset: Int) extends Keyword with CaseDefEnd
  @leaf class `case class`(offset: Int) extends Keyword with TemplateIntro
  @leaf class `case object`(offset: Int) extends Keyword with TemplateIntro
  @leaf class `catch`(offset: Int) extends Keyword
  @leaf class `class `(offset: Int) extends Keyword with TemplateIntro
  @leaf class `def`(offset: Int) extends Keyword with DclIntro
  @leaf class `do`(offset: Int) extends Keyword with ExprIntro
  @leaf class `else`(offset: Int) extends Keyword
  @leaf class `extends`(offset: Int) extends Keyword
  @leaf class `finally`(offset: Int) extends Keyword
  @leaf class `for`(offset: Int) extends Keyword with ExprIntro
  @leaf class `forSome`(offset: Int) extends Keyword
  @leaf class `if`(offset: Int) extends Keyword with ExprIntro
  @leaf class `import`(offset: Int) extends Keyword
  @leaf class `match`(offset: Int) extends Keyword
  @leaf class `macro`(offset: Int) extends Keyword
  @leaf class `new`(offset: Int) extends Keyword with ExprIntro
  @leaf class `object`(offset: Int) extends Keyword with TemplateIntro
  @leaf class `package `(offset: Int) extends Keyword
  @leaf class `return`(offset: Int) extends Keyword with ExprIntro
  @leaf class `super`(offset: Int) extends Keyword with ExprIntro with TypeIntro
  @leaf class `this`(offset: Int) extends Keyword with ExprIntro with TypeIntro
  @leaf class `throw`(offset: Int) extends Keyword with ExprIntro
  @leaf class `trait`(offset: Int) extends Keyword with TemplateIntro
  @leaf class `try`(offset: Int) extends Keyword with ExprIntro
  @leaf class `type`(offset: Int) extends Keyword with DclIntro
  @leaf class `val`(offset: Int) extends Keyword with DclIntro
  @leaf class `var`(offset: Int) extends Keyword with DclIntro
  @leaf class `while`(offset: Int) extends Keyword with ExprIntro
  @leaf class `with`(offset: Int) extends Keyword
  @leaf class `yield`(offset: Int) extends Keyword

  @branch trait Modifier extends Keyword
  @branch trait LocalModifier extends Modifier
  @leaf class `abstract`(offset: Int) extends LocalModifier
  @leaf class `final`(offset: Int) extends LocalModifier
  @leaf class `sealed`(offset: Int) extends LocalModifier
  @leaf class `implicit`(offset: Int) extends LocalModifier
  @leaf class `lazy`(offset: Int) extends LocalModifier
  @leaf class `private`(offset: Int) extends Modifier
  @leaf class `protected`(offset: Int) extends Modifier
  @leaf class `override`(offset: Int) extends Modifier

  @branch trait Delim extends Tok
  @branch trait StatSep extends Delim
  @leaf class `(`(offset: Int) extends Delim with ExprIntro with TypeIntro
  @leaf class `)`(offset: Int) extends Delim
  @leaf class `[`(offset: Int) extends Delim
  @leaf class `]`(offset: Int) extends Delim
  @leaf class `{`(offset: Int) extends Delim with ExprIntro
  @leaf class `}`(offset: Int) extends Delim with StatSeqEnd with CaseDefEnd
  @leaf class `,`(offset: Int) extends Delim
  @leaf class `;`(offset: Int) extends StatSep
  @leaf class `:`(offset: Int) extends Delim
  @leaf class `.`(offset: Int) extends Delim
  @leaf class `=`(offset: Int) extends Delim
  @leaf class `@`(offset: Int) extends Delim with TypeIntro
  @leaf class `#`(offset: Int) extends Delim
  @leaf class `_ `(offset: Int) extends Delim with ExprIntro with TypeIntro
  @leaf class `=>`(offset: Int) extends Delim
  @leaf class `<-`(offset: Int) extends Delim
  @leaf class `<:`(offset: Int) extends Delim
  @leaf class `>:`(offset: Int) extends Delim
  @leaf class `<%`(offset: Int) extends Delim
  @leaf class `\n`(offset: Int) extends StatSep
  @leaf class `\n\n`(offset: Int) extends StatSep
  
  @leaf class EOF(offset: Int) extends StatSep with StatSeqEnd with CaseDefEnd
  @leaf class XMLStart(offset: Int) extends Tok with ExprIntro
}
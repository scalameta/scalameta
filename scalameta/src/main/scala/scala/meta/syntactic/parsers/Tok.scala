package scala.meta.syntactic
package parsers

import org.scalameta.tokens._
import scala.reflect.ClassTag
import scala.language.experimental.macros
import scala.meta.syntactic.show._

@root trait Tok {
  def is[T: ClassTag]: Boolean    = implicitly[ClassTag[T]].runtimeClass.isAssignableFrom(this.getClass)
  def isNot[T: ClassTag]: Boolean = !is[T]
  def offset: Int
  def name: String
  def code: String
  final override def toString = this.show[Raw]
}
object Tok {
  @branch trait Static extends Tok
  @branch trait Dynamic extends Tok

  @branch trait TypeIntro extends Tok
  @branch trait ExprIntro extends Tok
  @branch trait DefIntro extends Tok
  @branch trait TemplateIntro extends DefIntro
  @branch trait DclIntro extends DefIntro
  @branch trait StatSeqEnd extends Tok
  @branch trait CaseDefEnd extends Tok

  @branch trait CantStartStat extends Tok
  @branch trait CanEndStat extends Tok

  @token class Ident(code: Predef.String, value: Predef.String, isBackquoted: Boolean, offset: Int) extends Dynamic with ExprIntro with TypeIntro with CanEndStat { def name = "identifier" }

  object Interpolation {
    @token class Id(code: Predef.String, value: Predef.String, offset: Int) extends Dynamic with ExprIntro { def name = "interpolation id" }
    @token class Start(code: Predef.String, offset: Int) extends Dynamic with Tok { def name = "interpolation start" }
    @token class Part(code: Predef.String, value: Predef.String, offset: Int) extends Dynamic with Tok { def name = "interpolation part" }
    @token class SpliceStart(offset: Int) extends Static with Tok { def name = "splice start"; def code = "$" }
    @token class SpliceEnd(offset: Int) extends Static with Tok { def name = "splice end"; def code = "" }
    @token class End(code: Predef.String, offset: Int) extends Dynamic with Tok { def name = "interpolation end" }
  }

  @branch trait Literal extends ExprIntro with CanEndStat
  @branch trait DynamicLiteral extends Dynamic with Literal
  @branch trait NumericLiteral extends DynamicLiteral
  object Literal {
    @token class Int(code: Predef.String, value: scala.Int, offset: scala.Int) extends NumericLiteral { def name = "integer literal" }
    @token class Long(code: Predef.String, value: scala.Long, offset: scala.Int) extends NumericLiteral { def name = "long literal" }
    @token class Float(code: Predef.String, value: scala.Float, offset: scala.Int) extends NumericLiteral { def name = "float literal" }
    @token class Double(code: Predef.String, value: scala.Double, offset: scala.Int) extends NumericLiteral { def name = "double literal" }
    @token class Char(code: Predef.String, value: scala.Char, offset: scala.Int) extends DynamicLiteral { def name = "character literal" }
    @token class Symbol(code: Predef.String, value: scala.Symbol, offset: scala.Int) extends DynamicLiteral { def name = "symbol literal" }
    @token class String(code: Predef.String, value: Predef.String, offset: scala.Int) extends DynamicLiteral { def name = "string literal" }
  }
  @token class `null`(offset: Int) extends Keyword with Literal
  @token class `true`(offset: Int) extends Keyword with Literal
  @token class `false`(offset: Int) extends Keyword with Literal

  @branch trait Keyword extends Static with Tok
  @token class `case`(offset: Int) extends Keyword with CaseDefEnd with TemplateIntro
  @token class `catch`(offset: Int) extends Keyword with CantStartStat
  @token class `class `(offset: Int) extends Keyword with TemplateIntro
  @token class `def`(offset: Int) extends Keyword with DclIntro
  @token class `do`(offset: Int) extends Keyword with ExprIntro
  @token class `else`(offset: Int) extends Keyword with CantStartStat
  @token class `extends`(offset: Int) extends Keyword with CantStartStat
  @token class `finally`(offset: Int) extends Keyword with CantStartStat
  @token class `for`(offset: Int) extends Keyword with ExprIntro
  @token class `forSome`(offset: Int) extends Keyword with CantStartStat
  @token class `if`(offset: Int) extends Keyword with ExprIntro
  @token class `import`(offset: Int) extends Keyword
  @token class `match`(offset: Int) extends Keyword with CantStartStat
  @token class `macro`(offset: Int) extends Keyword
  @token class `new`(offset: Int) extends Keyword with ExprIntro
  @token class `object`(offset: Int) extends Keyword with TemplateIntro
  @token class `package `(offset: Int) extends Keyword
  @token class `return`(offset: Int) extends Keyword with ExprIntro with CanEndStat
  @token class `super`(offset: Int) extends Keyword with ExprIntro with TypeIntro
  @token class `this`(offset: Int) extends Keyword with ExprIntro with TypeIntro
  @token class `throw`(offset: Int) extends Keyword with ExprIntro
  @token class `trait`(offset: Int) extends Keyword with TemplateIntro
  @token class `try`(offset: Int) extends Keyword with ExprIntro
  @token class `type`(offset: Int) extends Keyword with DclIntro with CanEndStat
  @token class `val`(offset: Int) extends Keyword with DclIntro
  @token class `var`(offset: Int) extends Keyword with DclIntro
  @token class `while`(offset: Int) extends Keyword with ExprIntro
  @token class `with`(offset: Int) extends Keyword with CantStartStat
  @token class `yield`(offset: Int) extends Keyword with CantStartStat

  @branch trait Modifier extends Keyword
  @branch trait LocalModifier extends Modifier
  @token class `abstract`(offset: Int) extends LocalModifier
  @token class `final`(offset: Int) extends LocalModifier
  @token class `sealed`(offset: Int) extends LocalModifier
  @token class `implicit`(offset: Int) extends LocalModifier
  @token class `lazy`(offset: Int) extends LocalModifier
  @token class `private`(offset: Int) extends Modifier
  @token class `protected`(offset: Int) extends Modifier
  @token class `override`(offset: Int) extends Modifier

  @branch trait Delim extends Static with Tok
  @branch trait StatSep extends Delim
  @token class `(`(offset: Int) extends Delim with ExprIntro with TypeIntro
  @token class `)`(offset: Int) extends Delim with CantStartStat with CanEndStat
  @token class `[`(offset: Int) extends Delim with CantStartStat
  @token class `]`(offset: Int) extends Delim with CantStartStat with CanEndStat
  @token class `{`(offset: Int) extends Delim with ExprIntro
  @token class `}`(offset: Int) extends Delim with StatSeqEnd with CaseDefEnd with CantStartStat with CanEndStat
  @token class `,`(offset: Int) extends Delim with CantStartStat
  @token class `;`(offset: Int) extends StatSep with CantStartStat
  @token class `:`(offset: Int) extends Delim with CantStartStat
  @token class `.`(offset: Int) extends Delim with CantStartStat
  @token class `=`(offset: Int) extends Delim with CantStartStat
  @token class `@`(offset: Int) extends Delim with TypeIntro
  @token class `#`(offset: Int) extends Delim with CantStartStat
  @token class `_ `(offset: Int) extends Delim with ExprIntro with TypeIntro with CanEndStat
  @token class `=>`(offset: Int) extends Delim with CantStartStat
  @token class `<-`(offset: Int) extends Delim with CantStartStat
  @token class `<:`(offset: Int) extends Delim with CantStartStat
  @token class `>:`(offset: Int) extends Delim with CantStartStat
  @token class `<%`(offset: Int) extends Delim with CantStartStat

  @branch trait Whitespace extends Static with Tok
  @token class ` `(offset: Int) extends Whitespace
  @token class `\t`(offset: Int) extends Whitespace
  @token class `\r`(offset: Int) extends Whitespace
  @token class `\n`(offset: Int) extends Whitespace with StatSep with CantStartStat
  @token class `\f`(offset: Int) extends Whitespace

  @token class EOF(offset: Int) extends Static with StatSep with StatSeqEnd with CaseDefEnd with CantStartStat { def name = "end of file"; def code = "" }
  @token class XMLStart(offset: Int) extends Dynamic with Tok with ExprIntro with CanEndStat { def name = ???; def code = ??? }
}

package scala.meta.syntactic
package parsers

import org.scalameta.tokens._
import scala.reflect.ClassTag
import scala.language.experimental.macros
import scala.meta.syntactic.show._
import scala.meta.Origin

@root trait Tok {
  def is[T: ClassTag]: Boolean    = implicitly[ClassTag[T]].runtimeClass.isAssignableFrom(this.getClass)
  def isNot[T: ClassTag]: Boolean = !is[T]
  def origin: Origin
  def start: Int
  def end: Int
  def name: String
  def code: String = {
    val buf = new StringBuffer
    var i = start
    while (i <= end) {
      buf.append(origin.content(i))
      i += 1
    }
    buf.toString
  }
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

  @token class Ident(origin: Origin, start: Int, end: Int) extends Dynamic with ExprIntro with TypeIntro with CanEndStat { def name = "identifier" }

  object Interpolation {
    @token class Id(origin: Origin, start: Int, end: Int) extends Dynamic with ExprIntro { def name = "interpolation id" }
    @token class Start(origin: Origin, start: Int, end: Int) extends Dynamic with Tok { def name = "interpolation start" }
    @token class Part(origin: Origin, start: Int, end: Int) extends Dynamic with Tok { def name = "interpolation part" }
    @token class SpliceStart(origin: Origin, start: Int) extends Static with Tok { def name = "splice start"; override def code = "$" }
    @token class SpliceEnd(origin: Origin, start: Int) extends Static with Tok { def name = "splice end"; override def code = "" }
    @token class End(origin: Origin, start: Int, end: Int) extends Dynamic with Tok with CanEndStat { def name = "interpolation end" }
  }

  @branch trait Literal extends ExprIntro with CanEndStat
  @branch trait DynamicLiteral extends Dynamic with Literal
  @branch trait NumericLiteral extends DynamicLiteral
  object Literal {
    @token class Int(origin: Origin, start: scala.Int, end: scala.Int, value: Boolean => scala.Int) extends NumericLiteral { def name = "integer literal" }
    @token class Long(origin: Origin, start: scala.Int, end: scala.Int, value: Boolean => scala.Long) extends NumericLiteral { def name = "long literal" }
    @token class Float(origin: Origin, start: scala.Int, end: scala.Int, value: Boolean => scala.Float) extends NumericLiteral { def name = "float literal" }
    @token class Double(origin: Origin, start: scala.Int, end: scala.Int, value: Boolean => scala.Double) extends NumericLiteral { def name = "double literal" }
    @token class Char(origin: Origin, start: scala.Int, end: scala.Int, value: scala.Char) extends DynamicLiteral { def name = "character literal" }
    @token class Symbol(origin: Origin, start: scala.Int, end: scala.Int, value: scala.Symbol) extends DynamicLiteral { def name = "symbol literal" }
    @token class String(origin: Origin, start: scala.Int, end: scala.Int, value: Predef.String) extends DynamicLiteral { def name = "string literal" }
  }
  @token class `null`(origin: Origin, start: Int) extends Keyword with Literal
  @token class `true`(origin: Origin, start: Int) extends Keyword with Literal
  @token class `false`(origin: Origin, start: Int) extends Keyword with Literal

  @branch trait Keyword extends Static with Tok
  @token class `case`(origin: Origin, start: Int) extends Keyword with CaseDefEnd with TemplateIntro
  @token class `catch`(origin: Origin, start: Int) extends Keyword with CantStartStat
  @token class `class `(origin: Origin, start: Int) extends Keyword with TemplateIntro
  @token class `def`(origin: Origin, start: Int) extends Keyword with DclIntro
  @token class `do`(origin: Origin, start: Int) extends Keyword with ExprIntro
  @token class `else`(origin: Origin, start: Int) extends Keyword with CantStartStat
  @token class `extends`(origin: Origin, start: Int) extends Keyword with CantStartStat
  @token class `finally`(origin: Origin, start: Int) extends Keyword with CantStartStat
  @token class `for`(origin: Origin, start: Int) extends Keyword with ExprIntro
  @token class `forSome`(origin: Origin, start: Int) extends Keyword with CantStartStat
  @token class `if`(origin: Origin, start: Int) extends Keyword with ExprIntro
  @token class `import`(origin: Origin, start: Int) extends Keyword
  @token class `match`(origin: Origin, start: Int) extends Keyword with CantStartStat
  @token class `macro`(origin: Origin, start: Int) extends Keyword
  @token class `new`(origin: Origin, start: Int) extends Keyword with ExprIntro
  @token class `object`(origin: Origin, start: Int) extends Keyword with TemplateIntro
  @token class `package `(origin: Origin, start: Int) extends Keyword
  @token class `return`(origin: Origin, start: Int) extends Keyword with ExprIntro with CanEndStat
  @token class `super`(origin: Origin, start: Int) extends Keyword with ExprIntro with TypeIntro
  @token class `this`(origin: Origin, start: Int) extends Keyword with ExprIntro with TypeIntro
  @token class `throw`(origin: Origin, start: Int) extends Keyword with ExprIntro
  @token class `trait`(origin: Origin, start: Int) extends Keyword with TemplateIntro
  @token class `try`(origin: Origin, start: Int) extends Keyword with ExprIntro
  @token class `type`(origin: Origin, start: Int) extends Keyword with DclIntro with CanEndStat
  @token class `val`(origin: Origin, start: Int) extends Keyword with DclIntro
  @token class `var`(origin: Origin, start: Int) extends Keyword with DclIntro
  @token class `while`(origin: Origin, start: Int) extends Keyword with ExprIntro
  @token class `with`(origin: Origin, start: Int) extends Keyword with CantStartStat
  @token class `yield`(origin: Origin, start: Int) extends Keyword with CantStartStat

  @branch trait Modifier extends Keyword
  @branch trait LocalModifier extends Modifier
  @token class `abstract`(origin: Origin, start: Int) extends LocalModifier
  @token class `final`(origin: Origin, start: Int) extends LocalModifier
  @token class `sealed`(origin: Origin, start: Int) extends LocalModifier
  @token class `implicit`(origin: Origin, start: Int) extends LocalModifier
  @token class `lazy`(origin: Origin, start: Int) extends LocalModifier
  @token class `private`(origin: Origin, start: Int) extends Modifier
  @token class `protected`(origin: Origin, start: Int) extends Modifier
  @token class `override`(origin: Origin, start: Int) extends Modifier

  @branch trait Delim extends Tok
  @branch trait StaticDelim extends Delim with Static
  @branch trait DynamicDelim extends Delim with Static
  @branch trait StatSep extends Delim
  @token class `(`(origin: Origin, start: Int) extends StaticDelim with ExprIntro with TypeIntro
  @token class `)`(origin: Origin, start: Int) extends StaticDelim with CantStartStat with CanEndStat
  @token class `[`(origin: Origin, start: Int) extends StaticDelim with CantStartStat
  @token class `]`(origin: Origin, start: Int) extends StaticDelim with CantStartStat with CanEndStat
  @token class `{`(origin: Origin, start: Int) extends StaticDelim with ExprIntro
  @token class `}`(origin: Origin, start: Int) extends StaticDelim with StatSeqEnd with CaseDefEnd with CantStartStat with CanEndStat
  @token class `,`(origin: Origin, start: Int) extends StaticDelim with CantStartStat
  @token class `;`(origin: Origin, start: Int) extends StaticDelim with StatSep with CantStartStat
  @token class `:`(origin: Origin, start: Int) extends StaticDelim with CantStartStat
  @token class `.`(origin: Origin, start: Int) extends StaticDelim with CantStartStat
  @token class `=`(origin: Origin, start: Int) extends StaticDelim with CantStartStat
  @token class `@`(origin: Origin, start: Int) extends StaticDelim with TypeIntro
  @token class `#`(origin: Origin, start: Int) extends StaticDelim with CantStartStat
  @token class `_ `(origin: Origin, start: Int) extends StaticDelim with ExprIntro with TypeIntro with CanEndStat
  @token class `=>`(origin: Origin, start: Int, end: Int) extends DynamicDelim with CantStartStat { def name = "right arrow" }
  @token class `<-`(origin: Origin, start: Int, end: Int) extends DynamicDelim with CantStartStat { def name = "left arrow" }
  @token class `<:`(origin: Origin, start: Int) extends StaticDelim with CantStartStat
  @token class `>:`(origin: Origin, start: Int) extends StaticDelim with CantStartStat
  @token class `<%`(origin: Origin, start: Int) extends StaticDelim with CantStartStat

  @branch trait Whitespace extends Static with Tok
  @token class ` `(origin: Origin, start: Int) extends Whitespace
  @token class `\t`(origin: Origin, start: Int) extends Whitespace
  @token class `\r`(origin: Origin, start: Int) extends Whitespace
  @token class `\n`(origin: Origin, start: Int) extends Whitespace with StatSep with CantStartStat
  // TODO: \n\n is a virtual token emitted by TokIterator to appease the semi-ported scalac parser
  // it will never occur in a token stream produced by RichOrigin.tokens
  @token class `\n\n`(origin: Origin, start: Int) extends Whitespace with StatSep with CantStartStat
  @token class `\f`(origin: Origin, start: Int) extends Whitespace

  @token class Comment(origin: Origin, start: Int, end: Int) extends Dynamic with Tok { def name = "comment" }

  @token class EOF(origin: Origin) extends Static with StatSep with StatSeqEnd with CaseDefEnd with CantStartStat { def name = "end of file"; override def code = ""; def start = origin.content.length; def end = origin.content.length - 1 }
  @token class XMLStart(origin: Origin, start: Int, end: Int) extends Dynamic with Tok with ExprIntro with CanEndStat { def name = ??? }
}

package scala.meta
package syntactic

import org.scalameta.tokens._
import org.scalameta.default._
import org.scalameta.default.Param._
import scala.reflect.ClassTag
import scala.language.experimental.macros

@root trait Token {
  type ThisType <: Token
  def is[T: ClassTag]: Boolean    = implicitly[ClassTag[T]].runtimeClass.isAssignableFrom(this.getClass)
  def isNot[T: ClassTag]: Boolean = !is[T]
  def input: Input
  def start: Int
  def end: Int
  def adjust(input: Input = this.input, start: Param[Int] = Default, end: Param[Int] = Default, delta: Param[Int] = Default): ThisType
  def name: String
  def code: String = {
    val buf = new StringBuffer
    var i = start
    while (i <= end) {
      buf.append(input.content(i))
      i += 1
    }
    buf.toString
  }
  final override def toString = this.show[Raw]
}

object Token {
  @branch trait Static extends Token
  @branch trait Dynamic extends Token

  @branch trait TypeIntro extends Token
  @branch trait ExprIntro extends Token
  @branch trait DefIntro extends Token
  @branch trait TemplateIntro extends DefIntro
  @branch trait DclIntro extends DefIntro
  @branch trait StatSeqEnd extends Token
  @branch trait CaseDefEnd extends Token

  @branch trait CantStartStat extends Token
  @branch trait CanEndStat extends Token

  @token class Ident(input: Input, start: Int, end: Int) extends Dynamic with ExprIntro with TypeIntro with CanEndStat { def name = "identifier" }

  object Interpolation {
    @token class Id(input: Input, start: Int, end: Int) extends Dynamic with ExprIntro { def name = "interpolation id" }
    @token class Start(input: Input, start: Int, end: Int) extends Dynamic with Token { def name = "interpolation start" }
    @token class Part(input: Input, start: Int, end: Int) extends Dynamic with Token { def name = "interpolation part" }
    @token class SpliceStart(input: Input, start: Int) extends Static with Token { def name = "splice start"; override def code = "$" }
    @token class SpliceEnd(input: Input, start: Int) extends Static with Token { def name = "splice end"; override def code = "" }
    @token class End(input: Input, start: Int, end: Int) extends Dynamic with Token with CanEndStat { def name = "interpolation end" }
  }

  @branch trait Literal extends ExprIntro with CanEndStat
  @branch trait DynamicLiteral extends Dynamic with Literal
  @branch trait NumericLiteral extends DynamicLiteral
  object Literal {
    @token class Int(input: Input, start: scala.Int, end: scala.Int, value: Boolean => scala.Int) extends NumericLiteral { def name = "integer literal" }
    @token class Long(input: Input, start: scala.Int, end: scala.Int, value: Boolean => scala.Long) extends NumericLiteral { def name = "long literal" }
    @token class Float(input: Input, start: scala.Int, end: scala.Int, value: Boolean => scala.Float) extends NumericLiteral { def name = "float literal" }
    @token class Double(input: Input, start: scala.Int, end: scala.Int, value: Boolean => scala.Double) extends NumericLiteral { def name = "double literal" }
    @token class Char(input: Input, start: scala.Int, end: scala.Int, value: scala.Char) extends DynamicLiteral { def name = "character literal" }
    @token class Symbol(input: Input, start: scala.Int, end: scala.Int, value: scala.Symbol) extends DynamicLiteral { def name = "symbol literal" }
    @token class String(input: Input, start: scala.Int, end: scala.Int, value: Predef.String) extends DynamicLiteral { def name = "string literal" }
  }
  @token class `null`(input: Input, start: Int) extends Keyword with Literal
  @token class `true`(input: Input, start: Int) extends Keyword with Literal
  @token class `false`(input: Input, start: Int) extends Keyword with Literal

  @branch trait Keyword extends Static with Token
  @token class `case`(input: Input, start: Int) extends Keyword with CaseDefEnd with TemplateIntro
  @token class `catch`(input: Input, start: Int) extends Keyword with CantStartStat
  @token class `class `(input: Input, start: Int) extends Keyword with TemplateIntro
  @token class `def`(input: Input, start: Int) extends Keyword with DclIntro
  @token class `do`(input: Input, start: Int) extends Keyword with ExprIntro
  @token class `else`(input: Input, start: Int) extends Keyword with CantStartStat
  @token class `extends`(input: Input, start: Int) extends Keyword with CantStartStat
  @token class `finally`(input: Input, start: Int) extends Keyword with CantStartStat
  @token class `for`(input: Input, start: Int) extends Keyword with ExprIntro
  @token class `forSome`(input: Input, start: Int) extends Keyword with CantStartStat
  @token class `if`(input: Input, start: Int) extends Keyword with ExprIntro
  @token class `import`(input: Input, start: Int) extends Keyword
  @token class `match`(input: Input, start: Int) extends Keyword with CantStartStat
  @token class `macro`(input: Input, start: Int) extends Keyword
  @token class `new`(input: Input, start: Int) extends Keyword with ExprIntro
  @token class `object`(input: Input, start: Int) extends Keyword with TemplateIntro
  @token class `package `(input: Input, start: Int) extends Keyword
  @token class `return`(input: Input, start: Int) extends Keyword with ExprIntro with CanEndStat
  @token class `super`(input: Input, start: Int) extends Keyword with ExprIntro with TypeIntro
  @token class `this`(input: Input, start: Int) extends Keyword with ExprIntro with TypeIntro with CanEndStat
  @token class `throw`(input: Input, start: Int) extends Keyword with ExprIntro
  @token class `trait`(input: Input, start: Int) extends Keyword with TemplateIntro
  @token class `try`(input: Input, start: Int) extends Keyword with ExprIntro
  @token class `type`(input: Input, start: Int) extends Keyword with DclIntro with CanEndStat
  @token class `val`(input: Input, start: Int) extends Keyword with DclIntro
  @token class `var`(input: Input, start: Int) extends Keyword with DclIntro
  @token class `while`(input: Input, start: Int) extends Keyword with ExprIntro
  @token class `with`(input: Input, start: Int) extends Keyword with CantStartStat
  @token class `yield`(input: Input, start: Int) extends Keyword with CantStartStat

  @branch trait Modifier extends Keyword
  @branch trait LocalModifier extends Modifier
  @token class `abstract`(input: Input, start: Int) extends LocalModifier
  @token class `final`(input: Input, start: Int) extends LocalModifier
  @token class `sealed`(input: Input, start: Int) extends LocalModifier
  @token class `implicit`(input: Input, start: Int) extends LocalModifier
  @token class `lazy`(input: Input, start: Int) extends LocalModifier
  @token class `private`(input: Input, start: Int) extends Modifier
  @token class `protected`(input: Input, start: Int) extends Modifier
  @token class `override`(input: Input, start: Int) extends Modifier

  @branch trait Delim extends Token
  @branch trait StaticDelim extends Delim with Static
  @branch trait DynamicDelim extends Delim with Static
  @branch trait StatSep extends Delim
  @token class `(`(input: Input, start: Int) extends StaticDelim with ExprIntro with TypeIntro
  @token class `)`(input: Input, start: Int) extends StaticDelim with CantStartStat with CanEndStat
  @token class `[`(input: Input, start: Int) extends StaticDelim with CantStartStat
  @token class `]`(input: Input, start: Int) extends StaticDelim with CantStartStat with CanEndStat
  @token class `{`(input: Input, start: Int) extends StaticDelim with ExprIntro
  @token class `}`(input: Input, start: Int) extends StaticDelim with StatSeqEnd with CaseDefEnd with CantStartStat with CanEndStat
  @token class `,`(input: Input, start: Int) extends StaticDelim with CantStartStat
  @token class `;`(input: Input, start: Int) extends StaticDelim with StatSep with CantStartStat
  @token class `:`(input: Input, start: Int) extends StaticDelim with CantStartStat
  @token class `.`(input: Input, start: Int) extends StaticDelim with CantStartStat
  @token class `=`(input: Input, start: Int) extends StaticDelim with CantStartStat
  @token class `@`(input: Input, start: Int) extends StaticDelim with TypeIntro
  @token class `#`(input: Input, start: Int) extends StaticDelim with CantStartStat
  @token class `_ `(input: Input, start: Int) extends StaticDelim with ExprIntro with TypeIntro with CanEndStat
  @token class `=>`(input: Input, start: Int, end: Int) extends DynamicDelim with CantStartStat { def name = "right arrow" }
  @token class `<-`(input: Input, start: Int, end: Int) extends DynamicDelim with CantStartStat { def name = "left arrow" }
  @token class `<:`(input: Input, start: Int) extends StaticDelim with CantStartStat
  @token class `>:`(input: Input, start: Int) extends StaticDelim with CantStartStat
  @token class `<%`(input: Input, start: Int) extends StaticDelim with CantStartStat

  @branch trait Whitespace extends Static with Token
  @token class ` `(input: Input, start: Int) extends Whitespace
  @token class `\t`(input: Input, start: Int) extends Whitespace
  @token class `\r`(input: Input, start: Int) extends Whitespace
  @token class `\n`(input: Input, start: Int) extends Whitespace with StatSep with CantStartStat
  // TODO: \n\n is a virtual token emitted by TokIterator to appease the semi-ported scalac parser
  // it will never occur in a token stream produced by XtensionInputLike.tokens
  @token class `\n\n`(input: Input, start: Int) extends Whitespace with StatSep with CantStartStat
  @token class `\f`(input: Input, start: Int) extends Whitespace

  @token class Comment(input: Input, start: Int, end: Int) extends Dynamic with Token { def name = "comment" }

  // NOTE: in order to maintain compatibility with scala.reflect's implementation,
  // rank = 1 means .., rank = 2 means ..., etc
  @token class Ellipsis(input: Input, start: Int, end: Int, rank: Int) extends Dynamic with Token { def name = "." * (rank + 1) }

  // TODO: after we bootstrap, Unquote.tree will become scala.meta.Tree
  // however, for now, we will keep it at Any in order to also support scala.reflect trees
  @token class Unquote(input: Input, start: Int, end: Int, tree: Any) extends Dynamic with Token { def name = "unquote " + tree }

  @token class EOF(input: Input) extends Static with StatSep with StatSeqEnd with CaseDefEnd with CantStartStat {
    def name = "end of file"
    override def code = ""
    def start = input.content.length
    def end = input.content.length - 1
  }

  // TODO: implement XML literals
  @token class XMLStart(input: Input, start: Int, end: Int) extends Dynamic with Token with ExprIntro with CanEndStat { def name = ??? }
}

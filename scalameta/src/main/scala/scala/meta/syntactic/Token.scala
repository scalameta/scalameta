package scala.meta
package syntactic

import org.scalameta.adt // NOTE: no underscore import!
import org.scalameta.tokens._
import org.scalameta.default._
import org.scalameta.default.Param._
import scala.reflect.ClassTag
import scala.language.experimental.macros

@root trait Token {
  def prototype: Token.Prototype

  type ThisType <: Token
  def is[T: ClassTag]: Boolean    = implicitly[ClassTag[T]].runtimeClass.isAssignableFrom(this.getClass)
  def isNot[T: ClassTag]: Boolean = !is[T]

  def input: Input
  def dialect: Dialect
  def index: Int
  def prev: Token = if (index > 0) input.tokens(dialect).apply(index - 1) else this
  def next: Token = if (index < input.tokens(dialect).length) input.tokens(dialect).apply(index + 1) else this
  def start: Int
  def end: Int
  def adjust(input: Input = this.input, dialect: Dialect = this.dialect, index: Param[Int] = Default, start: Param[Int] = Default, end: Param[Int] = Default, delta: Param[Int] = Default): ThisType

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

  @branch trait Trivia extends Token
  @branch trait TypeIntro extends Token
  @branch trait ExprIntro extends Token
  @branch trait CaseIntro extends Token
  @branch trait DefIntro extends Token
  @branch trait TemplateIntro extends DefIntro
  @branch trait DclIntro extends DefIntro
  @branch trait StatSeqEnd extends Token
  @branch trait CaseDefEnd extends Token
  @branch trait CantStartStat extends Token
  @branch trait CanEndStat extends Token

  @token class Ident(start: Int, end: Int) extends Dynamic with ExprIntro with TypeIntro with CanEndStat { def name = "identifier" }

  object Interpolation {
    @token class Id(start: Int, end: Int) extends Dynamic with ExprIntro { def name = "interpolation id" }
    @token class Start(start: Int, end: Int) extends Dynamic { def name = "interpolation start" }
    @token class Part(start: Int, end: Int) extends Dynamic { def name = "interpolation part" }
    @token class SpliceStart(start: Int) extends Static { def name = "splice start"; override def code = "$" }
    @token class SpliceEnd(start: Int) extends Static { def name = "splice end"; override def code = "" }
    @token class End(start: Int, end: Int) extends Dynamic with CanEndStat { def name = "interpolation end" }
  }

  @branch trait Literal extends ExprIntro with CanEndStat
  @branch trait StaticLiteral extends Static with Literal
  @branch trait DynamicLiteral extends Dynamic with Literal
  @branch trait NumericLiteral extends DynamicLiteral
  object Literal {
    @token class Int(start: scala.Int, end: scala.Int, value: Boolean => scala.Int) extends NumericLiteral { def name = "integer literal" }
    @token class Long(start: scala.Int, end: scala.Int, value: Boolean => scala.Long) extends NumericLiteral { def name = "long literal" }
    @token class Float(start: scala.Int, end: scala.Int, value: Boolean => scala.Float) extends NumericLiteral { def name = "float literal" }
    @token class Double(start: scala.Int, end: scala.Int, value: Boolean => scala.Double) extends NumericLiteral { def name = "double literal" }
    @token class Char(start: scala.Int, end: scala.Int, value: scala.Char) extends DynamicLiteral { def name = "character literal" }
    @token class Symbol(start: scala.Int, end: scala.Int, value: scala.Symbol) extends DynamicLiteral { def name = "symbol literal" }
    @token class String(start: scala.Int, end: scala.Int, value: Predef.String) extends DynamicLiteral { def name = "string literal" }
  }
  @token class `null`(start: Int) extends Keyword with StaticLiteral
  @token class `true`(start: Int) extends Keyword with StaticLiteral
  @token class `false`(start: Int) extends Keyword with StaticLiteral

  @branch trait Keyword extends Static
  @token class `case`(start: Int) extends Keyword {
    override def is[T: ClassTag]: Boolean = {
      val T = implicitly[ClassTag[T]].runtimeClass
      lazy val caseClassOrCaseObject = {
        def loop(token: Token): Token = if (token.is[Trivia]) loop(token.next) else token
        val nonTrivialNext = loop(this.next)
        nonTrivialNext.is[`class `] || nonTrivialNext.is[`object`]
      }
      if (T == classOf[DefIntro]) caseClassOrCaseObject
      else if (T == classOf[TemplateIntro]) caseClassOrCaseObject
      else if (T == classOf[CaseDefEnd]) !caseClassOrCaseObject
      else if (T == classOf[CaseIntro]) !caseClassOrCaseObject
      else super.is[T]
    }
  }
  @token class `catch`(start: Int) extends Keyword with CantStartStat
  @token class `class `(start: Int) extends Keyword with TemplateIntro
  @token class `def`(start: Int) extends Keyword with DclIntro
  @token class `do`(start: Int) extends Keyword with ExprIntro
  @token class `else`(start: Int) extends Keyword with CantStartStat
  @token class `extends`(start: Int) extends Keyword with CantStartStat
  @token class `finally`(start: Int) extends Keyword with CantStartStat
  @token class `for`(start: Int) extends Keyword with ExprIntro
  @token class `forSome`(start: Int) extends Keyword with CantStartStat
  @token class `if`(start: Int) extends Keyword with ExprIntro
  @token class `import`(start: Int) extends Keyword
  @token class `match`(start: Int) extends Keyword with CantStartStat
  @token class `macro`(start: Int) extends Keyword
  @token class `new`(start: Int) extends Keyword with ExprIntro
  @token class `object`(start: Int) extends Keyword with TemplateIntro
  @token class `package `(start: Int) extends Keyword
  @token class `return`(start: Int) extends Keyword with ExprIntro with CanEndStat
  @token class `super`(start: Int) extends Keyword with ExprIntro with TypeIntro
  @token class `this`(start: Int) extends Keyword with ExprIntro with TypeIntro with CanEndStat
  @token class `throw`(start: Int) extends Keyword with ExprIntro
  @token class `trait`(start: Int) extends Keyword with TemplateIntro
  @token class `try`(start: Int) extends Keyword with ExprIntro
  @token class `type`(start: Int) extends Keyword with DclIntro with CanEndStat
  @token class `val`(start: Int) extends Keyword with DclIntro
  @token class `var`(start: Int) extends Keyword with DclIntro
  @token class `while`(start: Int) extends Keyword with ExprIntro
  @token class `with`(start: Int) extends Keyword with CantStartStat
  @token class `yield`(start: Int) extends Keyword with CantStartStat

  @branch trait Modifier extends Token with DefIntro with TemplateIntro
  @branch trait NonlocalModifier extends Modifier
  @branch trait LocalModifier extends Modifier
  @token class `abstract`(start: Int) extends LocalModifier with Keyword
  @token class `final`(start: Int) extends LocalModifier with Keyword
  @token class `sealed`(start: Int) extends LocalModifier with Keyword
  @token class `implicit`(start: Int) extends LocalModifier with Keyword
  @token class `lazy`(start: Int) extends LocalModifier with Keyword
  @token class `private`(start: Int) extends NonlocalModifier with Keyword
  @token class `protected`(start: Int) extends NonlocalModifier with Keyword
  @token class `override`(start: Int) extends NonlocalModifier with Keyword

  @branch trait Delim extends Token
  @branch trait StaticDelim extends Delim with Static
  @branch trait DynamicDelim extends Delim with Static
  @branch trait StatSep extends Delim
  @token class `(`(start: Int) extends StaticDelim with ExprIntro with TypeIntro
  @token class `)`(start: Int) extends StaticDelim with CantStartStat with CanEndStat
  @token class `[`(start: Int) extends StaticDelim with CantStartStat
  @token class `]`(start: Int) extends StaticDelim with CantStartStat with CanEndStat
  @token class `{`(start: Int) extends StaticDelim with ExprIntro
  @token class `}`(start: Int) extends StaticDelim with StatSeqEnd with CaseDefEnd with CantStartStat with CanEndStat
  @token class `,`(start: Int) extends StaticDelim with CantStartStat
  @token class `;`(start: Int) extends StaticDelim with StatSep with CantStartStat
  @token class `:`(start: Int) extends StaticDelim with CantStartStat
  @token class `.`(start: Int) extends StaticDelim with CantStartStat
  @token class `=`(start: Int) extends StaticDelim with CantStartStat
  @token class `@`(start: Int) extends StaticDelim with TypeIntro with DefIntro with TemplateIntro
  @token class `#`(start: Int) extends StaticDelim with CantStartStat
  @token class `_ `(start: Int) extends StaticDelim with ExprIntro with TypeIntro with CanEndStat
  @token class `=>`(start: Int, end: Int) extends DynamicDelim with CantStartStat { def name = "right arrow" }
  @token class `<-`(start: Int, end: Int) extends DynamicDelim with CantStartStat { def name = "left arrow" }
  @token class `<:`(start: Int) extends StaticDelim with CantStartStat
  @token class `>:`(start: Int) extends StaticDelim with CantStartStat
  @token class `<%`(start: Int) extends StaticDelim with CantStartStat

  @branch trait Whitespace extends Static with Trivia
  @token class ` `(start: Int) extends Whitespace
  @token class `\t`(start: Int) extends Whitespace
  @token class `\r`(start: Int) extends Whitespace
  @token class `\n`(start: Int) extends Whitespace with StatSep with CantStartStat
  // TODO: \n\n is a virtual token emitted by TokIterator to appease the semi-ported scalac parser
  // it will never occur in a token stream produced by XtensionInputLike.tokens
  @token class `\n\n`(start: Int) extends Whitespace with StatSep with CantStartStat
  @token class `\f`(start: Int) extends Whitespace

  @token class Comment(start: Int, end: Int) extends Dynamic with Trivia { def name = "comment" }

  // NOTE: in order to maintain compatibility with scala.reflect's implementation,
  // Ellipsis.rank = 1 means .., Ellipsis.rank = 2 means ..., etc
  @token class Ellipsis(start: Int, end: Int, rank: Int) extends Dynamic {
    def name = "ellipsis"
  }

  // TODO: after we bootstrap, Unquote.tree will become scala.meta.Tree
  // however, for now, we will keep it at Any in order to also support scala.reflect trees
  @token class Unquote(start: Int, end: Int, tree: Any) extends Dynamic {
    def name = "unquote"
    override def is[T: ClassTag]: Boolean = {
      val T = implicitly[ClassTag[T]].runtimeClass
      lazy val nonTrivialNext = {
        def loop(token: Token): Token = if (token.is[Trivia]) loop(token.next) else token
        loop(this.next)
      }
      if (T == classOf[CanEndStat]) true
      else if (T == classOf[CantStartStat]) false
      else if (T == classOf[CaseDefEnd]) false
      else if (T == classOf[Delim]) false
      else if (T == classOf[Keyword]) false
      else if (T == classOf[Literal]) false
      else if (T == classOf[LocalModifier]) false
      else if (T == classOf[Modifier]) false
      else if (T == classOf[NonlocalModifier]) false
      else if (T == classOf[StatSep]) false
      else if (T == classOf[StatSeqEnd]) false
      else if (T == classOf[Whitespace]) false
      else if (T == classOf[CaseIntro]) false
      else if (T == classOf[DclIntro]) nonTrivialNext.is[DclIntro]
      else if (T == classOf[DefIntro]) nonTrivialNext.is[DefIntro]
      else if (T == classOf[TemplateIntro]) nonTrivialNext.is[TemplateIntro]
      else if (T == classOf[ExprIntro]) true
      else if (T == classOf[TypeIntro]) true
      else super.is[T]
    }
  }

  @token class BOF() extends Static {
    def name = "beginning of file"
    override def code = ""
    def start = 0
    def end = -1
  }

  @token class EOF() extends Static with StatSep with StatSeqEnd with CaseDefEnd with CantStartStat {
    def name = "end of file"
    override def code = ""
    def start = input.content.length
    def end = input.content.length - 1
  }

  // TODO: implement XML literals
  @token class XMLStart(start: Int, end: Int) extends Dynamic with ExprIntro with CanEndStat { def name = ??? }

  @adt.root trait Prototype { def input: Input }
  object Prototype {
    @adt.leaf object None extends Prototype { def input = Input.None }
    @adt.leaf class Some(token: Token) extends Prototype { def input = token.input }
  }
}

package scala.meta
package syntactic.parsers

sealed trait Tok {
  def isIdent: Boolean         = this.isInstanceOf[Tok.Ident]
  def isLit: Boolean           = this.isInstanceOf[Tok.Lit]
  def isNumericLit: Boolean    = this.isInstanceOf[Tok.NumericLit]
  def isKeyword: Boolean       = this.isInstanceOf[Tok.Keyword]
  def isMod: Boolean           = this.isInstanceOf[Tok.Mod]
  def isLocalMod: Boolean      = this.isInstanceOf[Tok.LocalMod]
  def isExprIntro: Boolean     = this.isInstanceOf[Tok.ExprIntro]
  def isTemplateIntro: Boolean = this.isInstanceOf[Tok.TemplateIntro]
  def isTypeIntro: Boolean     = this.isInstanceOf[Tok.TypeIntro]
  def isDclIntro: Boolean      = this.isInstanceOf[Tok.DclIntro]
  def isStatSep: Boolean       = this.isInstanceOf[Tok.StatSep]
  def isStatSeqEnd: Boolean    = this.isInstanceOf[Tok.StatSeqEnd]
  def isCaseDefEnd: Boolean    = this.isInstanceOf[Tok.CaseDefEnd]
}
object Tok {
  sealed trait TypeIntro extends Tok
  sealed trait ExprIntro extends Tok
  sealed trait TemplateIntro extends Tok
  sealed trait DclIntro extends Tok
  sealed trait StatSeqEnd extends Tok
  sealed trait CaseDefEnd extends Tok

  final case class Ident(value: Predef.String, isBackquoted: Boolean) extends ExprIntro with TypeIntro
  final case class InterpolationId(value: Predef.String) extends ExprIntro
  final case class StringPart(value: Predef.String) extends Tok  

  sealed trait Lit extends ExprIntro
  sealed trait NumericLit extends Lit
  final case class Int(value: scala.Int) extends NumericLit
  final case class Long(value: scala.Long) extends NumericLit
  final case class Float(value: scala.Float) extends NumericLit
  final case class Double(value: scala.Double) extends NumericLit
  final case class Char(value: scala.Char) extends Lit
  final case class Symbol(value: scala.Symbol) extends Lit
  final case class String(value: Predef.String) extends Lit
  case object Null extends Keyword with Lit
  case object True extends Keyword with Lit
  case object False extends Keyword with Lit

  sealed trait Keyword extends Tok
  case object Case extends Keyword with CaseDefEnd
  case object CaseClass extends Keyword with TemplateIntro
  case object CaseObject extends Keyword with TemplateIntro
  case object Catch extends Keyword
  case object Class extends Keyword with TemplateIntro
  case object Def extends Keyword with DclIntro
  case object Do extends Keyword with ExprIntro
  case object Else extends Keyword
  case object Extends extends Keyword
  case object Finally extends Keyword
  case object For extends Keyword with ExprIntro
  case object ForSome extends Keyword
  case object If extends Keyword with ExprIntro
  case object Import extends Keyword
  case object Match extends Keyword
  case object Macro extends Keyword
  case object New extends Keyword with ExprIntro
  case object Object extends Keyword with TemplateIntro
  case object Package extends Keyword
  case object Return extends Keyword with ExprIntro
  case object Super extends Keyword with ExprIntro with TypeIntro
  case object This extends Keyword with ExprIntro with TypeIntro
  case object Throw extends Keyword with ExprIntro
  case object Trait extends Keyword with TemplateIntro
  case object Try extends Keyword with ExprIntro
  case object Type extends Keyword with DclIntro
  case object Val extends Keyword with DclIntro
  case object Var extends Keyword with DclIntro
  case object While extends Keyword with ExprIntro
  case object With extends Keyword
  case object Yield extends Keyword

  sealed trait Mod extends Keyword
  sealed trait LocalMod extends Mod
  case object Abstract extends LocalMod
  case object Final extends LocalMod
  case object Sealed extends LocalMod
  case object Implicit extends LocalMod
  case object Lazy extends LocalMod
  case object Private extends Mod
  case object Protected extends Mod
  case object Override extends Mod

  sealed trait Delim extends Tok
  sealed trait StatSep extends Delim
  case object `(` extends Delim with ExprIntro with TypeIntro
  case object `)` extends Delim
  case object `[` extends Delim
  case object `]` extends Delim
  case object `{` extends Delim with ExprIntro
  case object `}` extends Delim with StatSeqEnd with CaseDefEnd
  case object `,` extends Delim
  case object `;` extends StatSep
  case object `:` extends Delim
  case object `.` extends Delim
  case object `=` extends Delim
  case object `@` extends Delim with TypeIntro
  case object `#` extends Delim
  case object `_` extends Delim with ExprIntro with TypeIntro
  case object `=>` extends Delim
  case object `<-` extends Delim
  case object `<:` extends Delim
  case object `>:` extends Delim
  case object `<%` extends Delim
  case object `\n` extends StatSep
  case object `\n\n` extends StatSep
  case object EOF extends StatSep with StatSeqEnd with CaseDefEnd

  case object Empty extends Tok
  case object Undef extends Tok
  case object Error extends Tok
  case object Ignore extends Tok
  case object Escape extends Tok
  case object XMLStart extends Tok with ExprIntro
  case object `\\` extends Tok
  case object ` ` extends Tok
}
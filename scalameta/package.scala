package scala

import scala.language.implicitConversions
import org.scalameta.adt._
import org.scalameta.annotations._
import org.scalameta.convert._

package object meta {
  @quasiquote[Stat]('q)             implicit class TermQuote(ctx: StringContext)
  @quasiquote[Type.Arg]('t)         implicit class TypeQuote(ctx: StringContext)
  @quasiquote[Pat.Arg]('p)          implicit class PatternQuote(ctx: StringContext)

  @quasiquote[Tree]('param)         implicit class ParamQuote(ctx: StringContext)
  @quasiquote[Term.Arg]('arg)       implicit class ArgQuote(ctx: StringContext)
  @quasiquote[Enum]('enum)          implicit class EnumQuote(ctx: StringContext)
  @quasiquote[Mod]('mod)            implicit class ModQuote(ctx: StringContext)
  @quasiquote[Aux.Case]('cas)       implicit class CaseQuote(ctx: StringContext)
  @quasiquote[Aux.Parent]('parent)  implicit class ParentQuote(ctx: StringContext)
  @quasiquote[Aux.Template]('templ) implicit class TemplateQuote(ctx: StringContext)

  final case class MetaException(msg: String) extends Exception(msg)
}

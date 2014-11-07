package scala

import scala.language.implicitConversions
import org.scalameta.adt._
import org.scalameta.annotations._
import org.scalameta.convert._

package object meta {
  @quasiquote[Stat]('q)            implicit class TermQuote(ctx: StringContext)
  @quasiquote[Type.Arg]('t)        implicit class TypeQuote(ctx: StringContext)
  @quasiquote[Pat.Arg]('p)         implicit class PatternQuote(ctx: StringContext)
  @quasiquote[Templ.Param]('param) implicit class ParamQuote(ctx: StringContext)
  @quasiquote[Type.Param]('tparam) implicit class TparamQuote(ctx: StringContext)
  @quasiquote[Term.Arg]('arg)      implicit class ArgQuote(ctx: StringContext)
  @quasiquote[Enum]('enum)         implicit class EnumQuote(ctx: StringContext)
  @quasiquote[Mod]('mod)           implicit class ModQuote(ctx: StringContext)
  @quasiquote[Templ]('templ)       implicit class TemplateQuote(ctx: StringContext)
  @quasiquote[Ctor.Ref]('ctorref)  implicit class CtorRefQuote(ctx: StringContext)
  @quasiquote[Selector]('sel)      implicit class SelectorQuote(ctx: StringContext)
  @quasiquote[Case]('cas)          implicit class CaseQuote(ctx: StringContext)

  final case class MetaException(msg: String) extends Exception(msg)
}

package scala

import scala.language.implicitConversions
import org.scalameta.adt._
import org.scalameta.annotations._
import org.scalameta.convert._

package object meta {
  // TODO: it would be ideal to have these as annotations on corresponding AST nodes
  // e.g. instead of `@branch trait Stat extends Tree`
  // we will have `@quasiquote('q) @branch trait Stat extends Tree`
  // that would probably allow us for every AST node to have an associated quasiquote interpolator in the doc
  // upd. this might also require non-local macro expansion because of hierarchical structure of the `scala.meta` package
  // (if we annotate scala.meta.package.Type.Arg, we need to somehow put the TypeQuote implicit class into scala.meta.package)
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

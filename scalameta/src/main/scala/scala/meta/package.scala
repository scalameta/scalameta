package scala

import org.scalameta.annotations._
import scala.meta._
import scala.meta.macros.{Api => MacroApi}
import scala.meta.syntactic.{Api => SyntacticApi}
import scala.meta.semantic.{Api => SemanticApi}
import scala.meta.ui.{Api => UIApi}

package object meta extends MacroApi with SyntacticApi with SemanticApi with UIApi {
  // TODO: it would be ideal to have these as annotations on corresponding AST nodes
  // e.g. instead of `@branch trait Stat extends Tree`
  // we will have `@quasiquote('q) @branch trait Stat extends Tree`
  // that would probably allow us for every AST node to have an associated quasiquote interpolator in the doc
  // upd. this might also require non-local macro expansion because of hierarchical structure of the `scala.meta` package
  // (if we annotate scala.meta.package.Type.Arg, we need to somehow put the TypeQuote implicit class into scala.meta.package)
  @quasiquote[Stat]('q)                implicit class TermQuote(ctx: StringContext)
  @quasiquote[Term.Arg]('arg)          implicit class TermArgQuote(ctx: StringContext)
  @quasiquote[Term.Param]('param)      implicit class TermParamQuote(ctx: StringContext)
  @quasiquote[Type.Arg]('t)            implicit class TypeQuote(ctx: StringContext)
  @quasiquote[Type.Param]('tparam)     implicit class TypeParamQuote(ctx: StringContext)
  @quasiquote[Case, Pat.Arg]('p)       implicit class CaseOrPatternQuote(ctx: StringContext)
  @quasiquote[Pat.Type]('pt)           implicit class PatternTypeQuote(ctx: StringContext)
  @quasiquote[Ctor.Ref, Term]('ctor)   implicit class CtorQuote(ctx: StringContext)
  @quasiquote[Template]('template)     implicit class TemplateQuote(ctx: StringContext)
  @quasiquote[Mod]('mod)               implicit class ModQuote(ctx: StringContext)
  @quasiquote[Enumerator]('enumerator) implicit class EnumeratorQuote(ctx: StringContext)
  @quasiquote[Importee]('importee)     implicit class ImporteeQuote(ctx: StringContext)
  @quasiquote[Source]('source)         implicit class SourceQuote(ctx: StringContext)
}

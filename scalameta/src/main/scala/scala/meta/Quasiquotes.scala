package scala.meta

import org.scalameta.ast.{quasiquote => astQuasiquote}
import org.scalameta.tokens.{quasiquote => tokenQuasiquote}
import scala.meta._

private[meta] trait Quasiquotes {
  // TODO: it would be ideal to have these as annotations on corresponding AST nodes
  // e.g. instead of `@branch trait Stat extends Tree`
  // we will have `@quasiquote('q) @branch trait Stat extends Tree`
  // that would probably allow us for every AST node to have an associated quasiquote interpolator in the doc
  // upd. this might also require non-local macro expansion because of hierarchical structure of the `scala.meta` package
  // (if we annotate scala.meta.package.Type.Arg, we need to somehow put the TypeQuote implicit class into scala.meta.package)
  // TODO: overloading Case and Pat.Arg within p"..." is probably not the best idea
  // however, cas"..." is so ugly that I'm willing to be conceptually impure here
  @astQuasiquote[Stat]('q)                implicit class XtensionQuasiquoteTerm(ctx: StringContext)
  @astQuasiquote[Term.Arg]('arg)          implicit class XtensionQuasiquoteTermArg(ctx: StringContext)
  @astQuasiquote[Term.Param]('param)      implicit class XtensionQuasiquoteTermParam(ctx: StringContext)
  @astQuasiquote[Type.Arg]('t)            implicit class XtensionQuasiquoteType(ctx: StringContext)
  @astQuasiquote[Type.Param]('tparam)     implicit class XtensionQuasiquoteTypeParam(ctx: StringContext)
  @astQuasiquote[Case, Pat.Arg]('p)       implicit class XtensionQuasiquoteCaseOrPattern(ctx: StringContext)
  @astQuasiquote[Pat.Type]('pt)           implicit class XtensionQuasiquotePatternType(ctx: StringContext)
  @astQuasiquote[Ctor.Ref, Term]('ctor)   implicit class XtensionQuasiquoteCtor(ctx: StringContext)
  @astQuasiquote[Template]('template)     implicit class XtensionQuasiquoteTemplate(ctx: StringContext)
  @astQuasiquote[Mod]('mod)               implicit class XtensionQuasiquoteMod(ctx: StringContext)
  @astQuasiquote[Enumerator]('enumerator) implicit class XtensionQuasiquoteEnumerator(ctx: StringContext)
  @astQuasiquote[Importee]('importee)     implicit class XtensionQuasiquoteImportee(ctx: StringContext)
  @astQuasiquote[Source]('source)         implicit class XtensionQuasiquoteSource(ctx: StringContext)
  @tokenQuasiquote('toks)                 implicit class XtensionQuasiquoteTokens(ctx: StringContext)
}

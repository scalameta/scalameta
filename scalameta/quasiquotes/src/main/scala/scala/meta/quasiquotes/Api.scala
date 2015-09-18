package scala.meta
package quasiquotes

import org.scalameta.ast._

private[meta] trait Api {
  // TODO: it would be ideal to have these as annotations on corresponding AST nodes
  // e.g. instead of `@branch trait Stat extends Tree`
  // we will have `@quasiquote('q) @branch trait Stat extends Tree`
  // that would probably allow us for every AST node to have an associated quasiquote interpolator in the doc
  // upd. this might also require non-local macro expansion because of hierarchical structure of the `scala.meta` package
  // (if we annotate scala.meta.package.Type.Arg, we need to somehow put the TypeQuote implicit class into scala.meta.package)
  // TODO: overloading Case and Pat.Arg within p"..." is probably not the best idea
  // however, cas"..." is so ugly that I'm willing to be conceptually impure here
  @quasiquote[Stat]('q)                implicit class XtensionQuasiquoteTerm(ctx: StringContext)
  @quasiquote[Term.Arg]('arg)          implicit class XtensionQuasiquoteTermArg(ctx: StringContext)
  @quasiquote[Term.Param]('param)      implicit class XtensionQuasiquoteTermParam(ctx: StringContext)
  @quasiquote[Type.Arg]('t)            implicit class XtensionQuasiquoteType(ctx: StringContext)
  @quasiquote[Type.Param]('tparam)     implicit class XtensionQuasiquoteTypeParam(ctx: StringContext)
  @quasiquote[Case, Pat.Arg]('p)       implicit class XtensionQuasiquoteCaseOrPattern(ctx: StringContext)
  @quasiquote[Pat.Type]('pt)           implicit class XtensionQuasiquotePatternType(ctx: StringContext)
  @quasiquote[Ctor.Call]('ctor)        implicit class XtensionQuasiquoteCtor(ctx: StringContext)
  @quasiquote[Template]('template)     implicit class XtensionQuasiquoteTemplate(ctx: StringContext)
  @quasiquote[Mod]('mod)               implicit class XtensionQuasiquoteMod(ctx: StringContext)
  @quasiquote[Enumerator]('enumerator) implicit class XtensionQuasiquoteEnumerator(ctx: StringContext)
  @quasiquote[Importee]('importee)     implicit class XtensionQuasiquoteImportee(ctx: StringContext)
  @quasiquote[Source]('source)         implicit class XtensionQuasiquoteSource(ctx: StringContext)
}

private[meta] trait Aliases {
  type Lift[O, I] = scala.meta.quasiquotes.Lift[O, I]
  val Lift = scala.meta.quasiquotes.Lift

  type Unlift[I, O] = scala.meta.quasiquotes.Unlift[I, O]
  val Unlift = scala.meta.quasiquotes.Unlift
}

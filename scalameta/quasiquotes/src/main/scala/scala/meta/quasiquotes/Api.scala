package scala.meta
package quasiquotes

import scala.meta.internal.ast.quasiquote

private[meta] trait Api {
  // TODO: it would be ideal to have these as annotations on corresponding AST nodes
  // e.g. instead of `@branch trait Stat extends Tree`
  // we will have `@quasiquote('q) @branch trait Stat extends Tree`
  // that would probably allow us for every AST node to have an associated quasiquote interpolator in the doc
  // upd. this might also require non-local macro expansion because of hierarchical structure of the `scala.meta` package
  // (if we annotate scala.meta.package.Type.Arg, we need to somehow put the TypeQuote implicit class into scala.meta.package)
  // TODO: overloading Case and Pat.Arg within p"..." is probably not the best idea
  // however, cas"..." is so ugly that I'm willing to be conceptually impure here
  @quasiquote[Ctor, Stat]('q)          implicit class XtensionQuasiquoteTerm(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Term.Arg]('arg)          implicit class XtensionQuasiquoteTermArg(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Term.Param]('param)      implicit class XtensionQuasiquoteTermParam(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Type]('t)                implicit class XtensionQuasiquoteType(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Type.Arg]('targ)         implicit class XtensionQuasiquoteTypeArg(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Type.Param]('tparam)     implicit class XtensionQuasiquoteTypeParam(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Case, Pat]('p)           implicit class XtensionQuasiquoteCaseOrPattern(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Pat.Arg]('parg)          implicit class XtensionQuasiquotePatternArg(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Pat.Type]('pt)           implicit class XtensionQuasiquotePatternType(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Ctor.Call]('ctor)        implicit class XtensionQuasiquoteCtor(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Template]('template)     implicit class XtensionQuasiquoteTemplate(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Mod]('mod)               implicit class XtensionQuasiquoteMod(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Enumerator]('enumerator) implicit class XtensionQuasiquoteEnumerator(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Importer]('importer)     implicit class XtensionQuasiquoteImporter(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Importee]('importee)     implicit class XtensionQuasiquoteImportee(ctx: StringContext) extends QuasiquoteParsers
  @quasiquote[Source]('source)         implicit class XtensionQuasiquoteSource(ctx: StringContext) extends QuasiquoteParsers
}

private[meta] trait Aliases {
  type Lift[O, I] = scala.meta.quasiquotes.Lift[O, I]
  lazy val Lift = scala.meta.quasiquotes.Lift

  type Unlift[I, O] = scala.meta.quasiquotes.Unlift[I, O]
  lazy val Unlift = scala.meta.quasiquotes.Unlift
}

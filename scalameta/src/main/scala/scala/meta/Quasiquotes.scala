package scala
package meta

import org.scalameta.annotations._
import scala.meta._

private[meta] trait Quasiquotes {
  // TODO: it would be ideal to have these as annotations on corresponding AST nodes
  // e.g. instead of `@branch trait Stat extends Tree`
  // we will have `@quasiquote('q) @branch trait Stat extends Tree`
  // that would probably allow us for every AST node to have an associated quasiquote interpolator in the doc
  // upd. this might also require non-local macro expansion because of hierarchical structure of the `scala.meta` package
  // (if we annotate scala.meta.package.Type.Arg, we need to somehow put the TypeQuote implicit class into scala.meta.package)
  @quasiquote[Stat]('q)                implicit class XtensionQuasiquoteTerm(ctx: StringContext)
  @quasiquote[Term.Arg]('arg)          implicit class XtensionQuasiquoteTermArg(ctx: StringContext)
  @quasiquote[Term.Param]('param)      implicit class XtensionQuasiquoteTermParam(ctx: StringContext)
  @quasiquote[Type.Arg]('t)            implicit class XtensionQuasiquoteType(ctx: StringContext)
  @quasiquote[Type.Param]('tparam)     implicit class XtensionQuasiquoteTypeParam(ctx: StringContext)
  @quasiquote[Case, Pat.Arg]('p)       implicit class XtensionQuasiquoteCaseOrPattern(ctx: StringContext)
  @quasiquote[Pat.Type]('pt)           implicit class XtensionQuasiquotePatternType(ctx: StringContext)
  @quasiquote[Ctor.Ref, Term]('ctor)   implicit class XtensionQuasiquoteCtor(ctx: StringContext)
  @quasiquote[Template]('template)     implicit class XtensionQuasiquoteTemplate(ctx: StringContext)
  @quasiquote[Mod]('mod)               implicit class XtensionQuasiquoteMod(ctx: StringContext)
  @quasiquote[Enumerator]('enumerator) implicit class XtensionQuasiquoteEnumerator(ctx: StringContext)
  @quasiquote[Importee]('importee)     implicit class XtensionQuasiquoteImportee(ctx: StringContext)
  @quasiquote[Source]('source)         implicit class XtensionQuasiquoteSource(ctx: StringContext)
}
package scala.meta
package quasiquotes

import scala.meta.internal.trees.quasiquote

private[meta] trait Api {
  @quasiquote[Ctor, Stat]('q)          implicit class XtensionQuasiquoteTerm(ctx: StringContext)
  @quasiquote[Term.Param]('param)      implicit class XtensionQuasiquoteTermParam(ctx: StringContext)
  @quasiquote[Type]('t)                implicit class XtensionQuasiquoteType(ctx: StringContext)
  @quasiquote[Type.Param]('tparam)     implicit class XtensionQuasiquoteTypeParam(ctx: StringContext)
  @quasiquote[Case, Pat]('p)           implicit class XtensionQuasiquoteCaseOrPattern(ctx: StringContext)
  @quasiquote[Init]('init)             implicit class XtensionQuasiquoteInit(ctx: StringContext)
  @quasiquote[Self]('self)             implicit class XtensionQuasiquoteSelf(ctx: StringContext)
  @quasiquote[Template]('template)     implicit class XtensionQuasiquoteTemplate(ctx: StringContext)
  @quasiquote[Mod]('mod)               implicit class XtensionQuasiquoteMod(ctx: StringContext)
  @quasiquote[Enumerator]('enumerator) implicit class XtensionQuasiquoteEnumerator(ctx: StringContext)
  @quasiquote[Importer]('importer)     implicit class XtensionQuasiquoteImporter(ctx: StringContext)
  @quasiquote[Importee]('importee)     implicit class XtensionQuasiquoteImportee(ctx: StringContext)
  @quasiquote[Source]('source)         implicit class XtensionQuasiquoteSource(ctx: StringContext)
}

private[meta] trait Aliases {
  type Lift[O, I] = scala.meta.quasiquotes.Lift[O, I]
  lazy val Lift = scala.meta.quasiquotes.Lift

  type Unlift[I, O] = scala.meta.quasiquotes.Unlift[I, O]
  lazy val Unlift = scala.meta.quasiquotes.Unlift
}

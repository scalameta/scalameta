package scala.meta
package quasiquotes

import scala.meta.internal.trees.quasiquote
import scala.meta.parsers.Parse
import scala.meta.internal.quasiquotes._

type Qunapply
object Qunapply

private[meta] trait Api {
  // val a: Ctor
  // @quasiquote[Ctor, Stat]('q)
  // implicit class XtensionQuasiquoteTerm(ctx: StringContext)
  // @quasiquote[Term.Param]('param)
  // implicit class XtensionQuasiquoteTermParam(ctx: StringContext)
  // @quasiquote[Type]('t)
  // implicit class XtensionQuasiquoteType(ctx: StringContext)
  // @quasiquote[Type.Param]('tparam)
  // implicit class XtensionQuasiquoteTypeParam(ctx: StringContext)
  // @quasiquote[Case, Pat]('p)
  // implicit class XtensionQuasiquoteCaseOrPattern(ctx: StringContext)
  // @quasiquote[Init]('init)
  // implicit class XtensionQuasiquoteInit(ctx: StringContext)
  // @quasiquote[Self]('self)
  // implicit class XtensionQuasiquoteSelf(ctx: StringContext)
  // @quasiquote[Template]('template)
  // implicit class XtensionQuasiquoteTemplate(ctx: StringContext)
  // @quasiquote[Mod]('mod)
  // implicit class XtensionQuasiquoteMod(ctx: StringContext)
  // @quasiquote[Enumerator]('enumerator)
  // implicit class XtensionQuasiquoteEnumerator(ctx: StringContext)
  // @quasiquote[Importer]('importer)
  // implicit class XtensionQuasiquoteImporter(ctx: StringContext)
  // @quasiquote[Importee]('importee)
  // implicit class XtensionQuasiquoteImportee(ctx: StringContext)
  // @quasiquote[Source]('source)
  // implicit class XtensionQuasiquoteSource(ctx: StringContext)
  extension (stringContext: scala.StringContext)
    @annotation.compileTimeOnly(".q should not be called directly. Use q\"...\" string interpolation.")
    def q: Qunapply = ???
  extension (inline sc: Qunapply)
    transparent inline def unapply(inline scrutinee: Any)(implicit dialect: scala.meta.Dialect): Option[Seq[Any]] =
      ${ ReificationMacros.unapplyQuasiquoteImpl('sc, 'scrutinee, 'dialect) }

  extension (inline sc: StringContext) {
    transparent inline def q(inline args: Any*)(implicit dialect: scala.meta.Dialect) = ${ ReificationMacros.quasiquoteImpl('sc, 'args, 'dialect) }
    transparent inline def t(inline args: Any*)(implicit dialect: scala.meta.Dialect) = ${ ReificationMacros.typeImpl('sc, 'args, 'dialect) }
    transparent inline def p(inline args: Any*)(implicit dialect: scala.meta.Dialect) = ${ ReificationMacros.caseOrPatternImpl('sc, 'args, 'dialect) }
  }

  object XTensionQuasiquoteTerm {
    private[meta] def parse(input: scala.meta.inputs.Input, dialect: scala.meta.Dialect) = {
      val parse = implicitly[Parse[Ctor]];
      parse(input, dialect)
    }.orElse{
      val parse = implicitly[Parse[Stat]];
      parse(input, dialect)
    } match {
      case x: scala.meta.parsers.Parsed.Success[_] => x.tree
      case x: scala.meta.parsers.Parsed.Error => throw x.details
    }
  }
  object XTensionQuasiquoteType {
    private[meta] def parse(input: scala.meta.inputs.Input, dialect: scala.meta.Dialect) = {
      val parse = implicitly[Parse[Type]];
      parse(input, dialect)
    } match {
      case x: scala.meta.parsers.Parsed.Success[_] => x.tree
      case x: scala.meta.parsers.Parsed.Error => throw x.details
    }
  }
  object XTensionQuasiquoteCaseOrPattern {
    private[meta] def parse(input: scala.meta.inputs.Input, dialect: scala.meta.Dialect) = {
      val parse = implicitly[Parse[Case]];
      parse(input, dialect)
    }.orElse{
      val parse = implicitly[Parse[Pat]];
      parse(input, dialect)
    } match {
      case x: scala.meta.parsers.Parsed.Success[_] => x.tree
      case x: scala.meta.parsers.Parsed.Error => throw x.details
    }
  }
}

private[meta] trait Aliases {
  type Lift[O, I] = scala.meta.quasiquotes.Lift[O, I]
  lazy val Lift = scala.meta.quasiquotes.Lift

  type Unlift[I, O] = scala.meta.quasiquotes.Unlift[I, O]
  lazy val Unlift = scala.meta.quasiquotes.Unlift
}

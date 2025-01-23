package scala.meta
package quasiquotes

import scala.meta._
import scala.meta.internal.quasiquotes._
import scala.meta.internal.trees.quasiquote
import scala.meta.parsers.Parse

type QuasiquoteUnapply

private[meta] trait Api {

  // apply methods
  extension (inline sc: StringContext) {
    transparent inline def q(inline args: Any*): Tree = ${ ReificationMacros.statImpl('sc, 'args) }
    transparent inline def param(inline args: Any*): Term.Param =
      ${ ReificationMacros.termParamImpl('sc, 'args) }
    transparent inline def t(inline args: Any*): Type = ${ ReificationMacros.typeImpl('sc, 'args) }
    transparent inline def tparam(inline args: Any*): Type.Param =
      ${ ReificationMacros.typeParamImpl('sc, 'args) }
    transparent inline def p(inline args: Any*): Tree =
      ${ ReificationMacros.caseOrPatternImpl('sc, 'args) }
    transparent inline def init(inline args: Any*): Init = ${ ReificationMacros.initImpl('sc, 'args) }
    transparent inline def self(inline args: Any*): Self = ${ ReificationMacros.selfImpl('sc, 'args) }
    transparent inline def template(inline args: Any*): Template =
      ${ ReificationMacros.templateImpl('sc, 'args) }
    transparent inline def mod(inline args: Any*): Mod = ${ ReificationMacros.modImpl('sc, 'args) }
    transparent inline def enumerator(inline args: Any*): Enumerator =
      ${ ReificationMacros.enumeratorImpl('sc, 'args) }
    transparent inline def importer(inline args: Any*): Importer =
      ${ ReificationMacros.importerImpl('sc, 'args) }
    transparent inline def importee(inline args: Any*): Importee =
      ${ ReificationMacros.importeeImpl('sc, 'args) }
    transparent inline def source(inline args: Any*): Source =
      ${ ReificationMacros.sourceImpl('sc, 'args) }
  }

  // unapply methods
  extension (stringContext: scala.StringContext)
    @annotation.compileTimeOnly(
      ".q should not be called directly. Use q\"...\" string interpolation."
    )
    def q: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".param should not be called directly. Use param\"...\" string interpolation."
    )
    def param: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".t should not be called directly. Use t\"...\" string interpolation."
    )
    def t: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".tparam should not be called directly. Use tparam\"...\" string interpolation."
    )
    def tparam: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".p should not be called directly. Use p\"...\" string interpolation."
    )
    def p: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".init should not be called directly. Use init\"...\" string interpolation."
    )
    def init: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".self should not be called directly. Use self\"...\" string interpolation."
    )
    def self: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".template should not be called directly. Use template\"...\" string interpolation."
    )
    def template: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".mod should not be called directly. Use mod\"...\" string interpolation."
    )
    def mod: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".enumerator should not be called directly. Use enumerator\"...\" string interpolation."
    )
    def enumerator: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".importer should not be called directly. Use importer\"...\" string interpolation."
    )
    def importer: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".importee should not be called directly. Use importee\"...\" string interpolation."
    )
    def importee: QuasiquoteUnapply = ???
    @annotation.compileTimeOnly(
      ".source should not be called directly. Use source\"...\" string interpolation."
    )
    def source: QuasiquoteUnapply = ???

  extension (inline sc: QuasiquoteUnapply)
    transparent inline def unapply(scrutinee: Any): Any =
      ${ ReificationMacros.unapplyImpl('sc, 'scrutinee) }

  // parsers
  object XTensionQuasiquoteStat {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api
      .parseAny[Ctor, Stat]
  }
  object XTensionQuasiquoteTermParam {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api
      .parseAny[Term.Param]
  }
  object XTensionQuasiquoteType {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api.parseAny[Type]
  }
  object XTensionQuasiquoteTypeParam {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api
      .parseAny[Type.Param]
  }
  object XTensionQuasiquoteCaseOrPattern {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api.parseAny[Case, Pat]
  }
  object XTensionQuasiquoteInit {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api.parseAny[Init]
  }
  object XTensionQuasiquoteSelf {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api.parseAny[Self]
  }
  object XTensionQuasiquoteTemplate {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api.parseAny[Template]
  }
  object XTensionQuasiquoteMod {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api.parseAny[Mod]
  }
  object XTensionQuasiquoteEnumerator {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api
      .parseAny[Enumerator]
  }
  object XTensionQuasiquoteImporter {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api.parseAny[Importer]
  }
  object XTensionQuasiquoteImportee {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api.parseAny[Importee]
  }
  object XTensionQuasiquoteSource {
    private[meta] def parse(implicit input: inputs.Input, dialect: Dialect) = Api.parseAny[Source]
  }
}

private[meta] object Api {
  def parse[A <: Tree](implicit input: inputs.Input, dialect: Dialect, parser: Parse[A]) =
    parser(input, dialect)

  def parseAny[A <: Tree, B <: Tree](implicit
      input: inputs.Input,
      dialect: Dialect,
      parserA: Parse[A],
      parserB: Parse[B]
  ) = parse[A].orElse(parse[B]).get

  def parseAny[A <: Tree](implicit input: inputs.Input, dialect: Dialect, parser: Parse[A]) =
    parse[A].get
}

private[meta] trait Aliases {
  type Lift[O, I] = scala.meta.quasiquotes.Lift[O, I]
  lazy val Lift = scala.meta.quasiquotes.Lift

  type Unlift[I, O] = scala.meta.quasiquotes.Unlift[I, O]
  lazy val Unlift = scala.meta.quasiquotes.Unlift
}

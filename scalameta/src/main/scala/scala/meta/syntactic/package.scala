package scala.meta

import scala.meta.syntactic.parsers._
import scala.meta.syntactic.tokenizers._
import scala.meta.syntactic.quasiquotes._
import org.scalameta.annotations._
import org.scalameta.convert._

package object syntactic {
  // ===========================
  // PART 1: QUASIQUOTES
  // ===========================
  // TODO: it would be ideal to have these as annotations on corresponding AST nodes
  // e.g. instead of `@branch trait Stat extends Tree`
  // we will have `@quasiquote('q) @branch trait Stat extends Tree`
  // that would probably allow us for every AST node to have an associated quasiquote interpolator in the doc
  // upd. this might also require non-local macro expansion because of hierarchical structure of the `scala.meta` package
  // (if we annotate scala.meta.package.Type.Arg, we need to somehow put the TypeQuote implicit class into scala.meta.package)
  @quasiquote[Stat]('q)                 implicit class TermQuote(ctx: StringContext)
  @quasiquote[Term.Arg]('arg)           implicit class TermArgQuote(ctx: StringContext)
  @quasiquote[Term.Param]('param)       implicit class TermParamQuote(ctx: StringContext)
  @quasiquote[Type.Arg]('t)             implicit class TypeQuote(ctx: StringContext)
  @quasiquote[Type.Param]('tparam)      implicit class TypeParamQuote(ctx: StringContext)
  @quasiquote[Case, Pat.Arg]('p)        implicit class CaseOrPatternQuote(ctx: StringContext)
  @quasiquote[Ctor.Ref, Term]('ctorref) implicit class CtorRefQuote(ctx: StringContext)
  @quasiquote[Template]('template)      implicit class TemplateQuote(ctx: StringContext)
  @quasiquote[Mod]('mod)                implicit class ModQuote(ctx: StringContext)
  @quasiquote[Enumerator]('enumerator)  implicit class EnumeratorQuote(ctx: StringContext)
  @quasiquote[Importee]('importee)      implicit class ImporteeQuote(ctx: StringContext)
  @quasiquote[Source]('source)          implicit class SourceQuote(ctx: StringContext)

  // ===========================
  // PART 2: PARSING
  // ===========================
  type Token = scala.meta.syntactic.tokenizers.Token
  val Token = scala.meta.syntactic.tokenizers.Token

  trait Parse[T] extends Convert[Origin, T]
  object Parse {
    def apply[T](f: Origin => T): Parse[T] = new Parse[T] { def apply(origin: Origin): T = f(origin) }
    implicit def parseStat(implicit dialect: Dialect): Parse[Stat] = apply(origin => new Parser(origin).parseStat())
    implicit def parseStats(implicit dialect: Dialect): Parse[List[Stat]] = apply(origin => new Parser(origin).parseStats())
    implicit def parseTerm(implicit dialect: Dialect): Parse[Term] = apply(origin => new Parser(origin).parseTerm())
    implicit def parseTermArg(implicit dialect: Dialect): Parse[Term.Arg] = apply(origin => new Parser(origin).parseTermArg())
    implicit def parseTermParam(implicit dialect: Dialect): Parse[Term.Param] = apply(origin => new Parser(origin).parseTermParam())
    implicit def parseType(implicit dialect: Dialect): Parse[Type] = apply(origin => new Parser(origin).parseType())
    implicit def parseTypeArg(implicit dialect: Dialect): Parse[Type.Arg] = apply(origin => new Parser(origin).parseTypeArg())
    implicit def parseTypeParam(implicit dialect: Dialect): Parse[Type.Param] = apply(origin => new Parser(origin).parseTypeParam())
    implicit def parsePat(implicit dialect: Dialect): Parse[Pat] = apply(origin => new Parser(origin).parsePat())
    implicit def parsePatArg(implicit dialect: Dialect): Parse[Pat.Arg] = apply(origin => new Parser(origin).parsePatArg())
    implicit def parseCase(implicit dialect: Dialect): Parse[Case] = apply(origin => new Parser(origin).parseCase())
    implicit def parseCtorRef(implicit dialect: Dialect): Parse[Ctor.Ref] = apply(origin => new Parser(origin).parseCtorRef())
    implicit def parseTemplate(implicit dialect: Dialect): Parse[Template] = apply(origin => new Parser(origin).parseTemplate())
    implicit def parseMod(implicit dialect: Dialect): Parse[Mod] = apply(origin => new Parser(origin).parseMod())
    implicit def parseEnumerator(implicit dialect: Dialect): Parse[Enumerator] = apply(origin => new Parser(origin).parseEnumerator())
    implicit def parseImportee(implicit dialect: Dialect): Parse[Importee] = apply(origin => new Parser(origin).parseImportee())
    implicit def parseSource(implicit dialect: Dialect): Parse[Source] = apply(origin => new Parser(origin).parseSource())
  }

  implicit class RichOrigin[T](val originLike: T)(implicit ev: Convert[T, Origin], dialect: Dialect) {
    private lazy val origin = ev(originLike)
    def parse[T: Parse](implicit ev: Parse[T]): T = ev(origin)
    def tokens: Vector[Token] = tokenize(origin)
  }
}

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
  @quasiquote[Stat]('q)             implicit class TermQuote(ctx: StringContext)
  @quasiquote[Type.Arg]('t)         implicit class TypeQuote(ctx: StringContext)
  @quasiquote[Pat.Arg]('p)          implicit class PatternQuote(ctx: StringContext)
  @quasiquote[Templ.Param]('param)  implicit class ParamQuote(ctx: StringContext)
  @quasiquote[Type.Param]('tparam)  implicit class TparamQuote(ctx: StringContext)
  @quasiquote[Term.Arg]('arg)       implicit class ArgQuote(ctx: StringContext)
  @quasiquote[Enum]('enum)          implicit class EnumQuote(ctx: StringContext)
  @quasiquote[Mod]('mod)            implicit class ModQuote(ctx: StringContext)
  @quasiquote[Templ]('templ)        implicit class TemplateQuote(ctx: StringContext)
  @quasiquote[Ctor.Ref]('ctorref)   implicit class CtorRefQuote(ctx: StringContext)
  @quasiquote[Selector]('importsel) implicit class SelectorQuote(ctx: StringContext)
  @quasiquote[Case]('cas)           implicit class CaseQuote(ctx: StringContext)
  @quasiquote[Source]('source)      implicit class SourceQuote(ctx: StringContext)

  // ===========================
  // PART 2: PARSING
  // ===========================
  type Token = scala.meta.syntactic.tokenizers.Token
  val Token = scala.meta.syntactic.tokenizers.Token

  trait Parse[T] extends Convert[Origin, T]
  object Parse {
    def apply[T](f: Origin => T): Parse[T] = new Parse[T] { def apply(origin: Origin): T = f(origin) }
    implicit def parseSource(implicit dialect: Dialect): Parse[Source] = apply(origin => new Parser(origin).parseSource())
    implicit def parseTerm(implicit dialect: Dialect): Parse[Term] = apply(origin => new Parser(origin).parseTerm())
    implicit def parseType(implicit dialect: Dialect): Parse[Type.Arg] = apply(origin => new Parser(origin).parseType())
    implicit def parsePat(implicit dialect: Dialect): Parse[Pat.Arg] = apply(origin => new Parser(origin).parsePat())
    implicit def parseStat(implicit dialect: Dialect): Parse[Stat] = apply(origin => new Parser(origin).parseStat())
    implicit def parseStats(implicit dialect: Dialect): Parse[List[Stat]] = apply(origin => new Parser(origin).parseStats())
    implicit def parseParam(implicit dialect: Dialect): Parse[Templ.Param] = apply(origin => new Parser(origin).parseParam())
    implicit def parseTparam(implicit dialect: Dialect): Parse[Type.Param] = apply(origin => new Parser(origin).parseTparam())
    implicit def parseTermArg(implicit dialect: Dialect): Parse[Term.Arg] = apply(origin => new Parser(origin).parseTermArg())
    implicit def parseEnum(implicit dialect: Dialect): Parse[Enum] = apply(origin => new Parser(origin).parseEnum())
    implicit def parseMod(implicit dialect: Dialect): Parse[Mod] = apply(origin => new Parser(origin).parseMod())
    implicit def parseTempl(implicit dialect: Dialect): Parse[Templ] = apply(origin => new Parser(origin).parseTempl())
    implicit def parseCtorRef(implicit dialect: Dialect): Parse[Ctor.Ref] = apply(origin => new Parser(origin).parseCtorRef())
    implicit def parseSelector(implicit dialect: Dialect): Parse[Selector] = apply(origin => new Parser(origin).parseSelector())
    implicit def parseCase(implicit dialect: Dialect): Parse[Case] = apply(origin => new Parser(origin).parseCase())
  }

  implicit class RichOrigin[T](val originLike: T)(implicit ev: Convert[T, Origin], dialect: Dialect) {
    private lazy val origin = ev(originLike)
    def parse[T: Parse](implicit ev: Parse[T]): T = ev(origin)
    def tokens: Vector[Token] = tokenize(origin)
  }
}

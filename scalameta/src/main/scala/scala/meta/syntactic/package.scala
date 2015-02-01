package scala.meta

import scala.meta.syntactic.parsers._
import scala.meta.syntactic.tokenizers._
import scala.meta.syntactic.quasiquotes._
import org.scalameta.annotations._
import org.scalameta.convert._

package object syntactic {
  // ===========================
  // PART 1: PARSING
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

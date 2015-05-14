package scala.meta
package syntactic

import scala.meta.internal.parsers._
import scala.meta.internal.tokenizers._
import org.scalameta.annotations._
import org.scalameta.convert._
import org.scalameta.invariants._
import scala.annotation.implicitNotFound

private[meta] trait Api {
  type Input = scala.meta.syntactic.Input
  val Input = scala.meta.syntactic.Input

  type Position = scala.meta.syntactic.Position
  val Position = scala.meta.syntactic.Position

  type Point = scala.meta.syntactic.Point
  val Point = scala.meta.syntactic.Point

  type Token = scala.meta.syntactic.Token
  val Token = scala.meta.syntactic.Token

  type Tokens = scala.meta.syntactic.Tokens
  val Tokens = scala.meta.syntactic.Tokens
  implicit class XtensionTokens(tokens: Seq[Token]) {
    def toTokens: Tokens = Tokens(tokens: _*)
  }

  // ===========================
  // PART 1: PARSING
  // ===========================
  @implicitNotFound(msg = "don't know how to parse ${T} (if you're sure that ${T} is parseable, double-check that you've imported a dialect, e.g. scala.meta.dialects.Scala211)")
  trait Parse[T] extends Convert[Input, T]
  object Parse {
    def apply[T](f: Input => T): Parse[T] = new Parse[T] { def apply(input: Input): T = f(input) }
    implicit def parseStat(implicit dialect: Dialect): Parse[Stat] = apply(input => new Parser(input).parseStat())
    implicit def parseTerm(implicit dialect: Dialect): Parse[Term] = apply(input => new Parser(input).parseTerm())
    implicit def parseTermArg(implicit dialect: Dialect): Parse[Term.Arg] = apply(input => new Parser(input).parseTermArg())
    implicit def parseTermParam(implicit dialect: Dialect): Parse[Term.Param] = apply(input => new Parser(input).parseTermParam())
    implicit def parseType(implicit dialect: Dialect): Parse[Type] = apply(input => new Parser(input).parseType())
    implicit def parseTypeArg(implicit dialect: Dialect): Parse[Type.Arg] = apply(input => new Parser(input).parseTypeArg())
    implicit def parseTypeParam(implicit dialect: Dialect): Parse[Type.Param] = apply(input => new Parser(input).parseTypeParam())
    implicit def parsePat(implicit dialect: Dialect): Parse[Pat] = apply(input => new Parser(input).parsePat())
    implicit def parsePatArg(implicit dialect: Dialect): Parse[Pat.Arg] = apply(input => new Parser(input).parsePatArg())
    implicit def parsePatType(implicit dialect: Dialect): Parse[Pat.Type] = apply(input => new Parser(input).parsePatType())
    implicit def parseCase(implicit dialect: Dialect): Parse[Case] = apply(input => new Parser(input).parseCase())
    implicit def parseCtorRef(implicit dialect: Dialect): Parse[Ctor.Ref] = apply(input => new Parser(input).parseCtorRef())
    implicit def parseTemplate(implicit dialect: Dialect): Parse[Template] = apply(input => new Parser(input).parseTemplate())
    implicit def parseMod(implicit dialect: Dialect): Parse[Mod] = apply(input => new Parser(input).parseMod())
    implicit def parseEnumerator(implicit dialect: Dialect): Parse[Enumerator] = apply(input => new Parser(input).parseEnumerator())
    implicit def parseImportee(implicit dialect: Dialect): Parse[Importee] = apply(input => new Parser(input).parseImportee())
    implicit def parseSource(implicit dialect: Dialect): Parse[Source] = apply(input => new Parser(input).parseSource())
  }

  implicit class XtensionInputLike[T](inputLike: T) {
    def parse[U](implicit convert: Convert[T, Input], dialect: Dialect, parse: Parse[U]): U = parse(convert(inputLike))
    def tokens(implicit convert: Convert[T, Input], dialect: Dialect): Tokens = convert(inputLike).tokens
  }

  implicit class XtensionSyntacticTree(tree: Tree) {
    def input = tree.tokens.input
    def dialect = tree.tokens.dialect
    def position = {
      // NOTE: can't do Position(tree.tokens.head, tree.tokens.last) because of two reasons:
      // 1) a tree can have no tokens (e.g. a synthetic primary constructor), but we still need to compute a position from it
      // 2) if a tree is parsed from Input.Virtual, then we can't really say that it has any position
      // NOTE: if you're changing this, make sure that you don't degrade
      // `time parse ~/Projects/core/scalameta/src/main/scala/scala/meta/internal/parsers/Parsers.scala`
      // I wish we had better benchmarking facilities, of course, but that's what we have right now
      if (tree.tokens.isInstanceOf[Tokens.Real]) {
        val tokens = tree.tokens.asInstanceOf[Tokens.Real]
        Position.Real(tokens.input, tokens.head, tokens.last)
      } else if (tree.tokens.isInstanceOf[Tokens.Slice]) {
        val tokens = tree.tokens.asInstanceOf[Tokens.Slice]
        val basis = tokens.tokens.asInstanceOf[Tokens.Real]
        Position.Real(basis.input, basis(tokens.from), basis(tokens.until - 1))
      } else {
        Position.Virtual(Input.Virtual(tree.tokens))
      }
    }
    def start = tree.position.start
    def end = tree.position.end
  }
}

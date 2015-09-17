package scala.meta
package syntactic

import org.scalameta.convert._

private[meta] trait SyntacticApi {
  implicit class XtensionSyntacticTree(tree: Tree) {
    def input: Input = tree.tokens.input
    def dialect: Dialect = tree.tokens.dialect
    def position: Position = {
      // NOTE: can't do Position(tree.tokens.head, tree.tokens.last) because of two reasons:
      // 1) a tree can have no tokens (e.g. a synthetic primary constructor), but we still need to compute a position from it
      // 2) if a tree is parsed from Input.Virtual, then we can't really say that it has any position
      // TODO: compute something sensible for Position.point
      // TODO: WorkaroundTokens is necessary, because otherwise we get a patmat warning
      import scala.meta.syntactic.{Tokens => WorkaroundTokens}
      tree.tokens match {
        case WorkaroundTokens.Slice(WorkaroundTokens.Tokenized(content, _, tokens @ _*), from, until) =>
          Position.Range(content, tokens(from).position.start, tokens(from).position.start, tokens(until - 1).position.end)
        case other @ WorkaroundTokens.Slice(basis, from, _) =>
          Position.Assorted(other, basis(from).position.start)
        case WorkaroundTokens.Tokenized(content, _, tokens @ _*) =>
          Position.Range(content, tokens.head.position.start, tokens.head.position.start, tokens.last.position.end)
        case other =>
          Position.Assorted(other, other.head.position.start)
      }
    }
    def start: Point = tree.position.start
    def point: Point = tree.position.point
    def end: Point = tree.position.end
  }
}

private[meta] trait GenericParseApi {
  type ParseException = scala.meta.syntactic.ParseException
  val ParseException = scala.meta.syntactic.ParseException

  implicit class XtensionParseInputLike[T](inputLike: T) {
    def parse[U](implicit convert: Convert[T, Input], parse: Parse[U]): U = {
      parse(convert(inputLike))
    }
  }
}

object api extends SyntacticApi
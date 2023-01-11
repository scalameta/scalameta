package scala.meta.internal.parsers

import scala.meta.tokens.Token

private[parsers] class TokenRef private (
    val regions: List[SepRegion],
    val token: Token,
    val pos: Int,
    val nextPos: Int,
    val pointPos: Int
) {
  def withRegions(regions: List[SepRegion]): TokenRef =
    new TokenRef(regions, token, pos, nextPos, pointPos)
}

private[parsers] object TokenRef {
  def apply(
      regions: List[SepRegion],
      token: Token,
      pos: Int
  ): TokenRef =
    apply(regions, token, pos, pos + 1, pos)
  def apply(
      regions: List[SepRegion],
      token: Token,
      pos: Int,
      nextPos: Int,
      pointPos: Int
  ): TokenRef =
    new TokenRef(regions, token, pos, nextPos, pointPos)
}

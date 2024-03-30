package scala.meta.internal.parsers

import scala.meta.tokens.Token

private[parsers] class TokenRef private (
    val regions: List[SepRegion],
    val token: Token,
    val pos: Int,
    val nextPos: Int,
    val pointPos: Int,
    var next: TokenRef = null
) {
  def withRegions(regions: List[SepRegion]): TokenRef =
    if (regions eq this.regions) this else new TokenRef(regions, token, pos, nextPos, pointPos)
}

private[parsers] object TokenRef {
  def apply(regions: List[SepRegion], token: Token, pos: Int, next: TokenRef): TokenRef =
    apply(regions, token, pos, pos + 1, pos, next)
  def apply(
      regions: List[SepRegion],
      token: Token,
      pos: Int,
      nextPos: Int,
      pointPos: Int,
      next: TokenRef = null
  ): TokenRef = new TokenRef(regions, token, pos, nextPos, pointPos, next)
}

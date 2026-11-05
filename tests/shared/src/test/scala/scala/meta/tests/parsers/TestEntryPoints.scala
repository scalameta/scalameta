package scala.meta.tests.parsers

import scala.meta.parsers.Parse

private[meta] object TestEntryPoints {

  val Expr = Parse.toParse(p => p.expr())
  val Pattern = Parse.toParse(p => p.pattern())
  val PatternTyp = Parse.toParse(p => p.patternTyp())
  val Typ = Parse.toParse(p => p.typ())
  val TopStat = Parse.toParse(p => p.statSeq(p.topStat).head)
  val TemplateStat = Parse.toParse(p => p.statSeq(p.templateStat).head)
  val BlockStat = Parse.toParse(p => p.blockStatSeq().head)
  val Source = Parse.toParse(p => p.source())

}

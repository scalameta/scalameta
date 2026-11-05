package scala.meta.tests.parsers

import scala.meta._
import scala.meta.parsers.Parse

private[meta] object TestEntryPoints {

  val Expr = Parse.toParse[Term](classOf[Term], p => p.expr())
  val Pattern = Parse.toParse[Pat](classOf[Pat], p => p.pattern())
  val PatternTyp = Parse.toParse[Type](classOf[Type], p => p.patternTyp())
  val Typ = Parse.toParse[Type](classOf[Type], p => p.typ())
  val TopStat = Parse.toParse[Stat](classOf[Stat], p => p.statSeq(p.topStat).head)
  val TemplateStat = Parse.toParse[Stat](classOf[Stat], p => p.statSeq(p.templateStat).head)
  val BlockStat = Parse.toParse[Stat](classOf[Stat], p => p.blockStatSeq().head)
  val Source = Parse.toParse[Source](classOf[Source], p => p.source())

}

package scala.meta.testkit

import scala.collection.mutable
import scala.meta._
import scala.meta.parsers.Parsed
import org.scalatest.FunSuiteLike

object ScalametaParserProperties {

  type ParserBug = Observation[BugKind]
  private val ParserBug = Observation.apply[BugKind] _

  sealed abstract class BugKind

  /** The following property did not hold:
    * scalac can parse file implies that scala.meta can parse file
    **/
  case object ParserBroken extends BugKind

  /** The following property did not hold:
    * syntheticTree.structure == synthethicTree.syntax.parse.structure
    **/
  case object PrettyPrinterBroken extends BugKind

  def onParseSuccess(source: Source): List[ParserBug] = {
    val syntheticTree = Source(source.stats) // simple trick to remove origin.
    syntheticTree.syntax.parse[Source] match {
      case Parsed.Success(parsedFromSyntheticTree) =>
        StructurallyEqual(syntheticTree, parsedFromSyntheticTree) match {
          case Left(err) =>
            List(
              ParserBug(err.mismatchClass,
                        err.lineNumber,
                        PrettyPrinterBroken))
          case _ => Nil
        }
      case _ =>
        List(ParserBug("can't parse", 0, PrettyPrinterBroken))
    }
  }

  def onParseError(scalaFile: CorpusFile, err: Parsed.Error): List[ParserBug] =
    if (ScalacParser.canParseInput(scalaFile.read))
      List(ParserBug(err.details.getMessage, err.pos.startLine, ParserBroken))
    else Nil

  def runAnalysis(corpusSize: Int = Int.MaxValue) = {
    val corpus =
      Corpus
        .files(Corpus.fastparse)
        .take(corpusSize)
        .toBuffer
        .par
    SyntaxAnalysis.run[ParserBug](corpus) { file =>
      file.jFile.parse[Source] match {
        case Parsed.Success(s) => onParseSuccess(s)
        case e: Parsed.Error => onParseError(file, e)
      }
    }
  }

  def runAndPrintAnalysis(): Unit = {
    val result = runAnalysis(100)
    val markdown = Observation.markdownTable(result.toList)
    println(markdown)
  }

  def main(args: Array[String]): Unit = {
    runAndPrintAnalysis()
  }
}

object ScalametaParserPropertyTest extends FunSuiteLike {
  import ScalametaParserProperties._
  def main(args: Array[String]): Unit = {
    val result = runAnalysis()
    val parserBroken = result.count(_._2.kind == ParserBroken)
    val prettyPrinterBroken = result.count(_._2.kind == PrettyPrinterBroken)
    println(s"""Parser broken: $parserBroken
               |Pretty printer broken: $prettyPrinterBroken""".stripMargin)
    assert(parserBroken <= 4)
    assert(prettyPrinterBroken <= 1888)
  }
}

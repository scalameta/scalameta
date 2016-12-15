package scala.meta.testkit

import scala.meta._
import scala.meta.parsers.Parsed

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

  def onParseSuccess(source: Source): Seq[ParserBug] = {
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

  def onParseError(scalaFile: CorpusFile, err: Parsed.Error): Seq[ParserBug] =
    if (ScalacParser.canParseInput(scalaFile.read))
      Seq(ParserBug(err.details.getMessage, err.pos.start.line, ParserBroken))
    else Nil

  def runAndPrintAnalysis(): Unit = {
    val corpus =
      Corpus
        .files(Corpus.fastparse)
        .take(100) // configure size of experiment
        .toBuffer
        .par
    val result =
      SyntaxAnalysis.run[ParserBug](corpus) { file =>
        file.jFile.parse[Source] match {
          case Parsed.Success(s) => onParseSuccess(s)
          case e: Parsed.Error => onParseError(file, e)
        }
      }
    val markdown = Observation.markdownTable(result)
    println(markdown)
  }

  def main(args: Array[String]): Unit = {
    runAndPrintAnalysis()
  }
}

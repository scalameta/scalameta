package scala.meta.testkit

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

  case class XmlLitCount(expr: Int, pat: Int) {
    def +(that: XmlLitCount) = XmlLitCount(expr + that.expr, pat + that.pat)
  }

  def runXmlAnalysis(corpusSize: Int = Int.MaxValue) = {
    val corpus =
      Corpus
        .files(Corpus.fastparse)
        .take(corpusSize)
        .toBuffer
        .par

    SyntaxAnalysis.run[XmlLitCount](corpus) { file =>
      file.jFile.parse[Source] match {
        case Parsed.Success(s) =>
          var expr = 0
          var pat = 0
          s.traverse {
            case _: Term.Xml => expr += 1
            case _: Pat.Xml => pat += 1
          }
          Seq(XmlLitCount(expr, pat))

        case e: Parsed.Error =>
          Nil
      }
    }
  }

  def runAndPrintAnalysis(): Unit = {
    val result = runAnalysis(100)
    val markdown = Observation.markdownTable(result)
    println(markdown)
  }

  def main(args: Array[String]): Unit = {
    runAndPrintAnalysis()
  }
}

object ScalametaParserPropertyTest extends FunSuiteLike {
  import ScalametaParserProperties._
//  def main(args: Array[String]): Unit = {
//    val result = runAnalysis()
//    val parserProken = result.count(_._2.kind == ParserBroken)
//    val prettyPrinterBroken = result.count(_._2.kind == PrettyPrinterBroken)
//    println(s"""Parser broken: $parserProken
//               |Pretty printer broken: $prettyPrinterBroken""".stripMargin)
//    assert(parserProken <= 7)
//    assert(prettyPrinterBroken <= 1922)
//  }

  def main(args: Array[String]): Unit = {
    val result = runXmlAnalysis()
    val xmlCount = result.foldLeft(XmlLitCount(0, 0)) {
      case (acc, (_, count)) => acc + count
    }
    println(s"Xml expr: ${xmlCount.expr}")
    println(s"Xml pat: ${xmlCount.pat}")
  }
}

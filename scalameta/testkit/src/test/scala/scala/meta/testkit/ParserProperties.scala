package scala.meta.testkit

import scala.meta._
import scala.meta.parsers.Parsed
import scala.meta.testkit.StructurallyEqual.TreeDiff

object ParserProperties {
  sealed abstract class ParsePropertyViolation(val msg: String, val line: Int) {
    def msgInCode: String =
      s"<code>${msg.replaceAll("\n", "</br>")}</code>"
  }

  /** The following property did not hold:
    * scalac can parse file implies that scala.meta can parse file
    **/
  class ParserBroken(msg: String, line: Int)
      extends ParsePropertyViolation(msg, line)

  /** The following property did not hold:
    * syntheticTree.structure == synthethicTree.syntax.parse.structure
    **/
  class PrettyPrinterBroken(err: String, line: Int)
      extends ParsePropertyViolation(err, line)

  def onParseSuccess(source: Source): Seq[ParsePropertyViolation] = {
    val syntheticTree = Source(source.stats) // simple trick to remove origin.
    syntheticTree.syntax.parse[Source] match {
      case Parsed.Success(parsedFromSyntheticTree) =>
        StructurallyEqual(syntheticTree, parsedFromSyntheticTree) match {
          case Left(err) =>
            List(new PrettyPrinterBroken(err.mismatchClass, err.lineNumber))
          case _ => Nil
        }
      case _ => List(new PrettyPrinterBroken("can't parse", 0))
    }
  }

  def onParseError(scalaFile: ScalaFile,
                   err: Parsed.Error): Seq[ParsePropertyViolation] =
    if (ScalacParser.canParseInput(scalaFile.read))
      Seq(new ParserBroken(err.details.getMessage, err.pos.start.line))
    else Nil

  def manual(filename: String): Unit = {
    val contents = FileOps.getFile("target", "repos", filename)
    val source = contents.parse[Source].get
    onParseSuccess(source) match {
      case err :: Nil => println(err)
      case Nil => println("OK")
    }
  }

  def testProperties(): Unit = {
    val corpus = MillionsOfLinesOfScalaCode
      .files()
      .filter(x =>
        MillionsOfLinesOfScalaCode.fastparseCorpusApproved(x.jFile.getPath))
      .take(100000)
      .toArray
      .par
    val result =
      SyntaxAnalysis.run[ParsePropertyViolation](corpus)(onParseSuccess,
                                                         onParseError)
    val grouped = result.groupBy(_._2.getClass.getSimpleName)
    grouped.foreach {
      case (cat, rs) =>
        println(s"## $cat")
        println(s"URL | details |")
        println(s"--- | --- |")
        rs.sortBy(_._2.msg).foreach {
          case (f, x) =>
            println(s"${f.githubUrlAtLine(x.line)} | ${x.msgInCode}")
        }
    }
    grouped.foreach {
      case (cat, rs) =>
        println(s"$cat: ${rs.length}")
    }
  }

  def main(args: Array[String]): Unit = {
    testProperties()
//    manual( "akka/akka-actor/src/main/scala/akka/actor/FaultHandling.scala" )
  }

}

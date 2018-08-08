package scala.meta.testkit

import org.scalatest.FunSuiteLike
import org.scalatest.exceptions.TestFailedException
import collection.JavaConverters._
import org.scalactic.source.Position
import org.scalatest.exceptions.StackDepthException

trait DiffAssertions extends FunSuiteLike {

  def assertNoDiffOrPrintExpected(obtained: String, expected: String, title: String = "")(
      implicit source: Position): Boolean = {
    try assertNoDiff(obtained, expected, title)
    catch {
      case ex: Exception =>
        obtained.lines.toList match {
          case head +: tail =>
            println("    \"\"\"|" + head)
            tail.foreach(line => println("       |" + line))
          case head +: Nil =>
            println(head)
          case Nil =>
            println("obtained is empty")
        }
        throw ex
    }
  }

  def assertNoDiff(obtained: String, expected: String, title: String = "")(
      implicit source: Position): Boolean = {
    if (obtained.isEmpty && !expected.isEmpty) fail("Obtained empty output!")
    val result = compareContents(obtained, expected)
    if (result.isEmpty) true
    else {
      throw DiffFailure(title, expected, obtained, result, source)
    }
  }

  private def header[T](t: T): String = {
    val line = s"=" * (t.toString.length + 3)
    s"$line\n=> $t\n$line"
  }

  private case class DiffFailure(
      title: String,
      expected: String,
      obtained: String,
      diff: String,
      source: Position
  ) extends TestFailedException(
        { _: StackDepthException =>
          Some(title + "\n" + error2message(obtained, expected))
        },
        None,
        source
      )

  private def error2message(obtained: String, expected: String): String = {
    val sb = new StringBuilder
    if (obtained.length < 1000) {
      sb.append(
        s"""#${header("Obtained")}
            #${stripTrailingWhitespace(obtained)}
            #
            #""".stripMargin('#')
      )
    }
    sb.append(
      s"""#${header("Diff")}
          #${stripTrailingWhitespace(compareContents(obtained, expected))}""".stripMargin('#')
    )
    sb.toString()
  }

  private def stripTrailingWhitespace(str: String): String = str.replaceAll(" \n", "∙\n")

  private def splitIntoLines(string: String): Seq[String] =
    string.trim.replace("\r\n", "\n").split("\n")

  private def compareContents(original: String, revised: String): String =
    compareContents(splitIntoLines(original), splitIntoLines(revised))

  private def compareContents(original: Seq[String], revised: Seq[String]): String = {
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
    if (diff.getDeltas.isEmpty) ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
          "obtained",
          "expected",
          original.asJava,
          diff,
          1
        )
        .asScala
        .mkString("\n")
  }

}

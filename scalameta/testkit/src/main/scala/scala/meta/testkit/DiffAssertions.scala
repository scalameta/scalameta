package scala.meta.testkit

import java.util
import org.scalatest.FunSuiteLike
import org.scalatest.exceptions.TestFailedException
import collection.JavaConverters._

object DiffAssertions extends DiffAssertions {
  override def trimLines: Boolean = false
}
trait DiffAssertions extends FunSuiteLike {

  // NOTE: Not trimming lines may cause problems on AppVeyor.
  protected def trimLines: Boolean = true

  def assertNoDiff(obtained: String, expected: String, title: String = ""): Boolean = {
    if (obtained.isEmpty && !expected.isEmpty) fail("Obtained empty output!")
    val result = compareContents(obtained, expected)
    if (result.isEmpty) true
    else {
      throw DiffFailure(title, expected, obtained, result)
    }
  }

  private def header[T](t: T): String = {
    val line = s"=" * (t.toString.length + 3)
    s"$line\n=> $t\n$line"
  }

  private case class DiffFailure(title: String, expected: String, obtained: String, diff: String)
      extends TestFailedException(title + "\n" + error2message(obtained, expected), 3)

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

  private def compareContents(original: String, revised: String): String =
    compareContents(original.trim.split("\n"), revised.trim.split("\n"))

  private def trim(lines: Seq[String]): util.List[String] = {
    if (trimLines) lines.map(_.trim).asJava
    else lines.asJava
  }

  private def compareContents(original: Seq[String], revised: Seq[String]): String = {
    val diff = difflib.DiffUtils.diff(trim(original), trim(revised))
    if (diff.getDeltas.isEmpty) ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
          "original",
          "revised",
          original.asJava,
          diff,
          1
        )
        .asScala
        .drop(3)
        .mkString("\n")
  }

}

package scala.meta.tests

import scala.meta.internal.debug.logger

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import java.util.TimeZone

import org.scalatest.FunSuiteLike
import org.scalatest.exceptions.TestFailedException
import org.scalautils.Equality

/**
  * Produces fancy diffs for scalatest === string equality checks.
  *
  * To use, create a class like this:
  *
  * {{{
  *   class MySuite extends FunSuite with DiffAssertions
  * }}}
  *
  * Before:
  * {{{
  *   [info]   "...ar</foo> [0..14)
  *   [info]    [1[5..15])
  *   [info]     [14..15)
  *   [info]   EOF [15..." did not equal "...ar</foo> [0..14)
  *   [info]    [1[4..14])
  *   [info]     [14..15)
  *   [info]   EOF [15..." (TokenizerSuite.scala:824)
  *
  * }}}
  *
  * After:
  * {{{
  *   [info]   ===========
  *   [info]   => Obtained
  *   [info]   ===========
  *   [info]   BOF [0..0)
  *   [info]    [0..0)
  *   [info]   <foo>bar</foo> [0..14)
  *   [info]    [15..15)
  *   [info]     [14..15)
  *   [info]   EOF [15..15)
  *   [info]
  *   [info]   =======
  *   [info]   => Diff
  *   [info]   =======
  *   [info]    <foo>bar</foo> [0..14)
  *   [info]   - [15..15)
  *   [info]   + [14..14)
  *   [info]      [14..15) (TokenizerSuite.scala:13)
  * }}}
  */
trait DiffAssertions extends FunSuiteLike {

  implicit val strEquals = new Equality[String] {
    override def areEqual(a: String, b: Any): Boolean = b match {
      case b: String =>
        assertNoDiff(a, b)
      case _ =>
        false
    }
  }

  case class DiffFailure(title: String,
                         expected: String,
                         obtained: String,
                         diff: String)
      extends TestFailedException(
        title + "\n" + error2message(obtained, expected),
        1)

  def error2message(obtained: String, expected: String): String = {
    val sb = new StringBuilder
    if (obtained.length < 1000) {
      sb.append(s"""
                   #${logger.header("Obtained")}
                   #${trailingSpace(obtained)}
         """.stripMargin('#'))
    }
    sb.append(s"""
                 #${logger.header("Diff")}
                 #${trailingSpace(compareContents(obtained, expected))}
         """.stripMargin('#'))
    sb.toString()
  }

  def assertNoDiff(obtained: String,
                   expected: String,
                   title: String = ""): Boolean = {
    val result = compareContents(obtained, expected)
    if (result.isEmpty) true
    else {
      throw DiffFailure(title, expected, obtained, result)
    }
  }

  def trailingSpace(str: String): String = str.replaceAll(" \n", "∙\n")

  def compareContents(original: String, revised: String): String = {
    compareContents(original.trim.split("\n"), revised.trim.split("\n"))
  }

  def compareContents(original: Seq[String], revised: Seq[String]): String = {
    import collection.JavaConverters._
    val diff = difflib.DiffUtils.diff(original.asJava, revised.asJava)
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

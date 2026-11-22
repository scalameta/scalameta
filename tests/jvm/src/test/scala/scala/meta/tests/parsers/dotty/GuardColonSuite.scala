package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.inputs.InputException
import scala.meta.tests.BuildInfo

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import scala.util.control.NonFatal

/** Pins the parse of each input in `guard-colon.txt`, one line per dialect. */
class GuardColonSuite extends BaseDottySuite {

  import GuardColonSuite._

  test("guard-colon.expect")(assertNoDiff(obtained, read(ExpectFile)))

  private def parse(code: String)(implicit dialect: Dialect): String =
    try oneLine(templStat(code).reprint)
    catch {
      case e: InputException =>
        val pos = e.getPos
        s"error ${pos.startLine + 1}:${pos.startColumn + 1}: ${e.getShortMessage}"
      case NonFatal(e) => oneLine(s"exception $e")
    }

  private def oneLine(text: String): String = text.nl2lf.replace("\n", "\\n")

  def obtained: String = {
    val sb = new StringBuilder
    read(InputsFile).split("(?m)^==== ").iterator.drop(1).foreach { input =>
      val eol = input.indexOf('\n')
      val id = input.substring(0, eol)
      val code = input.substring(eol + 1)
      Dialects.foreach { case (name, dialect) =>
        sb.append(s"$id $name: ${parse(code)(dialect)}\n")
      }
    }
    sb.toString
  }

}

object GuardColonSuite {

  private val Dialects = List("Scala3" -> dialects.Scala3, "Scala32" -> dialects.Scala32)

  private val InputsFile = Paths.get(BuildInfo.resourcesDirectory, "guard-colon.txt")
  private val ExpectFile = InputsFile.resolveSibling("guard-colon.expect")

  private def read(path: Path): String = new String(Files.readAllBytes(path), UTF_8)

  /** Rewrites the expect file from today's output. `sbt save-expect` runs it. */
  def main(args: Array[String]): Unit = Files
    .write(ExpectFile, new GuardColonSuite().obtained.getBytes(UTF_8))

}

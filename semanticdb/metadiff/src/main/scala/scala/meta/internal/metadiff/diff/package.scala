package scala.meta.internal.metadiff

import difflib.DiffUtils
import java.util.regex.Pattern
import scala.collection.JavaConverters._

package object diff {

  val EOL = System.lineSeparator()

  type Diff = Option[String]

  implicit class DiffOps(diff: Diff) {

    def name(name: String): Diff =
      diff.map { d =>
        val headerBar = "=" * name.length
        s"$name$EOL" +
          s"$headerBar$EOL" +
          d
      }

    def concat(that: Diff): Diff = diffConcat(diff, that)

    def extractLines: Option[Seq[String]] = diff.map { text =>
      text.lines
        .filterNot(_.startsWith("@@"))
        .map { line =>
          line.substring(1)
        }
        .toSeq
    }

  }

  def diffLines(linesFrom: TraversableOnce[String], linesTo: TraversableOnce[String], contextSize: Int = 1000000): Diff = {
    val origLines = linesFrom.toSeq.asJava
    val patch = DiffUtils.diff(origLines, linesTo.toSeq.asJava)
    if (patch.getDeltas.isEmpty) None
    else {
      val diffStr = DiffUtils
        .generateUnifiedDiff("", "", origLines, patch, contextSize)
        .asScala
        .drop(3)
        .mkString(EOL)
      Some(diffStr + EOL)
    }
  }

  def diffObj[T](objFrom: T, objTo: T): Diff =
    if (objFrom == objTo) None
    else
      diffPatch('-', Some(objFrom.toString)) concat
        diffPatch('+', Some(objTo.toString))

  def diffPatch(char: Char, diff: Diff): Diff =
    diff.map { d =>
      val sb = new StringBuilder
      d.split(Pattern.quote(EOL)).foreach { line =>
        sb += char
        sb ++= line
        sb ++= EOL
      }
      sb.toString()
    }

  def diffConcat(diffs: Diff*): Diff =
    diffs.flatten match {
      case Seq() => None
      case s => Some(s.mkString)
    }

  def diffConcat(diffs: Iterable[Diff]): Diff = diffConcat(diffs.toSeq: _*)

  def diffHeader(headerFrom: String, headerTo: String, diff: Diff): Diff =
    diff.map { d =>
      s"--- $headerFrom$EOL" +
        s"+++ $headerTo$EOL" + d
    }

}

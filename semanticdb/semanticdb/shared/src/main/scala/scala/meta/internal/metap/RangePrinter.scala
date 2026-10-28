package scala.meta.internal.metap

import scala.meta.inputs._
import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.semanticdb._

trait RangePrinter extends BasePrinter {
  def pprint(range: Range): Unit = {
    out.print("[")
    out.print(range.startLine)
    out.print(":")
    out.print(range.startCharacter)
    out.print("..")
    out.print(range.endLine)
    out.print(":")
    out.print(range.endCharacter)
    out.print(")")
  }

  private val input = Input.String(doc.text)
  def substring(range: Option[Range]): Option[String] = range match {
    case Some(range) if doc.text.nonEmpty =>
      val pos = range.toPosition(input)
      Some(if (pos.isEmpty) "" else doc.text.substring(pos.start, pos.end))
    case _ => None
  }
}

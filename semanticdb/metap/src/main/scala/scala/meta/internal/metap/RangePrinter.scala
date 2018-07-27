package scala.meta.internal.metap

import scala.collection.mutable
import scala.math.Ordering
import scala.meta.inputs._
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

  private val inputCache = new mutable.HashMap[TextDocument, Input]
  implicit class DocumentOps(doc: TextDocument) {
    def substring(range: Option[Range]): Option[String] = {
      range.flatMap { range =>
        if (doc.text.nonEmpty) {
          val input = inputCache.getOrElseUpdate(doc, Input.String(doc.text))
          val pos = Position.Range(
            input,
            range.startLine,
            range.startCharacter,
            range.endLine,
            range.endCharacter)
          Some(pos.text)
        } else {
          None
        }
      }
    }
  }

  implicit def rangeOrder: Ordering[Range] = {
    Ordering.by(r => (r.startLine, r.startCharacter, r.endLine, r.endCharacter))
  }
}

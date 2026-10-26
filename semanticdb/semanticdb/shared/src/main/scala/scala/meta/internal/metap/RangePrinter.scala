package scala.meta.internal.metap

import scala.meta.inputs._
import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.semanticdb._

import scala.collection.mutable

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
    def substring(range: Option[Range]): Option[String] = range match {
      case Some(range) if doc.text.nonEmpty =>
        val pos = range.toPosition(inputCache.getOrElseUpdate(doc, Input.String(doc.text)))
        Some(if (pos.isEmpty) "" else pos.text)
      case _ => None
    }
  }
}

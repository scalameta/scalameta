package scala.meta.internal.metap

import scala.collection.mutable
import scala.math.Ordering
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

  private val lineToOffsetCache = new mutable.HashMap[TextDocument, Array[Int]]
  implicit class DocumentOps(doc: TextDocument) {
    def substring(range: Option[Range]): Option[String] = {
      range.flatMap { range =>
        if (doc.text.nonEmpty) {
          val lineToOffset = lineToOffsetCache.getOrElseUpdate(doc, {
            val chars = doc.text.toArray
            val buf = new mutable.ArrayBuffer[Int]
            buf += 0
            var i = 0
            while (i < chars.length) {
              if (chars(i) == '\n') buf += (i + 1)
              i += 1
            }
            if (buf.last != chars.length) buf += chars.length
            buf.toArray
          })
          val startOffset = lineToOffset(range.startLine) + range.startCharacter
          val endOffset = lineToOffset(range.endLine) + range.endCharacter
          Some(doc.text.substring(startOffset, endOffset))
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

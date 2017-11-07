package scala.meta.internal.semanticdb

import org.scalameta.unreachable
import scala.{meta => m}

trait MessageOps { self: DatabaseOps =>
  implicit class XtensionCompilationUnitMessages(unit: g.CompilationUnit) {
    def reportedMessages: List[m.Message] = {
      val mstarts = {
        val x =
          unit.body.metadata.get("semanticdbMstarts").map(_.asInstanceOf[Map[Int, m.Position]])
        if (x.nonEmpty) unit.body.removeMetadata("semanticdbMstarts")
        x.getOrElse(Map.empty)
      }
      val messages = unit.hijackedMessages.map {
        case (gpos, gseverity, text) =>
          val mpos = {
            // NOTE: The caret in unused import warnings points to Importee.pos, but
            // the message position start/end point to the enclosing Import.pos.
            // See https://github.com/scalameta/scalameta/issues/839
            if (text == "Unused import") {
              mstarts.get(gpos.point) match {
                case Some(mpos) => mpos
                case None =>
                  if (unit.source.content(gpos.point) == '_') // Importee.Wildcard()
                    gpos.withStart(gpos.point).withEnd(gpos.point + 1).toMeta
                  else gpos.toMeta
              }
            } else gpos.toMeta
          }
          val mseverity = gseverity match {
            case 0 => m.Severity.Info
            case 1 => m.Severity.Warning
            case 2 => m.Severity.Error
            case _ => unreachable
          }
          m.Message(mpos, mseverity, text)
      }
      messages
    }
  }
}

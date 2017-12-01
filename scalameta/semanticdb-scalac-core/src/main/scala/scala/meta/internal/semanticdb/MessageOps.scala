package scala.meta.internal.semanticdb

import org.langmeta.internal.semanticdb.{schema => s}
import org.scalameta.unreachable
import scala.{meta => m}

trait MessageOps { self: DatabaseOps =>
  implicit class XtensionCompilationUnitMessages(unit: g.CompilationUnit) {
    def reportedMessages(mstarts: collection.Map[Int, s.Position]): List[s.Message] = {
      val messages = unit.hijackedMessages.map {
        case (gpos, gseverity, text) =>
          val spos: s.Position = {
            // NOTE: The caret in unused import warnings points to Importee.pos, but
            // the message position start/end point to the enclosing Import.pos.
            // See https://github.com/scalameta/scalameta/issues/839
            if (text == "Unused import") {
              mstarts.get(gpos.point) match {
                case Some(name) => name
                case None =>
                  if (unit.source.content(gpos.point) == '_') // Importee.Wildcard()
                    gpos.withStart(gpos.point).withEnd(gpos.point + 1).toSchema
                  else gpos.toSchema
              }
            } else gpos.toSchema
          }
          val mseverity = gseverity match {
            case 0 => s.Message.Severity.INFO
            case 1 => s.Message.Severity.WARNING
            case 2 => s.Message.Severity.ERROR
            case _ => unreachable
          }
          s.Message(Some(spos), mseverity, text)
      }
      messages
    }
  }
}

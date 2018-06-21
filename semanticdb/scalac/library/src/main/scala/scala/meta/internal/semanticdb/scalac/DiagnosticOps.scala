package scala.meta.internal.semanticdb.scalac

import org.scalameta.unreachable
import scala.{meta => m}
import scala.meta.internal.inputs._
import scala.meta.internal.{semanticdb => s}

trait DiagnosticOps { self: SemanticdbOps =>
  implicit class XtensionCompilationUnitDiagnostics(unit: g.CompilationUnit) {
    def reportedDiagnostics(mstarts: collection.Map[Int, m.Name]): List[s.Diagnostic] = {
      unit.hijackedDiagnostics.map {
        case (gpos, gseverity, text) =>
          val mpos: m.Position = {
            // NOTE: The caret in unused import warnings points to Importee.pos, but
            // the message position start/end point to the enclosing Import.pos.
            // See https://github.com/scalameta/scalameta/issues/839
            if (text == "Unused import") {
              mstarts.get(gpos.point) match {
                case Some(name) => name.pos
                case None =>
                  if (unit.source.content(gpos.point) == '_') // Importee.Wildcard()
                    gpos.withStart(gpos.point).withEnd(gpos.point + 1).toMeta
                  else gpos.toMeta
              }
            } else gpos.toMeta
          }
          val sseverity = gseverity match {
            case 0 => s.Diagnostic.Severity.INFORMATION
            case 1 => s.Diagnostic.Severity.WARNING
            case 2 => s.Diagnostic.Severity.ERROR
            case _ => unreachable
          }
          s.Diagnostic(Some(mpos.toRange), sseverity, text)
      }
    }
  }
}

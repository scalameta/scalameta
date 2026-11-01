package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.{semanticdb => s}

import scala.{meta => m}

trait DiagnosticOps {
  self: SemanticdbOps =>
  implicit class XtensionCompilationUnitDiagnostics(unit: g.CompilationUnit) {
    def reportedDiagnostics(mnameAt: Int => m.Name): Iterator[s.Diagnostic] = unit
      .hijackedDiagnostics.map { case (gpos, gseverity, text) =>
        val mpos: m.Position =
          // NOTE: The caret in unused import warnings points to Importee.pos, but
          // the message position start/end point to the enclosing Import.pos.
          // See https://github.com/scalameta/scalameta/issues/839
          if (text == "Unused import") mnameAt(gpos.point) match {
            case name: m.Name => name.pos
            case _ =>
              if (unit.source.content(gpos.point) == '_') // Importee.Wildcard()
                m.Position.Range(gpos.source.toInput, gpos.point, gpos.point + 1)
              else gpos.toMeta
          }
          else gpos.toMeta
        val sseverity = gseverity match {
          case 0 => s.Diagnostic.Severity.INFORMATION
          case 1 => s.Diagnostic.Severity.WARNING
          case 2 => s.Diagnostic.Severity.ERROR
          case _ => s.Diagnostic.Severity.UNKNOWN_SEVERITY
        }
        s.Diagnostic(Some(mpos.toRange), sseverity, text)
      }
  }
}

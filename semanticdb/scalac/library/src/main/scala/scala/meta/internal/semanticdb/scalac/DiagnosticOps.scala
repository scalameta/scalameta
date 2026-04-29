package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.{semanticdb => s}

import scala.reflect.internal.Reporter
import scala.{meta => m}

trait DiagnosticOps {
  self: SemanticdbOps =>
  implicit class XtensionCompilationUnitDiagnostics(unit: g.CompilationUnit) {
    def reportedDiagnostics(mstarts: collection.Map[Int, m.Name]): List[s.Diagnostic] = unit
      .hijackedDiagnostics.map { info =>
        val text = info.msg
        val gpos = info.pos
        val mpos: m.Position =
          // NOTE: The caret in unused import warnings points to Importee.pos, but
          // the message position start/end point to the enclosing Import.pos.
          // See https://github.com/scalameta/scalameta/issues/839
          if (text == "Unused import") mstarts.get(gpos.point) match {
            case Some(name) => name.pos
            case None =>
              if (unit.source.content(gpos.point) == '_') // Importee.Wildcard()
                m.Position.Range(gpos.source.toInput, gpos.point, gpos.point + 1)
              else gpos.toMeta
          }
          else gpos.toMeta

        val sseverity = info.severity match {
          case Reporter.INFO => s.Diagnostic.Severity.INFORMATION
          case Reporter.WARNING => s.Diagnostic.Severity.WARNING
          case Reporter.ERROR => s.Diagnostic.Severity.ERROR
        }
        s.Diagnostic(Some(mpos.toRange), sseverity, text)
      }
  }
}

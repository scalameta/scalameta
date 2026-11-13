package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb => s}

import scala.reflect.internal.util.{Position => gPosition}
import scala.{meta => m}

trait DiagnosticOps {
  self: SemanticdbOps =>

  import s.Implicits._

  implicit class XtensionHijackedDiagnostics(gdiagnostics: Iterator[(gPosition, Int, String)]) {
    def toDiagnostics(mtreeAt: Int => m.Tree): Iterator[s.Diagnostic] = gdiagnostics
      .map { case (gpos, gseverity, text) =>
        /* NOTE: gpos could cover an enclosing tree, so look up the innermost one at the caret.
         * See https://github.com/scalameta/scalameta/issues/839 */
        val mtree = mtreeAt(gpos.point)
        val mpos = if (null eq mtree) gpos.toMeta else mtree.pos
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

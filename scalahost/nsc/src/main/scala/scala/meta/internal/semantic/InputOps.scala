package scala.meta.internal
package semantic

import scala.{meta => m}
import scala.reflect.internal.util.{Position => GPosition, SourceFile => GSourceFile}
import scala.reflect.io.{PlainFile => GPlainFile}

trait InputOps { self: DatabaseOps =>

  implicit class XtensionGSourceFileInput(gsource: GSourceFile) {
    def toAbsolutePath: m.AbsolutePath = gsource.file match {
      case gfile: GPlainFile => m.AbsolutePath(gfile.file)
      case other => sys.error(s"unsupported file " + other)
    }
    def toInput: m.Input = m.Input.File(gsource.toAbsolutePath)
  }

  implicit class XtensionGPositionMPosition(pos: GPosition) {
    def toMeta: m.Position = {
      assert(pos.isRange)
      val input = m.Input.File(pos.source.toAbsolutePath)
      val mstart = m.Point.Offset(input, pos.start)
      val mend = m.Point.Offset(input, pos.end)
      if (pos.point > pos.start && pos.point < pos.end) {
        val mpoint = m.Point.Offset(input, pos.point)
        m.Position.RangeWithPoint(input, mstart, mpoint, mend)
      } else {
        m.Position.Range(input, mstart, mend)
      }
    }
  }
}

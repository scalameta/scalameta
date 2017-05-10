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
      val input = m.Input.File(pos.source.toAbsolutePath)
      if (pos.isRange) {
        m.Position.Range(input, m.Point.Offset(input, pos.start), m.Point.Offset(input, pos.end))
      } else {
        // NOTE: Even with -Yrangepos enabled we cannot be guaranteed that all positions are
        // range positions. In the case we encounter a non-range position we assume start == end.
        val mpoint = m.Point.Offset(input, pos.point)
        m.Position.Range(input, mpoint, mpoint)
      }
    }
  }
}

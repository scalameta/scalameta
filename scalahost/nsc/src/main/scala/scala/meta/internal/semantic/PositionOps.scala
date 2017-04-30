package scala.meta.internal
package semantic

import java.io.{File => JFile}
import scala.{meta => m}
import scala.reflect.internal.util.{Position => GPosition, SourceFile => GSourceFile}
import scala.reflect.io.{AbstractFile => GFile, PlainFile => GPlainFile}

trait PositionOps { self: DatabaseOps =>

  implicit class XtensionGSourceFileLocation(gsource: GSourceFile) {
    def toAbsolutePath: m.AbsolutePath = gsource.file.toAbsolutePath
  }

  implicit class XtensionGFileAddr(gfile: GFile) {
    def toAbsolutePath: m.AbsolutePath = gfile match {
      case gfile: GPlainFile => m.AbsolutePath(gfile.file)
      case other => sys.error(s"unsupported file " + other)
    }
  }

  implicit class XtensionGPositionMPosition(pos: GPosition) {
    def toMeta: m.Position = {
      assert(pos.isRange)
      val input = m.Input.File(pos.source.toAbsolutePath)
      m.Position.Range(input, m.Point.Offset(input, pos.start), m.Point.Offset(input, pos.end))
    }
  }
}

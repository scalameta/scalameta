package scala.meta.internal
package scalahost
package databases

import java.io.{File => JFile}
import scala.{meta => m}
import scala.reflect.internal.util.{Position => GPosition, SourceFile => GSourceFile}
import scala.reflect.io.{AbstractFile => GFile, PlainFile => GPlainFile}
import scala.meta.internal.io.PlatformIO

trait AnchorOps {
  self: DatabaseOps =>

  private def toAbsolutePath(jfile: JFile): m.AbsolutePath = {
    m.AbsolutePath(jfile)
  }

  implicit class XtensionGSourceFileLocation(gsource: GSourceFile) {
    def toAbsolutePath: m.AbsolutePath = gsource.file.toAbsolutePath
  }

  implicit class XtensionGFileAddr(gfile: GFile) {
    def toAbsolutePath: m.AbsolutePath = gfile match {
      case gfile: GPlainFile => self.toAbsolutePath(gfile.file)
      case other => sys.error(s"unsupported file " + other)
    }
  }

  implicit class XtensionMPositionAnchor(pos: m.Position) {
    def toAnchor: m.Anchor = pos.input match {
      case scala.meta.inputs.Input.File(path, _) =>
        m.Anchor(toAbsolutePath(path.toFile), pos.start.offset, pos.end.offset)
      case other =>
        sys.error(s"unsupported input " + other)
    }
  }

  implicit class XtensionGPositionAnchor(pos: GPosition) {
    def toAnchor: m.Anchor = {
      assert(pos.isRange)
      m.Anchor(pos.source.toAbsolutePath, pos.start, pos.end)
    }
  }
}

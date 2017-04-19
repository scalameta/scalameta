package scala.meta.internal
package scalahost
package mirrors

import scala.{meta => m}
import scala.reflect.internal.util.{Position => GPosition, SourceFile => GSourceFile}
import scala.reflect.io.{AbstractFile => GFile, PlainFile => GPlainFile}

trait AnchorOps { self: OnlineMirror =>
  implicit class XtensionGSourceFileLocation(gsource: GSourceFile) {
    def toAbsolutePath: m.AbsolutePath = gsource.file.toAbsolutePath
  }

  implicit class XtensionGFileAddr(gfile: GFile) {
    def toAbsolutePath: m.AbsolutePath = gfile match {
      case gfile: GPlainFile => m.AbsolutePath(gfile.file)
      case other => sys.error(s"unsupported file " + other)
    }
  }

  implicit class XtensionGPositionAnchor(pos: GPosition) {
    def toAnchor: m.Anchor = {
      assert(pos.isRange)
      m.Anchor(pos.source.toAbsolutePath, pos.start, pos.end)
    }
  }
}

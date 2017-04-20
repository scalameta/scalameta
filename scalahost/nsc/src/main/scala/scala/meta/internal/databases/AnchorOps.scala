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

  private def toRelativePath(jfile: JFile): m.RelativePath = {
    // NOTE: This is an extremely important line that underpins
    // the current strategy of extraction of semantic databases.
    //
    // In order to convert from absolute paths used by scalac compilations
    // into relative paths used by semantic databases, we strip off
    // the current working directory of the JVM process that runs the compiler.
    m.AbsolutePath(jfile).relativize(PlatformIO.workingDirectory)
  }

  implicit class XtensionGSourceFileLocation(gsource: GSourceFile) {
    def toRelativePath: m.RelativePath = gsource.file.toRelativePath
  }

  implicit class XtensionGFileAddr(gfile: GFile) {
    def toRelativePath: m.RelativePath = gfile match {
      case gfile: GPlainFile => self.toRelativePath(gfile.file)
      case other => sys.error(s"unsupported file " + other)
    }
  }

  implicit class XtensionMPositionAnchor(pos: m.Position) {
    def toAnchor: m.Anchor = pos.input match {
      case scala.meta.inputs.Input.File(path, _) =>
        m.Anchor(toRelativePath(path.toFile), pos.start.offset, pos.end.offset)
      case other =>
        sys.error(s"unsupported input " + other)
    }
  }

  implicit class XtensionGPositionAnchor(pos: GPosition) {
    def toAnchor: m.Anchor = {
      assert(pos.isRange)
      m.Anchor(pos.source.toRelativePath, pos.start, pos.end)
    }
  }
}

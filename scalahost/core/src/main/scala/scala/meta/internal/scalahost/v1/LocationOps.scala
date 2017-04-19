package scala.meta.internal
package scalahost
package v1

import java.io.{File => JFile}
import scala.collection.mutable
import scala.reflect.io.{AbstractFile => GFile, PlainFile => GPlainFile}
import scala.reflect.internal.util.{Position => GPosition}
import scala.reflect.internal.util.{SourceFile => GSourceFile}
import scala.tools.nsc.Global
import scala.{meta => m}
import scala.meta.io.AbsolutePath
import scala.meta.semantic.v1._

trait LocationOps {
  implicit class XtensionGSourceFileLocation(gsource: GSourceFile) {
    def toAbsolutePath: AbsolutePath = gsource.file.toAbsolutePath
  }

  implicit class XtensionGFileAddr(gfile: GFile) {
    def toAbsolutePath: AbsolutePath = gfile match {
      case gfile: GPlainFile => AbsolutePath(gfile.file)
      case other => sys.error(s"unsupported file " + other)
    }
  }

  implicit class XtensionMInputAddr(minput: m.Input) {
    def toAbsolutePath: AbsolutePath = minput match {
      case scala.meta.inputs.Input.File(path, _) => path
      case other => sys.error(s"unsupported input " + other)
    }
  }

  implicit class XtensionGPositionLocation(pos: GPosition) {
    def toSemantic: Location = {
      assert(pos.isRange)
      Location(pos.source.toAbsolutePath, pos.start, pos.end)
    }
  }

  implicit class XtensionMPositionLocation(pos: m.Position) {
    def toSemantic: Location = {
      Location(pos.input.toAbsolutePath, pos.start.offset, pos.end.offset)
    }
  }
}

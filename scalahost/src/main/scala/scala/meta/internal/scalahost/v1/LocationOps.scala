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
import scala.meta.semantic.v1._

trait LocationOps {
  implicit class XtensionGSourceFileAddr(gsource: GSourceFile) {
    def toAddr: Address = gsource.file.toAddr
  }

  implicit class XtensionGFileAddr(gfile: GFile) {
    def toAddr: Address = gfile match {
      case gfile: GPlainFile => Address.File(gfile.file.getAbsolutePath)
      case other => sys.error(s"unsupported file " + other)
    }
  }

  implicit class XtensionMInputAddr(minput: m.Input) {
    def toAddr: Address = minput match {
      case scala.meta.inputs.Input.File(file, _) => Address.File(file.getAbsolutePath)
      case other => sys.error(s"unsupported input " + other)
    }
  }

  val gfileMap = mutable.Map[GFile, Address]().withDefault(_.toAddr)
  implicit class XtensionGPositionLocation(pos: GPosition) {
    def toSemantic: Location = {
      assert(pos.isRange)
      Location(gfileMap(pos.source.file), pos.start, pos.end)
    }
  }

  val minputMap = mutable.Map[m.Input, Address]().withDefault(_.toAddr)
  implicit class XtensionMPositionLocation(pos: m.Position) {
    def toSemantic: Location = {
      Location(minputMap(pos.input), pos.start.offset, pos.end.offset)
    }
  }
}

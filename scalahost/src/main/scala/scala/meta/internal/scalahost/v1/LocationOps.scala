package scala.meta.internal
package scalahost
package v1

import java.net.URI
import scala.collection.mutable
import scala.reflect.internal.util.{Position => GPosition}
import scala.reflect.io.{AbstractFile => GFile}
import scala.{meta => m}
import scala.meta.semantic.v1.Location

trait LocationOps {
  val gfileMap = mutable
    .Map[GFile, URI]()
    .withDefault({
      case gfile: scala.reflect.io.PlainFile => gfile.file.toURI
      case other                             => sys.error(s"unsupported file " + other)
    })

  implicit class XtensionGPositionLocation(pos: GPosition) {
    def toSemantic: Location = {
      assert(pos.isRange)
      Location(gfileMap(pos.source.file).toString, pos.start, pos.end)
    }
  }

  val minputMap = mutable
    .Map[m.Input, URI]()
    .withDefault({
      case scala.meta.inputs.Input.File(file, _) => file.toURI
      case other                                 => sys.error(s"unsupported input " + other)
    })

  implicit class XtensionMPositionLocation(pos: m.Position) {
    def toSemantic: Location = {
      Location(minputMap(pos.input).toString, pos.start.offset, pos.end.offset)
    }
  }
}

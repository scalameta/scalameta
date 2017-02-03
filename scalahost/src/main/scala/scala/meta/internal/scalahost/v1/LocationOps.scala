package scala.meta.internal
package scalahost
package v1

import scala.{meta => m}
import scala.reflect.internal.util.{Position => GPosition}
import scala.meta.semantic.v1.Location

trait LocationOps {
  implicit class XtensionGPositionLocation(pos: GPosition) {
    def toSemantic: Location = {
      assert(pos.isRange)
      Location(pos.source.file.file.toURI.toString, pos.start, pos.end)
    }
  }

  implicit class XtensionMPositionLocation(pos: m.Position) {
    def toSemantic: Location = {
      val scala.meta.inputs.Input.File(file, _) = pos.input
      Location(file.toURI.toString, pos.start.offset, pos.end.offset)
    }
  }
}
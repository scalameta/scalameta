package scala.meta.internal
package scalahost
package v1

import scala.{meta => m}
import scala.reflect.internal.util.{Position => GPosition}
import scala.meta.semantic.v1.Location

trait LocationOps extends ReflectionToolkit {
  implicit class XtensionGPosition(pos: GPosition) {
    def toSemantic = {
      assert(pos.isRange)
      Location(pos.source.file.file.toURI.toString, pos.start, pos.end)
    }
  }

  implicit class XtensionMPosition(pos: m.Position) {
    def toSemantic = {
      val scala.meta.inputs.Input.File(file, _) = pos.input
      Location(file.toURI.toString, pos.start.offset, pos.end.offset)
    }
  }
}
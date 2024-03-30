package scala.meta
package internal
package parsers

import scala.meta.inputs._
import scala.meta.parsers._
import scala.meta.tokenizers._

object Absolutize {
  implicit class XtensionPositionAbsolutize(private val pos: Position) extends AnyVal {
    def absolutize: Position = pos match {
      case Position.Range(Input.Slice(input, absoluteStart, _), start, end) =>
        val start1 = absoluteStart + start
        val end1 = absoluteStart + end
        Position.Range(input, start1, end1)
      case other => other
    }
  }

  implicit class XtensionExceptionAbsolutize(private val ex: Throwable) extends AnyVal {
    def absolutize: Throwable = {
      val ex1 = ex match {
        case TokenizeException(pos, message) => TokenizeException(pos.absolutize, message)
        case ParseException(pos, message) => ParseException(pos.absolutize, message)
        case PlatformInvocationTargetException(ex) => ex
      }
      ex1.setStackTrace(ex.getStackTrace)
      ex1
    }
  }
}

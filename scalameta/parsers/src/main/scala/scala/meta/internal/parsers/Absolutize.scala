package scala.meta
package internal
package parsers

import java.lang.reflect.InvocationTargetException
import scala.meta.inputs._
import scala.meta.parsers._
import scala.meta.tokenizers._
import scala.meta.internal.ast.Origin

object Absolutize {
  implicit class XtensionPositionAbsolutize(pos: Position) {
    def absolutize: Position = {
      pos match {
        case Position.Range(input, start, end) =>
          require(start.input == end.input)
          val start1 = start.absolutize
          val end1 = end.absolutize
          val input1 = start1.input
          Position.Range(input1, start1, end1)
        case other =>
          other
      }
    }
  }

  implicit class XtensionPointAbsolutize(pos: Point) {
    def absolutize: Point = {
      pos match {
        case Point.Offset(Input.Slice(input, absoluteStart, _), relativeOffset) =>
          Point.Offset(input, absoluteStart + relativeOffset)
        case other =>
          other
      }
    }
  }

  implicit class XtensionExceptionAbsolutize(ex: Throwable) {
    def absolutize: Throwable = {
      val ex1 = ex match {
        case TokenizeException(pos, message) =>
          TokenizeException(pos.absolutize, message)
        case ParseException(pos, message) =>
          ParseException(pos.absolutize, message)
        case ex: InvocationTargetException =>
          new InvocationTargetException(ex.getTargetException.absolutize, ex.getMessage)
      }
      ex1.setStackTrace(ex.getStackTrace)
      ex1
    }
  }
}
package scala.meta
package parsers
package common

import org.scalameta.convert._
import scala.meta.inputs._

private[meta] trait Api {
  implicit class XtensionParseInputLike[T](inputLike: T) {
    def parse[U](implicit convert: Convert[T, Input], dialect: Dialect, parse: Parse[U]): U = {
      parse(convert(inputLike))
    }
  }
}

private[meta] trait Aliases {
  type ParseException = scala.meta.parsers.common.ParseException
  val ParseException = scala.meta.parsers.common.ParseException
}

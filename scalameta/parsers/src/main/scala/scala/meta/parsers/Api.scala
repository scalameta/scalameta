package scala.meta
package parsers

import org.scalameta.convert._
import scala.meta.inputs._

private[meta] trait Api {
  implicit class XtensionParseInputLike[T](inputLike: T) {
    def parse[U](implicit convert: Convert[T, Input], parse: Parse[U], dialect: Dialect): U = {
      val input = convert(inputLike)
      parse(input)(dialect)
    }
  }
}

private[meta] trait Aliases {
  type ParseException = scala.meta.parsers.ParseException
  val ParseException = scala.meta.parsers.ParseException
}

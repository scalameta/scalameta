package scala.meta
package parsers

import org.scalameta.convert._
import scala.meta.inputs._

class InputWithDialect(input: Input, dialect: Dialect) {
  def parse[U](implicit parse: Parse[U]): Parsed[U] = {
    parse.apply(input)(dialect)
  }
}

private[meta] trait Api {
  implicit class XtensionParseInputLike[T](inputLike: T) {
    def parse[U](implicit convert: Convert[T, Input], parse: Parse[U], dialect: Dialect): Parsed[U] = {
      val input = convert(inputLike)
      parse.apply(input)(dialect)
    }
  }
  implicit class XtensionDialectParseInputLike(dialect: Dialect) {
    def apply[T](inputLike: T)(implicit convert: Convert[T, Input]): InputWithDialect = {
      val input = convert(inputLike)
      new InputWithDialect(input, dialect)
    }
  }
}

private[meta] trait Aliases {
  type Parsed[+T] = scala.meta.parsers.Parsed[T]
  val Parsed = scala.meta.parsers.Parsed

  type ParseException = scala.meta.parsers.ParseException
  val ParseException = scala.meta.parsers.ParseException
}

package scala.meta
package parsers

import scala.meta.common._
import scala.meta.inputs._

private[meta] trait Api {
  implicit class XtensionParseInputLike[T](inputLike: T) {
    def parse[U](implicit convert: Convert[T, Input], parse: Parse[U], dialect: Dialect): Parsed[U] = {
      (dialect, convert(inputLike)).parse[U]
    }
  }
  implicit class XtensionParsersDialectInput(dialect: Dialect) {
    def apply[T](inputLike: T)(implicit convert: Convert[T, Input]): (Dialect, Input) = {
      (dialect, convert(inputLike))
    }
  }
  implicit class XtensionParseDialectInput(dialectInput: (Dialect, Input)) {
    def parse[U](implicit parse: Parse[U]): Parsed[U] = {
      val (dialect, input) = dialectInput
      parse.apply(input, dialect)
    }
  }
  implicit class XtensionParseInputDialect(inputDialect: (Input, Dialect)) {
    def parse[U](implicit parse: Parse[U]): Parsed[U] = {
      val (input, dialect) = inputDialect
      (dialect, input).parse[U]
    }
  }
}

private[meta] trait Aliases {
  type Parsed[+T] = scala.meta.parsers.Parsed[T]
  lazy val Parsed = scala.meta.parsers.Parsed

  type ParseException = scala.meta.parsers.ParseException
  lazy val ParseException = scala.meta.parsers.ParseException
}

package example

import scala.language.implicitConversions

class ImplicitConversion {
  implicit def string2Number(
      string: String
  ): Int = 42
  val message = ""
  val number = 42
  val tuple = (1, 2)
  val char: Char = 'a'

  // extension methods
  message
    .stripSuffix("h")
  tuple + "Hello"

  // implicit conversions
  val x: Int = message

  // interpolators
  s"Hello $message $number"
  s"""Hello
     |$message
     |$number""".stripMargin

  val a: Int = char
  val b: Long = char
  val toLong: Int = 42
  // The line below reproduces a known bug, where the symbol occurrence is
  // "scala/Int#toLong()." instead of "example/ImplicitConversions.toLong.".
  // This bug happens because our current heuristic to detect implicit
  // conversions is based on the name of the symbol, and the "toLong" variable
  // has the same name as the implicit conversion that's being applied.
  val c: Long = toLong
}

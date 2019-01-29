package example

import scala.language/*scala.language.*/.implicitConversions/*scala.language.implicitConversions.*/

class ImplicitConversion/*example.ImplicitConversion#*/ {
  implicit def string2Number/*example.ImplicitConversion#string2Number().*/(
      string/*example.ImplicitConversion#string2Number().(string)*/: String/*scala.Predef.String#*/
  ): Int/*scala.Int#*/ = 42
  val message/*example.ImplicitConversion#message.*/ = ""
  val number/*example.ImplicitConversion#number.*/ = 42
  val tuple/*example.ImplicitConversion#tuple.*/ = (1, 2)
  val char/*example.ImplicitConversion#char.*/: Char/*scala.Char#*/ = 'a'

  // extension methods
  message/*example.ImplicitConversion#message.*/
    .stripSuffix/*scala.collection.immutable.StringLike#stripSuffix().*/("h")
  tuple/*example.ImplicitConversion#tuple.*/ +/*scala.Predef.any2stringadd#`+`().*/ "Hello"

  // implicit conversions
  val x/*example.ImplicitConversion#x.*/: Int/*scala.Int#*/ = message/*example.ImplicitConversion#message.*/

  // interpolators
  s/*scala.StringContext#s().*/"Hello $message/*example.ImplicitConversion#message.*/ $number/*example.ImplicitConversion#number.*/"
  s/*scala.StringContext#s().*/"""Hello
     |$message/*example.ImplicitConversion#message.*/
     |$number/*example.ImplicitConversion#number.*/""".stripMargin/*scala.collection.immutable.StringLike#stripMargin(+1).*/

  val a/*example.ImplicitConversion#a.*/: Int/*scala.Int#*/ = char/*scala.Char#toInt().*/
  val b/*example.ImplicitConversion#b.*/: Long/*scala.Long#*/ = char/*scala.Char#toLong().*/
}

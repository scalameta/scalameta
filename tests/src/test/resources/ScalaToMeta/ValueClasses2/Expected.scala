package scala
final abstract class Byte extends AnyVal {
  def toByte: Byte
  def toShort: Short
  def toChar: Char
  def toInt: Int
  def toLong: Long
  def toFloat: Float
  def toDouble: Double
  def unary_~ : Int
  def unary_+ : Int
  def unary_- : Int
  def +(x: String): String
  def <<(x: Int): Int
  def <<(x: Long): Int
  def >>>(x: Int): Int
  def >>>(x: Long): Int
  def >>(x: Int): Int
  def >>(x: Long): Int
  def ==(x: Byte): Boolean
  def ==(x: Short): Boolean
  def ==(x: Char): Boolean
  def ==(x: Int): Boolean
  def ==(x: Long): Boolean
  def ==(x: Float): Boolean
  def ==(x: Double): Boolean
  def !=(x: Byte): Boolean
  def !=(x: Short): Boolean
  def !=(x: Char): Boolean
  def !=(x: Int): Boolean
  def !=(x: Long): Boolean
  def !=(x: Float): Boolean
  def !=(x: Double): Boolean
  def <(x: Byte): Boolean
  def <(x: Short): Boolean
  def <(x: Char): Boolean
  def <(x: Int): Boolean
  def <(x: Long): Boolean
  def <(x: Float): Boolean
  def <(x: Double): Boolean
  def <=(x: Byte): Boolean
  def <=(x: Short): Boolean
  def <=(x: Char): Boolean
  def <=(x: Int): Boolean
  def <=(x: Long): Boolean
  def <=(x: Float): Boolean
  def <=(x: Double): Boolean
  def >(x: Byte): Boolean
  def >(x: Short): Boolean
  def >(x: Char): Boolean
  def >(x: Int): Boolean
  def >(x: Long): Boolean
  def >(x: Float): Boolean
  def >(x: Double): Boolean
  def >=(x: Byte): Boolean
  def >=(x: Short): Boolean
  def >=(x: Char): Boolean
  def >=(x: Int): Boolean
  def >=(x: Long): Boolean
  def >=(x: Float): Boolean
  def >=(x: Double): Boolean
  def |(x: Byte): Int
  def |(x: Short): Int
  def |(x: Char): Int
  def |(x: Int): Int
  def |(x: Long): Long
  def &(x: Byte): Int
  def &(x: Short): Int
  def &(x: Char): Int
  def &(x: Int): Int
  def &(x: Long): Long
  def ^(x: Byte): Int
  def ^(x: Short): Int
  def ^(x: Char): Int
  def ^(x: Int): Int
  def ^(x: Long): Long
  def +(x: Byte): Int
  def +(x: Short): Int
  def +(x: Char): Int
  def +(x: Int): Int
  def +(x: Long): Long
  def +(x: Float): Float
  def +(x: Double): Double
  def -(x: Byte): Int
  def -(x: Short): Int
  def -(x: Char): Int
  def -(x: Int): Int
  def -(x: Long): Long
  def -(x: Float): Float
  def -(x: Double): Double
  def *(x: Byte): Int
  def *(x: Short): Int
  def *(x: Char): Int
  def *(x: Int): Int
  def *(x: Long): Long
  def *(x: Float): Float
  def *(x: Double): Double
  def /(x: Byte): Int
  def /(x: Short): Int
  def /(x: Char): Int
  def /(x: Int): Int
  def /(x: Long): Long
  def /(x: Float): Float
  def /(x: Double): Double
  def %(x: Byte): Int
  def %(x: Short): Int
  def %(x: Char): Int
  def %(x: Int): Int
  def %(x: Long): Long
  def %(x: Float): Float
  def %(x: Double): Double
  override def getClass(): Class[Byte] = null
}
object Byte extends AnyValCompanion {
  final val MinValue = java.lang.Byte.MIN_VALUE
  final val MaxValue = java.lang.Byte.MAX_VALUE
  def box(x: Byte): java.lang.Byte = java.lang.Byte.valueOf(x)
  def unbox(x: java.lang.Object): Byte = x.asInstanceOf[java.lang.Byte].byteValue()
  override def toString = "object scala.Byte"
  import scala.language.implicitConversions
  implicit def byte2short(x: Byte): Short = x.toShort
  implicit def byte2int(x: Byte): Int = x.toInt
  implicit def byte2long(x: Byte): Long = x.toLong
  implicit def byte2float(x: Byte): Float = x.toFloat
  implicit def byte2double(x: Byte): Double = x.toDouble
}
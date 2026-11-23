package scala.meta.internal.scalacp

import scala.tools.asm.{Attribute, ClassReader, Label}
import scala.tools.scalap.Main
import scala.tools.scalap.scalax.rules.scalasig.{ByteCode, ScalaSig, ScalaSigAttributeParsers}

final case class ScalaSigAttribute(scalaSig: ScalaSig) extends Attribute(Main.SCALA_SIG)
object ScalaSigAttribute extends Attribute(Main.SCALA_SIG) {
  override def read(
      cr: ClassReader,
      off: Int,
      len: Int,
      buf: Array[Char],
      codeOff: Int,
      labels: Array[Label],
  ): Attribute = {
    val bytes = new Array[Byte](len)
    var i = 0
    while (i < len) {
      bytes(i) = cr.readByte(off + i).toByte
      i += 1
    }
    val scalaSig = ScalaSigAttributeParsers.parse(ByteCode(bytes))
    ScalaSigAttribute(scalaSig)
  }
}

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
      labels: Array[Label]
  ): Attribute = {
    val bytecode = new ByteCode(cr.b, off, len)
    val scalaSig = ScalaSigAttributeParsers.parse(bytecode)
    ScalaSigAttribute(scalaSig)
  }
}

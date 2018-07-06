package scala.meta.internal.scalacp

import scala.tools.asm.Attribute
import scala.tools.asm.ClassReader
import scala.tools.asm.Label
import scala.tools.scalap.Main
import scala.tools.scalap.scalax.rules.scalasig.ByteCode
import scala.tools.scalap.scalax.rules.scalasig.ScalaSig
import scala.tools.scalap.scalax.rules.scalasig.ScalaSigAttributeParsers

sealed abstract class BaseScalaSigAttribute(`type`: String) extends Attribute(`type`) {
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
final case class ScalaSigAttribute(scalaSig: ScalaSig) extends BaseScalaSigAttribute(Main.SCALA_SIG)
object ScalaSigAttribute extends BaseScalaSigAttribute(Main.SCALA_SIG)

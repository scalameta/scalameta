// NOTE: copy/pasted from https://github.com/lampepfl/dotty/tree/1962ada58fcd2333a2e40179ab0ac6efb6167ed2

package scala.meta
package internal
package tasty

import TastyFormat._
import collection.mutable
import TastyBuffer._
import java.util.UUID

class TastyPickler {

  private val sections = new mutable.ArrayBuffer[(TastyName.NameRef, TastyBuffer)]

  private val headerBuffer = {
    val buf = new TastyBuffer(24)
    for (ch <- header) buf.writeByte(ch.toByte)
    buf.writeNat(MajorVersion)
    buf.writeNat(MinorVersion)
    val uuid = UUID.randomUUID()
    buf.writeUncompressedLong(uuid.getMostSignificantBits)
    buf.writeUncompressedLong(uuid.getLeastSignificantBits)
    buf
  }

  val nameBuffer = new NameBuffer

  def newSection(name: String, buf: TastyBuffer) =
    sections += ((nameBuffer.nameIndex(name), buf))

  def assembleParts(): Array[Byte] = {
    def lengthWithLength(buf: TastyBuffer) = {
      buf.assemble()
      buf.length + natSize(buf.length)
    }
    val totalSize =
      headerBuffer.length +
      lengthWithLength(nameBuffer) + {
        for ((nameRef, buf) <- sections) yield
          natSize(nameRef.index) + lengthWithLength(buf)
      }.sum
    val all = new TastyBuffer(totalSize)
    all.writeBytes(headerBuffer.bytes, headerBuffer.length)
    all.writeNat(nameBuffer.length)
    all.writeBytes(nameBuffer.bytes, nameBuffer.length)
    for ((nameRef, buf) <- sections) {
      all.writeNat(nameRef.index)
      all.writeNat(buf.length)
      all.writeBytes(buf.bytes, buf.length)
    }
    assert(all.length == totalSize && all.bytes.length == totalSize, s"totalSize = $totalSize, all.length = ${all.length}, all.bytes.length = ${all.bytes.length}")
    all.bytes
  }
}

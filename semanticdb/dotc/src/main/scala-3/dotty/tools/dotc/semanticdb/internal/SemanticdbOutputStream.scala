/*
 * Protocol Buffers - Google's data interchange format
 * Copyright 2008 Google Inc.  All rights reserved.
 *
 * Use of this source code is governed by a BSD-style
 * license that can be found in META-INF/LICENSE-protobuf.txt or at
 * https://developers.google.com/open-source/licenses/bsd
 *
 * Adapted by scalameta from CodedOutputStream of protobuf-java and of its port in
 * scalapb/protobuf-scala-runtime: changed to the API that the SemanticDB classes need.
 */
package dotty.tools.dotc.semanticdb.internal

import java.io.{IOException, OutputStream}
import java.nio.charset.StandardCharsets

object SemanticdbOutputStream {

  def newInstance(output: OutputStream): SemanticdbOutputStream =
    new SemanticdbOutputStream(new Array[Byte](BufferSize), output)

  def newInstance(buf: Array[Byte]): SemanticdbOutputStream = new SemanticdbOutputStream(buf, null)

  def computeDoubleSize(fieldNumber: Int, value: Double): Int = computeTagSize(fieldNumber) + 8
  def computeFloatSize(fieldNumber: Int, value: Float): Int = computeTagSize(fieldNumber) + 4
  def computeBoolSize(fieldNumber: Int, value: Boolean): Int = computeTagSize(fieldNumber) + 1

  def computeInt64Size(fieldNumber: Int, value: Long): Int = computeTagSize(fieldNumber) +
    computeUInt64SizeNoTag(value)

  def computeInt32Size(fieldNumber: Int, value: Int): Int = computeTagSize(fieldNumber) +
    computeUInt64SizeNoTag(value)

  def computeEnumSize(fieldNumber: Int, value: Int): Int = computeInt32Size(fieldNumber, value)

  def computeStringSize(fieldNumber: Int, value: String): Int = {
    val length = utf8Length(value)
    computeTagSize(fieldNumber) + computeUInt32SizeNoTag(length) + length
  }

  private def computeTagSize(fieldNumber: Int): Int =
    computeUInt32SizeNoTag(WireFormat.makeTag(fieldNumber, 0))

  // count the groups of 7 bits, with 9/64 in place of 1/7, as protobuf-java does
  def computeUInt32SizeNoTag(value: Int): Int = 352 - Integer.numberOfLeadingZeros(value) * 9 >>> 6

  private def computeUInt64SizeNoTag(value: Long): Int =
    640 - java.lang.Long.numberOfLeadingZeros(value) * 9 >>> 6

  /** Returns the length of `String.getBytes(UTF_8)`, which encodes a lone surrogate as `?`. */
  private def utf8Length(value: String): Int = {
    var length = value.length
    var i = 0
    while (i < value.length) {
      val c = value.charAt(i)
      if (c >= 0x800) {
        if (!Character.isSurrogate(c)) length += 2
        else if (Character.isHighSurrogate(c) && i + 1 < value.length &&
          Character.isLowSurrogate(value.charAt(i + 1))) {
          length += 2
          i += 1
        }
      } else if (c >= 0x80) length += 1
      i += 1
    }
    length
  }

  private val BufferSize = 4096

  final class OutOfSpaceException
      extends IOException("SemanticdbOutputStream ran out of space in its byte array.")

}

/**
 * Writes protobuf's wire format into a byte array, or into an output stream through a buffer.
 *
 * @param buffer
 *   the byte array, or the buffer of the output stream
 * @param output
 *   the output stream, or `null` for a byte array
 */
final class SemanticdbOutputStream private (buffer: Array[Byte], output: OutputStream | Null) {

  private var position = 0

  /** Writes the buffer to the output stream, but does not flush the output stream. */
  def flush(): Unit = output match {
    case out: OutputStream =>
      out.write(buffer, 0, position)
      position = 0
    case null =>
  }

  /** Checks that the byte array is full. */
  def checkNoSpaceLeft(): Unit = if (position != buffer.length)
    throw new IllegalStateException("Did not write as much data as expected.")

  def writeTag(fieldNumber: Int, wireType: Int): Unit =
    writeUInt32NoTag(WireFormat.makeTag(fieldNumber, wireType))

  def writeDouble(fieldNumber: Int, value: Double): Unit = {
    writeTag(fieldNumber, WireFormat.WireTypeFixed64)
    writeRawLittleEndian64(java.lang.Double.doubleToRawLongBits(value))
  }

  def writeFloat(fieldNumber: Int, value: Float): Unit = {
    writeTag(fieldNumber, WireFormat.WireTypeFixed32)
    writeRawLittleEndian32(java.lang.Float.floatToRawIntBits(value))
  }

  def writeInt64(fieldNumber: Int, value: Long): Unit = {
    writeTag(fieldNumber, WireFormat.WireTypeVarint)
    writeUInt64NoTag(value)
  }

  def writeInt32(fieldNumber: Int, value: Int): Unit = {
    writeTag(fieldNumber, WireFormat.WireTypeVarint)
    writeUInt64NoTag(value)
  }

  def writeBool(fieldNumber: Int, value: Boolean): Unit = {
    writeTag(fieldNumber, WireFormat.WireTypeVarint)
    writeRawByte(if (value) 1 else 0)
  }

  def writeEnum(fieldNumber: Int, value: Int): Unit = writeInt32(fieldNumber, value)

  def writeString(fieldNumber: Int, value: String): Unit = {
    writeTag(fieldNumber, WireFormat.WireTypeLengthDelimited)
    val bytes = value.getBytes(StandardCharsets.UTF_8).nn
    writeUInt32NoTag(bytes.length)
    writeRawBytes(bytes)
  }

  def writeUInt32NoTag(value: Int): Unit = {
    var v = value
    while ((v & ~0x7f) != 0) {
      writeRawByte(v & 0x7f | 0x80)
      v >>>= 7
    }
    writeRawByte(v)
  }

  private def writeUInt64NoTag(value: Long): Unit = {
    var v = value
    while ((v & ~0x7fL) != 0) {
      writeRawByte(v.toInt & 0x7f | 0x80)
      v >>>= 7
    }
    writeRawByte(v.toInt)
  }

  private def writeRawLittleEndian32(value: Int): Unit = {
    writeRawByte(value)
    writeRawByte(value >> 8)
    writeRawByte(value >> 16)
    writeRawByte(value >> 24)
  }

  private def writeRawLittleEndian64(value: Long): Unit = {
    writeRawLittleEndian32(value.toInt)
    writeRawLittleEndian32((value >> 32).toInt)
  }

  private def writeRawByte(value: Int): Unit = {
    if (position == buffer.length) refreshBuffer()
    buffer(position) = value.toByte
    position += 1
  }

  private def writeRawBytes(value: Array[Byte]): Unit =
    if (value.length <= buffer.length - position) {
      System.arraycopy(value, 0, buffer, position, value.length)
      position += value.length
    } else output match {
      case out: OutputStream =>
        flush()
        out.write(value)
      case null => throw new SemanticdbOutputStream.OutOfSpaceException
    }

  private def refreshBuffer(): Unit =
    if (output == null) throw new SemanticdbOutputStream.OutOfSpaceException else flush()

}

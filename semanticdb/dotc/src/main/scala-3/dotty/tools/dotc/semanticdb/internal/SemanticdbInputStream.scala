/*
 * Protocol Buffers - Google's data interchange format
 * Copyright 2008 Google Inc.  All rights reserved.
 *
 * Use of this source code is governed by a BSD-style
 * license that can be found in META-INF/LICENSE-protobuf.txt or at
 * https://developers.google.com/open-source/licenses/bsd
 *
 * Adapted by scalameta from CodedInputStream of protobuf-java and of its port in
 * scalapb/protobuf-scala-runtime: changed to the API that the SemanticDB classes need.
 */
package dotty.tools.dotc.semanticdb.internal

import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, StandardCharsets}

object SemanticdbInputStream {

  def newInstance(input: InputStream): SemanticdbInputStream =
    new SemanticdbInputStream(new Array[Byte](BufferSize), 0, input)

  def newInstance(buf: Array[Byte]): SemanticdbInputStream =
    new SemanticdbInputStream(buf, buf.length, null)

  private val BufferSize = 4096
  private val RecursionLimit = 100

}

/**
 * Reads protobuf's wire format from a byte array or from an input stream, which it buffers.
 *
 * @param buffer
 *   the byte array, or the buffer of the input stream
 * @param bufferSize
 *   the number of bytes in `buffer` that are before the current limit
 * @param input
 *   the input stream, or `null` for a byte array
 */
final class SemanticdbInputStream private (
    buffer: Array[Byte],
    private var bufferSize: Int,
    input: InputStream | Null,
) {

  private var bufferPos = 0

  /**
   * The number of input bytes before `buffer`, so the position in the input is
   * `totalBytesRetired + bufferPos`.
   */
  private var totalBytesRetired = 0

  /** The absolute position where the current message ends. */
  private var currentLimit = Int.MaxValue

  /** The number of bytes in `buffer` after `bufferSize`, which are past the current limit. */
  private var bufferSizeAfterLimit = 0
  private var lastTag = 0
  private var recursionDepth = 0
  private val utf8 = StandardCharsets.UTF_8.nn.newDecoder().nn

  /** Returns `true` at the end of the input or at the current limit. */
  def isAtEnd: Boolean = bufferPos == bufferSize && !tryRefillBuffer(1)

  /** Returns the next tag, or 0 at the end of the input or at the current limit. */
  def readTag(): Int = {
    if (isAtEnd) lastTag = 0
    else {
      lastTag = readRawVarint32()
      if (WireFormat.getTagFieldNumber(lastTag) == 0)
        throw InvalidProtocolBufferException.invalidTag()
    }
    lastTag
  }

  def checkLastTagWas(value: Int): Unit =
    if (lastTag != value) throw InvalidProtocolBufferException.invalidEndTag()

  /** Skips a field, and returns `false` if its tag ends a group. */
  def skipField(tag: Int): Boolean = WireFormat.getTagWireType(tag) match {
    case WireFormat.WireTypeVarint =>
      readRawVarint64()
      true
    case WireFormat.WireTypeFixed64 =>
      skipRawBytes(8)
      true
    case WireFormat.WireTypeLengthDelimited =>
      skipRawBytes(readRawVarint32())
      true
    case WireFormat.WireTypeStartGroup =>
      if (recursionDepth >= SemanticdbInputStream.RecursionLimit)
        throw InvalidProtocolBufferException.recursionLimitExceeded()
      recursionDepth += 1
      skipMessage()
      recursionDepth -= 1
      checkLastTagWas(
        WireFormat.makeTag(WireFormat.getTagFieldNumber(tag), WireFormat.WireTypeEndGroup),
      )
      true
    case WireFormat.WireTypeEndGroup => false
    case WireFormat.WireTypeFixed32 =>
      skipRawBytes(4)
      true
    case _ => throw InvalidProtocolBufferException.invalidWireType()
  }

  def readDouble(): Double = java.lang.Double.longBitsToDouble(readRawLittleEndian64())
  def readFloat(): Float = java.lang.Float.intBitsToFloat(readRawLittleEndian32())
  def readInt64(): Long = readRawVarint64()
  def readInt32(): Int = readRawVarint32()
  def readBool(): Boolean = readRawVarint64() != 0
  def readEnum(): Int = readRawVarint32()

  /** Reads a string, with U+FFFD in place of each malformed sequence of UTF-8. */
  def readString(): String = {
    val size = readSize()
    if (size <= bufferSize - bufferPos) {
      val result = new String(buffer, bufferPos, size, StandardCharsets.UTF_8)
      bufferPos += size
      result
    } else new String(readRawBytesSlowPath(size), StandardCharsets.UTF_8)
  }

  /** Reads a string, which must be valid UTF-8. */
  def readStringRequireUtf8(): String = {
    val size = readSize()
    if (size <= bufferSize - bufferPos) {
      val pos = bufferPos
      bufferPos += size
      requireUtf8(buffer, pos, size)
    } else requireUtf8(readRawBytesSlowPath(size), 0, size)
  }

  /** Reads a varint, and discards the bits past 32. */
  def readRawVarint32(): Int = readRawVarint64().toInt

  private def readRawVarint64(): Long = {
    var result = 0L
    var shift = 0
    while (shift < 64) {
      val b = readRawByte()
      result |= (b & 0x7f).toLong << shift
      if (b >= 0) return result
      shift += 7
    }
    throw InvalidProtocolBufferException.malformedVarint()
  }

  /**
   * Sets a new limit inside the current one, such as the end of a nested message.
   *
   * @param byteLimit
   *   the number of bytes from the position to the new limit
   * @return
   *   the limit to pass to [[popLimit]] when the nested message ends
   */
  def pushLimit(byteLimit: Int): Int = {
    if (byteLimit < 0) throw InvalidProtocolBufferException.negativeSize()
    val limit = byteLimit.toLong + totalBytesRetired + bufferPos
    val oldLimit = currentLimit
    if (limit > oldLimit) throw InvalidProtocolBufferException.truncatedMessage()
    currentLimit = limit.toInt
    recomputeBufferSizeAfterLimit()
    oldLimit
  }

  def popLimit(oldLimit: Int): Unit = {
    currentLimit = oldLimit
    recomputeBufferSizeAfterLimit()
  }

  /** Reads until the end of the input or until a tag that ends a group. */
  private def skipMessage(): Unit = {
    var tag = readTag()
    while (tag != 0 && skipField(tag)) tag = readTag()
  }

  /** Returns the string that a range of bytes encodes. The bytes must be valid UTF-8. */
  private def requireUtf8(bytes: Array[Byte], offset: Int, size: Int): String = {
    val result = new String(bytes, offset, size, StandardCharsets.UTF_8)
    // `new String` replaces malformed UTF-8 with U+FFFD, so decode strictly only a result with it
    try if (result.indexOf('\uFFFD') >= 0) utf8.decode(ByteBuffer.wrap(bytes, offset, size))
    catch { case _: CharacterCodingException => throw InvalidProtocolBufferException.invalidUtf8() }
    result
  }

  private def readSize(): Int = {
    val size = readRawVarint32()
    if (size < 0) throw InvalidProtocolBufferException.negativeSize()
    size
  }

  private def readRawByte(): Byte = {
    if (bufferPos == bufferSize) refillBuffer(1)
    val b = buffer(bufferPos)
    bufferPos += 1
    b
  }

  private def readRawLittleEndian32(): Int = {
    if (bufferSize - bufferPos < 4) refillBuffer(4)
    val pos = bufferPos
    bufferPos += 4
    buffer(pos) & 0xff | (buffer(pos + 1) & 0xff) << 8 | (buffer(pos + 2) & 0xff) << 16 |
      (buffer(pos + 3) & 0xff) << 24
  }

  private def readRawLittleEndian64(): Long = {
    val low = readRawLittleEndian32()
    readRawLittleEndian32().toLong << 32 | low & 0xffffffffL
  }

  /**
   * Returns the next bytes of the input.
   *
   * @param size
   *   the number of bytes, more than the buffer has
   */
  private def readRawBytesSlowPath(size: Int): Array[Byte] = {
    if (input == null || totalBytesRetired.toLong + bufferPos + size > currentLimit)
      throw InvalidProtocolBufferException.truncatedMessage()
    val bytes = new Array[Byte](size)
    var pos = 0
    while (pos < size) {
      if (bufferPos == bufferSize) refillBuffer(1)
      val n = math.min(size - pos, bufferSize - bufferPos)
      System.arraycopy(buffer, bufferPos, bytes, pos, n)
      bufferPos += n
      pos += n
    }
    bytes
  }

  private def skipRawBytes(size: Int): Unit = {
    if (size < 0) throw InvalidProtocolBufferException.negativeSize()
    if (totalBytesRetired.toLong + bufferPos + size > currentLimit)
      throw InvalidProtocolBufferException.truncatedMessage()
    var left = size
    while (left > bufferSize - bufferPos) {
      left -= bufferSize - bufferPos
      bufferPos = bufferSize
      refillBuffer(1)
    }
    bufferPos += left
  }

  private def refillBuffer(n: Int): Unit =
    if (!tryRefillBuffer(n)) throw InvalidProtocolBufferException.truncatedMessage()

  private def tryRefillBuffer(n: Int): Boolean = input match {
    case in: InputStream if totalBytesRetired.toLong + bufferPos + n <= currentLimit =>
      // the guard holds only if the limit is past the buffer, so `bufferSizeAfterLimit` is 0
      System.arraycopy(buffer, bufferPos, buffer, 0, bufferSize - bufferPos)
      totalBytesRetired += bufferPos
      bufferSize -= bufferPos
      bufferPos = 0
      var eof = false
      while (!eof && bufferSize < n) {
        val read = in.read(buffer, bufferSize, buffer.length - bufferSize)
        if (read == 0 || read < -1)
          throw new IllegalStateException(s"InputStream#read(byte[]) returned $read")
        if (read == -1) eof = true
        else {
          bufferSize += read
          recomputeBufferSizeAfterLimit()
        }
      }
      bufferSize >= n
    case _ => false
  }

  private def recomputeBufferSizeAfterLimit(): Unit = {
    bufferSize += bufferSizeAfterLimit
    val bufferEnd = totalBytesRetired + bufferSize
    bufferSizeAfterLimit = math.max(0, bufferEnd - currentLimit)
    bufferSize -= bufferSizeAfterLimit
  }

}

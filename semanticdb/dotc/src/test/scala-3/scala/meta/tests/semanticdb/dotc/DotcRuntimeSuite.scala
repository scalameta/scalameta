package scala.meta.tests.semanticdb.dotc

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, FilterInputStream}

import dotty.tools.dotc.semanticdb.internal.{InvalidProtocolBufferException, MD5,
  SemanticdbInputStream, SemanticdbOutputStream}
import dotty.tools.dotc.{semanticdb => d}

class DotcRuntimeSuite extends munit.FunSuite {

  /**
   * Returns an input stream whose `read` returns one byte at a time, so that SemanticdbInputStream
   * refills its buffer after every byte.
   */
  private def slowInput(bytes: Array[Byte]) = {
    val in = new ByteArrayInputStream(bytes)
    new FilterInputStream(in) {
      override def read(b: Array[Byte], off: Int, len: Int): Int = super.read(b, off, 1)
    }
  }

  test("a message larger than the buffer, through streams") {
    val symbols = List(d.SymbolInformation("a/B#"))
    val document = d.TextDocument(uri = "a/B.scala", text = "é" * 5000, symbols = symbols)
    val doc = d.TextDocuments(List(document))
    val out = new ByteArrayOutputStream
    val output = SemanticdbOutputStream.newInstance(out)
    doc.writeTo(output)
    output.flush()
    assertEquals(out.toByteArray.toList, doc.toByteArray.toList)
    assertEquals(
      d.TextDocuments.parseFrom(SemanticdbInputStream.newInstance(slowInput(out.toByteArray))),
      doc,
    )
  }

  test("the calls of mtags-shared") {
    val out = new ByteArrayOutputStream
    val output = SemanticdbOutputStream.newInstance(out)
    output.writeString(1, "a")
    output.writeString(2, "é")
    output.flush()
    val input = SemanticdbInputStream.newInstance(slowInput(out.toByteArray))
    val fields = List.newBuilder[(Int, String)]
    while (!input.isAtEnd) fields += input.readTag() -> input.readString()
    assertEquals(fields.result(), List(10 -> "a", 18 -> "é"))
  }

  test("a string of invalid UTF-8") {
    val bytes = Array[Byte](18, 2, 'a'.toByte, 0xff.toByte)
    intercept[InvalidProtocolBufferException](d.TextDocument.parseFrom(bytes))
    intercept[InvalidProtocolBufferException](
      d.TextDocument.parseFrom(SemanticdbInputStream.newInstance(slowInput(bytes))),
    )
    val input = SemanticdbInputStream.newInstance(bytes)
    assertEquals(input.readTag() -> input.readString(), 18 -> "a\uFFFD")
  }

  test("unknown fields of every wire type") {
    val out = new ByteArrayOutputStream
    val output = com.google.protobuf.CodedOutputStream.newInstance(out)
    output.writeInt64(5, -1L)
    output.writeDouble(6, 1.5)
    output.writeString(7, "é")
    output.writeTag(8, 3) // a group, which holds a field
    output.writeFixed32(9, 1)
    output.writeTag(8, 4)
    output.writeFloat(10, 1.5f)
    output.writeInt32(1, 7)
    output.flush()
    assertEquals(d.Range.parseFrom(out.toByteArray), d.Range(startLine = 7))
  }

  test("MD5")(assertEquals(MD5.compute("é"), "66DDCD97CFDEABB2F6FB8A999B4BC76F"))

}

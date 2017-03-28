package scala.meta.internal.io

import scala.annotation.tailrec

import scala.meta.io._

import java.io.InputStream
import java.io.InputStreamReader
import java.nio.charset.Charset

object InputStreamIO {
  def slurp(stream: InputStream, charset: Charset): String = {
    val reader = new InputStreamReader(stream, charset)
    val buffer = new Array[Char](4096)
    val builder = new java.lang.StringBuilder

    @tailrec def loop(): Unit = {
      val charsRead = reader.read(buffer)
      if (charsRead >= 0) {
        builder.append(buffer, 0, charsRead)
        loop()
      }
    }

    loop()
    reader.close()
    builder.toString
  }
}

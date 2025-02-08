package scala.meta.internal.io

import java.io._

trait InputStreamIO[A] {
  def read(is: Array[Byte]): A
  def read(is: InputStream): A
}

object InputStreamIO {
  def readBytes(is: InputStream): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val buffer = new Array[Byte](4096)
    var nread = -1
    while ({
      nread = is.read(buffer, 0, buffer.length)
      nread != -1
    }) baos.write(buffer, 0, nread)
    baos.toByteArray
  }
}

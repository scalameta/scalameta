package star.meta.internal.io

import java.io._

object InputStreamIO {
  def readBytes(is: InputStream): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val buffer = new Array[Byte](4096)
    var nread = -1
    do {
      nread = is.read(buffer, 0, buffer.length)
      if (nread != -1) baos.write(buffer, 0, nread)
    } while (nread != -1)
    baos.toByteArray
  }
}

package scala.meta.internal.semanticdb.scalac

object Hex {
  // copied from scalameta/metals, see
  // https://github.com/scalameta/metals/blob/8ba8ac9c29d4ab51424de9ad1c9a2e86d956a75d/mtags/src/main/scala/scala/meta/internal/mtags/MD5.scala#L16-L27

  private val hexArray = "0123456789ABCDEF".toCharArray
  def bytesToHex(bytes: Array[Byte]): String = {
    val hexChars = new Array[Char](bytes.length * 2)
    var j = 0
    while (j < bytes.length) {
      val v: Int = bytes(j) & 0xff
      hexChars(j * 2) = hexArray(v >>> 4)
      hexChars(j * 2 + 1) = hexArray(v & 0x0f)
      j += 1
    }
    new String(hexChars)
  }
}

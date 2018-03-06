package scala.meta.internal.metacp

import java.nio.file.Files
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

import org.langmeta.io.AbsolutePath

object Checksum {
  def apply(file: AbsolutePath): String = {
    require(file.isFile, s"$file is not a regular file")
    val digest = MessageDigest.getInstance("md5")
    // Alternative checksum approaches:
    // * digest entire file contents: slower but more correct
    // * crc32 checksum entries for zip/jar files: more complicated
    digest.update(file.toString.getBytes())
    digest.update(Files.getLastModifiedTime(file.toNIO).toMillis.toByte)
    digest.update(Files.size(file.toNIO).toByte)
    val hash = DatatypeConverter.printHexBinary(digest.digest())
    hash
  }
}

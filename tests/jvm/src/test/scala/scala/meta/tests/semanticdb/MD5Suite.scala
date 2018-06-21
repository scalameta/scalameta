package scala.meta.tests.semanticdb

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import java.security.DigestInputStream
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter
import org.scalatest.FunSuite
import org.scalatest.tagobjects.Slow
import scala.collection.mutable
import scala.meta.internal.semanticdb.Locator
import scala.meta.io.AbsolutePath
import scala.meta.tests.BuildInfo

class MD5Suite extends FunSuite {

  def fileMD5(file: AbsolutePath): String = {
    val fos = Files.newInputStream(file.toNIO)
    val md = MessageDigest.getInstance("MD5")
    val dis = new DigestInputStream(fos, md)
    try {
      while (dis.read() != -1) ()
    } finally {
      fos.close()
    }
    DatatypeConverter.printHexBinary(md.digest())
  }

  def stringMD5(string: String): String = {
    val md = MessageDigest.getInstance("MD5")
    md.update(string.getBytes(StandardCharsets.UTF_8))
    DatatypeConverter.printHexBinary(md.digest())
  }

  val md5Fingerprings = mutable.Set.empty[String]

  Locator(Paths.get(BuildInfo.databaseClasspath)) { (_, docs) =>
    val doc = docs.documents.head
    test(doc.uri, Slow) {
      val fromText = stringMD5(doc.text)
      assert(
        doc.md5 == fromText,
        "TextDocument.md5 does not match stringMD5(TextDocument.md5)"
      )
      val fromFile = fileMD5(AbsolutePath(doc.uri))
      assert(
        doc.md5 == fromFile,
        "TextDocument.md5 does not match fileMD5(Paths.get(TextDocument.uri))"
      )
      assert(!md5Fingerprings.contains(doc.md5), "Fingerprint was not unique")
      md5Fingerprings += doc.md5
    }
  }

}

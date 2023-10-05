package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.Locator
import scala.meta.internal.semanticdb.scalac.Hex
import scala.meta.internal.semanticdb.scalac.SemanticdbPaths
import scala.meta.io.AbsolutePath
import scala.meta.io.RelativePath
import scala.meta.tests.BuildInfo
import scala.meta.tests.Slow

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.DigestInputStream
import java.security.MessageDigest

import scala.collection.mutable

import munit.FunSuite

class MD5Suite extends FunSuite {

  def fileMD5(file: AbsolutePath): String = {
    val fos = Files.newInputStream(file.toNIO)
    val md = MessageDigest.getInstance("MD5")
    val dis = new DigestInputStream(fos, md)
    try while (dis.read() != -1) ()
    finally fos.close()
    Hex.bytesToHex(md.digest())
  }

  def stringMD5(string: String): String = {
    val md = MessageDigest.getInstance("MD5")
    md.update(string.getBytes(StandardCharsets.UTF_8))
    Hex.bytesToHex(md.digest())
  }

  val md5Fingerprints = mutable.Set.empty[String]

  private val databaseClasspath: Path = Paths.get(BuildInfo.databaseClasspath)
  Locator(databaseClasspath) { (_, docs) =>
    val doc = docs.documents.head
    test(doc.uri.tag(Slow)) {
      val fromText = stringMD5(doc.text)
      assertEquals(doc.md5, fromText, "TextDocument.md5 does not match stringMD5(TextDocument.md5)")

      val scalaFile = {
        val targetroot = AbsolutePath(databaseClasspath)
        val semanticdbFile = SemanticdbPaths.toSemanticdb(doc, targetroot)
        SemanticdbPaths.toScala(semanticdbFile, AbsolutePath.workingDirectory, targetroot)
      }

      val fromFile = fileMD5(scalaFile)
      assertEquals(
        doc.md5,
        fromFile,
        "TextDocument.md5 does not match fileMD5(Paths.get(TextDocument.uri))"
      )
      assert(!md5Fingerprints.contains(doc.md5), "Fingerprint was not unique")
      md5Fingerprints += doc.md5
    }
  }

}

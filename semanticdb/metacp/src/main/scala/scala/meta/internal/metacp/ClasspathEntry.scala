package scala.meta.internal.metacp

import java.io.File
import java.io.FilterInputStream
import java.io.InputStream
import java.nio.file.Files
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.meta.io.AbsolutePath

/** Represents a entry in a classpath that is either a package, file on disk or zip entry. */
sealed abstract class ClasspathEntry {
  def name: String
}

/** A classpath entry that can be read as an InputStream. */
sealed abstract class ReadableClasspathEntry extends ClasspathEntry {

  /** Returns an input stream to read the bytes of this classpath entry.
    *
    * @note The caller is responsible for closing the InputStream.
    */
  def openInputStream(): InputStream
}

object ClasspathEntry {
  final case class Package(name: String) extends ClasspathEntry {
    val members = mutable.Map.empty[String, ClasspathEntry]
  }
  final case class OnDisk(path: AbsolutePath) extends ReadableClasspathEntry {
    override def name: String = path.toString
    def openInputStream(): InputStream =
      Files.newInputStream(path.toNIO)
  }
  final case class InJar(entry: ZipEntry, zip: File) extends ReadableClasspathEntry {
    override def name: String = entry.getName
    def openInputStream(): InputStream = {
      val openFile = new ZipFile(zip)
      val delegate = openFile.getInputStream(entry)
      new FilterInputStream(delegate) {
        override def close(): Unit = openFile.close()
      }
    }
  }
}

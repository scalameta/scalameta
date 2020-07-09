package scala.meta.internal.classpath

import java.io.File
import java.io.FilterInputStream
import java.io.InputStream
import java.nio.file.Files
import java.nio.file.Path
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.meta.io.AbsolutePath

/** Represents a entry in a classpath that is either a package, file on disk or zip entry. */
sealed abstract class ClasspathElement {
  def relativeUri: String
}

/** A classpath entry that can be read as an InputStream. */
sealed abstract class Classfile extends ClasspathElement {

  /**
   * Returns an input stream to read the bytes of this classpath entry.
   *
   * @note The caller is responsible for closing the InputStream.
   */
  def openInputStream(): InputStream
}

/** A classpath entry that is a directory. */
final case class Classdir(relativeUri: String) extends ClasspathElement {
  def resolve(filename: String): Option[ClasspathElement] = {
    members.get(filename).orElse {
      val uri = relativeUri + filename
      modules.iterator
        .map(_.resolve(uri))
        .find(Files.exists(_))
        .map(x => UncompressedClassfile(uri, AbsolutePath(x)))
    }
  }
  val members = mutable.Map.empty[String, ClasspathElement]

  /**
   * Java 9+ modules for this package based on JEP-220
   *
   * Details: https://bugs.openjdk.java.net/browse/JDK-8066492
   *
   * For example, the package "java/lang/" will have a module "/modules/java.base" which is the
   * root directory containing classfiles for JDK libraries like `java/lang/Thread#`.
   */
  var modules: List[Path] = Nil
}

/** A classpath entry that is a classfile on disk. */
final case class UncompressedClassfile(relativeUri: String, path: AbsolutePath) extends Classfile {
  def openInputStream(): InputStream =
    Files.newInputStream(path.toNIO)
}

/** A classpath entry that is a classfile inside a jar file. */
final case class CompressedClassfile(entry: ZipEntry, zip: File) extends Classfile {
  override def relativeUri: String = entry.getName
  def openInputStream(): InputStream = {
    val openFile = new ZipFile(zip)
    val delegate = openFile.getInputStream(entry)
    new FilterInputStream(delegate) {
      override def close(): Unit = openFile.close()
    }
  }
}

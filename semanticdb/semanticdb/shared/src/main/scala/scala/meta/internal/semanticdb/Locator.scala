package scala.meta.internal.semanticdb

import org.scalameta.collections.Conversions._

import java.io.InputStream
import java.nio.file._
import java.util.jar._

object Locator {
  type Visitor = (Path, () => TextDocuments) => Unit

  def apply(paths: List[Path])(fn: Visitor): Unit = paths.foreach(path => apply(path)(fn))

  def apply(path: Path)(fn: Visitor): Unit = if (Files.exists(path))
    if (Files.isDirectory(path)) Files.walk(path).iterator().toScala
      .filter(_.toString.endsWith(".semanticdb")).toArray
      // NOTE: nio.file.Path.compareTo is file system specific,
      // and the behavior is different on windows vs. unix
      .sortBy(_.toString.toLowerCase).foreach(path => visit(path, Files.newInputStream(path), fn))
    else if (path.toString.endsWith(".jar")) {
      // NOTE: Can't use nio.Files.walk because nio.FileSystems
      // is not supported on Scala Native.
      val jar = new JarFile(path.toFile)
      val buf = List.newBuilder[JarEntry]
      val jarIt = jar.entries()
      while (jarIt.hasMoreElements) {
        val jarEntry = jarIt.nextElement()
        if (jarEntry.getName.endsWith(".semanticdb")) buf += jarEntry
      }
      val jarEntries = buf.result().sortBy(_.getName.toLowerCase)
      jarEntries.foreach { jarEntry =>
        val path = Paths.get(jarEntry.getName)
        visit(path, jar.getInputStream(jarEntry), fn)
      }
      val manifest = jar.getManifest
      if (manifest != null) {
        val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
        if (classpathAttr != null) classpathAttr.split(" ").foreach { relativePath =>
          val parentPath = path.toAbsolutePath.getParent
          apply(parentPath.resolve(relativePath))(fn)
        }
      }
    } else if (path.toString.endsWith(".semanticdb")) visit(path, Files.newInputStream(path), fn)

  private def visit(path: Path, stream: InputStream, fn: Visitor): Unit =
    try fn(path, () => TextDocuments.parseFrom(stream))
    finally stream.close()

}

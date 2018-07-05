package scala.meta.internal.semanticdb

import java.nio.file._
import java.util.jar._
import scala.collection.JavaConverters._

object Locator {
  def apply(paths: List[Path])(fn: (Path, TextDocuments) => Unit): Unit = {
    paths.foreach(path => apply(path)(fn))
  }

  def apply(path: Path)(fn: (Path, TextDocuments) => Unit): Unit = {
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) {
        Files
          .walk(path)
          .iterator()
          .asScala
          .filter(_.toString.endsWith(".semanticdb"))
          .toArray
          // NOTE: nio.file.Path.compareTo is file system specific,
          // and the behavior is different on windows vs. unix
          .sortBy(_.toString.toLowerCase)
          .foreach { path =>
            val stream = Files.newInputStream(path)
            try fn(path, TextDocuments.parseFrom(stream))
            finally stream.close()
          }
      } else {
        if (path.toString.endsWith(".jar")) {
          // NOTE: Can't use nio.Files.walk because nio.FileSystems
          // is not supported on Scala Native.
          val jar = new JarFile(path.toFile)
          val buf = List.newBuilder[JarEntry]
          val jarIt = jar.entries()
          while (jarIt.hasMoreElements) {
            val jarEntry = jarIt.nextElement()
            if (jarEntry.getName.endsWith(".semanticdb")) {
              buf += jarEntry
            }
          }
          val jarEntries = buf.result.sortBy(_.getName.toLowerCase)
          jarEntries.foreach { jarEntry =>
            val path = Paths.get(jarEntry.getName)
            val stream = jar.getInputStream(jarEntry)
            try fn(path, TextDocuments.parseFrom(stream))
            finally stream.close()
          }
          val manifest = jar.getManifest
          if (manifest != null) {
            val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
            if (classpathAttr != null) {
              classpathAttr.split(" ").foreach { relativePath =>
                val parentPath = path.toAbsolutePath.getParent
                apply(parentPath.resolve(relativePath))(fn)
              }
            }
          }
        } else if (path.toString.endsWith(".semanticdb")) {
          val stream = Files.newInputStream(path)
          try fn(path, TextDocuments.parseFrom(stream))
          finally stream.close()
        } else {
          ()
        }
      }
    } else {
      ()
    }
  }
}

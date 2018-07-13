package scala.meta.internal

import java.util.jar.Manifest
import java.nio.file.FileVisitor
import java.nio.file.Files
import java.nio.file.Path
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath

package object io {

  implicit class XtensionClasspath(cp: Classpath) {
    def visit(getVisitor: AbsolutePath => FileVisitor[Path]): Unit = {
      cp.foreach { root =>
        val visitor = getVisitor(root.path)
        Files.walkFileTree(root.path.toNIO, visitor)
      }
    }

    def foreach(fn: ClasspathFile => Unit): Unit = {
      def processEntry(base: AbsolutePath, manifestJar: Option[ClasspathFile]): Unit = {
        if (base.isDirectory) {
          fn(ClasspathFile(base, None, manifestJar))
        } else if (base.isFile) {
          PathIO.extension(base.toNIO) match {
            case "jar" =>
              FileIO.withJarFileSystem(base, create = false, close = true) { root =>
                val classpathFile = ClasspathFile(
                  root,
                  enclosingJar = Some(base),
                  enclosingManifestJar = manifestJar
                )
                fn(classpathFile)
                root.toManifest.foreach { manifest =>
                  val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
                  if (classpathAttr != null) {
                    classpathAttr.split(" ").foreach { relativePath =>
                      val jar = base.resolveSibling(_ => relativePath)
                      processEntry(jar, manifestJar = Some(classpathFile))
                    }
                  }
                }
              }
            case _ =>
              sys.error(s"Expected jar file, obtained $base")
          }
        } else {
          ()
        }
      }
      cp.entries.foreach { entry =>
        processEntry(entry, manifestJar = None)
      }
    }
  }

  implicit class XtensionAbsolutePathManifest(root: AbsolutePath) {
    def toManifest: Option[Manifest] = {
      val manifestPath = root.resolve("META-INF/MANIFEST.MF")
      if (manifestPath.isFile) {
        val in = Files.newInputStream(manifestPath.toNIO)
        try Some(new Manifest(in))
        finally in.close()
      } else {
        None
      }
    }
  }

}

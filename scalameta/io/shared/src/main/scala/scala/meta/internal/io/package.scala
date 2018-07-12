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
        val visitor = getVisitor(root)
        Files.walkFileTree(root.toNIO, visitor)
      }
    }
    def foreach(fn: AbsolutePath => Unit): Unit = {
      def processEntry(base: AbsolutePath): Unit = {
        if (base.isDirectory) {
          fn(base)
        } else if (base.isFile) {
          PathIO.extension(base.toNIO) match {
            case "jar" =>
              FileIO.withJarFileSystem(base, create = false, close = true) { root =>
                fn(root)
                val manifestPath = root.resolve("META-INF/MANIFEST.MF")
                if (manifestPath.isFile) {
                  val manifest = new Manifest()
                  val in = Files.newInputStream(manifestPath.toNIO)
                  try manifest.read(in)
                  finally in.close()
                  val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
                  if (classpathAttr != null) {
                    classpathAttr.split(" ").foreach { relativePath =>
                      val jar = base.resolveSibling(_ => relativePath)
                      processEntry(jar)
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
        processEntry(entry)
      }
    }
  }

}

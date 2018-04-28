package scala.meta.internal

import java.nio.file.FileVisitor
import java.nio.file.Files
import java.nio.file.Path
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath

package object io {

  implicit class XtensionClasspath(cp: Classpath) {
    def visit(getVisitor: AbsolutePath => FileVisitor[Path]): Unit = {
      cp.entries.foreach { base =>
        if (base.isDirectory) {
          val visitor = getVisitor(base)
          Files.walkFileTree(base.toNIO, visitor)
        } else if (base.isFile) {
          PathIO.extension(base.toNIO) match {
            case "jar" =>
              val root = FileIO.jarRootPath(base)
              val visitor = getVisitor(root)
              Files.walkFileTree(root.toNIO, visitor)
            case _ =>
              sys.error(s"Expected jar file, obtained $base")
          }
        } else {
          ()
        }
      }
    }
  }

}

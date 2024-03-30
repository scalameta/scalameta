package scala.meta.internal.semanticdb

import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath

import java.nio.file.Files
import java.nio.file.StandardOpenOption

package object scalac {

  implicit class XtensionSchemaTextDocument(private val sdocument: s.TextDocument) extends AnyVal {
    private def write(targetroot: AbsolutePath, append: Boolean): Unit = {
      val openOption =
        if (append) StandardOpenOption.APPEND else StandardOpenOption.TRUNCATE_EXISTING
      val out = SemanticdbPaths.toSemanticdb(sdocument, targetroot)
      val bytes = s.TextDocuments(sdocument :: Nil).toByteArray
      if (!Files.exists(out.toNIO.getParent)) Files.createDirectories(out.toNIO.getParent)
      Files.write(out.toNIO, bytes, StandardOpenOption.CREATE, openOption)
    }
    def save(targetroot: AbsolutePath): Unit = write(targetroot, append = false)
    def append(targetroot: AbsolutePath): Unit = write(targetroot, append = true)
  }

}

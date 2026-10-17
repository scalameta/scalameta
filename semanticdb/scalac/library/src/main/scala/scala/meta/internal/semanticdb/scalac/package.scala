package scala.meta.internal.semanticdb

import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath

import java.io.IOException
import java.nio.file.{Files, Path, StandardCopyOption}

import scala.util.control.NonFatal

package object scalac {

  implicit class XtensionSchemaTextDocument(private val sdocument: s.TextDocument) extends AnyVal {
    private def write(targetroot: AbsolutePath, append: Boolean): Unit = {
      val out = SemanticdbPaths.toSemanticdb(sdocument, targetroot).toNIO
      val existing =
        if (append && Files.exists(out))
          try s.TextDocuments.parseFrom(Files.readAllBytes(out)).documents
          catch { case NonFatal(_) => Nil }
        else Nil
      val bytes = s.TextDocuments(existing :+ sdocument).toByteArray
      if (!Files.exists(out.getParent)) Files.createDirectories(out.getParent)
      replace(out, bytes)
    }
    def save(targetroot: AbsolutePath): Unit = write(targetroot, append = false)
    def append(targetroot: AbsolutePath): Unit = write(targetroot, append = true)
  }

  /* move a sibling over `out`, so that a symlink at `out`, such as one into a build cache, is
   * replaced rather than written through */
  private def replace(out: Path, bytes: Array[Byte]): Unit = {
    // a fixed name, so that the next write reuses a temp file that a killed run left behind
    val tmp = out.resolveSibling(s"${out.getFileName}.tmp")
    try {
      Files.write(tmp, bytes)
      try Files.move(tmp, out, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      catch { case _: IOException => Files.move(tmp, out, StandardCopyOption.REPLACE_EXISTING) }
    } catch { case _: IOException => Files.write(out, bytes) }
    finally
      try Files.deleteIfExists(tmp)
      catch { case _: IOException => }
  }

}

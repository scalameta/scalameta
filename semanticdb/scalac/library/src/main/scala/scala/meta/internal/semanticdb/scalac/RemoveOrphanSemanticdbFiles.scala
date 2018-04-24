package scala.meta.internal.semanticdb.scalac

import java.io.IOException
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath

object RemoveOrphanSemanticdbFiles {

  /** Removes *.semanticdb files that have no corresponding *.scala file. */
  def process(sourceroot: AbsolutePath, targetroot: AbsolutePath): Unit = {
    Files.walkFileTree(
      targetroot.resolve(SemanticdbPaths.semanticdbPrefix).toNIO,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(file.getFileName) == SemanticdbPaths.semanticdbExtension) {
            val scalafile = SemanticdbPaths.toScala(AbsolutePath(file), sourceroot, targetroot)
            if (!scalafile.isFile) {
              Files.delete(file)
            }
          }
          FileVisitResult.CONTINUE
        }
        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          val files = Files.newDirectoryStream(dir)
          val isEmpty = !files.iterator().hasNext
          files.close()
          if (isEmpty) {
            Files.delete(dir)
          }
          FileVisitResult.CONTINUE
        }
      }
    )
  }

}

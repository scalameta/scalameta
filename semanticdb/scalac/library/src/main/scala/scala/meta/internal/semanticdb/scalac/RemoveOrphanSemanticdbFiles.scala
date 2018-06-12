package scala.meta.internal.semanticdb.scalac

import java.io.IOException
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath

object RemoveOrphanSemanticdbFiles {

  /** Removes *.semanticdb files that have no corresponding *.scala file. */
  def process(config: SemanticdbConfig): Unit = {
    val sourceroot = config.sourceroot
    val targetroot = config.targetroot
    val semanticdbroot = targetroot.resolve(SemanticdbPaths.semanticdbPrefix).toNIO
    if (!Files.exists(semanticdbroot)) return

    Files.walkFileTree(
      semanticdbroot,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(file.getFileName) == SemanticdbPaths.semanticdbExtension) {
            val scalafile = SemanticdbPaths.toScala(AbsolutePath(file), sourceroot, targetroot)
            if (!scalafile.isFile || !config.fileFilter.matches(scalafile.toString)) {
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

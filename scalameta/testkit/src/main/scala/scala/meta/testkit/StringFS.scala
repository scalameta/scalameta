package scala.meta.testkit

import scala.meta.io.AbsolutePath
import java.io.File
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import scala.meta.internal.io.FileIO
import scala.meta.io.RelativePath

object StringFS {

  /**
    * Creates a temporary directory with a layout matching the markup in the string.
    *
    * Example syntax of the expected markup in the string:
    * {{{
    *   fromString("""
    *   /build.sbt
    *   lazy val core = project
    *   /src/main/scala/core/Foo.scala
    *   package core
    *   object Foo.scala
    *   """)
    * }}}
    *
    * Use `asString` for the inverse, go from a temporary directory to a string.
    *
    * @param layout the string representing the directory layout.
    *               NOTE. Lines starting with forward slash / are always interpreted
    *               as the start of a new file entry.
    * @param root the temporary directory to apply the layout markup.
    *             If not provided, defaults to a fresh temporary directory.
    */
  def fromString(
      layout: String,
      root: AbsolutePath = AbsolutePath(Files.createTempDirectory("scalameta")),
      charset: Charset = StandardCharsets.UTF_8
  ): AbsolutePath = {
    if (!layout.trim.isEmpty) {
      layout.split("(?=\n/)").foreach { row =>
        row.stripPrefix("\n").split("\n", 2).toList match {
          case path :: contents :: Nil =>
            val file = root.resolve(path.stripPrefix("/"))
            Files.createDirectories(file.toNIO.getParent)
            Files.write(
              file.toNIO,
              contents.getBytes(charset),
              StandardOpenOption.CREATE,
              StandardOpenOption.TRUNCATE_EXISTING
            )
          case els =>
            throw new IllegalArgumentException(
              s"Unable to split argument info path/contents! \n$els")

        }
      }
    }
    root
  }

  /** Gives a string representation of a directory.
    *
    * Performs the inverse as fromString.
    *
    * Example:
    * {{{
    * val layout = """
    * /Main.scala
    * object A { def foo() = print("foo") }
    * """
    * assert(asString(fromString(layout)) == layout)
    * }}}
    *
    * @param root the directory to print as a string
    * @param includePath an optional filter function to exclude files
    */
  def asString(
      root: AbsolutePath,
      includePath: RelativePath => Boolean = _ => true,
      charset: Charset = StandardCharsets.UTF_8
  ): String = {
    import scala.collection.JavaConverters._
    FileIO
      .listAllFilesRecursively(root)
      .files
      .filter(includePath)
      .sortBy(_.toNIO)
      .map { path =>
        val contents = FileIO.slurp(root.resolve(path), charset)
        s"""|/$path
            |$contents""".stripMargin
      }
      .mkString("\n")
      .replace(File.separatorChar, '/') // ensure original separators
  }

}

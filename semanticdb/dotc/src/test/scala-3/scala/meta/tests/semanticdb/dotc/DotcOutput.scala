package scala.meta.tests.semanticdb.dotc

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters._
import scala.util.Using

/** The output directories of the module, and the files in them. */
object DotcOutput {

  /** ScalaPB's output, before DotcRewriter edits it. */
  val Raw: Path = DotcBuildInfo.raw.toPath
  val Committed: Path = DotcBuildInfo.committed.toPath
  val Classes: Path = DotcBuildInfo.classes.toPath

  /** Returns the text of each file under `dir`, by its path relative to `dir`. */
  def read(dir: Path): SortedMap[String, String] = SortedMap.from(files(dir).map(file =>
    dir.relativize(file).iterator.asScala.mkString("/") ->
      new String(Files.readAllBytes(file), StandardCharsets.UTF_8),
  ))

  /** Returns a line for each path whose file differs between two sets of files. */
  def diff(
      generated: SortedMap[String, String],
      committed: SortedMap[String, String],
  ): List[String] = (generated.keySet ++ committed.keySet).toList.flatMap(path =>
    (generated.get(path), committed.get(path)) match {
      case (Some(g), Some(c)) => if (g == c) None else Some(s"$path differs")
      case (Some(_), None) => Some(s"$path is missing")
      case _ => Some(s"$path is not generated")
    },
  )

  /** Deletes every file under `dir`, and then writes `files`, keyed by their paths under `dir`. */
  def write(dir: Path, files: SortedMap[String, String]): Unit = {
    this.files(dir).foreach(Files.delete)
    files.foreach { case (path, text) =>
      val file = dir.resolve(path)
      Files.createDirectories(file.getParent)
      Files.write(file, text.getBytes(StandardCharsets.UTF_8))
    }
  }

  /** Returns the result of `f` on a new temporary directory, and then deletes the directory. */
  def withTempDir[A](f: Path => A): A = {
    val dir = Files.createTempDirectory("semanticdb-dotc")
    try f(dir)
    finally Using.resource(Files.walk(dir))(_.iterator.asScala.toList.reverse.foreach(Files.delete))
  }

  private def files(dir: Path): List[Path] =
    if (!Files.isDirectory(dir)) Nil
    else Using.resource(Files.walk(dir))(_.iterator.asScala.filter(Files.isRegularFile(_)).toList)

}

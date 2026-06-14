package scala.meta.internal.bench

import scala.meta.AbsolutePath
import scala.meta.internal.io.FileIO

import java.nio.file._

trait FileFixtures {
  lazy val scalapDir: AbsolutePath = AbsolutePath(BuildInfo.sourceroot)
    .resolve("bench/corpus/scalap")

  lazy val scalapFiles: List[Path] = FileIO.listAllFilesRecursively(scalapDir).iterator.map(_.toNIO)
    .toList
}

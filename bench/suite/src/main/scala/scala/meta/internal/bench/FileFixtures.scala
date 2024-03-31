package scala.meta.internal.bench

import org.scalameta.collections._

import java.nio.file._

trait FileFixtures {
  lazy val scalapDir: Path = Paths.get(s"${BuildInfo.sourceroot}/bench/corpus/scalap")

  lazy val scalapFiles: List[Path] = {
    val all = Files.walk(scalapDir).iterator.toScala.toList
    all.filter(p => !Files.isDirectory(p))
  }
}

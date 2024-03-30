package scala.meta.internal.bench

import java.nio.file._
import org.scalameta.collections._

trait FileFixtures {
  lazy val scalapDir: Path = { Paths.get(s"${BuildInfo.sourceroot}/bench/corpus/scalap") }

  lazy val scalapFiles: List[Path] = {
    val all = Files.walk(scalapDir).iterator.toScala.toList
    all.filter(p => !Files.isDirectory(p))
  }
}

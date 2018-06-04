package scala.meta.internal.bench

import java.nio.file._
import scala.collection.JavaConverters._

trait FileFixtures {
  lazy val scalapDir: Path = {
    Paths.get(s"${BuildInfo.sourceroot}/bench/corpus/scalap")
  }

  lazy val scalapFiles: List[Path] = {
    val all = Files.walk(scalapDir).iterator.asScala.toList
    all.filter(p => !Files.isDirectory(p))
  }
}

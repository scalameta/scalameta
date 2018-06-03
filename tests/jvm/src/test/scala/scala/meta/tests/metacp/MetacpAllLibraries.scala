package scala.meta.tests.metacp

import java.nio.file._
import scala.meta.cli._
import scala.meta.io._
import scala.meta.metacp._

object MetacpAllLibraries {
  def main(args: Array[String]): Unit = {
    val cacheDir = AbsolutePath(Files.createTempDirectory("metacp"))
    try {
      Libraries.suite.foreach { library =>
        println(s"Converting ${library.name}...")
        val settings = Settings().withClasspath(library.classpath()).withCacheDir(cacheDir).withPar(true)
        val reporter = Reporter().withOut(System.out).withErr(System.err)
        assert(Metacp.process(settings, reporter).nonEmpty)
      }
    } finally {
      cacheDir.toFile.delete()
    }
  }
}

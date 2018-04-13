package scala.meta.tests.metacp

import org.langmeta.internal.io.PlatformFileIO

class MetacpConcurrencySuite extends BaseMetacpSuite {
  def run(): Unit = {}
  test("the same jar can be processed concurrently") {
    1.to(5).par.foreach { _ =>
      val Some(result) = runMetacp(scalaLibraryClasspath)
      result.shallow.foreach { entry =>
        PlatformFileIO.withJarFileSystem(entry, create = false) { root =>
          root.resolve("META-INF").resolve("semanticdb.semanticidx").readAllBytes
          Thread.sleep(1000)
        }
      }
    }
  }
}

package scala.meta.tests.metacp

class MetacpConcurrencySuite extends BaseMetacpSuite {
  test("the same jar can be processed concurrently") {
    1.to(5).par.foreach { _ =>
      runMetacp(scalaLibraryClasspath)
    }
  }
}

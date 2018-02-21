package scala.meta.tests.metacp

class MetacpSuite extends BaseMetacpSuite {

  checkMetacp("scala-library", () => scalaLibraryJar)

  checkLibrary(scalameta)
  checkLibrary(akka)
  checkLibrary(spark)
}

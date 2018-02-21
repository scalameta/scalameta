package scala.meta.tests.metacp

class MetacpSuite extends BaseMetacpSuite {

  check("scala-library", () => scalaLibraryJar)

  checkLibrary(scalameta)
  checkLibrary(akka)
  checkLibrary(spark)
}

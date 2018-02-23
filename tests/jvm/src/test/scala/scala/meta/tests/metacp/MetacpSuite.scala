package scala.meta.tests.metacp

// TODO(olafur): move to slow/integration tests. Each run takes ~30s on my laptop
// and produces ~350mb of artifacts in a temporary directory.
class MetacpSuite extends BaseMetacpSuite {

  checkMetacp("scala-library", () => scalaLibraryJar)
  checkNoCrashes(scalameta)
  checkNoCrashes(akka)
  checkNoCrashes(spark)

}

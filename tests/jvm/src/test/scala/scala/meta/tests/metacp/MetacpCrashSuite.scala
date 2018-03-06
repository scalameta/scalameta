package scala.meta.tests.metacp

// TODO(olafur): move to slow/integration tests. Each run takes ~20s on my laptop
// and produces ~60mb of artifacts in a temporary directory.
class MetacpCrashSuite extends BaseMetacpSuite {

  checkMetacp("scala-library", () => scalaLibraryJar)
  checkNoCrashes(scalameta)
  checkNoCrashes(akka)
  checkNoCrashes(spark)
  checkNoCrashes(jdk)

}

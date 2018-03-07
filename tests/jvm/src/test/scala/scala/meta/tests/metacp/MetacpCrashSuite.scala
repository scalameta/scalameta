package scala.meta.tests.metacp

// TODO(olafur): move to slow/integration tests. Each run takes ~20s on my laptop
// and produces ~60mb of artifacts in a temporary directory.
class MetacpCrashSuite extends BaseMetacpSuite {

  checkMetacp("scala-library", () => scalaLibraryClasspath)
  checkMetacp(scalameta)
  checkMetacp(akka)
  checkMetacp(spark)
  checkMetacp(jdk)

}

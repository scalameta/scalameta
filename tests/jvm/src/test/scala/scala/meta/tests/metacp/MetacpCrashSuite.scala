package scala.meta.tests.metacp

// TODO(olafur): move to slow/integration tests.
class MetacpCrashSuite extends BaseMetacpSuite {

  checkMetacp("scala-library", () => scalaLibraryClasspath)
  checkMetacp(scalameta)
  checkMetacp(akka)
  checkMetacp(akkaStream211)
  checkMetacp(spark)
  checkMetacp(jdk)
  checkMetacp(play)

}

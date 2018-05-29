package scala.meta.tests.metacp

class MetacpCrashSuite extends BaseMetacpSuite {

  checkMetacp("scala-library", () => scalaLibraryClasspath)
  checkMetacp(scalameta)
  checkMetacp(akka)
  checkMetacp(akkaStream211)
  checkMetacp(spark)
  checkMetacp(jdk)
  checkMetacp(play)

}

package scala.meta.tests.metacp

class MetacpSuite extends BaseMetacpSuite {

  check("scala-library", () => scalaLibraryJar)
  checkLibrary("org.scalameta", "scalameta_2.12", "3.2.0")
  checkLibrary("com.typesafe.akka", "akka-testkit_2.12", "2.5.9")
  checkLibrary("org.apache.spark", "spark-sql_2.11", "2.2.1")

}

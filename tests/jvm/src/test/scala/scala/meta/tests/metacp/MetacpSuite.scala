package scala.meta.tests.metacp

import java.io.File
import java.nio.file.Files
import scala.meta.internal.metacp._
import scala.meta.tests.cli.BaseCliSuite

class MetacpSuite extends BaseCliSuite {

  private val tmp = Files.createTempDirectory("metacp")
  tmp.toFile.deleteOnExit()

  def check(name: String, classpath: () => String): Unit = {
    test(name) {
      val cp = classpath()
      val args = Array[String](
        "-cp",
        cp,
        "-d",
        tmp.toString
      )
      val settings = Settings.parse(args.toList).get
      val exit = Main.process(settings)
      assert(exit == 0)
    }
  }

  def checkLibrary(organization: String, artifact: String, version: String): Unit = {
    check(
      List(organization, artifact, version).mkString(File.pathSeparator), { () =>
        val jars = Jars
          .fetch(organization, artifact, version)
          .filterNot(_.toString.contains("scala-lang"))
        jars.mkString(File.pathSeparator)
      }
    )
  }

  check("scala-library", () => scalaLibraryJar)
  checkLibrary("org.scalameta", "scalameta_2.12", "3.2.0")

  // Akka is blocked by https://github.com/scalameta/scalameta/issues/1306
  // checkLibrary("com.typesafe.akka", "akka-testkit_2.12", "2.5.9")

  // Spark is blocked by https://github.com/scalameta/scalameta/issues/1305
  // checkLibrary("org.apache.spark", "spark-sql_2.11", "2.2.1")

}

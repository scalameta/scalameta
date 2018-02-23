package scala.meta.tests.metacp

import java.io.File
import java.nio.file.Files
import scala.meta.internal.metacp.Main
import scala.meta.internal.metacp.Settings
import scala.meta.tests.cli.BaseCliSuite

abstract class BaseMetacpSuite extends BaseCliSuite {

  private val tmp = Files.createTempDirectory("metacp")
  tmp.toFile.deleteOnExit()

  def checkMetacp(name: String, classpath: () => String): Unit = {
    test(name) {
      val cp = classpath()
      val args = Array[String](
        "-cp",
        cp,
        "-d",
        tmp.toString
      )
      // println(tmp) // uncomment to manually inspect metacp artifacts
      val settings = Settings.parse(args.toList).get
      val exit = Main.process(settings)
      assert(exit == 0)
    }
  }

  def checkNoCrashes(library: Library): Unit = {
    checkMetacp(library.name, () => library.classpath())
  }

  val scalameta = Library("org.scalameta", "scalameta_2.12", "3.2.0")
  val akka = Library("com.typesafe.akka", "akka-testkit_2.12", "2.5.9")
  val spark = Library("org.apache.spark", "spark-sql_2.11", "2.2.1")
  val kafka = Library("org.apache.kafka", "kafka_2.12", "1.0.0")
  val flink = Library("org.apache.flink", "flink-parent", "1.4.1")
  val grpc = Library("io.grpc", "grpc-all", "1.10.0")

  val allLibraries = List(
    scalameta,
    akka,
    spark,
    kafka,
    flink,
    grpc
  )

}

case class Library(organization: String, artifact: String, version: String) {
  def name = List(organization, artifact, version).mkString(":")
  def classpath(): String = {
    val jars = Jars
      .fetch(organization, artifact, version)
      .filterNot(_.toString.contains("scala-lang"))
    jars.mkString(File.pathSeparator)
  }
}

package scala.meta.tests.metacp

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.mutable.ArrayBuffer
import scala.meta.internal.metacp.Javacp
import scala.meta.internal.metacp.Main
import scala.meta.internal.metacp.Settings
import scala.meta.tests.cli.BaseCliSuite
import scala.tools.asm.tree.ClassNode
import scala.util.control.NonFatal
import org.langmeta.internal.io.PathIO
import org.langmeta.io.Classpath

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
      println(tmp)
      val settings = Settings.parse(args.toList).get
      val exit = Main.process(settings)
      assert(exit == 0)
    }
  }

  def checkLibrary(coordinates: Coordinates): Unit = {
    checkMetacp(coordinates.name, coordinates.classpath)
  }

  val scalameta = Coordinates("org.scalameta", "scalameta_2.12", "3.2.0")
  val akka = Coordinates("com.typesafe.akka", "akka-testkit_2.12", "2.5.9")
  val spark = Coordinates("org.apache.spark", "spark-sql_2.11", "2.2.1")
  val kafka = Coordinates("org.apache.kafka", "kafka_2.12", "1.0.0")
  val flink = Coordinates("org.apache.flink", "flink-parent", "1.4.1")
  val grpc = Coordinates("io.grpc", "grpc-all", "1.10.0")

  val allLibraries = List(
    scalameta,
    akka,
    spark,
    kafka,
    flink,
    grpc
  )

}

case class Coordinates(organization: String, artifact: String, version: String) {
  def name = List(organization, artifact, version).mkString(File.pathSeparator)
  def classpath: () => String = { () =>
    val jars = Jars
      .fetch(organization, artifact, version)
      .filterNot(_.toString.contains("scala-lang"))
    jars.mkString(File.pathSeparator)
  }
}

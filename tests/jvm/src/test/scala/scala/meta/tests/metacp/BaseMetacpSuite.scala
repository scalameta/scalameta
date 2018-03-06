package scala.meta.tests.metacp

import java.io.File
import java.io.OutputStream
import java.io.PrintStream
import java.nio.file.Files

import org.langmeta.io.AbsolutePath
import org.langmeta.io.Classpath

import scala.meta.cli.Metacp
import scala.meta.tests.cli.BaseCliSuite

abstract class BaseMetacpSuite extends BaseCliSuite {

  val tmp: AbsolutePath = AbsolutePath(Files.createTempDirectory("metacp"))
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
      val devnull = new PrintStream(new OutputStream {
        override def write(b: Int): Unit = ()
      })
      val exit = Metacp.process(args, devnull, System.err)
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

  def bootClasspath: String =
    Classpath(
      sys.props
        .collectFirst { case (k, v) if k.endsWith(".boot.class.path") => v }
        .getOrElse("")).shallow
      .filter(p => Files.isRegularFile(p.toNIO))
      .mkString(File.pathSeparator)

  val jdk = Library("JDK", () => bootClasspath)

  val allLibraries = List[Library](
    scalameta,
    akka,
    spark,
    kafka,
    flink,
    grpc,
    jdk
  )

}

case class Library(name: String, classpath: () => String)
object Library {
  def apply(organization: String, artifact: String, version: String): Library =
    Library(
      List(organization, artifact, version).mkString(":"), { () =>
        val jars = Jars
          .fetch(organization, artifact, version)
          .filterNot(_.toString.contains("scala-lang"))
        jars.mkString(File.pathSeparator)
      }
    )
}

package scala.meta.tests.metacp

import java.io.File
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.tests._

object Libraries {
  lazy val suite: List[Library] = {
    val buf = List.newBuilder[Library]
    buf += Library.jdk
    buf += Library.scalaLibrary
    buf += Library("org.scalameta", "scalameta_2.12", "3.2.0")
    buf += Library("com.typesafe.akka", "akka-testkit_2.12", "2.5.9")
    buf += Library("com.typesafe.akka", "akka-stream_2.11", "2.5.9")
    buf += Library("org.apache.spark", "spark-sql_2.11", "2.2.1")
    buf += Library("org.apache.kafka", "kafka_2.12", "1.0.0")
    buf += Library("org.apache.flink", "flink-parent", "1.4.1")
    buf += Library("io.grpc", "grpc-all", "1.10.0")
    buf += Library("com.typesafe.play", "play_2.12", "2.6.12")
    buf.result
  }
}

case class Library(name: String, classpath: () => Classpath)
object Library {
  def apply(organization: String, artifact: String, version: String): Library =
    Library(
      List(organization, artifact, version).mkString(":"), { () =>
        val jars = Jars.fetch(organization, artifact, version)
        Classpath(jars)
      }
    )
  lazy val jdk: Library = {
    val bootClasspath = Classpath(
      sys.props
        .collectFirst { case (k, v) if k.endsWith(".boot.class.path") => v }
        .getOrElse("")).entries.filter(_.isFile)
    Library("JDK", () => Classpath(bootClasspath))
  }
  lazy val scalaLibrary: Library = {
    Library("scala-library", () => {
      val version = BuildInfo.scalaVersion
      Classpath(Jars.fetch("org.scala-lang", "scala-library", version))
    })
  }
}

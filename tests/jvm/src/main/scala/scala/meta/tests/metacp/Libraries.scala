package scala.meta.tests.metacp

import scala.meta.io.Classpath
import scala.meta.tests._

object Libraries {
  lazy val suite: List[Library] = {
    val buf = List.newBuilder[Library]
    buf += Library.jdk
    buf += Library.scalaLibrary
    buf += Library(
      "org.scalameta",
      "scalameta_2.12",
      "3.2.0",
      provided = List(
        ModuleID.scalaReflect("2.12.7")
      )
    )
    buf += Library("com.typesafe.akka", "akka-testkit_2.12", "2.5.9")
    buf += Library("com.typesafe.akka", "akka-actor_2.11", "2.5.9")
    buf += Library(
      "org.apache.spark",
      "spark-sql_2.11",
      "2.2.1",
      provided = List(
        ModuleID(
          "org.eclipse.jetty",
          "jetty-servlet",
          "9.3.11.v20160721"
        )
      )
    )
    buf += Library("org.apache.kafka", "kafka_2.12", "1.0.0")
    buf += Library("org.apache.flink", "flink-parent", "1.4.1")
    buf += Library("io.grpc", "grpc-all", "1.10.0")
    buf += Library("io.buoyant", "linkerd-core_2.12", "1.4.3")
    // FIXME: Currently converting this library fails the following:
    //   missing symbol: sourcecode
    //   missing symbol: fastparse
    //   NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. The provided classpath or classpaths should include the Scala library as well as JDK jars such as rt.jar.
    //   [info] - io.get-coursier:coursier-cache_2.12:1.0.3 *** FAILED ***
    //   [info]   None was empty (ConvertSuite.scala:22)
    // buf += Library(
    //   "io.get-coursier",
    //   "coursier-cache_2.12",
    //   "1.0.3",
    //   provided = List(
    //     ModuleID.scalaReflect("2.12.6")
    //   )
    // )
    buf.result
  }
}

case class Library(name: String, classpath: () => Classpath)
object Library {
  def apply(
      organization: String,
      artifact: String,
      version: String,
      provided: List[ModuleID] = Nil
  ): Library =
    Library(
      List(organization, artifact, version).mkString(":"), { () =>
        val jars = Jars.fetch(organization, artifact, version, provided = provided)
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

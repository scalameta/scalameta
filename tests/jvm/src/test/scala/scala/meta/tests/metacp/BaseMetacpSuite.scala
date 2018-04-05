package scala.meta.tests.metacp

import java.nio.file.Files
import org.langmeta.io.AbsolutePath
import org.langmeta.io.Classpath
import scala.meta.cli.Metacp
import scala.meta.metacp._
import scala.meta.tests.cli.BaseCliSuite

abstract class BaseMetacpSuite extends BaseCliSuite {

  val tmp: AbsolutePath = AbsolutePath(Files.createTempDirectory("metacp"))
  tmp.toFile.deleteOnExit()

  def checkMetacp(name: String, classpath: () => Classpath): Unit = {
    test(name) {
      val settings = Settings()
        .withClasspath(classpath())
//        .withCacheDir(tmp)
        .withPar(true)
      val reporter = Reporter().withOut(System.out).withErr(System.err)
      val result = Metacp.process(settings, reporter)
      assert(result.nonEmpty)
    }
  }

  def checkMetacp(library: Library): Unit = {
    checkMetacp(library.name, () => library.classpath())
  }

  val scalameta = Library("org.scalameta", "scalameta_2.12", "3.2.0")
  val akka = Library("com.typesafe.akka", "akka-testkit_2.12", "2.5.9")
  val akkaStream211 = Library("com.typesafe.akka", "akka-stream_2.11", "2.5.9")
  val spark = Library("org.apache.spark", "spark-sql_2.11", "2.2.1")
  val kafka = Library("org.apache.kafka", "kafka_2.12", "1.0.0")
  val flink = Library("org.apache.flink", "flink-parent", "1.4.1")
  val grpc = Library("io.grpc", "grpc-all", "1.10.0")
  val play = Library("com.typesafe.play", "play_2.12", "2.6.12")
  val jdk = {
    val bootClasspath = Classpath(
      sys.props
        .collectFirst { case (k, v) if k.endsWith(".boot.class.path") => v }
        .getOrElse(""))
    Library("JDK", () => bootClasspath)
  }

  val allLibraries = List[Library](
    scalameta,
    akka,
    spark,
    kafka,
    flink,
    grpc,
    play,
    jdk
  )

}

case class Library(name: String, classpath: () => Classpath)
object Library {
  def apply(organization: String, artifact: String, version: String): Library =
    Library(
      List(organization, artifact, version).mkString(":"), { () =>
        val jars = Jars
          .fetch(organization, artifact, version)
          .filterNot(_.toString.contains("scala-lang"))
        Classpath(jars)
      }
    )
}

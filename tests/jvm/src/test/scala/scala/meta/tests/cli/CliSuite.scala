package scala.meta.tests.cli

import java.nio.charset.StandardCharsets._
import java.nio.file._
import org.scalatest.FunSuite
import scala.meta.cli._

class CliSuite extends FunSuite {
  val sourceroot = Files.createTempDirectory("sourceroot_")
  val helloWorldScala = sourceroot.resolve("HelloWorld.scala")
  Files.write(helloWorldScala, """
    object HelloWorld {
      def main(args: Array[String]): Unit = {
        println("hello world")
      }
    }
  """.getBytes(UTF_8))
  val target = Files.createTempDirectory("target_")
  val helloWorldSemanticdb = target.resolve("META-INF/semanticdb/HelloWorld.semanticdb")

  test("metac " + helloWorldScala) {
    val scalaLibraryJar = sys.props("sbt.paths.scalalibrary.classes")
    if (scalaLibraryJar == null) sys.error("sbt.paths.scalalibrary.classes not set. broken build?")
    val success = Metac.process(Array(
      "-cp",
      scalaLibraryJar,
      "-P:semanticdb:sourceroot:" + sourceroot.toString,
      "-d",
      target.toString,
      helloWorldScala.toString))
    assert(success)
    assert(Files.exists(helloWorldSemanticdb))
  }

  test("metap " + helloWorldSemanticdb) {
    val success = Metap.process(Array(helloWorldSemanticdb.toString))
    assert(success)
  }
}
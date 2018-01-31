package scala.meta.tests.metac

import java.nio.charset.StandardCharsets._
import java.nio.file._
import org.scalatest.FunSuite
import scala.meta.cli.Metac

class MetacSuite extends FunSuite {
  test("metac HelloWorld.scala") {
    val classpath = sys.props("sbt.paths.scalalibrary.classes")
    if (classpath == null) sys.error("classpath not set. broken build?")
    val testDir = Files.createTempDirectory("metac_")
    val testFile = testDir.resolve("HelloWorld.scala")
    Files.write(testFile, """
      object HelloWorld {
        def main(args: Array[String]): Unit = {
          println("hello world")
        }
      }
    """.getBytes(UTF_8))
    val success = Metac.process(Array(
      "-cp",
      classpath,
      "-P:semanticdb:sourceroot:" + testDir.toString,
      "-d",
      testDir.toString,
      testFile.toString))
    assert(success)
    assert(Files.exists(testDir.resolve("META-INF/semanticdb/HelloWorld.semanticdb")))
  }
}
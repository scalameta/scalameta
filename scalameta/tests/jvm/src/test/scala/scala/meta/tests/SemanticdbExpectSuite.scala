package scala.meta
package tests

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.meta.testkit.DiffAssertions
import org.scalatest.FunSuite

class SemanticdbExpectSuite extends FunSuite with DiffAssertions {
  test("semanticdb.expect") {
    BuildInfo.scalaVersion.split("\\.").take(2).toList match {
      // both the compiler and stdlib are different between Scala versions.
      // For the sake of simplicity, we only run the expect test against the
      // output of 2.11. It's possible to add another expect file for 2.12
      // later down the road if that turns out to be useful.
      case "2" :: "11" :: Nil =>
        val obtained = SemanticdbExpectSuite.getMirror.toString
        val expected = new String(Files.readAllBytes(SemanticdbExpectSuite.expectPath))
        assertNoDiff(obtained, expected)
      case _ => // do nothing.
    }
  }
}

object SemanticdbExpectSuite {
  val expectPath: Path =
    Paths.get("scalameta", "tests", "jvm", "src", "test", "resources", "semanticdb.expect")
  def getMirror: Mirror = {
    val mirror = Database.load(Classpath(BuildInfo.mirrorClasspath))
    val sorted = Database(mirror.entries.sortBy(_.input.syntax))
    sorted
  }
}

// To save the current behavior, run:
// testsJVM/test:runMain scala.meta.tests.SaveSemanticdbExpectTest
object SaveSemanticdbExpectTest {
  def main(args: Array[String]): Unit = {
    val mirror = SemanticdbExpectSuite.getMirror
    Files.write(SemanticdbExpectSuite.expectPath, mirror.toString.getBytes("UTF-8"))
  }
}

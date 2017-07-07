package scala.meta
package tests

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.meta.testkit.DiffAssertions
import org.scalatest.FunSuite

class SemanticdbExpectSuite extends FunSuite with DiffAssertions {
  test("semanticdb.expect") {
    val obtained = SemanticdbExpectSuite.getMirror.toString
    val expected = new String(Files.readAllBytes(SemanticdbExpectSuite.expectPath))
    assertNoDiff(obtained, expected)
  }
}

object SemanticdbExpectSuite {
  val expectPath: Path =
    Paths.get("scalameta", "tests", "jvm", "src", "test", "resources", "semanticdb.expect")
  def getMirror: Mirror = {
    val mirror = Database.load(Classpath(BuildInfo.mirrorClasspath))
    val normalizedMirror = Database(mirror.entries.map { attrs =>
      attrs.copy(dialect = dialects.Scala212)
    })
    normalizedMirror
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

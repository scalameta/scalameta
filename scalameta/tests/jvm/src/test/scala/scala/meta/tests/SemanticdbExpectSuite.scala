package scala.meta
package tests

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.meta.testkit.DiffAssertions
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.InputStreamIO
import scala.meta.internal.io.PathIO
import org.scalatest.FunSuite

class SemanticdbExpectSuite extends FunSuite with DiffAssertions {
  test("semanticdb.expect") {
    val mirror = Database.load(Classpath(BuildInfo.mirrorClasspath))
    val obtained = mirror.toString
    val expected = new String(Files.readAllBytes(SemanticdbExpectSuite.expectPath))
    assertNoDiff(obtained, expected)
  }
}

object SemanticdbExpectSuite {
  val expectPath: Path =
    Paths.get("scalameta", "tests", "jvm", "src", "test", "resources", "semanticdb.expect")
}

// To save the current behavior, run:
// testsJVM/test:runMain scala.meta.tests.SaveSemanticdbExpectTest
object SaveSemanticdbExpectTest {
  def main(args: Array[String]): Unit = {
    val mirror = Database.load(Classpath(BuildInfo.mirrorClasspath))
    Files.write(SemanticdbExpectSuite.expectPath, mirror.toString.getBytes("UTF-8"))
  }
}

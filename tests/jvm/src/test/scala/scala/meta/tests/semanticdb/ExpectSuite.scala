package scala.meta.tests
package semanticdb

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.charset.Charset
import scala.meta.testkit.DiffAssertions
import scala.meta.internal.io.FileIO
import org.scalatest.FunSuite
import scala.meta._
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb._
import org.langmeta.io.AbsolutePath

class ExpectSuite extends FunSuite with DiffAssertions {
  test("semanticdb.expect") {
    BuildInfo.scalaVersion.split("\\.").take(2).toList match {
      // both the compiler and stdlib are different between Scala versions.
      // For the sake of simplicity, we only run the expect test against the
      // output of 2.12. It's possible to add another expect file for 2.12
      // later down the road if that turns out to be useful.
      case "2" :: "12" :: Nil =>
        val sourceroot = PathIO.workingDirectory
        val database = SemanticdbExpectSuite.loadDatabase
        val obtained = database.toString
        val expected = FileIO.slurp(AbsolutePath(SemanticdbExpectSuite.expectPath), Charset.forName("UTF-8"))
        assertNoDiff(obtained, expected)
        val roundtrip = database.toSchema(sourceroot).toDb(Some(Sourcepath(sourceroot)))
        assertNoDiff(roundtrip.toString, expected, "Roundtrip")
      case _ => // do nothing.
    }
  }
}

object SemanticdbExpectSuite {
  val expectPath: Path =
    Paths.get("tests", "jvm", "src", "test", "resources", "semanticdb.expect")
  def loadDatabase: Database = {
    val database = Database.load(Classpath(BuildInfo.databaseClasspath))
    val sorted = Database(database.documents.sortBy(_.input.syntax))
    sorted
  }
}

// To save the current behavior, run:
// testsJVM/test:runMain scala.meta.tests.semanticdb.SaveSemanticdbExpectTest
object SaveSemanticdbExpectTest {
  def main(args: Array[String]): Unit = {
    val database = SemanticdbExpectSuite.loadDatabase
    Files.write(SemanticdbExpectSuite.expectPath, database.toString.getBytes("UTF-8"))
  }
}

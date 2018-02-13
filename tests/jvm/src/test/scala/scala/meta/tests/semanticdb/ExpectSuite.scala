package scala.meta.tests
package semanticdb

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets._
import scala.collection.JavaConverters._
import scala.meta.testkit.DiffAssertions
import scala.meta.internal.io.FileIO
import scala.meta._
import scala.meta.internal.metacp.{Main => Metacp, Settings => MetacpSettings}
import scala.meta.internal.metap.{Main => Metap}
import scala.meta.tests.cli._
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb._
import org.langmeta.io.AbsolutePath
import org.scalatest.FunSuite

class ExpectSuite extends FunSuite with DiffAssertions {
  BuildInfo.scalaVersion.split("\\.").take(2).toList match {
    // both the compiler and stdlib are different between Scala versions.
    // For the sake of simplicity, we only run the expect test against the
    // output of 2.12. It's possible to add another expect file for 2.12
    // later down the road if that turns out to be useful.
    case "2" :: "12" :: Nil =>
      test("metacp.expect") {
        import MetacpExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("lowlevel.expect") {
        import LowlevelExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("highlevel.expect") {
        import HighlevelExpect._
        assertNoDiff(loadObtained, loadExpected)
        val sourceroot = PathIO.workingDirectory
        val roundtrip = loadDatabase.toSchema(sourceroot).toDb(Some(Sourcepath(sourceroot)))
        assertNoDiff(roundtrip.toString, loadExpected, "Roundtrip")
      }
    case _ =>
      ()
  }
}

trait ExpectHelpers {
  def filename: String
  def path: Path =
    Paths.get("tests", "jvm", "src", "test", "resources", filename)
  def loadExpected: String =
    new String(Files.readAllBytes(path), UTF_8)
  def saveExpected(value: String): Unit =
    Files.write(path, value.getBytes(UTF_8))
}

object MetacpExpect extends ExpectHelpers {
  def filename: String = "metacp.expect"

  def loadObtained: String = {
    val classpath = BuildInfo.databaseClasspath
    val target = Files.createTempDirectory("target_")
    val metacp_settings = MetacpSettings.parse(List("-d", target.toString, classpath)).get
    val (metacp_exitcode, _) = CliSuite.communicate(Metacp.process(metacp_settings))
    assert(metacp_exitcode == 0)
    val paths = Files.walk(target).iterator.asScala
    val semanticdbs = paths.filter(_.toFile.isFile).map(_.toString).toArray.sorted
    val (metap_exitcode, metap_stdout) = CliSuite.communicate(Metap.process(semanticdbs))
    assert(metap_exitcode == 0)
    metap_stdout
  }
}

object LowlevelExpect extends ExpectHelpers {
  def filename: String = "lowlevel.expect"

  def loadObtained: String = {
    val paths = Files.walk(Paths.get(BuildInfo.databaseClasspath)).iterator.asScala
    val semanticdbs = paths.map(_.toString).filter(_.endsWith(".semanticdb")).toArray.sorted
    val (exitcode, stdout) = CliSuite.communicate(Metap.process(semanticdbs))
    assert(exitcode == 0)
    stdout
  }
}

object HighlevelExpect extends ExpectHelpers {
  def filename: String = "highlevel.expect"

  def loadObtained: String = loadDatabase.syntax

  def loadDatabase: Database = {
    val database = Database.load(Classpath(BuildInfo.databaseClasspath))
    val sorted = Database(database.documents.sortBy(_.input.syntax))
    sorted
  }
}

// To save the current behavior, run:
// testsJVM/test:runMain scala.meta.tests.semanticdb.SaveExpectTest
object SaveExpectTest {
  def main(args: Array[String]): Unit = {
    LowlevelExpect.saveExpected(LowlevelExpect.loadObtained)
    HighlevelExpect.saveExpected(HighlevelExpect.loadObtained)
    MetacpExpect.saveExpected(MetacpExpect.loadObtained)
  }
}

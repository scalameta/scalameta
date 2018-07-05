package scala.meta.tests.semanticdb

import java.nio.file.Files
import org.scalactic.source.Position
import org.scalatest.FunSuite
import org.scalatest.tagobjects.Slow
import org.scalatest.BeforeAndAfterEach
import scala.meta.internal.io.FileIO
import scala.meta.internal.semanticdb.scalac.SemanticdbPaths
import scala.meta.io.RelativePath
import scala.meta.testkit.DiffAssertions

class IncrementalSuite extends FunSuite with BeforeAndAfterEach with DiffAssertions {

  var zinc: ZincProject = _

  override def beforeEach(): Unit = {
    zinc = new ZincProject()
  }

  def assertIndexMatches(expected: String)(implicit source: Position): Unit = {
    val path = zinc.targetroot.resolve("META-INF/semanticdb/semanticdb.index")
    val index = FileIO.readIndex(path)
    val obtained = MetacpIndexExpect.printIndex(index)
    assertNoDiff(obtained, expected)
  }

  val A: String =
    """|_empty_/A. => src/A.scala.semanticdb
       |""".stripMargin

  val AB: String =
    """|_empty_/A. => src/A.scala.semanticdb
       |_empty_/B. => src/B.scala.semanticdb
       |""".stripMargin

  val ABC: String =
    """|_empty_/A. => src/A.scala.semanticdb
       |_empty_/B. => src/B.scala.semanticdb
       |_empty_/C. => src/A.scala.semanticdb
       |""".stripMargin

  test("update file", Slow) {
    zinc.assertCompiles(
      """|/src/A.scala
         |object A
         |/src/B.scala
         |object B
         |""".stripMargin
    )
    assertIndexMatches(AB)
    zinc.assertCompiles(
      """|/src/A.scala
         |object A
         |object C
         |/src/B.scala
         |object B
         |""".stripMargin
    )
    assertIndexMatches(ABC)
  }

  test("add new file", Slow) {
    zinc.assertCompiles(
      """|/src/A.scala
         |object A
         |""".stripMargin
    )
    assertIndexMatches(A)
    zinc.assertCompiles(
      """|/src/B.scala
         |object B
         |""".stripMargin
    )
    assertIndexMatches(AB)
  }

  test("delete file", Slow) {
    val B = RelativePath("src/B.scala")
    val Bsemanticdb = SemanticdbPaths.toSemanticdb(B, zinc.targetroot)
    zinc.assertCompiles(
      """|/src/A.scala
         |object A
         |/src/B.scala
         |object B
         |""".stripMargin
    )
    Files.delete(zinc.sourceroot.resolve(B).toNIO)
    assertIndexMatches(AB)
    assert(Bsemanticdb.isFile)
    zinc.assertCompiles("")
    assert(!Bsemanticdb.isFile, "orphan SemanticDB was not removed")
    assertIndexMatches(A)
  }

  test("java only", Slow) {
    zinc.assertCompiles(
      """|/src/A.scala
         |object A
         |/src/B.java
         |public class B {}
         |""".stripMargin
    )
    assertIndexMatches(
      """|_empty_/A. => src/A.scala.semanticdb
         |_empty_/B# => src/B.java.semanticdb
         |""".stripMargin
    )
    Files.delete(zinc.sourceroot.resolve("src/B.java").toNIO)
    zinc.assertCompiles("")
    assertIndexMatches(A)
  }

}

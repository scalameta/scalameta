package scala.meta.tests.semanticdb

import java.nio.file.Files
import org.scalatest.FunSuite
import scala.meta.cli.Metac
import scala.meta.internal.semanticdb.scalac.SemanticdbPaths
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath
import scala.meta.io.RelativePath
import scala.meta.metac
import scala.meta.testkit.DiffAssertions
import scala.meta.testkit.StringFS
import scala.meta.tests.cli.CliSuite
import scala.meta.tests.metacp.Library

class ConfigSuite extends FunSuite with DiffAssertions {

  val A = RelativePath("A.scala")

  def generateTargetroot(): AbsolutePath = {
    val target = AbsolutePath(Files.createTempDirectory("semanticdb"))
    target.toFile.deleteOnExit()
    target
  }

  def check(
      name: String,
      scalacArgs: List[String],
      input: String,
      fn: s.TextDocument => Unit,
      targetroot: AbsolutePath = generateTargetroot()
  ): Unit = {
    test(name) {
      val sourceroot = StringFS.fromString(input)
      val (metacIsSuccess, metacOut, _) = CliSuite.withReporter { reporter =>
        val settings = metac
          .Settings()
          .withScalacArgs(
            scalacArgs ++ List(
              "-d",
              targetroot.toString,
              "-P:semanticdb:sourceroot:" + sourceroot.toString,
              "-cp",
              Library.scalaLibrary.classpath().syntax,
              sourceroot.resolve(A).toString
            )
          )
        Metac.process(settings, reporter)
      }
      assert(metacIsSuccess, metacOut)
      assert(metacOut.isEmpty)
      val semanticdbPath = SemanticdbPaths.toSemanticdb(A, targetroot)
      val docs = s.TextDocuments.parseFrom(semanticdbPath.readAllBytes)
      assert(docs.documents.length == 1, docs.toProtoString)
      fn(docs.documents.head)
    }
  }

  check(
    "symbols:local",
    List("-P:semanticdb:symbols:locals"),
    """
      |/A.scala
      |object A {
      |  val x = {
      |    val y = 1 // local0
      |    val z = 2 // local1
      |    y + z
      |  }
      |}
    """.stripMargin, { doc =>
      val symbols = doc.symbols.map(_.symbol).sorted.mkString("\n")
      assertNoDiffOrPrintExpected(
        symbols,
        """|local0
           |local1
           |""".stripMargin
      )
    }
  )

  private val customTargetroot = generateTargetroot()
  check(
    "targetroot:<custom>",
    List(s"-P:semanticdb:targetroot:$customTargetroot"),
    """|/A.scala
       |object A
       |""".stripMargin, { doc =>
      val Seq(occurrence) = doc.occurrences
      assert(occurrence.symbol == "_empty_/A.")
    },
    targetroot = customTargetroot
  )

  check(
    "md5:off",
    List(s"-P:semanticdb:md5:off"),
    """|/A.scala
       |object A
    """.stripMargin, { doc =>
      assert(doc.md5.isEmpty)
    }
  )

  check(
    "synthetics:off",
    List(s"-P:semanticdb:synthetics:off"),
    """|/A.scala
       |object A {
       |   List(1).map(_ + 1)
       |}
       |""".stripMargin, { doc =>
      assert(doc.synthetics.isEmpty)
    }
  )

  check(
    "synthetics:on",
    List(s"-P:semanticdb:synthetics:on"),
    """|/A.scala
       |object A {
       |   List(1).map(_ + 1)
       |}
       |""".stripMargin, { doc =>
      assert(doc.synthetics.nonEmpty)
    }
  )

}

package scala.meta.tests.semanticdb

import java.nio.file.Files
import org.scalatest.FunSuite
import scala.meta.cli.Metac
import scala.meta.internal.io.FileIO
import scala.meta.internal.semanticdb.Locator
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

  val sourceroot = StringFS.fromString(
    """|/HelloWorld.scala
       |package a
       |object Hello {
       |  val x = {
       |    val y = 1
       |    val z = 2
       |    y + z
       |  }
       |  def xx = x * 2
       |}
       |""".stripMargin
  )

  val helloWorld = RelativePath("HelloWorld.scala")

  def check(scalacArgs: List[String], fn: s.TextDocument => Unit): Unit = {
    test(scalacArgs.mkString(" ")) {
      val target = AbsolutePath(Files.createTempDirectory("semanticdb"))
      target.toFile.deleteOnExit()
      val (metacIsSuccess, metacOut, _) = CliSuite.withReporter { reporter =>
        val settings = metac
          .Settings()
          .withScalacArgs(
            scalacArgs ++ List(
              "-d",
              target.toString,
              "-P:semanticdb:sourceroot:" + sourceroot.toString,
              "-cp",
              Library.scalaLibrary.classpath().syntax,
              sourceroot.resolve(helloWorld).toString
            )
          )
        Metac.process(settings, reporter)
      }
      assert(metacIsSuccess, metacOut)
      assert(metacOut.isEmpty)
      val semanticdbPath = SemanticdbPaths.toSemanticdb(helloWorld, target)
      val docs = s.TextDocuments.parseFrom(semanticdbPath.readAllBytes)
      assert(docs.documents.length == 1, docs.toProtoString)
      fn(docs.documents.head)
    }
  }

  check(
    List("-P:semanticdb:symbols:locals"), { doc =>
      val symbols = doc.symbols.map(_.symbol).sorted.mkString("\n")
      assertNoDiffOrPrintExpected(
        symbols,
        """|local0
           |local1
           |""".stripMargin
      )
    }
  )

}

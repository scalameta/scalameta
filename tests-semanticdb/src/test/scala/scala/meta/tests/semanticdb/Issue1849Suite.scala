package scala.meta.tests.semanticdb

import scala.meta.cli.Metac
import scala.meta.internal.semanticdb.scalac.SemanticdbPaths
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath
import scala.meta.metac
import scala.meta.testkit.StringFS
import scala.meta.tests.cli.CliTestUtils
import scala.meta.tests.metacp.Library

import java.nio.file.Files

import scala.collection.JavaConverters._

import munit.FunSuite

// https://github.com/scalameta/scalameta/issues/1849
class Issue1849Suite extends FunSuite {

  test("source file names with a non-ASCII letter and a space") {
    val sourceroot = StringFS.fromString(
      """|/å.scala
         |object A
         |/b c.scala
         |object B
         |/c.scala
         |object C
         |""".stripMargin,
    )
    val targetroot = AbsolutePath(Files.createTempDirectory("semanticdb"))
    val sources = List("å.scala", "b c.scala", "c.scala").map(sourceroot.resolve(_).toString)
    val (metacIsSuccess, _, metacErr) = CliTestUtils.withReporter { reporter =>
      val settings = metac.Settings().withScalacArgs(
        List(
          "-d",
          targetroot.toString,
          "-P:semanticdb:sourceroot:" + sourceroot.toString,
          "-cp",
          Library.scalaLibrary.classpath().syntax,
        ) ++ sources,
      )
      Metac.process(settings, reporter)
    }
    assert(metacIsSuccess, metacErr)
    val semanticdbroot = targetroot.resolve(SemanticdbPaths.semanticdbPrefix)
    val uris = Files.list(semanticdbroot.toNIO).iterator().asScala.toList.flatMap(file =>
      s.TextDocuments.parseFrom(Files.readAllBytes(file)).documents
        .map(doc => file.getFileName.toString + " " + doc.uri),
    )
    assertEquals(
      uris.sorted,
      List(
        "b c.scala.semanticdb b%20c.scala",
        "c.scala.semanticdb c.scala",
        "å.scala.semanticdb å.scala",
      ),
    )
  }

}

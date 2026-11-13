package scala.meta.internal.symtab

import scala.meta.cli.Reporter
import scala.meta.internal.classpath.ClasspathIndex
import scala.meta.internal.io.{FileIO, PathIO}
import scala.meta.internal.metacp._
import scala.meta.internal.metap.DocumentPrinter
import scala.meta.internal.scalacp.Scalalib
import scala.meta.internal.semanticdb.Locator
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.metap.Format
import scala.meta.tests.BuildInfo
import scala.meta.tests.metacp.Library

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import munit.FunSuite

/**
 * Compares the symbols that the Scala 3 symbol table reads with the expect files of the latest 2.13
 * row. The suite converts and prints as metacp and metap do, since neither is built for Scala 3.
 */
class ExpectSuite extends FunSuite {

  private def expected(name: String): String = new String(
    Files.readAllBytes(Paths.get(BuildInfo.resourcesDirectory).resolve(name + "_2.13")),
    UTF_8,
  )

  private def newOutput(prefix: String) = AbsolutePath(Files.createTempDirectory(prefix))

  private def metap(root: Path): String = {
    val bytes = new ByteArrayOutputStream
    val out = new PrintStream(bytes, true, UTF_8.name)
    val reporter = Reporter().withOut(out)
    val settings = scala.meta.metap.Settings().withFormat(Format.Detailed)
    var first = true
    Locator(root) { (_, payload) =>
      if (first) first = false else out.println("")
      payload().documents.foreach(new DocumentPrinter(settings, reporter, _).print())
    }
    new String(bytes.toByteArray, UTF_8)
  }

  test("metacp.expect") {
    val classDirectory = AbsolutePath(BuildInfo.databaseClasspath)
    val classpath = Classpath(classDirectory) ++ Library.scalaLibrary.classpath()
    val reporter = Reporter()
    val index = ClasspathIndex(classpath, includeJdk = true, reporter)
    val settings = scala.meta.metacp.Settings()
    val out = newOutput("metacp_")
    FileIO.listAllFilesRecursively(classDirectory).foreach(file =>
      if (PathIO.extension(file.toNIO) == "class" && Files.size(file.toNIO) > 0) ClassfileInfos
        .fromClassNode(file.toClassNode, index, settings, reporter).foreach(_.save(out)),
    )
    assertNoDiff(metap(out.toNIO), expected("metacp.expect"))
  }

  test("scalalib.expect") {
    val out = newOutput("scalalib_")
    Scalalib.synthetics(Scalalib.isHashHashNullary(ClasspathIndex(Classpath(Nil))))
      .foreach(_.save(out))
    assertNoDiff(metap(out.toNIO), expected("scalalib.expect"))
  }

}

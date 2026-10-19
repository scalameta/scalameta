package scala.meta.tests.semanticdb

import scala.meta.cli.Metacp
import scala.meta.internal.classpath.ClasspathUtils
import scala.meta.internal.io.{FileIO, PathIO}
import scala.meta.internal.semanticdb.scalac.SemanticdbPlugin
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.testkit.StringFS
import scala.meta.tests.cli.CliTestUtils
import scala.meta.tests.metacp.Library

import java.io.File
import java.nio.file.Files

import scala.tools.nsc.{Main => ScalacMain}

import munit.Assertions

// metac reads the semanticdb files of a compiled snippet, metacp its classfiles
trait MetacpSnippets {
  self: Assertions =>

  def symtab(root: AbsolutePath): Map[String, s.SymbolInformation] = FileIO
    .listAllFilesRecursively(root).iterator
    .filter(file => PathIO.extension(file.toNIO) == "semanticdb")
    .flatMap(file => s.TextDocuments.parseFrom(file.readAllBytes).documents).flatMap(_.symbols)
    .map(info => info.symbol -> info).toMap

  def compile(source: String)(implicit loc: munit.Location): AbsolutePath = {
    val sourceroot = StringFS.fromString("/a.scala\n" + source)
    val classes = AbsolutePath(Files.createTempDirectory("semanticdb"))
    val plugin = ClasspathUtils.getClassPathEntries(classOf[SemanticdbPlugin].getClassLoader)
      .mkString(File.pathSeparator)
    val (_, _, compileErr) = CliTestUtils.communicate((out, err) =>
      Console.withOut(out)(Console.withErr(err)(ScalacMain.process(Array(
        "-d",
        classes.toString,
        "-Xplugin:" + plugin,
        "-Xplugin-require:semanticdb",
        "-Yrangepos",
        "-P:semanticdb:sourceroot:" + sourceroot.toString,
        "-cp",
        Library.scalaLibrary.classpath().syntax,
        sourceroot.resolve("a.scala").toString,
      )))),
    )
    assert(!ScalacMain.reporter.hasErrors, compileErr)
    classes
  }

  def convert(classes: AbsolutePath)(implicit loc: munit.Location): AbsolutePath = {
    val out = AbsolutePath(Files.createTempDirectory("metacp"))
    val (converted, _, convertErr) = CliTestUtils.withReporter(reporter =>
      Metacp.process(
        scala.meta.metacp.Settings().withOut(out).withClasspath(Classpath(classes))
          .withUsejavacp(true).withScalaLibrarySynthetics(false),
        reporter,
      ),
    )
    assert(converted.classpath.nonEmpty, convertErr)
    out
  }

}

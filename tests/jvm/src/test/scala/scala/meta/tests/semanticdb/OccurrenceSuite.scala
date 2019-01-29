package scala.meta.tests.semanticdb

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import org.scalatest.FunSuite
import scala.meta.inputs.Input
import scala.meta.inputs.Position
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.semanticdb.scalac.SemanticdbPaths
import scala.meta.io.AbsolutePath
import scala.meta.testkit.DiffAssertions
import scala.meta.tests.BuildInfo

class OccurrenceSuite extends FunSuite with DiffAssertions {
  OccurrenceSuite.testCases().foreach { t =>
    test(t.name) {
      val body = t.body()
      assertNoDiff(body.obtained, body.expected)
    }
  }
}

object OccurrenceSuite {
  def isExcluded(path: AbsolutePath): Boolean =
    path.toNIO.endsWith("Exclude.scala")
  val expectroot = AbsolutePath(Paths.get("tests", "jvm", "src", "test", "resources"))
  case class TestBody(
      obtained: String,
      expected: String,
      expectpath: AbsolutePath
  )
  case class TestCase(
      name: String,
      body: () => TestBody
  )
  def testCases(): Seq[TestCase] = {
    for {
      dir <- BuildInfo.integrationSourceDirectories
      absdir = AbsolutePath(dir)
      if absdir.isDirectory
      source <- FileIO.listAllFilesRecursively(absdir)
      if PathIO.extension(source.toNIO) == "scala"
      if !isExcluded(source)
    } yield {
      val relpath = source.toRelative(absdir)
      val testname = relpath.toURI(false).toString
      TestCase(
        testname,
        () => {
          val sourceroot = AbsolutePath(BuildInfo.databaseSourcepath)
          val targetroot = AbsolutePath(BuildInfo.databaseClasspath)
          val semanticdb = SemanticdbPaths.toSemanticdb(source.toRelative(sourceroot), targetroot)
          val textdocument = TextDocuments.parseFrom(semanticdb.readAllBytes).documents.head
          val expectpath = expectroot.resolve(relpath)
          val obtained = OccurrenceSuite.printTextDocument(textdocument)
          val expected =
            if (expectpath.isFile) {
              FileIO.slurp(expectpath, StandardCharsets.UTF_8)
            } else {
              "// missing expect file\n"
            }
          TestBody(
            obtained,
            expected,
            expectpath
          )
        }
      )
    }
  }
  def saveExpected(): Unit = {
    testCases().foreach { t =>
      val body = t.body()
      Files.createDirectories(body.expectpath.toNIO.getParent)
      Files.write(body.expectpath.toNIO, body.obtained.getBytes(StandardCharsets.UTF_8))
    }
  }
  def printTextDocument(doc: TextDocument): String = {
    val symtab = doc.symbols.iterator.map(info => info.symbol -> info).toMap
    val sb = new StringBuilder
    val occurrences = doc.occurrences.sorted
    val input = Input.String(doc.text)
    var offset = 0
    occurrences.foreach { occ =>
      val range = occ.range.get
      val pos = Position.Range(
        input,
        range.startLine,
        range.startCharacter,
        range.endLine,
        range.endCharacter
      )
      sb.append(doc.text.substring(offset, pos.end))
      val isPrimaryConstructor =
        symtab.get(occ.symbol).exists(_.isPrimary)
      if (!occ.symbol.isPackage && !isPrimaryConstructor) {
        printSymbol(sb, occ.symbol)
      }
      offset = pos.end
    }
    sb.append(doc.text.substring(offset))
    sb.toString()
  }

  def printSymbol(sb: StringBuilder, symbol: String): Unit = {
    sb.append("/*")
      // replace package / with dot . to not upset GitHub syntax highlighting.
      .append(symbol.replace('/', '.'))
      .append("*/")
  }

  implicit val occurrenceOrdering: Ordering[SymbolOccurrence] =
    new Ordering[SymbolOccurrence] {
      override def compare(x: SymbolOccurrence, y: SymbolOccurrence): Int = {
        if (x.range.isEmpty) 0
        else if (y.range.isEmpty) 0
        else {
          val a = x.range.get
          val b = y.range.get
          val byLine = Integer.compare(
            a.startLine,
            b.startLine
          )
          if (byLine != 0) {
            byLine
          } else {
            val byCharacter = Integer.compare(
              a.startCharacter,
              b.startCharacter
            )
            byCharacter
          }
        }
      }
    }
}

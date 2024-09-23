package scala.meta.tests.semanticdb

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
import scala.meta.tests.BuildInfo
import scala.meta.tests.Utils

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import munit.FunSuite

class OccurrenceSuite extends FunSuite {

  private def testBody(body: OccurrenceSuite.TestBody): Unit = {
    val expectedCompat =
      if (ScalaVersion.atLeast213_15) body.expected
      else body.expected.replace("  } yield a/*=>local13*/", "  } yield a/*=>local11*/")
    assertNoDiff(body.obtained, expectedCompat)
  }

  ScalaVersion.doIf("OccurrenceSuite", ScalaVersion.is212 || ScalaVersion.is213) {
    OccurrenceSuite.testCases.foreach(t => test(t.name)(t.body.fold(fail(_), testBody)))
  }
}

object OccurrenceSuite {
  def isExcluded(path: AbsolutePath): Boolean = path.toNIO.endsWith("Exclude.scala")
  case class TestBody(obtained: String, expected: String, expectpath: AbsolutePath)
  class TestCase(val name: String)(f: => Either[String, TestBody]) {
    lazy val body: Either[String, TestBody] = f
  }
  lazy val testCases: collection.Seq[TestCase] = {
    val sourceroot = AbsolutePath(BuildInfo.databaseSourcepath)
    val targetroot = AbsolutePath(BuildInfo.databaseClasspath)
    for {
      dir <- BuildInfo.integrationSourceDirectories
      absdir = AbsolutePath(dir) if absdir.isDirectory
      source <- FileIO.listAllFilesRecursively(absdir) if PathIO.extension(source.toNIO) == "scala"
      if !isExcluded(source)
      relpath = source.toRelative(absdir)
    } yield {
      val relpathstr = relpath.toString
      val relpathnoext = relpathstr.stripSuffix(".scala")
      def forVer(version: String) = relpathnoext + "_" + version + ".scala"
      val expectpathOpt = Utils.getFirstAbsResourceOpt(
        forVer(BuildInfo.scalaVersion),
        forVer(BuildInfo.scalaBinaryVersion),
        relpathstr
      )
      new TestCase(relpath.toURI(false).toString)(
        expectpathOpt.toRight(s"// missing expect file: $relpathstr\n").right.map { expectpath =>
          val semanticdb = SemanticdbPaths.toSemanticdb(source.toRelative(sourceroot), targetroot)
          val textdocument = TextDocuments.parseFrom(semanticdb.readAllBytes).documents.head
          val obtained = OccurrenceSuite.printTextDocument(textdocument)
          val expected = FileIO.slurp(expectpath, StandardCharsets.UTF_8)
          TestBody(obtained, expected, expectpath)
        }
      )
    }
  }
  def saveExpected(): Unit = testCases.foreach(_.body.right.foreach { body =>
    Files.createDirectories(body.expectpath.toNIO.getParent)
    Files.write(body.expectpath.toNIO, body.obtained.getBytes(StandardCharsets.UTF_8))
  })
  def printTextDocument(doc: TextDocument): String = {
    val symtab = doc.symbols.iterator.map(info => info.symbol -> info).toMap
    val sb = new StringBuilder
    val occurrences = doc.occurrences.sorted
    val input = Input.VirtualFile(doc.uri, doc.text)
    var offset = 0
    occurrences.foreach { occ =>
      val range = occ.range.get
      val pos = Position
        .Range(input, range.startLine, range.startCharacter, range.endLine, range.endCharacter)
      if (pos.end >= offset) {
        sb.append(doc.text.substring(offset, pos.end))
        val isPrimaryConstructor = symtab.get(occ.symbol).exists(_.isPrimary)
        if (!occ.symbol.isPackage && !isPrimaryConstructor) printSymbol(sb, occ.symbol, occ.role)
        offset = pos.end
      }
    }
    sb.append(doc.text.substring(offset))
    sb.toString()
  }

  def printSymbol(sb: StringBuilder, symbol: String, role: SymbolOccurrence.Role): Unit = {
    val arrow = if (role.isDefinition) "<=" else "=>"
    sb.append("/*").append(arrow)
      // replace package / with dot . to not upset GitHub syntax highlighting.
      .append(symbol.replace('/', '.')).append("*/")
  }

  implicit val occurrenceOrdering: Ordering[SymbolOccurrence] = new Ordering[SymbolOccurrence] {
    override def compare(x: SymbolOccurrence, y: SymbolOccurrence): Int =
      if (x.range.isEmpty) 0
      else if (y.range.isEmpty) 0
      else {
        val a = x.range.get
        val b = y.range.get
        val byLine = Integer.compare(a.startLine, b.startLine)
        if (byLine != 0) byLine
        else {
          val byCharacter = Integer.compare(a.startCharacter, b.startCharacter)
          byCharacter
        }
      }
  }
}

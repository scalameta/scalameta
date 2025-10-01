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

  private def testBody(body: OccurrenceSuite.TestBody): Unit =
    assertNoDiff(body.obtained, body.expected)

  ScalaVersion.doIf("OccurrenceSuite", ScalaVersion.is212 || ScalaVersion.is213)(
    OccurrenceSuite.testCases.foreach(t => test(t.name)(t.body.fold(fail(_), testBody)))
  )
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
          val expectedPrevious213 =
            if (expectpath.syntax.endsWith("ValPattern.scala")) expectedPrevious213ValPattern
            else expected
          val expectedCompat = ScalaVersion.getExpected(
            compat = List(
              ScalaVersion.Full("2.13.16") -> expectedPrevious213,
              ScalaVersion.Full("2.13.15") -> expectedPrevious213,
              ScalaVersion.Full("2.13.14") -> expectedPrevious213,
              ScalaVersion.Scala212 -> expectedPrevious213,
              ScalaVersion.Scala211 -> expectedPrevious213
            ),
            expected
          )
          TestBody(obtained, expectedCompat, expectpath)
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
    val input = Input.VirtualFile(doc.uri, doc.text)
    var offset = 0
    doc.occurrences.foreach { occ =>
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

  private val expectedPrevious213ValPattern =
    """|package example
       |
       |class ValPattern/*<=example.ValPattern#*/ {
       |
       |  val (left/*<=example.ValPattern#left.*/, right/*<=example.ValPattern#right.*/) = (1, 2)
       |  val Some/*=>scala.Some.*/(number1/*<=example.ValPattern#number1.*/) =
       |    Some/*=>scala.Some.*/(1)
       |
       |  var (leftVar/*<=example.ValPattern#leftVar().*/, rightVar/*<=example.ValPattern#rightVar().*/) = (1, 2)
       |  var Some/*=>scala.Some.*/(number1Var/*<=example.ValPattern#number1Var().*/) =
       |    Some/*=>scala.Some.*/(1)
       |
       |  def app/*<=example.ValPattern#app().*/(): Unit/*=>scala.Unit#*/ = {
       |    println/*=>scala.Predef.println(+1).*/(
       |      (
       |        number1/*=>example.ValPattern#number1.*/,
       |        left/*=>example.ValPattern#left.*/,
       |        right/*=>example.ValPattern#right.*/,
       |        number1Var/*=>example.ValPattern#number1Var().*/,
       |        leftVar/*=>example.ValPattern#leftVar().*/,
       |        rightVar/*=>example.ValPattern#rightVar().*/
       |      )
       |    )
       |    locally/*=>scala.Predef.locally().*/ {
       |      val (left/*<=local6*/, right/*<=local7*/) = (1, 2)
       |      val Some/*=>scala.Some.*/(number1/*<=local10*/) =
       |        Some/*=>scala.Some.*/(1)
       |
       |      var (leftVar/*<=local11*/, rightVar/*<=local12*/) = (1, 2)
       |      var Some/*=>scala.Some.*/(number1Var/*<=local15*/) =
       |        Some/*=>scala.Some.*/(1)
       |      println/*=>scala.Predef.println(+1).*/(
       |        (
       |          number1/*=>local10*/,
       |          left/*=>local6*/,
       |          right/*=>local7*/,
       |          number1Var/*=>local15*/,
       |          leftVar/*=>local11*/,
       |          rightVar/*=>local12*/
       |        )
       |      )
       |    }
       |  }
       |
       |}
       |
       |""".stripMargin

}

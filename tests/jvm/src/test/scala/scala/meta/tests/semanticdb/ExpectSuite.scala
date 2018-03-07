package scala.meta.tests
package semanticdb

import scala.meta.internal.{semanticdb3 => s}
import java.nio.file._
import java.nio.charset.StandardCharsets._
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.meta.testkit.DiffAssertions
import scala.meta.internal.io.FileIO
import scala.meta._
import scala.meta.cli._
import scala.meta.tests.cli._
import scala.compat.Platform.EOL
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb._
import org.langmeta.io.AbsolutePath
import org.scalatest.FunSuite
import org.scalatest.FunSuiteLike

class ExpectSuite extends FunSuite with DiffAssertions {
  BuildInfo.scalaVersion.split("\\.").take(2).toList match {
    // both the compiler and stdlib are different between Scala versions.
    // For the sake of simplicity, we only run the expect test against the
    // output of 2.12. It's possible to add another expect file for 2.12
    // later down the road if that turns out to be useful.
    case "2" :: "12" :: Nil =>
      test("scalalib.expect") {
        import ScalalibExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("metacp.expect") {
        import MetacpExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("metacp.owners") {
        import MetacpOwnersExpect._
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
      test("metac.owners") {
        import MetacOwnersExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("metac-metacp.diff") {
        import MetacMetacpDiffExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
    case _ =>
      ()
  }
}

trait ExpectHelpers extends FunSuiteLike {
  def filename: String
  def path: Path =
    Paths.get("tests", "jvm", "src", "test", "resources", filename)
  def loadExpected: String =
    new String(Files.readAllBytes(path), UTF_8)
  def saveExpected(value: String): Unit =
    Files.write(path, value.getBytes(UTF_8))

  def normalizeSymbol(sym: s.SymbolInformation): s.SymbolInformation = {
    sym.copy(
      language = None,
      signature = None,
      overrides = Nil,
      members = Nil
    )
  }

  def loadSymbols(path: Path): Map[String, s.SymbolInformation] = {
    for {
      file <- FileIO.listAllFilesRecursively(AbsolutePath(path)).iterator
      if PathIO.extension(file.toNIO) == "semanticdb"
      doc <- s.TextDocuments.parseFrom(file.readAllBytes).documents
      sym <- doc.symbols
    } yield sym.symbol -> sym
  }.toMap
  def metacpSymbols = loadSymbols(decompiledPath(Paths.get(BuildInfo.databaseClasspath)))
  def metacSymbols = loadSymbols(Paths.get(BuildInfo.databaseClasspath))

  def unifiedDiff(
      originalTitle: String,
      revisedTitle: String,
      original: String,
      revised: String): String = {
    import scala.collection.JavaConverters._
    val originalLines = original.split("\n").toSeq.asJava
    val revisedLines = revised.split("\n").toSeq.asJava
    val OnlyCurlyBrace = "\\s+}".r
    val diff = {
      val lines = difflib.DiffUtils
        .generateUnifiedDiff(
          originalTitle,
          revisedTitle,
          originalLines,
          difflib.DiffUtils.diff(originalLines, revisedLines),
          3
        )
        .asScala
      if (lines.lengthCompare(2) > 0) {
        lines.remove(2) // remove lines like "@@ -3,18 +3,16 @@"
      }
      lines
        .filterNot(line => OnlyCurlyBrace.findFirstIn(line).isDefined)
        .mkString("\n")
    }
    diff
  }

  protected def highlevelDatabase(path: Path): Database = {
    val database = Database.load(Classpath(path.toString))
    val sorted = Database(database.documents.sortBy(_.input.syntax))
    sorted
  }

  protected def lowlevelSyntax(dirOrJar: Path): String = {
    val (success, out, err) = CliSuite.communicate { (out, err) =>
      val settings = scala.meta.metap.Settings().withPaths(List(dirOrJar))
      val reporter = scala.meta.metap.Reporter().withOut(out).withErr(err)
      Metap.process(settings, reporter)
    }
    if (!success) {
      println(out)
      println(err)
      assert(success, "metap failed")
    }
    assert(err.isEmpty)
    out
  }

  protected def decompiledPath(in: Path): Path = {
    val target = Files.createTempDirectory("target_")
    val (outPath, out, err) = CliSuite.communicate { (out, err) =>
      val settings = scala.meta.metacp
        .Settings()
        .withCacheDir(AbsolutePath(target))
        .withClasspath(Classpath(AbsolutePath(in)))
        .withScalaLibrarySynthetics(false)
      val reporter = scala.meta.metacp.Reporter().withOut(out).withErr(err)
      Metacp.process(settings, reporter) match {
        case Some(Classpath(List(outPath))) => outPath
        case other => sys.error(s"unexpected metacp result: $other")
      }
    }
    assert(out.isEmpty)
    assert(err.isEmpty)
    outPath.toNIO
  }

  protected def ownerSyntax(path: Path): String = {
    val paths = Files.walk(path).iterator.asScala
    val semanticdbs = paths.map(_.toString).filter(_.endsWith(".semanticdb")).toArray.sorted
    val owners = mutable.Map[String, String]()
    semanticdbs.foreach { semanticdb =>
      FileIO.readAllDocuments(AbsolutePath(semanticdb)).foreach { document =>
        document.symbols.foreach { info =>
          if (!info.symbol.startsWith(info.owner)) {
            sys.error(s"invalid owner ${info.owner} for ${info.symbol}")
          }
          if (info.symbol.startsWith("local") && info.owner != "") {
            sys.error(s"invalid owner ${info.owner} for ${info.symbol}")
          }
          val key = {
            if (info.symbol.startsWith("local")) info.symbol + "_" + document.uri
            else info.symbol
          }
          val value = {
            if (info.owner.nonEmpty) info.owner
            else "<none>"
          }
          owners.get(key) match {
            case Some(existingValue) =>
              if (value != existingValue) {
                sys.error(s"conflicting owners for $key: $existingValue, $value")
              }
            case None =>
              owners(key) = value
          }
        }
      }
    }
    // NOTE: This logic arranges the symbols into a neat tree,
    // but unfortunately we can't enable it right now, because we don't
    // save SymbolInformation for owners.
    //
    // val children = owners.toList.groupBy(_._2).mapValues(_.map(_._1))
    // val visited = mutable.Set[String]()
    // val buf = new StringBuilder
    // def loop(sym: String, level: Int): Unit = {
    //   visited += sym
    //   if (sym != "<none>") buf.append((" " * level) + sym + EOL)
    //   children.getOrElse(sym, Nil).sorted.foreach(loop(_, level + 2))
    // }
    // loop("<none>", -2)
    // if (owners.size != visited.size) {
    //   sys.error(s"dangling owners: ${(owners.keySet -- visited).mkString(", ")}")
    // }
    // buf.append(EOL)
    // buf.toString
    owners.toList.sortBy(_._1).map(kv => s"${kv._1} => ${kv._2}").mkString(EOL)
  }
}

object ScalalibExpect extends ExpectHelpers {
  def filename: String = "scalalib.expect"
  def loadObtained: String = {
    val tmp = Files.createTempDirectory("scala-library-synthetics")
    tmp.toFile.deleteOnExit()
    val settings = scala.meta.metacp
      .Settings()
      .withCacheDir(AbsolutePath(tmp))
      .withClasspath(Classpath(Nil))
      .withScalaLibrarySynthetics(true)
    val reporter = scala.meta.metacp.Reporter()
    Metacp.process(settings, reporter) match {
      case Some(Classpath(List(jar))) => lowlevelSyntax(jar.toNIO)
      case other => sys.error(s"unexpected metacp result: $other")
    }
  }
}

object MetacpExpect extends ExpectHelpers {
  def filename: String = "metacp.expect"
  def loadObtained: String = lowlevelSyntax(decompiledPath(Paths.get(BuildInfo.databaseClasspath)))
}

object MetacpOwnersExpect extends ExpectHelpers {
  def filename: String = "metacp.owners"
  def loadObtained: String = ownerSyntax(decompiledPath(Paths.get(BuildInfo.databaseClasspath)))
}

object LowlevelExpect extends ExpectHelpers {
  def filename: String = "lowlevel.expect"
  def loadObtained: String = lowlevelSyntax(Paths.get(BuildInfo.databaseClasspath))
}

object HighlevelExpect extends ExpectHelpers {
  def filename: String = "highlevel.expect"
  def loadObtained: String = loadDatabase.syntax
  def loadDatabase: Database = highlevelDatabase(Paths.get(BuildInfo.databaseClasspath))
}

object MetacOwnersExpect extends ExpectHelpers {
  def filename: String = "metac.owners"
  def loadObtained: String = ownerSyntax(Paths.get(BuildInfo.databaseClasspath))
}

object MetacMetacpDiffExpect extends ExpectHelpers {
  def filename: String = "metac-metacp.diff"
  def loadObtained: String = {
    val javacp = metacpSymbols
    val metac = metacSymbols.valuesIterator.toSeq.sortBy(_.symbol)
    val symbols = for {
      sym <- metac.iterator
      javasym <- {
        if (sym.symbol.contains("com.javacp")) Some(javacp.getOrElse(sym.symbol, s.SymbolInformation()))
        else javacp.get(sym.symbol)
      }
    } yield {
      val header = "=" * sym.symbol.length
      val diff = unifiedDiff(
        "metac",
        "javacp",
        normalizeSymbol(sym).toProtoString,
        normalizeSymbol(javasym).toProtoString
      )
      if (diff.isEmpty) ""
      else {
        s"""$header
           |${sym.symbol}
           |$header
           |$diff
           |
           |
           |""".stripMargin
      }
    }
    symbols.mkString
  }
}

// To save the current behavior, run `sbt save-expect`.
object SaveExpectTest {
  def main(args: Array[String]): Unit = {
    ScalalibExpect.saveExpected(ScalalibExpect.loadObtained)
    MetacpExpect.saveExpected(MetacpExpect.loadObtained)
    MetacpOwnersExpect.saveExpected(MetacpOwnersExpect.loadObtained)
    LowlevelExpect.saveExpected(LowlevelExpect.loadObtained)
    HighlevelExpect.saveExpected(HighlevelExpect.loadObtained)
    MetacOwnersExpect.saveExpected(MetacOwnersExpect.loadObtained)
    MetacMetacpDiffExpect.saveExpected(MetacMetacpDiffExpect.loadObtained)
  }
}

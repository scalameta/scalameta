package scala.meta.tests
package semanticdb

import scala.meta.internal.{semanticdb => s}
import java.nio.file._
import java.nio.charset.StandardCharsets._
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.cli._
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.internal.semanticdb._
import scala.meta.io.AbsolutePath
import scala.meta.tests.cli._
import scala.meta.testkit.DiffAssertions
import org.scalatest.FunSuite
import org.scalatest.FunSuiteLike
import scala.meta.tests.metacp.Library

class ExpectSuite extends FunSuite with DiffAssertions {
  BuildInfo.scalaVersion.split("\\.").take(2).toList match {
    // both the compiler and stdlib are different between Scala versions.
    // For the sake of simplicity, we only run the expect test against the
    // output of 2.12. It's possible to add another expect file for 2.11
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
      test("metacp.index") {
        import MetacpIndexExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("metac.expect") {
        import MetacExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("metac.index") {
        import MetacIndexExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("metac-metacp.diff") {
        import MetacMetacpExpectDiffExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("manifest.metap") {
        import ManifestMetap._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("manifest.metacp") {
        import ManifestMetacp._
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

  protected def unifiedDiff(
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
        .filterNot(_.startsWith("@@"))
        .mkString("\n")
    }
    diff
  }

  protected def normalizedSymbols(path: Path): Map[String, s.SymbolInformation] = {
    for {
      file <- FileIO.listAllFilesRecursively(AbsolutePath(path)).iterator
      if PathIO.extension(file.toNIO) == "semanticdb"
      doc <- s.TextDocuments.parseFrom(file.readAllBytes).documents
      sym <- doc.symbols
    } yield sym.symbol -> sym
  }.toMap

  protected def metap(dirOrJar: Path): String = {
    val (success, out, err) = CliSuite.communicate { (out, err) =>
      import scala.meta.metap.Format._
      val settings = scala.meta.metap.Settings().withFormat(Detailed).withPaths(List(dirOrJar))
      val reporter = Reporter().withOut(out).withErr(err)
      Metap.process(settings, reporter)
    }
    if (!success) {
      println(out)
      println(err)
      fail("metap failed")
    }
    assert(err.isEmpty)
    out
  }

  protected def metacp(in: Path): Path = {
    val target = Files.createTempDirectory("target_")
    val (outPath, out, err) = CliSuite.communicate { (out, err) =>
      val settings = scala.meta.metacp
        .Settings()
        .withCacheDir(AbsolutePath(target))
        .withClasspath(Classpath(AbsolutePath(in)))
        .withDependencyClasspath(Classpath(
          Library.scalaLibrary.classpath().entries ++
            Library.jdk.classpath().entries
        ))
        .withScalaLibrarySynthetics(false)
      val reporter = Reporter().withOut(out).withErr(err)
      Metacp.process(settings, reporter) match {
        case Some(Classpath(List(outPath))) => outPath
        case Some(other) => sys.error(s"unexpected metacp result: $other")
        case None => null
      }
    }
    if (outPath == null) {
      println(out)
      println(err)
      fail("metacp failed")
    }
    assert(err.isEmpty)
    outPath.toNIO
  }

  protected def index(path: Path): String = {
    val semanticdbSemanticidx = path.resolve("META-INF/semanticdb.semanticidx")
    if (Files.exists(semanticdbSemanticidx)) {
      val index = FileIO.readIndex(AbsolutePath(semanticdbSemanticidx))
      printIndex(index)
    } else {
      ""
    }
  }

  def printIndex(index: s.Index): String = {
    val buf = new StringBuilder
    buf.append("Packages:" + EOL)
    buf.append("=========" + EOL)
    index.packages.sortBy(_.symbol).foreach { entry =>
      buf.append(entry.symbol + EOL)
      entry.members.sorted.foreach(member => buf.append("  " + member + EOL))
    }
    buf.append(EOL)
    buf.append("Toplevels:" + EOL)
    buf.append("==========" + EOL)
    index.toplevels.sortBy(_.symbol).foreach { entry =>
      buf.append(s"${entry.symbol} => ${entry.uri}" + EOL)
    }
    buf.toString
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
    val reporter = Reporter()
    Metacp.process(settings, reporter) match {
      case Some(Classpath(List(jar))) => metap(jar.toNIO)
      case other => sys.error(s"unexpected metacp result: $other")
    }
  }
}

object MetacpExpect extends ExpectHelpers {
  def filename: String = "metacp.expect"
  def loadObtained: String = metap(metacp(Paths.get(BuildInfo.databaseClasspath)))
}

object MetacpIndexExpect extends ExpectHelpers {
  def filename: String = "metacp.index"
  def loadObtained: String = index(metacp(Paths.get(BuildInfo.databaseClasspath)))
}

object MetacExpect extends ExpectHelpers {
  def filename: String = "metac.expect"
  def loadObtained: String = metap(Paths.get(BuildInfo.databaseClasspath))
}

object MetacIndexExpect extends ExpectHelpers {
  def filename: String = "metac.index"
  def loadObtained: String = index(Paths.get(BuildInfo.databaseClasspath))
}

object MetacMetacpExpectDiffExpect extends ExpectHelpers {
  def filename: String = "metac-metacp.expect.diff"
  def loadObtained: String = {
    val metacp = metacpSymbols
    val metac = metacSymbols.valuesIterator.toSeq.sortBy(_.symbol)
    val symbols = for {
      sym <- metac.iterator
      javasym <- {
        if (sym.symbol.contains("com.javacp")) {
          // metac references to java defined symbols in com.javacp must have a corresponding metacp entry.
          Some(metacp.getOrElse(sym.symbol, s.SymbolInformation()))
        } else {
          metacp.get(sym.symbol)
        }
      }
    } yield {
      val header = "=" * sym.symbol.length
      val diff = unifiedDiff(
        "metac",
        "metacp",
        sym.toProtoString,
        javasym.toProtoString
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
  def metacpSymbols = normalizedSymbols(metacp(Paths.get(BuildInfo.databaseClasspath)))
  def metacSymbols = normalizedSymbols(Paths.get(BuildInfo.databaseClasspath))
}

object ManifestMetap extends ExpectHelpers {
  def filename: String = "manifest.metap"
  def loadObtained: String = {
    val manifestJar = path.getParent.resolve("manifest.jar")
    metap(manifestJar)
  }
}

object ManifestMetacp extends ExpectHelpers {
  def filename: String = "manifest.metacp"
  def loadObtained: String = {
    val manifestJar = path.getParent.resolve("manifest.jar")
    metap(metacp(manifestJar))
  }
}

// To save the current behavior, run `sbt save-expect`.
object SaveExpectTest {
  def main(args: Array[String]): Unit = {
    ScalalibExpect.saveExpected(ScalalibExpect.loadObtained)
    MetacpExpect.saveExpected(MetacpExpect.loadObtained)
    MetacpIndexExpect.saveExpected(MetacpIndexExpect.loadObtained)
    MetacExpect.saveExpected(MetacExpect.loadObtained)
    MetacIndexExpect.saveExpected(MetacIndexExpect.loadObtained)
    MetacMetacpExpectDiffExpect.saveExpected(MetacMetacpExpectDiffExpect.loadObtained)
    ManifestMetap.saveExpected(ManifestMetap.loadObtained)
    ManifestMetacp.saveExpected(ManifestMetacp.loadObtained)
  }
}

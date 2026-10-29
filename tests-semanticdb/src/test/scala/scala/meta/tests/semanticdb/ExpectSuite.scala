package scala.meta.tests
package semanticdb

import scala.meta.cli._
import scala.meta.internal.io._
import scala.meta.internal.semanticdb._
import scala.meta.io.{AbsolutePath, Classpath}
import scala.meta.tests.cli.CliTestUtils
import scala.meta.tests.metacp.MetacpOps

import java.io._
import java.nio.charset.StandardCharsets._
import java.nio.file._
import java.util.jar._

import scala.collection.JavaConverters._
import scala.collection.mutable

import munit.FunSuite
import munit.diff.DiffUtils

class ExpectSuite extends FunSuite {

  override def munitIgnore: Boolean = !ScalaVersion.isLatest212Or213

  test("scalalib.expect") {
    import ScalalibExpect._
    this.assertNoDiff(loadObtained, loadExpected)
  }
  test("metacp.expect") {
    import MetacpExpect._
    this.assertNoDiff(loadObtained, loadExpected)
  }
  test("metac.expect") {
    import MetacExpect._
    this.assertNoDiff(loadObtained, loadExpected)
  }

  test("metac-metacp.diff") {
    import MetacMetacpDiffExpect._
    this.assertNoDiff(loadObtained, loadExpected)
  }
  test("manifest.metap") {
    import ManifestMetap._
    this.assertNoDiff(loadObtained, loadExpected)
  }

  test("manifest.metacp") {
    import ManifestMetacp._
    this.assertNoDiff(loadObtained, loadExpected)
  }
  test("metacp.undefined") {
    import MetacpUndefined._
    this.assertNoDiff(loadObtained, loadExpected)
  }
}

trait ExpectHelpers extends munit.Assertions {

  def filename: String

  def loadExpected: String = new String(Files.readAllBytes(path), UTF_8)
  def loadObtained: String

  def path: Path = {
    def filenameToPath(filename: String) = Utils.getResourceOpt(filename)
    val filenameWithVersion = filename + "_" + BuildInfo.scalaBinaryVersion
    filenameToPath(filename) match {
      case Some(baseFile) if baseFile.toFile().exists() => baseFile
      case _ => filenameToPath(filenameWithVersion).get
    }
  }
  def manifestJar: Path = path.resolveSibling("manifest.jar")
  def classDirectory: Path = Paths.get(BuildInfo.databaseClasspath)

  def saveExpected(): Unit = Files.write(path, loadObtained.getBytes(UTF_8))

  protected def unifiedDiff(
      originalTitle: String,
      revisedTitle: String,
      original: String,
      revised: String,
  ): String = {
    val originalLines = original.split("\n").toSeq.asJava
    val revisedLines = revised.split("\n").toSeq.asJava
    val OnlyCurlyBrace = "\\s+}".r
    val diff = {
      val lines = DiffUtils.generateUnifiedDiff(
        originalTitle,
        revisedTitle,
        originalLines,
        DiffUtils.diff(originalLines, revisedLines),
        3,
      ).asScala
      if (lines.lengthCompare(2) > 0) lines.remove(2) // remove lines like "@@ -3,18 +3,16 @@"
      lines.filterNot(line => OnlyCurlyBrace.findFirstIn(line).isDefined)
        .filterNot(_.startsWith("@@")).mkString("\n")
    }
    diff
  }

  protected def loadMiniSymtab(path: Path): Map[String, SymbolInformation] = {
    for {
      file <- FileIO.listAllFilesRecursively(AbsolutePath(path)).iterator
      if PathIO.extension(file.toNIO) == "semanticdb"
      doc <- TextDocuments.parseFrom(file.readAllBytes).documents
      sym <- doc.symbols
    } yield sym.symbol -> sym
  }.toMap

  protected def metap(dirOrJar: Path): String = {
    val (success, out, err) = CliTestUtils.communicate { (out, err) =>
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
    val (outPath, out, err) = CliTestUtils.communicate { (out, err) =>
      val settings = scala.meta.metacp.Settings().withOut(AbsolutePath(target))
        .withClasspath(Classpath(AbsolutePath(in))).withUsejavacp(true)
        .withScalaLibrarySynthetics(false)
      val reporter = Reporter().withOut(out).withErr(err)
      val result = Metacp.process(settings, reporter)
      result.classpath match {
        case Some(Classpath(List(outPath))) => outPath
        case Some(other) => sys.error(s"unexpected metacp result: $other")
        case None => null
      }
    }
    if (outPath == null) {
      println(err)
      fail("metacp failed")
    }
    assert(err.isEmpty)
    outPath.toNIO
  }

}

object ScalalibExpect extends ExpectHelpers {
  def filename: String = "scalalib.expect"
  def loadObtained: String = {
    val tmp = Files.createTempDirectory("scala-library-synthetics")
    tmp.toFile.deleteOnExit()
    val settings = scala.meta.metacp.Settings().withOut(AbsolutePath(tmp))
      .withClasspath(Classpath(Nil)).withScalaLibrarySynthetics(true)
    val reporter = Reporter().withSilentOut()
    Metacp.process(settings, reporter).classpath match {
      case Some(Classpath(List(jar))) => metap(jar.toNIO)
      case other => sys.error(s"unexpected metacp result: $other")
    }
  }
}

object MetacpExpect extends ExpectHelpers {
  def filename: String = "metacp.expect"
  def loadObtained: String = metap(metacp(classDirectory))
}

object MetacExpect extends ExpectHelpers {
  def filename: String = "metac.expect"
  def loadObtained: String = metap(classDirectory)
}

object MetacMetacpDiffExpect extends ExpectHelpers {
  def filename: String = "metac-metacp.diff"
  def loadObtained: String = {
    val metacp = metacpSymbols
    val metac = metacSymbols.valuesIterator.toSeq.sortBy(_.symbol)
    val symbols = for {
      sym <- metac.iterator
      javasym <- metacp.get(sym.symbol)
    } yield {
      val header = "=" * sym.symbol.length
      val diff = unifiedDiff(
        "metac",
        "metacp",
        renumberLocals(sym.toProtoString),
        renumberLocals(javasym.toProtoString),
      )
      if (diff.isEmpty) ""
      else s"""$header
              |${sym.symbol}
              |$header
              |$diff
              |
              |
              |""".stripMargin
    }
    symbols.mkString
  }
  def metacpSymbols = loadMiniSymtab(metacp(classDirectory))
  def metacSymbols = loadMiniSymtab(classDirectory)

  // metac and metacp number local symbols differently
  private def renumberLocals(proto: String): String = {
    val ids = mutable.Map.empty[String, Int]
    "(?<=\")local\\d+(?=\")".r
      .replaceAllIn(proto, m => "local" + ids.getOrElseUpdate(m.matched, ids.size))
  }
}

object ManifestMetap extends ExpectHelpers {
  def filename: String = "manifest.metap"
  def loadObtained: String = metap(manifestJar)
}

object ManifestMetacp extends ExpectHelpers {
  def filename: String = "manifest.metacp"
  def loadObtained: String = metap(metacp(manifestJar))
}

object MetacpUndefined extends ExpectHelpers {
  def filename: String = "metacp.undefined"
  def loadObtained: String = {
    val root = metacp(classDirectory)
    val classpath = Classpath(AbsolutePath(root))
    val interesting = MetacpOps.collectReferencedToUndefinedSymbols(classpath)(x =>
      // We are only interested in references to symbols in semanticdb/integration
      // that have no SymbolInformation. It's expected that references to the packages
      // scala/ and java/ package have no SymbolInformation because we only process
      // databaseClasspath, scala-library and the JDK are only --dependency-classpath.
      !x.startsWith("scala/") && !x.startsWith("java/") && x != "local_wildcard",
    )
    val sb = new StringBuilder()
    interesting.sortBy(_._1).foreach { case (path, errors) =>
      sb.append("---\n").append(root.relativize(path)).append("\n---\n")
      errors.toSeq.sorted.foreach(error => sb.append(error).append("\n"))
    }
    sb.toString()
  }
}

// To save the current behavior, run `sbt save-expect`.
object SaveExpectTest {
  def main(args: Array[String]): Unit = {
    ScalalibExpect.saveExpected()
    MetacpExpect.saveExpected()
    MetacExpect.saveExpected()
    MetacMetacpDiffExpect.saveExpected()
    ManifestMetap.saveExpected()
    ManifestMetacp.saveExpected()
    MetacpUndefined.saveExpected()
    OccurrenceSuite.saveExpected()
  }
}

object SaveManifestTest {
  def main(args: Array[String]): Unit = {
    val classes = ManifestMetacp.classDirectory

    val manifest = Utils.getResourceOpt("manifest.jar").get
    val part0 = Utils.getResourceOpt("part0.jar").get
    val part1 = Utils.getResourceOpt("part1.jar").get

    withJar(manifest) { jos =>
      jos.putNextEntry(new JarEntry("META-INF/MANIFEST.MF"))
      val manifest =
        """|Manifest-Version: 1.0
           |Class-Path: part0.jar part1.jar
           |""".stripMargin + "\n"
      jos.write(manifest.getBytes(UTF_8))
      jos.closeEntry()
    }

    // take only the classes of example/Empty.scala
    val emptyClassfiles = List("A.class", "B.class").map(classes.resolve)
    withJar(part0)(jos =>
      emptyClassfiles.foreach { classfile =>
        jos.putNextEntry(new JarEntry(classes.relativize(classfile).toString))
        jos.write(Files.readAllBytes(classfile))
        jos.closeEntry()
      },
    )

    val emptySemanticdbRelPath =
      "META-INF/semanticdb/semanticdb/integration/src/main/scala/example/Empty.scala.semanticdb"
    val emptySemanticdbAbsPath = classes.resolve(emptySemanticdbRelPath)
    withJar(part1) { jos =>
      jos.putNextEntry(new JarEntry(emptySemanticdbRelPath))
      jos.write(Files.readAllBytes(emptySemanticdbAbsPath))
      jos.closeEntry()
    }

    ManifestMetap.saveExpected()
    ManifestMetacp.saveExpected()
  }

  private def withJar(path: Path)(fn: JarOutputStream => Unit): Unit = {
    val os = Files.newOutputStream(path)
    val bos = new BufferedOutputStream(os)
    val jos = new JarOutputStream(bos)
    try fn(jos)
    finally {
      jos.close()
      bos.close()
      os.close()
    }
  }
}

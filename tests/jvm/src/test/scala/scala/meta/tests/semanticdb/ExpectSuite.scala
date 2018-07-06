package scala.meta.tests
package semanticdb

import scala.meta.internal.{semanticdb => s}
import java.io._
import java.nio.file._
import java.nio.charset.StandardCharsets._
import java.util.jar._
import scala.collection.JavaConverters._
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
import scala.meta.tests.metacp.MetacpOps

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
      test("metac.expect") {
        import MetacExpect._
        assertNoDiff(loadObtained, loadExpected)
      }
      test("metac-metacp.diff") {
        import MetacMetacpDiffExpect._
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
      test("metacp.undefined") {
        import MetacpUndefined._
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

  protected def loadMiniSymtab(path: Path): Map[String, s.SymbolInformation] = {
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
        .withOut(AbsolutePath(target))
        .withClasspath(Classpath(AbsolutePath(in)))
        .withDependencyClasspath(Library.scalaLibrary.classpath() ++ Library.jdk.classpath())
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

}

object ScalalibExpect extends ExpectHelpers {
  def filename: String = "scalalib.expect"
  def loadObtained: String = {
    val tmp = Files.createTempDirectory("scala-library-synthetics")
    tmp.toFile.deleteOnExit()
    val settings = scala.meta.metacp
      .Settings()
      .withOut(AbsolutePath(tmp))
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

object MetacExpect extends ExpectHelpers {
  def filename: String = "metac.expect"
  def loadObtained: String = metap(Paths.get(BuildInfo.databaseClasspath))
}

object MetacMetacpDiffExpect extends ExpectHelpers {
  def filename: String = "metac-metacp.diff"
  def loadObtained: String = {
    val metacp = sortDeclarations(metacpSymbols)
    val metac = sortDeclarations(metacSymbols).valuesIterator.toSeq.sortBy(_.symbol)
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
  def metacpSymbols = loadMiniSymtab(metacp(Paths.get(BuildInfo.databaseClasspath)))
  def metacSymbols = loadMiniSymtab(Paths.get(BuildInfo.databaseClasspath))

  // FIXME: https://github.com/scalameta/scalameta/issues/1642
  // We sort class signature declarations to make it easier to eye-ball actual bugs
  // in metac-metacp.diff. Without sorting, the diffs become noisy for questionable
  // benefit since at the moment the biggest priority is to fix all metac/metacp
  // differences in symbol formats, signatures, accessibilities, etc.
  // Presevering the source ordering of declarations is important for documentation tools
  // so we should eventually stop sorting them here.
  private def sortDeclarations(
      symtab: Map[String, SymbolInformation]
  ): Map[String, SymbolInformation] = symtab.map {
    case (key, sym) =>
      val newSymbol = sym.signature match {
        case c: ClassSignature if sym.language.isJava =>
          val sortedJavaDeclarations = c.declarations.get.symlinks.sorted
          sym.copy(
            signature = c.copy(
              declarations = Some(
                c.declarations.get.copy(symlinks = sortedJavaDeclarations)
              )
            )
          )
        case _ => sym
      }
      key -> newSymbol
  }
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

object MetacpUndefined extends ExpectHelpers {
  def filename: String = "metacp.undefined"
  def loadObtained: String = {
    val classpath = Classpath(AbsolutePath(metacp(Paths.get(BuildInfo.databaseClasspath))))
    val undefined = MetacpOps.collectReferencedToUndefinedSymbols(classpath)
    val interesting = undefined.filterNot { symbol =>
      // We are only interested in references to symbols in semanticdb/integration
      // that have no SymbolInformation. It's expected that references to the packages
      // scala/ and java/ package have no SymbolInformation because we only process
      // databaseClasspath, scala-library and the JDK are only --dependency-classpath.
      symbol.startsWith("scala/") ||
      symbol.startsWith("java/") ||
      symbol == "local_wildcard"
    }
    interesting.toSeq.sorted.mkString("", "\n", "\n")
  }
}

// To save the current behavior, run `sbt save-expect`.
object SaveExpectTest {
  def main(args: Array[String]): Unit = {
    ScalalibExpect.saveExpected(ScalalibExpect.loadObtained)
    MetacpExpect.saveExpected(MetacpExpect.loadObtained)
    MetacExpect.saveExpected(MetacExpect.loadObtained)
    MetacMetacpDiffExpect.saveExpected(MetacMetacpDiffExpect.loadObtained)
    ManifestMetap.saveExpected(ManifestMetap.loadObtained)
    ManifestMetacp.saveExpected(ManifestMetacp.loadObtained)
    MetacpUndefined.saveExpected(MetacpUndefined.loadObtained)
  }
}

object SaveManifestTest {
  def main(args: Array[String]): Unit = {
    val classes = Paths.get(BuildInfo.databaseClasspath)
    val resources = Paths.get("tests", "jvm", "src", "test", "resources")

    val manifest = resources.resolve("manifest.jar")
    val part0 = resources.resolve("part0.jar")
    val part1 = resources.resolve("part1.jar")

    withJar(manifest) { jos =>
      jos.putNextEntry(new JarEntry("META-INF/MANIFEST.MF"))
      val manifest = """
        |Manifest-Version: 1.0
        |Class-Path: part0.jar part1.jar
      """.trim.stripMargin + "\n\n"
      jos.write(manifest.getBytes(UTF_8))
      jos.closeEntry()
    }

    val emptyClassfiles =
      Files.list(classes).iterator.asScala.toList.filter(f => Files.isRegularFile(f))
    withJar(part0) { jos =>
      emptyClassfiles.foreach { classfile =>
        jos.putNextEntry(new JarEntry(classes.relativize(classfile).toString))
        jos.write(Files.readAllBytes(classfile))
        jos.closeEntry()
      }
    }

    val emptySemanticdbRelPath =
      "META-INF/semanticdb/semanticdb/integration/src/main/scala/example/Empty.scala.semanticdb"
    val emptySemanticdbAbsPath = classes.resolve(emptySemanticdbRelPath)
    withJar(part1) { jos =>
      jos.putNextEntry(new JarEntry(emptySemanticdbRelPath))
      jos.write(Files.readAllBytes(emptySemanticdbAbsPath))
      jos.closeEntry()
    }

    ManifestMetap.saveExpected(ManifestMetap.loadObtained)
    ManifestMetacp.saveExpected(ManifestMetacp.loadObtained)
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

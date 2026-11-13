package scala.meta.tests.semanticdb

import scala.meta.tests.BuildInfo

import java.io.File
import java.nio.file.Paths
import java.util.zip.ZipFile
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.XPathFactory

import scala.collection.JavaConverters._

import munit.FunSuite

class ArtifactsSuite extends FunSuite {

  private def classes(jar: String): Set[String] = {
    val zip = new ZipFile(jar)
    try zip.entries().asScala.map(_.getName).filter(_.endsWith(".class")).toSet
    finally zip.close()
  }

  // leave out a package nested in another one
  private def packages(classes: Set[String]): List[String] = {
    val dirs = classes.map(_.split('/').init.mkString("/"))
    dirs.filterNot(dir => dirs.exists(x => dir.startsWith(x + "/"))).toList.sorted
  }

  private def hasEntry(jar: File, name: String): Boolean = {
    val zip = new ZipFile(jar)
    try zip.getEntry(name) ne null
    finally zip.close()
  }

  private val pluginClasses = classes(sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar"))
  private val compilerClasses = sys.props("sbt.paths.scala-compiler.jars").split(File.pathSeparator)
    .flatMap(classes).toSet

  // https://github.com/scalameta/scalameta/issues/4080
  test("#4080: plugin classes outside scalameta") {
    val bundled = pluginClasses.diff(compilerClasses)
      .filterNot(x => x.startsWith("scala/meta/") || x.startsWith("org/scalameta/"))
    assertEquals(packages(bundled), Nil)
  }

  test("#4080: plugin classes in the compiler classpath")(
    assertEquals(packages(pluginClasses.intersect(compilerClasses)), Nil),
  )

  test("symbol table packages in the plugin") {
    val inPlugin = pluginClasses.map(_.split('/').init.mkString("/"))
    val internal = List("classpath", "javacp", "javacp/asm", "metacp", "scalacp", "symtab")
    val expected = internal.map("scala/meta/internal/" + _) :+ "scala/meta/metacp"
    assertEquals(expected.filterNot(inPlugin), Nil)
  }

  test("symbol table classes on the test classpath") {
    val name = "scala/meta/internal/symtab/GlobalSymbolTable.class"
    val entries = sys.props("sbt.paths.tests.test.classes").split(File.pathSeparator).filter { x =>
      val file = new File(x)
      if (file.isDirectory) new File(file, name).isFile else file.isFile && hasEntry(file, name)
    }
    assertEquals(entries.length, 1, entries.mkString(File.pathSeparator))
    val entry = Paths.get(entries(0))
    assert(entry.startsWith(BuildInfo.databaseSourcepath), entry)
  }

  test("core's pom") {
    val pom = DocumentBuilderFactory.newInstance.newDocumentBuilder
      .parse(new File(sys.props("sbt.paths.semanticdb-scalac-core.pom")))
    val xpath = XPathFactory.newInstance.newXPath
    val symtab = s"semanticdb-symtab_${BuildInfo.scalaBinaryVersion}"
    val obtained = xpath
      .evaluate(s"/project/dependencies/dependency[artifactId='$symtab']/version", pom)
    assertEquals(obtained, xpath.evaluate("/project/version", pom))
  }

}

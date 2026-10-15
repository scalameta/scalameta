package scala.meta.tests.semanticdb

import java.io.File
import java.util.zip.ZipFile

import scala.collection.JavaConverters._

import munit.FunSuite

// https://github.com/scalameta/scalameta/issues/4080
class Issue4080Suite extends FunSuite {

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

  private val isScala212 = scala.util.Properties.versionNumberString.startsWith("2.12")

  private val pluginClasses = classes(sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar"))
  private val compilerClasses = sys.props("sbt.paths.scala-compiler.jars").split(File.pathSeparator)
    .flatMap(classes).toSet

  test("plugin classes outside scalameta") {
    val bundled = pluginClasses.diff(compilerClasses)
      .filterNot(x => x.startsWith("scala/meta/") || x.startsWith("org/scalameta/"))
    val scalaPackages =
      if (isScala212) List(
        "scala/annotation",
        "scala/collection/compat",
        "scala/jdk",
        "scala/tools/scalap",
        "scala/util",
      )
      else List(
        "scala/collection/compat",
        "scala/tools/scalap",
        "scala/util/control/compat",
        "scala/util/matching/compat",
      )
    val expected = List(
      "buildinfo",
      "com/google/protobuf",
      "com/lihaoyi",
      "fastparse",
      "geny",
      "org/portablescala/reflect",
    ) ++ scalaPackages ++ List("scalapb", "sourcecode")
    assertEquals(packages(bundled), expected)
  }

  test("plugin classes in the compiler classpath")(
    assertEquals(packages(pluginClasses.intersect(compilerClasses)), Nil),
  )

}

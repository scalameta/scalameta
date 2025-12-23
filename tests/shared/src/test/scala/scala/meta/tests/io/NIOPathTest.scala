package scala.meta.tests
package io

import scala.meta.internal.io._
import scala.meta.io._

import java.io.File
import java.nio.file.{Files, Path, Paths}

import scala.util.{Success, Try}

import munit.FunSuite

class NIOPathTest extends FunSuite {

  def rootString = File.listRoots()(0).getPath

  def file: Path = Paths.get("build.sbt")
  def project: Path = Paths.get("project")
  def abs: Path = Paths.get(rootString).resolve("bar").resolve("foo")
  def cwd: Path = Paths.get(PlatformPathIO.workingDirectoryString)
  val nonNormalizedFile: Path = Paths.get("project", "..", "bin", "scalafmt")

  test(".toString") {
    assertEquals(file.toString, "build.sbt")
    assertEquals(project.toString, "project")
    assertEquals(abs.toString, s"${rootString}bar${File.separator}foo")
    assertEquals(nonNormalizedFile.toString, PathIO.fromUnix("project/../bin/scalafmt"))
  }

  test(".isAbsolute") {
    assert(!file.isAbsolute)
    assert(abs.isAbsolute)
    assert(cwd.isAbsolute)
  }
  test(".getRoot") {
    assertEquals(file.getRoot, null)
    assertNotEquals(Paths.get("").toAbsolutePath.getRoot, null)
  }
  test(".getFileName") {
    assertEquals(file.getFileName.toString, "build.sbt")
    assertEquals(abs.getFileName.toString, "foo")
    assertEquals(nonNormalizedFile.getFileName.toString, "scalafmt")
  }
  test(".getParent")(assertEquals(abs.getParent.getFileName.toString, "bar"))
  test(".getNameCount") {
    assertEquals(Paths.get(rootString).getNameCount, 0, rootString)
    assertEquals(Paths.get("").getNameCount, 1)
    assertEquals(abs.getNameCount, 2, abs)
    assertEquals(nonNormalizedFile.getNameCount, 4, nonNormalizedFile)
  }
  test(".getName(index)") {
    assertEquals(file.getName(0).toString, "build.sbt", file)
    assertEquals(abs.getName(0).toString, "bar", abs)
    assertEquals(abs.getName(1).toString, "foo", abs)
  }
  test(".subpath")(assertEquals(abs.subpath(0, 1).toString, "bar", abs))
  test(".startsWith(Path)") {
    assert(!abs.startsWith("bar"))
    assert(!file.startsWith("build"))
    assert(file.startsWith("build.sbt"))
  }
  test(".endsWith(Path)") {
    assert(abs.endsWith("foo"))
    assert(!file.endsWith("build"))
    assert(file.endsWith("build.sbt"))
  }
  test(".normalize") {
    assertEquals(file.resolve("foo").resolve("..").normalize().toString, "build.sbt")
    assertEquals(nonNormalizedFile.normalize().toString, PathIO.fromUnix("bin/scalafmt"))
  }
  test(".resolve") {
    assert(!file.resolve("bar").isAbsolute)
    assert(abs.resolve("bar").isAbsolute)
  }
  test(".resolveSibling(Path)") {
    assertEquals(file.resolveSibling("build.scala").toString, "build.scala")
    assertEquals(abs.resolveSibling("foobar"), abs.getParent.resolve("foobar"))
  }
  test(".relativize(Path)")(assertEquals(abs.relativize(abs.resolve("qux")), Paths.get("qux")))
  test("file.toUri")(
    assert(file.toUri.getPath.endsWith("build.sbt"))
    // NOTE: Paths API seems to work inconsistently under Scala Native.
    // [info] - .toUri *** FAILED ***
    // [info]   "/Users/eburmako/Projects/scalameta/project" did not end with "project/" (NIOPathTest.scala:84)
    // assert(project.toUri.getPath.endsWith("project/"))
  )

  test("jar.toUri")(
    if (scala.meta.internal.platform.isJVM) {
      val jar = Files.createTempDirectory("scalameta").resolve("foo.jar")
      FileIO.withJarFileSystem(AbsolutePath(jar), true, true) { root =>
        val hello = root.resolve("hello")
        Files.createDirectories(hello.toNIO)
        assert(hello.toURI.toString.endsWith("hello"))
      }
    }
  )

  test(".toAbsolutePath")(
    assert(file.toAbsolutePath.endsWith(file))
    // NOTE: Paths API seems to work inconsistently under Scala Native.
    // [info] - .toAbsolutePath *** FAILED ***
    // [info]   /bar/foo did not equal //bar/foo (NIOPathTest.scala:88)
    // assertEquals(abs.toAbsolutePath, abs)
    // NOTE: Paths API seems to work inconsistently under Scala Native.
    // [info] - .toAbsolutePath *** FAILED ***
    // [info]   /Users/eburmako/Projects/scalameta/. did not equal /Users/eburmako/Projects/scalameta (NIOPathTest.scala:92)
    // assertEquals(cwd, Paths.get("").toAbsolutePath)
  )
  test(".toFile") {
    assert(file.toFile.isFile)
    assert(project.toFile.isDirectory)
    assert(cwd.toFile.isDirectory)
  }

  private val pathsWithNames =
    if (File.separatorChar == '\\') Seq(
      ("//foo/bar//baz/", "\\\\foo\\bar\\", 1, Seq("baz")),
      ("//foo/bar///baz", "\\\\foo\\bar\\", 1, Seq("baz")),
      ("/foo/bar//baz/", "\\", 3, Seq("foo", "bar", "baz")),
      ("/foo/bar///baz", "\\", 3, Seq("foo", "bar", "baz")),
      ("foo/bar////baz", null, 3, Seq("foo", "bar", "baz")),
      ("c:/foo/bar///baz", "c:\\", 3, Seq("foo", "bar", "baz")),
      ("c:\\foo\\\\bar\\baz", "c:\\", 3, Seq("foo", "bar", "baz")),
      ("foo\\bar\\baz", null, 3, Seq("foo", "bar", "baz")),
      ("\\\\foo\\bar\\baz", "\\\\foo\\bar\\", 1, Seq("baz"))
    )
    else Seq(
      ("//foo/bar//baz/", "/", 3, Seq("foo", "bar", "baz")),
      ("//foo/bar///baz", "/", 3, Seq("foo", "bar", "baz")),
      ("/foo/bar/baz/", "/", 3, Seq("foo", "bar", "baz")),
      ("/foo/bar/baz", "/", 3, Seq("foo", "bar", "baz")),
      ("foo/bar////baz", null, 3, Seq("foo", "bar", "baz"))
    )
  pathsWithNames.foreach { case (file, root, cnt, names) =>
    test(s".getName: $file -> [${names.mkString(",")}]") {
      try {
        val path = Paths.get(file)
        val actualRoot = Option(path.getRoot).fold[String](null)(_.toString)
        val actualNames = Seq.newBuilder[String]
        var idx = 0
        while (Try(path.getName(idx)) match {
            case Success(name) =>
              idx += 1
              actualNames += name.toString
              true
            case _ => false
          }) {}
        assertEquals((actualRoot, actualNames.result(), path.getNameCount), (root, names, cnt))
        assertEquals(names.length, cnt)
      } catch { case e: Throwable => fail("failed", e) }
    }
  }

}

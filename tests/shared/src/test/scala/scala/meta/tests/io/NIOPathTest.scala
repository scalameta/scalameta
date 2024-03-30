package scala.meta.tests
package io

import scala.meta.internal.io._
import scala.meta.io._

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

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
    assertEquals(nonNormalizedFile.toString, PathIO.fromUnix("project/../bin/scalafmt"))
  }

  test(".isAbsolute") {
    assert(!file.isAbsolute)
    assert(abs.isAbsolute)
    assert(cwd.isAbsolute)
  }
  test(".getRoot") {
    assertEquals(file.getRoot, null)
    assert(Paths.get("").toAbsolutePath.getRoot != null)
  }
  test(".getFileName") {
    assertEquals(file.getFileName.toString, "build.sbt")
    assertEquals(abs.getFileName.toString, "foo")
    assertEquals(nonNormalizedFile.getFileName.toString, "scalafmt")
  }
  test(".getParent")(assertEquals(abs.getParent.getFileName.toString, "bar"))
  test(".getNameCount") {
    assert(Paths.get(rootString).getNameCount == 0)
    assert(Paths.get("").getNameCount == 1)
    assertEquals(abs.getNameCount, 2)
    assertEquals(nonNormalizedFile.getNameCount, 4)
  }
  test(".getName(index)") {
    assert(file.getName(0).toString == "build.sbt")
    assert(abs.getName(0).toString == "bar")
    assert(abs.getName(1).toString == "foo")
  }
  test(".subpath")(assert(abs.subpath(0, 1).toString == "bar"))
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
    assert(file.resolve("foo").resolve("..").normalize().toString == "build.sbt")
    assert(nonNormalizedFile.normalize().toString == PathIO.fromUnix("bin/scalafmt"))
  }
  test(".resolve") {
    assert(!file.resolve("bar").isAbsolute)
    assert(abs.resolve("bar").isAbsolute)
  }
  test(".resolveSibling(Path)") {
    assert(file.resolveSibling("build.scala").toString == "build.scala")
    assert(abs.resolveSibling("foobar") == abs.getParent.resolve("foobar"))
  }
  test(".relativize(Path)")(assert(abs.relativize(abs.resolve("qux")) == Paths.get("qux")))
  test("file.toUri") {
    assert(file.toUri.getPath.endsWith("build.sbt"))
    // NOTE: Paths API seems to work inconsistently under Scala Native.
    // [info] - .toUri *** FAILED ***
    // [info]   "/Users/eburmako/Projects/scalameta/project" did not end with "project/" (NIOPathTest.scala:84)
    // assert(project.toUri.getPath.endsWith("project/"))
  }

  test("jar.toUri") {
    if (scala.meta.internal.platform.isJVM) {
      val jar = Files.createTempDirectory("scalameta").resolve("foo.jar")
      FileIO.withJarFileSystem(AbsolutePath(jar), true, true) { root =>
        val hello = root.resolve("hello")
        Files.createDirectories(hello.toNIO)
        assert(hello.toURI.toString.endsWith("hello"))
      }
    }
  }

  test(".toAbsolutePath") {
    assert(file.toAbsolutePath.endsWith(file))
    // NOTE: Paths API seems to work inconsistently under Scala Native.
    // [info] - .toAbsolutePath *** FAILED ***
    // [info]   /bar/foo did not equal //bar/foo (NIOPathTest.scala:88)
    // assertEquals(abs.toAbsolutePath, abs)
    // NOTE: Paths API seems to work inconsistently under Scala Native.
    // [info] - .toAbsolutePath *** FAILED ***
    // [info]   /Users/eburmako/Projects/scalameta/. did not equal /Users/eburmako/Projects/scalameta (NIOPathTest.scala:92)
    // assertEquals(cwd, Paths.get("").toAbsolutePath)
  }
  test(".toFile") {
    assert(file.toFile.isFile)
    assert(project.toFile.isDirectory)
    assert(cwd.toFile.isDirectory)
  }
}

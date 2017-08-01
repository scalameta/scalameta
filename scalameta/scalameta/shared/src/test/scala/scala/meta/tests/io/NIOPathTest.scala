package scala.meta.tests.io

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import org.scalatest.FunSuite

class NIOPathTest extends FunSuite {

  def file: Path = Paths.get("build.sbt")
  def project: Path = Paths.get("project")
  def abs: Path = Paths.get(File.separator).resolve("bar").resolve("foo")

  test(".isAbsolute") {
    assert(!file.isAbsolute)
    assert(abs.isAbsolute)
  }
  test(".getRoot") {
    assert(file.getRoot == null)
    assert(Paths.get("").toAbsolutePath.getRoot != null)
  }
  test(".getFileName") {
    assert(file.getFileName.toString == "build.sbt")
    assert(abs.getFileName.toString == "foo")
  }
  test(".getParent") {
    assert(abs.getParent.getFileName.toString == "bar")
  }
  test(".getNameCount") {
    assert(abs.getNameCount == 2)
  }
  test(".getName(index)") {
    assert(file.getName(0).toString == "build.sbt")
    assert(abs.getName(0).toString == "bar")
    assert(abs.getName(1).toString == "foo")
  }
  test(".subpath") {
    assert(abs.subpath(0, 1).toString == "bar")
  }
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
  }
  test(".resolve") {
    assert(!file.resolve("bar").isAbsolute)
    assert(abs.resolve("bar").isAbsolute)
  }
  test(".resolveSibling(Path)") {
    assert(file.resolveSibling("build.scala").toString == "build.scala")
    assert(abs.resolveSibling("foobar") == abs.getParent.resolve("foobar"))
  }
  test(".relativize(Path)") {
    assert(abs.relativize(abs.resolve("qux")) == Paths.get("qux"))
  }
  test(".toUri") {
    assert(file.toUri.getPath.endsWith("build.sbt"))
    assert(project.toUri.getPath.endsWith("project/"))
  }
  test(".toAbsolutePath") {
    assert(file.toAbsolutePath.endsWith(file))
    assert(abs.toAbsolutePath == abs)
  }
  test(".toFile") {
    assert(file.toFile.isFile)
    assert(project.toFile.isDirectory)
  }
}

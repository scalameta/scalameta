package scala.meta.tests
package io

import scala.meta._
import scala.meta.internal.io._

import munit.FunSuite

class IOSuite extends FunSuite {
  val buildSbt: AbsolutePath = RelativePath("build.sbt").toAbsolute

  test("PathIO.workingDirectory") {
    val obtained = PathIO.workingDirectory.toString
    // assuming we never run tests from root directory, check that the
    // returned value is not the default value "/" when running outside node.
    assert(obtained != "/")
  }

  test("FileIO.listFiles(Directory)") {
    val obtained = FileIO.listFiles(PathIO.workingDirectory)
    assert(obtained.contains(buildSbt))
    assert(!obtained.contains("."))
  }

  test("FileIO.listFiles(File)")(assert(FileIO.listFiles(buildSbt).isEmpty))

  test("FileIO.listAllFilesRecursively") {
    val bin = PathIO.workingDirectory.resolve("bin")
    val obtained = FileIO.listAllFilesRecursively(bin)
    val scalafmt = bin.resolve("scalafmt")
    assert(obtained.contains(scalafmt), s"$scalafmt not contained in:\n${obtained.mkString("\n")}")
  }

  test("FileIO.readAllBytes") {
    val obtained = new String(FileIO.readAllBytes(buildSbt))
    assert(obtained.contains("project"))
  }

  test("Input.File.slurp") {
    val obtained = new String(Input.File(buildSbt).chars)
    assert(obtained.contains("project"))
  }

  test("AbsolutePath(relpath)(customCwd)") {
    implicit val customWorkingDirectory = AbsolutePath.root
    val obtained = AbsolutePath("foo")
    assertEquals(obtained, customWorkingDirectory.resolve("foo"))
  }

  test("AbsolutePath.exists") {
    assert(buildSbt.exists) // positive: real file
    assert(PathIO.workingDirectory.exists) // positive: a directory also exists
    assert(!buildSbt.resolveSibling(_ + ".does.not.exist").exists) // negative: absent sibling
  }

  test("AbsolutePath.fileName") {
    assert(buildSbt.fileName == "build.sbt")
    assert(AbsolutePath.root.fileName == "") // edge: a filesystem root has no file name
  }

  test("AbsolutePath.parent") {
    assert(buildSbt.parent == PathIO.workingDirectory)
    assert(AbsolutePath.root.parent == AbsolutePath.root) // edge: a root's parent is itself
  }

  test("AbsolutePath.parentOpt") {
    assert(buildSbt.parentOpt == Some(PathIO.workingDirectory)) // positive
    assert(AbsolutePath.root.parentOpt.isEmpty) // negative: a root has no parent
  }

  test("RelativePath.fileName") {
    assert(RelativePath("foo/bar.scala").fileName == "bar.scala")
    assert(RelativePath("").fileName == "") // edge: the empty relative path
  }

  test("RelativePath.parent") {
    assert(RelativePath("foo/bar").parent == RelativePath("foo"))
    assert(RelativePath("foo").parent == RelativePath("")) // edge: single segment
  }

  test("RelativePath.parentOpt") {
    assert(RelativePath("foo/bar").parentOpt == Some(RelativePath("foo"))) // positive
    assert(RelativePath("foo").parentOpt.isEmpty) // negative: single segment has no parent
  }

  test("Classpath.filter") {
    val a = buildSbt
    val b = buildSbt.resolveSibling(_ => "other.sbt")
    val cp = Classpath(List(a, b))
    assert(cp.filter(_ == a) == Classpath(List(a))) // positive: keeps the matching entry
    assert(cp.filter(_ => false) == Classpath(Nil)) // negative: drops everything
  }

}

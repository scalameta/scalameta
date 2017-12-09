package scala.meta.tests
package io

import scala.meta._
import scala.meta.internal.io._
import org.scalatest.FunSuite

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

  test("FileIO.listFiles(File)") {
    assert(FileIO.listFiles(buildSbt).isEmpty)
  }

  test("FileIO.listAllFilesRecursively") {
    val bin = PathIO.workingDirectory.resolve("bin")
    val obtained = FileIO.listAllFilesRecursively(bin)
    val scalafmt = bin.resolve("scalafmt")
    assert(obtained.contains(scalafmt))
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
    assert(obtained == customWorkingDirectory.resolve("foo"))
  }

}

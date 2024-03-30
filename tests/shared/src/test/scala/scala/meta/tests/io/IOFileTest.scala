package scala.meta.tests
package io

import scala.meta.internal.io._

import java.io.File

import munit.FunSuite

class IOFileTest extends FunSuite {
  val file = new File("build.sbt")
  val project = new File("project")
  val nestedFile = new File("project", "build.properties")
  val nonNormalizedFile = new File(new File(new File(project, ".."), "bin"), "scalafmt")

  test(".toString") {
    assertEquals(file.toString, "build.sbt")
    assertEquals(project.toString, "project")
    assertEquals(nestedFile.toString, PathIO.fromUnix("project/build.properties"))
    assertEquals(nonNormalizedFile.toString, PathIO.fromUnix("project/../bin/scalafmt"))
  }

  test(".isFile") {
    assert(file.isFile)
    assert(!project.isFile)
    assert(nestedFile.isFile)
  }

  test(".isDirectory") {
    assert(!file.isDirectory)
    assert(project.isDirectory)
  }

  test(".getAbsolutePath") {
    assert(file.getAbsolutePath.endsWith("build.sbt"))
    assert(project.getAbsolutePath.endsWith("project"))
  }

  test(".getPath") {
    assertEquals(file.getPath, "build.sbt")
    assertEquals(project.getPath, "project")
  }

  test(".toPath") {
    assert(file.toPath.endsWith("build.sbt"))
    assert(project.toPath.endsWith("project"))
  }

  test(".exists") {
    assert(file.exists())
    assert(project.exists())
  }

  test(".toURI") {
    // NOTE: File.toURI is not available on Scala Native
//    assert(file.toURI.getPath.endsWith("build.sbt"))
//    assert(project.toURI.getPath.endsWith("project/"))
  }

  test("File.pathSeparator") {
    val obtained = File.pathSeparator
    assert(obtained == ":" || obtained == ";")
  }

  test("File.fileSeparator") {
    val obtained = File.separator
    assert(obtained == "/" || obtained == "\\")
  }

}

package scala.meta.tests
package io

import java.io.File
import org.scalatest.FunSuite
import scala.meta.internal.io._

class IOFileTest extends FunSuite {
  val file = new File("build.sbt")
  val project = new File("project")
  val nestedFile = new File("project", "build.properties")
  val nonNormalizedFile = new File(new File(new File(project, ".."), "bin"), "scalafmt")

  test(".toString") {
    assert(file.toString == "build.sbt")
    assert(project.toString == "project")
    assert(nestedFile.toString == PathIO.fromUnix("project/build.properties"))
    assert(nonNormalizedFile.toString == PathIO.fromUnix("project/../bin/scalafmt"))
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
    assert(file.getPath == "build.sbt")
    assert(project.getPath == "project")
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

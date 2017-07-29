package scala.meta.tests.io

import java.io.File
import org.scalatest.FunSuite

class IOFileTest extends FunSuite {
  val file = new File("build.sbt")
  val project = new File("project")
  val nestedFile = new File("project", "build.properties")

  test(".toString") {
    assert(file.toString == "build.sbt")
    assert(project.toString == "project")
    assert(nestedFile.toString == "project/build.properties"
      || nestedFile.toString == "project\\build.properties")
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
    assert(file.toURI.getPath.endsWith("build.sbt"))
    assert(project.toURI.getPath.endsWith("project/"))
  }

}

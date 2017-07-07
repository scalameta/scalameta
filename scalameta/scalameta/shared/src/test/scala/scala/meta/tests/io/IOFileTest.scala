package scala.meta.tests.io

import java.io.File
import org.scalatest.FunSuite

class IOFileTest extends FunSuite {
  val file = new File("build.sbt")
  val target = new File("target")

  test(".toString") {
    assert(file.toString == "build.sbt")
    assert(target.toString == "target")
  }

  test(".isFile") {
    assert(file.isFile)
  }

  test(".isDirectory") {
    assert(target.isDirectory)
  }

  test(".getAbsolutePath") {
    assert(file.getAbsolutePath.endsWith("build.sbt"))
    assert(target.getAbsolutePath.endsWith("target"))
  }

  test(".getPath") {
    assert(file.getPath == "build.sbt")
    assert(target.getPath == "target")
  }

  test(".toPath") {
    assert(file.toPath.endsWith("build.sbt"))
    assert(target.toPath.endsWith("target"))
  }

  test(".exists") {
    assert(file.exists())
    assert(target.exists())
  }

  test(".toURI") {
    assert(file.toURI.getPath.endsWith("build.sbt"))
    assert(target.toURI.getPath.endsWith("target/"))
  }

}

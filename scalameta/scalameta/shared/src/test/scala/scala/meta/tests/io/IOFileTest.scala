package scala.meta.tests.io

import java.io.File
import org.scalatest.FunSuite

class IOFileTest extends FunSuite {
  val file = new File("build.sbt")
  val target = new File("target")
  val streams = new File("target", "streams")

  test(".toString") {
    assert(file.toString == "build.sbt")
    assert(target.toString == "target")
    assert(streams.toString == "target/streams" || streams.toString == "target\\streams")
  }

  test(".isFile") {
    assert(file.isFile)
    assert(!streams.isFile)
  }

  test(".isDirectory") {
    assert(!file.isDirectory)
    assert(target.isDirectory)
    assert(streams.isDirectory)
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

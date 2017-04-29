package scala.meta.tests.io

import scala.meta.inputs.Input
import scala.meta.internal.io.PathIO
import scala.meta.io.RelativePath

import org.scalatest.FunSuite

class IOSuite extends FunSuite {
  test("PathIO.workingDirectory") {
    val obtained = PathIO.workingDirectory.toString
    // assuming we never run tests from root directory, check that the
    // returned value is not the default value "/" when running outside node.
    assert(obtained != "/")
  }

  test("PathIO.pathSeparator") {
    val obtained = PathIO.pathSeparator
    assert(obtained == ":" || obtained == ";")
  }

  test("PathIO.fileSeparator") {
    val obtained = PathIO.fileSeparator
    assert(obtained == "/" || obtained == "\\")
  }

  test("PathIO.isAbsolute") {
    val obtained = PathIO.isAbsolutePath(PathIO.workingDirectory.toString)
    assert(obtained)
  }

  test("Input.File.slurp") {
    // NOTE: reading file contents of a relative path is not ideal.
    val obtained = new String(Input.File(RelativePath("build.sbt").toAbsolute).chars)
    assert(obtained.contains("project"))
  }

}

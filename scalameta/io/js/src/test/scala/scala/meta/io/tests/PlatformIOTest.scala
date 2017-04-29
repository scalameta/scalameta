package scala.meta.io
package tests

import scala.meta.internal.io.PlatformIO

import org.scalatest.FunSuite

class PlatformIOTest extends FunSuite {
  test("workingDirectory") {
    val obtained = PlatformIO.workingDirectory.absolute
    // assuming we never run tests from root directory, check that the
    // returned value is not the default value "/" when running outside node.
    assert(obtained != "/")
  }

  test("slurp") {
    // NOTE: reading a relative path in tests is not hermetic.
    val obtained = PlatformIO.slurp(AbsolutePath.fromRelative("build.sbt"))
    assert(obtained.contains("project"))
  }

  test("pathSeparator") {
    val obtained = PlatformIO.pathSeparator
    assert(obtained == ":" || obtained == ";")
  }

  test("fileSeparator") {
    val obtained = PlatformIO.fileSeparator
    assert(obtained == "/" || obtained == "\\")
  }

  test("isAbsolute") {
    val obtained = PlatformIO.isAbsolutePath(PlatformIO.workingDirectory.absolute)
    assert(obtained)
  }
}

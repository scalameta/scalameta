package scala.meta.tests.io

import scala.meta.internal.io.PathIO

import munit.FunSuite

class PathIOSuite extends FunSuite {
  def check(path: String, expectedDir: String, expectedName: String): Unit = test(path) {
    val obtainedDir = PathIO.dirname(path)
    assertEquals(obtainedDir, expectedDir, "Unexpected dirName")

    val obtainedName = PathIO.basename(path)
    assertEquals(obtainedName, expectedName, "Unexpected baseName")
  }

  check("/", "/", "")
  check("//", "/", "")
  check("///", "//", "")

  check("/a", "/", "a")
  check("/a/", "/", "a")

  check("/a/b", "/a/", "b")
  check("/a/b/", "/a/", "b")

  // relative paths are treated as if they are absolute paths
  check("a", "/", "a")
  check("a/", "/", "a")
  check("a/b", "a/", "b")
  check("a/b/", "a/", "b")
}

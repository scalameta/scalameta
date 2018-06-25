package scala.meta.tests.io

import org.scalatest.FunSuite
import scala.meta.internal.io.PathIO

class PathIOSuite extends FunSuite {
  def check(path: String, expectedDir: String, expectedName: String): Unit =
    test(path) {
      val obtainedDir = PathIO.dirname(path)
      assert(obtainedDir == expectedDir, "Unexpected dirName")

      val obtainedName = PathIO.basename(path)
      assert(obtainedName == expectedName, "Unexpected baseName")
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

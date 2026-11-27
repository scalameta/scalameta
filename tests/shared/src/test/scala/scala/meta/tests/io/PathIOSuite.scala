package scala.meta.tests.io

import scala.meta.internal.io.PathIO

import java.io.File
import java.nio.file.Paths

import munit.FunSuite

class PathIOSuite extends FunSuite {

  def check(path: String, expectedDir: String, expectedName: String)(implicit
      loc: munit.Location,
  ): Unit = test(path) {
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

  test("PathIO.isWindows")(assertEquals(PathIO.isWindows, File.pathSeparator == ";"))

  test("PathIO.toUnix") {
    assertEquals(PathIO.toUnix(s"a${File.separator}b"), "a/b")
    assertEquals(PathIO.toUnix("a/b"), "a/b")
  }

  test("PathIO.fromUnix")(assertEquals(PathIO.fromUnix("a/b"), s"a${File.separator}b"))

  def checkExtension(name: String, expected: String)(implicit loc: munit.Location): Unit =
    test(s"PathIO.fileExtension: $name") {
      assertEquals(PathIO.fileExtension(name), expected, "Unexpected extension of name")
      assertEquals(
        PathIO.fileExtension(Paths.get("a", name)),
        expected,
        "Unexpected extension of path",
      )
    }

  checkExtension("foo.jar", ".jar")
  checkExtension("Foo.class", ".class")
  checkExtension("foo.tar.gz", ".gz")
  checkExtension("foo", "")
  checkExtension("foo.", ".")
  checkExtension(".bashrc", ".bashrc")

}

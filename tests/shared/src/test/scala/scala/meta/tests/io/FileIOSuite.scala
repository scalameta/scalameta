package scala.meta.tests
package io

import scala.meta.internal.io._
import scala.meta.internal.platform.{isJS, isNative}
import scala.meta.io._

import java.io.File
import java.nio.charset.{Charset, StandardCharsets}

import scala.util.{Failure, Success, Try}

import munit.FunSuite

class FileIOSuite extends FunSuite {

  private val tmp = FunFixture[AbsolutePath](_ => TestTree.mkdtemp(), TestTree.delete)

  private val treeEntries = Seq("file", "dir", "link-file", "link-dir", "link-broken", "missing")

  private implicit val stringOutput: OutputStreamIO[String] =
    (text, os) => os.write(text.getBytes(StandardCharsets.UTF_8))

  private def assumePosix(): Unit = assume(File.separatorChar == '/', "needs links and permissions")

  private def assumePermissions(): Unit = {
    assumePosix()
    assume(!TestTree.ignoresPermissions, "a directory with mode 000 stays readable")
  }

  private def mkTree(root: AbsolutePath): Unit = {
    TestTree.writeBytes(root.resolve("file"), Array[Byte]('a'))
    TestTree.mkdir(root.resolve("dir"))
    TestTree.symlink(root.resolve("link-file"), root.resolve("file"))
    TestTree.symlink(root.resolve("link-dir"), root.resolve("dir"))
    TestTree.symlink(root.resolve("link-broken"), root.resolve("missing"))
  }

  /** Returns a line per entry of `treeEntries`, with the names of the checks that hold for it. */
  private def checks(root: AbsolutePath)(checks: (String, AbsolutePath => Boolean)*): String = {
    val sb = new StringBuilder
    treeEntries.foreach { name =>
      val path = root.resolve(name)
      sb.append(name).append(':')
      checks.foreach { case (check, holds) => if (holds(path)) sb.append(' ').append(check) }
      sb.append('\n')
    }
    sb.toString
  }

  /** Returns the value as a string, or the exception, with `<root>` in place of the root. */
  private def show(root: AbsolutePath)(value: => Any): String = {
    val res = Try(value) match {
      case s: Success[Any] => s.value.toString
      case f: Failure[Any] => f.exception.toString
    }
    res.replace(root.toString, "<root>")
  }

  /** Returns the sorted paths that `listAllFilesRecursively` lists, or the exception. */
  private def listAll(root: AbsolutePath, dir: String): String = show(root)(
    FileIO.listAllFilesRecursively(root.resolve(dir)).map(_.toString).sorted.mkString("\n"),
  )

  private def slurp(root: AbsolutePath, bytes: Array[Byte], charset: Charset): String = {
    val path = root.resolve("a.txt")
    TestTree.writeBytes(path, bytes)
    show(root)(FileIO.slurp(path, charset))
  }

  private def write(root: AbsolutePath, file: String): String = {
    val path = root.resolve(file)
    show(root) {
      FileIO.write(path, "text")
      FileIO.slurp(path, StandardCharsets.UTF_8)
    }
  }

  private def withMode[A](dir: AbsolutePath, permissions: String)(body: => A): A = {
    TestTree.chmod(dir, permissions)
    try body
    finally TestTree.chmod(dir, "rwx------")
  }

  tmp.test("AbsolutePath checks on a tree") { root =>
    assumePosix()
    mkTree(root)
    val obtained =
      checks(root)("exists" -> (_.exists), "isFile" -> (_.isFile), "isDirectory" -> (_.isDirectory))
    val expected =
      if (isJS)
        """|file: exists isFile
           |dir: exists isDirectory
           |link-file: exists
           |link-dir: exists
           |link-broken:
           |missing:
           |""".stripMargin
      else
        """|file: exists isFile
           |dir: exists isDirectory
           |link-file: exists isFile
           |link-dir: exists isDirectory
           |link-broken:
           |missing:
           |""".stripMargin
    assertNoDiff(obtained, expected)
  }

  tmp.test("java.io.File checks on a tree") { root =>
    assumePosix()
    mkTree(root)
    val obtained = checks(root)(
      "exists" -> (_.toFile.exists()),
      "isFile" -> (_.toFile.isFile),
      "isDirectory" -> (_.toFile.isDirectory),
    )
    val expected =
      if (isJS)
        """|file: exists isFile
           |dir: exists isDirectory
           |link-file: exists
           |link-dir: exists
           |link-broken:
           |missing:
           |""".stripMargin
      else
        """|file: exists isFile
           |dir: exists isDirectory
           |link-file: exists isFile
           |link-dir: exists isDirectory
           |link-broken:
           |missing:
           |""".stripMargin
    assertNoDiff(obtained, expected)
  }

  tmp.test("FileIO.slurp of the byte 0xFF in UTF-8") { root =>
    val expected =
      if (isNative) "java.nio.charset.MalformedInputException: Input length = 1" else "�"
    assertNoDiff(slurp(root, Array[Byte](-1), StandardCharsets.UTF_8), expected)
  }

  tmp.test("FileIO.slurp of the byte 0xE9 in ISO-8859-1") { root =>
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: TypeError [ERR_INVALID_ARG_VALUE]: " +
        "The argument 'encoding' is invalid encoding. Received 'ISO-8859-1'"
      else "é"
    assertNoDiff(slurp(root, Array[Byte](0xe9.toByte), StandardCharsets.ISO_8859_1), expected)
  }

  tmp.test("FileIO.write into an existing directory") { root =>
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: EEXIST: file already exists, mkdir '<root>'"
      else "text"
    assertNoDiff(write(root, "a.txt"), expected)
  }

  tmp.test("FileIO.write into a missing directory") { root =>
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: TypeError [ERR_INVALID_ARG_TYPE]: " +
        "The \"data\" argument must be of type string or an instance of Buffer, TypedArray, " +
        "or DataView. Received an instance of Array"
      else "text"
    assertNoDiff(write(root, "b/a.txt"), expected)
  }

  tmp.test("FileIO.write into a missing directory of a missing directory") { root =>
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: ENOENT: no such file or directory, mkdir '<root>/b/c'"
      else "text"
    assertNoDiff(write(root, "b/c/a.txt"), PathIO.fromUnix(expected))
  }

  tmp.test("FileIO.listAllFilesRecursively of a link to a file") { root =>
    assumePosix()
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    TestTree.symlink(root.resolve("link"), root.resolve("a.txt"))
    assertNoDiff(
      listAll(root, ""),
      """|<root>/a.txt
         |<root>/link
         |""".stripMargin,
    )
  }

  tmp.test("FileIO.listAllFilesRecursively of a link to a directory") { root =>
    assumePosix()
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/a.txt"), Array[Byte]('a'))
    TestTree.symlink(root.resolve("link"), root.resolve("dir"))
    val expected =
      if (isJS)
        """|<root>/dir/a.txt
           |<root>/link
           |""".stripMargin
      else
        """|<root>/dir/a.txt
           |<root>/link/a.txt
           |""".stripMargin
    assertNoDiff(listAll(root, ""), expected)
  }

  tmp.test("FileIO.listAllFilesRecursively of a broken link") { root =>
    assumePosix()
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    TestTree.symlink(root.resolve("link"), root.resolve("missing"))
    val expected =
      if (isJS || isNative)
        """|<root>/a.txt
           |<root>/link
           |""".stripMargin
      else "<root>/a.txt"
    assertNoDiff(listAll(root, ""), expected)
  }

  tmp.test("FileIO.listAllFilesRecursively of a missing root") { root =>
    val expected =
      if (isJS || isNative) "<root>/missing"
      else "java.nio.file.NoSuchFileException: <root>/missing"
    assertNoDiff(listAll(root, "missing"), PathIO.fromUnix(expected))
  }

  tmp.test("FileIO.listAllFilesRecursively of a root that is a file") { root =>
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    assertNoDiff(listAll(root, "a.txt"), PathIO.fromUnix("<root>/a.txt"))
  }

  tmp.test("FileIO.listAllFilesRecursively of a directory with mode 000") { root =>
    assumePermissions()
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/b.txt"), Array[Byte]('b'))
    val obtained = withMode(root.resolve("dir"), "---------")(listAll(root, ""))
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: EACCES: permission denied, scandir '<root>/dir'"
      else "<root>/a.txt"
    assertNoDiff(obtained, expected)
  }

  tmp.test("FileIO.listAllFilesRecursively of a directory with mode 0444") { root =>
    assumePermissions()
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/b.txt"), Array[Byte]('b'))
    val obtained = withMode(root.resolve("dir"), "r--r--r--")(listAll(root, ""))
    val expected =
      if (isJS || isNative)
        """|<root>/a.txt
           |<root>/dir/b.txt
           |""".stripMargin
      else "<root>/a.txt"
    assertNoDiff(obtained, expected)
  }

  tmp.test("FileIO.listAllFilesRecursively of nested directories") { root =>
    Seq("a", "c").foreach(dir => TestTree.mkdir(root.resolve(dir)))
    Seq("a/x", "a/y", "b", "c/z").foreach(file => TestTree.writeBytes(root.resolve(file), Array()))
    // some file systems sort the entries of readdir, and others do not
    val expected = TestTree.readdir(root).flatMap(name =>
      if (name == "b") List(name) else TestTree.readdir(root.resolve(name)).map(s"$name/" + _),
    )
    val obtained = FileIO.listAllFilesRecursively(root).files.map(f => PathIO.toUnix(f.toString))
    if (isJS) assertEquals(obtained, expected) else assertEquals(obtained.toSet, expected.toSet)
  }

}

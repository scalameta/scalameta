package scala.meta.tests
package io

import scala.meta.internal.io.Implicits._
import scala.meta.internal.io.PlatformFlags.{isJS, isNative}
import scala.meta.internal.io._
import scala.meta.io._

import java.io.File
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path

import scala.util.{Failure, Random, Success, Try}

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

  /** Returns the sorted paths that `walkFiles` returns, or the exception. */
  private def walk(root: AbsolutePath, path: AbsolutePath, visitor: FileIO.WalkVisitor): String =
    show(root)(FileIO.walkFiles(visitor)(path.toNIO).map(_.toString).sorted.mkString("\n"))

  /** Returns the sorted paths that `findFiles` returns, or the exception. */
  private def find(root: AbsolutePath, path: AbsolutePath): String =
    show(root)(FileIO.findFiles(path.toNIO).map(_.toString).sorted.mkString("\n"))

  private def withMode[A](dir: AbsolutePath, permissions: String)(body: => A): A = {
    TestTree.chmod(dir, permissions)
    try body
    finally TestTree.chmod(dir, "rwx------")
  }

  /** Records the calls of a walk. */
  private class Recorder(root: AbsolutePath, failStop: Boolean = false) extends FileIO.WalkVisitor {
    private val sb = new StringBuilder
    def calls: String = sb.toString
    private def record(call: String, path: Path): Unit = sb.append(call).append(' ')
      .append(path.toString.replace(root.toString, "<root>")).append('\n')
    override def onTree(dir: Path, fileStat: FileStat): FileIO.WalkVisit = {
      record("onTree", dir)
      FileIO.WalkVisit.Good
    }
    override def onFile(file: Path, fileStat: FileStat): FileIO.WalkVisit = {
      record("onFile", file)
      FileIO.WalkVisit.Good
    }
    override def onFailStop(file: Path, exc: Throwable): Boolean = {
      record("onFailStop", file)
      failStop
    }
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

  tmp.test("FileIO checks on a tree") { root =>
    assumePosix()
    mkTree(root)
    val obtained = checks(root)(
      "exists" -> (p => FileIO.exists(p.toNIO)),
      "isDirectory" -> (p => FileIO.isDirectory(p.toNIO)),
      "isRegularFile" -> (p => FileIO.isRegularFile(p.toNIO)),
      "isRegularFileNoLinks" -> (p => FileIO.isRegularFileNoLinks(p.toNIO)),
    )
    val expected =
      if (isJS)
        """|file: exists isRegularFile isRegularFileNoLinks
           |dir: exists isDirectory
           |link-file: exists isRegularFile
           |link-dir: exists
           |link-broken:
           |missing:
           |""".stripMargin
      else
        """|file: exists isRegularFile isRegularFileNoLinks
           |dir: exists isDirectory
           |link-file: exists isRegularFile
           |link-dir: exists isDirectory
           |link-broken:
           |missing:
           |""".stripMargin
    assertNoDiff(obtained, expected)
  }

  tmp.test("FileIO.getFileStat on a tree, following links") { root =>
    assumePosix()
    mkTree(root)
    def stat(path: AbsolutePath) = FileIO.getFileStat(path.toNIO, followLinks = true)
    val obtained = checks(root)(
      "stat" -> (stat(_).isDefined),
      "isDirectory" -> (stat(_).exists(_.isDirectory)),
      "isRegularFile" -> (stat(_).exists(_.isRegularFile)),
      "isSymlink" -> (stat(_).exists(_.isSymlink)),
    )
    assertNoDiff(
      obtained,
      """|file: stat isRegularFile
         |dir: stat isDirectory
         |link-file: stat isRegularFile
         |link-dir: stat isDirectory
         |link-broken:
         |missing:
         |""".stripMargin,
    )
  }

  tmp.test("FileIO.getFileStat on a tree, without following links") { root =>
    assumePosix()
    mkTree(root)
    def stat(path: AbsolutePath) = FileIO.getFileStat(path.toNIO, followLinks = false)
    val obtained = checks(root)(
      "stat" -> (stat(_).isDefined),
      "isDirectory" -> (stat(_).exists(_.isDirectory)),
      "isRegularFile" -> (stat(_).exists(_.isRegularFile)),
      "isSymlink" -> (stat(_).exists(_.isSymlink)),
    )
    assertNoDiff(
      obtained,
      """|file: stat isRegularFile
         |dir: stat isDirectory
         |link-file: stat isSymlink
         |link-dir: stat isSymlink
         |link-broken: stat isSymlink
         |missing:
         |""".stripMargin,
    )
  }

  tmp.test("FileIO.mkdir") { root =>
    FileIO.mkdir(root.resolve("a").toNIO)
    assertEquals(root.resolve("a").isDirectory, true)
  }

  tmp.test("FileIO.mkdir in a missing directory") { root =>
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: ENOENT: no such file or directory, mkdir '<root>/a/b'"
      else if (isNative) "java.io.IOException"
      else "java.nio.file.NoSuchFileException: <root>/a/b"
    assertNoDiff(show(root)(FileIO.mkdir(root.resolve("a/b").toNIO)), PathIO.fromUnix(expected))
  }

  tmp.test("FileIO.mkdirs") { root =>
    FileIO.mkdirs(root.resolve("a/b").toNIO)
    assertEquals(root.resolve("a/b").isDirectory, true)
  }

  test("FileIO.mkdtemp") {
    val dir = AbsolutePath(FileIO.mkdtemp("scalameta-"))
    try {
      assertEquals(dir.isDirectory, true)
      assert(dir.fileName.startsWith("scalameta-"), dir)
    } finally TestTree.delete(dir)
  }

  tmp.test("FileIO.move over an existing file") { root =>
    val src = root.resolve("a.txt").toNIO
    val dst = root.resolve("b.txt").toNIO
    FileIO.writeFile(src, "a")(StandardCharsets.UTF_8)
    FileIO.writeFile(dst, "b")(StandardCharsets.UTF_8)
    FileIO.move(src, dst)
    assertEquals(FileIO.exists(src), false)
    assertEquals(FileIO.readFile(dst)(StandardCharsets.UTF_8), "a")
  }

  tmp.test("FileIO.delete") { root =>
    val path = root.resolve("a.txt").toNIO
    FileIO.writeFile(path, "a")(StandardCharsets.UTF_8)
    FileIO.delete(path)
    assertEquals(FileIO.exists(path), false)
  }

  tmp.test("FileIO.symlink") { root =>
    assumePosix()
    val link = root.resolve("link").toNIO
    FileIO.writeFile(root.resolve("a.txt").toNIO, "a")(StandardCharsets.UTF_8)
    FileIO.symlink(link, root.resolve("a.txt").toNIO)
    assertEquals(FileIO.getFileStat(link, followLinks = false).map(_.isSymlink), Some(true))
    assertEquals(FileIO.readFile(link)(StandardCharsets.UTF_8), "a")
  }

  tmp.test("FileIO.writeFile and readFile in UTF-8") { root =>
    val path = root.resolve("a.txt").toNIO
    FileIO.writeFile(path, "aé")(StandardCharsets.UTF_8)
    assertEquals(FileIO.readAllBytes(AbsolutePath(path)).toList, "aé".getBytes("UTF-8").toList)
    assertEquals(FileIO.readFile(path)(StandardCharsets.UTF_8), "aé")
  }

  tmp.test("FileIO.readFile of the byte 0xFF in UTF-8") { root =>
    val path = root.resolve("a.txt")
    TestTree.writeBytes(path, Array[Byte](-1))
    assertEquals(FileIO.readFile(path.toNIO)(StandardCharsets.UTF_8), "�")
  }

  tmp.test("FileIO.writeFile in ISO-8859-1") { root =>
    val path = root.resolve("a.txt")
    val obtained = show(root) {
      FileIO.writeFile(path.toNIO, "é")(StandardCharsets.ISO_8859_1)
      FileIO.readAllBytes(path).toList
    }
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: TypeError [ERR_INVALID_ARG_VALUE]: " +
        "The argument 'encoding' is invalid encoding. Received 'ISO-8859-1'"
      else "List(-23)"
    assertNoDiff(obtained, expected)
    assertEquals(path.exists, !isJS)
  }

  tmp.test("FileIO.readFile of the byte 0xE9 in ISO-8859-1") { root =>
    val path = root.resolve("a.txt")
    TestTree.writeBytes(path, Array[Byte](0xe9.toByte))
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: TypeError [ERR_INVALID_ARG_VALUE]: " +
        "The argument 'encoding' is invalid encoding. Received 'ISO-8859-1'"
      else "é"
    assertNoDiff(show(root)(FileIO.readFile(path.toNIO)(StandardCharsets.ISO_8859_1)), expected)
  }

  tmp.test("FileIO.walkFiles with Skip at the root") { root =>
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    val visitor = new FileIO.WalkVisitor {
      override def onTree(dir: Path, fileStat: FileStat): FileIO.WalkVisit = FileIO.WalkVisit.Skip
    }
    assertNoDiff(walk(root, root, visitor), "")
  }

  tmp.test("FileIO.walkFiles with Skip at a subdirectory") { root =>
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/b.txt"), Array[Byte]('b'))
    val visitor = new FileIO.WalkVisitor {
      override def onTree(dir: Path, fileStat: FileStat): FileIO.WalkVisit =
        if (dir.endsWith("dir")) FileIO.WalkVisit.Skip else FileIO.WalkVisit.Good
    }
    assertNoDiff(walk(root, root, visitor), PathIO.fromUnix("<root>/a.txt"))
  }

  tmp.test("FileIO.walkFiles with Stop at a subdirectory") { root =>
    Seq("a", "b").foreach { dir =>
      TestTree.mkdir(root.resolve(dir))
      TestTree.writeBytes(root.resolve(s"$dir/x.txt"), Array[Byte]('x'))
    }
    var visits = 0
    val visitor = new FileIO.WalkVisitor {
      override def onTree(dir: Path, fileStat: FileStat): FileIO.WalkVisit = {
        visits += 1
        if (dir == root.toNIO) FileIO.WalkVisit.Good else FileIO.WalkVisit.Stop
      }
      override def onFile(file: Path, fileStat: FileStat): FileIO.WalkVisit = {
        visits += 1
        FileIO.WalkVisit.Good
      }
    }
    assertNoDiff(walk(root, root, visitor), "")
    assertEquals(visits, 2)
  }

  tmp.test("FileIO.walkFiles of a missing root") { root =>
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: ENOENT: no such file or directory, lstat '<root>/missing'"
      else "java.nio.file.NoSuchFileException: <root>/missing"
    assertNoDiff(
      walk(root, root.resolve("missing"), FileIO.WalkVisitor.empty),
      PathIO.fromUnix(expected),
    )
    assertNoDiff(walk(root, root.resolve("missing"), new Recorder(root)), "")
  }

  tmp.test("FileIO.walkFiles of a chain of directories") { root =>
    TestTree.mkdir(root.resolve("a"))
    TestTree.mkdir(root.resolve("a/b"))
    TestTree.writeBytes(root.resolve("a/b/c.txt"), Array[Byte]('c'))
    val recorder = new Recorder(root)
    assertNoDiff(walk(root, root, recorder), PathIO.fromUnix("<root>/a/b/c.txt"))
    assertNoDiff(
      recorder.calls,
      PathIO.fromUnix(
        """|onTree <root>
           |onTree <root>/a
           |onTree <root>/a/b
           |onFile <root>/a/b/c.txt
           |""".stripMargin,
      ),
    )
  }

  tmp.test("FileIO.walkFiles of nested directories") { root =>
    Seq("a", "c").foreach(dir => TestTree.mkdir(root.resolve(dir)))
    Seq("a/x", "a/y", "b", "c/z").foreach(file => TestTree.writeBytes(root.resolve(file), Array()))
    // some file systems sort the entries of readdir, and others do not
    val expected = TestTree.readdir(root).flatMap(name =>
      if (name == "b") List(name) else TestTree.readdir(root.resolve(name)).map(s"$name/" + _),
    )
    val obtained = FileIO.walkFiles(FileIO.WalkVisitor.empty)(root.toNIO)
      .map(f => PathIO.toUnix(root.toNIO.relativize(f).toString))
    if (isJS) assertEquals(obtained, expected) else assertEquals(obtained.toSet, expected.toSet)
  }

  tmp.test("FileIO.walkFiles of a directory with mode 000") { root =>
    assumePermissions()
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/b.txt"), Array[Byte]('b'))
    val failed =
      withMode(root.resolve("dir"), "---------")(walk(root, root, FileIO.WalkVisitor.empty))
    val skipped = withMode(root.resolve("dir"), "---------")(walk(root, root, new Recorder(root)))
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: EACCES: permission denied, scandir '<root>/dir'"
      else "java.nio.file.AccessDeniedException: <root>/dir"
    assertNoDiff(failed, expected)
    assertNoDiff(skipped, "<root>/a.txt")
  }

  tmp.test("FileIO.walkFiles of a directory with mode 0444") { root =>
    assumePermissions()
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/b.txt"), Array[Byte]('b'))
    val failed =
      withMode(root.resolve("dir"), "r--r--r--")(walk(root, root, FileIO.WalkVisitor.empty))
    val skipped = withMode(root.resolve("dir"), "r--r--r--")(walk(root, root, new Recorder(root)))
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: EACCES: permission denied, lstat '<root>/dir/b.txt'"
      else "java.nio.file.AccessDeniedException: <root>/dir/b.txt"
    assertNoDiff(failed, expected)
    assertNoDiff(skipped, "<root>/a.txt")
  }

  tmp.test("FileIO.walkFiles of a directory with mode 0444, stopping on failure") { root =>
    assumePermissions()
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/b.txt"), Array[Byte]('b'))
    TestTree.writeBytes(root.resolve("dir/c.txt"), Array[Byte]('c'))
    val recorder = new Recorder(root, failStop = true)
    val walked = withMode(root.resolve("dir"), "r--r--r--")(walk(root, root, recorder))
    assertNoDiff(walked, "")
    val failures = recorder.calls.linesIterator.count(_.startsWith("onFailStop"))
    assertEquals(failures, if (isNative) 2 else 1)
  }

  tmp.test("FileIO.findFiles and walkFiles of a growing tree") { root =>
    val path = root.toNIO
    def subpath(path: Path): Path = path.resolve(Random.alphanumeric.take(10).mkString)
    assertEquals(FileIO.findFiles(path), Nil)
    assertEquals(FileIO.walkFiles(FileIO.WalkVisitor.empty)(path), Nil)

    val subfile = subpath(path)
    FileIO.writeFile(subfile, "file")(StandardCharsets.UTF_8)
    assertEquals(FileIO.findFiles(path), Seq(subfile))
    assertEquals(FileIO.findFiles(subfile), Seq(subfile))
    assertEquals(FileIO.walkFiles(FileIO.WalkVisitor.empty)(path), Seq(subfile))
    assertEquals(FileIO.walkFiles(FileIO.WalkVisitor.empty)(subfile), Seq(subfile))
    assertEquals(
      FileIO.walkFiles(new FileIO.WalkVisitor {
        override def onTree(dir: Path, fileStat: FileStat): FileIO.WalkVisit =
          if (dir != path) FileIO.WalkVisit.Good else FileIO.WalkVisit.Skip
      })(path),
      Nil,
    )

    val subdir = subpath(path)
    FileIO.mkdir(subdir)
    assertEquals(FileIO.findFiles(path), Seq(subfile))
    assertEquals(FileIO.findFiles(subdir), Nil)

    val subsubfile = subpath(subdir)
    FileIO.writeFile(subsubfile, "file")(StandardCharsets.UTF_8)
    assertEquals(FileIO.findFiles(path).toSet, Set(subfile, subsubfile))
    assertEquals(FileIO.findFiles(subdir), Seq(subsubfile))
    assertEquals(FileIO.findFiles(subsubfile), Seq(subsubfile))
    assertEquals(
      FileIO.walkFiles(new FileIO.WalkVisitor {
        override def onTree(dir: Path, fileStat: FileStat): FileIO.WalkVisit =
          if (dir.startsWith(subdir)) FileIO.WalkVisit.Skip else FileIO.WalkVisit.Good
      })(path),
      Seq(subfile),
    )
  }

  tmp.test("FileIO.findFiles of links") { root =>
    assumePosix()
    mkTree(root)
    assertNoDiff(find(root, root), "<root>/file")
  }

  tmp.test("FileIO.findFiles of a missing root") { root =>
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: ENOENT: no such file or directory, lstat '<root>/missing'"
      else "java.nio.file.NoSuchFileException: <root>/missing"
    assertNoDiff(find(root, root.resolve("missing")), PathIO.fromUnix(expected))
  }

  tmp.test("FileIO.findFiles of a directory with mode 000") { root =>
    assumePermissions()
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/b.txt"), Array[Byte]('b'))
    val obtained = withMode(root.resolve("dir"), "---------")(find(root, root))
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: EACCES: permission denied, scandir '<root>/dir'"
      else if (isNative) "<root>/a.txt"
      else "java.io.UncheckedIOException: java.nio.file.AccessDeniedException: <root>/dir"
    assertNoDiff(obtained, expected)
  }

  tmp.test("FileIO.findFiles of a root with mode 000") { root =>
    assumePermissions()
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/b.txt"), Array[Byte]('b'))
    val obtained = withMode(root.resolve("dir"), "---------")(find(root, root.resolve("dir")))
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: EACCES: permission denied, scandir '<root>/dir'"
      else if (isNative) ""
      else "java.nio.file.AccessDeniedException: <root>/dir"
    assertNoDiff(obtained, expected)
  }

  tmp.test("FileIO.findFiles of a directory with mode 0444") { root =>
    assumePermissions()
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/b.txt"), Array[Byte]('b'))
    val obtained = withMode(root.resolve("dir"), "r--r--r--")(find(root, root))
    val expected =
      if (isJS) "scala.scalajs.js.JavaScriptException: " +
        "Error: EACCES: permission denied, lstat '<root>/dir/b.txt'"
      else if (isNative) "java.io.UncheckedIOException: <root>/dir/b.txt"
      else "java.io.UncheckedIOException: java.nio.file.AccessDeniedException: <root>/dir/b.txt"
    assertNoDiff(obtained, expected)
  }

  tmp.test("Path.exists") { root =>
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    Seq("a.txt", "missing").map(root.resolve(_).toNIO)
      .foreach(path => assertEquals(path.exists, FileIO.exists(path), path))
  }

  tmp.test("Path.isDirectory") { root =>
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    Seq("", "a.txt").map(root.resolve(_).toNIO)
      .foreach(path => assertEquals(path.isDirectory, FileIO.isDirectory(path), path))
  }

  tmp.test("Path.isRegularFile") { root =>
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    Seq("", "a.txt").map(root.resolve(_).toNIO)
      .foreach(path => assertEquals(path.isRegularFile, FileIO.isRegularFile(path), path))
  }

  tmp.test("Path.isRegularFileNoLinks") { root =>
    TestTree.writeBytes(root.resolve("a.txt"), Array[Byte]('a'))
    Seq("", "a.txt").map(root.resolve(_).toNIO).foreach(path =>
      assertEquals(path.isRegularFileNoLinks, FileIO.isRegularFileNoLinks(path), path),
    )
  }

  tmp.test("Path.findFiles") { root =>
    TestTree.mkdir(root.resolve("dir"))
    TestTree.writeBytes(root.resolve("dir/a.txt"), Array[Byte]('a'))
    assertEquals(root.toNIO.findFiles, FileIO.findFiles(root.toNIO))
  }

  tmp.test("Path.readFile") { root =>
    val path = root.resolve("a.txt")
    TestTree.writeBytes(path, "aé".getBytes(StandardCharsets.UTF_8))
    assertEquals(
      path.toNIO.readFile(StandardCharsets.UTF_8),
      FileIO.readFile(path.toNIO)(StandardCharsets.UTF_8),
    )
  }

  tmp.test("Path.writeFile") { root =>
    val path = root.resolve("a.txt").toNIO
    path.writeFile("aé")(StandardCharsets.UTF_8)
    assertEquals(FileIO.readFile(path)(StandardCharsets.UTF_8), "aé")
  }

  tmp.test("Path.delete") { root =>
    val path = root.resolve("a.txt")
    TestTree.writeBytes(path, Array[Byte]('a'))
    path.toNIO.delete()
    assertEquals(FileIO.exists(path.toNIO), false)
  }

  tmp.test("Path.mkdir") { root =>
    val path = root.resolve("a").toNIO
    path.mkdir()
    assertEquals(FileIO.isDirectory(path), true)
  }

  tmp.test("Path.mkdirs") { root =>
    val path = root.resolve("a/b").toNIO
    path.mkdirs()
    assertEquals(FileIO.isDirectory(path), true)
  }

}

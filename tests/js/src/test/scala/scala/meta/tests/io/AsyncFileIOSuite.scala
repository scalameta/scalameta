package scala.meta.tests.io

import scala.meta.internal.io.{ExecutionContexts, FileIO, PathIO, PlatformFileIO}
import scala.meta.io.AbsolutePath

import java.nio.charset.StandardCharsets

import scala.concurrent.Future
import scala.io.Codec

import munit.FunSuite

class AsyncFileIOSuite extends FunSuite {

  private implicit val codec: Codec = Codec.UTF8

  private val tmp = FunFixture[AbsolutePath](_ => TestTree.mkdtemp(), TestTree.delete)

  private def roundTrip(root: AbsolutePath, text: String) = {
    val path = root.resolve("a.txt").toNIO
    PlatformFileIO.writeFileAsync(path, text).flatMap { _ =>
      assertEquals(FileIO.readFile(path), text)
      PlatformFileIO.readFileAsync(path)
    }(ExecutionContexts.parasitic).map(assertEquals(_, text))(ExecutionContexts.parasitic)
  }

  /** Returns the message of the failure of the future, with `<root>` in place of the root. */
  private def failure(root: AbsolutePath)(future: Future[_]): Future[String] = future.failed
    .map(_.toString.replace(root.toString, "<root>"))(ExecutionContexts.parasitic)

  tmp.test("an empty file")(roundTrip(_, ""))

  tmp.test("a file with a character across its 1024th byte")(roundTrip(_, "a" * 1023 + "é" + "b"))

  tmp.test("a write over a longer file") { root =>
    val path = root.resolve("a.txt").toNIO
    FileIO.writeFile(path, "a longer text")
    PlatformFileIO.writeFileAsync(path, "short")
      .map(_ => assertEquals(FileIO.readFile(path), "short"))(ExecutionContexts.parasitic)
  }

  tmp.test("a read of a missing file")(root =>
    failure(root)(PlatformFileIO.readFileAsync(root.resolve("missing").toNIO)).map(assertNoDiff(
      _,
      PathIO.fromUnix(
        "scala.scalajs.js.JavaScriptException: " +
          "Error: ENOENT: no such file or directory, open '<root>/missing'",
      ),
    ))(ExecutionContexts.parasitic),
  )

  tmp.test("a write into a missing directory")(root =>
    failure(root)(PlatformFileIO.writeFileAsync(root.resolve("missing/a.txt").toNIO, "a"))
      .map(assertNoDiff(
        _,
        PathIO.fromUnix(
          "scala.scalajs.js.JavaScriptException: " +
            "Error: ENOENT: no such file or directory, open '<root>/missing/a.txt'",
        ),
      ))(ExecutionContexts.parasitic),
  )

  tmp.test("a read in ISO-8859-1") { root =>
    val path = root.resolve("a.txt")
    TestTree.writeBytes(path, Array[Byte](0xe9.toByte))
    failure(root)(PlatformFileIO.readFileAsync(path.toNIO)(StandardCharsets.ISO_8859_1))
      .map(assertNoDiff(
        _,
        "scala.scalajs.js.JavaScriptException: TypeError [ERR_INVALID_ARG_VALUE]: " +
          "The argument 'encoding' is invalid encoding. Received 'ISO-8859-1'",
      ))(ExecutionContexts.parasitic)
  }

  tmp.test("a write in ISO-8859-1") { root =>
    val path = root.resolve("a.txt")
    failure(root)(PlatformFileIO.writeFileAsync(path.toNIO, "é")(StandardCharsets.ISO_8859_1)).map {
      obtained =>
        assertNoDiff(
          obtained,
          "scala.scalajs.js.JavaScriptException: TypeError [ERR_INVALID_ARG_VALUE]: " +
            "The argument 'encoding' is invalid encoding. Received 'ISO-8859-1'",
        )
        assertEquals(path.exists, false)
    }(ExecutionContexts.parasitic)
  }

}

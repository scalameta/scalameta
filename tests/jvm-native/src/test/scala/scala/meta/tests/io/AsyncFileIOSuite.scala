package scala.meta.tests.io

import scala.meta.internal.io._
import scala.meta.io.AbsolutePath

import scala.concurrent.Future
import scala.io.Codec

import munit.FunSuite

class AsyncFileIOSuite extends FunSuite {

  private implicit val codec: Codec = Codec.UTF8

  private val pool = PlatformExecutionContexts.createContext(2)

  private val tmp = FunFixture[AbsolutePath](_ => TestTree.mkdtemp(), TestTree.delete)

  override def afterAll(): Unit = pool.shutdown()

  private def roundTrip(root: AbsolutePath, text: String) = {
    val path = root.resolve("a.txt").toNIO
    PlatformFileIO.writeFileAsync(path, text, pool).flatMap { _ =>
      assertEquals(FileIO.readFile(path), text)
      PlatformFileIO.readFileAsync(path, pool)
    }(ExecutionContexts.parasitic).map(assertEquals(_, text))(ExecutionContexts.parasitic)
  }

  /** Returns the failure of the future, with `<root>` in place of the root. */
  private def failure(root: AbsolutePath)(future: Future[_]): Future[String] = future.failed
    .map(_.toString.replace(root.toString, "<root>"))(ExecutionContexts.parasitic)

  tmp.test("an empty file")(roundTrip(_, ""))

  tmp.test("a file with a character across its 1024th byte")(roundTrip(_, "a" * 1023 + "é" + "b"))

  tmp.test("a write over a longer file") { root =>
    val path = root.resolve("a.txt").toNIO
    FileIO.writeFile(path, "a longer text")
    PlatformFileIO.writeFileAsync(path, "short", pool)
      .map(_ => assertEquals(FileIO.readFile(path), "short"))(ExecutionContexts.parasitic)
  }

  tmp.test("a read of a missing file")(root =>
    failure(root)(PlatformFileIO.readFileAsync(root.resolve("missing").toNIO, pool)).map(
      assertNoDiff(_, PathIO.fromUnix("java.nio.file.NoSuchFileException: <root>/missing")),
    )(ExecutionContexts.parasitic),
  )

  tmp.test("a write into a missing directory")(root =>
    failure(root)(PlatformFileIO.writeFileAsync(root.resolve("missing/a.txt").toNIO, "a", pool)).map(
      assertNoDiff(_, PathIO.fromUnix("java.nio.file.NoSuchFileException: <root>/missing/a.txt")),
    )(ExecutionContexts.parasitic),
  )

}

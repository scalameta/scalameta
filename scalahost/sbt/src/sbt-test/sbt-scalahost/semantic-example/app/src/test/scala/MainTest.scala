import scala.meta._
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.internal.semantic.{schema => s}
import java.nio.file._
import org.scalatest._

abstract class BaseMirrorSuite extends FunSuite {
  implicit val mirror = Mirror()
  org.scalameta.logger.elem(mirror.database, mirror.sources)
  def checkContents(f: String => Unit): Unit = checkDb { case (_, x) => f(x.contents) }
  def checkDb(f: (RelativePath, s.Attributes) => Unit): Unit = {
    val db = v.Database.load(Classpath(sys.props("scalameta.classpath"))).toSchema
    assert(db.entries.nonEmpty, s"$db.entries.nonEmpty")
    db.entries.foreach { case (path, attributes) => f(path, attributes) }
  }

  def assertNonEmptyMirror(): Unit = {
    test("Mirror.sources") {
      assert(mirror.sources.length == 3)
    }
    val paths = List(
      "library1/target/scala-2.11/classes/META-INF/semanticdb/library1/src/main/scala-2.11/Test.semanticdb",
      "library2/target/scala-2.11/classes/META-INF/semanticdb/library2/src/main/scala/Test.semanticdb",
      "library2/target/scala-2.11/test-classes/META-INF/semanticdb/library2/src/test/scala/TestMain.semanticdb"
    )
    // parent because fork := true
    val workingDir = Paths.get(sys.props("user.dir")).getParent
    paths.foreach { path =>
      val relpath = Paths.get(path.replaceAllLiterally("/", java.io.File.separator))
      test(relpath.toString) {
        val abspath = workingDir.resolve(relpath)
        assert(abspath.toFile.exists())
        assert(abspath.toFile.isFile())
      }
    }
    test("Mirror.message") {
      assert(mirror.database.messages.length == 1)
    }
  }
}

class Fat extends BaseMirrorSuite {
  assertNonEmptyMirror()
  test("s.Attributes.contents.nonEmpty") {
    checkContents(x => assert(x.nonEmpty))
  }
}

class Slim extends BaseMirrorSuite {
  assertNonEmptyMirror()
  test("s.Attributes.contents.isEmpty") {
    checkContents(x => assert(x.isEmpty))
  }
}
class Mix extends BaseMirrorSuite {
  assertNonEmptyMirror()
  test("s.Attributes.contents.isEmpty") {
    checkDb {
      case (path, attributes) =>
        val contents = attributes.contents
        if (path.toString.contains("test-classes")) {
          assert(contents.isEmpty, s"'$contents'.isEmpty")
        } else {
          assert(contents.nonEmpty, s"'$contents'.nonEmpty")
        }
    }
  }
}

class Disabled extends BaseMirrorSuite {
  test("Mirror.sources.isEmpty") {
    assert(mirror.sources.isEmpty)
  }
}

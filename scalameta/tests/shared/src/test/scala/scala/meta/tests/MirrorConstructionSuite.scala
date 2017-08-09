package scala.meta
package tests

import org.scalatest._
import scala.meta.internal.semanticdb.{vfs => v}
import scala.meta.internal.semanticdb.{schema => s}

trait BaseSemanticSuite extends FunSuiteLike {
  val sourcepath = Sourcepath(BuildInfo.databaseSourcepath)
  val classpath = Classpath(BuildInfo.databaseClasspath)
}

class MirrorConstructionSuite extends BaseSemanticSuite {
  def semanticdbs: List[AbsolutePath] = classpath.deep.collect {
    case path if v.SemanticdbPaths.isSemanticdb(path.name) =>
      path.base.resolve(path.name)
  }

  test("Database.load(Classpath, Sourcepath)") {
    val database = Database.load(classpath, sourcepath)
    assert(database.entries.nonEmpty)
  }

  test("Database.load(Array[Byte])") {
    semanticdbs.foreach { path =>
      val database = Database.load(path.readAllBytes)
      assert(database.entries.nonEmpty, path.toString)
    }
  }

  test("Database.load(Classpath)") {
    val database = Database.load(classpath)
    assert(database.entries.nonEmpty)
  }

  test("s.Attributes.filename has no Windows slash (\\)") {
    semanticdbs.foreach { path =>
      val sattrs = s.Attributes.parseFrom(path.readAllBytes)
      assert(!sattrs.filename.contains('\\'))
    }
  }
}

package scala.meta.tests
package semanticdb

import org.scalatest._
import scala.meta._
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
    assert(database.documents.nonEmpty)
  }

  test("Database.load(Array[Byte])") {
    semanticdbs.foreach { path =>
      val database = Database.load(path.readAllBytes)
      assert(database.documents.nonEmpty, path.toString)
    }
  }

  test("Database.load(Classpath)") {
    val database = Database.load(classpath)
    assert(database.documents.nonEmpty)
  }

  test("s.Document.uri has no Windows slash (\\)") {
    semanticdbs.foreach { path =>
      val sattrs = s.Document.parseFrom(path.readAllBytes)
      assert(!sattrs.uri.contains('\\'))
    }
  }

}

package scala.meta
package tests

import scala.meta.internal.io.FileIO
import org.scalatest._
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.internal.semantic.{schema => s}

class CrossPlatformSemanticSuite extends FunSuite {
  val sourceroot = AbsolutePath(BuildInfo.mirrorRoot)
  val classpath = Classpath(BuildInfo.mirrorClasspath)
  test("Database.load(Classpath, Sourceroot)") {
    val mirror = Database.load(classpath, sourceroot)
    assert(mirror.sources.nonEmpty)
  }

  test("Database.load(Array[Byte])") {
    val fragment = classpath.deep.find(x => v.Paths.isSemanticdb(x.name)).get
    val bytes = fragment.base.resolve(fragment.name).readAllBytes
    val mirror = Database.load(bytes)
    assert(mirror.sources.nonEmpty)
  }
}


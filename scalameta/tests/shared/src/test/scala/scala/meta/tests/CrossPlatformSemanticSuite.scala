package scala.meta
package tests

import scala.meta.internal.io.FileIO

import org.scalatest._
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.internal.semantic.{schema => s}

class CrossPlatformSemanticSuite extends FunSuite {
  val sourcepath = Sourcepath(BuildInfo.mirrorSourcepath)
  val classpath = Classpath(BuildInfo.mirrorClasspath)
  test("Database.load(Sourcepath, Classpath)") {
    val mirror = Database.load(classpath, sourcepath)
    assert(mirror.sources.nonEmpty)
  }

  test("Database.load(RelativePath, Array[Byte])") {
    val fragment = classpath.deep.find(x => v.Paths.isSemanticdb(x.name)).get
    val bytes = fragment.base.resolve(fragment.name).readAllBytes
    val mirror = Database.load(RelativePath("Foo.scala"), bytes)
    assert(mirror.sources.nonEmpty)
  }
}

package scala.meta
package tests

import org.scalatest._
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.internal.semantic.{schema => s}

class CrossPlatformSemanticSuite extends FunSuite {
  test("Database.load(Sourcepath, Classpath)") {
    val sourcepath = Sourcepath(BuildInfo.mirrorSourcepath)
    val classpath = Classpath(BuildInfo.mirrorClasspath)
    val mirror = Database.load(classpath, sourcepath)
    assert(mirror.sources.nonEmpty)
  }
}
package scala.meta.tests
package semantic

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.internal.{ast => m}
import scala.meta.dialects.Scala211

class SemanticSuite extends FunSuite {
  implicit val mirror = Mirror(Artifact(sys.props("sbt.paths.scalalibrary.classes")))

  test("simple") {
    assert(t"List[Int]".show[Semantics] === """
    """.trim.stripMargin)
  }
}
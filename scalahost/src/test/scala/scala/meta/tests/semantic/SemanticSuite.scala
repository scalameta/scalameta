package scala.meta.tests
package semantic

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.internal.{ast => m}
import scala.meta.dialects.Scala211

class SemanticSuite extends FunSuite {
  implicit val c = Context(Artifact(sys.props("sbt.paths.scalalibrary.classes")))

  test("simple") {
    assert(t"List[Int]".show[Semantics] === """
      |Type.Apply(Type.Name("List")[1], Seq(Type.Name("Int")[2]))
      |[1] {1}::scala.package#List
      |[2] {2}::scala#Int
      |[3] {2}::scala.package
      |[4] {3}::scala
      |[5] {0}::_root_
      |{1} Type.Singleton(Term.Name("package")[3]{1}<>)
      |{2} Type.Singleton(Term.Name("scala")[4]{2}<>)
      |{3} Type.Singleton(Term.Name("_root_")[5]{3}<>)
    """.trim.stripMargin)
  }
}
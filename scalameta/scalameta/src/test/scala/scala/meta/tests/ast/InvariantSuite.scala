package scala.meta.tests
package ast

import org.scalatest._
import org.scalameta.invariants._
import scala.meta._
import scala.meta.dialects.Scala211

class InvariantSuite extends FunSuite {
  test("secondary constructors in templates") {
    val primaryCtor = Ctor.Primary(Nil, Ctor.Name("this"), Nil)
    val secondaryCtor = Ctor.Secondary(Nil, Ctor.Name("this"), List(List()), Ctor.Name("this"))
    val stats = List(secondaryCtor)
    val template = Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(stats))
    Defn.Class(Nil, Type.Name("test"), Nil, primaryCtor, template)
    intercept[InvariantFailedException] { Defn.Trait(Nil, Type.Name("test"), Nil, primaryCtor, template) }
    intercept[InvariantFailedException] { Defn.Object(Nil, Term.Name("test"), template) }
    intercept[InvariantFailedException] { Pkg(Term.Name("test"), stats) }
    intercept[InvariantFailedException] { Pkg.Object(Nil, Term.Name("test"), template) }
  }
}
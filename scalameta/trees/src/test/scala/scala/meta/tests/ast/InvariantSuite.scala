package scala.meta.tests
package ast

import org.scalatest._
import org.scalameta.invariants._

class InvariantSuite extends FunSuite {
  test("secondary constructors in templates") {
    import scala.meta.internal.{ast => impl}
    val primaryCtor = impl.Ctor.Primary(Nil, impl.Ctor.Name("this"), Nil)
    val secondaryCtor = impl.Ctor.Secondary(Nil, impl.Ctor.Name("this"), List(List()), impl.Ctor.Name("this"))
    val stats = List(secondaryCtor)
    val template = impl.Template(Nil, Nil, impl.Term.Param(Nil, impl.Name.Anonymous(), None, None), Some(stats))
    impl.Defn.Class(Nil, impl.Type.Name("test"), Nil, primaryCtor, template)
    intercept[InvariantFailedException] { impl.Defn.Trait(Nil, impl.Type.Name("test"), Nil, primaryCtor, template) }
    intercept[InvariantFailedException] { impl.Defn.Object(Nil, impl.Term.Name("test"), template) }
    intercept[InvariantFailedException] { impl.Pkg(impl.Term.Name("test"), stats) }
    intercept[InvariantFailedException] { impl.Pkg.Object(Nil, impl.Term.Name("test"), template) }
  }
}
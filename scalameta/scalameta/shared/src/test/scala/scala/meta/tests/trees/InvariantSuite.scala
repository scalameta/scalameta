package scala.meta.tests
package trees

import org.scalatest._
import org.scalameta.invariants._
import scala.meta._
import scala.meta.dialects.Scala211

class InvariantSuite extends FunSuite {
  test("secondary constructors in templates") {
    val primaryCtor = Ctor.Primary(Nil, Name.Anonymous(), Nil)
    val secondaryCtor = Ctor.Secondary(Nil, Name.Anonymous(), List(List()), Init(Type.Singleton(Term.This(Name.Anonymous())), Name.Anonymous(), Nil), Nil)
    val stats = List(secondaryCtor)
    val template = Template(Nil, Nil, Self(Name.Anonymous(), None), stats)
    Defn.Class(Nil, Type.Name("test"), Nil, primaryCtor, template)
    intercept[InvariantFailedException] { Defn.Trait(Nil, Type.Name("test"), Nil, primaryCtor, template) }
    intercept[InvariantFailedException] { Defn.Object(Nil, Term.Name("test"), template) }
    intercept[InvariantFailedException] { Pkg(Term.Name("test"), stats) }
    intercept[InvariantFailedException] { Pkg.Object(Nil, Term.Name("test"), template) }
  }

  test("Lit.Float/Double") {
    intercept[NumberFormatException](Lit.Float("a"))
    intercept[NumberFormatException](Lit.Double("a"))
  }

  test("Term.Repeated") {
    import scala.meta._
    val xs = q"xs: _*"
    intercept[InvariantFailedException] { q"$xs + $xs" }
  }

  test("Pat.Var") {
    import scala.meta._
    val x = p"X"
    intercept[InvariantFailedException] { p"case $x =>" }
  }

  test("Type.ByName") {
    import scala.meta._
    val t = t"=> T"
    intercept[InvariantFailedException] { t"List[$t]" }
  }

  test("Type.Repeated") {
    import scala.meta._
    val t = t"T*"
    intercept[InvariantFailedException] { t"List[$t]" }
  }

  test("Pat.SeqWildcard") {
    import scala.meta._
    val p = p"_*"
    intercept[InvariantFailedException] { p"case $p =>" }
  }

  test("Type.Var") {
    import scala.meta._
    val p"$_: List[$tvar]" = p"xs: List[t]"
    assert(tvar.is[Type.Var])
    intercept[InvariantFailedException] { p"x: $tvar" }
    val okay1 = t"List[$tvar]"
    val okay2 = q"List[$tvar]"
    val okay3 = p"$okay2(x, y)"
  }

  test("Init") {
    val init = init"this()"
    intercept[InvariantFailedException] { q"new $init" }
  }

  test("Mod.Private/Protected") {
    val ref = q"foo.bar"
    intercept[InvariantFailedException] { mod"private[$ref]" }
    intercept[InvariantFailedException] { mod"protected[$ref]" }
  }
}

package scala.meta.tests
package trees

import org.scalameta.invariants._
import scala.meta._
import scala.meta.dialects.Scala211

class InvariantSuite extends TreeSuiteBase {

  test("secondary constructors in templates") {
    val primaryCtor = EmptyCtor()
    val secondaryCtor = Ctor
      .Secondary(Nil, anon, List(List()), init(Type.Singleton(Term.This(anon))), Nil)
    val template = tpl(secondaryCtor)
    assertSyntax("class test { def this() = this }")(
      Defn.Class(Nil, pname("test"), Nil, primaryCtor, template),
    )
    assertSyntax("trait test { def this() = this }")(
      Defn.Trait(Nil, pname("test"), Nil, primaryCtor, template),
    )
    assertSyntax("object test { def this() = this }")(Defn.Object(Nil, tname("test"), template))
    assertSyntax("package object test { def this() = this }")(
      Pkg.Object(Nil, tname("test"), template),
    )
  }

  test("Lit.Float/Double") {
    intercept[NumberFormatException](flt("a"))
    intercept[NumberFormatException](dbl("a"))
  }

  test("Term.Repeated") {
    import scala.meta._
    val xs = q"xs: _*"
    assertSyntax("xs: _* + (xs: _*)")(q"$xs + $xs")
  }

  test("Pat.Var") {
    import scala.meta._
    val x = p"X"
    assertSyntax("case X =>")(p"case $x =>")
  }

  test("Type.ByName") {
    import scala.meta._
    val t = t"=> T"
    assertSyntax("List[=> T]")(t"List[$t]")
  }

  test("Type.Repeated") {
    import scala.meta._
    val t = t"T*"
    assertSyntax("List[(T*)]")(t"List[$t]")
  }

  test("Pat.SeqWildcard") {
    import scala.meta._
    val p = p"_*"
    assertSyntax("case _* =>")(p"case $p =>")
  }

  test("Type.Var") {
    import scala.meta._
    val p"$_: List[$tvar]" = p"xs: List[t]"
    assert(tvar.is[Type.Var])
    assertSyntax("x: t")(p"x: $tvar")
    val okay1 = t"List[$tvar]"
    assertSyntax("List[t]")(okay1)
    val okay2 = q"List[$tvar]"
    assertSyntax("List[t]")(okay2)
    val okay3 = p"$okay2(x, y)"
    assertSyntax("List[t](x, y)")(okay3)
  }

  test("Init") {
    val init = init"this()"
    assertSyntax("new this()")(q"new $init")
  }

  test("Mod.Private/Protected") {
    val ref = q"foo.bar"
    assertSyntax("private[foo.bar]")(mod"private[$ref]")
    assertSyntax("protected[foo.bar]")(mod"protected[$ref]")
  }

  test("empty Term.Tuple")(assertSyntax("()")(Term.Tuple(Nil)))
  test("nested Term.Tuple")(assertSyntax("((()))")(Term.Tuple(Term.Tuple(Lit.Unit() :: Nil) :: Nil)))

  test("empty Pat.Tuple")(assertSyntax("()")(Pat.Tuple(Nil)))
  test("nested Pat.Tuple")(assertSyntax("((()))")(Pat.Tuple(Pat.Tuple(Lit.Unit() :: Nil) :: Nil)))

  test("empty Type.Tuple")(assertSyntax("()")(Type.Tuple(Nil)))
  test("nested Type.Tuple")(assertSyntax("((()))")(Type.Tuple(Type.Tuple(Lit.Unit() :: Nil) :: Nil)))

}

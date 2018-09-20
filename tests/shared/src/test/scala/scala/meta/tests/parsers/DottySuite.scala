package scala.meta.tests
package parsers

import scala.meta._, Type.{Name => TypeName, _}
import scala.meta.dialects.Dotty

class DottySuite extends ParseSuite {
  test("case List(xs: _*)") {
    val tree = pat("List(xs: _*)")
    assert(tree.structure === "Pat.Extract(Term.Name(\"List\"), List(Pat.Bind(Pat.Var(Term.Name(\"xs\")), Pat.SeqWildcard())))")
    assert(tree.syntax === "List(xs: _*)")
  }
  test("xml literals") {
    intercept[TokenizeException]{ term("<foo>{bar}</foo>") }
  }

  test("inline def x = 42") {
    val tree1@Defn.Def(List(Mod.Inline()), Term.Name("x"), Nil, Nil, None, Lit(42)) = templStat("inline def x = 42")
    val tree2@Defn.Def(List(Mod.Inline()), Term.Name("x"), Nil, Nil, None, Lit(42)) = blockStat("inline def x = 42")

    assert(tree1.syntax === "inline def x = 42")
    assert(tree2.syntax === "inline def x = 42")
  }

  test("inline can be used as a modifier") {
    val tree = dialects.Dotty("{ inline def x = 42 }").parse[Term].get
    assert(tree.syntax === "{ inline def x = 42 }")
  }

  test("mod\"inline\"") {
    assert(mod"inline".structure === "Mod.Inline()")
  }

  test("inline cannot be used as an identifier") {
    intercept[ParseException] {
      dialects.Dotty("{ val inline = 42 }").parse[Term].get
    }
  }

  test("trait parameters are allowed") {
    val tree = dialects.Dotty("trait Foo(bar: Int)").parse[Stat].get
    assert(tree.syntax === "trait Foo(bar: Int)")
    assert(q"trait Foo(bar: Int)".syntax === "trait Foo(bar: Int)")
  }

  test("view bounds not allowed") {
    intercept[ParseException] {
      dialects.Dotty("{ def foo[T <% Int](t: T) = ??? }").parse[Term].get
    }
  }

  test("A with B") {
    val And(TypeName("A"), TypeName("B")) = tpe("A with B")
  }

  test("A & B") {
    val And(TypeName("A"), TypeName("B")) = tpe("A & B")
  }

  test("A | B") {
    val Or(TypeName("A"), TypeName("B")) = tpe("A | B")
  }

  test("[T] => (T, T)") {
    val Type.Lambda(
      List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
      Type.Tuple(List(TypeName("T"), TypeName("T")))) = tpe("[T] => (T, T)")
  }

  test("literal types are allowed") {
    val tree = dialects.Dotty("val a: 42 = 42").parse[Stat].get
    assert(tree.syntax === "val a: 42 = 42")
  }

  test("implicit function type") {
    val Type.ImplicitFunction(List(Type.Name("String")), Type.Name("Int")) =
      tpe("implicit String => Int")

    val Type.ImplicitFunction(List(Type.Name("String"), Type.Name("Boolean")), Type.Name("Int")) =
      tpe("implicit (String, Boolean) => Int")

    val Defn.Def(Nil, Term.Name("f"), Nil, List(Nil),
      Some(Type.ImplicitFunction(List(Type.Name("Int")), Type.Name("Int"))), _) =
        templStat("def f(): implicit Int => Int = ???")

    val Defn.Val(Nil, List(Pat.Var(Term.Name("x"))),
      Some(Type.ImplicitFunction(List(Type.Name("String"), Type.Name("Int")), Type.Name("Int"))), _) =
        templStat("val x: implicit (String, Int) => Int = ???")

    val Defn.Def(Nil, Term.Name("f"), _, Nil,
      Some(
        Type.ImplicitFunction(List(Type.Name("A")),
          Type.ImplicitFunction(List(Type.Name("B")), Type.Tuple(List(Type.Name("A"), Type.Name("B")))))), _) =
            templStat("def f[A, B]: implicit A => implicit B => (A, B) = ???")
  }

  test("invalid implicit function types") {
    def failWithMessage(code: String) = {
      val error = intercept[ParseException](term(code))
      assert(error.getMessage.contains("function type expected"))
    }

    failWithMessage("{ def f(f: Int => implicit Int): Int = ??? }")
    failWithMessage("{ def f(): implicit Int = ??? }")
    failWithMessage("{ def f(): Int => implicit Int = ??? }")
  }

  test("Type.ImplicitFunction.syntax") {
    assert(t"implicit String => Int".syntax == "implicit String => Int")
    assert(t"implicit String => (Int, Double)".syntax == "implicit String => (Int, Double)")
    assert(t"implicit (String, Double) => Int".syntax == "implicit (String, Double) => Int")
  }

  test("case classes without a parameter list are not allowed") {
    intercept[ParseException](templStat("case class A"))
    intercept[ParseException](templStat("case class A[T]"))
    intercept[ParseException](templStat("case class A[T] private"))
  }

  test("case classes with an empty parameter list are allowed") {
    templStat("case class A()")
    templStat("case class A @Inject() ()")
    templStat("case class A private ()")
  }

  checkOK("def foo(implicit x: => Int) = 1")
  checkOK("def foo(implicit y: Int, x: => Int) = 1")

  test("trailing commas are allowed") {
    templStat("""|case class A(
                 |  x: X,
                 |)""".stripMargin)
  }

}

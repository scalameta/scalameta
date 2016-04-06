package scala.meta.tests
package parsers

import scala.meta._, Defn.{Trait, Object, Class}
import scala.meta.dialects.Scala211

class TemplateSuite extends ParseSuite {
  test("trait T") {
    val Trait(Nil, Type.Name("T"), Nil, EmptyCtor(), Template(Nil, Nil, EmptySelf(), None)) = templStat("trait T")
    // TODO: revisit this once we have trivia in place
    // assert(templ.hasStats === false)
  }

  test("trait T {}") {
    val Trait(Nil, Type.Name("T"), Nil, EmptyCtor(), Template(Nil, Nil, EmptySelf(), Some(Nil))) = templStat("trait T {}")
    // TODO: revisit this once we have trivia in place
    // assert(templ.hasStats === true)
  }

  test("trait F[T]") {
    val Trait(Nil, Type.Name("F"),
              Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil,
              EmptyCtor(),
              EmptyTemplate()) = templStat("trait F[T]")
  }

  test("trait A extends B") {
    val Trait(Nil, Type.Name("A"), Nil, EmptyCtor(),
              Template(Nil, Ctor.Name("B") :: Nil, EmptySelf(), None)) =
      templStat("trait A extends B")
  }

  test("trait Inner <: { val x : Int = 3 }") {
    val Trait(Nil, Type.Name("Inner"), Nil, EmptyCtor(),
              Template(Nil, Nil, EmptySelf(), Some(Seq(
                Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("x"))), Some(Type.Name("Int")), Lit(3)))))) =
    templStat("trait Inner <: { val x : Int = 3 }")
  }

  test("trait A extends { val x: Int } with B") {
    val Trait(Nil, Type.Name("A"), Nil, EmptyCtor(),
              Template(Defn.Val(Nil, List(Pat.Var.Term(Term.Name("x"))), Some(Type.Name("Int")), Lit(2)) :: Nil,
                       Ctor.Name("B") :: Nil, EmptySelf(), None)) =
      templStat("trait A extends { val x: Int = 2 } with B")
  }

  test("trait A extends { self: B => }") {
    val Trait(Nil, Type.Name("A"), Nil, EmptyCtor(),
              Template(Nil, Nil, Term.Param(Nil, Term.Name("self"), Some(Type.Name("B")), None), Some(Nil))) =
      templStat("trait A { self: B => }")
  }

  test("trait T { def x: Int }") {
    val Trait(Nil, Type.Name("T"), Nil, EmptyCtor(),
              Template(Nil, Nil, EmptySelf(),
                       Some(Decl.Def(Nil, Term.Name("x"), Nil, Nil, Type.Name("Int")) :: Nil))) =
      templStat("trait T { def x: Int }")
  }

  test("class C") {
    val Class(Nil, Type.Name("C"), Nil, EmptyCtor(), EmptyTemplate()) = templStat("class C")
  }

  test("class C[T]") {
    val Class(Nil, Type.Name("C"),
              Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil,
              EmptyCtor(),
              EmptyTemplate()) = templStat("class C[T]")
  }

  test("class A extends B") {
    val Class(Nil, Type.Name("A"), Nil, EmptyCtor(),
              Template(Nil, Ctor.Name("B") :: Nil, EmptySelf(), None)) =
      templStat("class A extends B")
  }

  test("class A extends { val x: Int } with B") {
    val Class(Nil, Type.Name("A"), Nil, EmptyCtor(),
              Template(Defn.Val(Nil, List(Pat.Var.Term(Term.Name("x"))), Some(Type.Name("Int")), Lit(2)) :: Nil,
                       Ctor.Name("B") :: Nil, EmptySelf(), None)) =
      templStat("class A extends { val x: Int = 2 } with B")
  }

  test("class A extends { self: B => }") {
    val Class(Nil, Type.Name("A"), Nil, EmptyCtor(),
              Template(Nil, Nil, Term.Param(Nil, Term.Name("self"), Some(Type.Name("B")), None), Some(Nil))) =
      templStat("class A { self: B => }")
  }

  test("class A { this => }") {
    val Class(Nil, Type.Name("A"), Nil, EmptyCtor(), Template(Nil, Nil, self @ EmptySelf(), Some(Nil))) = templStat("class A { this => }")
    // TODO: revisit this once we have trivia in place
    // assert(self.hasThis == true)
  }

  test("class C { def x: Int }") {
    val Class(Nil, Type.Name("C"), Nil, EmptyCtor(),
              Template(Nil, Nil, EmptySelf(), Some(Decl.Def(Nil, Term.Name("x"), Nil, Nil, Type.Name("Int")) :: Nil))) =
      templStat("class C { def x: Int }")
  }

  test("class C(x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, Ctor.Name("this"),
                          (Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C(x: Int)")
  }

  test("class C private(x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Mod.Private(Name.Anonymous()) :: Nil, Ctor.Name("this"),
                           (Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C private(x: Int)")
  }

  test("class C(val x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, Ctor.Name("this"),
                           (Term.Param(Mod.ValParam() :: Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C(val x: Int)")
  }

  test("class C(var x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, Ctor.Name("this"),
                           (Term.Param(Mod.VarParam() :: Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C(var x: Int)")
  }

  test("class C(implicit x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, Ctor.Name("this"),
                           (Term.Param(Mod.Implicit() :: Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C(implicit x: Int)")
  }

  test("class C(override val x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, Ctor.Name("this"),
                           (Term.Param(List(Mod.Override(), Mod.ValParam()), Term.Name("x"), Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C(override val x: Int)")
  }

  test("object O") {
    val Object(Nil, Term.Name("O"), EmptyTemplate()) = templStat("object O")
  }

  test("case object O") {
    val Object(Mod.Case() :: Nil, Term.Name("O"), EmptyTemplate()) = templStat("case object O")
  }

  test("object A extends B") {
    val Object(Nil, Term.Name("A"), Template(Nil, Ctor.Name("B") :: Nil, EmptySelf(), None)) =
      templStat("object A extends B")
  }

  test("object A extends { val x: Int } with B") {
    val Object(Nil, Term.Name("A"),
              Template(Defn.Val(Nil, List(Pat.Var.Term(Term.Name("x"))), Some(Type.Name("Int")), Lit(2)) :: Nil,
                       Ctor.Name("B") :: Nil, EmptySelf(), None)) =
      templStat("object A extends { val x: Int = 2 } with B")
  }

  test("object A extends { self: B => }") {
    val Object(Nil, Term.Name("A"),
               Template(Nil, Nil, Term.Param(Nil, Term.Name("self"), Some(Type.Name("B")), None), Some(Nil))) =
      templStat("object A { self: B => }")
  }
}

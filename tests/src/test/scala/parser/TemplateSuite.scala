import scala.meta.internal.ast._, Defn.{Trait, Object, Class}
import scala.meta.dialects.Scala211

class TemplateSuite extends ParseSuite {
  test("trait T") {
    val Trait(Nil, Type.Name("T"), Nil, templ @ EmptyTemplate()) = templStat("trait T")
    // TODO: revisit this once we have trivia in place
    // assert(templ.hasStats === false)
  }

  test("trait T {}") {
    val Trait(Nil, Type.Name("T"), Nil, templ @ EmptyTemplate()) = templStat("trait T {}")
    // TODO: revisit this once we have trivia in place
    // assert(templ.hasStats === true)
  }

  test("trait F[T]") {
    val Trait(Nil, Type.Name("F"),
              Type.Param(Nil, Some(Type.Name("T")), Nil, Nil, Nil, Type.Bounds(None, None)) :: Nil,
              EmptyTemplate()) = templStat("trait F[T]")
  }

  test("trait A extends B") {
    val Trait(Nil, Type.Name("A"), Nil,
              Templ(Nil, Ctor.Ref(Type.Name("B"), Nil) :: Nil, EmptySelf(), Nil)) =
      templStat("trait A extends B")
  }

  test("trait A extends { val x: Int } with B") {
    val Trait(Nil, Type.Name("A"), Nil,
              Templ(Defn.Val(Nil, List(Term.Name("x")), Some(Type.Name("Int")), Lit.Int(2)) :: Nil,
                       Ctor.Ref(Type.Name("B"), Nil) :: Nil, EmptySelf(), Nil)) =
      templStat("trait A extends { val x: Int = 2 } with B")
  }

  test("trait A extends { self: B => }") {
    val Trait(Nil, Type.Name("A"), Nil,
              Templ(Nil, Nil, Term.Param(Nil, Some(Term.Name("self")), Some(Type.Name("B")), None), Nil)) =
      templStat("trait A { self: B => }")
  }

  test("trait T { def x: Int }") {
    val Trait(Nil, Type.Name("T"), Nil,
              Templ(Nil, Nil, EmptySelf(),
                       Decl.Def(Nil, Term.Name("x"), Nil, Nil, Type.Name("Int")) :: Nil)) =
      templStat("trait T { def x: Int }")
  }

  test("class C") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, Nil),
              EmptyTemplate()) = templStat("class C")
  }

  test("class C[T]") {
    val Class(Nil, Type.Name("C"),
              Type.Param(Nil, Some(Type.Name("T")), Nil, Nil, Nil, Type.Bounds(None, None)) :: Nil,
              Ctor.Primary(Nil, Nil),
              EmptyTemplate()) = templStat("class C[T]")
  }

  test("class A extends B") {
    val Class(Nil, Type.Name("A"), Nil, Ctor.Primary(Nil, Nil),
              Templ(Nil, Ctor.Ref(Type.Name("B"), Nil) :: Nil, EmptySelf(), Nil)) =
      templStat("class A extends B")
  }

  test("class A extends { val x: Int } with B") {
    val Class(Nil, Type.Name("A"), Nil, Ctor.Primary(Nil, Nil),
              Templ(Defn.Val(Nil, List(Term.Name("x")), Some(Type.Name("Int")), Lit.Int(2)) :: Nil,
                       Ctor.Ref(Type.Name("B"), Nil) :: Nil, EmptySelf(), Nil)) =
      templStat("class A extends { val x: Int = 2 } with B")
  }

  test("class A extends { self: B => }") {
    val Class(Nil, Type.Name("A"), Nil, Ctor.Primary(Nil, Nil),
              Templ(Nil, Nil, Term.Param(Nil, Some(Term.Name("self")), Some(Type.Name("B")), None), Nil)) =
      templStat("class A { self: B => }")
  }

  test("class A { this => }") {
    val Class(Nil, Type.Name("A"), Nil, Ctor.Primary(Nil, Nil),
              Templ(Nil, Nil, self @ EmptySelf(), Nil)) =
      templStat("class A { this => }")
    // TODO: revisit this once we have trivia in place
    // assert(self.hasThis == true)
  }

  test("class C { def x: Int }") {
    val Class(Nil, Type.Name("C"), Nil, Ctor.Primary(Nil, Nil),
              Templ(Nil, Nil, EmptySelf(),
                       Decl.Def(Nil, Term.Name("x"), Nil, Nil, Type.Name("Int")) :: Nil)) =
      templStat("class C { def x: Int }")
  }

  test("class C(x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, (Term.Param(Nil, Some(Term.Name("x")),
                                            Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C(x: Int)")
  }

  test("class C private(x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Mod.Private() :: Nil,
                           (Term.Param(Nil, Some(Term.Name("x")),
                                       Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C private(x: Int)")
  }

  test("class C(val x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, (Templ.Param.Val(Nil, Some(Term.Name("x")), Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C(val x: Int)")
  }

  test("class C(var x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, (Templ.Param.Var(Nil, Some(Term.Name("x")), Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C(var x: Int)")
  }

  test("class C(implicit x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, (Term.Param(Mod.Implicit() :: Nil, Some(Term.Name("x")),
                                            Some(Type.Name("Int")), None) :: Nil) :: Nil),
              EmptyTemplate()) =
      templStat("class C(implicit x: Int)")
  }

  test("class C(override val x: Int)") {
    val Class(Nil, Type.Name("C"), Nil,
              Ctor.Primary(Nil, (Templ.Param.Val(List(Mod.Override()), Some(Term.Name("x")), Some(Type.Name("Int")), None) :: Nil) :: Nil),
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
    val Object(Nil, Term.Name("A"),
              Templ(Nil, Ctor.Ref(Type.Name("B"), Nil) :: Nil, EmptySelf(), Nil)) =
      templStat("object A extends B")
  }

  test("object A extends { val x: Int } with B") {
    val Object(Nil, Term.Name("A"),
              Templ(Defn.Val(Nil, List(Term.Name("x")), Some(Type.Name("Int")), Lit.Int(2)) :: Nil,
                       Ctor.Ref(Type.Name("B"), Nil) :: Nil, EmptySelf(), Nil)) =
      templStat("object A extends { val x: Int = 2 } with B")
  }

  test("object A extends { self: B => }") {
    val Object(Nil, Term.Name("A"),
               Templ(Nil, Nil, Term.Param(Nil, Some(Term.Name("self")), Some(Type.Name("B")), None), Nil)) =
      templStat("object A { self: B => }")
  }
}

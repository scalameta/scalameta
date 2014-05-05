import scala.reflect.core._, Aux._, Defn.{Trait, Object, Class}

class TemplateSuite extends ParseSuite {
  test("trait T") {
    val Trait(Nil, Type.Ident("T", false), Nil, Template.empty) = templStat("trait T")
  }

  test("trait F[T]") {
    val Trait(Nil, Type.Ident("F", false),
              TypeParam(Nil, Some(Type.Ident("T", false)), Nil, Nil, Nil, TypeBounds.empty) :: Nil,
              Template.empty) = templStat("trait F[T]")
  }

  test("trait A extends B") {
    val Trait(Nil, Type.Ident("A", false), Nil,
              Template(Nil, Parent(Type.Ident("B", false), Nil) :: Nil, Self.empty, Nil)) =
      templStat("trait A extends B")
  }

  test("trait A extends { val x: Int } with B") {
    val Trait(Nil, Type.Ident("A", false), Nil,
              Template(Defn.Val(Nil, List(Term.Ident("x", false)), Some(Type.Ident("Int", false)), Lit.Int(2)) :: Nil,
                       Parent(Type.Ident("B", false), Nil) :: Nil, Self.empty, Nil)) =
      templStat("trait A extends { val x: Int = 2 } with B")
  }

  test("trait A extends { self: B => }") {
    val Trait(Nil, Type.Ident("A", false), Nil,
              Template(Nil, Nil, Self(Some(Term.Ident("self", false)), Some(Type.Ident("B", false))), Nil)) =
      templStat("trait A { self: B => }")
  }

  test("trait T { def x: Int }") {
    val Trait(Nil, Type.Ident("T", false), Nil,
              Template(Nil, Nil, Self.empty,
                       Decl.Def(Nil, Term.Ident("x", false), Nil, Nil, Nil, Type.Ident("Int", false)) :: Nil)) =
      templStat("trait T { def x: Int }")
  }

  test("class C") {
    val Class(Nil, Type.Ident("C", false), Nil, Ctor.Primary.empty, Template.empty) = templStat("class C")
  }

  test("class C[T]") {
    val Class(Nil, Type.Ident("C", false),
              TypeParam(Nil, Some(Type.Ident("T", false)), Nil, Nil, Nil, TypeBounds.empty) :: Nil,
              Ctor.Primary.empty,
              Template.empty) = templStat("class C[T]")
  }

  test("class A extends B") {
    val Class(Nil, Type.Ident("A", false), Nil, Ctor.Primary.empty,
              Template(Nil, Parent(Type.Ident("B", false), Nil) :: Nil, Self.empty, Nil)) =
      templStat("class A extends B")
  }

  test("class A extends { val x: Int } with B") {
    val Class(Nil, Type.Ident("A", false), Nil, Ctor.Primary.empty,
              Template(Defn.Val(Nil, List(Term.Ident("x", false)), Some(Type.Ident("Int", false)), Lit.Int(2)) :: Nil,
                       Parent(Type.Ident("B", false), Nil) :: Nil, Self.empty, Nil)) =
      templStat("class A extends { val x: Int = 2 } with B")
  }

  test("class A extends { self: B => }") {
    val Class(Nil, Type.Ident("A", false), Nil, Ctor.Primary.empty,
              Template(Nil, Nil, Self(Some(Term.Ident("self", false)), Some(Type.Ident("B", false))), Nil)) =
      templStat("class A { self: B => }")
  }

  test("class C { def x: Int }") {
    val Class(Nil, Type.Ident("C", false), Nil, Ctor.Primary.empty,
              Template(Nil, Nil, Self.empty,
                       Decl.Def(Nil, Term.Ident("x", false), Nil, Nil, Nil, Type.Ident("Int", false)) :: Nil)) =
      templStat("class C { def x: Int }")
  }

  test("class C(x: Int)") {
    val Class(Nil, Type.Ident("C", false), Nil,
              Ctor.Primary(Nil, (Param(Nil, Some(Term.Ident("x", false)),
                                       Some(Type.Ident("Int", false)), None) :: Nil) :: Nil, Nil),
              Template.empty) =
      templStat("class C(x: Int)")
  }

  test("class C private(x: Int)") {
    val Class(Nil, Type.Ident("C", false), Nil,
              Ctor.Primary(Mod.Private(None) :: Nil,
                           (Param(Nil, Some(Term.Ident("x", false)),
                                  Some(Type.Ident("Int", false)), None) :: Nil) :: Nil, Nil),
              Template.empty) =
      templStat("class C private(x: Int)")
  }

  test("class C(val x: Int)") {
    val Class(Nil, Type.Ident("C", false), Nil,
              Ctor.Primary(Nil, (Param(Mod.ValParam() :: Nil, Some(Term.Ident("x", false)),
                                       Some(Type.Ident("Int", false)), None) :: Nil) :: Nil, Nil),
              Template.empty) =
      templStat("class C(val x: Int)")
  }

  test("class C(var x: Int)") {
    val Class(Nil, Type.Ident("C", false), Nil,
              Ctor.Primary(Nil, (Param(Mod.VarParam() :: Nil, Some(Term.Ident("x", false)),
                                       Some(Type.Ident("Int", false)), None) :: Nil) :: Nil, Nil),
              Template.empty) =
      templStat("class C(var x: Int)")
  }

  test("class C(implicit x: Int)") {
    val Class(Nil, Type.Ident("C", false), Nil,
              Ctor.Primary(Nil, Nil, Param(Nil, Some(Term.Ident("x", false)),
                                           Some(Type.Ident("Int", false)), None) :: Nil),
              Template.empty) =
      templStat("class C(implicit x: Int)")
  }



  test("object O") {
    val Object(Nil, Term.Ident("O", false), Template.empty) = templStat("object O")
  }

  test("case object O") {
    val Object(Mod.Case() :: Nil, Term.Ident("O", false), Template.empty) = templStat("case object O")
  }

  test("object A extends B") {
    val Object(Nil, Term.Ident("A", false),
              Template(Nil, Parent(Type.Ident("B", false), Nil) :: Nil, Self.empty, Nil)) =
      templStat("object A extends B")
  }

  test("object A extends { val x: Int } with B") {
    val Object(Nil, Term.Ident("A", false),
              Template(Defn.Val(Nil, List(Term.Ident("x", false)), Some(Type.Ident("Int", false)), Lit.Int(2)) :: Nil,
                       Parent(Type.Ident("B", false), Nil) :: Nil, Self.empty, Nil)) =
      templStat("object A extends { val x: Int = 2 } with B")
  }

  test("object A extends { self: B => }") {
    val Object(Nil, Term.Ident("A", false),
               Template(Nil, Nil, Self(Some(Term.Ident("self", false)), Some(Type.Ident("B", false))), Nil)) =
      templStat("object A { self: B => }")
  }
}

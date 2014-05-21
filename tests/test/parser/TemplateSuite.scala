import scala.reflect.core._, Aux._, Defn.{Trait, Object, Class}

class TemplateSuite extends ParseSuite {
  test("trait T") {
    val Trait(Nil, Type.Name("T", false), Nil, Aux.Template(Nil, Nil, Self(None, None), Nil)) = templStat("trait T")
  }

  test("trait F[T]") {
    val Trait(Nil, Type.Name("F", false),
              TypeParam.Named(Nil, Type.Name("T", false), Nil, Nil, Nil, TypeBounds(None, None)) :: Nil,
              Aux.Template(Nil, Nil, Self(None, None), Nil)) = templStat("trait F[T]")
  }

  test("trait A extends B") {
    val Trait(Nil, Type.Name("A", false), Nil,
              Template(Nil, Parent(Type.Name("B", false), Nil) :: Nil, Self(None, None), Nil)) =
      templStat("trait A extends B")
  }

  test("trait A extends { val x: Int } with B") {
    val Trait(Nil, Type.Name("A", false), Nil,
              Template(Defn.Val(Nil, List(Term.Name("x", false)), Some(Type.Name("Int", false)), Lit.Int(2)) :: Nil,
                       Parent(Type.Name("B", false), Nil) :: Nil, Self(None, None), Nil)) =
      templStat("trait A extends { val x: Int = 2 } with B")
  }

  test("trait A extends { self: B => }") {
    val Trait(Nil, Type.Name("A", false), Nil,
              Template(Nil, Nil, Self(Some(Term.Name("self", false)), Some(Type.Name("B", false))), Nil)) =
      templStat("trait A { self: B => }")
  }

  test("trait T { def x: Int }") {
    val Trait(Nil, Type.Name("T", false), Nil,
              Template(Nil, Nil, Self(None, None),
                       Decl.Def(Nil, Term.Name("x", false), Nil, Nil, Nil, Type.Name("Int", false)) :: Nil)) =
      templStat("trait T { def x: Int }")
  }

  test("class C") {
    val Class(Nil, Type.Name("C", false), Nil,
              Ctor.Primary(Nil, Nil, Nil),
              Aux.Template(Nil, Nil, Self(None, None), Nil)) = templStat("class C")
  }

  test("class C[T]") {
    val Class(Nil, Type.Name("C", false),
              TypeParam.Named(Nil, Type.Name("T", false), Nil, Nil, Nil, TypeBounds(None, None)) :: Nil,
              Ctor.Primary(Nil, Nil, Nil),
              Aux.Template(Nil, Nil, Self(None, None), Nil)) = templStat("class C[T]")
  }

  test("class A extends B") {
    val Class(Nil, Type.Name("A", false), Nil, Ctor.Primary(Nil, Nil, Nil),
              Template(Nil, Parent(Type.Name("B", false), Nil) :: Nil, Self(None, None), Nil)) =
      templStat("class A extends B")
  }

  test("class A extends { val x: Int } with B") {
    val Class(Nil, Type.Name("A", false), Nil, Ctor.Primary(Nil, Nil, Nil),
              Template(Defn.Val(Nil, List(Term.Name("x", false)), Some(Type.Name("Int", false)), Lit.Int(2)) :: Nil,
                       Parent(Type.Name("B", false), Nil) :: Nil, Self(None, None), Nil)) =
      templStat("class A extends { val x: Int = 2 } with B")
  }

  test("class A extends { self: B => }") {
    val Class(Nil, Type.Name("A", false), Nil, Ctor.Primary(Nil, Nil, Nil),
              Template(Nil, Nil, Self(Some(Term.Name("self", false)), Some(Type.Name("B", false))), Nil)) =
      templStat("class A { self: B => }")
  }

  test("class C { def x: Int }") {
    val Class(Nil, Type.Name("C", false), Nil, Ctor.Primary(Nil, Nil, Nil),
              Template(Nil, Nil, Self(None, None),
                       Decl.Def(Nil, Term.Name("x", false), Nil, Nil, Nil, Type.Name("Int", false)) :: Nil)) =
      templStat("class C { def x: Int }")
  }

  test("class C(x: Int)") {
    val Class(Nil, Type.Name("C", false), Nil,
              Ctor.Primary(Nil, (Param.Named(Nil, Term.Name("x", false),
                                             Some(Type.Name("Int", false)), None) :: Nil) :: Nil, Nil),
              Aux.Template(Nil, Nil, Self(None, None), Nil)) =
      templStat("class C(x: Int)")
  }

  test("class C private(x: Int)") {
    val Class(Nil, Type.Name("C", false), Nil,
              Ctor.Primary(Mod.Private(None) :: Nil,
                           (Param.Named(Nil, Term.Name("x", false),
                                       Some(Type.Name("Int", false)), None) :: Nil) :: Nil, Nil),
              Aux.Template(Nil, Nil, Self(None, None), Nil)) =
      templStat("class C private(x: Int)")
  }

  test("class C(val x: Int)") {
    val Class(Nil, Type.Name("C", false), Nil,
              Ctor.Primary(Nil, (Param.Named(Mod.ValParam() :: Nil, Term.Name("x", false),
                                             Some(Type.Name("Int", false)), None) :: Nil) :: Nil, Nil),
              Aux.Template(Nil, Nil, Self(None, None), Nil)) =
      templStat("class C(val x: Int)")
  }

  test("class C(var x: Int)") {
    val Class(Nil, Type.Name("C", false), Nil,
              Ctor.Primary(Nil, (Param.Named(Mod.VarParam() :: Nil, Term.Name("x", false),
                                             Some(Type.Name("Int", false)), None) :: Nil) :: Nil, Nil),
              Aux.Template(Nil, Nil, Self(None, None), Nil)) =
      templStat("class C(var x: Int)")
  }

  test("class C(implicit x: Int)") {
    val Class(Nil, Type.Name("C", false), Nil,
              Ctor.Primary(Nil, Nil, Param.Named(Nil, Term.Name("x", false),
                                                 Some(Type.Name("Int", false)), None) :: Nil),
              Aux.Template(Nil, Nil, Self(None, None), Nil)) =
      templStat("class C(implicit x: Int)")
  }



  test("object O") {
    val Object(Nil, Term.Name("O", false), Aux.Template(Nil, Nil, Self(None, None), Nil)) = templStat("object O")
  }

  test("case object O") {
    val Object(Mod.Case() :: Nil, Term.Name("O", false), Aux.Template(Nil, Nil, Self(None, None), Nil)) = templStat("case object O")
  }

  test("object A extends B") {
    val Object(Nil, Term.Name("A", false),
              Template(Nil, Parent(Type.Name("B", false), Nil) :: Nil, Self(None, None), Nil)) =
      templStat("object A extends B")
  }

  test("object A extends { val x: Int } with B") {
    val Object(Nil, Term.Name("A", false),
              Template(Defn.Val(Nil, List(Term.Name("x", false)), Some(Type.Name("Int", false)), Lit.Int(2)) :: Nil,
                       Parent(Type.Name("B", false), Nil) :: Nil, Self(None, None), Nil)) =
      templStat("object A extends { val x: Int = 2 } with B")
  }

  test("object A extends { self: B => }") {
    val Object(Nil, Term.Name("A", false),
               Template(Nil, Nil, Self(Some(Term.Name("self", false)), Some(Type.Name("B", false))), Nil)) =
      templStat("object A { self: B => }")
  }
}

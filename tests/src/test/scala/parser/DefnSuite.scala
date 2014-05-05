import scala.reflect.core._, Aux._

class DefnSuite extends ParseSuite {
  test("val x = 2") {
    val Defn.Val(Nil, Term.Ident("x", false) :: Nil, None, Lit.Int(2)) = templStat("val x = 2")
  }

  test("var x = 2") {
    val Defn.Var(Nil, Term.Ident("x", false) :: Nil, None, Some(Lit.Int(2))) = templStat("var x = 2")
  }

  test("val x, y = 2") {
    val Defn.Val(Nil, Term.Ident("x", false) :: Term.Ident("y", false) :: Nil,
                 None, Lit.Int(2)) = templStat("val x, y = 2")
  }

  test("val x: Int = 2") {
    val Defn.Val(Nil, Term.Ident("x", false) :: Nil,
                 Some(Type.Ident("Int", false)), Lit.Int(2)) = templStat("val x: Int = 2")
  }

  test("var x: Int = _") {
    val Defn.Var(Nil, Term.Ident("x", false) :: Nil,
                 Some(Type.Ident("Int", false)), None) = templStat("var x: Int = _")
  }

  test("val (x: Int) = 2") {
    val Defn.Val(Nil, Pat.Typed(Term.Ident("x", false), Type.Ident("Int", false)) :: Nil,
                 None, Lit.Int(2)) = templStat("val (x: Int) = 2")
  }

  test("type A = B") {
    val Defn.Type(Nil, Type.Ident("A", false), Nil, Type.Ident("B", false)) = templStat("type A = B")
  }

  test("type F[T] = List[T]") {
    val Defn.Type(Nil, Type.Ident("F", false),
                  TypeParam(Nil, Some(Type.Ident("T", false)), Nil, Nil, Nil, TypeBounds.empty) :: Nil,
                  Type.Apply(Type.Ident("List", false), Type.Ident("T", false) :: Nil)) = templStat("type F[T] = List[T]")
  }

  test("def x = 2") {
    val Defn.Def(Nil, Term.Ident("x", false), Nil, Nil, Nil, None, Lit.Int(2)) = templStat("def x = 2")
  }

  test("def x[A <: B] = 2") {
    val Defn.Def(Nil, Term.Ident("x", false),
                 TypeParam(Nil, Some(Type.Ident("A", false)), Nil, Nil, Nil,
                           TypeBounds(None, Some(Type.Ident("B", false)))) :: Nil,
                 Nil, Nil, None, Lit.Int(2)) = templStat("def x[A <: B] = 2")
  }

  test("def x[A <% B] = 2") {
    val Defn.Def(Nil, Term.Ident("x", false),
                 TypeParam(Nil, Some(Type.Ident("A", false)), Nil, Nil,
                           Type.Ident("B", false) :: Nil,
                           TypeBounds.empty) :: Nil,
                 Nil, Nil, None, Lit.Int(2)) = templStat("def x[A <% B] = 2")
  }


  test("def x[A: B] = 2") {
    val Defn.Def(Nil, Term.Ident("x", false),
                 TypeParam(Nil, Some(Type.Ident("A", false)), Nil,
                           Type.Ident("B", false) :: Nil,
                           Nil, TypeBounds.empty) :: Nil,
                 Nil, Nil, None, Lit.Int(2)) = templStat("def x[A: B] = 2")
  }

  test("def f(a: Int)(implicit b: Int) = a + b") {
    val Defn.Def(Nil, Term.Ident("f", false), Nil,
                 (Param(Nil, Some(Term.Ident("a", false)), Some(Type.Ident("Int", false)), None) :: Nil) :: Nil,
                 (Param(Nil, Some(Term.Ident("b", false)), Some(Type.Ident("Int", false)), None) :: Nil), None,
                 Term.Apply(Term.Select(Term.Ident("a", false), Term.Ident("+", false)), Term.Ident("b", false) :: Nil)) =
      templStat("def f(a: Int)(implicit b: Int) = a + b")
  }

  test("def proc { return 42 }") {
    val Defn.Procedure(Nil, Term.Ident("proc", false), Nil, Nil, Nil,
                       Term.Return(Lit.Int(42))) = templStat("def proc { return 42 }")

  }
}

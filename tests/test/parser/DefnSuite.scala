import scala.reflect.core._, Aux._

class DefnSuite extends ParseSuite {
  test("val x = 2") {
    val Defn.Val(Nil, Term.Name("x", false) :: Nil, None, Lit.Int(2)) = templStat("val x = 2")
  }

  test("var x = 2") {
    val Defn.Var(Nil, Term.Name("x", false) :: Nil, None, Some(Lit.Int(2))) = templStat("var x = 2")
  }

  test("val x, y = 2") {
    val Defn.Val(Nil, Term.Name("x", false) :: Term.Name("y", false) :: Nil,
                 None, Lit.Int(2)) = templStat("val x, y = 2")
  }

  test("val x: Int = 2") {
    val Defn.Val(Nil, Term.Name("x", false) :: Nil,
                 Some(Type.Name("Int", false)), Lit.Int(2)) = templStat("val x: Int = 2")
  }

  test("var x: Int = _") {
    val Defn.Var(Nil, Term.Name("x", false) :: Nil,
                 Some(Type.Name("Int", false)), None) = templStat("var x: Int = _")
  }

  test("val (x: Int) = 2") {
    val Defn.Val(Nil, Pat.Typed(Term.Name("x", false), Type.Name("Int", false)) :: Nil,
                 None, Lit.Int(2)) = templStat("val (x: Int) = 2")
  }

  test("type A = B") {
    val Defn.Type(Nil, Type.Name("A", false), Nil, Type.Name("B", false)) = templStat("type A = B")
  }

  test("type F[T] = List[T]") {
    val Defn.Type(Nil, Type.Name("F", false),
                  TypeParam.Named(Nil, Type.Name("T", false), Nil, Nil, Nil, TypeBounds(None, None)) :: Nil,
                  Type.Apply(Type.Name("List", false), Type.Name("T", false) :: Nil)) = templStat("type F[T] = List[T]")
  }

  test("def x = 2") {
    val Defn.Def(Nil, Term.Name("x", false), Nil, Nil, Nil, None, Lit.Int(2)) = templStat("def x = 2")
  }

  test("def x[A <: B] = 2") {
    val Defn.Def(Nil, Term.Name("x", false),
                 TypeParam.Named(Nil, Type.Name("A", false), Nil, Nil, Nil,
                                 TypeBounds(None, Some(Type.Name("B", false)))) :: Nil,
                 Nil, Nil, None, Lit.Int(2)) = templStat("def x[A <: B] = 2")
  }

  test("def x[A <% B] = 2") {
    val Defn.Def(Nil, Term.Name("x", false),
                 TypeParam.Named(Nil, Type.Name("A", false), Nil, Nil,
                                 Type.Name("B", false) :: Nil,
                                 TypeBounds(None, None)) :: Nil,
                 Nil, Nil, None, Lit.Int(2)) = templStat("def x[A <% B] = 2")
  }


  test("def x[A: B] = 2") {
    val Defn.Def(Nil, Term.Name("x", false),
                 TypeParam.Named(Nil, Type.Name("A", false), Nil,
                                 Type.Name("B", false) :: Nil,
                                 Nil, TypeBounds(None, None)) :: Nil,
                 Nil, Nil, None, Lit.Int(2)) = templStat("def x[A: B] = 2")
  }

  test("def f(a: Int)(implicit b: Int) = a + b") {
    val Defn.Def(Nil, Term.Name("f", false), Nil,
                 (Param.Named(Nil, Term.Name("a", false), Some(Type.Name("Int", false)), None) :: Nil) :: Nil,
                 (Param.Named(Nil, Term.Name("b", false), Some(Type.Name("Int", false)), None) :: Nil), None,
                 Term.ApplyInfix(Term.Name("a", false), Term.Name("+", false), Nil, Term.Name("b", false) :: Nil)) =
      templStat("def f(a: Int)(implicit b: Int) = a + b")
  }

  test("def proc { return 42 }") {
    val Defn.Procedure(Nil, Term.Name("proc", false), Nil, Nil, Nil,
                       Term.Return(Some(Lit.Int(42))) :: Nil) = templStat("def proc { return 42 }")

  }
}

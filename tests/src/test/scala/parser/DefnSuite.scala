import scala.meta.internal.ast._
import scala.meta.dialects.Scala211

class DefnSuite extends ParseSuite {
  test("val x = 2") {
    val Defn.Val(Nil, Term.Name("x") :: Nil, None, Lit.Int(2)) = templStat("val x = 2")
  }

  test("var x = 2") {
    val Defn.Var(Nil, Term.Name("x") :: Nil, None, Some(Lit.Int(2))) = templStat("var x = 2")
  }

  test("val x, y = 2") {
    val Defn.Val(Nil, Term.Name("x") :: Term.Name("y") :: Nil,
                 None, Lit.Int(2)) = templStat("val x, y = 2")
  }

  test("val x: Int = 2") {
    val Defn.Val(Nil, Term.Name("x") :: Nil,
                 Some(Type.Name("Int")), Lit.Int(2)) = templStat("val x: Int = 2")
  }

  test("var x: Int = _") {
    val Defn.Var(Nil, Term.Name("x") :: Nil,
                 Some(Type.Name("Int")), None) = templStat("var x: Int = _")
  }

  test("val (x: Int) = 2") {
    val Defn.Val(Nil, Pat.Typed(Term.Name("x"), Type.Name("Int")) :: Nil,
                 None, Lit.Int(2)) = templStat("val (x: Int) = 2")
  }

  test("type A = B") {
    val Defn.Type(Nil, Type.Name("A"), Nil, Type.Name("B")) = templStat("type A = B")
  }

  test("type F[T] = List[T]") {
    val Defn.Type(Nil, Type.Name("F"),
                  Type.Param(Nil, Some(Type.Name("T")), Nil, Nil, Nil, Type.Bounds(None, None)) :: Nil,
                  Type.Apply(Type.Name("List"), Type.Name("T") :: Nil)) = templStat("type F[T] = List[T]")
  }

  test("def x = 2") {
    val Defn.Def(Nil, Term.Name("x"), Nil, Nil, None, Lit.Int(2)) = templStat("def x = 2")
  }

  test("def x[A <: B] = 2") {
    val Defn.Def(Nil, Term.Name("x"),
                 Type.Param(Nil, Some(Type.Name("A")), Nil, Nil, Nil,
                            Type.Bounds(None, Some(Type.Name("B")))) :: Nil,
                 Nil, None, Lit.Int(2)) = templStat("def x[A <: B] = 2")
  }

  test("def x[A <% B] = 2") {
    val Defn.Def(Nil, Term.Name("x"),
                 Type.Param(Nil, Some(Type.Name("A")), Nil, Nil,
                            Type.Name("B") :: Nil,
                            Type.Bounds(None, None)) :: Nil,
                 Nil, None, Lit.Int(2)) = templStat("def x[A <% B] = 2")
  }


  test("def x[A: B] = 2") {
    val Defn.Def(Nil, Term.Name("x"),
                 Type.Param(Nil, Some(Type.Name("A")), Nil,
                            Type.Name("B") :: Nil,
                            Nil, Type.Bounds(None, None)) :: Nil,
                 Nil, None, Lit.Int(2)) = templStat("def x[A: B] = 2")
  }

  test("def f(a: Int)(implicit b: Int) = a + b") {
    val Defn.Def(Nil, Term.Name("f"), Nil,
                 (Term.Param(Nil, Some(Term.Name("a")), Some(Type.Name("Int")), None) :: Nil) ::
                 (Term.Param(Mod.Implicit() :: Nil, Some(Term.Name("b")), Some(Type.Name("Int")), None) :: Nil) :: Nil, None,
                 Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Term.Name("b") :: Nil)) =
      templStat("def f(a: Int)(implicit b: Int) = a + b")
  }

  test("def proc { return 42 }") {
    val Defn.Procedure(Nil, Term.Name("proc"), Nil, Nil,
                       (ret @ Term.Return(Lit.Int(42))) :: Nil) = templStat("def proc { return 42 }")
    assert(ret.hasExpr === true)
  }

  test("def f(x: Int): Int = macro impl") {
    val Defn.Macro(Nil, Term.Name("f"), Nil,
                   (Term.Param(List(), Some(Term.Name(x)), Some(Type.Name("Int")), None) :: Nil) :: Nil,
                   Type.Name("Int"), Term.Name("impl")) = templStat("def f(x: Int): Int = macro impl")
  }
}

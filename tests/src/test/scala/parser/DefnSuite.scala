import scala.reflect.core._

class DefnSuite extends ParseSuite {
  import Aux._

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
}

import scala.reflect.core._, Aux._

class DeclSuite extends ParseSuite {
  test("val x: Int") {
    val Decl.Val(Nil, List(Term.Name("x")), Type.Name("Int")) = templStat("val x: Int")
  }

  test("var x: Int") {
    val Decl.Var(Nil, List(Term.Name("x")), Type.Name("Int")) = templStat("var x: Int")
  }

  test("val x, y: Int") {
    val Decl.Val(Nil, List(Term.Name("x"), Term.Name("y")), Type.Name("Int")) =
      templStat("val x, y: Int")
    val Decl.Var(Nil, List(Term.Name("x"), Term.Name("y")), Type.Name("Int")) =
      templStat("var x, y: Int")
  }

  test("var x, y: Int") {
    val Decl.Var(Nil, List(Term.Name("x"), Term.Name("y")), Type.Name("Int")) =
      templStat("var x, y: Int")
  }

  test("type T") {
    val Decl.Type(Nil, Type.Name("T"), Nil, TypeBounds(None, None)) = templStat("type T")
  }

  test("type T <: hi") {
    val Decl.Type(Nil, Type.Name("T"), Nil,
                  TypeBounds(None, Some(Type.Name("hi")))) = templStat("type T <: hi")
  }

  test("type T >: lo") {
    val Decl.Type(Nil, Type.Name("T"), Nil,
                  TypeBounds(Some(Type.Name("lo")), None)) = templStat("type T >: lo")
  }

  test("type T >: lo <: hi") {
    val Decl.Type(Nil, Type.Name("T"), Nil,
                  TypeBounds(Some(Type.Name("lo")),
                             Some(Type.Name("hi")))) = templStat("type T >: lo <: hi")
  }

  test("type F[T]") {
   val Decl.Type(Nil, Type.Name("F"),
                 TypeParam.Named(Nil, Type.Name("T"),
                                 Nil, Nil, Nil, TypeBounds(None, None)) :: Nil,
                 TypeBounds(None, None)) = templStat("type F[T]")
  }

  test("type F[_]") {
    val Decl.Type(Nil, Type.Name("F"),
                  TypeParam.Anonymous(Nil, Nil, Nil, Nil, TypeBounds(None, None)) :: Nil,
                  TypeBounds(None, None)) = templStat("type F[_]")
  }

  test("type F[A <: B]") {
    val Decl.Type(Nil, Type.Name("F"),
                  TypeParam.Named(Nil, Type.Name("T"),
                                  Nil, Nil, Nil,
                                  TypeBounds(None, Some(Type.Name("B")))) :: Nil,
                  TypeBounds(None, None)) = templStat("type F[T <: B]")
  }

  test("type F[+T]") {
    val Decl.Type(Nil, Type.Name("F"),
                  TypeParam.Named(Mod.Covariant() :: Nil, Type.Name("T"),
                                  Nil, Nil, Nil, TypeBounds(None, None)) :: Nil,
                  TypeBounds(None, None)) = templStat("type F[+T]")
    val Decl.Type(Nil, Type.Name("F"),
                  TypeParam.Named(Mod.Contravariant() :: Nil, Type.Name("T"),
                                  Nil, Nil, Nil, TypeBounds(None, None)) :: Nil,
                  TypeBounds(None, None)) = templStat("type F[-T]")
  }

  test("def f") {
    val Decl.Procedure(Nil, Term.Name("f"), Nil, Nil, Nil) =
      templStat("def f")
  }

  test("def f(x: Int)") {
    val Decl.Procedure(Nil, Term.Name("f"), Nil,
                       (Param.Named(Nil, Term.Name("x"),
                                    Some(Type.Name("Int")),
                                    None) :: Nil) :: Nil, Nil) =
      templStat("def f(x: Int)")
  }

  test("def f(x: Int*)") {
    val Decl.Procedure(Nil, Term.Name("f"), Nil,
                       (Param.Named(Nil, Term.Name("x"),
                                    Some(ParamType.Repeated(Type.Name("Int"))),
                                    None) :: Nil) :: Nil, Nil) =
      templStat("def f(x: Int*)")
  }

  test("def f(x: => Int)") {
    val Decl.Procedure(Nil, Term.Name("f"), Nil,
                       (Param.Named(Nil, Term.Name("x"),
                                    Some(ParamType.ByName(Type.Name("Int"))),
                                    None) :: Nil) :: Nil, Nil) =
      templStat("def f(x: => Int)")
  }


  test("def f(implicit x: Int)") {
    val Decl.Procedure(Nil, Term.Name("f"), Nil, Nil,
                       Param.Named(Nil, Term.Name("x"),
                                   Some(Type.Name("Int")),
                                   None) :: Nil) =
      templStat("def f(implicit x: Int)")
  }

  test("def f: X") {
    val Decl.Def(Nil, Term.Name("f"), Nil, Nil, Nil, Type.Name("X")) =
      templStat("def f: X")
  }

  test("def f[T]: T") {
    val Decl.Def(Nil, Term.Name("f"),
                 TypeParam.Named(Nil, Type.Name("T"), Nil, Nil, Nil, TypeBounds(None, None)) :: Nil,
                 Nil, Nil, Type.Name("T")) =
      templStat("def f[T]: T")
  }
}

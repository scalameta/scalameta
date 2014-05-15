import scala.reflect.core._, Aux._

class DeclSuite extends ParseSuite {
  test("val x: Int") {
    val Decl.Val(Nil, List(Term.Name("x", false)), Type.Name("Int", false)) = templStat("val x: Int")
  }

  test("var x: Int") {
    val Decl.Var(Nil, List(Term.Name("x", false)), Type.Name("Int", false)) = templStat("var x: Int")
  }

  test("val x, y: Int") {
    val Decl.Val(Nil, List(Term.Name("x", false), Term.Name("y", false)), Type.Name("Int", false)) =
      templStat("val x, y: Int")
    val Decl.Var(Nil, List(Term.Name("x", false), Term.Name("y", false)), Type.Name("Int", false)) =
      templStat("var x, y: Int")
  }

  test("var x, y: Int") {
    val Decl.Var(Nil, List(Term.Name("x", false), Term.Name("y", false)), Type.Name("Int", false)) =
      templStat("var x, y: Int")
  }

  test("type T") {
    val Decl.Type(Nil, Type.Name("T", false), Nil, TypeBounds.empty) = templStat("type T")
  }

  test("type T <: hi") {
    val Decl.Type(Nil, Type.Name("T", false), Nil,
                  TypeBounds(None, Some(Type.Name("hi", false)))) = templStat("type T <: hi")
  }

  test("type T >: lo") {
    val Decl.Type(Nil, Type.Name("T", false), Nil,
                  TypeBounds(Some(Type.Name("lo", false)), None)) = templStat("type T >: lo")
  }

  test("type T >: lo <: hi") {
    val Decl.Type(Nil, Type.Name("T", false), Nil,
                  TypeBounds(Some(Type.Name("lo", false)),
                             Some(Type.Name("hi", false)))) = templStat("type T >: lo <: hi")
  }

  test("type F[T]") {
   val Decl.Type(Nil, Type.Name("F", false),
                 TypeParam.Named(Type.Name("T", false),
                                 Nil, Nil, Nil, TypeBounds.empty, Nil) :: Nil,
                 TypeBounds.empty) = templStat("type F[T]")
  }

  test("type F[_]") {
    val Decl.Type(Nil, Type.Name("F", false),
                  TypeParam.Anonymous(Nil, Nil, Nil, TypeBounds.empty, Nil) :: Nil,
                  TypeBounds.empty) = templStat("type F[_]")
  }

  test("type F[A <: B]") {
    val Decl.Type(Nil, Type.Name("F", false),
                  TypeParam.Named(Type.Name("T", false),
                                  Nil, Nil, Nil,
                                  TypeBounds(None, Some(Type.Name("B", false))), Nil) :: Nil,
                  TypeBounds.empty) = templStat("type F[T <: B]")
  }

  test("type F[+T]") {
    val Decl.Type(Nil, Type.Name("F", false),
                  TypeParam.Named(Type.Name("T", false),
                                  Nil, Nil, Nil, TypeBounds.empty, Mod.Covariant() :: Nil) :: Nil,
                  TypeBounds.empty) = templStat("type F[+T]")
    val Decl.Type(Nil, Type.Name("F", false),
                  TypeParam.Named(Type.Name("T", false),
                                  Nil, Nil, Nil, TypeBounds.empty, Mod.Contravariant() :: Nil) :: Nil,
                  TypeBounds.empty) = templStat("type F[-T]")
  }

  test("def f") {
    val Decl.Procedure(Nil, Term.Name("f", false), Nil, Nil, Nil) =
      templStat("def f")
  }

  test("def f(x: Int)") {
    val Decl.Procedure(Nil, Term.Name("f", false), Nil,
                       (Param.Named(Term.Name("x", false),
                                    Some(Type.Name("Int", false)),
                                    None, Nil) :: Nil) :: Nil, Nil) =
      templStat("def f(x: Int)")
  }

  test("def f(x: Int*)") {
    val Decl.Procedure(Nil, Term.Name("f", false), Nil,
                       (Param.Named(Term.Name("x", false),
                                    Some(ParamType.Repeated(Type.Name("Int", false))),
                                    None, Nil) :: Nil) :: Nil, Nil) =
      templStat("def f(x: Int*)")
  }

  test("def f(x: => Int)") {
    val Decl.Procedure(Nil, Term.Name("f", false), Nil,
                       (Param.Named(Term.Name("x", false),
                                    Some(ParamType.ByName(Type.Name("Int", false))),
                                    None, Nil) :: Nil) :: Nil, Nil) =
      templStat("def f(x: => Int)")
  }


  test("def f(implicit x: Int)") {
    val Decl.Procedure(Nil, Term.Name("f", false), Nil, Nil,
                       Param.Named(Term.Name("x", false),
                                   Some(Type.Name("Int", false)),
                                   None, Nil) :: Nil) =
      templStat("def f(implicit x: Int)")
  }

  test("def f: X") {
    val Decl.Def(Nil, Term.Name("f", false), Nil, Nil, Nil, Type.Name("X", false)) =
      templStat("def f: X")
  }

  test("def f[T]: T") {
    val Decl.Def(Nil, Term.Name("f", false),
                 TypeParam.Named(Type.Name("T", false), Nil, Nil, Nil, TypeBounds.empty, Nil) :: Nil,
                 Nil, Nil, Type.Name("T", false)) =
      templStat("def f[T]: T")
  }
}

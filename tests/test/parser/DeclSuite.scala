import scala.meta._, Aux._

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
    val Decl.Type(Nil, Type.Name("T"), Nil, bounds: TypeBounds) = templStat("type T")
    assert(bounds.hasLo === false)
    assert(bounds.hasHi === false)
  }

  test("type T <: hi") {
    val Decl.Type(Nil, Type.Name("T"), Nil,
                  bounds @ TypeBounds(Type.Name("Nothing"), Type.Name("hi"))) = templStat("type T <: hi")
    assert(bounds.hasLo === false)
    assert(bounds.hasHi === true)
  }

  test("type T >: lo") {
    val Decl.Type(Nil, Type.Name("T"), Nil,
                  bounds @ TypeBounds(Type.Name("lo"), Type.Name("Any"))) = templStat("type T >: lo")
    assert(bounds.hasLo === true)
    assert(bounds.hasHi === false)
  }

  test("type T >: lo <: hi") {
    val Decl.Type(Nil, Type.Name("T"), Nil,
                  bounds @ TypeBounds(Type.Name("lo"), Type.Name("hi"))) = templStat("type T >: lo <: hi")
    assert(bounds.hasLo === true)
    assert(bounds.hasHi === true)
  }

  test("type F[T]") {
   val Decl.Type(Nil, Type.Name("F"),
                 Param.Type(Nil, Some(Type.Name("T")),
                            Nil, Nil, Nil, EmptyBounds()) :: Nil,
                 EmptyBounds()) = templStat("type F[T]")
  }

  test("type F[_]") {
    val Decl.Type(Nil, Type.Name("F"),
                  Param.Type(Nil, None, Nil, Nil, Nil, EmptyBounds()) :: Nil,
                  EmptyBounds()) = templStat("type F[_]")
  }

  test("type F[A <: B]") {
    val Decl.Type(Nil, Type.Name("F"),
                  Param.Type(Nil, Some(Type.Name("T")),
                             Nil, Nil, Nil,
                             TypeBounds(Type.Name("Nothing"), Type.Name("B"))) :: Nil,
                  EmptyBounds()) = templStat("type F[T <: B]")
  }

  test("type F[+T]") {
    val Decl.Type(Nil, Type.Name("F"),
                  Param.Type(Mod.Covariant() :: Nil, Some(Type.Name("T")),
                             Nil, Nil, Nil, EmptyBounds()) :: Nil,
                  EmptyBounds()) = templStat("type F[+T]")
    val Decl.Type(Nil, Type.Name("F"),
                  Param.Type(Mod.Contravariant() :: Nil, Some(Type.Name("T")),
                             Nil, Nil, Nil, EmptyBounds()) :: Nil,
                  EmptyBounds()) = templStat("type F[-T]")
  }

  test("def f") {
    val Decl.Procedure(Nil, Term.Name("f"), Nil, Nil) =
      templStat("def f")
  }

  test("def f(x: Int)") {
    val Decl.Procedure(Nil, Term.Name("f"), Nil,
                       (Param.Term.Simple(Nil, Some(Term.Name("x")),
                                          Some(Type.Name("Int")),
                                          None) :: Nil) :: Nil) =
      templStat("def f(x: Int)")
  }

  test("def f(x: Int*)") {
    val Decl.Procedure(Nil, Term.Name("f"), Nil,
                       (Param.Term.Simple(Nil, Some(Term.Name("x")),
                                          Some(Type.Arg.Repeated(Type.Name("Int"))),
                                          None) :: Nil) :: Nil) =
      templStat("def f(x: Int*)")
  }

  test("def f(x: => Int)") {
    val Decl.Procedure(Nil, Term.Name("f"), Nil,
                       (Param.Term.Simple(Nil, Some(Term.Name("x")),
                                          Some(Type.Arg.ByName(Type.Name("Int"))),
                                          None) :: Nil) :: Nil) =
      templStat("def f(x: => Int)")
  }


  test("def f(implicit x: Int)") {
    val Decl.Procedure(Nil, Term.Name("f"), Nil,
                       (Param.Term.Simple(Mod.Implicit() :: Nil, Some(Term.Name("x")),
                                          Some(Type.Name("Int")),
                                          None) :: Nil) :: Nil) =
      templStat("def f(implicit x: Int)")
  }

  test("def f: X") {
    val Decl.Def(Nil, Term.Name("f"), Nil, Nil, Type.Name("X")) =
      templStat("def f: X")
  }

  test("def f[T]: T") {
    val Decl.Def(Nil, Term.Name("f"),
                 Param.Type(Nil, Some(Type.Name("T")), Nil, Nil, Nil, EmptyBounds()) :: Nil,
                 Nil, Type.Name("T")) =
      templStat("def f[T]: T")
  }
}

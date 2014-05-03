import scala.reflect.core._

class DeclSuite extends ParseSuite {
  import Aux._

  test("val x: Int") {
    val Decl.Val(Nil, List(Term.Ident("x", false)), Type.Ident("Int", false)) = templStat("val x: Int")
  }

  test("var x: Int") {
    val Decl.Var(Nil, List(Term.Ident("x", false)), Type.Ident("Int", false)) = templStat("var x: Int")
  }

  test("val x, y: Int") {
    val Decl.Val(Nil, List(Term.Ident("x", false), Term.Ident("y", false)), Type.Ident("Int", false)) =
      templStat("val x, y: Int")
    val Decl.Var(Nil, List(Term.Ident("x", false), Term.Ident("y", false)), Type.Ident("Int", false)) =
      templStat("var x, y: Int")
  }

  test("var x, y: Int") {
    val Decl.Var(Nil, List(Term.Ident("x", false), Term.Ident("y", false)), Type.Ident("Int", false)) =
      templStat("var x, y: Int")
  }

  test("type T") {
    val Decl.Type(Nil, Type.Ident("T", false), Nil, TypeBounds.empty) = templStat("type T")
  }

  test("type T <: hi") {
    val Decl.Type(Nil, Type.Ident("T", false), Nil,
                  TypeBounds(None, Some(Type.Ident("hi", false)))) = templStat("type T <: hi")
  }

  test("type T >: lo") {
    val Decl.Type(Nil, Type.Ident("T", false), Nil,
                  TypeBounds(Some(Type.Ident("lo", false)), None)) = templStat("type T >: lo")
  }

  test("type T >: lo <: hi") {
    val Decl.Type(Nil, Type.Ident("T", false), Nil,
                  TypeBounds(Some(Type.Ident("lo", false)),
                             Some(Type.Ident("hi", false)))) = templStat("type T >: lo <: hi")
  }

  test("type F[T]") {
   val Decl.Type(Nil, Type.Ident("F", false),
                 TypeParam(Nil, Some(Type.Ident("T", false)),
                           Nil, Nil, Nil, TypeBounds.empty) :: Nil,
                 TypeBounds.empty) = templStat("type F[T]")
  }

  test("type F[_]") {
    val Decl.Type(Nil, Type.Ident("F", false),
                  TypeParam(Nil, None, Nil, Nil, Nil, TypeBounds.empty) :: Nil,
                  TypeBounds.empty) = templStat("type F[_]")
  }

  test("type F[A <: B]") {
    val Decl.Type(Nil, Type.Ident("F", false),
                  TypeParam(Nil, Some(Type.Ident("T", false)),
                            Nil, Nil, Nil,
                            TypeBounds(None, Some(Type.Ident("B", false)))) :: Nil,
                  TypeBounds.empty) = templStat("type F[T <: B]")
  }

  test("type F[+T]") {
    val Decl.Type(Nil, Type.Ident("F", false),
                  TypeParam(Mod.Covariant() :: Nil, Some(Type.Ident("T", false)),
                            Nil, Nil, Nil, TypeBounds.empty) :: Nil,
                  TypeBounds.empty) = templStat("type F[+T]")
    val Decl.Type(Nil, Type.Ident("F", false),
                  TypeParam(Mod.Contravariant() :: Nil, Some(Type.Ident("T", false)),
                            Nil, Nil, Nil, TypeBounds.empty) :: Nil,
                  TypeBounds.empty) = templStat("type F[-T]")
  }
}

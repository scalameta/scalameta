import scala.reflect.core._

class TypeSuite extends ParseSuite {
  import Type._, Aux._

  test("T") {
    val Ident("T", false) = tpe("T")
  }

  test("F[T]") {
    val Apply(Ident("F", false), Ident("T", false) :: Nil) = tpe("F[T]")
  }

  test("F#T") {
    val Project(Ident("F", false), Ident("T", false)) = tpe("F#T")
  }

  test("f.T") {
    val Select(Term.Ident("f", false), Type.Ident("T", false)) = tpe("f.T")
  }

  test("f.type") {
    val Singleton(Term.Ident("f", false)) = tpe("f.type")
  }

  test("super.T") {
    val SuperSelect(None, None, Ident("T", false)) = tpe("super.T")
  }

  test("this.T") {
    val Select(Term.This(None), Ident("T", false)) = tpe("this.T")
  }

  test("(A, B)") {
    val Tuple(Type.Ident("A", false) :: Type.Ident("B", false) :: Nil) = tpe("(A, B)")
  }

  test("(A, B) => C") {
    val Function(Type.Ident("A", false) :: Type.Ident("B", false) :: Nil, Type.Ident("C", false)) =
      tpe("(A, B) => C")
  }

  test("T @foo") {
    val Annotate(Type.Ident("T", false), Mod.Annot(Type.Ident("foo", false), Nil) :: Nil) =
      tpe("T @foo")
  }

  test("A with B") {
    val Compound(Type.Ident("A", false) :: Type.Ident("B", false) :: Nil, Nil) = tpe("A with B")
  }

  test("{}") {
    val Compound(Nil, Nil) = tpe("{}")
  }

  test("A { def x: A; val y: B; type C }") {
    val Compound(Type.Ident("A", false) :: Nil,
                 Decl.Def(Nil, Term.Ident("x", false),
                          Nil, Nil, Nil, Type.Ident("Int", false)) ::
                 Decl.Val(Nil, List(Term.Ident("y", false)), Type.Ident("B", false)) ::
                 Decl.Type(Nil, Type.Ident("C", false), Nil, TypeBounds.empty) :: Nil) =
      tpe("A { def x: Int; val y: B; type C }")
  }

  test("F[_ >: lo <: hi]") {
    val Apply(Ident("F", false),
              Placeholder(TypeBounds(Some(Type.Ident("lo", false)),
                                     Some(Type.Ident("hi", false)))) :: Nil) =
      tpe("F[_ >: lo <: hi]")
  }

  test("F[_ >: lo") {
    val Apply(Ident("F", false),
              Placeholder(TypeBounds(Some(Type.Ident("lo", false)), None)) :: Nil) =
      tpe("F[_ >: lo]")
  }

  test("F[_ <: hi]") {
    val Apply(Ident("F", false),
              Placeholder(TypeBounds(None, Some(Type.Ident("hi", false)))) :: Nil) =
      tpe("F[_ <: hi]")
  }

  test("F[_]") {
    val Apply(Ident("F", false), Placeholder(TypeBounds.empty) :: Nil) =
      tpe("F[_]")
  }

  test("F[T] forSome { type T }") {
    val Existential(Apply(Ident("F", false), Ident("T", false) :: Nil),
                    Decl.Type(Nil, Ident("T", false), Nil, TypeBounds.empty) :: Nil) =
      tpe("F[T] forSome { type T }")
  }

  test("a.T forSome { val a: A }") {
    val Existential(Select(Term.Ident("a", false), Type.Ident("T", false)),
                    Decl.Val(Nil, Term.Ident("a", false) :: Nil, Type.Ident("A", false)) :: Nil) =
      tpe("a.T forSome { val a: A }")
  }
}

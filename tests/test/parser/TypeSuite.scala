import scala.reflect.core._, Type._, Aux._

class TypeSuite extends ParseSuite {
  test("T") {
    val Name("T", false) = tpe("T")
  }

  test("F[T]") {
    val Apply(Name("F", false), Name("T", false) :: Nil) = tpe("F[T]")
  }

  test("F#T") {
    val Project(Name("F", false), Name("T", false)) = tpe("F#T")
  }

  // TODO:
  // test("A * B") {
  //   val Apply(Name("*", false), Name("A", false) :: Name("B", false) :: Nil) = tpe("A * B")
  // }

  test("f.T") {
    val Select(Term.Name("f", false), Type.Name("T", false)) = tpe("f.T")
  }

  test("f.type") {
    val Singleton(Term.Name("f", false)) = tpe("f.type")
  }

  test("super.T") {
    val SuperSelect(None, None, Name("T", false)) = tpe("super.T")
  }

  test("this.T") {
    val Select(Term.This(None), Name("T", false)) = tpe("this.T")
  }

  test("(A, B)") {
    val Tuple(Type.Name("A", false) :: Type.Name("B", false) :: Nil) = tpe("(A, B)")
  }

  test("(A, B) => C") {
    val Function(Type.Name("A", false) :: Type.Name("B", false) :: Nil, Type.Name("C", false)) =
      tpe("(A, B) => C")
  }

  test("T @foo") {
    val Annotate(Type.Name("T", false), Mod.Annot(Type.Name("foo", false), Nil) :: Nil) =
      tpe("T @foo")
  }

  test("A with B") {
    val Compound(Type.Name("A", false) :: Type.Name("B", false) :: Nil, Nil) = tpe("A with B")
  }

  test("{}") {
    val Compound(Nil, Nil) = tpe("{}")
  }

  test("A { def x: A; val y: B; type C }") {
    val Compound(Type.Name("A", false) :: Nil,
                 Decl.Def(Nil, Term.Name("x", false),
                          Nil, Nil, Nil, Type.Name("Int", false)) ::
                 Decl.Val(Nil, List(Term.Name("y", false)), Type.Name("B", false)) ::
                 Decl.Type(Nil, Type.Name("C", false), Nil, TypeBounds.empty) :: Nil) =
      tpe("A { def x: Int; val y: B; type C }")
  }

  test("F[_ >: lo <: hi]") {
    val Apply(Name("F", false),
              Placeholder(TypeBounds(Some(Type.Name("lo", false)),
                                     Some(Type.Name("hi", false)))) :: Nil) =
      tpe("F[_ >: lo <: hi]")
  }

  test("F[_ >: lo") {
    val Apply(Name("F", false),
              Placeholder(TypeBounds(Some(Type.Name("lo", false)), None)) :: Nil) =
      tpe("F[_ >: lo]")
  }

  test("F[_ <: hi]") {
    val Apply(Name("F", false),
              Placeholder(TypeBounds(None, Some(Type.Name("hi", false)))) :: Nil) =
      tpe("F[_ <: hi]")
  }

  test("F[_]") {
    val Apply(Name("F", false), Placeholder(TypeBounds.empty) :: Nil) =
      tpe("F[_]")
  }

  test("F[T] forSome { type T }") {
    val Existential(Apply(Name("F", false), Name("T", false) :: Nil),
                    Decl.Type(Nil, Name("T", false), Nil, TypeBounds.empty) :: Nil) =
      tpe("F[T] forSome { type T }")
  }

  test("a.T forSome { val a: A }") {
    val Existential(Select(Term.Name("a", false), Type.Name("T", false)),
                    Decl.Val(Nil, Term.Name("a", false) :: Nil, Type.Name("A", false)) :: Nil) =
      tpe("a.T forSome { val a: A }")
  }
}

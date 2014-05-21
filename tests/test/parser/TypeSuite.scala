import scala.reflect.core._, Type._, Aux._

class TypeSuite extends ParseSuite {
  test("T") {
    val Name("T") = tpe("T")
  }

  test("F[T]") {
    val Apply(Name("F"), Name("T") :: Nil) = tpe("F[T]")
  }

  test("F#T") {
    val Project(Name("F"), Name("T")) = tpe("F#T")
  }

  // test("A * B") {
  //   val ApplyInfix(Name("A"), Name("*"), Name("B")) = tpe("A * B")
  // }

  test("A \\/ B") {
    val ApplyInfix(Name("A"), Name("\\/"), Name("B")) = tpe("A \\/ B")
  }

  test("f.T") {
    val Select(Term.Name("f"), Type.Name("T")) = tpe("f.T")
  }

  test("f.type") {
    val Singleton(Term.Name("f")) = tpe("f.type")
  }

  test("super.T") {
    val SuperSelect(None, None, Name("T")) = tpe("super.T")
  }

  test("this.T") {
    val Select(Term.This(None), Name("T")) = tpe("this.T")
  }

  test("(A, B)") {
    val Tuple(Type.Name("A") :: Type.Name("B") :: Nil) = tpe("(A, B)")
  }

  test("(A, B) => C") {
    val Function(Type.Name("A") :: Type.Name("B") :: Nil, Type.Name("C")) =
      tpe("(A, B) => C")
  }

  test("T @foo") {
    val Annotate(Type.Name("T"), Mod.Annot(Type.Name("foo"), Nil) :: Nil) =
      tpe("T @foo")
  }

  test("A with B") {
    val comp @ Compound(Type.Name("A") :: Type.Name("B") :: Nil, Nil) = tpe("A with B")
    assert(comp.hasExplicitRefinement == false)
  }

  test("A with B {}") {
    val comp @ Compound(Type.Name("A") :: Type.Name("B") :: Nil, Nil) = tpe("A with B {}")
    assert(comp.hasExplicitRefinement == true)
  }

  test("{}") {
    val comp @ Compound(Nil, Nil) = tpe("{}")
    assert(comp.hasExplicitRefinement == true)
  }

  test("A { def x: A; val y: B; type C }") {
    val Compound(Type.Name("A") :: Nil,
                 Decl.Def(Nil, Term.Name("x"),
                          Nil, Nil, Nil, Type.Name("Int")) ::
                 Decl.Val(Nil, List(Term.Name("y")), Type.Name("B")) ::
                 Decl.Type(Nil, Type.Name("C"), Nil, TypeBounds(None, None)) :: Nil) =
      tpe("A { def x: Int; val y: B; type C }")
  }

  test("F[_ >: lo <: hi]") {
    val Apply(Name("F"),
              Placeholder(TypeBounds(Some(Type.Name("lo")),
                                     Some(Type.Name("hi")))) :: Nil) =
      tpe("F[_ >: lo <: hi]")
  }

  test("F[_ >: lo") {
    val Apply(Name("F"),
              Placeholder(TypeBounds(Some(Type.Name("lo")), None)) :: Nil) =
      tpe("F[_ >: lo]")
  }

  test("F[_ <: hi]") {
    val Apply(Name("F"),
              Placeholder(TypeBounds(None, Some(Type.Name("hi")))) :: Nil) =
      tpe("F[_ <: hi]")
  }

  test("F[_]") {
    val Apply(Name("F"), Placeholder(TypeBounds(None, None)) :: Nil) =
      tpe("F[_]")
  }

  test("F[T] forSome { type T }") {
    val Existential(Apply(Name("F"), Name("T") :: Nil),
                    Decl.Type(Nil, Name("T"), Nil, TypeBounds(None, None)) :: Nil) =
      tpe("F[T] forSome { type T }")
  }

  test("a.T forSome { val a: A }") {
    val Existential(Select(Term.Name("a"), Type.Name("T")),
                    Decl.Val(Nil, Term.Name("a") :: Nil, Type.Name("A")) :: Nil) =
      tpe("a.T forSome { val a: A }")
  }
}

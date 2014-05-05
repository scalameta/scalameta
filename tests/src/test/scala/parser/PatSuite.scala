import scala.reflect.core._, Pat._, Term.Ident

class PatSuite extends ParseSuite {
  test("_") {
    val Wildcard() = pat("_")
  }

  test("a @ _") {
    val Bind(Ident("a", false), Wildcard()) = pat("a @ _")
  }

  test("a") {
    val Ident("a", false) = pat("a")
  }

  test("a: Int") {
    val Typed(Ident("a", false), Type.Ident("Int", false)) = pat("a: Int")
  }

  test("_: Int") {
    val Typed(Wildcard(), Type.Ident("Int", false)) = pat("_: Int")
  }

  test("_: F[t]") {
    val Typed(Wildcard(), Type.Apply(Type.Ident("F", false), Type.Ident("t", false) :: Nil)) = pat("_: F[t]")
  }

  test("foo(x)") {
    val Extract(Ident("foo", false), Nil, Ident("x", false) :: Nil) = pat("foo(x)")
  }

  test("foo(_*)") {
    val Extract(Ident("foo", false), Nil, SeqWildcard() :: Nil) = pat("foo(_*)")
  }

  test("foo(x @ _*)") {
    val Extract(Ident("foo", false), Nil,
                Bind(Ident("x", false), SeqWildcard()) :: Nil) = pat("foo(x @ _*)")
  }

  test("1 | 2 | 3") {
    val Alternative(Lit.Int(1), Lit.Int(2)) = pat("1 | 2")
    val Alternative(Lit.Int(1), Alternative(Lit.Int(2), Lit.Int(3))) = pat("1 | 2 | 3")
  }

  test("(true, false)") {
    val Tuple(Lit.True() :: Lit.False() :: Nil) = pat("(true, false)")
  }

  test("foo\"bar\"") {
    val Interpolate(Ident("foo", false),
                    Lit.String("bar") :: Nil, Nil) = pat("foo\"bar\"")
  }

  test("foo\"a $b c\"") {
    val Interpolate(Ident("foo", false),
                    Lit.String("a ") :: Lit.String(" c") :: Nil,
                    Ident("b", false) :: Nil) = pat("foo\"a $b c\"")
  }

  test("foo\"${b @ foo()}\"") {
    val Interpolate(Ident("foo", false),
                    Lit.String("") :: Lit.String("") :: Nil,
                    Bind(Ident("b", false),
                         Extract(Ident("foo", false), Nil, Nil)) :: Nil) = pat("foo\"${b @ foo()}\"")

  }
}

import scala.reflect.core._, Pat._, Term.Name

class PatSuite extends ParseSuite {
  test("_") {
    val Wildcard() = pat("_")
  }

  test("a @ _") {
    val Bind(Name("a", false), Wildcard()) = pat("a @ _")
  }

  test("a") {
    val Name("a", false) = pat("a")
  }

  test("a: Int") {
    val Typed(Name("a", false), Type.Name("Int", false)) = pat("a: Int")
  }

  test("_: Int") {
    val Typed(Wildcard(), Type.Name("Int", false)) = pat("_: Int")
  }

  test("_: F[t]") {
    val Typed(Wildcard(), Type.Apply(Type.Name("F", false), Type.Name("t", false) :: Nil)) = pat("_: F[t]")
  }

  test("foo(x)") {
    val Extract(Name("foo", false), Nil, Name("x", false) :: Nil) = pat("foo(x)")
  }

  test("foo(_*)") {
    val Extract(Name("foo", false), Nil, SeqWildcard() :: Nil) = pat("foo(_*)")
  }

  test("foo(x @ _*)") {
    val Extract(Name("foo", false), Nil,
                Bind(Name("x", false), SeqWildcard()) :: Nil) = pat("foo(x @ _*)")
  }

  test("1 | 2 | 3") {
    val Alternative(Lit.Int(1), Lit.Int(2)) = pat("1 | 2")
    val Alternative(Lit.Int(1), Alternative(Lit.Int(2), Lit.Int(3))) = pat("1 | 2 | 3")
  }

  test("(true, false)") {
    val Tuple(Lit.True() :: Lit.False() :: Nil) = pat("(true, false)")
  }

  test("foo\"bar\"") {
    val Interpolate(Name("foo", false),
                    Lit.String("bar") :: Nil, Nil) = pat("foo\"bar\"")
  }

  test("foo\"a $b c\"") {
    val Interpolate(Name("foo", false),
                    Lit.String("a ") :: Lit.String(" c") :: Nil,
                    Name("b", false) :: Nil) = pat("foo\"a $b c\"")
  }

  test("foo\"${b @ foo()}\"") {
    val Interpolate(Name("foo", false),
                    Lit.String("") :: Lit.String("") :: Nil,
                    Bind(Name("b", false),
                         Extract(Name("foo", false), Nil, Nil)) :: Nil) = pat("foo\"${b @ foo()}\"")

  }
}

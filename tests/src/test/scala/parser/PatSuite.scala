import scala.meta.syntactic.ast._, Pat._, Term.Name

class PatSuite extends ParseSuite {
  test("_") {
    val Wildcard() = pat("_")
  }

  test("a @ _") {
    val Bind(Name("a"), Wildcard()) = pat("a @ _")
  }

  test("a") {
    val Name("a") = pat("a")
  }

  test("a: Int") {
    val Typed(Name("a"), Type.Name("Int")) = pat("a: Int")
  }

  test("_: Int") {
    val Typed(Wildcard(), Type.Name("Int")) = pat("_: Int")
  }

  test("_: F[t]") {
    val Typed(Wildcard(), Type.Apply(Type.Name("F"), Type.Name("t") :: Nil)) = pat("_: F[t]")
  }

  test("foo(x)") {
    val Extract(Name("foo"), Nil, Name("x") :: Nil) = pat("foo(x)")
  }

  test("foo(_*)") {
    val Extract(Name("foo"), Nil, Arg.SeqWildcard() :: Nil) = pat("foo(_*)")
  }

  test("foo(x @ _*)") {
    val Extract(Name("foo"), Nil,
                Bind(Name("x"), Arg.SeqWildcard()) :: Nil) = pat("foo(x @ _*)")
  }

  test("a :: b") {
    val ExtractInfix(Name("a"), Name("::"), Name("b") :: Nil) = pat("a :: b")
  }

  test("1 | 2 | 3") {
    val Alternative(Lit.Int(1), Lit.Int(2)) = pat("1 | 2")
    val Alternative(Lit.Int(1), Alternative(Lit.Int(2), Lit.Int(3))) = pat("1 | 2 | 3")
  }

  test("(true, false)") {
    val Tuple(Lit.Bool(true) :: Lit.Bool(false) :: Nil) = pat("(true, false)")
  }

  test("foo\"bar\"") {
    val Interpolate(Name("foo"),
                    Lit.String("bar") :: Nil, Nil) = pat("foo\"bar\"")
  }

  test("foo\"a $b c\"") {
    val Interpolate(Name("foo"),
                    Lit.String("a ") :: Lit.String(" c") :: Nil,
                    Name("b") :: Nil) = pat("foo\"a $b c\"")
  }

  test("foo\"${b @ foo()}\"") {
    val Interpolate(Name("foo"),
                    Lit.String("") :: Lit.String("") :: Nil,
                    Bind(Name("b"),
                         Extract(Name("foo"), Nil, Nil)) :: Nil) = pat("foo\"${b @ foo()}\"")

  }
}

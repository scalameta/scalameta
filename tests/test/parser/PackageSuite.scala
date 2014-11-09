import scala.meta.syntactic.ast._, Defn.Class

class PackageSuite extends ParseSuite {
  test("class C") {
    val TopLevel(Class(Nil, Type.Name("C"), Nil,
                       Ctor.Primary(Nil, Nil),
                       EmptyTemplate()) :: Nil) = topLevel("class C")
  }

  test("package foo; class C") {
    val TopLevel((pkgfoo @ Pkg(Term.Name("foo"),
                               Class(Nil, Type.Name("C"), Nil,
                                     Ctor.Primary(Nil, Nil),
                                     EmptyTemplate()) :: Nil)) :: Nil) =
      topLevel("package foo; class C")
    assert(pkgfoo.hasBraces === false)
  }

  test("package foo { class C }") {
    val TopLevel((pkgfoo @Pkg(Term.Name("foo"),
                              Class(Nil, Type.Name("C"), Nil,
                                    Ctor.Primary(Nil, Nil),
                                    EmptyTemplate()) :: Nil)) :: Nil) =
      topLevel("package foo { class C }")
    assert(pkgfoo.hasBraces === true)
  }

  test("package foo.bar; class C") {
    val TopLevel((pkgfoobar @ Pkg(Term.Select(Term.Name("foo"), Term.Name("bar")),
                                  Class(Nil, Type.Name("C"), Nil,
                                        Ctor.Primary(Nil, Nil),
                                        EmptyTemplate()) :: Nil)) :: Nil) =
      topLevel("package foo.bar; class C")
    assert(pkgfoobar.hasBraces === false)
  }

  test("package foo.bar { class C }") {
    val TopLevel((pkgfoobar @ Pkg(Term.Select(Term.Name("foo"), Term.Name("bar")),
                                  Class(Nil, Type.Name("C"), Nil,
                                        Ctor.Primary(Nil, Nil),
                                        EmptyTemplate()) :: Nil)) :: Nil) =
      topLevel("package foo.bar { class C }")
    assert(pkgfoobar.hasBraces === true)
  }

  test("package foo; package bar; class C") {
    val TopLevel((pkgfoo @ Pkg(Term.Name("foo"),
                               (pkgbar @ Pkg(Term.Name("bar"),
                                             Class(Nil, Type.Name("C"), Nil,
                                                   Ctor.Primary(Nil, Nil),
                                                   EmptyTemplate()) :: Nil)) :: Nil)) :: Nil) =
      topLevel("package foo; package bar; class C")
    assert(pkgfoo.hasBraces === false)
    assert(pkgbar.hasBraces === false)
  }

  test("package foo { package bar { class C } }") {
    val TopLevel((pkgfoo @ Pkg(Term.Name("foo"),
                               (pkgbar @ Pkg(Term.Name("bar"),
                                             Class(Nil, Type.Name("C"), Nil,
                                                   Ctor.Primary(Nil, Nil),
                                                   EmptyTemplate()) :: Nil)) :: Nil)) :: Nil) =
      topLevel("package foo { package bar { class C } }")
    assert(pkgfoo.hasBraces === true)
    assert(pkgbar.hasBraces === true)
  }

  test("package foo {}; package bar {}") {
    val TopLevel((pkgfoo @ Pkg(Term.Name("foo"), Nil)) ::
                 (pkgbar @ Pkg(Term.Name("bar"), Nil)) :: Nil) =
      topLevel("package foo {}; package bar {}")
    assert(pkgfoo.hasBraces === true)
    assert(pkgbar.hasBraces === true)
  }

  test("package object foo") {
    val TopLevel(Pkg.Object(Nil, Term.Name("foo"),
                            EmptyTemplate()) :: Nil) = topLevel("package object foo")
  }

  test("import foo.bar; package object baz") {
    val TopLevel(Import(Import.Clause(Term.Name("foo"), Import.Name("bar") :: Nil) :: Nil) ::
                 Pkg.Object(Nil, Term.Name("baz"), EmptyTemplate()) :: Nil) =
      topLevel("import foo.bar; package object baz")
  }

  test("package foo; package bar; package baz") {
    val TopLevel(List(Pkg(Term.Name("foo"), List(Pkg(Term.Name("bar"), List(Pkg(Term.Name("baz"), List()))))))) =
      topLevel("package foo; package bar; package baz")
  }
}

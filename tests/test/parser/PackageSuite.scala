import scala.reflect.core._, Aux._, Defn.Class

class PackageSuite extends ParseSuite {
  test("class C") {
    val CompUnit(Class(Nil, Type.Name("C"), Nil,
                       Ctor.Primary(Nil, Nil, Nil),
                       Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) = compUnit("class C")
  }

  test("package foo; class C") {
    val CompUnit((pkgfoo @ Pkg(Term.Name("foo"),
                               Class(Nil, Type.Name("C"), Nil,
                                     Ctor.Primary(Nil, Nil, Nil),
                                     Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil)) :: Nil) =
      compUnit("package foo; class C")
    assert(pkgfoo.hasBraces === false)
  }

  test("package foo { class C }") {
    val CompUnit((pkgfoo @Pkg(Term.Name("foo"),
                              Class(Nil, Type.Name("C"), Nil,
                                    Ctor.Primary(Nil, Nil, Nil),
                                    Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil)) :: Nil) =
      compUnit("package foo { class C }")
    assert(pkgfoo.hasBraces === true)
  }

  test("package foo.bar; class C") {
    val CompUnit((pkgfoobar @ Pkg(Term.Select(Term.Name("foo"), Term.Name("bar")),
                                  Class(Nil, Type.Name("C"), Nil,
                                        Ctor.Primary(Nil, Nil, Nil),
                                        Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil)) :: Nil) =
      compUnit("package foo.bar; class C")
    assert(pkgfoobar.hasBraces === false)
  }

  test("package foo.bar { class C }") {
    val CompUnit((pkgfoobar @ Pkg(Term.Select(Term.Name("foo"), Term.Name("bar")),
                                  Class(Nil, Type.Name("C"), Nil,
                                        Ctor.Primary(Nil, Nil, Nil),
                                        Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil)) :: Nil) =
      compUnit("package foo.bar { class C }")
    assert(pkgfoobar.hasBraces === true)
  }

  test("package foo; package bar; class C") {
    val CompUnit((pkgfoo @ Pkg(Term.Name("foo"),
                               (pkgbar @ Pkg(Term.Name("bar"),
                                             Class(Nil, Type.Name("C"), Nil,
                                                   Ctor.Primary(Nil, Nil, Nil),
                                                   Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil)) :: Nil)) :: Nil) =
      compUnit("package foo; package bar; class C")
    assert(pkgfoo.hasBraces === false)
    assert(pkgbar.hasBraces === false)
  }

  test("package foo { package bar { class C } }") {
    val CompUnit((pkgfoo @ Pkg(Term.Name("foo"),
                               (pkgbar @ Pkg(Term.Name("bar"),
                                             Class(Nil, Type.Name("C"), Nil,
                                                   Ctor.Primary(Nil, Nil, Nil),
                                                   Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil)) :: Nil)) :: Nil) =
      compUnit("package foo { package bar { class C } }")
    assert(pkgfoo.hasBraces === true)
    assert(pkgbar.hasBraces === true)
  }

  test("package foo {}; package bar {}") {
    val CompUnit((pkgfoo @ Pkg(Term.Name("foo"), Nil)) ::
                 (pkgbar @ Pkg(Term.Name("bar"), Nil)) :: Nil) =
      compUnit("package foo {}; package bar {}")
    assert(pkgfoo.hasBraces === true)
    assert(pkgbar.hasBraces === true)
  }

  test("package object foo") {
    val CompUnit(Defn.Object(Mod.Package() :: Nil, Term.Name("foo"),
                             Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) = compUnit("package object foo")
  }

  test("import foo.bar; package object baz") {
    val CompUnit(Import(Import.Clause(Term.Name("foo"), Import.Selector.Name(DualName("bar")) :: Nil) :: Nil) ::
                 Defn.Object(Mod.Package() :: Nil, Term.Name("baz"), Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) =
      compUnit("import foo.bar; package object baz")
  }
}

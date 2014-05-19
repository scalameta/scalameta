import scala.reflect.core._, Aux._, Defn.Class

class PackageSuite extends ParseSuite {
  test("class C") {
    val CompUnit(Class(Nil, Type.Name("C", false), Nil,
                       Ctor.Primary(Nil, Nil, Nil),
                       Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) = compUnit("class C")
  }

  test("package foo; class C") {
    val CompUnit(Pkg.Header(Term.Name("foo", false),
                            Class(Nil, Type.Name("C", false), Nil,
                                  Ctor.Primary(Nil, Nil, Nil),
                                  Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) :: Nil) =
      compUnit("package foo; class C")

  }

  test("package foo { class C }") {
    val CompUnit(Pkg.Template(Term.Name("foo", false),
                           Class(Nil, Type.Name("C", false), Nil,
                                 Ctor.Primary(Nil, Nil, Nil),
                                 Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) :: Nil) =
      compUnit("package foo { class C }")

  }

  test("package foo.bar; class C") {
    val CompUnit(Pkg.Header(Term.Select(Term.Name("foo", false), Term.Name("bar", false)),
                            Class(Nil, Type.Name("C", false), Nil,
                                  Ctor.Primary(Nil, Nil, Nil),
                                  Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) :: Nil) =
      compUnit("package foo.bar; class C")
  }

  test("package foo.bar { class C }") {
    val CompUnit(Pkg.Template(Term.Select(Term.Name("foo", false), Term.Name("bar", false)),
                           Class(Nil, Type.Name("C", false), Nil,
                                 Ctor.Primary(Nil, Nil, Nil),
                                 Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) :: Nil) =
      compUnit("package foo.bar { class C }")
  }

  test("package foo; package bar; class C") {
    val CompUnit(Pkg.Header(Term.Name("foo", false),
                            Pkg.Header(Term.Name("bar", false),
                                       Class(Nil, Type.Name("C", false), Nil,
                                             Ctor.Primary(Nil, Nil, Nil),
                                             Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) :: Nil) :: Nil) =
      compUnit("package foo; package bar; class C")
  }

  test("package foo { package bar { class C } }") {
    val CompUnit(Pkg.Template(Term.Name("foo", false),
                           Pkg.Template(Term.Name("bar", false),
                                     Class(Nil, Type.Name("C", false), Nil,
                                           Ctor.Primary(Nil, Nil, Nil),
                                           Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) :: Nil) :: Nil) =
      compUnit("package foo { package bar { class C } }")
  }

  test("package foo {}; package bar {}") {
    val CompUnit(Pkg.Template(Term.Name("foo", false), Nil) ::
                 Pkg.Template(Term.Name("bar", false), Nil) :: Nil) =
      compUnit("package foo {}; package bar {}")
  }

  test("package object foo") {
    val CompUnit(Pkg.Object(Nil, Term.Name("foo", false),
                            Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) = compUnit("package object foo")
  }

  test("import foo.bar; package object baz") {
    val CompUnit(Import(Import.Clause(Term.Name("foo", false), Import.Selector.Name("bar") :: Nil) :: Nil) ::
                 Pkg.Object(Nil, Term.Name("baz", false), Aux.Template(Nil, Nil, Self(None, None), Nil)) :: Nil) =
      compUnit("import foo.bar; package object baz")
  }
}

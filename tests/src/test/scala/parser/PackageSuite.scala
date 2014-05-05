import scala.reflect.core._, Aux._, Defn.Class

class PackageSuite extends ParseSuite {
  test("class C") {
    val Pkg.Empty(Class(Nil, Type.Ident("C", false), Nil,
                        Ctor.Primary.empty, Template.empty) :: Nil) = compUnit("class C")
  }

  test("package foo; class C") {
    val Pkg.Named(Term.Ident("foo", false),
                  Class(Nil, Type.Ident("C", false), Nil,
                        Ctor.Primary.empty, Template.empty) :: Nil) = compUnit("package foo; class C")

  }

  test("package foo { class C }") {
    val Pkg.Named(Term.Ident("foo", false),
                  Class(Nil, Type.Ident("C", false), Nil,
                        Ctor.Primary.empty, Template.empty) :: Nil) = compUnit("package foo { class C }")

  }

  test("package foo.bar; class C") {
    val Pkg.Named(Term.Select(Term.Ident("foo", false), Term.Ident("bar", false)),
                  Class(Nil, Type.Ident("C", false), Nil,
                        Ctor.Primary.empty, Template.empty) :: Nil) = compUnit("package foo.bar; class C")
  }

  test("package foo.bar { class C }") {
    val Pkg.Named(Term.Select(Term.Ident("foo", false), Term.Ident("bar", false)),
                  Class(Nil, Type.Ident("C", false), Nil,
                        Ctor.Primary.empty, Template.empty) :: Nil) = compUnit("package foo.bar { class C }")
  }

  test("package foo; package bar; class C") {
    val Pkg.Named(Term.Ident("foo", false),
                  Pkg.Named(Term.Ident("bar", false),
                            Class(Nil, Type.Ident("C", false), Nil,
                                  Ctor.Primary.empty, Template.empty) :: Nil) :: Nil) =
      compUnit("package foo; package bar; class C")
  }

  test("package foo { package bar { class C } }") {
    val Pkg.Named(Term.Ident("foo", false),
                  Pkg.Named(Term.Ident("bar", false),
                            Class(Nil, Type.Ident("C", false), Nil,
                                  Ctor.Primary.empty, Template.empty) :: Nil) :: Nil) =
      compUnit("package foo { package bar { class C } }")
  }

  test("package foo {}; package bar {}") {
    val Pkg.Empty(Pkg.Named(Term.Ident("foo", false), Nil) ::
                  Pkg.Named(Term.Ident("bar", false), Nil) :: Nil) =
      compUnit("package foo {}; package bar {}")
  }

  test("package object foo") {
    val Pkg.Object(Nil, Term.Ident("foo", false), Template.empty) = compUnit("package object foo")
  }

  test("import foo.bar; package object baz") {
    val Pkg.Empty(Import(Import.Clause(Term.Ident("foo", false), Import.Selector.Name("bar") :: Nil) :: Nil) ::
                  Pkg.Object(Nil, Term.Ident("baz", false), Template.empty) :: Nil) =
      compUnit("import foo.bar; package object baz")
  }
}

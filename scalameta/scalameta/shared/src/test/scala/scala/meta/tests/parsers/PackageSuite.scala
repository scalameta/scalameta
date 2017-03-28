package scala.meta.tests
package parsers

import scala.meta._, Defn.Class
import scala.meta.dialects.Scala211

class PackageSuite extends ParseSuite {
  test("class C") {
    val Source(Class(Nil, Type.Name("C"), Nil, EmptyCtor(), EmptyTemplate()) :: Nil) = source("class C")
  }

  test("package foo; class C") {
    val Source((pkgfoo @ Pkg(Term.Name("foo"),
                             Class(Nil, Type.Name("C"), Nil, EmptyCtor(), EmptyTemplate()) :: Nil)) :: Nil) =
      source("package foo; class C")
    // TODO: revisit this once we have trivia in place
    // assert(pkgfoo.hasBraces === false)
  }

  test("package foo { class C }") {
    val Source((pkgfoo @Pkg(Term.Name("foo"),
                            Class(Nil, Type.Name("C"), Nil, EmptyCtor(), EmptyTemplate()) :: Nil)) :: Nil) =
      source("package foo { class C }")
    // TODO: revisit this once we have trivia in place
    // assert(pkgfoo.hasBraces === true)
  }

  test("package foo.bar; class C") {
    val Source((pkgfoobar @ Pkg(Term.Select(Term.Name("foo"), Term.Name("bar")),
                                Class(Nil, Type.Name("C"), Nil, EmptyCtor(), EmptyTemplate()) :: Nil)) :: Nil) =
      source("package foo.bar; class C")
    // TODO: revisit this once we have trivia in place
    // assert(pkgfoobar.hasBraces === false)
  }

  test("package foo.bar { class C }") {
    val Source((pkgfoobar @ Pkg(Term.Select(Term.Name("foo"), Term.Name("bar")),
                                Class(Nil, Type.Name("C"), Nil, EmptyCtor(), EmptyTemplate()) :: Nil)) :: Nil) =
      source("package foo.bar { class C }")
    // TODO: revisit this once we have trivia in place
    // assert(pkgfoobar.hasBraces === true)
  }

  test("package foo; package bar; class C") {
    val Source((pkgfoo @ Pkg(Term.Name("foo"),
                             (pkgbar @ Pkg(Term.Name("bar"),
                                           Class(Nil, Type.Name("C"), Nil, EmptyCtor(), EmptyTemplate()) :: Nil)) :: Nil)) :: Nil) =
      source("package foo; package bar; class C")
    // TODO: revisit this once we have trivia in place
    // assert(pkgfoo.hasBraces === false)
    // assert(pkgbar.hasBraces === false)
  }

  test("package foo { package bar { class C } }") {
    val Source((pkgfoo @ Pkg(Term.Name("foo"),
                             (pkgbar @ Pkg(Term.Name("bar"),
                                           Class(Nil, Type.Name("C"), Nil, EmptyCtor(), EmptyTemplate()) :: Nil)) :: Nil)) :: Nil) =
      source("package foo { package bar { class C } }")
    // TODO: revisit this once we have trivia in place
    // assert(pkgfoo.hasBraces === true)
    // assert(pkgbar.hasBraces === true)
  }

  test("package foo {}; package bar {}") {
    val Source((pkgfoo @ Pkg(Term.Name("foo"), Nil)) ::
               (pkgbar @ Pkg(Term.Name("bar"), Nil)) :: Nil) =
      source("package foo {}; package bar {}")
    // TODO: revisit this once we have trivia in place
    // assert(pkgfoo.hasBraces === true)
    // assert(pkgbar.hasBraces === true)
  }

  test("package object foo") {
    val Source(Pkg.Object(Nil, Term.Name("foo"), EmptyTemplate()) :: Nil) = source("package object foo")
  }

  test("import foo.bar; package object baz") {
    val Source(Import(Importer(Term.Name("foo"), Importee.Name(Name.Indeterminate("bar")) :: Nil) :: Nil) ::
                 Pkg.Object(Nil, Term.Name("baz"), EmptyTemplate()) :: Nil) =
      source("import foo.bar; package object baz")
  }

  test("package foo; package bar; package baz") {
    val Source(List(Pkg(Term.Name("foo"), List(Pkg(Term.Name("bar"), List(Pkg(Term.Name("baz"), List()))))))) =
      source("package foo; package bar; package baz")
  }
}

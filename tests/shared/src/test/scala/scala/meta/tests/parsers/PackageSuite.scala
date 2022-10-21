package scala.meta.tests
package parsers

import scala.meta._, Defn.Class
import scala.meta.dialects.Scala211

class PackageSuite extends ParseSuite {
  test("class C") {
    assertTree(source("class C")) {
      Source(Class(Nil, Type.Name("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil)
    }
  }

  test("package foo; class C") {
    assertTree(source("package foo; class C")) {
      Source(
        Pkg(
          Term.Name("foo"),
          Class(Nil, Type.Name("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo { class C }") {
    assertTree(source("package foo { class C }")) {
      Source(
        Pkg(
          Term.Name("foo"),
          Class(Nil, Type.Name("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo.bar; class C") {
    assertTree(source("package foo.bar; class C")) {
      Source(
        Pkg(
          Term.Select(Term.Name("foo"), Term.Name("bar")),
          Class(Nil, Type.Name("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo.bar { class C }") {
    assertTree(source("package foo.bar { class C }")) {
      Source(
        Pkg(
          Term.Select(Term.Name("foo"), Term.Name("bar")),
          Class(Nil, Type.Name("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo; package bar; class C") {
    assertTree(source("package foo; package bar; class C")) {
      Source(
        Pkg(
          Term.Name("foo"),
          Pkg(
            Term.Name("bar"),
            Class(Nil, Type.Name("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
          ) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo { package bar { class C } }") {
    assertTree(source("package foo { package bar { class C } }")) {
      Source(
        Pkg(
          Term.Name("foo"),
          Pkg(
            Term.Name("bar"),
            Class(Nil, Type.Name("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
          ) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo {}; package bar {}") {
    val Source(
      (pkgfoo @ Pkg(Term.Name("foo"), Nil)) ::
        (pkgbar @ Pkg(Term.Name("bar"), Nil)) :: Nil
    ) =
      source("package foo {}; package bar {}")
  }

  test("package object foo") {
    val Source(Pkg.Object(Nil, Term.Name("foo"), EmptyTemplate()) :: Nil) =
      source("package object foo")
  }

  test("import foo.bar; package object baz") {
    val Source(
      Import(Importer(Term.Name("foo"), Importee.Name(Name.Indeterminate("bar")) :: Nil) :: Nil) ::
        Pkg.Object(Nil, Term.Name("baz"), EmptyTemplate()) :: Nil
    ) =
      source("import foo.bar; package object baz")
  }

  test("package foo; package bar; package baz") {
    val Source(
      List(Pkg(Term.Name("foo"), List(Pkg(Term.Name("bar"), List(Pkg(Term.Name("baz"), List()))))))
    ) =
      source("package foo; package bar; package baz")
  }

  test("package { in newline") {
    val Source(
      List(Pkg(Term.Name("foo"), List(Pkg(Term.Name("bar"), List(Pkg(Term.Name("baz"), List()))))))
    ) =
      source(
        """|
           |package foo  // foo package left brace in newline
           |/* still okay */
           |{
           |  package bar /* also in newline */
           |  // open
           |  {
           |    package baz
           |    { }
           |  }
           |}
           |""".stripMargin
      )

  }
}

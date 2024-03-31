package scala.meta.tests
package parsers

import scala.meta._, Defn.Class
import scala.meta.dialects.Scala211

class PackageSuite extends ParseSuite {
  test("class C") {
    assertTree(source("class C")) {
      Source(Class(Nil, pname("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil)
    }
  }

  test("package foo; class C") {
    assertTree(source("package foo; class C")) {
      Source(
        Pkg(
          tname("foo"),
          Class(Nil, pname("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo { class C }") {
    assertTree(source("package foo { class C }")) {
      Source(
        Pkg(
          tname("foo"),
          Class(Nil, pname("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo.bar; class C") {
    assertTree(source("package foo.bar; class C")) {
      Source(
        Pkg(
          Term.Select(tname("foo"), tname("bar")),
          Class(Nil, pname("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo.bar { class C }") {
    assertTree(source("package foo.bar { class C }")) {
      Source(
        Pkg(
          Term.Select(tname("foo"), tname("bar")),
          Class(Nil, pname("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo; package bar; class C") {
    assertTree(source("package foo; package bar; class C")) {
      Source(
        Pkg(
          tname("foo"),
          Pkg(
            tname("bar"),
            Class(Nil, pname("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
          ) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo { package bar { class C } }") {
    assertTree(source("package foo { package bar { class C } }")) {
      Source(
        Pkg(
          tname("foo"),
          Pkg(
            tname("bar"),
            Class(Nil, pname("C"), Type.ParamClause(Nil), EmptyCtor(), EmptyTemplate()) :: Nil
          ) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo {}; package bar {}") {
    assertTree(source("package foo {}; package bar {}"))(Source(
      (Pkg(tname("foo"), Nil)) :: (Pkg(tname("bar"), Nil)) :: Nil
    ))
  }

  test("package object foo") {
    assertTree(source("package object foo"))(Source(
      Pkg.Object(Nil, tname("foo"), EmptyTemplate()) :: Nil
    ))
  }

  test("import foo.bar; package object baz") {

    assertTree(source("import foo.bar; package object baz"))(Source(
      Import(Importer(tname("foo"), Importee.Name(Name.Indeterminate("bar")) :: Nil) :: Nil) ::
        Pkg.Object(Nil, tname("baz"), EmptyTemplate()) :: Nil
    ))
  }

  test("package foo; package bar; package baz") {
    assertTree(source("package foo; package bar; package baz"))(Source(
      List(Pkg(tname("foo"), List(Pkg(tname("bar"), List(Pkg(tname("baz"), List()))))))
    ))

  }

  test("package { in newline") {
    assertTree(source(
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
    ))(Source(List(Pkg(tname("foo"), List(Pkg(tname("bar"), List(Pkg(tname("baz"), List()))))))))
  }
}

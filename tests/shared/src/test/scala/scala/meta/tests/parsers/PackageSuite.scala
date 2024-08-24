package scala.meta.tests
package parsers

import scala.meta._

class PackageSuite extends ParseSuite {
  import Defn.Class

  implicit val dialect: Dialect = dialects.Scala211

  test("class C") {
    assertTree(source("class C"))(Source(Class(Nil, pname("C"), Nil, ctor, tplNoBody()) :: Nil))
  }

  test("package foo; class C") {
    assertTree(source("package foo; class C")) {
      Source(Pkg(tname("foo"), Class(Nil, pname("C"), Nil, ctor, tplNoBody()) :: Nil) :: Nil)
    }

  }

  test("package foo { class C }") {
    assertTree(source("package foo { class C }")) {
      Source(Pkg(tname("foo"), Class(Nil, pname("C"), Nil, ctor, tplNoBody()) :: Nil) :: Nil)
    }

  }

  test("package foo.bar; class C") {
    assertTree(source("package foo.bar; class C")) {
      Source(
        Pkg(
          Term.Select(tname("foo"), tname("bar")),
          Class(Nil, pname("C"), Nil, ctor, tplNoBody()) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo.bar { class C }") {
    assertTree(source("package foo.bar { class C }")) {
      Source(
        Pkg(
          Term.Select(tname("foo"), tname("bar")),
          Class(Nil, pname("C"), Nil, ctor, tplNoBody()) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo; package bar; class C") {
    assertTree(source("package foo; package bar; class C")) {
      Source(
        Pkg(
          tname("foo"),
          Pkg(tname("bar"), Class(Nil, pname("C"), Nil, ctor, tplNoBody()) :: Nil) :: Nil
        ) :: Nil
      )
    }

  }

  test("package foo { package bar { class C } }") {
    assertTree(source("package foo { package bar { class C } }")) {
      Source(
        Pkg(
          tname("foo"),
          Pkg(tname("bar"), Class(Nil, pname("C"), Nil, ctor, tplNoBody()) :: Nil) :: Nil
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
      Pkg.Object(Nil, tname("foo"), tplNoBody()) :: Nil
    ))
  }

  test("import foo.bar; package object baz") {

    assertTree(source("import foo.bar; package object baz"))(Source(
      Import(Importer(tname("foo"), Importee.Name(Name.Indeterminate("bar")) :: Nil) :: Nil) ::
        Pkg.Object(Nil, tname("baz"), tplNoBody()) :: Nil
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

  test("code with Shebang line") {
    val code = """|#!/usr/bin/env foo bar && qux >/dev/null
                  |package foo
                  |
                  |import bar.baz
                  |
                  |class Qux()
                  |""".stripMargin
    val layout = """|#!/usr/bin/env foo bar && qux >/dev/null
                    |package foo
                    |import bar.baz
                    |class Qux()
                    |""".stripMargin
    val tree = Source(List(Pkg(
      tname("foo"),
      List(
        Import(List(Importer(tname("bar"), List(Importee.Name(Name("baz")))))),
        Defn.Class(Nil, pname("Qux"), Nil, ctorp(Nil), tplNoBody())
      )
    )))
    runTestAssert[Source](code, layout)(tree)
  }

}

package scala.meta.tests
package parsers

import scala.meta.Name.{Anonymous, Indeterminate}
import scala.meta.{Name => _, _}

class ImportSuite extends ParseSuite {
  import Importee._
  import Term.{Name => TermName, Select, Super, This}

  implicit val dialect: Dialect = dialects.Scala211

  test("import foo.bar")(assertTree(templStat("import foo.bar"))(Import(
    Importer(TermName("foo"), Name(Indeterminate("bar")) :: Nil) :: Nil
  )))

  test("import foo.bar.baz")(assertTree(templStat("import foo.bar.baz"))(Import(
    Importer(tselect("foo", "bar"), Name(Indeterminate("baz")) :: Nil) :: Nil
  )))

  test("import super.foo.bar")(assertTree(templStat("import super.foo.bar"))(Import(
    Importer(
      Select(Super(Anonymous(), Anonymous()), TermName("foo")),
      Name(Indeterminate("bar")) :: Nil
    ) :: Nil
  )))

  test("import this.foo.bar")(assertTree(templStat("import this.foo.bar"))(Import(
    Importer(Select(This(Anonymous()), TermName("foo")), Name(Indeterminate("bar")) :: Nil) :: Nil
  )))

  test("import foo.bar._")(assertTree(templStat("import foo.bar._"))(Import(
    Importer(tselect("foo", "bar"), Wildcard() :: Nil) :: Nil
  )))

  test("import super.foo._")(assertTree(templStat("import super.foo._"))(Import(
    Importer(Select(Super(Anonymous(), Anonymous()), TermName("foo")), Wildcard() :: Nil) :: Nil
  )))

  test("import this.foo._")(assertTree(templStat("import this.foo._"))(Import(
    Importer(Select(This(Anonymous()), TermName("foo")), Wildcard() :: Nil) :: Nil
  )))

  test("import foo.{bar}")(assertTree(templStat("import foo.{bar}"))(Import(
    Importer(TermName("foo"), Name(Indeterminate("bar")) :: Nil) :: Nil
  )))

  test("import foo.{bar, baz}")(assertTree(templStat("import foo.{bar, baz}"))(Import(
    Importer(TermName("foo"), Name(Indeterminate("bar")) :: Name(Indeterminate("baz")) :: Nil) ::
      Nil
  )))

  test("import foo.{bar => baz}")(assertTree(templStat("import foo.{bar => baz}"))(Import(
    Importer(TermName("foo"), Rename(Indeterminate("bar"), Indeterminate("baz")) :: Nil) :: Nil
  )))

  test("import foo.{bar => _}")(assertTree(templStat("import foo.{bar => _}"))(Import(
    Importer(TermName("foo"), Unimport(Indeterminate("bar")) :: Nil) :: Nil
  )))

  test("import foo.{_ => _}")(assertTree(templStat("import foo.{_ => _}"))(Import(
    Importer(TermName("foo"), Wildcard() :: Nil) :: Nil
  )))

  test("import foo.{bar => _, _}")(assertTree(templStat("import foo.{bar => _, _}"))(Import(
    Importer(TermName("foo"), Unimport(Indeterminate("bar")) :: Wildcard() :: Nil) :: Nil
  )))

  test("import foo.{bar, baz => _, _}")(
    assertTree(templStat("import foo.{bar, baz => _, _}"))(Import(
      Importer(
        TermName("foo"),
        Name(Indeterminate("bar")) :: Unimport(Indeterminate("baz")) :: Wildcard() :: Nil
      ) :: Nil
    ))
  )

  test("import a.b.{ _, c => _ }")(
    // invalid but we don't check anymore
    assertTree(templStat("import a.b.{ _, c => _ }"))(Import(
      List(Importer(tselect("a", "b"), List(Wildcard(), Unimport(Indeterminate("c")))))
    ))
  )

  test("source3-given-import") {
    val expected =
      Import(List(Importer(tselect("a", "b", "c"), List(Importee.GivenAll(), Importee.Wildcard()))))

    assertTree {
      implicit val dialect: Dialect = dialects.Scala212Source3
      templStat("import a.b.c.{ given, _ }")
    }(expected)

    assertTree {
      implicit val dialect: Dialect = dialects.Scala213Source3
      templStat("import a.b.c.{ given, _ }")
    }(expected)

    val expectedWithoutWildcard =
      Import(List(Importer(tselect("a", "b", "c"), List(Importee.Name(Indeterminate("given"))))))

    assertTree {
      implicit val dialect: Dialect = dialects.Scala212Source3
      templStat("import a.b.c.{ given }")
    }(expectedWithoutWildcard)

    assertTree {
      implicit val dialect: Dialect = dialects.Scala213Source3
      templStat("import a.b.c.{ given }")
    }(expectedWithoutWildcard)
  }

  test("import with comments") {
    val code =
      """|// c1
         |import a.b.{c => d} // c2
         |""".stripMargin
    code.parse[Source] match {
      case x: Parsed.Error => fail(x.message)
      case Parsed.Success(obtained) => obtained.stats match {
          case head :: Nil =>
            val layout =
              """|// c1
                 |import a.b.{c => d} // c2
                 |""".stripMargin
            val tree = Import.createWithComments(
              List(Importer(tselect("a", "b"), List(Importee.Rename(meta.Name("c"), meta.Name("d"))))),
              begComment = Seq("// c1"),
              endComment = Seq("// c2")
            )
            checkTree(head, layout)(tree)
            assertNoDiff(head.original, "import a.b.{c => d}")
          case _ => fail(s"Expected one stat: ${obtained.structure}")
        }
    }
  }

  test("package over imports with comments") {
    val code =
      """|package test.organizeImports
         |
         |// This comment is ambiguous and not linked to a specific import
         |import y.Y
         |import x.X
         |""".stripMargin
    code.parse[Source] match {
      case x: Parsed.Error => fail(x.message)
      case Parsed.Success(obtained) => obtained.stats match {
          case (pkg: Pkg) :: Nil => pkg.stats match {
              case one :: two :: Nil =>
                locally {
                  val layout =
                    """|// This comment is ambiguous and not linked to a specific import
                       |import y.Y
                       |""".stripMargin
                  val tree = Import.createWithComments(
                    List(Importer("y", List(Importee.Name(meta.Name("Y"))))),
                    begComment =
                      Seq("// This comment is ambiguous and not linked to a specific import")
                  )
                  checkTree(one, layout)(tree)
                  assertNoDiff(one.original, "import y.Y")
                }
                locally {
                  val layout =
                    """|import x.X
                       |""".stripMargin
                  val tree = Import(List(Importer("x", List(Importee.Name(meta.Name("X"))))))
                  checkTree(two, layout)(tree)
                  assertNoDiff(two.original, layout)
                }
              case x => fail(s"Not a package with 2 stats: ${pkg.structure}")
            }
          case x => fail(s"Not a source with package: ${x.structure}")
        }
    }
  }

}

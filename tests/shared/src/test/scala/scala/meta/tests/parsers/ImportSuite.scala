package scala.meta.tests
package parsers

import scala.meta.{Name => _, _}, Importee._, Term.{This, Name => TermName, Select, Super}
import scala.meta.Name.{Anonymous, Indeterminate}
import scala.meta.dialects.Scala211
import scala.meta.parsers.ParseException

class ImportSuite extends ParseSuite {
  test("import foo.bar") {
    assertTree(templStat("import foo.bar"))(
      Import(Importer(TermName("foo"), Name(Indeterminate("bar")) :: Nil) :: Nil)
    )
  }

  test("import foo.bar.baz") {
    assertTree(templStat("import foo.bar.baz"))(
      Import(
        Importer(Select(TermName("foo"), TermName("bar")), Name(Indeterminate("baz")) :: Nil) :: Nil
      )
    )
  }

  test("import super.foo.bar") {
    assertTree(templStat("import super.foo.bar"))(
      Import(
        Importer(
          Select(Super(Anonymous(), Anonymous()), TermName("foo")),
          Name(Indeterminate("bar")) :: Nil
        ) :: Nil
      )
    )
  }

  test("import this.foo.bar") {
    assertTree(templStat("import this.foo.bar"))(
      Import(
        Importer(
          Select(This(Anonymous()), TermName("foo")),
          Name(Indeterminate("bar")) :: Nil
        ) :: Nil
      )
    )
  }

  test("import foo.bar._") {
    assertTree(templStat("import foo.bar._"))(
      Import(Importer(Select(TermName("foo"), TermName("bar")), Wildcard() :: Nil) :: Nil)
    )
  }

  test("import super.foo._") {
    assertTree(templStat("import super.foo._"))(
      Import(
        Importer(Select(Super(Anonymous(), Anonymous()), TermName("foo")), Wildcard() :: Nil) :: Nil
      )
    )
  }

  test("import this.foo._") {
    assertTree(templStat("import this.foo._"))(
      Import(Importer(Select(This(Anonymous()), TermName("foo")), Wildcard() :: Nil) :: Nil)
    )
  }

  test("import foo.{bar}") {
    assertTree(templStat("import foo.{bar}"))(
      Import(Importer(TermName("foo"), Name(Indeterminate("bar")) :: Nil) :: Nil)
    )
  }

  test("import foo.{bar, baz}") {
    assertTree(templStat("import foo.{bar, baz}"))(
      Import(
        Importer(TermName("foo"), Name(Indeterminate("bar")) :: (Name(Indeterminate("baz"))) :: Nil)
          :: Nil
      )
    )
  }

  test("import foo.{bar => baz}") {
    assertTree(templStat("import foo.{bar => baz}"))(
      Import(
        Importer(TermName("foo"), Rename(Indeterminate("bar"), Indeterminate("baz")) :: Nil) :: Nil
      )
    )
  }

  test("import foo.{bar => _}") {
    assertTree(templStat("import foo.{bar => _}"))(
      Import(Importer(TermName("foo"), Unimport(Indeterminate("bar")) :: Nil) :: Nil)
    )
  }

  test("import foo.{_ => _}") {
    assertTree(templStat("import foo.{_ => _}"))(
      Import(Importer(TermName("foo"), Wildcard() :: Nil) :: Nil)
    )
  }

  test("import foo.{bar => _, _}") {
    assertTree(templStat("import foo.{bar => _, _}"))(
      Import(
        Importer(TermName("foo"), Unimport(Indeterminate("bar")) :: Wildcard() :: Nil) :: Nil
      )
    )
  }

  test("import foo.{bar, baz => _, _}") {
    assertTree(templStat("import foo.{bar, baz => _, _}"))(
      Import(
        Importer(
          TermName("foo"),
          (Name(Indeterminate("bar"))) :: Unimport(Indeterminate("baz")) :: Wildcard() :: Nil
        ) :: Nil
      )
    )
  }

  test("import a.b.{ _, c => _ }") {
    intercept[ParseException] {
      templStat("import a.b.{ _, c => _ }")
    }
  }

  test("source3-given-import") {
    val expected = Import(
      List(
        Importer(
          Term.Select(Term.Select(Term.Name("a"), Term.Name("b")), Term.Name("c")),
          List(Importee.GivenAll(), Importee.Wildcard())
        )
      )
    )

    val res212 =
      templStat("import a.b.c.{ given, _ }")(dialects.Scala212Source3)

    assertTree(res212)(expected)

    val res213 =
      templStat("import a.b.c.{ given, _ }")(dialects.Scala213Source3)

    assertTree(res213)(expected)

    val expectedWithoutWildcard = Import(
      List(
        Importer(
          Term.Select(Term.Select(Term.Name("a"), Term.Name("b")), Term.Name("c")),
          List(Importee.Name(Indeterminate("given")))
        )
      )
    )

    val res212NoWildcard =
      templStat("import a.b.c.{ given }")(dialects.Scala212Source3)

    assertTree(res212NoWildcard)(expectedWithoutWildcard)

    val res213NoWildcard =
      templStat("import a.b.c.{ given }")(dialects.Scala213Source3)

    assertTree(res213NoWildcard)(expectedWithoutWildcard)
  }
}

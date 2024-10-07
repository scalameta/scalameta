package scala.meta.tests
package parsers

import scala.meta.Name.Anonymous
import scala.meta.Name.Indeterminate
import scala.meta.parsers.ParseException
import scala.meta.{Name => _, _}

class ImportSuite extends ParseSuite {
  import Importee._
  import Term.Select
  import Term.Super
  import Term.This
  import Term.{Name => TermName}

  implicit val dialect: Dialect = dialects.Scala211

  test("import foo.bar") {
    assertTree(templStat("import foo.bar"))(Import(
      Importer(TermName("foo"), Name(Indeterminate("bar")) :: Nil) :: Nil
    ))
  }

  test("import foo.bar.baz") {
    assertTree(templStat("import foo.bar.baz"))(Import(
      Importer(Select(TermName("foo"), TermName("bar")), Name(Indeterminate("baz")) :: Nil) :: Nil
    ))
  }

  test("import super.foo.bar") {
    assertTree(templStat("import super.foo.bar"))(Import(
      Importer(
        Select(Super(Anonymous(), Anonymous()), TermName("foo")),
        Name(Indeterminate("bar")) :: Nil
      ) :: Nil
    ))
  }

  test("import this.foo.bar") {
    assertTree(templStat("import this.foo.bar"))(Import(
      Importer(Select(This(Anonymous()), TermName("foo")), Name(Indeterminate("bar")) :: Nil) :: Nil
    ))
  }

  test("import foo.bar._") {
    assertTree(templStat("import foo.bar._"))(Import(
      Importer(Select(TermName("foo"), TermName("bar")), Wildcard() :: Nil) :: Nil
    ))
  }

  test("import super.foo._") {
    assertTree(templStat("import super.foo._"))(Import(
      Importer(Select(Super(Anonymous(), Anonymous()), TermName("foo")), Wildcard() :: Nil) :: Nil
    ))
  }

  test("import this.foo._") {
    assertTree(templStat("import this.foo._"))(Import(
      Importer(Select(This(Anonymous()), TermName("foo")), Wildcard() :: Nil) :: Nil
    ))
  }

  test("import foo.{bar}") {
    assertTree(templStat("import foo.{bar}"))(Import(
      Importer(TermName("foo"), Name(Indeterminate("bar")) :: Nil) :: Nil
    ))
  }

  test("import foo.{bar, baz}") {
    assertTree(templStat("import foo.{bar, baz}"))(Import(
      Importer(
        TermName("foo"),
        Name(Indeterminate("bar")) :: (Name(Indeterminate("baz"))) :: Nil
      ) :: Nil
    ))
  }

  test("import foo.{bar => baz}") {
    assertTree(templStat("import foo.{bar => baz}"))(Import(
      Importer(TermName("foo"), Rename(Indeterminate("bar"), Indeterminate("baz")) :: Nil) :: Nil
    ))
  }

  test("import foo.{bar => _}") {
    assertTree(templStat("import foo.{bar => _}"))(Import(
      Importer(TermName("foo"), Unimport(Indeterminate("bar")) :: Nil) :: Nil
    ))
  }

  test("import foo.{_ => _}") {
    assertTree(templStat("import foo.{_ => _}"))(Import(
      Importer(TermName("foo"), Wildcard() :: Nil) :: Nil
    ))
  }

  test("import foo.{bar => _, _}") {
    assertTree(templStat("import foo.{bar => _, _}"))(Import(
      Importer(TermName("foo"), Unimport(Indeterminate("bar")) :: Wildcard() :: Nil) :: Nil
    ))
  }

  test("import foo.{bar, baz => _, _}") {
    assertTree(templStat("import foo.{bar, baz => _, _}"))(Import(
      Importer(
        TermName("foo"),
        Name(Indeterminate("bar")) :: Unimport(Indeterminate("baz")) :: Wildcard() :: Nil
      ) :: Nil
    ))
  }

  test("import a.b.{ _, c => _ }") {
    // invalid but we don't check anymore
    assertTree(templStat("import a.b.{ _, c => _ }"))(Import(List(
      Importer(Term.Select(tname("a"), tname("b")), List(Wildcard(), Unimport(Indeterminate("c"))))
    )))
  }

  test("source3-given-import") {
    val expected = Import(List(Importer(
      Term.Select(Term.Select(tname("a"), tname("b")), tname("c")),
      List(Importee.GivenAll(), Importee.Wildcard())
    )))

    assertTree {
      implicit val dialect: Dialect = dialects.Scala212Source3
      templStat("import a.b.c.{ given, _ }")
    }(expected)

    assertTree {
      implicit val dialect: Dialect = dialects.Scala213Source3
      templStat("import a.b.c.{ given, _ }")
    }(expected)

    val expectedWithoutWildcard = Import(List(Importer(
      Term.Select(Term.Select(tname("a"), tname("b")), tname("c")),
      List(Importee.Name(Indeterminate("given")))
    )))

    assertTree {
      implicit val dialect: Dialect = dialects.Scala212Source3
      templStat("import a.b.c.{ given }")
    }(expectedWithoutWildcard)

    assertTree {
      implicit val dialect: Dialect = dialects.Scala213Source3
      templStat("import a.b.c.{ given }")
    }(expectedWithoutWildcard)
  }
}

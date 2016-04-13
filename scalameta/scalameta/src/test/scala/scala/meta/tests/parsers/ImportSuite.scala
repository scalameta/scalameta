package scala.meta.tests
package parsers

import scala.meta.{Name => _, _}, Importee._, Term.{This, Name => TermName, Select, Super}
import scala.meta.Name.{Anonymous, Indeterminate}
import scala.meta.dialects.Scala211

class ImportSuite extends ParseSuite {
  test("import foo.bar") {
    val Import(Importer(TermName("foo"), Name(Indeterminate("bar")) :: Nil) :: Nil) = templStat("import foo.bar")
  }

  test("import foo.bar.baz") {
    val Import(Importer(Select(TermName("foo"), TermName("bar")), Name(Indeterminate("baz")) :: Nil) :: Nil) =
      templStat("import foo.bar.baz")
  }

  test("import super.foo.bar") {
    val Import(Importer(Select(Super(Anonymous(), Anonymous()), TermName("foo")), Name(Indeterminate("bar")) :: Nil) :: Nil) =
      templStat("import super.foo.bar")
  }

  test("import this.foo.bar") {
    val Import(Importer(Select(This(Anonymous()), TermName("foo")), Name(Indeterminate("bar")) :: Nil) :: Nil) =
      templStat("import this.foo.bar")
  }

  test("import foo.bar._") {
    val Import(Importer(Select(TermName("foo"), TermName("bar")), Wildcard() :: Nil) :: Nil) =
      templStat("import foo.bar._")
  }

  test("import super.foo._") {
    val Import(Importer(Select(Super(Anonymous(), Anonymous()), TermName("foo")), Wildcard() :: Nil) :: Nil) =
      templStat("import super.foo._")
  }

  test("import this.foo._") {
    val Import(Importer(Select(This(Anonymous()), TermName("foo")), Wildcard() :: Nil) :: Nil) =
      templStat("import this.foo._")
  }

  test("import foo.{bar}") {
    val Import(Importer(TermName("foo"), Name(Indeterminate("bar")) :: Nil) :: Nil) = templStat("import foo.{bar}")
  }

  test("import foo.{bar, baz}") {
    val Import(Importer(TermName("foo"), Name(Indeterminate("bar")) :: (Name(Indeterminate("baz"))) :: Nil) :: Nil) =
      templStat("import foo.{bar, baz}")
  }

  test("import foo.{bar => baz}") {
    val Import(Importer(TermName("foo"), Rename(Indeterminate("bar"), Indeterminate("baz")) :: Nil) :: Nil) =
      templStat("import foo.{bar => baz}")
  }

  test("import foo.{bar => _}") {
    val Import(Importer(TermName("foo"), Unimport(Indeterminate("bar")) :: Nil) :: Nil) =
      templStat("import foo.{bar => _}")
  }

  test("import foo.{_ => _}") {
    val Import(Importer(TermName("foo"), Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{_ => _}")
  }

  test("import foo.{bar => _, _}") {
    val Import(Importer(TermName("foo"), Unimport(Indeterminate("bar")) :: Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{bar => _, _}")
  }

  test("import foo.{bar, baz => _, _}") {
    val Import(Importer(TermName("foo"), (Name(Indeterminate("bar"))) :: Unimport(Indeterminate("baz")) :: Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{bar, baz => _, _}")
  }
}

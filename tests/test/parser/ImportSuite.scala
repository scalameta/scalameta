import scala.meta._, Import._, Term.{This, Name => TermName, Select, Super}

class ImportSuite extends ParseSuite {
  test("import foo.bar") {
    val Import(Clause(TermName("foo"), Name("bar") :: Nil) :: Nil) = templStat("import foo.bar")
  }

  test("import foo.bar.baz") {
    val Import(Clause(Select(TermName("foo"), TermName("bar")), Name("baz") :: Nil) :: Nil) =
      templStat("import foo.bar.baz")
  }

  test("import super.foo.bar") {
    val Import(Clause(Select(Super(None, None), TermName("foo")), Name("bar") :: Nil) :: Nil) =
      templStat("import super.foo.bar")
  }

  test("import this.foo.bar") {
    val Import(Clause(Select(This(None), TermName("foo")), Name("bar") :: Nil) :: Nil) =
      templStat("import this.foo.bar")
  }

  test("import foo.bar._") {
    val Import(Clause(Select(TermName("foo"), TermName("bar")), Wildcard() :: Nil) :: Nil) =
      templStat("import foo.bar._")
  }

  test("import super.foo._") {
    val Import(Clause(Select(Super(None, None), TermName("foo")), Wildcard() :: Nil) :: Nil) =
      templStat("import super.foo._")
  }

  test("import this.foo._") {
    val Import(Clause(Select(This(None), TermName("foo")), Wildcard() :: Nil) :: Nil) =
      templStat("import this.foo._")
  }

  test("import foo.{bar}") {
    val Import(Clause(TermName("foo"), Name("bar") :: Nil) :: Nil) = templStat("import foo.{bar}")
  }

  test("import foo.{bar, baz}") {
    val Import(Clause(TermName("foo"), Name("bar") :: (Name("baz")) :: Nil) :: Nil) =
      templStat("import foo.{bar, baz}")
  }

  test("import foo.{bar => baz}") {
    val Import(Clause(TermName("foo"), Rename(Name("bar"), Name("baz")) :: Nil) :: Nil) =
      templStat("import foo.{bar => baz}")
  }

  test("import foo.{bar => _}") {
    val Import(Clause(TermName("foo"), Unimport(Name("bar")) :: Nil) :: Nil) =
      templStat("import foo.{bar => _}")
  }

  test("import foo.{bar => _, _}") {
    val Import(Clause(TermName("foo"), Unimport(Name("bar")) :: Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{bar => _, _}")
  }

  test("import foo.{bar, baz => _, _}") {
    val Import(Clause(TermName("foo"), (Name("bar")) :: Unimport(Name("baz")) :: Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{bar, baz => _, _}")
  }
}

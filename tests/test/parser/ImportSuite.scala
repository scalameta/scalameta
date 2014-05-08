import scala.reflect.core._, Import._, Selector.{Name => SelectorName, _}, Term.{This, Name => TermName, SuperSelect, Select}

class ImportSuite extends ParseSuite {
  test("import foo.bar") {
    val Import(Clause(TermName("foo", false), SelectorName("bar") :: Nil) :: Nil) = templStat("import foo.bar")
  }

  test("import foo.bar.baz") {
    val Import(Clause(Select(TermName("foo", false), TermName("bar", false)), SelectorName("baz") :: Nil) :: Nil) =
      templStat("import foo.bar.baz")
  }

  test("import super.foo.bar") {
    val Import(Clause(SuperSelect(None, None, TermName("foo", false)), SelectorName("bar") :: Nil) :: Nil) =
      templStat("import super.foo.bar")
  }

  test("import this.foo.bar") {
    val Import(Clause(Select(This(None), TermName("foo", false)), SelectorName("bar") :: Nil) :: Nil) =
      templStat("import this.foo.bar")
  }

  test("import foo.bar._") {
    val Import(Clause(Select(TermName("foo", false), TermName("bar", false)), Wildcard() :: Nil) :: Nil) =
      templStat("import foo.bar._")
  }

  test("import super.foo._") {
    val Import(Clause(SuperSelect(None, None, TermName("foo", false)), Wildcard() :: Nil) :: Nil) =
      templStat("import super.foo._")
  }

  test("import this.foo._") {
    val Import(Clause(Select(This(None), TermName("foo", false)), Wildcard() :: Nil) :: Nil) =
      templStat("import this.foo._")
  }

  test("import foo.{bar}") {
    val Import(Clause(TermName("foo", false), SelectorName("bar") :: Nil) :: Nil) = templStat("import foo.{bar}")
  }

  test("import foo.{bar, baz}") {
    val Import(Clause(TermName("foo", false), SelectorName("bar") :: SelectorName("baz") :: Nil) :: Nil) =
      templStat("import foo.{bar, baz}")
  }

  test("import foo.{bar => baz}") {
    val Import(Clause(TermName("foo", false), Rename("bar", "baz") :: Nil) :: Nil) =
      templStat("import foo.{bar => baz}")
  }

  test("import foo.{bar => _}") {
    val Import(Clause(TermName("foo", false), Unimport("bar") :: Nil) :: Nil) =
      templStat("import foo.{bar => _}")
  }

  test("import foo.{bar => _, _}") {
    val Import(Clause(TermName("foo", false), Unimport("bar") :: Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{bar => _, _}")
  }

  test("import foo.{bar, baz => _, _}") {
    val Import(Clause(TermName("foo", false), SelectorName("bar") :: Unimport("baz") :: Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{bar, baz => _, _}")
  }
}
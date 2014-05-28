import scala.reflect.core._, Import._, Selector.{Name => SelectorName, _}, Term.{This, Name => TermName, Select}, Aux.Super, Name.{Either, Both}

class ImportSuite extends ParseSuite {
  test("import foo.bar") {
    val Import(Clause(TermName("foo"), SelectorName(Both("bar")) :: Nil) :: Nil) = templStat("import foo.bar")
  }

  test("import foo.bar.baz") {
    val Import(Clause(Select(TermName("foo"), TermName("bar")), SelectorName(Both("baz")) :: Nil) :: Nil) =
      templStat("import foo.bar.baz")
  }

  test("import super.foo.bar") {
    val Import(Clause(Select(Super(None, None), TermName("foo")), SelectorName(Both("bar")) :: Nil) :: Nil) =
      templStat("import super.foo.bar")
  }

  test("import this.foo.bar") {
    val Import(Clause(Select(This(None), TermName("foo")), SelectorName(Both("bar")) :: Nil) :: Nil) =
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
    val Import(Clause(TermName("foo"), SelectorName(Both("bar")) :: Nil) :: Nil) = templStat("import foo.{bar}")
  }

  test("import foo.{bar, baz}") {
    val Import(Clause(TermName("foo"), SelectorName(Both("bar")) :: SelectorName(Both("baz")) :: Nil) :: Nil) =
      templStat("import foo.{bar, baz}")
  }

  test("import foo.{bar => baz}") {
    val Import(Clause(TermName("foo"), Rename(Both("bar"), Both("baz")) :: Nil) :: Nil) =
      templStat("import foo.{bar => baz}")
  }

  test("import foo.{bar => _}") {
    val Import(Clause(TermName("foo"), Unimport(Both("bar")) :: Nil) :: Nil) =
      templStat("import foo.{bar => _}")
  }

  test("import foo.{bar => _, _}") {
    val Import(Clause(TermName("foo"), Unimport(Both("bar")) :: Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{bar => _, _}")
  }

  test("import foo.{bar, baz => _, _}") {
    val Import(Clause(TermName("foo"), SelectorName(Both("bar")) :: Unimport(Both("baz")) :: Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{bar, baz => _, _}")
  }
}
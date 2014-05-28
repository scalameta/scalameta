import scala.reflect.core._, Import._, Selector.{Name => SelectorName, _}, Term.{This, Name => TermName, Select}, Aux.Super

class ImportSuite extends ParseSuite {
  test("import foo.bar") {
    val Import(Clause(TermName("foo"), SelectorName(Name.Both("bar")) :: Nil) :: Nil) = templStat("import foo.bar")
  }

  test("import foo.bar.baz") {
    val Import(Clause(Select(TermName("foo"), TermName("bar")), SelectorName(Name.Both("baz")) :: Nil) :: Nil) =
      templStat("import foo.bar.baz")
  }

  test("import super.foo.bar") {
    val Import(Clause(Select(Super(None, None), TermName("foo")), SelectorName(Name.Both("bar")) :: Nil) :: Nil) =
      templStat("import super.foo.bar")
  }

  test("import this.foo.bar") {
    val Import(Clause(Select(This(None), TermName("foo")), SelectorName(Name.Both("bar")) :: Nil) :: Nil) =
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
    val Import(Clause(TermName("foo"), SelectorName(Name.Both("bar")) :: Nil) :: Nil) = templStat("import foo.{bar}")
  }

  test("import foo.{bar, baz}") {
    val Import(Clause(TermName("foo"), SelectorName(Name.Both("bar")) :: SelectorName(Name.Both("baz")) :: Nil) :: Nil) =
      templStat("import foo.{bar, baz}")
  }

  test("import foo.{bar => baz}") {
    val Import(Clause(TermName("foo"), Rename(Name.Both("bar"), Name.Both("baz")) :: Nil) :: Nil) =
      templStat("import foo.{bar => baz}")
  }

  test("import foo.{bar => _}") {
    val Import(Clause(TermName("foo"), Unimport(Name.Both("bar")) :: Nil) :: Nil) =
      templStat("import foo.{bar => _}")
  }

  test("import foo.{bar => _, _}") {
    val Import(Clause(TermName("foo"), Unimport(Name.Both("bar")) :: Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{bar => _, _}")
  }

  test("import foo.{bar, baz => _, _}") {
    val Import(Clause(TermName("foo"), SelectorName(Name.Both("bar")) :: Unimport(Name.Both("baz")) :: Wildcard() :: Nil) :: Nil) =
      templStat("import foo.{bar, baz => _, _}")
  }
}
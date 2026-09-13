package scala.meta.tests.parsers.dotty

import scala.meta._

class DedentedStringSuite extends BaseDottySuite {

  private val dialectWithFlag: Dialect = dialects.Scala3.withAllowDedentedStringLiterals(true)

  // a lone statement stops at the first line: only the opening quotes parse
  test("basic") {
    val code = "'''\n  i am cow\n  hear me moo\n  '''"
    val tree = lit('\'')
    runTestAssert[Stat](code)(tree)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestAssert[Stat](code)(tree)
    }
  }

  test("empty") {
    val code = "'''\n  '''"
    val tree = lit('\'')
    runTestAssert[Stat](code)(tree)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestAssert[Stat](code)(tree)
    }
  }

  test("content on opening line") {
    val code = "'''foo\n  '''"
    val layout = "''' foo '''"
    val tree = tinfix(lit('\''), tname("foo"), lit('\''))
    runTestAssert[Stat](code, layout)(tree)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestAssert[Stat](code, layout)(tree)
    }
  }

  test("in val definition") {
    val code = "val x = '''\n  foo\n  '''"
    val layout = "val x = '''"
    val tree = Defn.Val(Nil, List(patvar("x")), None, lit('\''))
    runTestAssert[Stat](code, layout)(tree)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestAssert[Stat](code, layout)(tree)
    }
  }

  test("in concatenation") {
    val code = "\"a\" + '''\n  b\n  ''' + \"c\""
    val layout = "\"a\" + '''"
    val tree = tinfix(lit("a"), tname("+"), lit('\''))
    runTestAssert[Stat](code, layout)(tree)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestAssert[Stat](code, layout)(tree)
    }
  }

  test("four-quote delimiter") {
    val code = "''''\n  '''\n  foo\n  ''''"
    val error =
      """|<input>:1: error: can't use unescaped LF in character literals
         |''''
         |    ^""".stripMargin
    runTestError[Stat](code, error)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestError[Stat](code, error)
    }
  }

  test("unclosed") {
    val code = "'''\n  foo"
    val tree = lit('\'')
    runTestAssert[Stat](code)(tree)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestAssert[Stat](code)(tree)
    }
  }

  test("two quotes alone") {
    val code = "''"
    val error =
      """|<input>:1: error: Macro quote must be followed by id, brace or bracket
         |''
         | ^""".stripMargin
    runTestError[Stat](code, error)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestError[Stat](code, error)
    }
  }

  test("interpolation - basic") {
    val code = "s'''\n  a $x b\n  '''"
    val error =
      """|<input>:1: error: `;` expected but `character constant` found
         |s'''
         | ^""".stripMargin
    runTestError[Stat](code, error)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestError[Stat](code, error)
    }
  }

  test("interpolation - nested dedented string") {
    val code = "s'''\n  a ${'''\n  b\n  '''} c\n  '''"
    val error =
      """|<input>:1: error: `;` expected but `character constant` found
         |s'''
         | ^""".stripMargin
    runTestError[Stat](code, error)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestError[Stat](code, error)
    }
  }

  test("interpolation - two quotes alone") {
    val code = "s''"
    val error =
      """|<input>:1: error: `;` expected but `'` found
         |s''
         | ^""".stripMargin
    runTestError[Stat](code, error)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestError[Stat](code, error)
    }
  }

  test("interpolation - char-like") {
    val code = "s'a'"
    val error =
      """|<input>:1: error: `;` expected but `character constant` found
         |s'a'
         | ^""".stripMargin
    runTestError[Stat](code, error)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestError[Stat](code, error)
    }
  }

  test("match case") {
    val code = "x match { case '''\n  a\n  ''' => 1 }"
    val layout = "x match {\n  case ''' a ''' => 1\n}"
    val tree = tmatch(tname("x"), Case(patinfix(lit('\''), "a", lit('\'')), None, lit(1)))
    runTestAssert[Stat](code, layout)(tree)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestAssert[Stat](code, layout)(tree)
    }
  }

  test("match case interpolation") {
    val code = "x match { case s'''\n  a $y b\n  ''' => 1 }"
    val error =
      """|<input>:1: error: `=>` expected but `character constant` found
         |x match { case s'''
         |                ^""".stripMargin
    runTestError[Stat](code, error)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestError[Stat](code, error)
    }
  }

  test("match case interpolation - two lines") {
    val code = "x match { case s'''\n  First: $a\n  Second: $b\n  ''' => 1 }"
    val error =
      """|<input>:1: error: `=>` expected but `character constant` found
         |x match { case s'''
         |                ^""".stripMargin
    runTestError[Stat](code, error)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestError[Stat](code, error)
    }
  }

  test("literal type") {
    val code = "val x: '''\n  a\n  ''' = y"
    val error =
      """|<input>:3: error: `;` expected but `=` found
         |  ''' = y
         |      ^""".stripMargin
    runTestError[Stat](code, error)
    locally {
      implicit val dialect: Dialect = dialectWithFlag
      runTestError[Stat](code, error)
    }
  }

}

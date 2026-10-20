package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}
import scala.meta.tests.semanticdb.ScalaVersion._

class OperatorNamesSuite extends SemanticdbSuite with MetacpSnippets {

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  // the symbols section, only the entries of local symbols
  private def locals(name: String, code: String, expected: String, compat: (Version, String)*)(
      implicit loc: munit.Location,
  ): Unit = test(s"$name: locals") {
    val section = computeDatabaseSectionFromSnippet(code, "Symbols")
    var isLocal = false
    val obtained = section.linesIterator.filter { line =>
      if (!line.startsWith(" ")) isLocal = line.startsWith("local")
      isLocal
    }.map(_ + "\n").mkString
    assertNoDiff(obtained, getExpected(compat, expected))
  }

  // the display name of `symbol`, from metac and from metacp
  private def metacp(name: String, code: String, symbol: String, metac: String, metacp: String)(
      implicit loc: munit.Location,
  ): Unit = test(s"$name: metacp") {
    val classes = compile(code)
    def show(infos: Map[String, s.SymbolInformation]) = infos.get(symbol)
      .fold("")(info => s"${info.symbol} => ${info.displayName}")
    assertNoDiff(show(symtab(classes)), metac, "metac")
    assertNoDiff(show(symtab(convert(classes))), metacp, "metacp")
  }

  occurrences(
    "pattern vars in a class",
    """|class c { var (p, q) = (1, 2) }
       |""".stripMargin,
    """|[0:6..0:7): c <= _empty_/c#
       |[0:8..0:8):  <= _empty_/c#`<init>`().
       |[0:15..0:16): p <= _empty_/c#p().
       |[0:18..0:19): q <= _empty_/c#q().
       |""".stripMargin,
  )

  occurrences(
    "pattern vars in a local class",
    """|class a {
       |  def m = {
       |    class c { var (p, q) = (1, 2) }
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:7): a <= _empty_/a#
       |[0:8..0:8):  <= _empty_/a#`<init>`().
       |[1:6..1:7): m <= _empty_/a#m().
       |[2:10..2:11): c <= local0
       |[2:12..2:12):  <= local1
       |[2:19..2:20): p <= local3
       |[2:22..2:23): q <= local5
       |""".stripMargin,
  )

  occurrences(
    "pattern vars named with an operator and eq",
    """|class c { var (a_+, eq) = (1, 2) }
       |""".stripMargin,
    """|[0:6..0:7): c <= _empty_/c#
       |[0:8..0:8):  <= _empty_/c#`<init>`().
       |[0:15..0:18): a_+ <= _empty_/c#`a_+`().
       |[0:20..0:22): eq <= _empty_/c#eq().
       |""".stripMargin,
  )

  occurrences(
    "pattern vals named with an operator and eq",
    """|class c { val (a_+, eq) = (1, 2) }
       |""".stripMargin,
    """|[0:6..0:7): c <= _empty_/c#
       |[0:8..0:8):  <= _empty_/c#`<init>`().
       |[0:15..0:18): a_+ <= _empty_/c#`a_+`.
       |[0:20..0:22): eq <= _empty_/c#eq.
       |""".stripMargin,
  )

  occurrences(
    "pattern vars named with an operator and eq in a block",
    """|class a {
       |  def m = {
       |    var (a_+, eq) = (1, 2)
       |    a_+ + eq
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:7): a <= _empty_/a#
       |[0:8..0:8):  <= _empty_/a#`<init>`().
       |[1:6..1:7): m <= _empty_/a#m().
       |[2:9..2:12): a_+ <= local0
       |[2:14..2:16): eq <= local1
       |[3:4..3:7): a_+ => local0
       |[3:8..3:9): + => scala/Int#`+`(+4).
       |[3:10..3:12): eq => local1
       |""".stripMargin,
  )

  occurrences(
    "case class with a field named with an operator",
    """|case class k(a_+ : Int)
       |object o { k(1).a_+ }
       |""".stripMargin,
    """|[0:11..0:12): k <= _empty_/k#
       |[0:12..0:12):  <= _empty_/k#`<init>`().
       |[0:13..0:16): a_+ <= _empty_/k#`<init>`().(`a_+`)
       |[0:13..0:16): a_+ <= _empty_/k#`a_+`.
       |[0:19..0:22): Int => scala/Int#
       |[1:7..1:8): o <= _empty_/o.
       |[1:11..1:12): k => _empty_/k.
       |[1:16..1:19): a_+ => _empty_/k#`a_+`.
       |""".stripMargin,
  )

  locals(
    "function parameter named x+",
    """|class c { val f = (`x+`: Int) => `x+` }
       |""".stripMargin,
    """|local0 => param x+: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  locals(
    "self named x+",
    """|class c { `x+` => def m = `x+` }
       |""".stripMargin,
    """|local0 => selfparam x+
       |""".stripMargin,
  )

  symbols(
    "val named _+",
    """|class c { val `_+` = 1 }
       |""".stripMargin,
    """|_empty_/c# => class c extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/c#`<init>`(). => primary ctor <init>()
       |_empty_/c#`_+`. => val method _+: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  metacp(
    "case class with a field named with an operator",
    """|case class k(a_+ : Int)
       |""".stripMargin,
    "_empty_/k#`a_+`.",
    "_empty_/k#`a_+`. => a_+",
    "_empty_/k#`a_+`. => a_+",
  )

  metacp(
    "case class with a var named with an operator: getter",
    """|case class k(var a_+ : Int)
       |""".stripMargin,
    "_empty_/k#`a_+`().",
    "_empty_/k#`a_+`(). => a_+",
    "_empty_/k#`a_+`(). => a_+",
  )

  metacp(
    "case class with a var named with an operator: setter",
    """|case class k(var a_+ : Int)
       |""".stripMargin,
    "_empty_/k#`a_+_=`().",
    "_empty_/k#`a_+_=`(). => a_+_=",
    "_empty_/k#`a_+_=`(). => a_+_=",
  )

  metacp(
    "val named _+",
    """|class c { val `_+` = 1 }
       |""".stripMargin,
    "_empty_/c#`_+`.",
    "_empty_/c#`_+`. => _+",
    "_empty_/c#`_+`. => _+",
  )

  symbols(
    "vals named a and a with a trailing space",
    """|class c { val `a ` = 1; val a = 2 }
       |""".stripMargin,
    """|_empty_/c# => class c extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/c#`<init>`(). => primary ctor <init>()
       |_empty_/c#`a `. => val method a : Int
       |  Int => scala/Int#
       |_empty_/c#a. => val method a: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  metacp(
    "val named a with a trailing space",
    """|class c { val `a ` = 1 }
       |""".stripMargin,
    "_empty_/c#`a `.",
    "_empty_/c#`a `. => a ",
    "_empty_/c#`a `. => a ",
  )

  metacp(
    "val named a$+",
    """|class c { val `a$+` = 1 }
       |""".stripMargin,
    "_empty_/c#`a$+`.",
    "_empty_/c#`a$+`. => a$+",
    "_empty_/c#`a$+`. => a$+",
  )

  metacp(
    "case class with a field named a b",
    """|case class k(`a b`: Int)
       |""".stripMargin,
    "_empty_/k#`a b`.",
    "_empty_/k#`a b`. => a b",
    "_empty_/k#`a b`. => a b",
  )

  locals(
    "function parameter named x y",
    """|class c { val f = (`x y`: Int) => `x y` }
       |""".stripMargin,
    """|local0 => param x y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  metacp(
    "val named a with a trailing space: symbol a",
    """|class c { val `a ` = 1 }
       |""".stripMargin,
    "_empty_/c#a.",
    "",
    "",
  )

  metacp(
    "private[this] val named a with a trailing space",
    """|class c { private[this] val `a ` = 1 }
       |""".stripMargin,
    "_empty_/c#`a `.",
    "_empty_/c#`a `. => a ",
    "_empty_/c#`a `. => a ",
  )

  metacp(
    "val named +$+",
    """|class c { val `+$+` = 1 }
       |""".stripMargin,
    "_empty_/c#`+$+`.",
    "_empty_/c#`+$+`. => +$+",
    "_empty_/c#`+$+`. => +$+",
  )

  metacp(
    "def named x$+",
    """|class c { def `x$+` = 1 }
       |""".stripMargin,
    "_empty_/c#`x$+`().",
    "_empty_/c#`x$+`(). => x$+",
    "_empty_/c#`x$+`(). => x$+",
  )

  metacp(
    "vals named a$+ and +$+: symbol plus",
    """|class c { val `a$+` = 1; val `+$+` = 2 }
       |""".stripMargin,
    "_empty_/c#plus.",
    "",
    "",
  )

  metacp(
    "private[c] def named +",
    """|class c { private[c] def + = 1 }
       |""".stripMargin,
    "_empty_/c#`+`().",
    "_empty_/c#`+`(). => +",
    "_empty_/c#`+`(). => +",
  )

  metacp(
    "private[c] val named a with a trailing space",
    """|class c { private[c] val `a ` = 1 }
       |""".stripMargin,
    "_empty_/c#`a `.",
    "_empty_/c#`a `. => a ",
    "_empty_/c#`a `. => a ",
  )

  metacp(
    "private[c] def named x$$+",
    """|class c { private[c] def `x$$+` = 1 }
       |""".stripMargin,
    "_empty_/c#`x$$+`().",
    "_empty_/c#`x$$+`(). => x$$+",
    "_empty_/c#`x$$+`(). => x$$+",
  )

  metacp(
    "def named x$$+",
    """|class c { def `x$$+` = 1 }
       |""".stripMargin,
    "_empty_/c#`x$$+`().",
    "_empty_/c#`x$$+`(). => x$$+",
    "_empty_/c#`x$$+`(). => x$$+",
  )

  metacp(
    "def named c$$x",
    """|class c { def `c$$x` = 1 }
       |""".stripMargin,
    "_empty_/c#c$$x().",
    "_empty_/c#c$$x(). => c$$x",
    "_empty_/c#c$$x(). => c$$x",
  )

  metacp(
    "private[c] def named c$$x",
    """|class c { private[c] def `c$$x` = 1 }
       |""".stripMargin,
    "_empty_/c#c$$x().",
    "_empty_/c#c$$x(). => c$$x",
    "_empty_/c#c$$x(). => c$$x",
  )

  metacp(
    "private[c] val named x$$+",
    """|class c { private[c] val `x$$+` = 1 }
       |""".stripMargin,
    "_empty_/c#`x$$+`.",
    "_empty_/c#`x$$+`. => x$$+",
    "_empty_/c#`x$$+`. => x$$+",
  )

  metacp(
    "private[c] def named x$$+ in a package",
    """|package p
       |class c { private[c] def `x$$+` = 1 }
       |""".stripMargin,
    "p/c#`x$$+`().",
    "p/c#`x$$+`(). => x$$+",
    "p/c#`x$$+`(). => x$$+",
  )

  metacp(
    "private[c] def named x$$+ in an object",
    """|object o { class c { private[c] def `x$$+` = 1 } }
       |""".stripMargin,
    "_empty_/o.c#`x$$+`().",
    "_empty_/o.c#`x$$+`(). => x$$+",
    "_empty_/o.c#`x$$+`(). => x$$+",
  )

  metacp(
    "private[c] def named x$$+ in a package object",
    """|package object p { class c { private[c] def `x$$+` = 1 } }
       |""".stripMargin,
    "p/package.c#`x$$+`().",
    "p/package.c#`x$$+`(). => x$$+",
    "p/package.c#`x$$+`(). => x$$+",
  )

  metacp(
    "private[o] def named x$$+ in an inner class",
    """|class o { class c { private[o] def `x$$+` = 1 } }
       |""".stripMargin,
    "_empty_/o#c#`x$$+`().",
    "_empty_/o#c#`x$$+`(). => x$$+",
    "_empty_/o#c#`x$$+`(). => x$$+",
  )

  metacp(
    "private def named x called by an @inline def",
    """|class c { private def x = 1; @inline final def m = x }
       |""".stripMargin,
    "_empty_/c#x().",
    "_empty_/c#x(). => x",
    "_empty_/c#x(). => x",
  )

  metacp(
    "private def named + called by an @inline def",
    """|class c { private def + = 1; @inline final def m = this.+ }
       |""".stripMargin,
    "_empty_/c#`+`().",
    "_empty_/c#`+`(). => +",
    "_empty_/c#`+`(). => +",
  )

  metacp(
    "private def named x$$+ called by an @inline def",
    """|class c { private def `x$$+` = 1; @inline final def m = `x$$+` }
       |""".stripMargin,
    "_empty_/c#`x$$+`().",
    "_empty_/c#`x$$+`(). => x$$+",
    "_empty_/c#`x$$+`(). => x$$+",
  )

  metacp(
    "private val named x$$+ called by an @inline def",
    """|class c { private val `x$$+` = 1; @inline final def m = `x$$+` }
       |""".stripMargin,
    "_empty_/c#`x$$+`.",
    "_empty_/c#`x$$+`. => x$$+",
    "_empty_/c#`x$$+`. => x$$+",
  )

}

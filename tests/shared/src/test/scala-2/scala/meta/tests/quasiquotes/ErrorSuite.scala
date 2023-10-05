// FIXME: https://github.com/scalatest/scalatest/issues/1112
// If a test suite is defined in an empty package (i.e. outside any package declarations),
// then it will spuriously fail to compile.
// Discussion: https://github.com/scalameta/scalameta/issues/772#issuecomment-362380136

// package scala.meta.tests
package quasiquotes

import org.scalameta.tests._
import scala.meta.tests.TreeSuiteBase

import munit._
import typecheckError.Options.WithPositions

// FIXME: https://github.com/scalatest/scalatest/issues/1112
// I had to remove $ characters from all test names in this file.
// This is because ScalaTest seems to erroneously consider dollars to be name terminators,
// so it would spuriously crash with "duplicated test" exceptions for e.g.:
// test("...$ in Pat.Extract") { ... } and test("...$ in Pat.ExtractInfix")  { .. }.

class ErrorSuite extends TreeSuiteBase {
  test("val q\"type name[A] = B\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val q"type $name[$X] = $Y" = q"type List[+A] = List[A]"
    """
      ).nl2lf
        // Scala 2.13.7 adds additional message
        .replaceAll(
          "Identifiers that begin with uppercase are not pattern variables but match the value in scope.\r?\n",
          ""
        ),
      """|<macro>:4: not found: value X
         |      val q"type $name[$X] = $Y" = q"type List[+A] = List[A]"
         |                        ^""".stripMargin
    )
  }

  test("q\"foo + class\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"foo + class"
    """
      ).nl2lf,
      """|<macro>:4: `;` expected but `class` found
         |      q"foo + class"
         |              ^""".stripMargin.nl2lf
    )
  }

  test("q\"foo(x)\" when x has incompatible type") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      class Dummy
      val x = new Dummy
      q"foo($x)"
    """
      ).nl2lf,
      """|<macro>:6: type mismatch when unquoting;
         | found   : Dummy
         | required: scala.meta.Term
         |      q"foo($x)"
         |            ^""".stripMargin.nl2lf
    )
  }

  test("q\"x\" when x has incompatible type") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      class Dummy
      val x = new Dummy
      q"$x"
    """
      ).nl2lf,
      """|<macro>:6: type mismatch when unquoting;
         | found   : Dummy
         | required: scala.meta.Stat
         |      q"$x"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("q\"foo(..xs)\" when xs has incompatible type") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      class Dummy
      val xs = List(new Dummy)
      q"foo(..$xs)"
    """
      ).nl2lf,
      """|<macro>:6: type mismatch when unquoting;
         | found   : List[Dummy]
         | required: scala.meta.Term.ArgClause
         |      q"foo(..$xs)"
         |           ^""".stripMargin.nl2lf
    )
  }

  test("q\"foo(xs)\" when xs has incompatible type") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xs = List(q"x")
      q"foo($xs)"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : List[scala.meta.Term.Name]
         | required: scala.meta.Term
         |      q"foo($xs)"
         |            ^""".stripMargin.nl2lf
    )
  }

  test("q\"xs\" when xs has incompatible type") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xs = List(q"x")
      q"$xs"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : List[scala.meta.Term.Name]
         | required: scala.meta.Stat
         |      q"$xs"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("q\"...xss\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"...$xss"
    """
      ).nl2lf,
      """|<macro>:5: rank mismatch when unquoting;
         | found   : ...$
         | required: $ or ..$
         |      q"...$xss"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("q\"xss\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"$xss"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : List[List[scala.meta.Term.Name]]
         | required: scala.meta.Stat
         |      q"$xss"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("q\"foo[..terms]\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val terms = List(q"T", q"U")
      q"foo[..$terms]"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : List[scala.meta.Term.Name]
         | required: scala.meta.Type.ArgClause
         |      q"foo[..$terms]"
         |           ^""".stripMargin.nl2lf
    )
  }

  test("q\"foo(x, ..ys, z, ..ts)\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val tree = q"foo(1, 2, 3)"
      tree match {
        case q"$_($x, ..$ys, $z, ..$ts)" =>
          println(x)
          println(ys)
          println(z)
      }
    """
      ).nl2lf,
      """|<macro>:6: rank mismatch when unquoting;
         | found   : ..$
         | required: $
         |Note that you can extract a list into an unquote when pattern matching,
         |it just cannot follow another list either directly or indirectly.
         |        case q"$_($x, ..$ys, $z, ..$ts)" =>
         |                                 ^""".stripMargin.nl2lf
    )
  }

  test("q\"\"\" \"$x\" \"\"\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val x = "hello"
      qQQQ "$x" QQQ
    """
      ).nl2lf,
      """|<macro>:5: can't unquote into string literals
      |      qQQQ "$x" QQQ
      |            ^""".replace("QQQ", "\"\"\"").stripMargin.nl2lf
    )
  }

  test("q\"val name = foo\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = q"x"
      q"val $name = foo"
    """
      ).nl2lf,
      """|<macro>:5: can't unquote a name here, use a pattern instead (e.g. p"x")
         |      q"val $name = foo"
         |            ^""".stripMargin.nl2lf
    )
  }

  test("q\"var name = foo\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = q"x"
      q"var $name = foo"
    """
      ).nl2lf,
      """|<macro>:5: can't unquote a name here, use a pattern instead (e.g. p"x")
         |      q"var $name = foo"
         |            ^""".stripMargin.nl2lf
    )
  }

  test("p\"name: T\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = q"x"
      p"$name: T"
    """
      ).nl2lf,
      """|<macro>:5: can't unquote a name here, use a pattern instead (e.g. p"x")
         |      p"$name: T"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("""q"qname" when qname has incompatible type """) {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = t"x"
      q"$name"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : scala.meta.Type.Name
         | required: scala.meta.Stat
         |      q"$name"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("""q"expr: tpe" when tpe has incompatible type """) {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val tpe = q"T"
      q"expr: $tpe"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : scala.meta.Term.Name
         | required: scala.meta.Type
         |      q"expr: $tpe"
         |              ^""".stripMargin.nl2lf
    )
  }

  test("""q"expr: tpe" when expr has incompatible type """) {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val expr = t"x"
      q"$expr: tpe"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : scala.meta.Type.Name
         | required: scala.meta.Term
         |      q"$expr: tpe"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("""q"expr: tpes" """) {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val tpes = List(q"T")
      q"expr: ..$tpes"
    """
      ).nl2lf,
      """|<macro>:5: `identifier` expected but `ellipsis` found
         |      q"expr: ..$tpes"
         |              ^""".stripMargin.nl2lf
    )
  }

  test("""q"expr.name" when name has incompatible type """) {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = t"T"
      q"expr.$name"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : scala.meta.Type.Name
         | required: scala.meta.Term.Name
         |      q"expr.$name"
         |             ^""".stripMargin.nl2lf
    )
  }

  test("""q"expr.name" when expr has incompatible type """) {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val expr = t"T"
      q"$expr.name"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : scala.meta.Type.Name
         | required: scala.meta.Term
         |      q"$expr.name"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("""q"expr.names" """) {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val names = List(q"T")
      q"expr. ..$names"
    """
      ).nl2lf,
      """|<macro>:5: `identifier` expected but `ellipsis` found
         |      q"expr. ..$names"
         |              ^""".stripMargin.nl2lf
    )
  }

  test("""p"pat @ pat"""") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val pat1 = p"`x`"
      val pat2 = p"y"
      p"$pat1 @ $pat2"
    """
      ).nl2lf,
      """|<macro>:6: can't unquote a name here, use a pattern instead (e.g. p"x")
         |      p"$pat1 @ $pat2"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("""p"ref[..tpes](..pats)""") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val p"$ref[..$tpes](..$pats)" = p"x[A, B]"
    """
      ).nl2lf,
      """|<macro>:4: pattern must be a value or have parens: x[A, B]
         |      val p"$ref[..$tpes](..$pats)" = p"x[A, B]"
         |                                               ^""".stripMargin.nl2lf
    )
  }

  test("""p"pat: tpe"""") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val pat = p"`x`"
      val tpe = t"T"
      p"$pat: $tpe"
    """
      ).nl2lf,
      """|<macro>:6: can't unquote a name here, use a pattern instead (e.g. p"x")
         |      p"$pat: $tpe"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("p\"case X: T =>\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val p"case $X: T => " = p"case x: T =>"
    """
      ).nl2lf // Scala 2.13.7 adds additional message
        .replaceAll(
          "Identifiers that begin with uppercase are not pattern variables but match the value in scope.\r?\n",
          ""
        ),
      """|<macro>:4: not found: value X
         |      val p"case $X: T => " = p"case x: T =>"
         |                  ^""".stripMargin.nl2lf
    )
  }

  test("""q"..mods def this(...paramss) = expr"""") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"private final def this(x: X, y: Y) = foo"
    """
      ).nl2lf,
      """|<macro>:4: `this` expected but `identifier` found
         |      q"private final def this(x: X, y: Y) = foo"
         |                                             ^""".stripMargin.nl2lf
    )
  }

  test("unquote List[T] into Option[List[T]]") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val stats = List(q"def x = 42")
      q"class C { $stats }"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : List[scala.meta.Defn.Def]
         | required: scala.meta.Stat
         |      q"class C { $stats }"
         |                  ^""".stripMargin
    )
  }

  test("unquote Option[List[T]] into Option[List[T]]") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val stats = Some(List(q"def x = 42"))
      q"class C { $stats }"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : Some[List[scala.meta.Defn.Def]]
         | required: scala.meta.Stat
         |      q"class C { $stats }"
         |                  ^""".stripMargin.nl2lf
    )
  }

  test("q\"package foo {}; package bar {}\"") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"package foo {}; package bar {}"
    """
      ).nl2lf,
      """|<macro>:4: these statements can't be mixed together, try source"..." instead
         |      q"package foo {}; package bar {}"
         |        ^""".stripMargin.nl2lf
    )
  }

  test("unquote into character literals") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = 'f'
      q"'$foo'"
    """
      ).nl2lf,
      """|<macro>:5: can't unquote into character literals
         |      q"'$foo'"
         |         ^""".stripMargin.nl2lf
    )
  }

  test("unquote into single-line string literals") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      qQQQ "$foo" QQQ
    """
      ).nl2lf,
      """|<macro>:5: can't unquote into string literals
         |      qQQQ "$foo" QQQ
         |            ^""".stripMargin.replace("QQQ", "\"\"\"").nl2lf
    )
  }

  test("unquote into single-line string interpolations") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      qQQQ s"$foo" QQQ
    """
      ).nl2lf,
      """|<macro>:5: can't unquote into string interpolations
      |      qQQQ s"$foo" QQQ
      |             ^""".replace("QQQ", "\"\"\"").stripMargin
    )
  }

  test("unquote into single-line string interpolations, with braces") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      qQQQ s"${foo}" QQQ
    """
      ).nl2lf,
      """|<macro>:5: can't unquote into string interpolations
         |      qQQQ s"${foo}" QQQ
         |             ^""".replace("QQQ", "\"\"\"").stripMargin
    )
  }

  test("unquote into single-line string interpolations, with braces amd complex expression") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      qQQQ s"${foo + foo}" QQQ
    """
      ).nl2lf,
      """|<macro>:5: can't unquote into string interpolations
         |      qQQQ s"${foo + foo}" QQQ
         |             ^""".replace("QQQ", "\"\"\"").stripMargin
    )
  }

  test("unquote into xml literals") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      q"<$foo></foo>"
    """
      ).nl2lf,
      """|<macro>:5: type mismatch when unquoting;
         | found   : String
         | required: scala.meta.Term.Name
         |      q"<$foo></foo>"
         |         ^""".stripMargin
    )
  }

  test("unquote into backquoted identifiers") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      q"`$foo`"
    """
      ).nl2lf,
      """|<macro>:5: can't unquote into quoted identifiers
         |      q"`$foo`"
         |         ^""".stripMargin
    )
  }

  test("unquote into single-line comments") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val content = "content"
      q"// $content has been unquoted"
    """
      ).nl2lf,
      """|<macro>:5: can't unquote into single-line comments
         |      q"// $content has been unquoted"
         |           ^""".stripMargin
    )
  }

  test("unquote into multiline comments") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val content = "content"
      q"/* $content has been unquoted */"
    """
      ).nl2lf,
      """|<macro>:5: can't unquote into multi-line comments
         |      q"/* $content has been unquoted */"
         |           ^""".stripMargin
    )
  }

  test("weirdness after dot-dot") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"..x${???}"
    """
      ).nl2lf,
      """|<macro>:4: $, ( or { expected but identifier found
         |      q"..x${???}"
         |          ^""".stripMargin
    )
  }

  test("weirdness after triple-dor") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"foo(...x${???})"
    """
      ).nl2lf,
      """|<macro>:4: $, ( or { expected but identifier found
         |      q"foo(...x${???})"
         |               ^""".stripMargin
    )
  }

  test("x before triple-dot") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"foo(x, ...$xss)"
    """
      ).nl2lf,
      """|<macro>:5: rank mismatch when unquoting;
         | found   : ...$
         | required: $ or ..$
         |      q"foo(x, ...$xss)"
         |               ^""".stripMargin
    )
  }

  test("no-dot before triple-dot") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val x = q"x"
      val xss = List(List(q"x"))
      q"foo($x, ...$xss)"
    """
      ).nl2lf,
      """|<macro>:6: rank mismatch when unquoting;
         | found   : ...$
         | required: $ or ..$
         |      q"foo($x, ...$xss)"
         |                ^""".stripMargin
    )
  }

  test("dot-dot before triple-dot") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xs = List(q"x")
      val xss = List(List(q"x"))
      q"foo(..$xs, ...$xss)"
    """
      ).nl2lf,
      """|<macro>:6: rank mismatch when unquoting;
         | found   : ...$
         | required: $ or ..$
         |      q"foo(..$xs, ...$xss)"
         |                   ^""".stripMargin
    )
  }

  test("triple-dot before triple-dot") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"foo(...$xss, ...$xss)"
    """
      ).nl2lf,
      """|<macro>:5: `)` expected but `,` found
         |      q"foo(...$xss, ...$xss)"
         |                   ^""".stripMargin
    )
  }

  test("triple-dot inside triple-dot (1)") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      ??? match {
        case q"$_(...$argss)(...$_)" =>
      }
    """
      ).nl2lf,
      """|<macro>:5: rank mismatch when unquoting;
         | found   : ..$
         | required: $
         |Note that you can extract a list into an unquote when pattern matching,
         |it just cannot follow another list either directly or indirectly.
         |        case q"$_(...$argss)(...$_)" =>
         |                            ^""".stripMargin
    )
  }

  test("triple-dot inside triple-dot (2)") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      ??? match {
        case q"$_(...$argss)(foo)(...$_)" =>
      }
    """
      ).nl2lf,
      """|<macro>:5: rank mismatch when unquoting;
         | found   : ..$
         | required: $
         |Note that you can extract a list into an unquote when pattern matching,
         |it just cannot follow another list either directly or indirectly.
         |        case q"$_(...$argss)(foo)(...$_)" =>
         |                                 ^""".stripMargin
    )
  }

  test("triple-dot in Term.ApplyInfix") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val argss = List(List("y"))
      q"x + (...$argss)"
    """
      ).nl2lf,
      """|<macro>:5: rank mismatch when unquoting;
         | found   : ...$
         | required: $ or ..$
         |      q"x + (...$argss)"
         |             ^""".stripMargin
    )
  }

  test("triple-dot in Pat.Extract") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val patss = List(List("x"))
      p"Foo(...$patss)"
    """
      ).nl2lf,
      """|<macro>:5: rank mismatch when unquoting;
         | found   : ...$
         | required: $ or ..$
         |      p"Foo(...$patss)"
         |            ^""".stripMargin
    )
  }

  test("triple-dot in Pat.ExtractInfix") {
    assertEquals(
      typecheckError(
        """
      import scala.meta._
      import scala.meta.dialects.Scala211
      val patss = List(List("x"))
      p"x Foo (...$patss)"
    """
      ).nl2lf,
      """|<macro>:5: rank mismatch when unquoting;
         | found   : ...$
         | required: $ or ..$
         |      p"x Foo (...$patss)"
         |               ^""".stripMargin
    )
  }

  test("expected-template") {
    assertNoDiff(
      typecheckError(
        """
      import scala.meta._
      val notReallyAParent = t"_root_.scala.AnyVal"
      q"class C $notReallyAParent"
    """
      ),
      """|<macro>:4: type mismatch when unquoting;
         | found   : scala.meta.Type.Select
         | required: scala.meta.Template
         |      q"class C $notReallyAParent"
         |                ^
         |""".stripMargin
    )
  }

  test("expected-init") {
    assertNoDiff(
      typecheckError(
        """
      import scala.meta._
      val notReallyAParent = t"_root_.scala.AnyVal"
      q"class C extends $notReallyAParent"
    """
      ),
      """|<macro>:4: type mismatch when unquoting;
         | found   : scala.meta.Type.Select
         | required: scala.meta.Init
         |      q"class C extends $notReallyAParent"
         |                        ^
         |""".stripMargin
    )
  }
}

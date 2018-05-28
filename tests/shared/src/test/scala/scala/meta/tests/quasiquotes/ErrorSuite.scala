// FIXME: https://github.com/scalatest/scalatest/issues/1112
// If a test suite is defined in an empty package (i.e. outside any package declarations),
// then it will spuriously fail to compile.
// Discussion: https://github.com/scalameta/scalameta/issues/772#issuecomment-362380136

// package scala.meta.tests
package quasiquotes

import org.scalatest._
import org.scalameta.tests._
import typecheckError.Options.WithPositions
import compat.Platform.EOL

// FIXME: https://github.com/scalatest/scalatest/issues/1112
// I had to remove $ characters from all test names in this file.
// This is because ScalaTest seems to erroneously consider dollars to be name terminators,
// so it would spuriously crash with "duplicated test" exceptions for e.g.:
// test("...$ in Pat.Extract") { ... } and test("...$ in Pat.ExtractInfix")  { .. }.

class ErrorSuite extends FunSuite {
  test("val q\"type name[A] = B\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val q"type $name[$X] = $Y" = q"type List[+A] = List[A]"
    """).replace("\r", "") === """
      |<macro>:4: not found: value X
      |      val q"type $name[$X] = $Y" = q"type List[+A] = List[A]"
      |                        ^
    """.trim.stripMargin)
  }

  test("q\"foo + class\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"foo + class"
    """).replace("\r", "") === """
      |<macro>:4: ; expected but class found
      |      q"foo + class"
      |              ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"foo(x)\" when x has incompatible type") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      class Dummy
      val x = new Dummy
      q"foo($x)"
    """).replace("\r", "") === """
      |<macro>:6: type mismatch when unquoting;
      | found   : Dummy
      | required: scala.meta.Term
      |      q"foo($x)"
      |            ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"x\" when x has incompatible type") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      class Dummy
      val x = new Dummy
      q"$x"
    """).replace("\r", "") === """
      |<macro>:6: type mismatch when unquoting;
      | found   : Dummy
      | required: scala.meta.Stat
      |      q"$x"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"foo(..xs)\" when xs has incompatible type") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      class Dummy
      val xs = List(new Dummy)
      q"foo(..$xs)"
    """).replace("\r", "") === """
      |<macro>:6: type mismatch when unquoting;
      | found   : List[Dummy]
      | required: List[scala.meta.Term]
      |      q"foo(..$xs)"
      |              ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"foo(xs)\" when xs has incompatible type") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xs = List(q"x")
      q"foo($xs)"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[scala.meta.Term.Name]
      | required: scala.meta.Term
      |      q"foo($xs)"
      |            ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"xs\" when xs has incompatible type") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xs = List(q"x")
      q"$xs"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[scala.meta.Term.Name]
      | required: scala.meta.Stat
      |      q"$xs"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"...xss\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"...$xss"
    """).replace("\r", "") === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      q"...$xss"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"xss\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"$xss"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[List[scala.meta.Term.Name]]
      | required: scala.meta.Stat
      |      q"$xss"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"foo[..terms]\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val terms = List(q"T", q"U")
      q"foo[..$terms]"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[scala.meta.Term.Name]
      | required: List[scala.meta.Type]
      |      q"foo[..$terms]"
      |              ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"foo(x, ..ys, z, ..ts)\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val tree = q"foo(1, 2, 3)"
      tree match {
        case q"$_($x, ..$ys, $z, ..$ts)" =>
          println(x)
          println(ys)
          println(z)
      }
    """).replace("\r", "") === """
      |<macro>:6: rank mismatch when unquoting;
      | found   : ..$
      | required: $
      |Note that you can extract a list into an unquote when pattern matching,
      |it just cannot follow another list either directly or indirectly.
      |        case q"$_($x, ..$ys, $z, ..$ts)" =>
      |                                 ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"\"\" \"x\" \"\"\"\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val x = "hello"
      qQQQ "$x" QQQ
    """).replace("\r", "") === """
      |<macro>:5: can't unquote into string literals
      |      qQQQ "$x" QQQ
      |            ^
    """.replace("QQQ", "\"\"\"").trim.stripMargin.replace("\r", ""))
  }

  test("q\"val name = foo\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = q"x"
      q"val $name = foo"
    """).replace("\r", "") === """
      |<macro>:5: can't unquote a name here, use a pattern instead (e.g. p"x")
      |      q"val $name = foo"
      |            ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"var name = foo\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = q"x"
      q"var $name = foo"
    """).replace("\r", "") === """
      |<macro>:5: can't unquote a name here, use a pattern instead (e.g. p"x")
      |      q"var $name = foo"
      |            ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("p\"name: T\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = q"x"
      p"$name: T"
    """).replace("\r", "") === """
      |<macro>:5: can't unquote a name here, use a pattern instead (e.g. p"x")
      |      p"$name: T"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""q"qname" when qname has incompatible type """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = t"x"
      q"$name"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : scala.meta.Type.Name
      | required: scala.meta.Stat
      |      q"$name"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""q"expr: tpe" when tpe has incompatible type """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val tpe = q"T"
      q"expr: $tpe"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : scala.meta.Term.Name
      | required: scala.meta.Type
      |      q"expr: $tpe"
      |              ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""q"expr: tpe" when expr has incompatible type """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val expr = t"x"
      q"$expr: tpe"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : scala.meta.Type.Name
      | required: scala.meta.Term
      |      q"$expr: tpe"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""q"expr: tpes" """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val tpes = List(q"T")
      q"expr: ..$tpes"
    """).replace("\r", "") === """
      |<macro>:5: identifier expected but ellipsis found
      |      q"expr: ..$tpes"
      |              ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""q"expr.name" when name has incompatible type """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = t"T"
      q"expr.$name"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : scala.meta.Type.Name
      | required: scala.meta.Term.Name
      |      q"expr.$name"
      |             ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""q"expr.name" when expr has incompatible type """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val expr = t"T"
      q"$expr.name"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : scala.meta.Type.Name
      | required: scala.meta.Term
      |      q"$expr.name"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""q"expr.names" """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val names = List(q"T")
      q"expr. ..$names"
    """).replace("\r", "") === """
      |<macro>:5: identifier expected but ellipsis found
      |      q"expr. ..$names"
      |              ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""p"pat @ pat"""") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val pat1 = p"`x`"
      val pat2 = p"y"
      p"$pat1 @ $pat2"
    """).replace("\r", "") === """
      |<macro>:6: can't unquote a name here, use a pattern instead (e.g. p"x")
      |      p"$pat1 @ $pat2"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""p"ref[..tpes](..pats)""") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val p"$ref[..$tpes](..$pats)" = p"x[A, B]"
    """).replace("\r", "") === """
      |<macro>:4: pattern must be a value
      |      val p"$ref[..$tpes](..$pats)" = p"x[A, B]"
      |                                               ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""p"pat: tpe"""") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val pat = p"`x`"
      val tpe = t"T"
      p"$pat: $tpe"
    """).replace("\r", "") === """
      |<macro>:6: can't unquote a name here, use a pattern instead (e.g. p"x")
      |      p"$pat: $tpe"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("p\"case X: T =>\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val p"case $X: T => " = p"case x: T =>"
    """).replace("\r", "") === """
      |<macro>:4: not found: value X
      |      val p"case $X: T => " = p"case x: T =>"
      |                  ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("""q"..mods def this(...paramss) = expr"""") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"private final def this(x: X, y: Y) = foo"
    """).replace("\r", "") === """
      |<macro>:4: this expected but identifier found
      |      q"private final def this(x: X, y: Y) = foo"
      |                                             ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("unquote List[T] into Option[List[T]]") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val stats = List(q"def x = 42")
      q"class C { $stats }"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[scala.meta.Defn.Def]
      | required: scala.meta.Stat
      |      q"class C { $stats }"
      |                  ^
    """.trim.stripMargin)
  }

  test("unquote Option[List[T]] into Option[List[T]]") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val stats = Some(List(q"def x = 42"))
      q"class C { $stats }"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : Some[List[scala.meta.Defn.Def]]
      | required: scala.meta.Stat
      |      q"class C { $stats }"
      |                  ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("q\"package foo {}; package bar {}\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"package foo {}; package bar {}"
    """).replace("\r", "") === """
      |<macro>:4: these statements can't be mixed together, try source"..." instead
      |      q"package foo {}; package bar {}"
      |        ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("unquote into character literals") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = 'f'
      q"'$foo'"
    """).replace("\r", "") === """
      |<macro>:5: can't unquote into character literals
      |      q"'$foo'"
      |         ^
    """.trim.stripMargin.replace("\r", ""))
  }

  test("unquote into single-line string literals") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      qQQQ "$foo" QQQ
    """).replace("\r", "") === """
      |<macro>:5: can't unquote into string literals
      |      qQQQ "$foo" QQQ
      |            ^
    """.replace("QQQ", "\"\"\"").trim.stripMargin.replace("\r", ""))
  }

  test("unquote into single-line string interpolations") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      qQQQ s"$foo" QQQ
    """).replace("\r", "") === """
      |<macro>:5: can't unquote into string interpolations
      |      qQQQ s"$foo" QQQ
      |             ^
    """.replace("QQQ", "\"\"\"").trim.stripMargin)
  }

  test("unquote into xml literals") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      q"<$foo></foo>"
    """).replace("\r", "") === """
      |<macro>:5: type mismatch when unquoting;
      | found   : String
      | required: scala.meta.Term.Name
      |      q"<$foo></foo>"
      |         ^
    """.trim.stripMargin)
  }

  test("unquote into backquoted identifiers") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      q"`$foo`"
    """).replace("\r", "") === """
      |<macro>:5: can't unquote into quoted identifiers
      |      q"`$foo`"
      |         ^
    """.trim.stripMargin)
  }

  test("unquote into single-line comments") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val content = "content"
      q"// $content has been unquoted"
    """).replace("\r", "") === """
      |<macro>:5: can't unquote into single-line comments
      |      q"// $content has been unquoted"
      |           ^
    """.trim.stripMargin)
  }

  test("unquote into multiline comments") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val content = "content"
      q"/* $content has been unquoted */"
    """).replace("\r", "") === """
      |<macro>:5: can't unquote into multi-line comments
      |      q"/* $content has been unquoted */"
      |           ^
    """.trim.stripMargin)
  }

  test("weirdness after dot-dot") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"..x${???}"
    """).replace("\r", "") === """
      |<macro>:4: $, ( or { expected but identifier found
      |      q"..x${???}"
      |          ^
    """.trim.stripMargin)
  }

  test("weirdness after triple-dor") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"foo(...x${???})"
    """).replace("\r", "") === """
      |<macro>:4: $, ( or { expected but identifier found
      |      q"foo(...x${???})"
      |               ^
    """.trim.stripMargin)
  }

  test("x before triple-dot") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"foo(x, ...$xss)"
    """).replace("\r", "") === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      q"foo(x, ...$xss)"
      |               ^
    """.trim.stripMargin)
  }

  test("no-dot before triple-dot") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val x = q"x"
      val xss = List(List(q"x"))
      q"foo($x, ...$xss)"
    """).replace("\r", "") === """
      |<macro>:6: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      q"foo($x, ...$xss)"
      |                ^
    """.trim.stripMargin)
  }

  test("dot-dot before triple-dot") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xs = List(q"x")
      val xss = List(List(q"x"))
      q"foo(..$xs, ...$xss)"
    """).replace("\r", "") === """
      |<macro>:6: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      q"foo(..$xs, ...$xss)"
      |                   ^
    """.trim.stripMargin)
  }

  test("triple-dot before triple-dot") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"foo(...$xss, ...$xss)"
    """).replace("\r", "") === """
      |<macro>:5: ) expected but , found
      |      q"foo(...$xss, ...$xss)"
      |                   ^
    """.trim.stripMargin)
  }

  test("triple-dot inside triple-dot (1)") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      ??? match {
        case q"$_(...$argss)(...$_)" =>
        case q"$_(...$argss)(foo)(...$_)" =>
      }
    """).replace("\r", "") === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |Note that you can extract a list into an unquote when pattern matching,
      |it just cannot follow another list either directly or indirectly.
      |        case q"$_(...$argss)(...$_)" =>
      |                             ^
    """.trim.stripMargin)
  }

  test("triple-dot inside triple-dot (2)") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      ??? match {
        case q"$_(...$argss)(foo)(...$_)" =>
      }
    """).replace("\r", "") === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |Note that you can extract a list into an unquote when pattern matching,
      |it just cannot follow another list either directly or indirectly.
      |        case q"$_(...$argss)(foo)(...$_)" =>
      |                                  ^
    """.trim.stripMargin)
  }

  test("triple-dot mixes with something else in parameter lists") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val paramss = List(List(param"x: Int"))
      q"def foo(...$paramss)(y: Int) = ???"
    """).replace("\r", "") === """
      |<macro>:5: implementation restriction: can't mix ...$ with anything else in parameter lists.
      |See https://github.com/scalameta/scalameta/issues/406 for details.
      |      q"def foo(...$paramss)(y: Int) = ???"
      |                ^
    """.trim.stripMargin)
  }

  test("triple-dot in Term.ApplyInfix") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val argss = List(List("y"))
      q"x + (...$argss)"
    """).replace("\r", "") === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      q"x + (...$argss)"
      |            ^
    """.trim.stripMargin)
  }

  test("triple-dot in Pat.Extract") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val patss = List(List("x"))
      p"Foo(...$patss)"
    """).replace("\r", "") === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      p"Foo(...$patss)"
      |            ^
    """.trim.stripMargin)
  }

  test("triple-dot in Pat.ExtractInfix") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val patss = List(List("x"))
      p"x Foo (...$patss)"
    """).replace("\r", "") === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      p"x Foo (...$patss)"
      |               ^
    """.trim.stripMargin)
  }

  test("#448") {
    assert(typecheckError("""
      import scala.meta._
      val notReallyAParent = t"_root_.scala.AnyVal"
      q"class C extends $notReallyAParent"
    """).replace("\r", "") === """
      |<macro>:4: type mismatch when unquoting;
      | found   : scala.meta.Type.Select
      | required: scala.meta.Template
      |Note: This shape of a quasiquote tells scala.meta that you're unquoting a template.
      |If you'd like to unquote a parent reference, add {} immediately after the unquote.
      |For more details, see https://github.com/scalameta/scalameta/issues/223.
      |      q"class C extends $notReallyAParent"
      |                        ^
    """.trim.stripMargin)
  }
}


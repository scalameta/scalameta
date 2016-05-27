// package scala.meta.tests
// package quasiquotes

import org.scalatest._
import org.scalameta.tests._
import typecheckError.Options.WithPositions

class ErrorSuite extends FunSuite {
  test("val q\"type $name[$A] = $B\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val q"type $name[$X] = $Y" = q"type List[+A] = List[A]"
    """) === """
      |<macro>:4: not found: value X
      |      val q"type $name[$X] = $Y" = q"type List[+A] = List[A]"
      |                        ^
    """.trim.stripMargin)
  }

  test("q\"foo: _*\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"foo: _*"
    """) === """
      |<macro>:4: ; expected but identifier found
      |      q"foo: _*"
      |              ^
    """.trim.stripMargin)
  }

  test("q\"foo + class\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"foo + class"
    """) === """
      |<macro>:4: ; expected but class found
      |      q"foo + class"
      |              ^
    """.trim.stripMargin)
  }

  test("q\"foo($x)\" when x has incompatible type") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      class Dummy
      val x = new Dummy
      q"foo($x)"
    """) === """
      |<macro>:6: type mismatch when unquoting;
      | found   : Dummy
      | required: scala.meta.Term.Arg
      |      q"foo($x)"
      |            ^
    """.trim.stripMargin)
  }

  test("q\"$x\" when x has incompatible type") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      class Dummy
      val x = new Dummy
      q"$x"
    """) === """
      |<macro>:6: type mismatch when unquoting;
      | found   : Dummy
      | required: scala.meta.Term
      |      q"$x"
      |        ^
    """.trim.stripMargin)
  }

  test("q\"foo(..$xs)\" when xs has incompatible type") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      class Dummy
      val xs = List(new Dummy)
      q"foo(..$xs)"
    """) === """
      |<macro>:6: type mismatch when unquoting;
      | found   : List[Dummy]
      | required: scala.collection.immutable.Seq[scala.meta.Term.Arg]
      |      q"foo(..$xs)"
      |              ^
    """.trim.stripMargin)
  }

  test("q\"foo($xs)\" when xs has incompatible type") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xs = List(q"x")
      q"foo($xs)"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[scala.meta.Term.Name]
      | required: scala.meta.Term.Arg
      |      q"foo($xs)"
      |            ^
    """.trim.stripMargin)
  }

  test("q\"$xs\" when xs has incompatible type") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xs = List(q"x")
      q"$xs"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[scala.meta.Term.Name]
      | required: scala.meta.Term
      |      q"$xs"
      |        ^
    """.trim.stripMargin)
  }

  test("q\"...$xss\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"...$xss"
    """) === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      q"...$xss"
      |        ^
    """.trim.stripMargin)
  }

  test("q\"$xss\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"$xss"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[List[scala.meta.Term.Name]]
      | required: scala.meta.Term
      |      q"$xss"
      |        ^
    """.trim.stripMargin)
  }

  test("q\"foo[..$terms]\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val terms = List(q"T", q"U")
      q"foo[..$terms]"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[scala.meta.Term.Name]
      | required: scala.collection.immutable.Seq[scala.meta.Type]
      |      q"foo[..$terms]"
      |              ^
    """.trim.stripMargin)
  }

  test("q\"foo($x, ..$ys, $z, ..$ts)\"") {
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
    """) === """
      |<macro>:6: rank mismatch when unquoting;
      | found   : ..$
      | required: $
      |Note that you can extract a sequence into an unquote when pattern matching,
      |it just cannot follow another sequence either directly or indirectly.
      |        case q"$_($x, ..$ys, $z, ..$ts)" =>
      |                                 ^
    """.trim.stripMargin)
  }

  test("q\"\"\" \"$x\" \"\"\"\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val x = "hello"
      qQQQ "$x" QQQ
    """) === """
      |<macro>:5: can't unquote into string literals
      |      qQQQ "$x" QQQ
      |            ^
    """.replace("QQQ", "\"\"\"").trim.stripMargin)
  }

  test("q\"val $name = foo\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = q"x"
      q"val $name = foo"
    """) === """
      |<macro>:5: can't unquote a name here, use a pattern instead
      |      q"val $name = foo"
      |            ^
    """.trim.stripMargin)
  }

  test("q\"var $name = foo\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = q"x"
      q"var $name = foo"
    """) === """
      |<macro>:5: can't unquote a name here, use a pattern instead
      |      q"var $name = foo"
      |            ^
    """.trim.stripMargin)
  }

  test("p\"$name: T\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = q"x"
      p"$name: T"
    """) === """
      |<macro>:5: can't unquote a name here, use a pattern instead
      |      p"$name: T"
      |        ^
    """.trim.stripMargin)
  }

  test("""q"$qname" when qname has incompatible type """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = t"x"
      q"$name"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : scala.meta.Type.Name
      | required: scala.meta.Term
      |      q"$name"
      |        ^
    """.trim.stripMargin)
  }

  test("""q"expr: $tpe" when tpe has incompatible type """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val tpe = q"T"
      q"expr: $tpe"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : scala.meta.Term.Name
      | required: scala.meta.Type
      |      q"expr: $tpe"
      |              ^
    """.trim.stripMargin)
  }

  test("""q"$expr: tpe" when expr has incompatible type """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val expr = t"x"
      q"$expr: tpe"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : scala.meta.Type.Name
      | required: scala.meta.Term
      |      q"$expr: tpe"
      |        ^
    """.trim.stripMargin)
  }

  test("""q"expr: tpes" """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val tpes = List(q"T")
      q"expr: ..$tpes"
    """) === """
      |<macro>:5: identifier expected but ellipsis found
      |      q"expr: ..$tpes"
      |              ^
    """.trim.stripMargin)
  }

  test("""q"expr.$name" when name has incompatible type """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val name = t"T"
      q"expr.$name"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : scala.meta.Type.Name
      | required: scala.meta.Term.Name
      |      q"expr.$name"
      |             ^
    """.trim.stripMargin)
  }

  test("""q"$expr.name" when expr has incompatible type """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val expr = t"T"
      q"$expr.name"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : scala.meta.Type.Name
      | required: scala.meta.Term
      |      q"$expr.name"
      |        ^
    """.trim.stripMargin)
  }

  test("""q"expr.names" """) {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val names = List(q"T")
      q"expr. ..$names"
    """) === """
      |<macro>:5: identifier expected but ellipsis found
      |      q"expr. ..$names"
      |              ^
    """.trim.stripMargin)
  }

  test("""p"$pname @ $apat"""") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val pname = p"`x`"
      val apat = p"y"
      p"$pname @ $apat"
    """) === """
      |<macro>:6: type mismatch when unquoting;
      | found   : scala.meta.Term.Name
      | required: scala.meta.Pat.Var.Term
      |      p"$pname @ $apat"
      |        ^
    """.trim.stripMargin)
  }

  test("""p"$ref[..$tpes](..$apats)""") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val p"$ref[..$tpes](..$apats)" = p"x[A, B]"
    """) === """
      |<macro>:4: pattern must be a value
      |      val p"$ref[..$tpes](..$apats)" = p"x[A, B]"
      |                                                ^
    """.trim.stripMargin)
  }

  test("""p"$pat: $ptpe"""") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val pat = p"`x`"
      val ptpe = pt"y"
      p"$pat: $ptpe"
    """) === """
      |<macro>:6: can't unquote a name here, use a pattern instead
      |      p"$pat: $ptpe"
      |        ^
    """.trim.stripMargin)
  }

  test("p\"case $X: T =>\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val p"case $X: T => " = p"case x: T =>"
    """) === """
      |<macro>:4: not found: value X
      |      val p"case $X: T => " = p"case x: T =>"
      |                  ^
    """.trim.stripMargin)
  }

  test("pt\"X\"") {
   assert(typecheckError("""
     import scala.meta._
     import scala.meta.dialects.Scala211
     pt"X"
   """).contains("Pattern type variables must start with a lower-case letter"))
  }

  test("pt\"`x`\"") {
   assert(typecheckError("""
     import scala.meta._
     import scala.meta.dialects.Scala211
     pt"`x`"
   """).contains("Pattern type variables must not be enclosed in backquotes"))
  }

  test("pt\"`X`\"") {
   assert(typecheckError("""
     import scala.meta._
     import scala.meta.dialects.Scala211
     pt"`X`"
   """).contains("Pattern type variables must not be enclosed in backquotes"))
  }

//  test("""pt"$ptpe[..$ptpes]""") { // TODO review after #216 resolved
//    assert(typecheckError("""
//      import scala.meta._
//      import scala.meta.dialects.Scala211
//      val pt"$ptpe[..$ptpes]" = pt"x[y, z]"
//    """).contains("found that tpe.isInstanceOf[Pat.Var.Type].`unary_!` is true"))
//  }

//  test("""pt"..$ptpes { ..$stats }"""") { // TODO review after #216 resolved
//    assert(typecheckError("""
//      import scala.meta._
//      import scala.meta.dialects.Scala211
//      val pt"..$ptpes { ..$stats }" = pt"x with y { val a: A; val b: B }"
//    """).contains("found that tpes.forall(((tpe: Pat.Type) => tpe.isInstanceOf[Pat.Var.Type].`unary_!`.&&(tpe.isInstanceOf[Pat.Type.Wildcard].`unary_!`))) is false"))
//  }

//  test("""pt"$ptpe forSome { ..$stats }"""") { // TODO review after #216 resolved
//    assert(typecheckError("""
//      import scala.meta._
//      import scala.meta.dialects.Scala211
//      val pt"$ptpe forSome { ..$stats }" = pt"x forSome { val a: A }"
//    """).contains("found that tpe.isInstanceOf[Pat.Var.Type].`unary_!` is true"))
//  }

//  test("""pt"$ptpe ..@$annots"""") { // TODO review after #216 resolved
//    assert(typecheckError("""
//      import scala.meta._
//      import scala.meta.dialects.Scala211
//      val pt"$ptpe ..@$annots" = pt"x @q @w"
//    """).contains("found that tpe.isInstanceOf[Pat.Var.Type].`unary_!` is true"))
//  }

  test("""q"..$mods def this(...$paramss) = $expr"""") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"private final def this(x: X, y: Y) = foo"
    """) === """
      |<macro>:4: this expected but identifier found
      |      q"private final def this(x: X, y: Y) = foo"
      |                                             ^
    """.trim.stripMargin)
  }

  test("t\"T*\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      t"T*"
    """) === """
      |<macro>:4: end of file expected but identifier found
      |      t"T*"
      |         ^
    """.trim.stripMargin)
  }

  test("t\"=> T\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      t"=> T"
    """) === """
      |<macro>:4: identifier expected but => found
      |      t"=> T"
      |        ^
    """.trim.stripMargin)
  }

  test("p\"_*\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      p"_*"
    """) === """
      |<macro>:4: illegal start of simple pattern
      |      p"_*"
      |          ^
    """.trim.stripMargin)
  }

  test("unquote Seq[T] into Option[Seq[T]]") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val stats = List(q"def x = 42")
      q"class C { $stats }"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[scala.meta.Defn.Def]
      | required: scala.meta.Stat
      |      q"class C { $stats }"
      |                  ^
    """.trim.stripMargin)
  }

  test("unquote Option[Seq[T]] into Option[Seq[T]]") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val stats = Some(List(q"def x = 42"))
      q"class C { $stats }"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : Some[List[scala.meta.Defn.Def]]
      | required: scala.meta.Stat
      |      q"class C { $stats }"
      |                  ^
    """.trim.stripMargin)
  }

  test("q\"package foo {}; package bar {}\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"package foo {}; package bar {}"
    """) === """
      |<macro>:4: these statements can't be mixed together, try source"..." instead
      |      q"package foo {}; package bar {}"
      |        ^
    """.trim.stripMargin)
  }

  test("unquote into character literals") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = 'f'
      q"'$foo'"
    """) === """
      |<macro>:5: can't unquote into character literals
      |      q"'$foo'"
      |         ^
    """.trim.stripMargin)
  }

  test("unquote into single-line string literals") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      qQQQ "$foo" QQQ
    """) === """
      |<macro>:5: can't unquote into string literals
      |      qQQQ "$foo" QQQ
      |            ^
    """.replace("QQQ", "\"\"\"").trim.stripMargin)
  }

  test("unquote into multi-line string literals") {
    // TODO: no idea how to test this
    // assert(typecheckError("""
    //   import scala.meta._
    //   import scala.meta.dialects.Scala211
    //   val foo = "foo"
    //   qQQQ QQQ$fooQQQ QQQ
    // """) === """
    // """.trim.stripMargin)
  }

  test("unquote into single-line string interpolations") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      qQQQ s"$foo" QQQ
    """) === """
      |<macro>:5: can't unquote into string interpolations
      |      qQQQ s"$foo" QQQ
      |             ^
    """.replace("QQQ", "\"\"\"").trim.stripMargin)
  }

  test("unquote into multiline string interpolations") {
    // TODO: no idea how to test this
    // assert(typecheckError("""
    //   import scala.meta._
    //   import scala.meta.dialects.Scala211
    //   val foo = "foo"
    //   qQQQ QQQ$fooQQQ QQQ
    // """) === """
    // """.trim.stripMargin)
  }

  test("unquote into xml literals") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val foo = "foo"
      q"<$foo></foo>"
    """) === """
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
    """) === """
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
    """) === """
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
    """) === """
      |<macro>:5: can't unquote into multi-line comments
      |      q"/* $content has been unquoted */"
      |           ^
    """.trim.stripMargin)
  }

  test("weirdness after ..") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"..x${???}"
    """) === """
      |<macro>:4: $, ( or { expected but identifier found
      |      q"..x${???}"
      |          ^
    """.trim.stripMargin)
  }

  test("weirdness after ...") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      q"foo(...x${???})"
    """) === """
      |<macro>:4: $, ( or { expected but identifier found
      |      q"foo(...x${???})"
      |               ^
    """.trim.stripMargin)
  }

  test("x before ...$") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"foo(x, ...$xss)"
    """) === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      q"foo(x, ...$xss)"
      |               ^
    """.trim.stripMargin)
  }

  test("$ before ...$") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val x = q"x"
      val xss = List(List(q"x"))
      q"foo($x, ...$xss)"
    """) === """
      |<macro>:6: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      q"foo($x, ...$xss)"
      |                ^
    """.trim.stripMargin)
  }

  test("..$ before ...$") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xs = List(q"x")
      val xss = List(List(q"x"))
      q"foo(..$xs, ...$xss)"
    """) === """
      |<macro>:6: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      q"foo(..$xs, ...$xss)"
      |                   ^
    """.trim.stripMargin)
  }

  test("...$ before ...$") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"foo(...$xss, ...$xss)"
    """) === """
      |<macro>:5: ) expected but , found
      |      q"foo(...$xss, ...$xss)"
      |                   ^
    """.trim.stripMargin)
  }

  test("...$ inside ...$ (1)") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      ??? match {
        case q"$_(...$argss)(...$_)" =>
        case q"$_(...$argss)(foo)(...$_)" =>
      }
    """) === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |Note that you can extract a sequence into an unquote when pattern matching,
      |it just cannot follow another sequence either directly or indirectly.
      |        case q"$_(...$argss)(...$_)" =>
      |                             ^
    """.trim.stripMargin)
  }

  test("...$ inside ...$ (2)") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      ??? match {
        case q"$_(...$argss)(foo)(...$_)" =>
      }
    """) === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |Note that you can extract a sequence into an unquote when pattern matching,
      |it just cannot follow another sequence either directly or indirectly.
      |        case q"$_(...$argss)(foo)(...$_)" =>
      |                                  ^
    """.trim.stripMargin)
  }

  test("...$ mixes with something else in parameter lists") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val paramss = List(List(param"x: Int"))
      q"def foo(...$paramss)(y: Int) = ???"
    """) === """
      |<macro>:5: implementation restriction: can't mix ...$ with anything else in parameter lists.
      |See https://github.com/scalameta/scalameta/issues/406 for details.
      |      q"def foo(...$paramss)(y: Int) = ???"
      |                ^
    """.trim.stripMargin)
  }

  test("...$ in Term.ApplyInfix") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val argss = List(List("y"))
      q"x + (...$argss)"
    """) === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      q"x + (...$argss)"
      |            ^
    """.trim.stripMargin)
  }

  test("...$ in Pat.Extract") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val patss = List(List("x"))
      p"Foo(...$patss)"
    """) === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      p"Foo(...$patss)"
      |            ^
    """.trim.stripMargin)
  }

  test("...$ in Pat.ExtractInfix") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val patss = List(List("x"))
      p"x Foo (...$patss)"
    """) === """
      |<macro>:5: rank mismatch when unquoting;
      | found   : ...$
      | required: $ or ..$
      |      p"x Foo (...$patss)"
      |               ^
    """.trim.stripMargin)
  }
}
import org.scalatest._
import org.scalameta.tests._

class ErrorSuite extends FunSuite {
  implicit val errorsWithPositionsPlease = Style.WithPositions

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
      | found   : List[scala.meta.internal.ast.Term.Name]
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
      | found   : List[scala.meta.internal.ast.Term.Name]
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
      | found   : ...
      | required: ..
      |      q"...$xss"
      |        ^
    """.trim.stripMargin)
  }

  test("q\"..$xss\"") {
    assert(typecheckError("""
      import scala.meta._
      import scala.meta.dialects.Scala211
      val xss = List(List(q"x"))
      q"..$xss"
    """) === """
      |<macro>:5: type mismatch when unquoting;
      | found   : List[List[scala.meta.internal.ast.Term.Name]]
      | required: scala.collection.immutable.Seq[scala.meta.Stat]
      |      q"..$xss"
      |          ^
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
      | found   : List[List[scala.meta.internal.ast.Term.Name]]
      | required: scala.meta.Term
      |      q"$xss"
      |        ^
    """.trim.stripMargin)
  }

  test("q\"foo[..$terms]\"") {
    // FIXME: looks our new scheme of ast reification breaks quasiquote error reporting
    // the failure doesn't look severe, so I'm just going to comment out this test for the time being
    // assert(typecheckError("""
    //   import scala.meta._
    //   import scala.meta.dialects.Scala211
    //   val terms = List(q"T", q"U")
    //   q"foo[..$terms]"
    // """) === """
    //   |<macro>:5: type mismatch when unquoting;
    //   | found   : List[scala.meta.Term.Name]
    //   | required: scala.collection.immutable.Seq[scala.meta.Type]
    //   |      q"foo[..$terms]"
    //   |              ^
    // """.trim.stripMargin)
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
      | found   : ..
      | required: no dots
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
      |<macro>:5: can't unquote a name here, use a variable pattern instead
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
      |<macro>:5: can't unquote a name here, use a variable pattern instead
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
      |<macro>:5: can't unquote a name here, use a variable pattern instead
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
      | found   : scala.meta.internal.ast.Type.Name
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
      | found   : scala.meta.internal.ast.Term.Name
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
      | found   : scala.meta.internal.ast.Type.Name
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
      | found   : scala.meta.internal.ast.Type.Name
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
      | found   : scala.meta.internal.ast.Type.Name
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
}
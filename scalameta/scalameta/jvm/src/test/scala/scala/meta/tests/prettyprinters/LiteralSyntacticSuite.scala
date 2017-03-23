
package scala.meta.tests
package prettyprinters

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.ast._

// TODO: move back into SyntacticSuite. Scala.js does not support runtime checks
// on primitives like Float/Double/Char/Byte/... See:
// https://www.scala-js.org/doc/semantics.html#runtime-type-tests-are-based-on-values
// The TreeSyntax pretty printer relies on runtime type checks.
class LiteralSyntacticSuite extends SyntacticSuite {
  test("literalTypes") {
    intercept[ParseException] {
      dialects.Scala211("val a : 42 = 42").parse[Stat].get.show[Syntax]
    }
    val Scala211 = null // TODO: #389
    import scala.meta.dialects.Dotty
    assert(q"val a: 42 = 42".show[Syntax] === "val a: 42 = 42")
    assert(q"val a: 42L = 42L".show[Syntax] === "val a: 42L = 42L")
    assert(q"val a: 42d = 42d".show[Syntax] === "val a: 42.0d = 42.0d")
    assert(q"val a: 42f = 42f".show[Syntax] === "val a: 42.0f = 42.0f")
    assert(q"val a: true = true".show[Syntax] === "val a: true = true")
    assert(q"val a: false = false".show[Syntax] === "val a: false = false")
    assert(dialects.Dotty("val a: \"42\" = \"42\"").parse[Stat].get.show[Syntax] === "val a: \"42\" = \"42\"")
    assert(pat("_: 42").show[Syntax] === "_: 42")
    assert(pat("_: 42.0f").show[Syntax] === "_: 42.0f")
    assert(pat("_: 42.0d").show[Syntax] === "_: 42.0d")
    assert(pat("_: 42L").show[Syntax] === "_: 42L")
    assert(pat("_: true").show[Syntax] === "_: true")
    assert(pat("_: false").show[Syntax] === "_: false")
  }

  test("assorted literals") {
    assert(templStat("true").show[Syntax] === "true")
    assert(templStat("false").show[Syntax] === "false")
    assert(templStat("0").show[Syntax] === "0")
    assert(templStat("0l").show[Syntax] === "0L")
    assert(templStat("0L").show[Syntax] === "0L")
    assert(templStat("0f").show[Syntax] === "0.0f")
    assert(templStat("0F").show[Syntax] === "0.0f")
    assert(templStat("0.0").show[Syntax] === "0.0d")
    assert(templStat("0d").show[Syntax] === "0.0d")
    assert(templStat("0D").show[Syntax] === "0.0d")
    assert(templStat("'0'").show[Syntax] === "'0'")
    assert(templStat("\"0\"").show[Syntax] === "\"0\"")
    assert(templStat("'zero").show[Syntax] === "'zero")
    assert(templStat("null").show[Syntax] === "null")
    assert(templStat("()").show[Syntax] === "()")
  }
}



package scala.meta.tests
package prettyprinters

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.ast._

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
}


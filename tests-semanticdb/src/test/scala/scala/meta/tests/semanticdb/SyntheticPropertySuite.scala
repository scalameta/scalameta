package scala.meta.tests
package semanticdb

import scala.meta.internal.semanticdb._
import scala.meta.internal.{semanticdb => s}

// Regression tests for #1492: compiler-synthesized symbols (members generated for
// `case` classes and `AnyVal` classes) must carry the SYNTHETIC property, while
// symbols written by the user must not.
class SyntheticPropertySuite extends SemanticdbSuite {

  private def info(doc: s.TextDocument, suffix: String): s.SymbolInformation =
    doc.symbols.filter(_.symbol.endsWith(suffix)) match {
      case Seq(info) => info
      case Nil => fail(
          s"no symbol ending with '$suffix' in:\n" + doc.symbols.map(_.symbol).sorted.mkString("\n"),
        )
      case many => fail(s"ambiguous suffix '$suffix' matched: " + many.map(_.symbol).mkString(", "))
    }

  private def assertSynthetic(doc: s.TextDocument, suffix: String): Unit =
    assert(info(doc, suffix).isSynthetic, s"expected '$suffix' to be SYNTHETIC")

  private def assertNotSynthetic(doc: s.TextDocument, suffix: String): Unit =
    assert(!info(doc, suffix).isSynthetic, s"expected '$suffix' to NOT be SYNTHETIC")

  targeted(
    """|package p
       |case class Foo(x: Int) {
       |  def bar: Int = x
       |}
       |""".stripMargin,
    { doc =>
      // positive: members the compiler synthesizes for case classes
      assertSynthetic(doc, "p/Foo#equals().")
      assertSynthetic(doc, "p/Foo#hashCode().")
      assertSynthetic(doc, "p/Foo#copy().")
      assertSynthetic(doc, "p/Foo#canEqual().")
      assertSynthetic(doc, "p/Foo#productArity().")
      assertSynthetic(doc, "p/Foo.apply().")
      assertSynthetic(doc, "p/Foo.unapply().")
      // negative: user-written declarations are not synthetic
      assertNotSynthetic(doc, "p/Foo#")
      assertNotSynthetic(doc, "p/Foo#bar().")
      assertNotSynthetic(doc, "p/Foo#x.")
    },
  )

  targeted(
    """|package q
       |class Meters(val value: Int) extends AnyVal
       |""".stripMargin,
    { doc =>
      // positive: equals/hashCode synthesized for value classes
      assertSynthetic(doc, "q/Meters#equals().")
      assertSynthetic(doc, "q/Meters#hashCode().")
      // negative: the user-declared value accessor is not synthetic
      assertNotSynthetic(doc, "q/Meters#")
      assertNotSynthetic(doc, "q/Meters#value.")
    },
  )

  targeted(
    // #1492: an explicit self binding is written in source, so even though scalac
    // marks the self symbol synthetic it must not get the SYNTHETIC property.
    """|package r
       |trait T { self => }
       |""".stripMargin,
    { doc =>
      val self = doc.symbols.find(_.isSelfParameter).getOrElse(fail("no self parameter symbol found"))
      assert(!self.isSynthetic, "explicit self parameter must NOT be SYNTHETIC")
    },
  )

}

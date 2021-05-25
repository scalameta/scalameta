package scala.meta.tests
package prettyprinters

import scala.meta._

class PlatformSyntacticSuite extends scala.meta.tests.parsers.ParseSuite {
  test("JVM has proper floats") {
    // this gives different results work in a JS runtime
    assert(Lit.Float(1.40f).syntax == "1.4f") // trailing zero is lost
    assert(Lit.Float(1.4f).syntax == "1.4f")
    // cross-platform way to accomplish the same
    assert(Lit.Float("1.40").syntax == "1.40f")
    assert(Lit.Float("1.4").syntax == "1.4f")
  }

  test("syntax vs inputSyntax: full") {
    val code = "import     foo.{ bar as   baz,   *  }"
    val tree = parsers.Parse.parseStat(Input.String(code), dialects.Scala3).get
    assertEquals(tree.inputSyntax, code)
    locally {
      implicit val dialect = dialects.Scala213
      assertEquals(tree.syntax, "import foo.{ bar => baz, _ }")
    }
    locally {
      implicit val dialect = dialects.Scala3
      assertEquals(tree.syntax, code)
    }
  }

  test("syntax vs inputSyntax: partial") {
    val importCode = "import     foo.{ bar as   baz,   *  }"
    val code = s"object a { $importCode }"
    val tree = parsers.Parse.parseStat(Input.String(code), dialects.Scala3).get
    val importTree = tree.asInstanceOf[Defn.Object].templ.stats.head
    assertEquals(importTree.inputSyntax, importCode)
    locally {
      implicit val dialect = dialects.Scala213
      assertEquals(importTree.syntax, "import foo.{ bar => baz, _ }")
    }
    locally {
      implicit val dialect = dialects.Scala3
      assertEquals(importTree.syntax, importCode)
    }
  }
}

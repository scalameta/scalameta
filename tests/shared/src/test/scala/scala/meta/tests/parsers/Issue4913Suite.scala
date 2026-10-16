package scala.meta.tests
package parsers

import scala.meta._

import munit.Location

// https://github.com/scalameta/scalameta/pull/4913
class Issue4913Suite extends ParseSuite {

  // for each ctor and init, the line where it and its name start, with `@@` at the start
  private def check(code: String, expected: String, dialect: Dialect = dialects.Scala213)(implicit
      loc: Location,
  ): Unit = test(code.linesIterator.next()) {
    val sb = new StringBuilder
    def at(label: String, t: Tree) = {
      val pos = t.pos
      val line = code.split("\n", -1)(pos.startLine)
      sb.append(label).append(": ").append(line.patch(pos.startColumn, "@@", 0)).append("\n")
    }
    dialect(code).parse[Source].get.traverse {
      case t: Ctor.Primary =>
        at("ctor", t)
        at("ctor name", t.name)
      case t: Init => at("init name", t.name)
    }
    assertNoDiff(sb.toString, expected)
  }

  check(
    """|class C // c
       |""".stripMargin,
    """|ctor: class C@@ // c
       |ctor name: class C@@ // c
       |""".stripMargin,
  )

  check(
    """|class C private[a] // c
       |""".stripMargin,
    """|ctor: class C @@private[a] // c
       |ctor name: class C private[a]@@ // c
       |""".stripMargin,
  )

  check(
    """|class C[T] // c
       |""".stripMargin,
    """|ctor: class C[T]@@ // c
       |ctor name: class C[T]@@ // c
       |""".stripMargin,
  )

  check(
    """|trait T // c
       |""".stripMargin,
    """|ctor: trait T@@ // c
       |ctor name: trait T@@ // c
       |""".stripMargin,
  )

  check(
    """|class C(x: Int) // c
       |""".stripMargin,
    """|ctor: class C@@(x: Int) // c
       |ctor name: class C@@(x: Int) // c
       |""".stripMargin,
  )

  check(
    """|class C { } // c
       |""".stripMargin,
    """|ctor: class C @@{ } // c
       |ctor name: class C @@{ } // c
       |""".stripMargin,
  )

  check(
    """|class D extends C // c
       |""".stripMargin,
    """|ctor: class D @@extends C // c
       |ctor name: class D @@extends C // c
       |init name: class D extends C@@ // c
       |""".stripMargin,
  )

  check(
    """|object O { new C // c
       |}
       |""".stripMargin,
    """|init name: object O { new C@@ // c
       |""".stripMargin,
  )

  check(
    """|object O { new C(1) // c
       |}
       |""".stripMargin,
    """|init name: object O { new C@@(1) // c
       |""".stripMargin,
  )

  check(
    """|enum E:
       |  case A // c
       |  case B
       |""".stripMargin,
    """|ctor: enum E@@:
       |ctor name: enum E@@:
       |ctor:   case A@@ // c
       |ctor name:   case A@@ // c
       |ctor:   case B@@
       |ctor name:   case B@@
       |""".stripMargin,
    dialects.Scala3,
  )

  check(
    """|class C // c
       |  extends B
       |""".stripMargin,
    """|ctor:   @@extends B
       |ctor name:   @@extends B
       |init name:   extends B@@
       |""".stripMargin,
  )

  check(
    """|class C // c
       |{ }
       |""".stripMargin,
    """|ctor: @@{ }
       |ctor name: @@{ }
       |""".stripMargin,
  )

}

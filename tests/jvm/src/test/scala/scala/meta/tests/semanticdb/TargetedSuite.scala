package scala.meta.tests
package semanticdb

// Contributing tips:
// - To run an individual test: ~testsJVM/testOnly *TargetedSuite -- -z "package YYY"
//   and replace YYY with the package name of your test.
// - On test failure, the obtained output is printed to the console for
//   easy copy-paste to replace the current expected output.
// - Try to follow the alphabetical order of the enclosing package, at the time
//   of this writing the latest package is `j`, so the next package should be `l`.
// - glhf, and if you have any questions don't hesitate to ask in the gitter channel :)
class TargetedSuite extends SemanticdbSuite {

  targeted(
    // curried function application with named args, #648
    """package a
      |object Curry {
      |  def bar(children: Int)(x: Int) = children + x
      |  <<bar>>(children = 4)(3)
      |}
    """.trim.stripMargin,
    { (_, second) => assert(second == "a/Curry.bar().") }
  )

  targeted(
    """
      |package b
      |case class User(name: String, age: Int)
      |object M {
      |  val u: User = ???
      |  u.<<copy>>(<<age>> = 43)
      |}
    """.trim.stripMargin,
    { (_, copy, age) =>
      assert(copy == "b/User#copy().")
      assert(age == "b/User#copy().(age)")
    }
  )

  // Checks def macros that we can't test in expect tests because expect tests have no dependencies.
  occurrences(
    """|package e
       |import scala.meta._
       |import munit._
       |object x extends FunSuite {
       |  val x = q"Foo"
       |  val y = q"Bar"
       |  val z = q"$x + $y"
       |  val k = sourcecode.Name.generate
       |  assert(x.value == "Foo")
       |}
    """.stripMargin,
    """|[0:8..0:9): e <= e/
       |[1:7..1:12): scala => scala/
       |[1:13..1:17): meta => scala/meta/
       |[2:7..2:12): munit => munit/
       |[3:7..3:8): x <= e/x.
       |[3:17..3:25): FunSuite => munit/FunSuite#
       |[3:26..3:26):  => munit/FunSuite#`<init>`().
       |[4:6..4:7): x <= e/x.x.
       |[4:10..4:11): q => scala/meta/internal/quasiquotes/Unlift.
       |[5:6..5:7): y <= e/x.y.
       |[5:10..5:11): q => scala/meta/internal/quasiquotes/Unlift.
       |[6:6..6:7): z <= e/x.z.
       |[6:10..6:11): q => scala/meta/internal/quasiquotes/Unlift.
       |[6:13..6:14): x => e/x.x.
       |[6:18..6:19): y => e/x.y.
       |[7:6..7:7): k <= e/x.k.
       |[7:10..7:20): sourcecode => sourcecode/
       |[7:21..7:25): Name => sourcecode/Name.
       |[7:26..7:34): generate => sourcecode/NameMacros#generate().
       |[8:2..8:8): assert => munit/Assertions#assert().
       |[8:9..8:10): x => e/x.x.
       |[8:11..8:16): value => scala/meta/Term.Name#value().
       |[8:17..8:19): == => java/lang/Object#`==`().
       |""".stripMargin
  )

  targeted(
    """package f
      |object an {
      |  for {
      |    i <- List(1, 2)
      |    <<j>> <- List(3, 4)
      |  } yield j
      |}
      """.stripMargin,
    { (db, j) =>
      val denot = db.symbols.find(_.symbol == j).get
      assert(denot.symbol.startsWith("local"))
    }
  )

  targeted(
    """package g
      |object ao {
      |  object <<foo>>
      |  def <<foo>>(a: Int): Unit = ()
      |  def <<foo>>(a: String): Unit = ()
      |}
    """.stripMargin,
    (doc, foo1, foo2, foo3) => {
      assert(foo1 == "g/ao.foo.")
      assert(foo2 == "g/ao.foo().")
      assert(foo3 == "g/ao.foo(+1).")
    }
  )

  targeted(
    """package h
      |object ao {
      |  val local = 1
      |  def foo(a: Int = 1, b: Int = 2, c: Int = 3): Int = a + b + c
      |  def baseCase = <<foo>>(<<local>>, <<c>> = 3)
      |}
    """.stripMargin,
    (doc, foo1, local, c) => {
      assert(foo1 == "h/ao.foo().")
      assert(local == "h/ao.local.")
      assert(c == "h/ao.foo().(c)")
    }
  )

  targeted(
    """package i
      |object ao {
      |  val local = 1
      |  def foo(a: Int = 1, b: Int = 2, c: Int = 3): Int = a + b + c
      |  def recursive = <<foo>>(<<local>>, <<c>> = <<foo>>(<<local>>, <<c>> = 3))
      |}
    """.stripMargin,
    (doc, foo1, local1, c1, foo2, local2, c2) => {
      assert(foo1 == "i/ao.foo().")
      assert(foo2 == "i/ao.foo().")
      assert(local1 == "i/ao.local.")
      assert(local2 == "i/ao.local.")
      assert(c1 == "i/ao.foo().(c)")
      assert(c2 == "i/ao.foo().(c)")
    }
  )

  targeted(
    """package j
      |object ao {
      |  case class Msg(body: String, head: String = "default", tail: String)
      |  val bodyText = "body"
      |  val msg = <<Msg>>(<<bodyText>>, tail = "tail")
      |}
    """.stripMargin,
    (doc, msg, bodyText) => {
      assert(msg == "j/ao.Msg.")
      assert(bodyText == "j/ao.bodyText.")
    }
  )

  targeted(
    """package k
      |class target {
      |  def foo(a: Int, b: Int = 1, c: Int = 2): Int = ???
      |}
      |object consumer {
      |  def unstableQual = new target
      |  unstableQual.<<foo>>(1, <<c>> = 1)
      |}
    """.stripMargin,
    (doc, foo, c) => {
      assert(foo == "k/target#foo().")
      assert(c == "k/target#foo().(c)")
    }
  )
}

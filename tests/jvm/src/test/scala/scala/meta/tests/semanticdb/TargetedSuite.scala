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
    """|package a
       |object Curry {
       |  def bar(children: Int)(x: Int) = children + x
       |  <<bar>>(children = 4)(3)
       |}
       |""".stripMargin,
    (_, second) => assertEquals(second, "a/Curry.bar().")
  )

  targeted(
    """|
       |package b
       |case class User(name: String, age: Int)
       |object M {
       |  val u: User = ???
       |  u.<<copy>>(<<age>> = 43)
       |}
       |""".stripMargin,
    { (_, copy, age) =>
      assertEquals(copy, "b/User#copy().")
      assertEquals(age, "b/User#copy().(age)")
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
       |""".stripMargin,
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
    """|package f
       |import scala.List
       |object an {
       |  for {
       |    i <- List.apply(1, 2)
       |    <<j>> <- List.apply(3, 4)
       |  } yield j
       |}
       |""".stripMargin,
    { (db, j) =>
      val denot = db.symbols.find(_.symbol == j).get
      assert(denot.symbol.startsWith("local"))
    }
  )

  targeted(
    """|package g
       |object ao {
       |  object <<foo>>
       |  def <<foo>>(a: Int): Unit = ()
       |  def <<foo>>(a: String): Unit = ()
       |}
       |""".stripMargin,
    (doc, foo1, foo2, foo3) => {
      assertEquals(foo1, "g/ao.foo.")
      assertEquals(foo2, "g/ao.foo().")
      assertEquals(foo3, "g/ao.foo(+1).")
    }
  )

  targeted(
    """|package h
       |object ao {
       |  val local = 1
       |  def foo(a: Int = 1, b: Int = 2, c: Int = 3): Int = a + b + c
       |  def baseCase = <<foo>>(<<local>>, <<c>> = 3)
       |}
       |""".stripMargin,
    (doc, foo1, local, c) => {
      assertEquals(foo1, "h/ao.foo().")
      assertEquals(local, "h/ao.local.")
      assertEquals(c, "h/ao.foo().(c)")
    }
  )

  targeted(
    """|package i
       |object ao {
       |  val local = 1
       |  def foo(a: Int = 1, b: Int = 2, c: Int = 3): Int = a + b + c
       |  def recursive = <<foo>>(<<local>>, <<c>> = <<foo>>(<<local>>, <<c>> = 3))
       |}
       |""".stripMargin,
    (doc, foo1, local1, c1, foo2, local2, c2) => {
      assertEquals(foo1, "i/ao.foo().")
      assertEquals(foo2, "i/ao.foo().")
      assertEquals(local1, "i/ao.local.")
      assertEquals(local2, "i/ao.local.")
      assertEquals(c1, "i/ao.foo().(c)")
      assertEquals(c2, "i/ao.foo().(c)")
    }
  )

  targeted(
    """|package j
       |object ao {
       |  case class Msg(body: String, head: String = "default", tail: String)
       |  val bodyText = "body"
       |  val msg = <<Msg>>(<<bodyText>>, tail = "tail")
       |}
       |""".stripMargin,
    (doc, msg, bodyText) => {
      assertEquals(msg, "j/ao.Msg.")
      assertEquals(bodyText, "j/ao.bodyText.")
    }
  )

  targeted(
    """|package k
       |class target {
       |  def foo(a: Int, b: Int = 1, c: Int = 2): Int = ???
       |}
       |object consumer {
       |  def unstableQual = new target
       |  unstableQual.<<foo>>(1, <<c>> = 1)
       |}
       |""".stripMargin,
    (doc, foo, c) => {
      assertEquals(foo, "k/target#foo().")
      assertEquals(c, "k/target#foo().(c)")
    }
  )

  targeted(
    """|package l
       |trait GrandParent{ def method: String }
       |trait Parent extends GrandParent{ override def method: String = "" }
       |trait Child extends Parent
       |object Max extends Child{ 
       |  override def method: String = "" 
       |  override def toString = "a"
       |}
       |
       |""".stripMargin,
    doc => {

      def overriddenSymbols(sym: String) = doc.symbols.find(_.symbol == sym).map { info =>
        info.overriddenSymbols
      }
      assertEquals(overriddenSymbols("l/GrandParent#method()."), Some(Nil))
      assertEquals(overriddenSymbols("l/Parent#method()."), Some(List("l/GrandParent#method().")))
      assertEquals(overriddenSymbols("l/Child#method()."), None)
      assertEquals(
        overriddenSymbols("l/Max.method()."),
        Some(List("l/Parent#method().", "l/GrandParent#method()."))
      )
      assertEquals(
        overriddenSymbols("l/Max.toString()."),
        Some(List("java/lang/Object#toString().", "scala/Any#toString()."))
      )

    }
  )

  targeted(
    """|package m
       |  import scala.languageFeature.implicitConversions
       |  object ImplicitConversion {
       |    val a: Int = 5
       |    val b: Long = <<a>>
       |    val c: Double = <<a>>
       |    val d: Float = <<a>>
       |    val toLong: Int = 42
       |    val e: Long = <<toLong>>
       |}
       |""".stripMargin,
    (_, long, double, float, toInt) => {
      assertEquals(long, "m/ImplicitConversion.a.")
      assertEquals(double, "m/ImplicitConversion.a.")
      assertEquals(float, "m/ImplicitConversion.a.")
      assertEquals(toInt, "scala/Int#toLong().")
    }
  )

  targeted(
    """|
       |  package n
       | object ForCompWithFilter {
       |  val foo: Option[(Int, Int)] = None
       |  for {
       |    (_, _) <- <<foo>>
       |    (_, _) <- <<foo>>
       |  } yield ()
       |}
       |""".stripMargin,
    (_, foo1, foo2) => {
      assertEquals(foo1, "n/ForCompWithFilter.foo.")
      assertEquals(foo2, "n/ForCompWithFilter.foo.")
    }
  )

  locally { // #3738
    val code = """|trait AmbiguousMend {
                  |  def x
                  |}
                  |""".stripMargin
    test(code) {
      assertEquals(
        computePayloadFromSnippet(code).linesIterator.drop(3).filter(!_.startsWith("Uri => "))
          .mkString("", "\n", "\n"),
        """|Summary:
           |Schema => SemanticDB v4
           |Text => non-empty
           |Language => Scala
           |Symbols => 2 entries
           |Occurrences => 2 entries
           |
           |Symbols:
           |_empty_/AmbiguousMend# => trait AmbiguousMend extends AnyRef { +1 decls }
           |  AnyRef => scala/AnyRef#
           |_empty_/AmbiguousMend#x(). => abstract method x: Unit
           |  Unit => scala/Unit#
           |
           |Occurrences:
           |[0:6..0:19): AmbiguousMend <= _empty_/AmbiguousMend#
           |[1:6..1:7): x <= _empty_/AmbiguousMend#x().
           |""".stripMargin
      )
    }
  }

}

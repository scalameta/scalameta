package scala.meta.tests.parsers.dotty

import scala.meta._

class ErasedDefsSuite extends BaseDottySuite {

  /**
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/experimental/erased-defs.html]]
   */

  test("def with erased params") {
    val code = "def methodWithErasedEv(erased ev: Ev, x: Int): Int = x + 2"
    runTestError[Stat](
      code,
      """|<input>:1: error: : expected but identifier found
         |def methodWithErasedEv(erased ev: Ev, x: Int): Int = x + 2
         |                              ^""".stripMargin
    )
  }

  test("val with erased lambda") {
    val code = "val lambdaWithErasedEv: (erased Ev, Int) => Int = (erased ev, x) => x + 2"
    runTestError[Stat](
      code,
      """|<input>:1: error: identifier expected but , found
         |val lambdaWithErasedEv: (erased Ev, Int) => Int = (erased ev, x) => x + 2
         |                                  ^""".stripMargin
    )
  }

  test("erased val") {
    val code = "erased val erasedEvidence: Ev = null"
    runTestError[Stat](
      code,
      """|<input>:1: error: ; expected but val found
         |erased val erasedEvidence: Ev = null
         |       ^""".stripMargin
    )
  }

  test("def with only one erased param") {
    val code = "def methodWithErasedEv(x: Int, erased ev: Ev): Int = ???"
    runTestError[Stat](
      code,
      """|<input>:1: error: : expected but identifier found
         |def methodWithErasedEv(x: Int, erased ev: Ev): Int = ???
         |                                      ^""".stripMargin
    )
  }

  test("def with both using and erased") {
    val code = "def turnedOn(using erased ev: IsOff[S]): Machine[On] = new Machine[On]"
    runTestError[Stat](
      code,
      """|<input>:1: error: identifier expected but : found
         |def turnedOn(using erased ev: IsOff[S]): Machine[On] = new Machine[On]
         |                            ^""".stripMargin
    )
  }

  test("erased class") {
    val code = "erased class CanRead"
    runTestError[Stat](
      code,
      """|<input>:1: error: ; expected but class found
         |erased class CanRead
         |       ^""".stripMargin
    )
  }

  test("erased given") {
    val code = "erased given IsEmpty[Empty] = new IsEmpty[Empty]"
    runTestError[Stat](
      code,
      """|<input>:1: error: ; expected but given found
         |erased given IsEmpty[Empty] = new IsEmpty[Empty]
         |       ^""".stripMargin
    )
  }

  test("erased param in lambda as argument") {
    val code =
      """|List(1, 2, 3).map {
         |  (using erased i: Int) => i
         |}""".stripMargin
    runTestError[Stat](
      code,
      """|<input>:2: error: ; expected but => found
         |  (using erased i: Int) => i
         |                        ^""".stripMargin
    )
  }

  test("anonymous-method 1") {
    val code = "val fun = (using erased ctx: Context) => ctx.open"
    runTestError[Stat](
      code,
      """|<input>:1: error: ; expected but => found
         |val fun = (using erased ctx: Context) => ctx.open
         |                                      ^""".stripMargin
    )
  }

  test("anonymous-method 2") {
    val code = "val fun = (using erased ctx) => ctx.open"
    runTestError[Stat](
      code,
      """|<input>:1: error: ; expected but => found
         |val fun = (using erased ctx) => ctx.open
         |                             ^""".stripMargin
    )
  }

  test("anonymous-method 3") {
    val code = "val fun = (using erased _: Context) => ctx.open"
    runTestError[Stat](
      code,
      """|<input>:1: error: ; expected but => found
         |val fun = (using erased _: Context) => ctx.open
         |                                    ^""".stripMargin
    )
  }

  test("lambda-method-parameter") {
    val code =
      """|LazyBody {
         |  (using erased ctx: Context) => 3
         |}
         |""".stripMargin
    runTestError[Stat](
      code,
      """|<input>:2: error: ; expected but => found
         |  (using erased ctx: Context) => 3
         |                              ^""".stripMargin
    )
  }

}

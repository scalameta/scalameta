package scala.meta.tests.parsers.dotty

import scala.meta._

class NamedTuplesSuite extends BaseDottySuite {

  test("simple-named-term") {
    runTestAssert[Stat]("(a = 123, b = \"123\")")(Term.Tuple(
      List(Term.Assign(tname("a"), lit(123)), Term.Assign(tname("b"), lit("123")))
    ))
  }

  test("complex-named-term") {
    runTestAssert[Stat]("(a = 123, b = \"123\", c = (a = 123, d = 2.01d))")(Term.Tuple(List(
      Term.Assign(tname("a"), lit(123)),
      Term.Assign(tname("b"), lit("123")),
      Term.Assign(
        tname("c"),
        Term.Tuple(List(Term.Assign(tname("a"), lit(123)), Term.Assign(tname("d"), lit(2.01d))))
      )
    )))
  }

  test("simple-named-type") {
    runTestAssert[Type]("(a: Int, b: String)")(Type.Tuple(List(
      Type.TypedParam(pname("a"), pname("Int"), Nil),
      Type.TypedParam(pname("b"), pname("String"), Nil)
    )))

  }

  test("simple-named-type-assign") {
    runTestAssert[Stat]("type T = (a: Int, b: String)")(Defn.Type(
      Nil,
      pname("T"),
      Nil,
      Type.Tuple(List(
        Type.TypedParam(pname("a"), pname("Int"), Nil),
        Type.TypedParam(pname("b"), pname("String"), Nil)
      )),
      noBounds
    ))
  }

  test("named-tuple-summon") {
    val code = """|val y = summon[Tuple2[Int, String] <:< (x: Int, y: String)]
                  |""".stripMargin
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(patvar("y")),
      None,
      tapplytype(
        tname("summon"),
        pinfix(
          papply("Tuple2", "Int", "String"),
          "<:<",
          Type.Tuple(List(
            Type.TypedParam(pname("x"), pname("Int"), Nil),
            Type.TypedParam(pname("y"), pname("String"), Nil)
          ))
        )
      )
    ))
  }

  test("complex-named-type") {
    runTestAssert[Type]("(a: Int, b: String, c: (a: Int, d: Double))")(Type.Tuple(List(
      Type.TypedParam(pname("a"), pname("Int"), Nil),
      Type.TypedParam(pname("b"), pname("String"), Nil),
      Type.TypedParam(
        pname("c"),
        Type.Tuple(List(
          Type.TypedParam(pname("a"), pname("Int"), Nil),
          Type.TypedParam(pname("d"), pname("Double"), Nil)
        )),
        Nil
      )
    )))

  }

  test("simple-named-pattern") {
    runTestAssert[Stat](
      """|a match {
         |  case (a = 123, b = "123") =>
         |}
         |""".stripMargin
    )(tmatch(
      tname("a"),
      Case(
        Pat.Tuple(List(Pat.Assign(tname("a"), lit(123)), Pat.Assign(tname("b"), lit("123")))),
        None,
        blk()
      )
    ))
  }

  test("complex-named-pattern") {
    runTestAssert[Stat](
      """|a match {
         |  case (a = 123, b = (c = "123", d = 123)) =>
         |}
         |""".stripMargin
    )(tmatch(
      tname("a"),
      Case(
        Pat.Tuple(List(
          Pat.Assign(tname("a"), lit(123)),
          Pat.Assign(
            tname("b"),
            Pat.Tuple(List(Pat.Assign(tname("c"), lit("123")), Pat.Assign(tname("d"), lit(123))))
          )
        )),
        None,
        blk()
      )
    ))
  }

  test("extractor with named fields: all") {
    val code = """|a match {
                  |  case Foo(name = nme, id = 123) =>
                  |}
                  |""".stripMargin
    val error = """|<input>:2: error: `)` expected but `=` found
                   |  case Foo(name = nme, id = 123) =>
                   |                ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("extractor with named fields: some, with varargs") {
    val code = """|a match {
                  |  case Foo(x = y, z, rest*) =>
                  |}
                  |""".stripMargin
    val error = """|<input>:2: error: `)` expected but `=` found
                   |  case Foo(x = y, z, rest*) =>
                   |             ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("extractor with named fields: assignment") {
    val code = """val Foo(name = name, id = id) = Foo(name = "123", id = 456)"""
    val error = """|<input>:1: error: `)` expected but `=` found
                   |val Foo(name = name, id = id) = Foo(name = "123", id = 456)
                   |             ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("extractor with named fields: case clause only") {
    val code = """case (a = 123, b = (c = "123", d = 123)) =>"""
    val tree = Case(
      Pat.Tuple(List(
        Pat.Assign("a", lit(123)),
        Pat.Assign("b", Pat.Tuple(List(Pat.Assign("c", lit("123")), Pat.Assign("d", lit(123)))))
      )),
      None,
      blk()
    )
    runTestAssert[Case](code)(tree)
  }

}

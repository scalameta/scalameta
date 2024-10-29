package scala.meta.tests.parsers.dotty

import scala.meta._

class NamedTuplesSuite extends BaseDottySuite {

  test("simple-named-term") {
    runTestAssert[Stat]("(a = 123, b = \"123\")")(Term.Tuple(
      List(Term.Assign(Term.Name("a"), Lit.Int(123)), Term.Assign(Term.Name("b"), Lit.String("123")))
    ))
  }

  test("complex-named-term") {
    runTestAssert[Stat]("(a = 123, b = \"123\", c = (a = 123, d = 2.01d))")(Term.Tuple(List(
      Term.Assign(Term.Name("a"), Lit.Int(123)),
      Term.Assign(Term.Name("b"), Lit.String("123")),
      Term.Assign(
        Term.Name("c"),
        Term.Tuple(List(
          Term.Assign(Term.Name("a"), Lit.Int(123)),
          Term.Assign(Term.Name("d"), Lit.Double(2.01d))
        ))
      )
    )))
  }

  test("simple-named-type") {
    runTestAssert[Type]("(a: Int, b: String)")(Type.Tuple(List(
      Type.TypedParam(Type.Name("a"), Type.Name("Int"), Nil),
      Type.TypedParam(Type.Name("b"), Type.Name("String"), Nil)
    )))

  }
  test("simple-named-type-assign") {
    runTestAssert[Stat]("type T = (a: Int, b: String)")(Defn.Type(
      Nil,
      Type.Name("T"),
      Type.ParamClause(Nil),
      Type.Tuple(List(
        Type.TypedParam(Type.Name("a"), Type.Name("Int"), Nil),
        Type.TypedParam(Type.Name("b"), Type.Name("String"), Nil)
      )),
      Type.Bounds(None, None)
    ))

  }

  test("named-tuple-summon") {
    val code = """|val y = summon[Tuple2[Int, String] <:< (x: Int, y: String)]
                  |""".stripMargin
    runTestAssert[Stat](code)(Defn.Val(
      Nil,
      List(Pat.Var(Term.Name("y"))),
      None,
      Term.ApplyType(
        Term.Name("summon"),
        Type.ArgClause(List(Type.ApplyInfix(
          Type
            .Apply(Type.Name("Tuple2"), Type.ArgClause(List(Type.Name("Int"), Type.Name("String")))),
          Type.Name("<:<"),
          Type.Tuple(List(
            Type.TypedParam(Type.Name("x"), Type.Name("Int"), Nil),
            Type.TypedParam(Type.Name("y"), Type.Name("String"), Nil)
          ))
        )))
      )
    ))
  }

  test("complex-named-type") {
    runTestAssert[Type]("(a: Int, b: String, c: (a: Int, d: Double))")(Type.Tuple(List(
      Type.TypedParam(Type.Name("a"), Type.Name("Int"), Nil),
      Type.TypedParam(Type.Name("b"), Type.Name("String"), Nil),
      Type.TypedParam(
        Type.Name("c"),
        Type.Tuple(List(
          Type.TypedParam(Type.Name("a"), Type.Name("Int"), Nil),
          Type.TypedParam(Type.Name("d"), Type.Name("Double"), Nil)
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
    )(Term.Match(
      Term.Name("a"),
      Term.CasesBlock(List(Case(
        Pat.Tuple(List(
          Pat.Assign(Term.Name("a"), Lit.Int(123)),
          Pat.Assign(Term.Name("b"), Lit.String("123"))
        )),
        None,
        Term.Block(Nil)
      ))),
      Nil
    ))
  }

  test("complex-named-pattern") {
    runTestAssert[Stat](
      """|a match {
         |  case (a = 123, b = (c = "123", d = 123)) =>
         |}
         |""".stripMargin
    )(Term.Match(
      Term.Name("a"),
      Term.CasesBlock(List(Case(
        Pat.Tuple(List(
          Pat.Assign(Term.Name("a"), Lit.Int(123)),
          Pat.Assign(
            Term.Name("b"),
            Pat.Tuple(List(
              Pat.Assign(Term.Name("c"), Lit.String("123")),
              Pat.Assign(Term.Name("d"), Lit.Int(123))
            ))
          )
        )),
        None,
        Term.Block(Nil)
      ))),
      Nil
    ))
  }
}

package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class MatchTypeSuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)

  test("simple") {
    val intput =
      runTestAssert[Stat](
        """|type Elem[X] = X match {
           |  case String => Char
           |  case Array[t] => t
           |}
           |""".stripMargin
      )(
        Defn.Type(
          Nil,
          Type.Name("Elem"),
          List(Type.Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, None), Nil, Nil)),
          Type.Match(
            Type.Name("X"),
            List(
              TypeCase(Type.Name("String"), Type.Name("Char")),
              TypeCase(Type.Apply(Type.Name("Array"), List(Type.Name("t"))), Type.Name("t"))
            )
          )
        )
      )
  }

  test("simple-indentation") {
    val intput =
      runTestAssert[Stat](
        """|type Elem[X] = 
           |  X match
           |    case String => 
           |      Char
           |    case Array[t] => t
           |""".stripMargin,
        assertLayout = Some(
          """|type Elem[X] = X match {
             |  case String => Char
             |  case Array[t] => t
             |}
             |""".stripMargin
        )
      )(
        Defn.Type(
          Nil,
          Type.Name("Elem"),
          List(Type.Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, None), Nil, Nil)),
          Type.Match(
            Type.Name("X"),
            List(
              TypeCase(Type.Name("String"), Type.Name("Char")),
              TypeCase(Type.Apply(Type.Name("Array"), List(Type.Name("t"))), Type.Name("t"))
            )
          )
        )
      )
  }

  test("tuple") {
    runTestAssert[Stat](
      """|type Head[X <: Tuple] = X match {
         |  case (x1, ?) => x1
         |}
         |""".stripMargin
    )(
      Defn.Type(
        Nil,
        Type.Name("Head"),
        List(
          Type
            .Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, Some(Type.Name("Tuple"))), Nil, Nil)
        ),
        Type.Match(
          Type.Name("X"),
          List(
            TypeCase(
              Type.Tuple(
                List(Type.Name("x1"), Type.Placeholder(Type.Bounds(None, None)))
              ),
              Type.Name("x1")
            )
          )
        )
      )
    )
  }

  test("recursive") {
    runTestAssert[Stat](
      """|type Len[X] <: Int = X match {
         |  case Unit => 0
         |  case x *: xs => S[Len[xs]]
         |}
         |""".stripMargin
    )(
      Defn.Type(
        Nil,
        Type.Name("Len"),
        List(Type.Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Type.Match(
          Type.Name("X"),
          List(
            TypeCase(Type.Name("Unit"), Lit.Int(0)),
            TypeCase(
              Type.ApplyInfix(Type.Name("x"), Type.Name("*:"), Type.Name("xs")),
              Type.Apply(Type.Name("S"), List(Type.Apply(Type.Name("Len"), List(Type.Name("xs")))))
            )
          )
        ),
        Type.Bounds(None, Some(Type.Name("Int")))
      )
    )
  }

  test("concat") {
    runTestAssert[Stat](
      """|type Concat[X <: Tuple, Y <: Tuple] <: Tuple = X match {
         |  case Unit => Y
         |  case x1 *: xs1 => x1 *: Concat[xs1, Y]
         |}
         |""".stripMargin
    )(
      Defn.Type(
        Nil,
        Type.Name("Concat"),
        List(
          Type
            .Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, Some(Type.Name("Tuple"))), Nil, Nil),
          Type
            .Param(Nil, Type.Name("Y"), Nil, Type.Bounds(None, Some(Type.Name("Tuple"))), Nil, Nil)
        ),
        Type.Match(
          Type.Name("X"),
          List(
            TypeCase(Type.Name("Unit"), Type.Name("Y")),
            TypeCase(
              Type.ApplyInfix(Type.Name("x1"), Type.Name("*:"), Type.Name("xs1")),
              Type.ApplyInfix(
                Type.Name("x1"),
                Type.Name("*:"),
                Type.Apply(Type.Name("Concat"), List(Type.Name("xs1"), Type.Name("Y")))
              )
            )
          )
        ),
        Type.Bounds(None, Some(Type.Name("Tuple")))
      )
    )
  }

  test("indent") {
    runTestAssert[Stat](
      """|type Elem[X] = X match
         |  case String => Char
         |  case Array[t] => t
         |
         |""".stripMargin,
      assertLayout = Some(
        """|type Elem[X] = X match {
           |  case String => Char
           |  case Array[t] => t
           |}
           |""".stripMargin
      )
    )(
      Defn.Type(
        Nil,
        Type.Name("Elem"),
        List(Type.Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Type.Match(
          Type.Name("X"),
          List(
            TypeCase(Type.Name("String"), Type.Name("Char")),
            TypeCase(Type.Apply(Type.Name("Array"), List(Type.Name("t"))), Type.Name("t"))
          )
        )
      )
    )
  }
}

package scala.meta.tests.parsers.dotty

import scala.meta._

class MatchTypeSuite extends BaseDottySuite {

  implicit def parseBlock(code: String, dialect: Dialect): Stat = blockStat(code)(dialect)

  test("simple") {
    val intput = runTestAssert[Stat](
      """|type Elem[X] = X match {
         |  case String => Char
         |  case Array[t] => t
         |}
         |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("Elem"),
      List(pparam("X")),
      Type.Match(
        pname("X"),
        List(
          TypeCase(pname("String"), pname("Char")),
          TypeCase(Type.Apply(pname("Array"), List(pname("t"))), pname("t"))
        )
      )
    ))
  }

  test("simple-indentation") {
    val intput = runTestAssert[Stat](
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
    )(Defn.Type(
      Nil,
      pname("Elem"),
      List(pparam("X")),
      Type.Match(
        pname("X"),
        List(
          TypeCase(pname("String"), pname("Char")),
          TypeCase(Type.Apply(pname("Array"), List(pname("t"))), pname("t"))
        )
      )
    ))
  }

  test("tuple") {
    runTestAssert[Stat](
      """|type Head[X <: Tuple] = X match {
         |  case (x1, ?) => x1
         |}
         |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("Head"),
      List(pparam("X", hiBound("Tuple"))),
      Type.Match(
        pname("X"),
        List(
          TypeCase(Type.Tuple(List(pname("x1"), Type.Wildcard(Type.Bounds(None, None)))), pname("x1"))
        )
      )
    ))
  }

  test("recursive") {
    runTestAssert[Stat](
      """|type Len[X] <: Int = X match {
         |  case Unit => 0
         |  case x *: xs => S[Len[xs]]
         |}
         |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("Len"),
      List(pparam("X")),
      Type.Match(
        pname("X"),
        List(
          TypeCase(pname("Unit"), int(0)),
          TypeCase(
            Type.ApplyInfix(pname("x"), pname("*:"), pname("xs")),
            Type.Apply(pname("S"), List(Type.Apply(pname("Len"), List(pname("xs")))))
          )
        )
      ),
      hiBound("Int")
    ))
  }

  test("concat") {
    runTestAssert[Stat](
      """|type Concat[X <: Tuple, Y <: Tuple] <: Tuple = X match {
         |  case Unit => Y
         |  case x1 *: xs1 => x1 *: Concat[xs1, Y]
         |}
         |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("Concat"),
      List(pparam("X", hiBound("Tuple")), pparam("Y", hiBound("Tuple"))),
      Type.Match(
        pname("X"),
        List(
          TypeCase(pname("Unit"), pname("Y")),
          TypeCase(
            Type.ApplyInfix(pname("x1"), pname("*:"), pname("xs1")),
            Type.ApplyInfix(
              pname("x1"),
              pname("*:"),
              Type.Apply(pname("Concat"), List(pname("xs1"), pname("Y")))
            )
          )
        )
      ),
      hiBound("Tuple")
    ))
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
    )(Defn.Type(
      Nil,
      pname("Elem"),
      List(pparam("X")),
      Type.Match(
        pname("X"),
        List(
          TypeCase(pname("String"), pname("Char")),
          TypeCase(Type.Apply(pname("Array"), List(pname("t"))), pname("t"))
        )
      )
    ))
  }
  test("double-newline") {
    runTestAssert[Stat](
      """|object match_types:
         |
         |  type Combine[Left, Right] = Left match
         |    case Unit => Right
         |
         |    case ? => Left
         |""".stripMargin,
      assertLayout = Some(
        """|object match_types {
           |  type Combine[Left, Right] = Left match {
           |    case Unit => Right
           |    case ? => Left
           |  }
           |}
           |""".stripMargin
      )
    )(Defn.Object(
      Nil,
      tname("match_types"),
      tpl(Defn.Type(
        Nil,
        pname("Combine"),
        List(pparam("Left"), pparam("Right")),
        Type.Match(
          pname("Left"),
          List(TypeCase(pname("Unit"), pname("Right")), TypeCase(pname("?"), pname("Left")))
        ),
        noBounds
      ))
    ))
  }

  test("wildcard") {
    implicit val dialect: Dialect = dialects.Scala3Future
    runTestAssert[Stat](
      """|object match_types:
         |  type Combine[L, R] = L match
         |    case Foo[_] => L
         |    case Bar[?] => L
         |    case _ => R
         |    case ? => L
         |""".stripMargin,
      assertLayout = Some(
        """|object match_types {
           |  type Combine[L, R] = L match {
           |    case Foo[_] => L
           |    case Bar[?] => L
           |    case _ => R
           |    case ? => L
           |  }
           |}
           |""".stripMargin
      )
    )(Defn.Object(
      Nil,
      tname("match_types"),
      tpl(Defn.Type(
        Nil,
        pname("Combine"),
        List(pparam("L"), pparam("R")),
        Type.Match(
          pname("L"),
          List(
            TypeCase(
              Type.AnonymousLambda(Type.Apply(pname("Foo"), List(Type.AnonymousParam(None)))),
              pname("L")
            ),
            TypeCase(
              Type.Apply(pname("Bar"), List(Type.Wildcard(Type.Bounds(None, None)))),
              pname("L")
            ),
            TypeCase(Type.PatWildcard(), pname("R")),
            TypeCase(pname("?"), pname("L"))
          )
        ),
        noBounds
      ))
    ))
  }
}

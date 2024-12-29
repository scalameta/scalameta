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
        List(TypeCase(pname("String"), pname("Char")), TypeCase(papply("Array", "t"), pname("t")))
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
        List(TypeCase(pname("String"), pname("Char")), TypeCase(papply("Array", "t"), pname("t")))
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
      Type.Match(pname("X"), List(TypeCase(Type.Tuple(List(pname("x1"), pwildcard)), pname("x1"))))
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
          TypeCase(pinfix("x", "*:", pname("xs")), papply("S", papply("Len", "xs")))
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
          TypeCase(pinfix("x1", "*:", pname("xs1")), pinfix("x1", "*:", papply("Concat", "xs1", "Y")))
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
        List(TypeCase(pname("String"), pname("Char")), TypeCase(papply("Array", "t"), pname("t")))
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
            TypeCase(Type.AnonymousLambda(papply("Foo", Type.AnonymousParam(None))), pname("L")),
            TypeCase(papply("Bar", pwildcard), pname("L")),
            TypeCase(Type.PatWildcard(), pname("R")),
            TypeCase(pname("?"), pname("L"))
          )
        ),
        noBounds
      ))
    ))
  }

  test("#4015") {
    val code = """|type T1 = A[[T] =>> T match
                  |    case _ => Int]
                  |""".stripMargin
    val layout = """|type T1 = A[[T] =>> T match {
                    |  case _ => Int
                    |}]
                    |""".stripMargin
    val tree = Defn.Type(
      Nil,
      pname("T1"),
      Nil,
      papply(
        "A",
        Type
          .Lambda(List(pparam("T")), Type.Match(pname("T"), List(TypeCase(Type.PatWildcard(), "Int"))))
      ),
      noBounds
    )

    runTestAssert[Stat](code, layout)(tree)
  }

}

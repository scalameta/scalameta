package scala.meta.tests.parsers.dotty

import scala.meta._

class NewFunctionsSuite extends BaseDottySuite {

  test("type-lambda") {
    // cannot carry +/- but can carry bounds >: , <:
    runTestAssert[Type]("[X, Y] =>> Map[Y, X]")(Type.Lambda(
      List(pparam("X"), pparam("Y")),
      Type.Apply(pname("Map"), List(pname("Y"), pname("X")))
    ))
    runTestAssert[Type]("[X >: L <: U] =>> R")(
      Type.Lambda(List(pparam(Nil, "X", bounds("L", "U"), vb = Nil, cb = Nil)), pname("R"))
    )
    runTestAssert[Type]("[X] =>> (X, X)")(
      Type.Lambda(List(pparam("X")), Type.Tuple(List(pname("X"), pname("X"))))
    )
    runTestAssert[Type]("[X] =>> [Y] =>> (X, Y)")(Type.Lambda(
      List(pparam("X")),
      Type.Lambda(List(pparam("Y")), Type.Tuple(List(pname("X"), pname("Y"))))
    ))
  }

  test("type-lambda-alias") {
    runTestAssert[Stat]("type Tuple = [X] =>> (X, X)")(Defn.Type(
      Nil,
      pname("Tuple"),
      Nil,
      Type.Lambda(List(pparam("X")), Type.Tuple(List(pname("X"), pname("X"))))
    ))
  }

  test("context-function-single") {
    runTestAssert[Stat]("def table(init: Table ?=> Unit): Unit")(Decl.Def(
      Nil,
      tname("table"),
      Nil,
      List(List(tparam("init", Type.ContextFunction(List(pname("Table")), pname("Unit"))))),
      pname("Unit")
    ))
  }

  test("context-function-multi") {
    runTestAssert[Stat]("def table(init: (T1, List[T2]) ?=> Unit): Unit")(Decl.Def(
      Nil,
      tname("table"),
      Nil,
      List(List(tparam(
        Nil,
        "init",
        Type.ContextFunction(
          List(pname("T1"), Type.Apply(pname("List"), List(pname("T2")))),
          pname("Unit")
        )
      ))),
      pname("Unit")
    ))
  }

  test("context-function-as-typedef") {
    runTestAssert[Stat]("type Executable[T] = ExecutionContext ?=> T")(Defn.Type(
      Nil,
      pname("Executable"),
      List(pparam("T")),
      Type.ContextFunction(List(pname("ExecutionContext")), pname("T"))
    ))

    val code = """|x match {
                  |  case t: (Context ?=> Symbol) @unchecked =>
                  |}""".stripMargin
    runTestAssert[Stat](code)(Term.Match(
      tname("x"),
      List(Case(
        Pat.Typed(
          Pat.Var(tname("t")),
          Type.Annotate(
            Type.ContextFunction(List(pname("Context")), pname("Symbol")),
            List(Mod.Annot(Init(pname("unchecked"), anon, emptyArgClause)))
          )
        ),
        None,
        Term.Block(Nil)
      ))
    ))
  }

  test("context-function-as-term") {
    runTestAssert[Stat]("def fx: String ?=> Int = s ?=> 3")(Defn.Def(
      Nil,
      tname("fx"),
      Nil,
      Nil,
      Some(Type.ContextFunction(List(pname("String")), pname("Int"))),
      Term.ContextFunction(List(tparam("s")), int(3))
    ))

    runTestAssert[Stat]("def fy: (String, Int) ?=> Int = (s, i) ?=> 3")(Defn.Def(
      Nil,
      tname("fy"),
      Nil,
      Nil,
      Some(Type.ContextFunction(List(pname("String"), pname("Int")), pname("Int"))),
      Term.ContextFunction(List(tparam("s"), tparam("i")), int(3))
    ))
  }

  test("polymorphic-func-term") {
    runTestAssert[Stat]("val t0 = [T] => (ts: List[T]) => ts.headOption")(Defn.Val(
      Nil,
      List(Pat.Var(tname("t0"))),
      None,
      Term.PolyFunction(
        List(pparam("T")),
        Term.Function(
          List(tparam("ts", Type.Apply(pname("List"), List(pname("T"))))),
          Term.Select(tname("ts"), tname("headOption"))
        )
      )
    ))
  }

  test("polymorphic-func-term-identity") {
    runTestAssert[Stat]("val pid = [T] => (t: T) => t")(Defn.Val(
      Nil,
      List(Pat.Var(tname("pid"))),
      None,
      Term.PolyFunction(List(pparam("T")), Term.Function(List(tparam("t", "T")), tname("t")))
    ))
  }

  test("polymorphic-func-term-complex") {
    runTestAssert[Stat]("val t1 = [F[_], G[_], T] => (ft: F[T], f: F[T] => G[T]) => f(ft)")(Defn.Val(
      Nil,
      List(Pat.Var(tname("t1"))),
      None,
      Term.PolyFunction(
        List(
          Type.Param(Nil, pname("F"), List(pparam("_")), noBounds, Nil, Nil),
          Type.Param(Nil, pname("G"), List(pparam("_")), noBounds, Nil, Nil),
          pparam("T")
        ),
        Term.Function(
          List(
            tparam("ft", Type.Apply(pname("F"), List(pname("T")))),
            tparam(
              Nil,
              "f",
              Type.Function(
                List(Type.Apply(pname("F"), List(pname("T")))),
                Type.Apply(pname("G"), List(pname("T")))
              )
            )
          ),
          Term.Apply(tname("f"), List(tname("ft")))
        )
      )
    ))
  }

  test("poly-function-type") {
    runTestAssert[Stat]("type F0 = [T] => List[T] => Option[T]")(Defn.Type(
      Nil,
      pname("F0"),
      Nil,
      Type.PolyFunction(
        List(pparam("T")),
        Type.Function(
          List(Type.Apply(pname("List"), List(pname("T")))),
          Type.Apply(pname("Option"), List(pname("T")))
        )
      )
    ))
  }
  test("poly-function-type") {
    runTestAssert[Stat](
      """|def foo = {
         |  f[[X] =>> String]
         |}""".stripMargin
    )(Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      Nil,
      None,
      Term
        .Block(List(Term.ApplyType(tname("f"), List(Type.Lambda(List(pparam("X")), pname("String"))))))
    ))
  }

  test("poly-function-type-method") {
    runTestAssert[Stat]("def m[T](f: [U] => U => U, t: T) = f(t)")(Defn.Def(
      Nil,
      tname("m"),
      List(pparam("T")),
      List(List(
        tparam(
          Nil,
          "f",
          Type.PolyFunction(List(pparam("U")), Type.Function(List(pname("U")), pname("U")))
        ),
        tparam("t", "T")
      )),
      None,
      Term.Apply(tname("f"), List(tname("t")))
    ))
  }

  test("poly-function-type-duo") {
    runTestAssert[Stat]("type F2 = [T, U] => (T, U) => Either[T, U]")(Defn.Type(
      Nil,
      pname("F2"),
      Nil,
      Type.PolyFunction(
        List(pparam("T"), pparam("U")),
        Type.Function(
          List(pname("T"), pname("U")),
          Type.Apply(pname("Either"), List(pname("T"), pname("U")))
        )
      )
    ))
  }

  test("poly-function-type-error") {
    runTestError[Stat](
      "type F2 = [T, U] => (T, U)",
      "polymorphic function types must have a value parameter"
    )
  }

  test("poly-function-type-complex") {
    runTestAssert[Stat]("type F1 = [F[_], G[_], T] => (F[T], F[T] => G[T]) => G[T]")(Defn.Type(
      Nil,
      pname("F1"),
      Nil,
      Type.PolyFunction(
        List(
          Type.Param(Nil, pname("F"), List(pparam("_")), noBounds, Nil, Nil),
          Type.Param(Nil, pname("G"), List(pparam("_")), noBounds, Nil, Nil),
          pparam("T")
        ),
        Type.Function(
          List(
            Type.Apply(pname("F"), List(pname("T"))),
            Type.Function(
              List(Type.Apply(pname("F"), List(pname("T")))),
              Type.Apply(pname("G"), List(pname("T")))
            )
          ),
          Type.Apply(pname("G"), List(pname("T")))
        )
      )
    ))
  }

  test("poly-context-function-type") {
    runTestAssert[Stat]("type F0 = [T] => List[T] ?=> Option[T]")(Defn.Type(
      Nil,
      pname("F0"),
      Nil,
      Type.PolyFunction(
        List(pparam("T")),
        Type.ContextFunction(
          List(Type.Apply(pname("List"), List(pname("T")))),
          Type.Apply(pname("Option"), List(pname("T")))
        )
      )
    ))
  }

  test("poly-context-function-complex") {
    runTestAssert[Stat](
      """|val t1 = [F[
         |    _
         |], T] =>
         |  (
         |      f: F[
         |        T
         |      ] => G[T]
         |) => f(ft)""".stripMargin,
      assertLayout = Some("val t1 = [F[_], T] => (f: F[T] => G[T]) => f(ft)")
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("t1"))),
      None,
      Term.PolyFunction(
        List(Type.Param(Nil, pname("F"), List(pparam("_")), noBounds, Nil, Nil), pparam("T")),
        Term.Function(
          List(tparam(
            Nil,
            "f",
            Type.Function(
              List(Type.Apply(pname("F"), List(pname("T")))),
              Type.Apply(pname("G"), List(pname("T")))
            )
          )),
          Term.Apply(tname("f"), List(tname("ft")))
        )
      )
    ))
  }

  test("poly-function-indentation") {
    runTestAssert[Stat](
      """|val thisIsAPolymorphicFunction =
         |  [
         |      PolymorphicFunctionTypeParam
         |  ] =>
         |    (polymorphicFunctionParam: List[
         |      T
         |]) => ts.headOption""".stripMargin,
      assertLayout =
        Some("val thisIsAPolymorphicFunction = [PolymorphicFunctionTypeParam] => (polymorphicFunctionParam: List[T]) => ts.headOption")
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("thisIsAPolymorphicFunction"))),
      None,
      Term.PolyFunction(
        List(pparam("PolymorphicFunctionTypeParam")),
        Term.Function(
          List(tparam("polymorphicFunctionParam", Type.Apply(pname("List"), List(pname("T"))))),
          Term.Select(tname("ts"), tname("headOption"))
        )
      )
    ))
  }

  test("dependent-type") {
    runTestAssert[Stat]("val extractor: (e: Entry) => e.Key = extractKey")(Defn.Val(
      Nil,
      List(Pat.Var(tname("extractor"))),
      Some(Type.Function(
        List(Type.TypedParam(pname("e"), pname("Entry"))),
        Type.Select(tname("e"), pname("Key"))
      )),
      tname("extractKey")
    ))
  }

  test("dependent-type-context") {
    runTestAssert[Stat]("val extractor: (e: Entry) ?=> e.Key = extractKey")(Defn.Val(
      Nil,
      List(Pat.Var(tname("extractor"))),
      Some(Type.ContextFunction(
        List(Type.TypedParam(pname("e"), pname("Entry"))),
        Type.Select(tname("e"), pname("Key"))
      )),
      tname("extractKey")
    ))
  }

  test("dependent-type-multi") {
    runTestAssert[Stat]("val extractor: (e: Entry, f: Other) => e.Key = extractKey")(Defn.Val(
      Nil,
      List(Pat.Var(tname("extractor"))),
      Some(Type.Function(
        List(Type.TypedParam(pname("e"), pname("Entry")), Type.TypedParam(pname("f"), pname("Other"))),
        Type.Select(tname("e"), pname("Key"))
      )),
      tname("extractKey")
    ))
  }

  test("dependent-type-term") {
    runTestAssert[Stat]("type T = (e: Entry) => e.Key")(Defn.Type(
      Nil,
      pname("T"),
      Nil,
      Type.Function(
        List(Type.TypedParam(pname("e"), pname("Entry"))),
        Type.Select(tname("e"), pname("Key"))
      ),
      noBounds
    ))
  }

  test("dependent-type-term-context") {
    runTestAssert[Stat]("type T = (e: Entry) ?=> e.Key")(Defn.Type(
      Nil,
      pname("T"),
      Nil,
      Type.ContextFunction(
        List(Type.TypedParam(pname("e"), pname("Entry"))),
        Type.Select(tname("e"), pname("Key"))
      ),
      noBounds
    ))
  }

  test("dependent-type-term-multi") {
    runTestAssert[Stat]("type T = (e: Entry, o: Other[? <: P]) => e.Key")(Defn.Type(
      Nil,
      pname("T"),
      Nil,
      Type.Function(
        List(
          Type.TypedParam(pname("e"), pname("Entry")),
          Type.TypedParam(pname("o"), Type.Apply(pname("Other"), List(Type.Wildcard(hiBound("P")))))
        ),
        Type.Select(tname("e"), pname("Key"))
      ),
      noBounds
    ))
  }

  test("dependent-type-arrow-after-nl") {
    runTestAssert[Stat](
      """|type T = 
         |  (e: Entry) 
         |  => e.Key
         |""".stripMargin,
      Some("type T = (e: Entry) => e.Key")
    )(Defn.Type(
      Nil,
      pname("T"),
      Nil,
      Type.Function(
        List(Type.TypedParam(pname("e"), pname("Entry"))),
        Type.Select(tname("e"), pname("Key"))
      ),
      noBounds
    ))
  }

  test("dependent-type-arrow-after-nl bad indent") {
    runTestError[Stat](
      """|type T = 
         |  (e: Entry) 
         |=> e.Key
         |""".stripMargin,
      """|<input>:3: error: illegal start of definition `=>`
         |=> e.Key
         |^""".stripMargin
    )
  }

  test("context-function-arrow-after-nl") {
    runTestAssert[Stat](
      """|type Executable[T] =
         |  ExecutionContext
         |  ?=> T
         |""".stripMargin,
      Some("type Executable[T] = ExecutionContext ?=> T")
    )(Defn.Type(
      Nil,
      pname("Executable"),
      pparam("T") :: Nil,
      Type.ContextFunction(pname("ExecutionContext") :: Nil, pname("T")),
      noBounds
    ))
  }

  test("context-function-arrow-after-nl with parens") {
    runTestAssert[Stat](
      """|type Executable[T] =
         |  (ExecutionContext)
         |  ?=> T
         |""".stripMargin,
      Some("type Executable[T] = ExecutionContext ?=> T")
    )(Defn.Type(
      Nil,
      pname("Executable"),
      pparam("T") :: Nil,
      Type.ContextFunction(pname("ExecutionContext") :: Nil, pname("T")),
      noBounds
    ))
  }

  test("context-function-arrow-after-nl bad indent") {
    runTestError[Stat](
      """|type Executable[T] =
         |  ExecutionContext
         |?=> T
         |""".stripMargin,
      """|error: illegal start of definition `?=>`
         |?=> T
         |^""".stripMargin
    )
  }

  test("lambda-function-arrow-after-nl") {
    runTestAssert[Stat](
      """|type Tuple =
         |  [X]
         |  =>> (X, X)
         |""".stripMargin,
      Some("type Tuple = [X] =>> (X, X)")
    )(Defn.Type(
      Nil,
      pname("Tuple"),
      Nil,
      Type.Lambda(pparam("X") :: Nil, Type.Tuple(List(pname("X"), pname("X")))),
      noBounds
    ))
  }

  test("lambda-function-arrow-after-nl no NL after =") {
    runTestAssert[Stat](
      """|type Tuple = [X]
         |  =>> (X, X)
         |""".stripMargin,
      Some("type Tuple = [X] =>> (X, X)")
    )(Defn.Type(
      Nil,
      pname("Tuple"),
      Nil,
      Type.Lambda(pparam("X") :: Nil, Type.Tuple(List(pname("X"), pname("X")))),
      noBounds
    ))
  }

  test("lambda-function-arrow-after-nl bad indent") {
    runTestError[Stat](
      """|type Tuple =
         |  [X]
         |=>> (X, X)
         |""".stripMargin,
      """|error: expected =>> or =>
         |  [X]
         |     ^""".stripMargin
    )
  }

  test("type-lambda-bounds") {
    runTestAssert[Stat]("type U <: [X] =>> Any")(Decl.Type(
      Nil,
      pname("U"),
      Nil,
      Type.Bounds(None, Some(Type.Lambda(List(pparam("X")), pname("Any"))))
    ))
  }

  test("type Macro[X] = (=> Quotes) ?=> Expr[X]") {
    runTestAssert[Stat]("type Macro[X] = (=> Quotes) ?=> Expr[X]")(Defn.Type(
      Nil,
      pname("Macro"),
      List(pparam("X")),
      Type.ContextFunction(
        List(Type.ByName(pname("Quotes"))),
        Type.Apply(pname("Expr"), List(pname("X")))
      ),
      noBounds
    ))
  }

  test("type-lmabda-bounds") {
    runTestAssert[Stat]("abstract class Repository[F[_]: [G[_]] =>> MonadCancel[G, Throwable]]")(
      Defn.Class(
        List(Mod.Abstract()),
        pname("Repository"),
        List(Type.Param(
          Nil,
          pname("F"),
          List(pparam("_")),
          noBounds,
          Nil,
          List(Type.Lambda(
            List(Type.Param(Nil, pname("G"), List(pparam("_")), noBounds, Nil, Nil)),
            Type.Apply(pname("MonadCancel"), List(pname("G"), pname("Throwable")))
          ))
        )),
        ctor,
        tplNoBody()
      )
    )
  }

  test("#3050 function without body") {
    runTestAssert[Stat](
      """|f{ (x1: A, x2: B => C) => }
         |""".stripMargin,
      """f { (x1: A, x2: B => C) =>
        |}
        |""".stripMargin
    )(Term.Apply(
      tname("f"),
      Term.Block(
        Term.Function(
          List(
            tparam("x1", "A"),
            tparam("x2", Type.Function(Type.FuncParamClause(List(pname("B"))), pname("C")))
          ),
          Term.Block(Nil)
        ) :: Nil
      ) :: Nil
    ))
  }

}

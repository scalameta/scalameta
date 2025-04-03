package scala.meta.tests.parsers.dotty

import scala.meta._

class NewFunctionsSuite extends BaseDottySuite {

  test("type-lambda") {
    // cannot carry +/- but can carry bounds >: , <:
    runTestAssert[Type]("[X, Y] =>> Map[Y, X]")(
      Type.Lambda(List(pparam("X"), pparam("Y")), papply("Map", "Y", "X"))
    )
    runTestAssert[Type]("[X >: L <: U] =>> R")(
      Type.Lambda(List(pparam("X", bounds("L", "U"))), pname("R"))
    )
    runTestAssert[Type]("[X] =>> (X, X)")(
      Type.Lambda(List(pparam("X")), Type.Tuple(List(pname("X"), pname("X"))))
    )
    runTestAssert[Type]("[X] =>> [Y] =>> (X, Y)")(Type.Lambda(
      List(pparam("X")),
      Type.Lambda(List(pparam("Y")), Type.Tuple(List(pname("X"), pname("Y"))))
    ))
  }

  test("type-lambda-alias")(runTestAssert[Stat]("type Tuple = [X] =>> (X, X)")(Defn.Type(
    Nil,
    pname("Tuple"),
    Nil,
    Type.Lambda(List(pparam("X")), Type.Tuple(List(pname("X"), pname("X"))))
  )))

  test("context-function-single")(
    runTestAssert[Stat]("def table(init: Table ?=> Unit): Unit")(Decl.Def(
      Nil,
      tname("table"),
      Nil,
      List(List(tparam("init", pctxfunc(pname("Table"))(pname("Unit"))))),
      pname("Unit")
    ))
  )

  test("context-function-multi")(
    runTestAssert[Stat]("def table(init: (T1, List[T2]) ?=> Unit): Unit")(Decl.Def(
      Nil,
      tname("table"),
      Nil,
      List(List(tparam(Nil, "init", pctxfunc(pname("T1"), papply("List", "T2"))(pname("Unit"))))),
      pname("Unit")
    ))
  )

  test("context-function-as-typedef") {
    runTestAssert[Stat]("type Executable[T] = ExecutionContext ?=> T")(Defn.Type(
      Nil,
      pname("Executable"),
      List(pparam("T")),
      pctxfunc(pname("ExecutionContext"))(pname("T"))
    ))

    val code =
      """|x match {
         |  case t: (Context ?=> Symbol) @unchecked =>
         |}""".stripMargin
    runTestAssert[Stat](code)(tmatch(
      tname("x"),
      Case(
        Pat.Typed(
          patvar("t"),
          Type.Annotate(pctxfunc(pname("Context"))(pname("Symbol")), List(Mod.Annot(init("unchecked"))))
        ),
        None,
        blk()
      )
    ))
  }

  test("context-function-as-term") {
    runTestAssert[Stat]("def fx: String ?=> Int = s ?=> 3")(Defn.Def(
      Nil,
      tname("fx"),
      Nil,
      Nil,
      Some(pctxfunc(pname("String"))(pname("Int"))),
      tctxfunc(tparam("s"))(int(3))
    ))

    runTestAssert[Stat]("def fy: (String, Int) ?=> Int = (s, i) ?=> 3")(Defn.Def(
      Nil,
      tname("fy"),
      Nil,
      Nil,
      Some(pctxfunc(pname("String"), pname("Int"))(pname("Int"))),
      tctxfunc(tparam("s"), tparam("i"))(int(3))
    ))
  }

  test("polymorphic-func-term")(
    runTestAssert[Stat]("val t0 = [T] => (ts: List[T]) => ts.headOption")(Defn.Val(
      Nil,
      List(patvar("t0")),
      None,
      tpolyfunc(pparam("T"))(tfunc(tparam("ts", papply("List", "T")))(tselect("ts", "headOption")))
    ))
  )

  test("polymorphic-func-term-identity")(runTestAssert[Stat]("val pid = [T] => (t: T) => t")(
    Defn
      .Val(Nil, List(patvar("pid")), None, tpolyfunc(pparam("T"))(tfunc(tparam("t", "T"))(tname("t"))))
  ))

  test("polymorphic-func-term-complex")(
    runTestAssert[Stat]("val t1 = [F[_], G[_], T] => (ft: F[T], f: F[T] => G[T]) => f(ft)")(Defn.Val(
      Nil,
      List(patvar("t1")),
      None,
      tpolyfunc(pparam(Nil, "F", List(pparam("_"))), pparam(Nil, "G", List(pparam("_"))), pparam("T"))(
        tfunc(
          tparam("ft", papply("F", "T")),
          tparam(Nil, "f", pfunc(papply("F", "T"))(papply("G", "T")))
        )(tapply(tname("f"), tname("ft")))
      )
    ))
  )

  test("poly-function-type")(runTestAssert[Stat]("type F0 = [T] => List[T] => Option[T]")(Defn.Type(
    Nil,
    pname("F0"),
    Nil,
    ppolyfunc(pparam("T"))(pfunc(papply("List", "T"))(papply("Option", "T")))
  )))
  test("poly-function-type")(
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
      blk(tapplytype(tname("f"), Type.Lambda(List(pparam("X")), pname("String"))))
    ))
  )

  test("poly-function-type-method")(
    runTestAssert[Stat]("def m[T](f: [U] => U => U, t: T) = f(t)")(Defn.Def(
      Nil,
      tname("m"),
      List(pparam("T")),
      List(List(
        tparam(Nil, "f", ppolyfunc(pparam("U"))(pfunc(pname("U"))(pname("U")))),
        tparam("t", "T")
      )),
      None,
      tapply(tname("f"), tname("t"))
    ))
  )

  test("poly-function-type-duo")(
    runTestAssert[Stat]("type F2 = [T, U] => (T, U) => Either[T, U]")(Defn.Type(
      Nil,
      pname("F2"),
      Nil,
      ppolyfunc(pparam("T"), pparam("U"))(pfunc(pname("T"), pname("U"))(papply("Either", "T", "U")))
    ))
  )

  test("poly-function-type-error")(runTestError[Stat](
    "type F2 = [T, U] => (T, U)",
    "polymorphic function types must have a value parameter"
  ))

  test("poly-function-type-complex")(
    runTestAssert[Stat]("type F1 = [F[_], G[_], T] => (F[T], F[T] => G[T]) => G[T]")(Defn.Type(
      Nil,
      pname("F1"),
      Nil,
      ppolyfunc(pparam(Nil, "F", List(pparam("_"))), pparam(Nil, "G", List(pparam("_"))), pparam("T"))(
        pfunc(papply("F", "T"), pfunc(papply("F", "T"))(papply("G", "T")))(papply("G", "T"))
      )
    ))
  )

  test("poly-context-function-type")(
    runTestAssert[Stat]("type F0 = [T] => List[T] ?=> Option[T]")(Defn.Type(
      Nil,
      pname("F0"),
      Nil,
      ppolyfunc(pparam("T"))(pctxfunc(papply("List", "T"))(papply("Option", "T")))
    ))
  )

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
      List(patvar("t1")),
      None,
      tpolyfunc(pparam(Nil, "F", List(pparam("_"))), pparam("T"))(
        tfunc(
          tparam(Nil, "f", pfunc(papply("F", "T"))(papply("G", "T")))
        )(tapply(tname("f"), tname("ft")))
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
      List(patvar("thisIsAPolymorphicFunction")),
      None,
      tpolyfunc(
        pparam("PolymorphicFunctionTypeParam")
      )(tfunc(tparam("polymorphicFunctionParam", papply("List", "T")))(tselect("ts", "headOption")))
    ))
  }

  test("dependent-type")(
    runTestAssert[Stat]("val extractor: (e: Entry) => e.Key = extractKey")(Defn.Val(
      Nil,
      List(patvar("extractor")),
      Some(pfunc(Type.TypedParam(pname("e"), pname("Entry")))(pselect("e", "Key"))),
      tname("extractKey")
    ))
  )

  test("dependent-type-context")(
    runTestAssert[Stat]("val extractor: (e: Entry) ?=> e.Key = extractKey")(Defn.Val(
      Nil,
      List(patvar("extractor")),
      Some(pctxfunc(Type.TypedParam(pname("e"), pname("Entry")))(pselect("e", "Key"))),
      tname("extractKey")
    ))
  )

  test("dependent-type-multi")(
    runTestAssert[Stat]("val extractor: (e: Entry, f: Other) => e.Key = extractKey")(Defn.Val(
      Nil,
      List(patvar("extractor")),
      Some(
        pfunc(Type.TypedParam(pname("e"), pname("Entry")), Type.TypedParam(pname("f"), pname("Other")))(
          pselect("e", "Key")
        )
      ),
      tname("extractKey")
    ))
  )

  test("dependent-type-term")(runTestAssert[Stat]("type T = (e: Entry) => e.Key")(Defn.Type(
    Nil,
    pname("T"),
    Nil,
    pfunc(Type.TypedParam(pname("e"), pname("Entry")))(pselect("e", "Key")),
    noBounds
  )))

  test("dependent-type-term-context")(runTestAssert[Stat]("type T = (e: Entry) ?=> e.Key")(Defn.Type(
    Nil,
    pname("T"),
    Nil,
    pctxfunc(Type.TypedParam(pname("e"), pname("Entry")))(pselect("e", "Key")),
    noBounds
  )))

  test("dependent-type-term-multi")(
    runTestAssert[Stat]("type T = (e: Entry, o: Other[? <: P]) => e.Key")(Defn.Type(
      Nil,
      pname("T"),
      Nil,
      pfunc(
        Type.TypedParam(pname("e"), pname("Entry")),
        Type.TypedParam(pname("o"), papply("Other", pwildcard(hiBound("P"))))
      )(pselect("e", "Key")),
      noBounds
    ))
  )

  test("dependent-type-arrow-after-nl")(
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
      pfunc(Type.TypedParam(pname("e"), pname("Entry")))(pselect("e", "Key")),
      noBounds
    ))
  )

  test("dependent-type-arrow-after-nl bad indent")(runTestError[Stat](
    """|type T = 
       |  (e: Entry) 
       |=> e.Key
       |""".stripMargin,
    """|<input>:3: error: illegal start of definition `=>`
       |=> e.Key
       |^""".stripMargin
  ))

  test("context-function-arrow-after-nl")(
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
      pctxfunc(pname("ExecutionContext"))(pname("T")),
      noBounds
    ))
  )

  test("context-function-arrow-after-nl with parens")(
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
      pctxfunc(pname("ExecutionContext"))(pname("T")),
      noBounds
    ))
  )

  test("context-function-arrow-after-nl bad indent")(runTestError[Stat](
    """|type Executable[T] =
       |  ExecutionContext
       |?=> T
       |""".stripMargin,
    """|error: illegal start of definition `?=>`
       |?=> T
       |^""".stripMargin
  ))

  test("lambda-function-arrow-after-nl")(
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
  )

  test("lambda-function-arrow-after-nl no NL after =")(
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
  )

  test("lambda-function-arrow-after-nl bad indent")(runTestError[Stat](
    """|type Tuple =
       |  [X]
       |=>> (X, X)
       |""".stripMargin,
    """|error: expected =>> or =>
       |  [X]
       |     ^""".stripMargin
  ))

  test("type-lambda-bounds")(runTestAssert[Stat]("type U <: [X] =>> Any")(
    Decl.Type(Nil, pname("U"), Nil, bounds(hi = Type.Lambda(List(pparam("X")), pname("Any"))))
  ))

  test("type Macro[X] = (=> Quotes) ?=> Expr[X]")(
    runTestAssert[Stat]("type Macro[X] = (=> Quotes) ?=> Expr[X]")(Defn.Type(
      Nil,
      pname("Macro"),
      List(pparam("X")),
      pctxfunc(Type.ByName(pname("Quotes")))(papply("Expr", "X")),
      noBounds
    ))
  )

  test("type-lmabda-bounds")(
    runTestAssert[Stat]("abstract class Repository[F[_]: [G[_]] =>> MonadCancel[G, Throwable]]")(
      Defn.Class(
        List(Mod.Abstract()),
        pname("Repository"),
        List(pparam(
          Nil,
          "F",
          List(pparam("_")),
          bounds(cb =
            List(Type.Lambda(
              List(pparam(Nil, "G", List(pparam("_")))),
              papply("MonadCancel", "G", "Throwable")
            ))
          )
        )),
        ctor,
        tplNoBody()
      )
    )
  )

  test("#3050 function without body")(
    runTestAssert[Stat](
      """|f{ (x1: A, x2: B => C) => }
         |""".stripMargin,
      """f { (x1: A, x2: B => C) =>
        |}
        |""".stripMargin
    )(tapply(
      tname("f"),
      blk(tfunc(tparam("x1", "A"), tparam("x2", pfunc(pname("B"))(pname("C"))))(blk()))
    ))
  )

  test("#3996 functions: precedence 1") {
    val code = "A => B => C => D"
    val layout = "A => B => C => D"
    val tree = pfunc("A")(pfunc("B")(pfunc("C")("D")))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 functions: precedence 2") {
    val code = "(A => B) => (C => D)"
    val layout = "(A => B) => C => D"
    val tree = pfunc(pfunc("A")("B"))(pfunc("C")("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 functions: precedence 3") {
    val code = "A => (B => C) => D"
    val layout = "A => (B => C) => D"
    val tree = pfunc("A")(pfunc(pfunc("B")("C"))("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 functions: precedence 4") {
    val code = "(A => (B => C)) => D"
    val layout = "(A => B => C) => D"
    val tree = pfunc(pfunc("A")(pfunc("B")("C")))("D")
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 functions: precedence 5") {
    val code = "A => ((B => C) => D)"
    val layout = "A => (B => C) => D"
    val tree = pfunc("A")(pfunc(pfunc("B")("C"))("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 context functions: precedence 1") {
    val code = "A ?=> B ?=> C ?=> D"
    val layout = "A ?=> B ?=> C ?=> D"
    val tree = pctxfunc("A")(pctxfunc("B")(pctxfunc("C")("D")))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 context functions: precedence 2") {
    val code = "(A ?=> B) ?=> (C ?=> D)"
    val layout = "(A ?=> B) ?=> C ?=> D"
    val tree = pctxfunc(pctxfunc("A")("B"))(pctxfunc("C")("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 context functions: precedence 3") {
    val code = "A ?=> (B ?=> C) ?=> D"
    val layout = "A ?=> (B ?=> C) ?=> D"
    val tree = pctxfunc("A")(pctxfunc(pctxfunc("B")("C"))("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 context functions: precedence 4") {
    val code = "(A ?=> (B ?=> C)) ?=> D"
    val layout = "(A ?=> B ?=> C) ?=> D"
    val tree = pctxfunc(pctxfunc("A")(pctxfunc("B")("C")))("D")
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 context functions: precedence 5") {
    val code = "A ?=> ((B ?=> C) ?=> D)"
    val layout = "A ?=> (B ?=> C) ?=> D"
    val tree = pctxfunc("A")(pctxfunc(pctxfunc("B")("C"))("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  // https://dotty.epfl.ch/docs/reference/experimental/purefuns.html
  // https://dotty.epfl.ch/docs/reference/experimental/cc.html#function-types-1

  test("#3996 pure functions: precedence 1") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "A -> B -> C -> D"
    val layout = "A -> B -> C -> D"
    val tree = purefunc("A")(purefunc("B")(purefunc("C")("D")))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 pure functions: precedence 2") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "(A -> B) -> (C -> D)"
    val layout = "(A -> B) -> C -> D"
    val tree = purefunc(purefunc("A")("B"))(purefunc("C")("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 pure functions: precedence 3") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "A -> (B -> C) -> D"
    val layout = "A -> (B -> C) -> D"
    val tree = purefunc("A")(purefunc(purefunc("B")("C"))("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 pure functions: precedence 4") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "(A -> (B -> C)) -> D"
    val layout = "(A -> B -> C) -> D"
    val tree = purefunc(purefunc("A")(purefunc("B")("C")))("D")
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 pure functions: precedence 5") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "A -> ((B -> C) -> D)"
    val layout = "A -> (B -> C) -> D"
    val tree = purefunc("A")(purefunc(purefunc("B")("C"))("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 pure context functions: precedence 1") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "A ?-> B ?-> C ?-> D"
    val layout = "A ?-> B ?-> C ?-> D"
    val tree = purectxfunc("A")(purectxfunc("B")(purectxfunc("C")("D")))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 pure context functions: precedence 2") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "(A ?-> B) ?-> (C ?-> D)"
    val layout = "(A ?-> B) ?-> C ?-> D"
    val tree = purectxfunc(purectxfunc("A")("B"))(purectxfunc("C")("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 pure context functions: precedence 3") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "A ?-> (B ?-> C) ?-> D"
    val layout = "A ?-> (B ?-> C) ?-> D"
    val tree = purectxfunc("A")(purectxfunc(purectxfunc("B")("C"))("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 pure context functions: precedence 4") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "(A ?-> (B ?-> C)) ?-> D"
    val layout = "(A ?-> B ?-> C) ?-> D"
    val tree = purectxfunc(purectxfunc("A")(purectxfunc("B")("C")))("D")
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 pure context functions: precedence 5") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "A ?-> ((B ?-> C) ?-> D)"
    val layout = "A ?-> (B ?-> C) ?-> D"
    val tree = purectxfunc("A")(purectxfunc(purectxfunc("B")("C"))("D"))
    runTestAssert[Type](code, layout)(tree)
  }

  test("#3996 pure functions: 1") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "val func: A -> B = foo"
    val layout = "val func: A -> B = foo"
    val tree = Defn.Val(Nil, List(patvar("func")), Some(purefunc("A")("B")), "foo")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 pure functions: 2") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def func(f: A -> B): Unit"
    val layout = "def func(f: A -> B): Unit"
    val tree = Decl.Def(Nil, "func", Nil, List(List(tparam("f", purefunc("A")("B")))), "Unit")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 pure functions: 3") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def map[T <: (A -> B)](f: T): A -> B = ???"
    val layout = "def map[T <: A -> B](f: T): A -> B = ???"
    val tree = Defn.Def(
      Nil,
      "map",
      List(pparam("T", hiBound(purefunc("A")("B")))),
      List(List(tparam("f", "T"))),
      Some(purefunc("A")("B")),
      "???"
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 pure context functions: 1") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "val func: A ?-> B = foo"
    val layout = "val func: A ?-> B = foo"
    val tree = Defn.Val(Nil, List(patvar("func")), Some(purectxfunc("A")("B")), "foo")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 pure context functions: 2") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def func(f: A ?-> B): Unit"
    val layout = "def func(f: A ?-> B): Unit"
    val tree = Decl.Def(Nil, "func", Nil, List(List(tparam("f", purectxfunc("A")("B")))), "Unit")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 pure context functions: 3") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def map[T <: (A ?-> B)](f: T): A ?-> B = ???"
    val layout = "def map[T <: A ?-> B](f: T): A ?-> B = ???"
    val tree = Defn.Def(
      Nil,
      "map",
      List(pparam("T", hiBound(purectxfunc("A")("B")))),
      List(List(tparam("f", "T"))),
      Some(purectxfunc("A")("B")),
      "???"
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 pure context functions: 4") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def map[T <: (A ?->{a, c} B)](f: T): A ?-> B = ???"
    val layout = "def map[T <: A ?->{a, c} B](f: T): A ?-> B = ???"
    val tree = Defn.Def(
      Nil,
      "map",
      List(pparam("T", hiBound(Type.Capturing(purectxfunc("A")("B"), List("a", "c"))))),
      List(List(tparam("f", "T"))),
      Some(purectxfunc("A")("B")),
      "???"
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 pure by-name: 1") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def func(f: -> B): Unit"
    val layout = "def func(f: -> B): Unit"
    val tree = Decl.Def(Nil, "func", Nil, List(List(tparam("f", Type.PureByName("B")))), "Unit")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 pure by-name: 2 with capturing") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def func(f: ->{a, b, c} B): Unit"
    val layout = "def func(f: ->{a, b, c} B): Unit"
    val tree = Decl.Def(
      Nil,
      "func",
      Nil,
      List(List(tparam("f", Type.Capturing(Type.PureByName("B"), List("a", "b", "c"))))),
      "Unit"
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4226 brackets") {
    val code =
      """|def foo =
         |  println()
         |  [T] => (a: T) => a
         |""".stripMargin
    val error =
      """|<input>:3: error: `;` expected but `=>` found
         |  [T] => (a: T) => a
         |      ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("#4226 parens") {
    val code =
      """|def foo =
         |  println()
         |  (a: T) => a
         |""".stripMargin
    val layout =
      """|def foo = {
         |  println()
         |  (a: T) => a
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, "foo", Nil, None, blk(tapply("println"), tfunc(tparam("a", "T"))("a")))
    runTestAssert[Stat](code, layout)(tree)
  }

}

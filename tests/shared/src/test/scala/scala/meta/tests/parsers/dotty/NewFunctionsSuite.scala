package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class NewFunctionsSuite extends BaseDottySuite {

  test("type-lambda") {
    // cannot carry +/- but can carry bounds >: , <:
    runTestAssert[Type]("[X, Y] =>> Map[Y, X]")(
      Type.Lambda(
        List(pparam("X"), pparam("Y")),
        Type.Apply(pname("Map"), List(pname("Y"), pname("X")))
      )
    )
    runTestAssert[Type]("[X >: L <: U] =>> R")(
      Type.Lambda(
        List(
          Type
            .Param(Nil, pname("X"), Nil, Type.Bounds(Some(pname("L")), Some(pname("U"))), Nil, Nil)
        ),
        pname("R")
      )
    )
    runTestAssert[Type]("[X] =>> (X, X)")(
      Type.Lambda(List(pparam("X")), Type.Tuple(List(pname("X"), pname("X"))))
    )
    runTestAssert[Type]("[X] =>> [Y] =>> (X, Y)")(
      Type.Lambda(
        List(pparam("X")),
        Type.Lambda(List(pparam("Y")), Type.Tuple(List(pname("X"), pname("Y"))))
      )
    )
  }

  test("type-lambda-alias") {
    runTestAssert[Stat]("type Tuple = [X] =>> (X, X)")(
      Defn.Type(
        Nil,
        Type.Name("Tuple"),
        Nil,
        Type.Lambda(
          List(Type.Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, None), Nil, Nil)),
          Type.Tuple(List(Type.Name("X"), Type.Name("X")))
        )
      )
    )
  }

  test("context-function-single") {
    runTestAssert[Stat]("def table(init: Table ?=> Unit): Unit")(
      Decl.Def(
        Nil,
        Term.Name("table"),
        Nil,
        List(
          List(
            Term.Param(
              Nil,
              Term.Name("init"),
              Some(Type.ContextFunction(List(Type.Name("Table")), Type.Name("Unit"))),
              None
            )
          )
        ),
        Type.Name("Unit")
      )
    )
  }

  test("context-function-multi") {
    runTestAssert[Stat]("def table(init: (T1, List[T2]) ?=> Unit): Unit")(
      Decl.Def(
        Nil,
        Term.Name("table"),
        Nil,
        List(
          List(
            Term.Param(
              Nil,
              Term.Name("init"),
              Some(
                Type.ContextFunction(
                  List(Type.Name("T1"), Type.Apply(Type.Name("List"), List(Type.Name("T2")))),
                  Type.Name("Unit")
                )
              ),
              None
            )
          )
        ),
        Type.Name("Unit")
      )
    )
  }

  test("context-function-as-typedef") {
    runTestAssert[Stat]("type Executable[T] = ExecutionContext ?=> T")(
      Defn.Type(
        Nil,
        Type.Name("Executable"),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Type.ContextFunction(List(Type.Name("ExecutionContext")), Type.Name("T"))
      )
    )

    val code = """|x match {
                  |  case t: (Context ?=> Symbol) @unchecked =>
                  |}""".stripMargin
    runTestAssert[Stat](code)(
      Term.Match(
        Term.Name("x"),
        List(
          Case(
            Pat.Typed(
              Pat.Var(Term.Name("t")),
              Type.Annotate(
                Type.ContextFunction(List(Type.Name("Context")), Type.Name("Symbol")),
                List(Mod.Annot(Init(Type.Name("unchecked"), Name(""), Nil)))
              )
            ),
            None,
            Term.Block(Nil)
          )
        )
      )
    )
  }

  test("context-function-as-term") {
    runTestAssert[Stat]("def fx: String ?=> Int = s ?=> 3")(
      Defn.Def(
        Nil,
        Term.Name("fx"),
        Nil,
        Nil,
        Some(Type.ContextFunction(List(Type.Name("String")), Type.Name("Int"))),
        Term.ContextFunction(List(Term.Param(Nil, Term.Name("s"), None, None)), Lit.Int(3))
      )
    )

    runTestAssert[Stat]("def fy: (String, Int) ?=> Int = (s, i) ?=> 3")(
      Defn.Def(
        Nil,
        Term.Name("fy"),
        Nil,
        Nil,
        Some(Type.ContextFunction(List(Type.Name("String"), Type.Name("Int")), Type.Name("Int"))),
        Term.ContextFunction(
          List(
            Term.Param(Nil, Term.Name("s"), None, None),
            Term.Param(Nil, Term.Name("i"), None, None)
          ),
          Lit.Int(3)
        )
      )
    )
  }

  test("polymorphic-func-term") {
    runTestAssert[Stat](
      "val t0 = [T] => (ts: List[T]) => ts.headOption"
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("t0"))),
        None,
        Term.PolyFunction(
          List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
          Term.Function(
            List(
              Term.Param(
                Nil,
                Term.Name("ts"),
                Some(Type.Apply(Type.Name("List"), List(Type.Name("T")))),
                None
              )
            ),
            Term.Select(Term.Name("ts"), Term.Name("headOption"))
          )
        )
      )
    )
  }

  test("polymorphic-func-term-identity") {
    runTestAssert[Stat](
      "val pid = [T] => (t: T) => t"
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("pid"))),
        None,
        Term.PolyFunction(
          List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
          Term.Function(
            List(Term.Param(Nil, Term.Name("t"), Some(Type.Name("T")), None)),
            Term.Name("t")
          )
        )
      )
    )
  }

  test("polymorphic-func-term-complex") {
    runTestAssert[Stat](
      "val t1 = [F[_], G[_], T] => (ft: F[T], f: F[T] => G[T]) => f(ft)"
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("t1"))),
        None,
        Term.PolyFunction(
          List(
            Type.Param(
              Nil,
              Type.Name("F"),
              List(Type.Param(Nil, phName, Nil, Type.Bounds(None, None), Nil, Nil)),
              Type.Bounds(None, None),
              Nil,
              Nil
            ),
            Type.Param(
              Nil,
              Type.Name("G"),
              List(Type.Param(Nil, phName, Nil, Type.Bounds(None, None), Nil, Nil)),
              Type.Bounds(None, None),
              Nil,
              Nil
            ),
            Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)
          ),
          Term.Function(
            List(
              Term.Param(
                Nil,
                Term.Name("ft"),
                Some(Type.Apply(Type.Name("F"), List(Type.Name("T")))),
                None
              ),
              Term.Param(
                Nil,
                Term.Name("f"),
                Some(
                  Type.Function(
                    List(Type.Apply(Type.Name("F"), List(Type.Name("T")))),
                    Type.Apply(Type.Name("G"), List(Type.Name("T")))
                  )
                ),
                None
              )
            ),
            Term.Apply(Term.Name("f"), List(Term.Name("ft")))
          )
        )
      )
    )
  }

  test("poly-function-type") {
    runTestAssert[Stat](
      "type F0 = [T] => List[T] => Option[T]"
    )(
      Defn.Type(
        Nil,
        Type.Name("F0"),
        Nil,
        Type.PolyFunction(
          List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
          Type.Function(
            List(Type.Apply(Type.Name("List"), List(Type.Name("T")))),
            Type.Apply(Type.Name("Option"), List(Type.Name("T")))
          )
        )
      )
    )
  }
  test("poly-function-type") {
    runTestAssert[Stat](
      """|def foo = {
         |  f[[X] =>> String]
         |}""".stripMargin
    )(
      Defn.Def(
        Nil,
        Term.Name("foo"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Term.ApplyType(
              Term.Name("f"),
              List(
                Type.Lambda(
                  List(Type.Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, None), Nil, Nil)),
                  Type.Name("String")
                )
              )
            )
          )
        )
      )
    )
  }

  test("poly-function-type-method") {
    runTestAssert[Stat](
      "def m[T](f: [U] => U => U, t: T) = f(t)"
    )(
      Defn.Def(
        Nil,
        Term.Name("m"),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(
          List(
            Term.Param(
              Nil,
              Term.Name("f"),
              Some(
                Type.PolyFunction(
                  List(Type.Param(Nil, Type.Name("U"), Nil, Type.Bounds(None, None), Nil, Nil)),
                  Type.Function(List(Type.Name("U")), Type.Name("U"))
                )
              ),
              None
            ),
            Term.Param(Nil, Term.Name("t"), Some(Type.Name("T")), None)
          )
        ),
        None,
        Term.Apply(Term.Name("f"), List(Term.Name("t")))
      )
    )
  }

  test("poly-function-type-duo") {
    runTestAssert[Stat](
      "type F2 = [T, U] => (T, U) => Either[T, U]"
    )(
      Defn.Type(
        Nil,
        Type.Name("F2"),
        Nil,
        Type.PolyFunction(
          List(
            Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil),
            Type.Param(Nil, Type.Name("U"), Nil, Type.Bounds(None, None), Nil, Nil)
          ),
          Type.Function(
            List(Type.Name("T"), Type.Name("U")),
            Type.Apply(Type.Name("Either"), List(Type.Name("T"), Type.Name("U")))
          )
        )
      )
    )
  }

  test("poly-function-type-error") {
    runTestError[Stat](
      "type F2 = [T, U] => (T, U)",
      "polymorphic function types must have a value parameter"
    )
  }

  test("poly-function-type-complex") {
    runTestAssert[Stat](
      "type F1 = [F[_], G[_], T] => (F[T], F[T] => G[T]) => G[T]"
    )(
      Defn.Type(
        Nil,
        Type.Name("F1"),
        Nil,
        Type.PolyFunction(
          List(
            Type.Param(
              Nil,
              Type.Name("F"),
              List(Type.Param(Nil, phName, Nil, Type.Bounds(None, None), Nil, Nil)),
              Type.Bounds(None, None),
              Nil,
              Nil
            ),
            Type.Param(
              Nil,
              Type.Name("G"),
              List(Type.Param(Nil, phName, Nil, Type.Bounds(None, None), Nil, Nil)),
              Type.Bounds(None, None),
              Nil,
              Nil
            ),
            Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)
          ),
          Type.Function(
            List(
              Type.Apply(Type.Name("F"), List(Type.Name("T"))),
              Type.Function(
                List(Type.Apply(Type.Name("F"), List(Type.Name("T")))),
                Type.Apply(Type.Name("G"), List(Type.Name("T")))
              )
            ),
            Type.Apply(Type.Name("G"), List(Type.Name("T")))
          )
        )
      )
    )
  }

  test("poly-context-function-type") {
    runTestAssert[Stat](
      "type F0 = [T] => List[T] ?=> Option[T]"
    )(
      Defn.Type(
        Nil,
        Type.Name("F0"),
        Nil,
        Type.PolyFunction(
          List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
          Type.ContextFunction(
            List(Type.Apply(Type.Name("List"), List(Type.Name("T")))),
            Type.Apply(Type.Name("Option"), List(Type.Name("T")))
          )
        )
      )
    )
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
      assertLayout = Some(
        "val t1 = [F[_], T] => (f: F[T] => G[T]) => f(ft)"
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("t1"))),
        None,
        Term.PolyFunction(
          List(
            Type.Param(
              Nil,
              Type.Name("F"),
              List(Type.Param(Nil, phName, Nil, Type.Bounds(None, None), Nil, Nil)),
              Type.Bounds(None, None),
              Nil,
              Nil
            ),
            Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)
          ),
          Term.Function(
            List(
              Term.Param(
                Nil,
                Term.Name("f"),
                Some(
                  Type.Function(
                    List(Type.Apply(Type.Name("F"), List(Type.Name("T")))),
                    Type.Apply(Type.Name("G"), List(Type.Name("T")))
                  )
                ),
                None
              )
            ),
            Term.Apply(Term.Name("f"), List(Term.Name("ft")))
          )
        )
      )
    )
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
      assertLayout = Some(
        "val thisIsAPolymorphicFunction = [PolymorphicFunctionTypeParam] => (polymorphicFunctionParam: List[T]) => ts.headOption"
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("thisIsAPolymorphicFunction"))),
        None,
        Term.PolyFunction(
          List(
            Type.Param(
              Nil,
              Type.Name("PolymorphicFunctionTypeParam"),
              Nil,
              Type.Bounds(None, None),
              Nil,
              Nil
            )
          ),
          Term.Function(
            List(
              Term.Param(
                Nil,
                Term.Name("polymorphicFunctionParam"),
                Some(Type.Apply(Type.Name("List"), List(Type.Name("T")))),
                None
              )
            ),
            Term.Select(Term.Name("ts"), Term.Name("headOption"))
          )
        )
      )
    )
  }

  test("dependent-type") {
    runTestAssert[Stat](
      "val extractor: (e: Entry) => e.Key = extractKey"
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("extractor"))),
        Some(
          Type.Function(
            List(Type.TypedParam(Type.Name("e"), Type.Name("Entry"))),
            Type.Select(Term.Name("e"), Type.Name("Key"))
          )
        ),
        Term.Name("extractKey")
      )
    )
  }

  test("dependent-type-context") {
    runTestAssert[Stat](
      "val extractor: (e: Entry) ?=> e.Key = extractKey"
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("extractor"))),
        Some(
          Type.ContextFunction(
            List(Type.TypedParam(Type.Name("e"), Type.Name("Entry"))),
            Type.Select(Term.Name("e"), Type.Name("Key"))
          )
        ),
        Term.Name("extractKey")
      )
    )
  }

  test("dependent-type-multi") {
    runTestAssert[Stat](
      "val extractor: (e: Entry, f: Other) => e.Key = extractKey"
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("extractor"))),
        Some(
          Type.Function(
            List(
              Type.TypedParam(Type.Name("e"), Type.Name("Entry")),
              Type.TypedParam(Type.Name("f"), Type.Name("Other"))
            ),
            Type.Select(Term.Name("e"), Type.Name("Key"))
          )
        ),
        Term.Name("extractKey")
      )
    )
  }

  test("dependent-type-term") {
    runTestAssert[Stat](
      "type T = (e: Entry) => e.Key"
    )(
      Defn.Type(
        Nil,
        Type.Name("T"),
        Nil,
        Type.Function(
          List(Type.TypedParam(Type.Name("e"), Type.Name("Entry"))),
          Type.Select(Term.Name("e"), Type.Name("Key"))
        ),
        Type.Bounds(None, None)
      )
    )
  }

  test("dependent-type-term-context") {
    runTestAssert[Stat](
      "type T = (e: Entry) ?=> e.Key"
    )(
      Defn.Type(
        Nil,
        Type.Name("T"),
        Nil,
        Type.ContextFunction(
          List(Type.TypedParam(Type.Name("e"), Type.Name("Entry"))),
          Type.Select(Term.Name("e"), Type.Name("Key"))
        ),
        Type.Bounds(None, None)
      )
    )
  }

  test("dependent-type-term-multi") {
    runTestAssert[Stat](
      "type T = (e: Entry, o: Other[? <: P]) => e.Key"
    )(
      Defn.Type(
        Nil,
        Type.Name("T"),
        Nil,
        Type.Function(
          List(
            Type.TypedParam(Type.Name("e"), Type.Name("Entry")),
            Type.TypedParam(
              Type.Name("o"),
              Type.Apply(
                Type.Name("Other"),
                List(Type.Wildcard(Type.Bounds(None, Some(Type.Name("P")))))
              )
            )
          ),
          Type.Select(Term.Name("e"), Type.Name("Key"))
        ),
        Type.Bounds(None, None)
      )
    )
  }

  test("dependent-type-arrow-after-nl") {
    runTestAssert[Stat](
      """|type T = 
         |  (e: Entry) 
         |  => e.Key
         |""".stripMargin,
      Some("type T = (e: Entry) => e.Key")
    )(
      Defn.Type(
        Nil,
        pname("T"),
        Nil,
        Type.Function(
          List(Type.TypedParam(pname("e"), pname("Entry"))),
          Type.Select(tname("e"), pname("Key"))
        ),
        Type.Bounds(None, None)
      )
    )
  }

  test("dependent-type-arrow-after-nl bad indent") {
    runTestError[Stat](
      """|type T = 
         |  (e: Entry) 
         |=> e.Key
         |""".stripMargin,
      """|error: ; expected but => found
         |=> e.Key
         |^""".stripMargin
    )
  }

  test("context-function-arrow-after-nl") {
    runTestError[Stat](
      """|type Executable[T] =
         |  ExecutionContext
         |  ?=> T
         |""".stripMargin,
      """|error: outdent expected but ?=> found
         |  ?=> T
         |  ^""".stripMargin
    )
  }

  test("context-function-arrow-after-nl with parens") {
    runTestError[Stat](
      """|type Executable[T] =
         |  (ExecutionContext)
         |  ?=> T
         |""".stripMargin,
      """|error: outdent expected but ?=> found
         |  ?=> T
         |  ^""".stripMargin
    )
  }

  test("context-function-arrow-after-nl bad indent") {
    runTestError[Stat](
      """|type Executable[T] =
         |  ExecutionContext
         |?=> T
         |""".stripMargin,
      """|error: illegal start of definition ?=>
         |?=> T
         |^""".stripMargin
    )
  }

  test("lambda-function-arrow-after-nl") {
    runTestError[Stat](
      """|type Tuple =
         |  [X]
         |  =>> (X, X)
         |""".stripMargin,
      """|error: expected =>> or =>
         |  [X]
         |     ^""".stripMargin
    )
  }

  test("lambda-function-arrow-after-nl no NL after =") {
    runTestError[Stat](
      """|type Tuple = [X]
         |  =>> (X, X)
         |""".stripMargin,
      """|error: expected =>> or =>
         |type Tuple = [X]
         |                ^""".stripMargin
    )
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
    runTestAssert[Stat](
      "type U <: [X] =>> Any"
    )(
      Decl.Type(
        Nil,
        Type.Name("U"),
        Nil,
        Type.Bounds(
          None,
          Some(
            Type.Lambda(
              List(Type.Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, None), Nil, Nil)),
              Type.Name("Any")
            )
          )
        )
      )
    )
  }

  test("type Macro[X] = (=> Quotes) ?=> Expr[X]") {
    runTestAssert[Stat](
      "type Macro[X] = (=> Quotes) ?=> Expr[X]"
    )(
      Defn.Type(
        Nil,
        Type.Name("Macro"),
        List(Type.Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Type.ContextFunction(
          List(Type.ByName(Type.Name("Quotes"))),
          Type.Apply(Type.Name("Expr"), List(Type.Name("X")))
        ),
        Type.Bounds(None, None)
      )
    )
  }

  test("type-lmabda-bounds") {
    runTestAssert[Stat](
      "abstract class Repository[F[_]: [G[_]] =>> MonadCancel[G, Throwable]]"
    )(
      Defn.Class(
        List(Mod.Abstract()),
        Type.Name("Repository"),
        List(
          Type.Param(
            Nil,
            Type.Name("F"),
            List(Type.Param(Nil, phName, Nil, Type.Bounds(None, None), Nil, Nil)),
            Type.Bounds(None, None),
            Nil,
            List(
              Type.Lambda(
                List(
                  Type.Param(
                    Nil,
                    Type.Name("G"),
                    List(Type.Param(Nil, phName, Nil, Type.Bounds(None, None), Nil, Nil)),
                    Type.Bounds(None, None),
                    Nil,
                    Nil
                  )
                ),
                Type.Apply(Type.Name("MonadCancel"), List(Type.Name("G"), Type.Name("Throwable")))
              )
            )
          )
        ),
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
      )
    )
  }
}

package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class NewFunctionsSuite extends BaseDottySuite {

  val parseTempl: String => Stat = code => templStat(code)(dialects.Dotty)
  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)
  implicit val parseType: String => Type = code => tpe(code)(dialects.Dotty)

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
    )(parseTempl)
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
    )(parseTempl)
  }

  test("context-function-as-typedef") {
    runTestAssert[Stat]("type Executable[T] = ExecutionContext ?=> T")(
      Defn.Type(
        Nil,
        Type.Name("Executable"),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Type.ContextFunction(List(Type.Name("ExecutionContext")), Type.Name("T"))
      )
    )(parseTempl)

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
              List(Type.Param(Nil, Name(""), Nil, Type.Bounds(None, None), Nil, Nil)),
              Type.Bounds(None, None),
              Nil,
              Nil
            ),
            Type.Param(
              Nil,
              Type.Name("G"),
              List(Type.Param(Nil, Name(""), Nil, Type.Bounds(None, None), Nil, Nil)),
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
              List(Type.Param(Nil, Name(""), Nil, Type.Bounds(None, None), Nil, Nil)),
              Type.Bounds(None, None),
              Nil,
              Nil
            ),
            Type.Param(
              Nil,
              Type.Name("G"),
              List(Type.Param(Nil, Name(""), Nil, Type.Bounds(None, None), Nil, Nil)),
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
}

package scala.meta.tests
package parsers

import scala.meta._, Term.{Name => TermName, _}, Type.{Name => TypeName},
Name.{Anonymous, Indeterminate}
import scala.meta.dialects.Scala211

class TermSuite extends ParseSuite {
  test("x") {
    val TermName("x") = term("x")
  }

  test("`x`") {
    val name @ TermName("x") = term("`x`")
  }

  test("a.b.c") {
    val outer @ Select(inner @ Select(TermName("a"), TermName("b")), TermName("c")) = term("a.b.c")
  }

  test("a.b c") {
    val outer @ Select(inner @ Select(TermName("a"), TermName("b")), TermName("c")) = term("a.b c")
  }

  test("foo.this") {
    val This(Indeterminate("foo")) = term("foo.this")
  }

  test("this") {
    val This(Anonymous()) = term("this")
  }

  test("a.super[b].c") {
    val Select(Super(Indeterminate("a"), Indeterminate("b")), TermName("c")) = term("a.super[b].c")
  }

  test("super[b].c") {
    val Select(Super(Anonymous(), Indeterminate("b")), TermName("c")) = term("super[b].c")
  }

  test("a.super.c") {
    val Select(Super(Indeterminate("a"), Anonymous()), TermName("c")) = term("a.super.c")
  }

  test("super.c") {
    val Select(Super(Anonymous(), Anonymous()), TermName("c")) = term("super.c")
  }

  test("s\"a $b c\"") {
    val Interpolate(TermName("s"), Lit("a ") :: Lit(" c") :: Nil, TermName("b") :: Nil) =
      term("s\"a $b c\"")
  }

  test("f(0)") {
    val Apply(TermName("f"), Lit(0) :: Nil) = term("f(0)")
  }

  test("f(x = 0)") {
    val Apply(TermName("f"), Assign(TermName("x"), Lit(0)) :: Nil) = term("f(x = 0)")
  }

  test("f(x: _*)") {
    val Apply(TermName("f"), Repeated(TermName("x")) :: Nil) = term("f(x: _*)")
  }

  test("f((x: _*))") {
    val Apply(TermName("f"), Repeated(TermName("x")) :: Nil) = term("f((x: _*))")
  }

  test("f(x = xs: _*)") {
    val Term.Apply(Term.Name("f"), List(Assign(Term.Name("x"), Repeated(Term.Name("xs"))))) =
      term("f(x = xs: _*)")
  }

  test("f(x = (xs: _*))") {
    val Term.Apply(Term.Name("f"), List(Assign(Term.Name("x"), Repeated(Term.Name("xs"))))) =
      term("f(x = (xs: _*))")
  }

  test("a + ()") {
    val ApplyInfix(TermName("a"), TermName("+"), Nil, Nil) = term("a + ()")
  }

  test("a + b") {
    val ApplyInfix(TermName("a"), TermName("+"), Nil, TermName("b") :: Nil) = term("a + b")
  }

  test("a + b + c") {
    val ApplyInfix(
      ApplyInfix(TermName("a"), TermName("+"), Nil, TermName("b") :: Nil),
      TermName("+"),
      Nil,
      TermName("c") :: Nil
    ) = term("a + b + c")
  }

  test("a :: b") {
    val ApplyInfix(TermName("a"), TermName("::"), Nil, TermName("b") :: Nil) = term("a :: b")
  }

  test("a :: b :: c") {
    val ApplyInfix(
      TermName("a"),
      TermName("::"),
      Nil,
      ApplyInfix(TermName("b"), TermName("::"), Nil, TermName("c") :: Nil) :: Nil
    ) = term("a :: b :: c")
  }

  test("!a") {
    val ApplyUnary(TermName("!"), TermName("a")) = term("!a")
  }

  test("!(a: _*)") {
    val ApplyUnary(TermName("!"), Repeated(TermName("a"))) = term("!(a: _*)")
  }

  test("a = true") {
    val Assign(TermName("a"), Lit(true)) = term("a = true")
  }

  test("a(0) = true") {
    val Assign(Apply(TermName("a"), (Lit(0) :: Nil)), Lit(true)) = term("a(0) = true")
  }

  test("return") {
    val ret @ Return(Lit(())) = term("return")
  }

  test("return 1") {
    val ret @ Return(Lit(1)) = term("return 1")
  }

  test("throw 1") {
    val Throw(Lit(1)) = term("throw 1")
  }

  test("1: Int") {
    val Ascribe(Lit(1), TypeName("Int")) = term("1: Int")
  }

  test("1: @foo") {
    val Annotate(Lit(1), Mod.Annot(Init(Type.Name("foo"), Name.Anonymous(), Nil)) :: Nil) =
      term("1: @foo")
  }

  test("(true, false)") {
    val Tuple(Lit(true) :: Lit(false) :: Nil) = term("(true, false)")
  }

  test("{ true; false }") {
    val Block(Lit(true) :: Lit(false) :: Nil) = term("{ true; false }")
  }

  test("{ true }") {
    val Block(Lit(true) :: Nil) = term("{ true }")
  }

  test("if (true) true else false") {
    val iff @ If(Lit(true), Lit(true), Lit(false)) = term("if (true) true else false")
  }

  test("if (true) true; else false") {
    val iff @ If(Lit(true), Lit(true), Lit(false)) = term("if (true) true; else false")
  }

  test("if (true) true") {
    val iff @ If(Lit(true), Lit(true), Lit(())) = term("if (true) true")
  }

  test("if (true && '' match...") {
    val file =
      """|
         |if (
         |  true && ("" match {
         |    case other ⇒ true
         |  })
         |  && true
         |) ""
         |""".stripMargin

    val Term.If(
      Term.ApplyInfix(
        Term.ApplyInfix(
          Lit.Boolean(true),
          Term.Name("&&"),
          Nil,
          List(
            Term.Match(
              Lit.String(""),
              List(Case(Pat.Var(Term.Name("other")), None, Lit.Boolean(true)))
            )
          )
        ),
        Term.Name("&&"),
        Nil,
        List(Lit.Boolean(true))
      ),
      Lit.String(""),
      Lit.Unit()
    ) = term(file)
  }

  test("() => x") {
    val Term.Function(Nil, Term.Name("x")) = term("() => x")
    val Term.Function(Nil, Term.Name("x")) = blockStat("() => x")
    val Term.Function(Nil, Term.Name("x")) = templStat("() => x")
  }

  test("(()) => x") {
    val Term.Function(Nil, Term.Name("x")) = term("(()) => x")
    val Term.Function(Nil, Term.Name("x")) = blockStat("(()) => x")
    val Term.Function(Nil, Term.Name("x")) = templStat("(()) => x")
  }

  test("x => x") {
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x")) =
      term("x => x")
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x")) =
      blockStat("x => x")
    intercept[ParseException] { templStat("x => x") }
  }

  test("(x) => x") {
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x")) =
      term("(x) => x")
    val Term.Function(List(Term.Param(Nil, Term.Name("x"), None, None)), Term.Name("x")) =
      blockStat("(x) => x")
    intercept[ParseException] { templStat("(x) => x") }
  }

  test("_ => x") {
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Name("x")) =
      term("_ => x")
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Name("x")) =
      blockStat("_ => x")
    intercept[ParseException] { templStat("_ => x") }
  }

  test("(_) => x") {
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Name("x")) =
      term("(_) => x")
    val Term.Function(List(Term.Param(Nil, Name.Anonymous(), None, None)), Term.Name("x")) =
      blockStat("(_) => x")
    intercept[ParseException] { templStat("(_) => x") }
  }

  test("x: Int => x") {
    // LAWL: this is how scalac's parser works
    val Term.Ascribe(Term.Name("x"), Type.Function(List(Type.Name("Int")), Type.Name("x"))) =
      term("x: Int => x")
    val Term.Function(
      List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = blockStat("x: Int => x")
    intercept[ParseException] { templStat("x: Int => x") }
  }

  test("(x: Int) => x") {
    val Term.Function(
      List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = term("(x: Int) => x")
    val Term.Function(
      List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = blockStat("(x: Int) => x")
    val Term.Function(
      List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = templStat("(x: Int) => x")
  }

  test("_: Int => x") {
    val Term.Ascribe(Term.Placeholder(), Type.Function(List(Type.Name("Int")), Type.Name("x"))) =
      term("_: Int => x")
    val Term.Function(
      List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = blockStat("_: Int => x")
    intercept[ParseException] { templStat("_: Int => x") }
  }

  test("(_: Int) => x") {
    val Term.Function(
      List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = term("(_: Int) => x")
    val Term.Function(
      List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = blockStat("(_: Int) => x")
    val Term.Function(
      List(Term.Param(Nil, Name.Anonymous(), Some(Type.Name("Int")), None)),
      Term.Name("x")
    ) = templStat("(_: Int) => x")
  }

  test("x: Int, y: Int => x") {
    intercept[ParseException] { term("x: Int, y: Int => x") }
    intercept[ParseException] { blockStat("x: Int, y: Int => x") }
    intercept[ParseException] { templStat("x: Int, y: Int => x") }
  }

  test("(x: Int, y: Int) => x") {
    val Term.Function(
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)
      ),
      Term.Name("x")
    ) = term("(x: Int, y: Int) => x")
    val Term.Function(
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)
      ),
      Term.Name("x")
    ) = blockStat("(x: Int, y: Int) => x")
    val Term.Function(
      List(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None),
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)
      ),
      Term.Name("x")
    ) = templStat("(x: Int, y: Int) => x")
  }

  test("{ implicit x => () }") {
    val Block(
      Function(
        Term.Param(Mod.Implicit() :: Nil, TermName("x"), None, None) :: Nil,
        Lit(())
      ) :: Nil
    ) = term("{ implicit x => () }")
  }

  test("1 match { case 1 => true }") {
    val Match(Lit(1), Case(Lit(1), None, Lit(true)) :: Nil) =
      term("1 match { case 1 => true }")
  }

  test("1 match { case 1 => }") {
    val Match(Lit(1), Case(Lit(1), None, Term.Block(Nil)) :: Nil) =
      term("1 match { case 1 => }")
  }

  test("1 match { case 1 if true => }") {
    val Match(Lit(1), Case(Lit(1), Some(Lit(true)), Term.Block(Nil)) :: Nil) =
      term("1 match { case 1 if true => }")
  }

  test("1 match { case case 1 if true => }") {
    val intercepted = intercept[ParseException] {
      term("1 match { case case 1 if true => }")
    }
    assertNoDiff(intercepted.shortMessage, "Unexpected `case`")
  }

  test("try 1") {
    val Try(Lit(1), Nil, None) = term("try 1")
  }

  test("try 1 catch 1") {
    val TryWithHandler(Lit(1), Lit(1), None) = term("try 1 catch 1")
  }

  test("try (2)") {
    val Try(Lit(2), Nil, None) = term("try (2)")
  }

  test("try 1 catch { case _ => }") {
    val Try(Lit(1), Case(Pat.Wildcard(), None, Term.Block(Nil)) :: Nil, None) =
      term("try 1 catch { case _ => }")
  }

  test("try 1 finally 1") {
    val Try(Lit(1), Nil, Some(Lit(1))) = term("try 1 finally 1")
  }

  test("{ case 1 => () }") {
    val PartialFunction(Case(Lit(1), None, Lit(())) :: Nil) =
      term("{ case 1 => () }")
  }

  test("while (true) false") {
    val While(Lit(true), Lit(false)) = term("while (true) false")
  }

  test("do false while(true)") {
    val Do(Lit(false), Lit(true)) = term("do false while(true)")
  }

  test("for (a <- b; if c; x = a) x") {
    val For(
      List(
        Enumerator.Generator(Pat.Var(TermName("a")), TermName("b")),
        Enumerator.Guard(TermName("c")),
        Enumerator.Val(Pat.Var(TermName("x")), TermName("a"))
      ),
      TermName("x")
    ) = term("for (a <- b; if c; x = a) x")

  }
  test("for (a <- b; if c; x = a) yield x") {
    val ForYield(
      List(
        Enumerator.Generator(Pat.Var(TermName("a")), TermName("b")),
        Enumerator.Guard(TermName("c")),
        Enumerator.Val(Pat.Var(TermName("x")), TermName("a"))
      ),
      TermName("x")
    ) = term("for (a <- b; if c; x = a) yield x")
  }

  test("f(_)") {
    val Apply(TermName("f"), List(Placeholder())) = term("f(_)")
  }

  test("_ + 1") {
    val ApplyInfix(Placeholder(), TermName("+"), Nil, Lit(1) :: Nil) = term("_ + 1")
  }

  test("1 + _") {
    val ApplyInfix(Lit(1), TermName("+"), Nil, Placeholder() :: Nil) = term("1 + _")
  }

  test("f _") {
    val Eta(TermName("f")) = term("f _")
  }

  test("new {}") {
    val NewAnonymous(Template(Nil, Nil, EmptySelf(), Nil)) = term("new {}")
  }

  test("new { x }") {
    val NewAnonymous(Template(Nil, Nil, EmptySelf(), List(Term.Name("x")))) = term("new { x }")
  }

  test("new A") {
    val New(Init(Type.Name("A"), Name.Anonymous(), Nil)) = term("new A")
  }

  test("new A(xs: _*)") {
    val New(Init(Type.Name("A"), Name.Anonymous(), List(List(Term.Repeated(Term.Name("xs")))))) =
      term("new A(xs: _*)")
  }

  test("new A {}") {
    val NewAnonymous(
      Template(Nil, Init(Type.Name("A"), Name.Anonymous(), Nil) :: Nil, EmptySelf(), Nil)
    ) = term("new A {}")
  }

  test("new A with B") {
    val NewAnonymous(
      Template(
        Nil,
        Init(Type.Name("A"), Name.Anonymous(), Nil) ::
          Init(Type.Name("B"), Name.Anonymous(), Nil) :: Nil,
        EmptySelf(),
        Nil
      )
    ) =
      term("new A with B")
  }

  test("new { val x: Int = 1 } with A") {
    val NewAnonymous(
      Template(
        Defn.Val(Nil, List(Pat.Var(TermName("x"))), Some(TypeName("Int")), Lit(1)) :: Nil,
        Init(Type.Name("A"), Name.Anonymous(), Nil) :: Nil,
        EmptySelf(),
        Nil
      )
    ) =
      term("new { val x: Int = 1 } with A")
  }

  test("new { self: T => }") {
    val NewAnonymous(Template(Nil, Nil, Self(TermName("self"), Some(TypeName("T"))), Nil)) =
      term("new { self: T => }")
  }

  test("a + (b = c)") {
    val ApplyInfix(TermName("a"), TermName("+"), Nil, Assign(TermName("b"), TermName("c")) :: Nil) =
      term("a + (b = c)")
  }

  test("(a = b) + c") {
    val ApplyInfix(Assign(TermName("a"), TermName("b")), TermName("+"), Nil, TermName("c") :: Nil) =
      term("(a = b) + c")
  }

  test("a + (b = c).d") {
    val ApplyInfix(
      TermName("a"),
      TermName("+"),
      Nil,
      Select(Assign(TermName("b"), TermName("c")), TermName("d")) :: Nil
    ) =
      term("a + (b = c).d")
  }

  test("a + (b: _*)") {
    val ApplyInfix(TermName("a"), TermName("+"), Nil, Repeated(TermName("b")) :: Nil) =
      term("a + (b: _*)")
  }

  test("a + ((b: _*))") {
    val ApplyInfix(TermName("a"), TermName("+"), Nil, Repeated(TermName("b")) :: Nil) =
      term("a + ((b: _*))")
  }

  test("local class") {
    val Term.Block(
      List(
        Defn.Class(
          List(Mod.Case()),
          Type.Name("C"),
          Nil,
          Ctor.Primary(
            Nil,
            Name.Anonymous(),
            List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)))
          ),
          EmptyTemplate()
        )
      )
    ) = term("{ case class C(x: Int); }")
  }

  test("xml literal - 1") {
    val Term.Block(
      List(
        Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Term.Xml(List(Lit("<p/>")), Nil)),
        Defn.Val(Nil, List(Pat.Var(Term.Name("y"))), None, Term.Name("x"))
      )
    ) =
      term("""{
      val x = <p/>
      val y = x
    }""")
  }

  test("implicit closure") {
    val Term.Apply(
      Term.Name("Action"),
      List(
        Term.Block(
          List(
            Term.Function(
              List(
                Term.Param(
                  List(Mod.Implicit()),
                  Term.Name("request"),
                  Some(Type.Apply(Type.Name("Request"), List(Type.Name("AnyContent")))),
                  None
                )
              ),
              Term.Name("Ok")
            )
          )
        )
      )
    ) =
      term("Action { implicit request: Request[AnyContent] => Ok }")
  }

  test("#312") {
    val Term.Block(
      List(
        Defn.Val(
          Nil,
          List(Pat.Var(Term.Name("x"))),
          None,
          Term.Ascribe(Term.Name("yz"), Type.Tuple(List(Type.Name("Y"), Type.Name("Z"))))
        ),
        Term.Tuple(List(Term.Name("x"), Term.Name("x")))
      )
    ) =
      term("""{
      val x = yz: (Y, Z)
      (x, x)
    }""")
  }

  test("spawn { var v: Int = _; ??? }") {
    val Term.Apply(
      Term.Name("spawn"),
      List(
        Term.Block(
          List(
            Defn.Var(Nil, List(Pat.Var(Term.Name("v"))), Some(Type.Name("Int")), None),
            Term.Name("???")
          )
        )
      )
    ) =
      term("spawn { var v: Int = _; ??? }")
  }

  test("#345") {
    val Term.Match(_, List(Case(_, _, rhs), _)) = term("""x match {
      case x => true
      // sobaka
      case y => y
    }""")
    assert(rhs.tokens.structure == "Tokens(true [26..30))")
  }

  test("a + (bs: _*) * c") {
    intercept[ParseException] { term("a + (bs: _*) * c") }
  }

  test("a + b: _*") {
    intercept[ParseException] { term("a + b: _*") }
  }

  test("foo(a + b: _*)") {
    val Term.Apply(
      Term.Name("foo"),
      List(
        Term.Repeated(Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("b"))))
      )
    ) =
      term("foo(a + b: _*)")
  }

  test("a + (c, d) * e") {
    val Term.ApplyInfix(
      Term.Name("a"),
      Term.Name("+"),
      Nil,
      List(
        Term.ApplyInfix(
          Term.Tuple(List(Term.Name("c"), Term.Name("d"))),
          Term.Name("*"),
          Nil,
          List(Term.Name("e"))
        )
      )
    ) =
      term("a + (c, d) * e")
  }

  test("a * (c, d) + e") {
    val Term.ApplyInfix(
      Term.ApplyInfix(Term.Name("a"), Term.Name("*"), Nil, List(Term.Name("c"), Term.Name("d"))),
      Term.Name("+"),
      Nil,
      List(Term.Name("e"))
    ) =
      term("a * (c, d) + e")
  }

  test("(a + b) c") {
    val Term.Select(
      Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("b"))),
      Term.Name("c")
    ) =
      term("(a + b) c")
  }

  test("a + b c") {
    val Term.Select(
      Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("b"))),
      Term.Name("c")
    ) =
      term("a + b c")
  }

  test("disallow parse[Stat] on statseqs") {
    intercept[ParseException] { stat("hello; world") }
  }

  test("\"stat;\".parse[Stat]") {
    val Term.Name("stat") = stat("stat;")
  }

  test("\"stat;\".parse[Term]") {
    intercept[ParseException] { term("stat;") }
  }

  test("$_") {
    intercept[ParseException](term(""" q"x + $_" """))
  }

  test("!x = y") {
    val Term.Assign(Term.ApplyUnary(Term.Name("!"), Term.Name("x")), Term.Name("y")) =
      term("!x = y")
  }

  test("x = (ys: _*)") {
    val Term.Assign(Term.Name("x"), Term.Repeated(Term.Name("ys"))) = term("x = (ys: _*)")
  }

  test("!(arr.cast[Ptr[Byte]] + sizeof[Ptr[_]]).cast[Ptr[Int]] = length") {
    val Term.Assign(
      Term.ApplyUnary(
        Term.Name("!"),
        Term.ApplyType(
          Term.Select(
            Term.ApplyInfix(
              Term.ApplyType(
                Term.Select(Term.Name("arr"), Term.Name("cast")),
                List(Type.Apply(Type.Name("Ptr"), List(Type.Name("Byte"))))
              ),
              Term.Name("+"),
              Nil,
              List(
                Term.ApplyType(
                  Term.Name("sizeof"),
                  List(
                    Type.Apply(Type.Name("Ptr"), List(Type.Placeholder(Type.Bounds(None, None))))
                  )
                )
              )
            ),
            Term.Name("cast")
          ),
          List(Type.Apply(Type.Name("Ptr"), List(Type.Name("Int"))))
        )
      ),
      Term.Name("length")
    ) =
      term("!(arr.cast[Ptr[Byte]] + sizeof[Ptr[_]]).cast[Ptr[Int]] = length")
  }

  test("(x ++ y)[T]") {
    val Term.ApplyType(
      Term.ApplyInfix(Term.Name("x"), Term.Name("++"), Nil, List(Term.Name("y"))),
      List(Type.Name("T"))
    ) = term("(x ++ y)[T]")
  }

  test(" structHydrators map { _[K]() } ") {
    val Term.ApplyInfix(
      Term.Name("structHydrators"),
      Term.Name("map"),
      Nil,
      List(
        Term.Block(List(Term.Apply(Term.ApplyType(Term.Placeholder(), List(Type.Name("K"))), Nil)))
      )
    ) = term(" structHydrators map { _[K]() } ")
  }

  test(" new C()[String]() ") {
    val Term.Apply(
      Term.ApplyType(
        New(Init(Type.Name("C"), Name.Anonymous(), List(List()))),
        List(Type.Name("String"))
      ),
      Nil
    ) = term(" new C()[String]() ")
  }

  test("#492 parse Unit in infix operations") {
    val Term.ApplyInfix(
      Term.Name("x"),
      Term.Name("=="),
      Nil,
      List(Term.ApplyInfix(Lit.Unit(), Term.Name("::"), Nil, List(Term.Name("Nil"))))
    ) = term("x == () :: Nil")
  }

  test("#492 parse hlist with Unit") {
    val Term.ApplyInfix(
      Lit.String("foo"),
      Term.Name("::"),
      Nil,
      List(
        Term.ApplyInfix(
          Lit.Unit(),
          Term.Name("::"),
          Nil,
          List(Term.ApplyInfix(Lit.Boolean(true), Term.Name("::"), Nil, List(Term.Name("HNil"))))
        )
      )
    ) = term(""""foo" :: () :: true :: HNil""")
  }

  test("nested-braces-in-paren") {
    val code = """(if (bar) {
        if (foo) { doFoo() }
        val x = 2
      })
    """

    val Term.If(
      Term.Name("bar"),
      Term.Block(
        List(
          Term.If(
            Term.Name("foo"),
            Term.Block(List(Term.Apply(Term.Name("doFoo"), Nil))),
            Lit.Unit()
          ),
          Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.Int(2))
        )
      ),
      Lit.Unit()
    ) = term(code)
  }

  test("fstring-interpolation") {
    val Term.Interpolate(
      Term.Name("f"),
      List(Lit.String("\\\\u"), Lit.String("%04x")),
      List(Term.Name("oct"))
    ) = term("""f"\\u$oct%04x"""")
  }

  test("block-arg") {
    val res = term("new Foo({str => str.length})")
    val termList = q"List($res)"
    assertEquals(term(termList.syntax).structure, termList.structure)
  }

  test("implicit-closure") {
    val res =
      term("""|function { implicit c =>
              |  {
              |    case bar => foo
              |  }
              |}""".stripMargin)

    val Term.Apply(
      Term.Name("function"),
      List(
        Term.Block(
          List(
            Term.Function(
              List(Term.Param(List(Mod.Implicit()), Term.Name("c"), None, None)),
              Term.PartialFunction(List(Case(Pat.Var(Term.Name("bar")), None, Term.Name("foo"))))
            )
          )
        )
      )
    ) = res
  }
}

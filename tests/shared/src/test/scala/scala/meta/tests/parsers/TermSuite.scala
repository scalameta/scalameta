package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.internal.trees.XtensionTreesName
import scala.meta.tokenizers.TokenizerOptions

import scala.language.implicitConversions

class TermSuite extends ParseSuite {
  import Name.{Anonymous, Indeterminate}
  import Term.{Name => _, _}

  implicit val dialect: Dialect = dialects.Scala211
  implicit def parseTerm(code: String)(implicit dialect: Dialect): Term = term(code)

  private def assertTerm(expr: String)(tree: Tree)(implicit loc: munit.Location): Unit =
    assertTree(term(expr))(tree)

  private def checkTerm(expr: String, syntax: String = null)(tree: Tree)(implicit
      loc: munit.Location
  ): Unit = checkTree(term(expr), syntax)(tree)

  test("x")(assertTerm("x")(tname("x")))

  test("`x`")(assertTerm("`x`")(tname("x")))

  test("a.b.c")(assertTerm("a.b.c")(tselect("a", "b", "c")))

  test("a.b c")(assertTerm("a.b c")(tpostfix(tselect("a", "b"), "c")))

  test("foo.this")(assertTerm("foo.this")(This(Indeterminate("foo"))))

  test("this")(assertTerm("this")(This(Anonymous())))

  test("a.super[b].c")(
    assertTerm("a.super[b].c")(tselect(Super(Indeterminate("a"), Indeterminate("b")), "c"))
  )

  test("super[b].c")(assertTerm("super[b].c")(tselect(Super(Anonymous(), Indeterminate("b")), "c")))

  test("a.super.c")(assertTerm("a.super.c")(tselect(Super(Indeterminate("a"), Anonymous()), "c")))

  test("super.c")(assertTerm("super.c")(tselect(Super(Anonymous(), Anonymous()), "c")))

  test("s\"a $b c\"")(assertTerm("s\"a $b c\"")(
    Interpolate(tname("s"), str("a ") :: str(" c") :: Nil, tname("b") :: Nil)
  ))

  test("f(0)")(assertTerm("f(0)")(Apply(tname("f"), int(0) :: Nil)))

  test("f(x = 0)")(assertTerm("f(x = 0)")(Apply(tname("f"), Assign(tname("x"), int(0)) :: Nil)))

  test("f(x: _*)")(assertTerm("f(x: _*)")(Apply(tname("f"), Repeated(tname("x")) :: Nil)))

  test("f((x: _*))")(assertTerm("f((x: _*))")(Apply(tname("f"), Repeated(tname("x")) :: Nil)))

  test("f(x = xs: _*)") {
    assertTerm("f(x = xs: _*)")(tapply(tname("f"), Assign(tname("x"), Repeated(tname("xs")))))
    matchSubStructure(
      "f(x = xs: _*)",
      {
        case Term.Apply.After_4_6_0(
              Term.Name("f"),
              Term.ArgClause(List(Assign(Term.Name("x"), Repeated(Term.Name("xs")))), None)
            ) => ()
      }
    )

    matchSubStructure(
      "f(x = xs: _*)",
      {
        case Term.Apply.internal.Latest(
              Term.Name("f"),
              Term.ArgClause(List(Assign(Term.Name("x"), Repeated(Term.Name("xs")))), None)
            ) => ()
      }
    )
  }

  test("f(x = (xs: _*))")(
    assertTerm("f(x = (xs: _*))")(tapply(tname("f"), Assign(tname("x"), Repeated(tname("xs")))))
  )

  test("a + () [scala211]") {
    implicit val dialect: Dialect = dialects.Scala211
    runTestAssert[Term]("a + ()")(tinfix("a", "+"))
  }

  test("a + () [scala212]") {
    implicit val dialect: Dialect = dialects.Scala212
    runTestAssert[Term]("a + ()")(tinfix("a", "+"))
  }

  test("a + () [scala213]") {
    implicit val dialect: Dialect = dialects.Scala213
    runTestAssert[Term]("a + ()", "a + (())")(tinfix("a", "+", lit()))
  }

  test("a + () [scala3]") {
    implicit val dialect: Dialect = dialects.Scala3
    runTestAssert[Term]("a + ()", "a + (())")(tinfix("a", "+", lit()))
  }

  test("a + (()) [scala211]") {
    implicit val dialect: Dialect = dialects.Scala211
    runTestAssert[Term]("a + (())")(tinfix("a", "+", lit()))
  }

  test("a + (()) [scala212]") {
    implicit val dialect: Dialect = dialects.Scala212
    runTestAssert[Term]("a + (())")(tinfix("a", "+", lit()))
  }

  test("a + (()) [scala213]") {
    implicit val dialect: Dialect = dialects.Scala213
    runTestAssert[Term]("a + (())")(tinfix("a", "+", lit()))
  }

  test("a + (()) [scala3]") {
    implicit val dialect: Dialect = dialects.Scala3
    runTestAssert[Term]("a + (())")(tinfix("a", "+", lit()))
  }

  test("a + b")(assertTerm("a + b")(ApplyInfix(tname("a"), tname("+"), Nil, tname("b") :: Nil)))

  test("a + b + c")(assertTerm("a + b + c")(ApplyInfix(
    ApplyInfix(tname("a"), tname("+"), Nil, tname("b") :: Nil),
    tname("+"),
    Nil,
    tname("c") :: Nil
  )))

  test("a :: b")(assertTerm("a :: b")(ApplyInfix(tname("a"), tname("::"), Nil, tname("b") :: Nil)))

  test("a :: b :: c")(assertTerm("a :: b :: c")(ApplyInfix(
    tname("a"),
    tname("::"),
    Nil,
    ApplyInfix(tname("b"), tname("::"), Nil, tname("c") :: Nil) :: Nil
  )))

  test("!a")(assertTerm("!a")(ApplyUnary(tname("!"), tname("a"))))

  test("!(a: _*)")(assertTerm("!(a: _*)")(ApplyUnary(tname("!"), Repeated(tname("a")))))

  test("a = true")(assertTerm("a = true")(Assign(tname("a"), bool(true))))

  test("a(0) = true")(assertTerm("a(0) = true")(Assign(Apply(tname("a"), int(0) :: Nil), bool(true))))

  test("return")(assertTerm("return")(Return(Lit.Unit())))

  test("return 1")(assertTerm("return 1")(Return(int(1))))

  test("throw 1")(assertTerm("throw 1")(Throw(int(1))))

  test("1: Int")(assertTerm("1: Int")(Ascribe(int(1), pname("Int"))))

  test("1: @foo")(assertTerm("1: @foo")(Annotate(int(1), Mod.Annot(init("foo")) :: Nil)))

  test("(true, false)")(assertTerm("(true, false)")(Tuple(bool(true) :: bool(false) :: Nil)))

  test("{ true; false }")(assertTerm("{ true; false }")(blk(bool(true), bool(false))))

  test("{ true }")(assertTerm("{ true }")(blk(bool(true))))

  test("if (true) true else false")(
    assertTerm("if (true) true else false")(If(bool(true), bool(true), bool(false)))
  )

  test("if (true) true; else false")(
    assertTerm("if (true) true; else false")(If(bool(true), bool(true), bool(false)))
  )

  test("if (true) true")(assertTerm("if (true) true")(If(bool(true), bool(true), Lit.Unit())))

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

    assertTerm(file)(Term.If(
      tinfix(
        tinfix(bool(true), "&&", tmatch(str(""), Case(patvar("other"), None, bool(true)))),
        "&&",
        bool(true)
      ),
      str(""),
      Lit.Unit()
    ))
  }

  test("() => x") {
    assertTerm("() => x")(tfunc()(tname("x")))
    assertTree(blockStat("() => x"))(tfunc()(tname("x")))
    assertTree(templStat("() => x"))(tfunc()(tname("x")))
  }

  test("(()) => x") {
    assertTerm("(()) => x")(tfunc()(tname("x")))
    assertTree(blockStat("(()) => x"))(tfunc()(tname("x")))
    assertTree(templStat("(()) => x"))(tfunc()(tname("x")))
  }

  test("x => x") {
    assertTerm("x => x")(tfunc(tparam("x"))(tname("x")))
    assertTree(blockStat("x => x"))(tfunc(tparam("x"))(tname("x")))

    intercept[ParseException](templStat("x => x"))
  }

  test("(x) => x") {
    assertTerm("(x) => x")(tfunc(tparam("x"))(tname("x")))
    assertTree(blockStat("(x) => x"))(tfunc(tparam("x"))(tname("x")))

    intercept[ParseException](templStat("(x) => x"))
  }

  test("_ => x") {
    assertTerm("_ => x")(tfunc(tparam("_"))(tname("x")))
    assertTree(blockStat("_ => x"))(tfunc(tparam("_"))(tname("x")))
    intercept[ParseException](templStat("_ => x"))
  }

  test("(_) => x") {
    assertTerm("(_) => x")(tfunc(tparam("_"))(tname("x")))
    assertTree(blockStat("(_) => x"))(tfunc(tparam("_"))(tname("x")))
    intercept[ParseException](templStat("(_) => x"))
  }

  test("x: Int => x") {
    // LAWL: this is how scalac's parser works
    assertTerm("x: Int => x")(Term.Ascribe(tname("x"), pfunc(pname("Int"))(pname("x"))))
    assertTree(blockStat("x: Int => x"))(tfunc(tparam("x", "Int"))(tname("x")))
    intercept[ParseException](templStat("x: Int => x"))
  }

  test("(x: Int) => x") {
    assertTerm("(x: Int) => x")(tfunc(tparam("x", "Int"))(tname("x")))
    assertTree(blockStat("(x: Int) => x"))(tfunc(tparam("x", "Int"))(tname("x")))

    assertTree(templStat("(x: Int) => x"))(tfunc(tparam("x", "Int"))(tname("x")))
  }

  test("_: Int => x") {
    assertTerm("_: Int => x")(Ascribe(Placeholder(), pfunc(pname("Int"))(pname("x"))))
    assertTree(blockStat("_: Int => x"))(tfunc(tparam("_", "Int"))(tname("x")))
    intercept[ParseException](templStat("_: Int => x"))
  }

  test("(_: Int) => x") {
    assertTerm("(_: Int) => x")(tfunc(tparam("_", "Int"))(tname("x")))
    assertTree(blockStat("(_: Int) => x"))(tfunc(tparam("_", "Int"))(tname("x")))
    assertTree(templStat("(_: Int) => x"))(tfunc(tparam("_", "Int"))(tname("x")))
  }

  test("x: Int, y: Int => x") {
    intercept[ParseException](term("x: Int, y: Int => x"))
    intercept[ParseException](blockStat("x: Int, y: Int => x"))
    intercept[ParseException](templStat("x: Int, y: Int => x"))
  }

  test("(x: Int, y: Int) => x") {
    assertTerm("(x: Int, y: Int) => x")(tfunc(tparam("x", "Int"), tparam("y", "Int"))(tname("x")))
    assertTree(
      blockStat("(x: Int, y: Int) => x")
    )(tfunc(tparam("x", "Int"), tparam("y", "Int"))(tname("x")))
    assertTree(
      templStat("(x: Int, y: Int) => x")
    )(tfunc(tparam("x", "Int"), tparam("y", "Int"))(tname("x")))
  }

  test("{ implicit x => () }")(assertTerm("{ implicit x => () }")(Block(
    Function(tparam(Mod.Implicit() :: Nil, "x") :: Nil, Lit.Unit()) :: Nil
  )))

  test("1 match { case 1 => true }")(
    assertTerm("1 match { case 1 => true }")(Match(int(1), Case(int(1), None, bool(true)) :: Nil))
  )

  test("1 match { case 1 => }")(
    assertTerm("1 match { case 1 => }")(Match(int(1), Case(int(1), None, blk()) :: Nil))
  )

  test("1 match { case 1 if true => }")(assertTerm("1 match { case 1 if true => }")(
    Match(int(1), Case(int(1), Some(bool(true)), blk()) :: Nil)
  ))

  test("1 match { case case 1 if true => }") {
    val intercepted = intercept[ParseException](term("1 match { case case 1 if true => }"))
    assertNoDiff(intercepted.shortMessage, "not expected `case`")
  }

  test("try 1")(assertTerm("try 1")(Try(int(1), Nil, None)))

  test("try 1 catch 1")(assertTerm("try 1 catch 1")(TryWithHandler(int(1), int(1), None)))

  test("try (2)")(assertTerm("try (2)")(Try(int(2), Nil, None)))

  test("try 1 catch { case _ => }")(
    assertTerm("try 1 catch { case _ => }")(Try(int(1), Case(patwildcard, None, blk()) :: Nil, None))
  )

  test("try 1 finally 1")(assertTerm("try 1 finally 1")(Try(int(1), Nil, Some(int(1)))))

  test("{ case 1 => () }")(
    assertTerm("{ case 1 => () }")(PartialFunction(Case(int(1), None, Lit.Unit()) :: Nil))
  )

  test("while (true) false")(assertTerm("while (true) false")(While(bool(true), bool(false))))

  test("do false while(true)")(assertTerm("do false while(true)")(Do(bool(false), bool(true))))

  test("for (a <- b; if c; x = a) x")(assertTerm("for (a <- b; if c; x = a) x")(For(
    List(
      Enumerator.Generator(patvar("a"), tname("b")),
      Enumerator.Guard(tname("c")),
      Enumerator.Val(patvar("x"), tname("a"))
    ),
    tname("x")
  )))

  test("for (a <- b; if c; x = a) yield x")(assertTerm("for (a <- b; if c; x = a) yield x")(ForYield(
    List(
      Enumerator.Generator(patvar("a"), tname("b")),
      Enumerator.Guard(tname("c")),
      Enumerator.Val(patvar("x"), tname("a"))
    ),
    tname("x")
  )))

  test("f(_)")(assertTerm("f(_)")(AnonymousFunction(Apply(tname("f"), List(Placeholder())))))

  test("_ + 1")(
    assertTerm("_ + 1")(AnonymousFunction(ApplyInfix(Placeholder(), tname("+"), Nil, int(1) :: Nil)))
  )

  test("1 + _")(
    assertTerm("1 + _")(AnonymousFunction(ApplyInfix(int(1), tname("+"), Nil, Placeholder() :: Nil)))
  )

  test("f _")(assertTerm("f _")(Eta(tname("f"))))

  test("new {}")(assertTerm("new {}")(NewAnonymous(tpl())))

  test("new { x }")(assertTerm("new { x }")(NewAnonymous(tpl(tname("x")))))

  test("new A")(assertTerm("new A")(New(init("A"))))

  test("new A(xs: _*)")(assertTerm("new A(xs: _*)")(New(init("A", List(Term.Repeated(tname("xs")))))))

  test("new A {}")(assertTerm("new A {}")(NewAnonymous(tpl(init("A") :: Nil, Nil))))

  test("new A with B")(assertTerm("new A with B")(NewAnonymous(tplNoBody(init("A"), init("B")))))

  test("new { val x: Int = 1 } with A")(
    assertTerm("new { val x: Int = 1 } with A")(NewAnonymous(Template(
      Defn.Val(Nil, List(patvar("x")), Some(pname("Int")), int(1)) :: Nil,
      init("A") :: Nil,
      EmptySelf(),
      Nil
    )))
  )

  test("new { self: T => }")(assertTerm("new { self: T => }")(NewAnonymous(tpl(self("self", "T")))))

  test("a + (b = c)")(assertTerm("a + (b = c)")(
    ApplyInfix(tname("a"), tname("+"), Nil, Assign(tname("b"), tname("c")) :: Nil)
  ))

  test("(a = b) + c")(assertTerm("(a = b) + c")(
    ApplyInfix(Assign(tname("a"), tname("b")), tname("+"), Nil, tname("c") :: Nil)
  ))

  test("a + (b = c).d")(assertTerm("a + (b = c).d")(tinfix("a", "+", tselect(Assign("b", "c"), "d"))))

  test("a + (b: _*)")(assertTerm("a + (b: _*)")(tinfix("a", "+", Repeated(tname("b")))))

  test("a + ((b: _*))")(assertTerm("a + ((b: _*))")(tinfix("a", "+", Repeated(tname("b")))))

  test("local class")(assertTerm("{ case class C(x: Int); }")(blk(
    Defn.Class(List(Mod.Case()), pname("C"), Nil, ctorp(tparam("x", "Int")), tplNoBody())
  )))

  test("xml literal - 1")(
    assertTerm(
      """|{
         |  val x = <p/>
         |  val y = x
         |}""".stripMargin
    )(blk(
      Defn.Val(Nil, List(patvar("x")), None, Term.Xml(List(str("<p/>")), Nil)),
      Defn.Val(Nil, List(patvar("y")), None, tname("x"))
    ))
  )

  test("implicit closure")(
    assertTerm("Action { implicit request: Request[AnyContent] => Ok }")(tapply(
      tname("Action"),
      blk(tfunc(tparam(List(Mod.Implicit()), "request", papply("Request", "AnyContent")))(tname("Ok")))
    ))
  )

  test("#312")(
    assertTerm(
      """|{
         |  val x = yz: (Y, Z)
         |  (x, x)
         |}""".stripMargin
    )(blk(
      Defn.Val(
        Nil,
        List(patvar("x")),
        None,
        Term.Ascribe(tname("yz"), Type.Tuple(List(pname("Y"), pname("Z"))))
      ),
      Term.Tuple(List(tname("x"), tname("x")))
    ))
  )

  test("spawn { var v: Int = _; ??? }")(assertTerm("spawn { var v: Int = _; ??? }")(tapply(
    tname("spawn"),
    blk(Defn.Var(Nil, List(patvar("v")), Some(pname("Int")), None), tname("???"))
  )))

  test("#345")(
    assertTerm(
      """|x match {
         |  case x => true
         |  // sobaka
         |  case y => y
         |}""".stripMargin
    )(tmatch(
      tname("x"),
      Case(patvar("x"), None, bool(true)),
      Case.createWithComments(patvar("y"), None, tname("y"), begComment = Seq("// sobaka"))
    ))
  )

  test("a + (bs: _*) * c")(intercept[ParseException](term("a + (bs: _*) * c")))

  test("a + b: _*")(intercept[ParseException](term("a + b: _*")))

  test("foo(a + b: _*)")(assertTerm("foo(a + b: _*)")(
    tapply(tname("foo"), Term.Repeated(tinfix(tname("a"), "+", tname("b"))))
  ))

  test("a + (c, d) * e")(assertTerm("a + (c, d) * e")(
    tinfix(tname("a"), "+", tinfix(Term.Tuple(List(tname("c"), tname("d"))), "*", tname("e")))
  ))

  test("a * (c, d) + e")(assertTerm("a * (c, d) + e")(
    tinfix(tinfix(tname("a"), "*", tname("c"), tname("d")), "+", tname("e"))
  ))

  test("(a + b) c")(assertTerm("(a + b) c")(tpostfix(tinfix("a", "+", "b"), "c")))

  test("a + b c")(assertTerm("a + b c")(tpostfix(tinfix("a", "+", "b"), "c")))

  test("disallow parse[Stat] on statseqs")(intercept[ParseException](stat("hello; world")))

  test("\"stat;\".parse[Stat]")(assertTrees(stat("stat;"))(tname("stat")))

  test("\"stat;\".parse[Term]")(intercept[ParseException](term("stat;")))

  test("$_")(intercept[ParseException](term(""" q"x + $_" """)))

  test("!x = y")(assertTerm("!x = y")(Term.Assign(Term.ApplyUnary(tname("!"), tname("x")), tname("y"))))

  test("x = (ys: _*)")(assertTerm("x = (ys: _*)")(Term.Assign(tname("x"), Term.Repeated(tname("ys")))))

  test("x = (ys: _`*`)") {
    val error =
      """|<input>:1: error: `identifier` expected but `)` found
         |x = (ys: _`*`)
         |             ^""".stripMargin
    runTestError[Stat]("x = (ys: _`*`)", error)
  }

  test("!(arr.cast[Ptr[Byte]] + sizeof[Ptr[_]]).cast[Ptr[Int]] = length")(
    assertTerm("!(arr.cast[Ptr[Byte]] + sizeof[Ptr[_]]).cast[Ptr[Int]] = length")(Term.Assign(
      Term.ApplyUnary(
        tname("!"),
        tapplytype(
          tselect(
            tinfix(
              tapplytype(tselect("arr", "cast"), papply("Ptr", "Byte")),
              "+",
              tapplytype("sizeof", papply("Ptr", pwildcard))
            ),
            "cast"
          ),
          papply("Ptr", "Int")
        )
      ),
      tname("length")
    ))
  )

  test("(x ++ y)[T]")(
    assertTerm("(x ++ y)[T]")(tapplytype(tinfix(tname("x"), "++", tname("y")), pname("T")))
  )

  test(" structHydrators map { _[K]() } ")(assertTerm(" structHydrators map { _[K]() } ")(tinfix(
    tname("structHydrators"),
    "map",
    blk(AnonymousFunction(tapply(tapplytype(Placeholder(), pname("K")))))
  )))

  test(" new C()[String]() ")(
    assertTerm(" new C()[String]() ")(tapply(tapplytype(New(init("C", Nil)), pname("String"))))
  )

  test("#492 parse Unit in infix operations")(
    assertTerm("x == () :: Nil")(tinfix(tname("x"), "==", tinfix(Lit.Unit(), "::", tname("Nil"))))
  )

  test("#492 parse hlist with Unit")(assertTerm(""""foo" :: () :: true :: HNil""")(
    tinfix(str("foo"), "::", tinfix(Lit.Unit(), "::", tinfix(bool(true), "::", tname("HNil"))))
  ))

  test("nested-braces-in-paren") {
    val code =
      """|(if (bar) {
         |  if (foo) { doFoo() }
         |  val x = 2
         |})
         |""".stripMargin

    assertTerm(code)(Term.If(
      tname("bar"),
      blk(
        Term.If(tname("foo"), blk(tapply(tname("doFoo"))), Lit.Unit()),
        Defn.Val(Nil, List(patvar("x")), None, int(2))
      ),
      Lit.Unit()
    ))
  }

  test("fstring-interpolation")(assertTerm("""f"\\u$oct%04x"""")(
    Term.Interpolate(tname("f"), List(str("\\\\u"), str("%04x")), List(tname("oct")))
  ))

  test("typed-interpolation")(assertTerm("""c"something"[String]""")(
    tapplytype(Term.Interpolate(tname("c"), List(str("something")), Nil), pname("String"))
  ))

  test("interpolation-with-escaped-quotes")(assertTerm("""s"\"$t\""""")(
    Interpolate(tname("s"), List(str("\\\""), str("\\\"")), List(tname("t")))
  ))

  test("implicit-closure")(
    assertTerm(
      """|function { implicit c =>
         |  {
         |    case bar => foo
         |  }
         |}""".stripMargin
    )(tapply(
      tname("function"),
      blk(tfunc(tparam(List(Mod.Implicit()), "c"))(Term.PartialFunction(List(
        Case(patvar("bar"), None, tname("foo"))
      ))))
    ))
  )

  test("type-partial-function") {
    val res = stat(
      """|val dynamicStrategy = resharding(
         |  { fakePartitionId: Int =>
         |    {
         |      case sendBox: SendBox.Args => 
         |    }: PartialFunction[ThriftStructIface, Unit]
         |  }
         |)
         |""".stripMargin
    )

    val expected = Defn.Val(
      Nil,
      List(patvar("dynamicStrategy")),
      None,
      tapply(
        tname("resharding"),
        blk(tfunc(tparam("fakePartitionId", "Int"))(Term.Ascribe(
          Term.PartialFunction(List(
            Case(Pat.Typed(patvar("sendBox"), pselect("SendBox", "Args")), None, blk())
          )),
          papply("PartialFunction", "ThriftStructIface", "Unit")
        )))
      )
    )
    assertTree(res)(expected)
  }

  test("partial-function-returning-implicit-closure")(
    assertTerm(
      """|{
         |  case true => implicit i => "xxx"
         |  case false => implicit i => i.toString
         |}""".stripMargin
    )(Term.PartialFunction(List(
      Case(bool(true), None, tfunc(tparam(List(Mod.Implicit()), "i"))(str("xxx"))),
      Case(bool(false), None, tfunc(tparam(List(Mod.Implicit()), "i"))(tselect("i", "toString")))
    )))
  )

  // https://github.com/scalameta/scalameta/issues/1843
  test("anonymous-function-#1843-1")(assertTerm("_ fun (_.bar)")(AnonymousFunction(
    ApplyInfix(Placeholder(), tname("fun"), Nil, List(AnonymousFunction(tselect(Placeholder(), "bar"))))
  )))
  test("anonymous-function-#1843-2")(assertTerm("_ fun _.bar")(AnonymousFunction(
    ApplyInfix(Placeholder(), tname("fun"), Nil, List(tselect(Placeholder(), "bar")))
  )))

  // https://scala-lang.org/files/archive/spec/2.13/06-expressions.html#placeholder-syntax-for-anonymous-functions
  test("anonymous-function-spec-1")(
    assertTerm("_ + 1")(AnonymousFunction(ApplyInfix(Placeholder(), tname("+"), Nil, List(int(1)))))
  )
  test("anonymous-function-spec-2")(assertTerm("_ * _")(AnonymousFunction(
    ApplyInfix(Placeholder(), tname("*"), Nil, List(Placeholder()))
  )))
  test("anonymous-function-spec-3")(assertTerm("(_: Int) * 2")(AnonymousFunction(
    ApplyInfix(Ascribe(Placeholder(), pname("Int")), tname("*"), Nil, List(int(2)))
  )))
  test("anonymous-function-spec-3.1")(
    assertTerm("1 + (_: Int) * 2 + 1")(AnonymousFunction(ApplyInfix(
      ApplyInfix(
        int(1),
        tname("+"),
        Nil,
        List(ApplyInfix(Ascribe(Placeholder(), pname("Int")), tname("*"), Nil, List(int(2))))
      ),
      tname("+"),
      Nil,
      List(int(1))
    )))
  )
  test("anonymous-function-spec-3.2")(assertTerm("(_: Int)")(Ascribe(Placeholder(), pname("Int"))))
  test("anonymous-function-spec-4")(
    assertTerm("if (_) x else y")(AnonymousFunction(If(Placeholder(), tname("x"), tname("y"))))
  )
  test("anonymous-function-spec-5")(
    assertTerm("_.map(f)")(AnonymousFunction(tapply(tselect(Placeholder(), "map"), "f")))
  )
  test("anonymous-function-spec-6")(assertTerm("_.map(_ + 1)")(AnonymousFunction(tapply(
    tselect(Placeholder(), "map"),
    AnonymousFunction(ApplyInfix(Placeholder(), tname("+"), Nil, List(int(1))))
  ))))

  test("anonymous-function-scalafmt-1")(
    assertTerm("foo >>= (_.http(registry, port).map(Option.apply(_)).catchSome { bar })")(ApplyInfix(
      tname("foo"),
      tname(">>="),
      Nil,
      List(AnonymousFunction(tapply(
        tselect(
          tapply(
            tselect(tapply(tselect(Placeholder(), "http"), "registry", "port"), "map"),
            AnonymousFunction(tapply(tselect("Option", "apply"), Placeholder()))
          ),
          "catchSome"
        ),
        blk(tname("bar"))
      )))
    ))
  )
  test("anonymous-function-scalafmt-2")(
    assertTerm("""scalacOptions ~= (_ filterNot (_ startsWith "-Xlint"))""")(ApplyInfix(
      tname("scalacOptions"),
      tname("~="),
      Nil,
      List(AnonymousFunction(ApplyInfix(
        Placeholder(),
        tname("filterNot"),
        Nil,
        List(AnonymousFunction(ApplyInfix(Placeholder(), tname("startsWith"), Nil, List(str("-Xlint")))))
      )))
    ))
  )
  test("anonymous-function-scalafmt-3")(assertTerm("`field-names` ~> (`private`(_: _*))")(ApplyInfix(
    tname("field-names"),
    tname("~>"),
    Nil,
    List(AnonymousFunction(Apply(tname("private"), List(Repeated(Placeholder())))))
  )))

  test("(a, b, c)")(assertTerm("(a, b, c)")(Term.Tuple(List(tname("a"), tname("b"), tname("c")))))

  test("((a, b, c))")(assertTerm("((a, b, c))")(Term.Tuple(List(tname("a"), tname("b"), tname("c")))))

  test("(a, b, c) :: ((a, b, c))")(assertTerm("(a, b, c) :: ((a, b, c))")(tinfix(
    Term.Tuple(List(tname("a"), tname("b"), tname("c"))),
    "::",
    Term.Tuple(List(tname("a"), tname("b"), tname("c")))
  )))

  test("((a, b, c)) :: ((a, b, c))")(assertTerm("((a, b, c)) :: ((a, b, c))")(tinfix(
    Term.Tuple(List(tname("a"), tname("b"), tname("c"))),
    "::",
    Term.Tuple(List(tname("a"), tname("b"), tname("c")))
  )))

  test("((a, b, c)) :: (a, b, c)")(assertTerm("((a, b, c)) :: (a, b, c)")(tinfix(
    Term.Tuple(List(tname("a"), tname("b"), tname("c"))),
    "::",
    tname("a"),
    tname("b"),
    tname("c")
  )))

  test("#2720 infix with repeated arg last")(
    assertTerm("a foo (b, c: _*)")(tinfix(tname("a"), "foo", tname("b"), Term.Repeated(tname("c"))))
  )
  test("#2720-for-comp")(assertTerm("for { `j`: Int <- Seq(4, 5, 6, 7)} yield `j`")(Term.ForYield(
    List(Enumerator.Generator(
      Pat.Typed(patvar("j"), pname("Int")),
      tapply(tname("Seq"), int(4), int(5), int(6), int(7))
    )),
    tname("j")
  )))

  test("#2720 infix with repeated arg not last") {
    val code = "a op (b: _*, c)"
    val tree = tinfix("a", "op", Term.Repeated("b"), "c")
    runTestAssert[Term](code)(tree)
  }

  test("#1384 string") {
    val tq = "\"" * 3
    val exprDq = raw"""("\n", "bar\n", "\nbaz")"""
    val exprTq = s"""($tq\n$tq, ${tq}bar\n$tq, $tq\nbaz$tq)"""
    checkTerm(exprDq, exprDq)(Term.Tuple(List(str("\n"), str("bar\n"), str("\nbaz"))))
    checkTerm(exprTq, exprTq)(Term.Tuple(List(str("\n"), str("bar\n"), str("\nbaz"))))
  }

  test("using-call-site in scala2")(checkStat("val a = f()(using a)(using 3, true)")(Defn.Val(
    Nil,
    List(patvar("a")),
    None,
    tapplyUsing(tapplyUsing(tapply(tname("f")), tname("a")), int(3), bool(true))
  )))

  test("scala3-syntax")(runTestError[Term](
    """|() match
       |  case _: Unit => ()""".stripMargin,
    "error: `{` expected but `case` found"
  ))

  test("using") {
    assertTerm("Set(using)")(tapply(tname("Set"), tname("using")))

    assertTerm("foo(using, bar)")(tapply(tname("foo"), tname("using"), tname("bar")))

    assertTerm(
      """|{
         |  val using ="asdsa"; 
         |  foo(using: String) 
         |}""".stripMargin
    )(blk(
      Defn.Val(Nil, List(patvar("using")), None, str("asdsa")),
      tapply(tname("foo"), Term.Ascribe(tname("using"), pname("String")))
    ))
  }

  test("implicit closure with ascribe") {
    val code =
      """|foo {
         |  implicit a => implicit b => {
         |    case bar => baz
         |  }: qux
         |}
         |""".stripMargin
    checkTerm(code)(tapply(
      tname("foo"),
      blk(tfunc(tparam(List(Mod.Implicit()), "a"))(tfunc(tparam(List(Mod.Implicit()), "b"))(
        Term
          .Ascribe(Term.PartialFunction(List(Case(patvar("bar"), None, tname("baz")))), pname("qux"))
      )))
    ))
  }

  test("implicit closure with val") {
    val code =
      """|foo { implicit a =>
         |  val bar = baz
         |  bar
         |}
         |""".stripMargin
    checkTerm(code)(tapply(
      tname("foo"),
      blk(tfunc(tparam(List(Mod.Implicit()), "a"))(blk(
        Defn.Val(Nil, List(patvar("bar")), None, tname("baz")),
        tname("bar")
      )))
    ))
  }

  test("if-with-parens-no-block [scala2]")(
    assertTerm(
      """|if (isEmpty)
         |  (None, None)
         |else {
         |  (Some(e), Some(f))
         |}
         |""".stripMargin
    )(Term.If(
      tname("isEmpty"),
      Term.Tuple(List(tname("None"), tname("None"))),
      blk(Term.Tuple(List(tapply(tname("Some"), tname("e")), tapply(tname("Some"), tname("f"))))),
      Nil
    ))
  )

  test("#3050 function without body") {
    val code =
      """|f { (x1: A, x2: B => C) =>
         |}
         |""".stripMargin
    checkTerm(code)(tapply(
      tname("f"),
      blk(tfunc(tparam("x1", "A"), tparam(Nil, "x2", pfunc(pname("B"))(pname("C"))))(blk()))
    ))
  }

  test("#3136: block catch handler, in braces") {
    val code =
      """|try ??? catch {
         |  val a = 10
         |  handler(a)
         |}
         |""".stripMargin
    checkTerm(code, code)(Term.TryWithHandler(
      tname("???"),
      blk(Defn.Val(Nil, List(patvar("a")), None, int(10)), tapply(tname("handler"), tname("a"))),
      None
    ))
  }

  test("#3138: infix with a following block") {
    val codeOnNextLine =
      """|{
         |  Foo bar (2, 3)
         |  { a }
         |}
         |""".stripMargin
    val codeOnSameLine =
      """|{
         |  Foo bar (2, 3) { a }
         |}
         |""".stripMargin
    val syntaxOnSameLine =
      """|{
         |  Foo bar (2, 3) {
         |    a
         |  }
         |}
         |""".stripMargin
    val treeOnSameLine =
      blk(tinfix(tname("Foo"), "bar", tapply(Term.Tuple(List(int(2), int(3))), blk(tname("a")))))
    runTestAssert[Stat](codeOnNextLine, Some(syntaxOnSameLine))(treeOnSameLine)
    runTestAssert[Stat](codeOnSameLine, Some(syntaxOnSameLine))(treeOnSameLine)
  }

  test("#3138: apply with a following block") {
    val codeOnNextLine =
      """|{
         |  Foo.bar (2, 3)
         |  { a }
         |}
         |""".stripMargin
    val codeOnSameLine =
      """|{
         |  Foo.bar (2, 3) { a }
         |}
         |""".stripMargin
    val syntaxOnSameLine =
      """|{
         |  Foo.bar(2, 3) {
         |    a
         |  }
         |}
         |""".stripMargin
    val treeOnSameLine = blk(tapply(tapply(tselect("Foo", "bar"), int(2), int(3)), blk(tname("a"))))
    runTestAssert[Stat](codeOnNextLine, Some(syntaxOnSameLine))(treeOnSameLine)
    runTestAssert[Stat](codeOnSameLine, Some(syntaxOnSameLine))(treeOnSameLine)
  }

  test("case: inside partial function") {
    runTestAssert[Term](
      """|{
         |  case foo
         |    if true =>
         |    List(bar)
         |}
         |""".stripMargin,
      Some(
        """|{
           |  case foo if true =>
           |    List(bar)
           |}""".stripMargin
      )
    )(Term.PartialFunction(
      Case(patvar("foo"), Some(bool(true)), tapply(tname("List"), tname("bar"))) :: Nil
    ))
  }

  test("expr with annotation, then match") {
    val code =
      """|underlyingStableClassRef(mbr.info.loBound): @unchecked match {
         |  case ref: TypeRef =>
         |}""".stripMargin
    val layout =
      """|(underlyingStableClassRef(mbr.info.loBound): @unchecked) match {
         |  case ref: TypeRef =>
         |}""".stripMargin
    runTestAssert[Term](code, Some(layout))(tmatch(
      Term.Annotate(
        tapply(tname("underlyingStableClassRef"), tselect("mbr", "info", "loBound")),
        List(Mod.Annot(init("unchecked")))
      ),
      Case(Pat.Typed(patvar("ref"), pname("TypeRef")), None, blk())
    ))
  }

  test("#3220") {
    val code =
      """|for {
         |  case (a, b) <- pairs
         |  x <- a to b
         |} yield x
         |""".stripMargin
    val layout = "for ( case (a, b) <- pairs; x <- a to b) yield x"
    runTestAssert[Term](code, Some(layout))(Term.ForYield(
      List(
        Enumerator.CaseGenerator(Pat.Tuple(List(patvar("a"), patvar("b"))), tname("pairs")),
        Enumerator.Generator(patvar("x"), tinfix(tname("a"), "to", tname("b")))
      ),
      tname("x")
    ))
  }

  test("#3224") {
    val code =
      """|for {
         |  x2 <- x1
         |} yield x2
         |  .x3 {
         |    case x4
         |        if x5.x6
         |          .x7(x8) =>
         |        x9
         |  }
         |""".stripMargin
    val layout =
      """|for (x2 <- x1) yield x2.x3 {
         |  case x4 if x5.x6.x7(x8) => x9
         |}
         |""".stripMargin
    runTestAssert[Term](code, Some(layout))(Term.ForYield(
      List(Enumerator.Generator(patvar("x2"), tname("x1"))),
      tapply(
        tselect("x2", "x3"),
        Term.PartialFunction(
          Case(patvar("x4"), Some(tapply(tselect("x5", "x6", "x7"), "x8")), tname("x9")) :: Nil
        )
      )
    ))
  }

  test("match on array-of-wildcard") {
    val code =
      """|obj match { case arr: Array[Array[_]] => }
         |""".stripMargin
    val layout =
      """|obj match {
         |  case arr: Array[Array[_]] =>
         |}
         |""".stripMargin
    runTestAssert[Term](code, Some(layout))(tmatch(
      tname("obj"),
      Case(Pat.Typed(patvar("arr"), papply("Array", papply("Array", pwildcard))), None, blk())
    ))
  }

  test("apply with arguments of various complexity") {
    val code =
      """|sc.submitJob(
         |  rdd,
         |  (iter: Iterator[Int]) => iter.toArray,
         |  partitions.getOrElse(rdd.partitions.indices),
         |  { case (_, _) => return }: (Int, Array[Int]) => Unit,
         |  { return }
         |)""".stripMargin
    val layout =
      """|sc.submitJob(rdd, (iter: Iterator[Int]) => iter.toArray, partitions.getOrElse(rdd.partitions.indices), {
         |  case (_, _) =>
         |    return
         |}: (Int, Array[Int]) => Unit, {
         |  return
         |})
         |""".stripMargin
    val tree = tapply(
      tselect("sc", "submitJob"),
      tname("rdd"),
      tfunc(tparam("iter", papply("Iterator", "Int")))(tselect("iter", "toArray")),
      tapply(tselect("partitions", "getOrElse"), tselect("rdd", "partitions", "indices")),
      Term.Ascribe(
        Term.PartialFunction(List(
          Case(Pat.Tuple(List(patwildcard, patwildcard)), None, Term.Return(Lit.Unit()))
        )),
        pfunc(pname("Int"), papply("Array", "Int"))(pname("Unit"))
      ),
      blk(Term.Return(Lit.Unit()))
    )
    runTestAssert[Term](code, Some(layout))(tree)
  }

  test("scalafmt #3911 for in parens, NL after `(`, NL between") {
    val code =
      """|for (
         |  a <- fooa
         |  b <- foob
         |  if a === b
         |) yield a
         |""".stripMargin
    val error =
      """|<input>:3: error: `)` expected but `<-` found
         |  b <- foob
         |    ^""".stripMargin
    runTestError[Term](code, error)
  }

  test("scalafmt #3911 for in parens, NL after `(`, `;` between") {
    val code =
      """|for (
         |  a <- fooa;
         |  b <- foob;
         |  if a === b
         |) yield a
         |""".stripMargin
    val layout = "for (a <- fooa; b <- foob; if a === b) yield a"
    val tree = Term.ForYield(
      List(
        Enumerator.Generator(patvar("a"), tname("fooa")),
        Enumerator.Generator(patvar("b"), tname("foob")),
        Enumerator.Guard(tinfix(tname("a"), "===", tname("b")))
      ),
      tname("a")
    )
    runTestAssert[Term](code, layout)(tree)
  }

  test("scalafmt #3911 for in parens, no NL after `(`, NL between") {
    val code =
      """|for (a <- fooa
         |     b <- foob
         |     if a === b) yield a
         |""".stripMargin
    val error =
      """|<input>:2: error: `)` expected but `<-` found
         |     b <- foob
         |       ^""".stripMargin
    runTestError[Term](code, error)
  }

  test("scalafmt #3911 for in parens, no NL after `(`, `;` between") {
    val code =
      """|for (a <- fooa;
         |     b <- foob;
         |     if a === b) yield a
         |""".stripMargin
    val layout = "for (a <- fooa; b <- foob; if a === b) yield a"
    val tree = Term.ForYield(
      List(
        Enumerator.Generator(patvar("a"), tname("fooa")),
        Enumerator.Generator(patvar("b"), tname("foob")),
        Enumerator.Guard(tinfix(tname("a"), "===", tname("b")))
      ),
      tname("a")
    )
    runTestAssert[Term](code, layout)(tree)
  }

  test("scalafmt #3911 for in parens, no NL after `(`, NL between, no NL before guard") {
    val code =
      """|for (a <- fooa if
         |       a > 0) yield a
         |""".stripMargin
    val layout = "for (a <- fooa; if a > 0) yield a"
    val tree = Term.ForYield(
      List(
        Enumerator.Generator(patvar("a"), tname("fooa")),
        Enumerator.Guard(tinfix(tname("a"), ">", lit(0)))
      ),
      tname("a")
    )
    runTestAssert[Term](code, layout)(tree)
  }

  test("scalafmt #3911 for in parens, no NL after `(`, `;` between, no NL before guard op") {
    val code =
      """|for (a <- fooa if a >
         |       0) yield a
         |""".stripMargin
    val layout = "for (a <- fooa; if a > 0) yield a"
    val tree = Term.ForYield(
      List(
        Enumerator.Generator(patvar("a"), tname("fooa")),
        Enumerator.Guard(tinfix(tname("a"), ">", lit(0)))
      ),
      tname("a")
    )
    runTestAssert[Term](code, layout)(tree)
  }

  test("#3713 cond in parens within enums") {
    val code =
      """|for (m <- decls
         |    if oneCond
         |      && (cond)
         |      && satisfiable) {}
         |
         |""".stripMargin
    val layout = "for (m <- decls; if oneCond && cond && satisfiable) {}"
    val tree = Term.For(
      List(
        Enumerator.Generator(patvar("m"), tname("decls")),
        Enumerator
          .Guard(tinfix(tinfix(tname("oneCond"), "&&", tname("cond")), "&&", tname("satisfiable")))
      ),
      blk()
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  Seq( // tuples of (operator, whether it's an assignment)
    ("foo", false),
    ("!=", false),
    ("+=", true),
    ("~=", true),
    (":=", true)
  ).foreach { case (op, isAssignment) =>
    test(s"isAssignmentOp($op) == $isAssignment") {
      matchSubStructure[Term](op, { case x: Name if x.isAssignmentOp == isAssignment => })
      matchSubStructure[Term](
        s"a $op b",
        { case x: Member.Infix if x.isAssignment == isAssignment => }
      )
    }
  }

  test("#3979 w/ grouped whitespace") {
    implicit def tokenizerOptions: TokenizerOptions = new TokenizerOptions(groupWhitespace = true)

    val code =
      """|for {
         |  _ <-
         |    if (a) {
         |      b
         |    } else {
         |      c
         |    }
         |
         |  _ <- d
         |} yield e
         |""".stripMargin
    val layout =
      """|for (_ <- if (a) {
         |  b
         |} else {
         |  c
         |}; _ <- d) yield e
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(
        Enumerator.Generator(patwildcard, Term.If(tname("a"), blk(tname("b")), blk(tname("c")), Nil)),
        Enumerator.Generator(patwildcard, tname("d"))
      )),
      tname("e")
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3979 w/ granular whitespace") {
    implicit def tokenizerOptions: TokenizerOptions = new TokenizerOptions(groupWhitespace = false)

    val code =
      """|for {
         |  _ <-
         |    if (a) {
         |      b
         |    } else {
         |      c
         |    }
         |
         |  _ <- d
         |} yield e
         |""".stripMargin
    val layout =
      """|for (_ <- if (a) {
         |  b
         |} else {
         |  c
         |}; _ <- d) yield e
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(
        Enumerator.Generator(patwildcard, Term.If(tname("a"), blk(tname("b")), blk(tname("c")), Nil)),
        Enumerator.Generator(patwildcard, tname("d"))
      )),
      tname("e")
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix with inline type args") {
    val code =
      """|a op [T]
         |  b
         |""".stripMargin
    val layout = "a op[T] b"
    val tree = tinfix("a", "op", List("T"), "b")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix with indented type args") {
    val code =
      """|a op
         |  [T]
         |  b
         |""".stripMargin
    val layout = "a op[T] b"
    val tree = tinfix("a", "op", List("T"), "b")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("postfix with illegal inline type args") {
    val code =
      """|a op [T]
         |""".stripMargin
    val error =
      """|<input>:1: error: type application is not allowed for postfix operators
         |a op [T]
         |     ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("postfix with illegal indented type args") {
    val code =
      """|a op
         |  [T]
         |""".stripMargin
    val error =
      """|<input>:2: error: type application is not allowed for postfix operators
         |  [T]
         |  ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("sfmt#4948 single-value named tuple") {
    val code = "(age = 2)"
    val layout = "age = 2"
    val tree = Term.Assign("age", lit(2))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("sfmt#4948 single-value named tuple in val rhs") {
    val code =
      """|{
         |  val _ = (age = 2)
         |}""".stripMargin
    val layout =
      """|{
         |  val _ = age = 2
         |}
         |""".stripMargin
    val tree = blk(Defn.Val(Nil, List(Pat.Wildcard()), None, Term.Assign("age", lit(2))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/right same precedence as middle/left, no parens") {
    val code = "foo *: bar * baz *: qux"
    val layout = "(foo *: bar) * (baz *: qux)"
    val tree = tinfix(tinfix("foo", "*:", "bar"), "*", tinfix("baz", "*:", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/right same precedence as middle/left, parens sides") {
    val code = "(foo *: bar) * (baz *: qux)"
    val layout = "(foo *: bar) * (baz *: qux)"
    val tree = tinfix(tinfix("foo", "*:", "bar"), "*", tinfix("baz", "*:", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/right same precedence as middle/left, parens middle") {
    val code = "foo *: (bar * baz) *: qux"
    val layout = "foo *: (bar * baz) *: qux"
    val tree = tinfix("foo", "*:", tinfix(tinfix("bar", "*", "baz"), "*:", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/left same precedence as middle/right, no parens") {
    val code = "foo * bar *: baz * qux"
    val layout = "foo * (bar *: baz) * qux"
    val tree = tinfix(tinfix("foo", "*", tinfix("bar", "*:", "baz")), "*", "qux")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/left same precedence as middle/right, parens sides") {
    val code = "(foo * bar) *: (baz * qux)"
    val layout = "(foo * bar) *: (baz * qux)"
    val tree = tinfix(tinfix("foo", "*", "bar"), "*:", tinfix("baz", "*", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/left same precedence as middle/right, parens middle") {
    val code = "foo * (bar *: baz) * qux"
    val layout = "foo * (bar *: baz) * qux"
    val tree = tinfix(tinfix("foo", "*", tinfix("bar", "*:", "baz")), "*", "qux")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/left same precedence as middle/left, no parens") {
    val code = "foo * bar * baz * qux"
    val layout = "foo * bar * baz * qux"
    val tree = tinfix(tinfix(tinfix("foo", "*", "bar"), "*", "baz"), "*", "qux")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/left same precedence as middle/left, parens sides") {
    val code = "(foo * bar) * (baz * qux)"
    val layout = "foo * bar * (baz * qux)"
    val tree = tinfix(tinfix("foo", "*", "bar"), "*", tinfix("baz", "*", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/left same precedence as middle/left, parens middle") {
    val code = "foo * (bar * baz) * qux"
    val layout = "foo * (bar * baz) * qux"
    val tree = tinfix(tinfix("foo", "*", tinfix("bar", "*", "baz")), "*", "qux")
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/right same precedence as middle/right, no parens") {
    val code = "foo *: bar *: baz *: qux"
    val layout = "foo *: bar *: baz *: qux"
    val tree = tinfix("foo", "*:", tinfix("bar", "*:", tinfix("baz", "*:", "qux")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/right same precedence as middle/right, parens sides") {
    val code = "(foo *: bar) *: (baz *: qux)"
    val layout = "(foo *: bar) *: baz *: qux"
    val tree = tinfix(tinfix("foo", "*:", "bar"), "*:", tinfix("baz", "*:", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/right same precedence as middle/right, parens middle") {
    val code = "foo *: (bar *: baz) *: qux"
    val layout = "foo *: (bar *: baz) *: qux"
    val tree = tinfix("foo", "*:", tinfix(tinfix("bar", "*:", "baz"), "*:", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/right higher precedence than middle/left, no parens") {
    val code = "foo :: bar == baz :: qux"
    val layout = "(foo :: bar) == (baz :: qux)"
    val tree = tinfix(tinfix("foo", "::", "bar"), "==", tinfix("baz", "::", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/right higher precedence than middle/left, parens") {
    val code = "(foo :: bar) == (baz :: qux)"
    val layout = "(foo :: bar) == (baz :: qux)"
    val tree = tinfix(tinfix("foo", "::", "bar"), "==", tinfix("baz", "::", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/right lower precedence than middle/left, no parens") {
    val code = "foo |: bar == baz |: qux"
    val layout = "foo |: (bar == baz) |: qux"
    val tree = tinfix("foo", "|:", tinfix(tinfix("bar", "==", "baz"), "|:", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: sides/right lower precedence than middle/left: parens") {
    val code = "(foo |: bar) == (baz |: qux)"
    val layout = "(foo |: bar) == (baz |: qux)"
    val tree = tinfix(tinfix("foo", "|:", "bar"), "==", tinfix("baz", "|:", "qux"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: mix 1") {
    val code = "foo :: (bar == baz) :: qux == xuq *: (zab +: rab) * oof"
    val layout = "(foo :: (bar == baz) :: qux) == (xuq *: zab +: rab) * oof"
    val tree = tinfix(
      tinfix("foo", "::", tinfix(tinfix("bar", "==", "baz"), "::", "qux")),
      "==",
      tinfix(tinfix("xuq", "*:", tinfix("zab", "+:", "rab")), "*", "oof")
    )
    parseAndCheckTree[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: mix 2") {
    val code = "foo :: (bar == baz) :: (qux == xuq) *: (zab +: rab) * oof"
    val layout = "foo :: (bar == baz) :: (((qux == xuq) *: zab +: rab) * oof)"
    val tree = tinfix(
      "foo",
      "::",
      tinfix(
        tinfix("bar", "==", "baz"),
        "::",
        tinfix(tinfix(tinfix("qux", "==", "xuq"), "*:", tinfix("zab", "+:", "rab")), "*", "oof")
      )
    )
    parseAndCheckTree[Stat](code, layout)(tree)
  }

  test("infix assoc and precedence: mix 3") {
    val code = "(foo :: bar) == (baz :: qux) == (xuq *: zab) +: (rab * oof)"
    val layout = "(foo :: bar) == (baz :: qux) == ((xuq *: zab) +: (rab * oof))"
    val tree = tinfix(
      tinfix(tinfix("foo", "::", "bar"), "==", tinfix("baz", "::", "qux")),
      "==",
      tinfix(tinfix("xuq", "*:", "zab"), "+:", tinfix("rab", "*", "oof"))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}

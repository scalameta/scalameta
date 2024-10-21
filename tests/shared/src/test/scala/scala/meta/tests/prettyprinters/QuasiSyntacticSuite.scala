package scala.meta.tests.prettyprinters

import scala.meta._
import scala.meta.trees.Origin

class QuasiSyntacticSuite extends scala.meta.tests.parsers.ParseSuite {

  import scala.meta.dialects.Scala211

  test("Type projections") {
    // Without lambda trick
    assertNoDiff(
      q"""class A { class B }
          type C = A#B
        """.syntax,
      """|class A { class B }
         |          type C = A#B
         |        """.stripMargin.lf2nl
    )
    // With lambda trick
    assertNoDiff(
      q"""
      def foo[F[_]]: Unit = ???
      foo[({ type T[A] = Either[Int, A] })#T]
        """.syntax,
      """|
         |      def foo[F[_]]: Unit = ???
         |      foo[({ type T[A] = Either[Int, A] })#T]
         |        """.stripMargin.lf2nl
    )
  }

  test("package object e extends D") {
    val q"package object $name $template" = q"package object e extends D"
    assertEquals(template.syntax, "extends D")
  }

  test("trait C extends A with B with D") {
    val q"trait $name $template" = q"trait C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("private sealed trait C extends A with B with D") {
    val q"private sealed trait $name $template" = q"private sealed trait C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("object C extends A with B with D") {
    val q"object $name $template" = q"object C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("private implicit object C extends A with B with D") {
    val q"private implicit object $name $template" =
      q"private implicit object C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("abstract class C extends A with B with D") {
    val q"abstract class $name $template" = q"abstract class C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("protected abstract class C extends A with B with D") {
    val q"protected abstract class $name $template" =
      q"protected abstract class C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("new C with A with B with D") {
    val Term.NewAnonymous(template) = q"new C with A with B with D"
    assertEquals(template.syntax, "C with A with B with D")
  }

  test("interpolator braces for operator identifiers") {
    implicit val parseStat: String => Stat = super.templStat
    checkWithOriginalSyntax[Stat](q"""s"$${+++}bar"""", """s"${+++}bar"""")("s\"${\n  +++\n}bar\"")
    checkWithOriginalSyntax[Stat](q"""s"$${+++}_bar"""", "s\"${+++}_bar\"")("s\"${\n  +++\n}_bar\"")
    checkWithOriginalSyntax[Stat](q"""s"$${+++}123"""", """s"${+++}123"""")("s\"${\n  +++\n}123\"")
    checkWithOriginalSyntax[Stat](q"""s"$${+++}***"""", """s"${+++}***"""")("s\"${\n  +++\n}***\"")
    checkWithOriginalSyntax[Stat](q"""s"$${+++} ***"""", "s\"${+++} ***\"")("s\"${\n  +++\n} ***\"")
  }

  test("interpolator braces for plain identifiers: check tokens") {
    val tree1: Tree = q"""s"$${foo}bar""""
    val numTokens = tree1.tokens.length
    assertEquals(tree1.tokens.length, numTokens)

    val input = Input.String(tree1.reprint)
    val source2 = new Origin.ParsedSource(input)
    val tree2 = tree1.withOrigin(new Origin.Parsed(source2, 0, numTokens))
    assertEquals(tree2.tokens.length, numTokens)
  }

  test("interpolator braces for plain identifiers") {
    implicit val parseStat: String => Stat = super.templStat
    checkWithOriginalSyntax[Stat](q"""s"$${foo}bar"""", """s"${foo}bar"""")("s\"${\n  foo\n}bar\"")
    checkWithOriginalSyntax[Stat](q"""s"$${foo}_bar"""", "s\"${foo}_bar\"")("s\"${\n  foo\n}_bar\"")
    checkWithOriginalSyntax[Stat](q"""s"$${foo}123"""", """s"${foo}123"""")("s\"${\n  foo\n}123\"")
    checkWithOriginalSyntax[Stat](q"""s"$${foo}***"""", """s"${foo}***"""")("s\"${\n  foo\n}***\"")
    checkWithOriginalSyntax[Stat](q"""s"$${foo} ***"""", "s\"${foo} ***\"")("s\"${\n  foo\n} ***\"")
  }

  test("Type.Function(Tuple, _) #557") {
    assertWithOriginalSyntax(t"((a, b)) => c", "((a, b)) => c", "((a, b)) => c")
    assertWithOriginalSyntax(t"((a, b), c) => c", "((a, b), c) => c", "((a, b), c) => c")
  }

  test("Term.Apply(_, List(Term.Function(...))) #572, #574") {
    assertWithOriginalSyntax(
      q"foo { implicit i: Int => () }",
      "foo { implicit i: Int => () }",
      "foo {\n  implicit i: Int => ()\n}"
    )
  }

  test("macro defs #581") {
    assertWithOriginalSyntax(q"def f = macro g", "def f = macro g", "def f = macro g")
    assertWithOriginalSyntax(q"def f: Int = macro g", "def f: Int = macro g", "def f: Int = macro g")
  }

  test("Importee.Rename") {
    assertWithOriginalSyntax(q"import a.{b=>c}", "import a.{b=>c}", "import a.{b => c}")
  }

  test("backquote importees when needed - scalafix #1337") {
    assertWithOriginalSyntax(q"import a.`{ q }`", "import a.`{ q }`", "import a.`{ q }`")
    assertWithOriginalSyntax(q"import a.`macro`", "import a.`macro`", "import a.`macro`")
  }

  test("show[Structure] should uppercase long literals suffix: '2l' -> '2L'") {
    assertTree(q"val x = 1l")(q"val x = 1L")
  }

  test("show[Structure] should lowercase float literals suffix: '0.01F' -> '0.01f'") {
    val expected = """|Defn.Val(
                      |  Nil,
                      |  List(
                      |    Pat.Var(Term.Name("x"))
                      |  ),
                      |  None,
                      |  Lit.Float(1f)
                      |)
                      |""".stripMargin
    assertStruct(q"val x = 1f")(expected)
    assertStruct(q"val x = 1F")(expected)
  }

  test("show[Structure] should lowercase double literals suffix: '0.01D' -> '0.01d'") {
    val expected = """|Defn.Val(
                      |  Nil,
                      |  List(
                      |    Pat.Var(Term.Name("x"))
                      |  ),
                      |  None,
                      |  Lit.Double(1d)
                      |)
                      |""".stripMargin
    assertStruct(q"val x = 1d")(expected)
    assertStruct(q"val x = 1D")(expected)
    assertStruct(q"val x = 1.0")(expected)
    assertStruct(q"val x = 1.0d")(expected)
  }

  test("#931 val `a b` = 2") {
    assertWithOriginalSyntax(q"val `a b` = 2", "val `a b` = 2", "val `a b` = 2")
  }

  test("#2097 val `macro` = 42") {
    assertWithOriginalSyntax(q"val `macro` = 42", "val `macro` = 42", "val `macro` = 42")
  }

  test("#1661 Names outside: Must start with either a letter or an operator") {
    assertWithOriginalSyntax(q"val `foo` = 2", "val `foo` = 2", "val foo = 2")
    assertWithOriginalSyntax(q"val `++++` = 2", "val `++++` = 2", "val ++++ = 2")
    assertWithOriginalSyntax(q"val `_+` = 2", "val `_+` = 2", "val `_+` = 2")
  }

  test("#1661 Names outside: Non-leading operators are accepted only after underscores") {
    assertWithOriginalSyntax(q"val `a_+` = 2", "val `a_+` = 2", "val a_+ = 2")
    assertWithOriginalSyntax(q"val `a_a_+` = 2", "val `a_a_+` = 2", "val a_a_+ = 2")
  }

  test("#1661 Names outside: Operators must not be followed by non-operators") {
    assertWithOriginalSyntax(q"val `+_a` = 2", "val `+_a` = 2", "val `+_a` = 2")
    assertWithOriginalSyntax(q"val `a_++` = 2", "val `a_++` = 2", "val a_++ = 2")
    assertWithOriginalSyntax(q"val `a_++a` = 2", "val `a_++a` = 2", "val `a_++a` = 2")
  }

  test("#1661 Names outside: Lexical letters and digits can follow underscores") {
    assertWithOriginalSyntax(q"val `_a` = 2", "val `_a` = 2", "val _a = 2")
    assertWithOriginalSyntax(q"val `a_a` = 2", "val `a_a` = 2", "val a_a = 2")
  }

  test("#1661 Names outside: Non-operators must not be followed by operators") {
    assertWithOriginalSyntax(q"val `a+` = 2", "val `a+` = 2", "val `a+` = 2")
    assertWithOriginalSyntax(q"val `a-b` = 2", "val `a-b` = 2", "val `a-b` = 2")
    assertWithOriginalSyntax(q"val `a:b` = 2", "val `a:b` = 2", "val `a:b` = 2")
  }

  test("#1661 Names outside: Comments must be handled carefully") {
    assertWithOriginalSyntax(q"val `/*` = 2", "val `/*` = 2", "val `/*` = 2")
    assertWithOriginalSyntax(q"val `//` = 2", "val `//` = 2", "val `//` = 2")
    assertWithOriginalSyntax(q"val `a_//` = 2", "val `a_//` = 2", "val `a_//` = 2")
  }

  test("#1817 ApplyInfix parentheses") {
    checkStat("list map println")(q"list map (println)")
    checkStat("list map add(1)")(q"list map (add(1))")
    checkStat("list map (add(_, 1))")(q"list map (add(_, 1))")
    checkStat("list map (bar: _*)")(q"list map (bar:_*)")
  }
  test("#1826 ApplyInfix parentheses on Select") {
    checkStat("list map (_.bar)")(q"list map (_.bar)")
    checkStat("list map Foo.bar")(q"list map (Foo.bar)")
  }
  test("1826 ApplyInfix parentheses on multiple Select") {
    checkStat("list map (_.foo.bar)")(q"list map (_.foo.bar)")
  }
  test("#1826 ApplyInfix parentheses on tuple") {
    checkStat("list map ((_, foo))")(q"list map ((_, foo))")
  }
  test("#1826 ApplyInfix parentheses on Apply") {
    checkStat("list map (_.->(foo))")(q"list map (_.->(foo))")
    checkStat("list map a.->(foo)")(q"list map a.->(foo)")
    checkStat("list map (_.diff(foo))")(q"list map (_.diff(foo))")
    checkStat("list map (_.diff.bar(foo))")(q"list map (_.diff.bar(foo))")
    checkStat("list map a.diff(foo)")(q"list map a.diff(foo)")
    checkStat("list map a.diff.bar(foo)")(q"list map a.diff.bar(foo)")
  }
  test("#1826 ApplyInfix parentheses on Function") {
    checkStat("list map (_ => foo)")(q"list map (_ => foo)")
  }
  test("#1826 ApplyInfix parentheses on ApplyInfix function") {
    checkStat("list map (_ diff foo)")(q"list map (_ diff foo)")
    // 'diff' has same precedence as 'map', so parentheses should be added
    checkStat("list map (a diff foo)")(q"list map (a diff foo)")
  }
  test("#1826 ApplyInfix parentheses on ApplyInfix operator") {
    checkStat("list map (_ -> foo)")(q"list map (_ -> foo)")
    // '->' has greater precendence than 'map', so parentheses are not needed
    checkStat("list map a -> foo")(q"list map (a -> foo)")
  }
  test("1826 ApplyInfix parentheses on Term.Match") {
    checkStat(s"list map (_ match {$EOL  case 1 => 2$EOL})")(q"list map (_ match { case 1 => 2})")
  }

  test("#1839 ApplyInfix parentheses on Term.Placeholder") {
    checkStat("list reduce (_ + _)")(q"list reduce (_ + _)")
    checkStat("list reduce (_ + _)")(q"list reduce (_ + (_))")
    checkStat("list reduce (_ + (_: Int))")(q"list reduce (_ + (_: Int))")
    checkStat("list reduce (_.foo + _.bar)")(q"list reduce (_.foo + _.bar)")
    checkStat("list reduce (_.a.b.c + _.d.e.f)")(q"list reduce (_.a.b.c + _.d.e.f)")
    checkStat("list reduce (_.a(foo) + _.b(bar))")(q"list reduce (_.a(foo) + _.b(bar))")
  }
  test("parentheses on function param clauses") {
    List(
      "def f: (B => B) => A" -> q"def f: (B => B) => A",
      "def f: B => B => A" -> q"def f: B => B => A",
      "def f: B => A" -> q"def f: B => A",
      "def f: (B => B) => A => A" -> q"def f: (B => B) => A => A",
      "def f: (B => B) => (A => A) => A" -> q"def f: (B => B) => (A => A) => A"
    ).foreach { case (code, expected) => checkStat(code, code)(expected) }
  }

  test("#1864 Terms with leading numerics are backquoted") {
    checkStat("""val `123foo` = "hello"""")(q""" val `123foo` = "hello" """)
  }

  test("#1868 Term.Eta preserves structure") {
    checkStat("(x _).y")(q"""(x _).y""")
    checkStat("x _")(q"""x _""")
  }

  test("#2447 Pat.Bind on tname")(checkStat("{\n  case x @ Y => x\n}")(q"{ case x @ Y => x }"))

  test("#2447 Pat.Bind on tname backticks") {
    checkStat("{\n  case x @ `y` => x\n}")(q"{ case x @ `y` => x }")
  }

  test("#1843 anonymous functions 1") {
    checkTree(q"list foo (_ fun (_.bar))")(Term.ApplyInfix(
      tname("list"),
      tname("foo"),
      Nil,
      List(Term.AnonymousFunction(Term.ApplyInfix(
        Term.Placeholder(),
        tname("fun"),
        Nil,
        List(Term.AnonymousFunction(Term.Select(Term.Placeholder(), tname("bar"))))
      )))
    ))
  }

  test("#1843 anonymous functions 2") {
    checkTree(q"list foo (_ fun _.bar)")(Term.ApplyInfix(
      tname("list"),
      tname("foo"),
      Nil,
      List(Term.AnonymousFunction(Term.ApplyInfix(
        Term.Placeholder(),
        tname("fun"),
        Nil,
        List(Term.Select(Term.Placeholder(), tname("bar")))
      )))
    ))
  }

  test("#2717 anonymous function with unary") {
    checkTree(q"xs span { !separates(_) }") {
      Term.ApplyInfix(
        tname("xs"),
        tname("span"),
        Nil,
        List(Term.Block(List(Term.AnonymousFunction(
          Term.ApplyUnary(tname("!"), Term.Apply(tname("separates"), List(Term.Placeholder())))
        ))))
      )
    }
  }

  test("anonymous function with new") {
    checkTree(q"foo map (new foo(_))") {
      Term.ApplyInfix(
        tname("foo"),
        tname("map"),
        Nil,
        List(Term.AnonymousFunction(Term.New(Init(pname("foo"), anon, List(List(Term.Placeholder()))))))
      )
    }
  }

  test("anonymous function with select") {
    checkTree(q"foo map (foo(_).bar)") {
      Term.ApplyInfix(
        tname("foo"),
        tname("map"),
        Nil,
        List(Term.AnonymousFunction(
          Term.Select(Term.Apply(tname("foo"), List(Term.Placeholder())), tname("bar"))
        ))
      )
    }
  }

  test("anonymous function with apply type") {
    checkTree(q"foo map (_.foo[A])") {
      Term.ApplyInfix(
        tname("foo"),
        tname("map"),
        Nil,
        List(Term.AnonymousFunction(
          Term.ApplyType(Term.Select(Term.Placeholder(), tname("foo")), List(pname("A")))
        ))
      )
    }
  }

  test("#2317 init block") {
    checkStat(
      """new Foo({
        |  str => str.length
        |})""".stripMargin
    )(q"new Foo({str => str.length})")
  }

  test("#1917 init lambda") {
    checkStat("new Foo((a: Int) => a + 1)")(q"new Foo((a: Int) => a + 1)")
  }

  test("#1596") {
    val tree: Term.Xml = q"<h1>a{b}</h1>"
    checkTree(tree)(Term.Xml(List(str("<h1>a"), str("</h1>")), Term.Block(List(tname("b"))) :: Nil))
    val Term.Xml(part1 :: part2 :: Nil, arg1 :: Nil) = tree

    assertEquals(part1.tokens.structure, """Tokens(Xml.Part(<h1>a) [0..5))""")

    assertEquals(
      arg1.tokens.structure,
      "Tokens(LeftBrace [5..6), Ident(b) [6..7), RightBrace [7..8))"
    )

    assertEquals(part2.tokens.structure, "Tokens(Xml.Part(</h1>) [8..13))")
  }

  test("#1063 original") {
    checkStat(
      """def withJsoup(html: Html)(cleaners: HtmlCleaner*): Html = withJsoup(html.body) {
        |  cleaners: _*
        |}""".stripMargin
    )(
      q"def withJsoup(html: Html)(cleaners: HtmlCleaner*): Html = withJsoup(html.body) { cleaners: _* }"
    )
  }

  test("#2708 term lassoc") {
    checkTree(
      q"""{
            () == ()
            (()) == (())
            () == () == ()
            (()) == (()) == (())
          }"""
    )(Term.Block(List(
      Term.ApplyInfix(Lit.Unit(), tname("=="), Nil, Nil),
      Term.ApplyInfix(Lit.Unit(), tname("=="), Nil, List(Lit.Unit())),
      Term.ApplyInfix(Term.ApplyInfix(Lit.Unit(), tname("=="), Nil, Nil), tname("=="), Nil, Nil),
      Term.ApplyInfix(
        Term.ApplyInfix(Lit.Unit(), tname("=="), Nil, List(Lit.Unit())),
        tname("=="),
        Nil,
        List(Lit.Unit())
      )
    )))
  }

  test("#2708 term rassoc") {
    checkTree(
      q"""{
            () :: ()
            (()) :: (())
            () :: () :: ()
            (()) :: (()) :: (())
          }"""
    )(Term.Block(List(
      Term.ApplyInfix(Lit.Unit(), tname("::"), Nil, Nil),
      Term.ApplyInfix(Lit.Unit(), tname("::"), Nil, List(Lit.Unit())),
      Term.ApplyInfix(
        Lit.Unit(),
        tname("::"),
        Nil,
        List(Term.ApplyInfix(Lit.Unit(), tname("::"), Nil, Nil))
      ),
      Term.ApplyInfix(
        Lit.Unit(),
        tname("::"),
        Nil,
        List(Term.ApplyInfix(Lit.Unit(), tname("::"), Nil, List(Lit.Unit())))
      )
    )))
  }

  test("#2708 pat lassoc") {
    checkTree(
      q"""foo match {
            case () == () =>
            case (()) == (()) =>
            case () == () == () =>
            case (()) == (()) == (()) =>
          }"""
    )(Term.Match(
      tname("foo"),
      List(
        Case(Pat.ExtractInfix(Lit.Unit(), tname("=="), Nil), None, Term.Block(Nil)),
        Case(Pat.ExtractInfix(Lit.Unit(), tname("=="), List(Lit.Unit())), None, Term.Block(Nil)),
        Case(
          Pat.ExtractInfix(Pat.ExtractInfix(Lit.Unit(), tname("=="), Nil), tname("=="), Nil),
          None,
          Term.Block(Nil)
        ),
        Case(
          Pat.ExtractInfix(
            Pat.ExtractInfix(Lit.Unit(), tname("=="), List(Lit.Unit())),
            tname("=="),
            List(Lit.Unit())
          ),
          None,
          Term.Block(Nil)
        )
      ),
      Nil
    ))
  }

  test("#2708 pat rassoc") {
    checkTree(
      q"""foo match {
            case () :: () =>
            case (()) :: (()) =>
            case () :: () :: () =>
            case (()) :: (()) :: (()) =>
          }"""
    )(Term.Match(
      tname("foo"),
      List(
        Case(Pat.ExtractInfix(Lit.Unit(), tname("::"), Nil), None, Term.Block(Nil)),
        Case(Pat.ExtractInfix(Lit.Unit(), tname("::"), List(Lit.Unit())), None, Term.Block(Nil)),
        Case(
          Pat.ExtractInfix(
            Lit.Unit(),
            tname("::"),
            List(Pat.ExtractInfix(Lit.Unit(), tname("::"), Nil))
          ),
          None,
          Term.Block(Nil)
        ),
        Case(
          Pat.ExtractInfix(
            Lit.Unit(),
            tname("::"),
            List(Pat.ExtractInfix(Lit.Unit(), tname("::"), List(Lit.Unit())))
          ),
          None,
          Term.Block(Nil)
        )
      ),
      Nil
    ))
  }

  test("pat infix: _ op (a | b)") {
    checkTree(p"_ op (a | b)", "_ op (a | b)") {
      Pat.ExtractInfix(
        Pat.Wildcard(),
        tname("op"),
        List(Pat.Alternative(Pat.Var(tname("a")), Pat.Var(tname("b"))))
      )
    }
  }

  test("pat infix: _ * (a + b)") {
    checkTree(p"_ * (a + b)", "_ * (a + b)") {
      Pat.ExtractInfix(
        Pat.Wildcard(),
        tname("*"),
        List(Pat.ExtractInfix(Pat.Var(tname("a")), tname("+"), List(Pat.Var(tname("b")))))
      )
    }
  }

  test("term infix: _ * (a + b)") {
    checkTree(q"_ * (a + b)", "_ * (a + b)") {
      Term.AnonymousFunction(Term.ApplyInfix(
        Term.Placeholder(),
        tname("*"),
        Nil,
        List(Term.ApplyInfix(tname("a"), tname("+"), Nil, List(tname("b"))))
      ))
    }
  }

  test("term infix: 1 + (2 / 3) * 4") {
    checkTree(q"1 + (2 / 3) * 4", "1 + 2 / 3 * 4") {
      Term.ApplyInfix(
        int(1),
        tname("+"),
        Nil,
        List(Term.ApplyInfix(
          Term.ApplyInfix(int(2), tname("/"), Nil, List(int(3))),
          tname("*"),
          Nil,
          List(int(4))
        ))
      )
    }
  }

  test("term infix: 1 + { 2 / 3 } * 4") {
    checkTree(
      q"1 + { 2 / 3 } * 4",
      """|1 + {
         |  2 / 3
         |} * 4""".stripMargin
    ) {
      Term.ApplyInfix(
        int(1),
        tname("+"),
        Nil,
        List(Term.ApplyInfix(
          Term.Block(List(Term.ApplyInfix(int(2), tname("/"), Nil, List(int(3))))),
          tname("*"),
          Nil,
          List(int(4))
        ))
      )
    }
  }

  test("term infix: { 2 / 3 } + 4") {
    checkTree(
      q"{ 2 / 3 } + 4",
      """|{
         |  2 / 3
         |} + 4""".stripMargin
    ) {
      Term.ApplyInfix(
        Term.Block(List(Term.ApplyInfix(int(2), tname("/"), Nil, List(int(3))))),
        tname("+"),
        Nil,
        List(int(4))
      )
    }
  }

  test("term anon func: foo.bar(_: Int, _: String)") {
    checkTree(q"foo.bar(_: Int, _: String)", "foo.bar(_: Int, _: String)") {
      Term.AnonymousFunction(Term.Apply(
        Term.Select(tname("foo"), tname("bar")),
        List(
          Term.Ascribe(Term.Placeholder(), pname("Int")),
          Term.Ascribe(Term.Placeholder(), pname("String"))
        )
      ))
    }
  }

}

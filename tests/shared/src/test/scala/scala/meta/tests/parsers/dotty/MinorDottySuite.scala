package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.Type.Apply
import scala.meta.Type.Placeholder

class MinorDottySuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)
  implicit val parseType: String => Type = code => tpe(code)(dialects.Dotty)
  implicit val parseSource: String => Source = code => source(code)(dialects.Dotty)

  val parseTempl: String => Stat = code => templStat(code)(dialects.Dotty)

  /**
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/other-new-features/open-classes.html
   *  https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html
   *  https://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html
   *  https://dotty.epfl.ch/docs/reference/changed-features/wildcards.html
   */
  test("open-class") {
    val Defn.Class(List(Mod.Open()), Type.Name("A"), _, _, _) =
      templStat("open class A {}")(dialects.Dotty)

    val Defn.Trait(List(Mod.Open()), Type.Name("C"), _, _, _) =
      templStat("open trait C {}")(dialects.Dotty)

    val Defn.Trait(List(Mod.Open(), Mod.Private(Name.Anonymous())), Type.Name("C"), _, _, _) =
      templStat("open private trait C {}")(dialects.Dotty)

    val Defn.Object(List(Mod.Open()), Term.Name("X"), _) =
      templStat("open object X {}")(dialects.Dotty)

  }

  test("open-class-negative-cases") {
    runTestError("final open class A {}", "illegal combination of modifiers: open and final")(
      parseTempl
    )
    runTestError(
      "open sealed trait C {}",
      "illegal combination of modifiers: open and sealed"
    )(parseTempl)
    runTestError("open def f(): Int = 3", "`open' modifier can be used only for classes")(
      parseTempl
    )
    runTestError("def f(open a: Int): Int = 3", "error")(parseTempl)
  }

  test("open-soft-modifier") {
    stat("def open(open: open): open = ???").structure
  }

  test("open-identifier") {
    runTestAssert[Stat]("def run(): Unit = { start; open(p); end }", assertLayout = None)(
      Defn.Def(
        Nil,
        Term.Name("run"),
        Nil,
        List(List()),
        Some(Type.Name("Unit")),
        Term.Block(
          List(
            Term.Name("start"),
            Term.Apply(Term.Name("open"), List(Term.Name("p"))),
            Term.Name("end")
          )
        )
      )
    )
  }

  test("case-classes-empty-plist") {
    templStat("case class A()")(dialects.Dotty)
    templStat("case class A @deprecated() ()")(dialects.Dotty)
    templStat("case class A private ()")(dialects.Dotty)
  }

  test("xml-literals") {
    intercept[TokenizeException] { term("<foo>{bar}</foo>")(dialects.Dotty) }
  }

  test("opaque-type-alias") {
    val input = "opaque type F = X"
    val typ = parseBlock(input).asInstanceOf[Defn.Type]
    val Type.Bounds(None, None) = typ.bounds
    runTestAssert[Stat](input)(
      Defn.Type(
        List(Mod.Opaque()),
        pname("F"),
        Nil,
        pname("X")
      )
    )(parseTempl)

  }

  test("opaque-type-bounded-alias") {
    val input = "opaque type F <: A & B = AB"
    val typ = parseBlock(input).asInstanceOf[Defn.Type]
    val Type.Bounds(None, Some(Type.And(Type.Name("A"), Type.Name("B")))) = typ.bounds
    runTestAssert[Stat](input)(
      Defn.Type(
        List(Mod.Opaque()),
        pname("F"),
        Nil,
        pname("AB")
      )
    )(parseTempl)
  }

  test("opaque-type-in-object") {
    runTestAssert[Source]("object X { opaque type IArray[+T] = Array }")(
      Source(
        List(
          Defn.Object(
            Nil,
            tname("X"),
            tpl(
              List(
                Defn.Type(
                  List(Mod.Opaque()),
                  pname("IArray"),
                  List(
                    Type.Param(
                      List(Mod.Covariant()),
                      Type.Name("T"),
                      Nil,
                      Type.Bounds(None, None),
                      Nil,
                      Nil
                    )
                  ),
                  pname("Array")
                )
              )
            )
          )
        )
      )
    )(parseSource)
  }

  test("opaque-type-mix-mods") {
    runTestAssert[Stat]("object X { private opaque type T = List[Int] }")(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Type(
              List(Mod.Private(Name("")), Mod.Opaque()),
              Type.Name("T"),
              Nil,
              Type.Apply(Type.Name("List"), List(Type.Name("Int")))
            )
          )
        )
      )
    )
    runTestAssert[Stat]("object X { opaque private type T = List[Int] }")(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Type(
              List(Mod.Opaque(), Mod.Private(Name(""))),
              Type.Name("T"),
              Nil,
              Type.Apply(Type.Name("List"), List(Type.Name("Int")))
            )
          )
        )
      )
    )
  }

  test("trait-parameters") {
    runTestAssert[Stat]("trait Foo(val foo: Int)(bar: Int)")(
      Defn.Trait(
        Nil,
        pname("Foo"),
        Nil,
        Ctor.Primary(
          Nil,
          anon,
          List(
            List(tparamval("foo", "Int")),
            List(tparam("bar", "Int"))
          )
        ),
        tpl(Nil)
      )
    )
  }

  test("trait-parameters-generic") {
    runTestAssert[Stat]("trait Foo[T](bar: T)")(
      Defn.Trait(Nil, pname("Foo"), List(pparam("T")), ctorp(List(tparam("bar", "T"))), tpl(Nil))
    )
  }

  test("class-parameters-using") {
    runTestAssert[Stat]("trait A(using String)")(
      Defn.Trait(Nil, pname("A"), Nil, ctorp(List(tparamUsing("", "String"))), tpl(Nil))
    )

    runTestAssert[Stat]("class A(using String)")(
      Defn.Class(Nil, pname("A"), Nil, ctorp(List(tparamUsing("", "String"))), tpl(Nil))
    )

    runTestAssert[Stat]("case class A(a: Int)(using b: String)")(
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        Ctor.Primary(Nil, anon, List(List(tparam("a", "Int")), List(tparamUsing("b", "String")))),
        tpl(Nil)
      )
    )
  }

  test("trait-extends-coma-separated") {
    runTestAssert[Stat](
      "trait Foo extends A, B, C",
      assertLayout = Some("trait Foo extends A with B with C")
    )(
      Defn.Trait(
        Nil,
        Type.Name("Foo"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, List(init("A"), init("B"), init("C")), Self(Name(""), None), Nil)
      )
    )(parseTempl)

    runTestAssert[Stat](
      "(new A(), new B())"
    )(
      Term.Tuple(
        List(
          Term.New(Init(Type.Name("A"), Name(""), List(List()))),
          Term.New(Init(Type.Name("B"), Name(""), List(List())))
        )
      )
    )(parseTempl)

  }

  // Super traits were removed in Scala 3
  test("super-trait") {
    runTestError[Stat]("super trait Foo", ". expected but trait found")
  }

  test("question-type") {
    runTestAssert[Stat]("val stat: Tree[? >: Untyped]")(
      Decl.Val(
        Nil,
        List(Pat.Var(Term.Name("stat"))),
        Type.Apply(
          Type.Name("Tree"),
          List(Type.Placeholder(Type.Bounds(Some(Type.Name("Untyped")), None)))
        )
      )
    )(parseTempl)

  }

  test("lazy-val-toplevel") {
    runTestAssert[Source]("lazy val x = 3")(
      Source(List(Defn.Val(List(Mod.Lazy()), List(Pat.Var(Term.Name("x"))), None, Lit.Int(3))))
    )
  }

  test("changed-operator-syntax") {
    // https://dotty.epfl.ch/docs/reference/changed-features/operators.html#syntax-change
    runTestAssert[Source]("""|object X {
                             |  println("hello")
                             |  ???
                             |  ??? match {
                             |    case 0 => 1
                             |  }
                             |}
                             |""".stripMargin)(
      Source(
        List(
          Defn.Object(
            Nil,
            Term.Name("X"),
            Template(
              Nil,
              Nil,
              Self(Name(""), None),
              List(
                Term.Apply(Term.Name("println"), List(Lit.String("hello"))),
                Term.Name("???"),
                Term.Match(Term.Name("???"), List(Case(Lit.Int(0), None, Lit.Int(1))))
              )
            )
          )
        )
      )
    )
  }

  test("type.param-with-name.anon") {
    runTestError[Stat]("trait F[_]", "identifier expected")
    runTestError[Stat]("class F[_]", "identifier expected")
    runTestError[Stat]("enum X[T]{ case A[_] extends X[Int] }", "identifier expected")
    runTestError[Stat]("extension [_](x: Int) def inc: Int = x + 1", "identifier expected")
    runTestError[Stat]("given [_](using Ord[T]) as Ord[List[T]]{}", "identifier expected")

    runTestAssert[Stat]("given [T](using Ord[T]) as Ord[List[T]]")(
      Defn.Given(
        Nil,
        Name(""),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              Name(""),
              Some(Type.Apply(Type.Name("Ord"), List(Type.Name("T")))),
              None
            )
          )
        ),
        Type.Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("List"), List(Type.Name("T"))))),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )
  }

  test("repeated-byname-class-parameter") {
    runTestAssert[Stat]("class Foo(bars: => Int*)")(
      Defn.Class(
        Nil,
        Type.Name("Foo"),
        Nil,
        Ctor.Primary(
          Nil,
          Name(""),
          List(
            List(
              Term.Param(
                Nil,
                Term.Name("bars"),
                Some(Type.Repeated(Type.ByName(Type.Name("Int")))),
                None
              )
            )
          )
        ),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )

    runTestAssert[Stat]("def fx(x: => Int*): Int = 3")(
      Defn.Def(
        Nil,
        Term.Name("fx"),
        Nil,
        List(
          List(
            Term
              .Param(Nil, Term.Name("x"), Some(Type.Repeated(Type.ByName(Type.Name("Int")))), None)
          )
        ),
        Some(Type.Name("Int")),
        Lit.Int(3)
      )
    )
  }

  test("lazy-abstract-class-value") {
    runTestAssert[Stat]("trait Foo { protected[this] lazy val from: Int }")(
      Defn.Trait(
        Nil,
        Type.Name("Foo"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Decl.Val(
              List(Mod.Protected(Term.This(Name(""))), Mod.Lazy()),
              List(Pat.Var(Term.Name("from"))),
              Type.Name("Int")
            )
          )
        )
      )
    )
    runTestError[Stat](
      "trait Foo { protected[this] lazy var from: Int }",
      "lazy not allowed here. Only vals can be lazy"
    )
  }

  test("type-wildcard-questionmark") {
    runTestAssert[Stat]("val x: List[?] = List(1)")(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("x"))),
        Some(Type.Apply(Type.Name("List"), List(Type.Placeholder(Type.Bounds(None, None))))),
        Term.Apply(Term.Name("List"), List(Lit.Int(1)))
      )
    )

    runTestAssert[Stat]("def x(a: List[?]): Unit = ()")(
      Defn.Def(
        Nil,
        Term.Name("x"),
        Nil,
        List(
          List(
            Term.Param(
              Nil,
              Term.Name("a"),
              Some(Type.Apply(Type.Name("List"), List(Type.Placeholder(Type.Bounds(None, None))))),
              None
            )
          )
        ),
        Some(Type.Name("Unit")),
        Lit.Unit()
      )
    )
  }

  test("unchecked-annotation") {
    runTestAssert[Stat]("val a :: Nil:  @unchecked = args")(
      Defn.Val(
        Nil,
        List(Pat.ExtractInfix(Pat.Var(Term.Name("a")), Term.Name("::"), List(Term.Name("Nil")))),
        Some(
          Type.Annotate(
            Type.AnonymousName(),
            List(Mod.Annot(Init(Type.Name("unchecked"), Name(""), Nil)))
          )
        ),
        Term.Name("args")
      )
    )

    runTestAssert[Stat]("val x:  @annotation.switch = 2")(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("x"))),
        Some(
          Type.Annotate(
            Type.AnonymousName(),
            List(
              Mod.Annot(
                Init(Type.Select(Term.Name("annotation"), Type.Name("switch")), Name(""), Nil)
              )
            )
          )
        ),
        Lit.Int(2)
      )
    )
  }

  val patternBinding = Term.Match(
    Lit.Int(1),
    List(Case(Pat.Bind(Pat.Var(Term.Name("intValue")), Lit.Int(1)), None, Term.Block(Nil)))
  )

  test("old-pattern-binding") {
    runTestAssert[Stat](
      """|1 match {
         |  case intValue @ 1 =>
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|1 match {
           |  case intValue as 1 =>
           |}
           |""".stripMargin
      )
    )(patternBinding)

  }

  test("new-pattern-binding") {
    runTestAssert[Stat]("""|1 match {
                           |  case intValue as 1 =>
                           |}
                           |""".stripMargin)(patternBinding)
  }

  test("new-pattern-binding-soft") {
    runTestAssert[Stat]("""|1 match {
                           |  case as as 1 =>
                           |}
                           |""".stripMargin)(
      Term.Match(
        Lit.Int(1),
        List(Case(Pat.Bind(Pat.Var(Term.Name("as")), Lit.Int(1)), None, Term.Block(Nil)))
      )
    )
  }

  test("new-pattern-binding-with-typed") {
    // yes parens are allowed here and don't change semantics
    val out = """|x match {
                 |  case (three as Some(3)): Option[Int] => "OK"
                 |}
                 |""".stripMargin
    runTestAssert[Stat](
      """|x match {
         |  case three as Some(3) : Option[Int] => "OK"
         |}
         |""".stripMargin,
      assertLayout = Some(out)
    )(
      Term.Match(
        Term.Name("x"),
        List(
          Case(
            Pat.Typed(
              Pat.Bind(
                Pat.Var(Term.Name("three")),
                Pat.Extract(Term.Name("Some"), List(Lit.Int(3)))
              ),
              Type.Apply(Type.Name("Option"), List(Type.Name("Int")))
            ),
            None,
            Lit.String("OK")
          )
        )
      )
    )
  }

  test("comment-after-coloneol") {
    val expected = "trait X { def x(): String }"
    runTestAssert[Stat](
      """trait X: // comment
        |  def x(): String
        |""".stripMargin,
      assertLayout = Some(expected)
    )(
      Defn.Trait(
        Nil,
        Type.Name("X"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Decl.Def(Nil, Term.Name("x"), Nil, List(List()), Type.Name("String")))
        )
      )
    )
  }

  test("opaque-type-indent-definition") {
    val expected = "opaque type LinearSet[Elem] = Set[Elem]"
    runTestAssert[Stat](
      """|opaque type LinearSet[Elem] =
         |  Set[Elem]
         |""".stripMargin,
      assertLayout = Some(expected)
    )(
      Defn.Type(
        List(Mod.Opaque()),
        Type.Name("LinearSet"),
        List(Type.Param(Nil, Type.Name("Elem"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Type.Apply(Type.Name("Set"), List(Type.Name("Elem")))
      )
    )
  }

  test("opaque-soft-keyword") {
    runTestAssert[Stat](
      "enum Kind { case Type(opaque: Boolean, transparent: Boolean) }"
    )(
      Defn.Enum(
        Nil,
        Type.Name("Kind"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.EnumCase(
              Nil,
              Term.Name("Type"),
              Nil,
              Ctor.Primary(
                Nil,
                Name(""),
                List(
                  List(
                    Term.Param(Nil, Term.Name("opaque"), Some(Type.Name("Boolean")), None),
                    Term.Param(Nil, Term.Name("transparent"), Some(Type.Name("Boolean")), None)
                  )
                )
              ),
              Nil
            )
          )
        )
      )
    )
  }

  test("capital-var-pattern-val") {
    runTestAssert[Stat](
      """val Private as _ = flags()
        |""".stripMargin
    )(
      Defn.Val(
        Nil,
        List(Pat.Bind(Pat.Var(Term.Name("Private")), Pat.Wildcard())),
        None,
        Term.Apply(Term.Name("flags"), Nil)
      )
    )
  }

  test("capital-var-pattern-case") {
    runTestAssert[Stat](
      """|flags() match {
         |  case Pattern as _ =>
         |}
         |""".stripMargin
    )(
      Term.Match(
        Term.Apply(Term.Name("flags"), Nil),
        List(Case(Pat.Bind(Pat.Var(Term.Name("Pattern")), Pat.Wildcard()), None, Term.Block(Nil)))
      )
    )
  }

  test("simple-derives") {
    val derivesEnum = """|enum Tree[T] derives Eq, Ordering, Show {
                         |  case Branch
                         |  case Leaf
                         |}
                         |""".stripMargin
    val template = parseTempl(derivesEnum)

    val derived = template.asInstanceOf[Defn.Enum].templ.derives
    val List(
      Type.Name("Eq"),
      Type.Name("Ordering"),
      Type.Name("Show")
    ) = derived

    runTestAssert[Stat](derivesEnum)(
      Defn.Enum(
        Nil,
        Type.Name("Tree"),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.EnumCase(Nil, Term.Name("Branch"), Nil, Ctor.Primary(Nil, Name(""), Nil), Nil),
            Defn.EnumCase(Nil, Term.Name("Leaf"), Nil, Ctor.Primary(Nil, Name(""), Nil), Nil)
          )
        )
      )
    )

  }

  test("extends-derives") {
    val derivesEnum = """|enum Tree[T] extends Bee derives Eq, scala.derives.Ordering, Show {
                         |  case Branch
                         |  case Leaf
                         |}
                         |""".stripMargin
    val template = parseTempl(derivesEnum)

    val derived = template.asInstanceOf[Defn.Enum].templ.derives
    val List(
      Type.Name("Eq"),
      Type.Select(Term.Select(Term.Name("scala"), Term.Name("derives")), Type.Name("Ordering")),
      Type.Name("Show")
    ) = derived

    runTestAssert[Stat](derivesEnum)(
      Defn.Enum(
        Nil,
        Type.Name("Tree"),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          List(Init(Type.Name("Bee"), Name(""), Nil)),
          Self(Name(""), None),
          List(
            Defn.EnumCase(Nil, Term.Name("Branch"), Nil, Ctor.Primary(Nil, Name(""), Nil), Nil),
            Defn.EnumCase(Nil, Term.Name("Leaf"), Nil, Ctor.Primary(Nil, Name(""), Nil), Nil)
          )
        )
      )
    )

  }

  test("case-class-derives") {
    val derivesEnum = """|case class Node(name : String) extends Tree derives Eq:
                         |  def hello() = ""
                         |  def bye() = ""
                         |
                         |""".stripMargin
    val template = parseTempl(derivesEnum)

    val derived = template.asInstanceOf[Defn.Class].templ.derives
    val List(
      Type.Name("Eq")
    ) = derived

    runTestAssert[Stat](
      derivesEnum,
      assertLayout = Some("""|case class Node(name: String) extends Tree derives Eq {
                             |  def hello() = ""
                             |  def bye() = ""
                             |}
                             |""".stripMargin)
    )(
      Defn.Class(
        List(Mod.Case()),
        Type.Name("Node"),
        Nil,
        Ctor.Primary(
          Nil,
          Name(""),
          List(List(Term.Param(Nil, Term.Name("name"), Some(Type.Name("String")), None)))
        ),
        Template(
          Nil,
          List(Init(Type.Name("Tree"), Name(""), Nil)),
          Self(Name(""), None),
          List(
            Defn.Def(Nil, Term.Name("hello"), Nil, List(List()), None, Lit.String("")),
            Defn.Def(Nil, Term.Name("bye"), Nil, List(List()), None, Lit.String(""))
          )
        )
      )
    )

  }

  test("catch-end-def") {
    val layout =
      """|object X {
         |  def fx = try action() catch {
         |    case ex =>
         |      err()
         |  }
         |  private abstract class X()
         |}
         |""".stripMargin
    runTestAssert[Stat](
      """|object X {
         |  def fx = try
         |    action()
         |  catch case ex => err()
         |
         |  private abstract class X()
         |}
         |""".stripMargin,
      assertLayout = Some(layout)
    )(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Def(
              Nil,
              Term.Name("fx"),
              Nil,
              Nil,
              None,
              Term.Try(
                Term.Apply(Term.Name("action"), Nil),
                List(Case(Pat.Var(Term.Name("ex")), None, Term.Apply(Term.Name("err"), Nil))),
                None
              )
            ),
            Defn.Class(
              List(Mod.Private(Name("")), Mod.Abstract()),
              Type.Name("X"),
              Nil,
              Ctor.Primary(Nil, Name(""), List(List())),
              Template(Nil, Nil, Self(Name(""), None), Nil)
            )
          )
        )
      )
    )
  }

  test("type-in-block") {
    runTestAssert[Stat](
      """|def hello = {
         |  type T
         |}
         |""".stripMargin
    )(
      Defn.Def(
        Nil,
        Term.Name("hello"),
        Nil,
        Nil,
        None,
        Term.Block(List(Decl.Type(Nil, Type.Name("T"), Nil, Type.Bounds(None, None))))
      )
    )
  }

  test("operator-next-line") {
    runTestAssert[Stat](
      """|val all = "-siteroot" +: "../docs"
         |    +: "-project" +:  Nil""".stripMargin,
      assertLayout = Some("""val all = "-siteroot" +: "../docs" +: "-project" +: Nil""")
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("all"))),
        None,
        Term.ApplyInfix(
          Lit.String("-siteroot"),
          Term.Name("+:"),
          Nil,
          List(
            Term.ApplyInfix(
              Lit.String("../docs"),
              Term.Name("+:"),
              Nil,
              List(
                Term.ApplyInfix(
                  Lit.String("-project"),
                  Term.Name("+:"),
                  Nil,
                  List(Term.Name("Nil"))
                )
              )
            )
          )
        )
      )
    )
  }

  test("operator-next-line-bad-indent") {
    runTestAssert[Stat](
      """|def withClasspath =
         |       "-siteroot" +: "../docs"
         |    +: "-project" +: Nil
         |""".stripMargin,
      assertLayout = Some("""def withClasspath = "-siteroot" +: "../docs" +: "-project" +: Nil""")
    )(
      Defn.Def(
        Nil,
        Term.Name("withClasspath"),
        Nil,
        Nil,
        None,
        Term.ApplyInfix(
          Lit.String("-siteroot"),
          Term.Name("+:"),
          Nil,
          List(
            Term.ApplyInfix(
              Lit.String("../docs"),
              Term.Name("+:"),
              Nil,
              List(
                Term.ApplyInfix(
                  Lit.String("-project"),
                  Term.Name("+:"),
                  Nil,
                  List(Term.Name("Nil"))
                )
              )
            )
          )
        )
      )
    )
  }

  test("colon-extractor") {
    runTestAssert[Stat](
      """|a match {case List(xs: _*) => }
         |""".stripMargin,
      assertLayout = Some(
        """|a match {
           |  case List(xs as _*) =>
           |}
           |""".stripMargin
      )
    )(
      Term.Match(
        Term.Name("a"),
        List(
          Case(
            Pat.Extract(
              Term.Name("List"),
              List(Pat.Bind(Pat.Var(Term.Name("xs")), Pat.SeqWildcard()))
            ),
            None,
            Term.Block(Nil)
          )
        )
      )
    )
  }

  test("at-extractor") {
    runTestAssert[Stat](
      """|a match {case List(xs@ _*) => }
         |""".stripMargin,
      assertLayout = Some(
        """|a match {
           |  case List(xs as _*) =>
           |}
           |""".stripMargin
      )
    )(
      Term.Match(
        Term.Name("a"),
        List(
          Case(
            Pat.Extract(
              Term.Name("List"),
              List(Pat.Bind(Pat.Var(Term.Name("xs")), Pat.SeqWildcard()))
            ),
            None,
            Term.Block(Nil)
          )
        )
      )
    )
  }

  test("empty-case-class") {
    val error = "case classes must have a parameter list"
    runTestError[Stat]("case class A", error)
    runTestError[Stat]("case class A[T]", error)
    runTestError[Stat]("case class A[T] private", error)
  }

  test("trailing-coma") {
    runTestAssert[Stat](
      """|case class A(
         |  x: X,
         |)""".stripMargin,
      assertLayout = Some(
        """|case class A(x: X)
           |""".stripMargin
      )
    )(
      Defn.Class(
        List(Mod.Case()),
        Type.Name("A"),
        Nil,
        Ctor.Primary(
          Nil,
          Name(""),
          List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None)))
        ),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )
  }

  test("complex-interpolation") {
    runTestAssert[Stat](
      """|val base =
         |  ""
         |  ++ s""
         |""".stripMargin,
      assertLayout = Some(
        """|val base = "" ++ (s"")
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("base"))),
        None,
        Term.ApplyInfix(
          Lit.String(""),
          Term.Name("++"),
          Nil,
          List(Term.Interpolate(Term.Name("s"), List(Lit.String("")), Nil))
        )
      )
    )
  }
}

package scala.meta.tests
package prettyprinters

import org.scalameta.invariants.InvariantFailedException
import scala.meta._
import scala.meta.prettyprinters.Show
import scala.meta.trees.Origin

class SyntacticSuite extends scala.meta.tests.parsers.ParseSuite {

  implicit class XtensionResetOrigin[T <: Tree](tree: T) {
    // NOTE: Ensures that neither the given tree nor its subtrees have their origins set.
    // This is necessary to force prettyprinting as opposed to reusing original syntax.
    def resetAllOrigins: T = tree.withOriginRecursive(Origin.None)
  }

  private implicit val dialect: Dialect = dialects.Scala211

  test("val x: Int (raw)") {
    val tree = templStat("val x: Int")
    assertTree(tree)(Decl.Val(Nil, List(Pat.Var(tname("x"))), pname("Int")))
  }

  test("val x: Int (code)") {
    val tree = templStat("val x: Int")
    assertEquals(tree.reprint, "val x: Int")
  }

  test("~(1 + 2) + ~x.y(z) + (~x).y(z)") {
    val tree = templStat("~(1 + 2) + ~x.y(z) + (~x).y(z)")
    assertEquals(tree.reprint, "~(1 + 2) + ~x.y(z) + (~x).y(z)")
  }

  test("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)") {
    val tree = templStat("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)")
    assertEquals(tree.reprint, "a + b + c && a + (b + c) && (a :: b :: c) && ((a :: b) :: c)")
  }

  test("(x map y).foo") {
    val tree = templStat("(x map y).foo")
    assertEquals(tree.reprint, "(x map y).foo")
  }

  test("multi-line string literals") {
    val tree = templStat(
      """|{
         |  val x = QQQ
         |    x
         |  QQQ
         |}""".stripMargin.replace("QQQ", "\"\"\"")
    )

    assertSameLines(
      tree.reprint,
      """|{
         |  val x = QQQ
         |    x
         |  QQQ
         |}
         |""".stripMargin.replace("QQQ", "\"\"\"")
    )
  }

  test("string literals with newlines and double quotes") {
    val tree = templStat(
      """|{
         |  val x = QQQ
         |    x
         |  QQQ
         |  val y = "\""
         |}""".stripMargin.replace("QQQ", "\"\"\"")
    )
    assertTree(tree)(Term.Block(List(
      Defn.Val(Nil, List(Pat.Var(tname("x"))), None, str("\n    x\n  ")),
      Defn.Val(Nil, List(Pat.Var(tname("y"))), None, str("\""))
    )))
    assertSameLines(
      tree.reprint,
      """|{
         |  val x = QQQ
         |    x
         |  QQQ
         |  val y = "\""
         |}
         |""".stripMargin.replace("QQQ", "\"\"\"")
    )
  }

  test("interpolations") {
    val tree = templStat(
      """|{
         |  val x = q"123 + $x + ${foo(123)} + 456"
         |  val y = QQQ
         |    $x
         |    $y
         |    ..$z
         |  QQQ
         |}""".stripMargin.replace("QQQ", "\"\"\"")
    )
    assertTree(tree)(Term.Block(List(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("x"))),
        None,
        Term.Interpolate(
          tname("q"),
          List(str("123 + "), str(" + "), str(" + 456")),
          List(tname("x"), Term.Block(Term.Apply(tname("foo"), List(int(123))) :: Nil))
        )
      ),
      Defn.Val(Nil, List(Pat.Var(tname("y"))), None, str("\n    $x\n    $y\n    ..$z\n  "))
    )))
    assertSameLines(
      tree.reprint,
      """|{
         |  val x = q"123 + $x + ${
         |    foo(123)
         |  } + 456"
         |  val y = QQQ
         |    $x
         |    $y
         |    ..$z
         |  QQQ
         |}""".stripMargin.replace("QQQ", "\"\"\"")
    )
  }

  test("foo.bar(bar) { baz }") {
    val tree = templStat("foo.bar(bar) { baz }")
    assertEquals(tree.reprint, s"foo.bar(bar) {$EOL  baz$EOL}")
  }

  test("Template.self stringifications") {
    assertEquals(templStat("new { val x = 2 }").reprint, "new { val x = 2 }")
    assertEquals(templStat("new { self => val x = 2 }").reprint, "new { self => val x = 2 }")
    assertEquals(
      templStat("new { self: Int => val x = 2 }").reprint,
      "new { self: Int => val x = 2 }"
    )
    assertEquals(
      templStat(
        """|
           |new {
           |  val x = 2
           |  val y = 3
           |}
           |""".stripMargin
      ).reprint,
      """|new {
         |  val x = 2
         |  val y = 3
         |}""".stripMargin.lf2nl
    )
    assertEquals(
      templStat(
        """|
           |new { self =>
           |  val x = 2
           |  val y = 3
           |}
           |""".stripMargin
      ).reprint,
      """|new { self =>
         |  val x = 2
         |  val y = 3
         |}""".stripMargin.lf2nl
    )
    assertEquals(
      templStat(
        """|
           |new { self: Int =>
           |  val x = 2
           |  val y = 3
           |}
           |""".stripMargin
      ).reprint,
      """|new { self: Int =>
         |  val x = 2
         |  val y = 3
         |}""".stripMargin.lf2nl
    )
    assertEquals(templStat("class B { x: B => }").reprint, "class B { x: B => }")
  }

  test("new X") {
    assertEquals(templStat("new X").reprint, "new X")
    assertEquals(templStat("new X {}").reprint, "new X {}")
  }

  test("(new X).bar") {
    assertEquals(templStat("(new X).bar").reprint, "(new X).bar")
    assertEquals(templStat("new X {}.bar").reprint, "new X {}.bar")
    assertEquals(templStat("new X().bar").reprint, "new X().bar")
  }

  test("ascribe and annotate") {
    assertEquals(templStat("_: Int").reprint, "_: Int")
    assertEquals(templStat("(_: Int) + 2").reprint, "(_: Int) + 2")
    assertEquals(templStat("x: @foo").reprint, "x: @foo")
    assertEquals(templStat("(x: @foo) + 2").reprint, "(x: @foo) + 2")
  }

  test("compound types") {
    assertEquals(tpe("Foo").reprint, "Foo")
    assertEquals(tpe("Foo {}").reprint, "Foo {}")
    assertEquals(
      tpe("Foo { type T = Int }").reprint,
      """|Foo {
         |  type T = Int
         |}""".stripMargin.lf2nl
    )
    assertEquals(
      tpe("Foo { type T = Int; type U <: String }").reprint,
      """|Foo {
         |  type T = Int
         |  type U <: String
         |}""".stripMargin.lf2nl
    )
    assertEquals(tpe("Foo with Bar").reprint, "Foo with Bar")
    assertEquals(tpe("Foo with Bar {}").reprint, "Foo with Bar {}")
    assertEquals(
      tpe("Foo with Bar { type T = Int }").reprint,
      """|Foo with Bar {
         |  type T = Int
         |}""".stripMargin.lf2nl
    )
    assertEquals(
      tpe("Foo with Bar { type T = Int; type U <: String }").reprint,
      """|Foo with Bar {
         |  type T = Int
         |  type U <: String
         |}""".stripMargin.lf2nl
    )
  }

  test("infix types") {
    assertEquals(tpe("Foo + Bar").reprint, "Foo + Bar")
    assertEquals(tpe("Foo & Bar").reprint, "Foo & Bar")
    assertEquals(tpe("Foo | Bar").reprint, "Foo | Bar")
  }

  test("literalTypes") {
    intercept[ParseException](dialects.Scala211("val a : 42 = 42").parse[Stat].get.reprint)
  }

  test("packages") {
    assertEquals(source("package foo.bar; class C").reprint, s"package foo.bar${EOL}class C")
    assertEquals(
      source("package foo.bar; class C; class D").reprint,
      s"package foo.bar${EOL}class C${EOL}class D"
    )
    assertEquals(source("package foo.bar { class C }").reprint, s"package foo.bar${EOL}class C")
    assertEquals(
      source("package foo.bar { class C; class D }").reprint,
      s"package foo.bar${EOL}class C${EOL}class D"
    )
  }

  test("type parameter mods") {
    assertEquals(source("class C[@foo T]").reprint, "class C[@foo T]")
    assertEquals(source("class C[+T]").reprint, "class C[+T]")
    assertEquals(source("class C[@foo +T]").reprint, "class C[@foo +T]")
  }

  test("primary constructor mods") {
    assertEquals(source("class C").reprint, "class C")
    assertEquals(source("class C private").reprint, "class C private")
    assertEquals(source("class C @foo(x)").reprint, "class C @foo(x)")
    assertEquals(source("class C @foo(x) private").reprint, "class C @foo(x) private")
    assertEquals(source("class C(x: Int)").reprint, "class C(x: Int)")
    assertEquals(source("class C private (x: Int)").reprint, "class C private (x: Int)")
    assertEquals(source("class C @foo(x) (x: Int)").reprint, "class C @foo(x) (x: Int)")
    assertEquals(
      source("class C @foo(x) private (x: Int)").reprint,
      "class C @foo(x) private (x: Int)"
    )
  }

  test("parentheses in patterns") {
    assertEquals(
      templStat("x match { case (xs: List[Int]) :+ x => ??? }").reprint,
      """|x match {
         |  case (xs: List[Int]) :+ x => ???
         |}""".stripMargin.lf2nl
    )
  }

  test("List(x, y) :: z") {
    assertEquals(templStat("List(x, y) :: z").reprint, "List(x, y) :: z")
    assertEquals(
      templStat("x match { case List(x, y) :: z => ??? }").reprint,
      """|x match {
         |  case List(x, y) :: z => ???
         |}""".stripMargin.lf2nl
    )
  }

  test("secondary ctor - expr") {
    assertEquals(
      source("class C(x: Int) { def this() = this(2) }").reprint,
      "class C(x: Int) { def this() = this(2) }"
    )
  }

  test("secondary ctor - block") {
    assertEquals(
      source("class C(x: Int) { def this() { this(2); println(\"OBLIVION!!!\") } }").reprint,
      """|class C(x: Int) {
         |  def this() = {
         |    this(2)
         |    println("OBLIVION!!!")
         |  }
         |}""".stripMargin.lf2nl
    )
  }

  test("case semicolons") {
    assertEquals(
      templStat("x match { case y => foo1; foo2 }").reprint,
      """|x match {
         |  case y =>
         |    foo1
         |    foo2
         |}""".stripMargin.lf2nl
    )
  }

  test("assorted literals") {
    assertEquals(templStat("true").reprint, "true")
    assertEquals(templStat("false").reprint, "false")
    assertEquals(templStat("0").reprint, "0")
    assertEquals(templStat("0l").reprint, "0L")
    assertEquals(templStat("0L").reprint, "0L")
    assertEquals(templStat("0f").reprint, "0f")
    assertEquals(templStat("0F").reprint, "0f")
    assertEquals(templStat("0.0f").reprint, "0.0f")
    assertEquals(templStat("0.0F").reprint, "0.0f")
    assertEquals(templStat("1.4f").reprint, "1.4f")
    assertEquals(templStat("1.40f").reprint, "1.40f")
    assertEquals(templStat("0.0").reprint, "0.0d")
    assertEquals(templStat("0d").reprint, "0d")
    assertEquals(templStat("0D").reprint, "0d")
    assertEquals(templStat("0.0d").reprint, "0.0d")
    assertEquals(templStat("0.0D").reprint, "0.0d")
    assertEquals(templStat("'0'").reprint, "'0'")
    assertEquals(templStat("\"0\"").reprint, "\"0\"")
    assertEquals(templStat("'zero").reprint, "'zero")
    assertEquals(templStat("null").reprint, "null")
    assertEquals(templStat("()").reprint, "()")
  }

  test("Lit.Double") {
    assertTree(templStat("1.4d"))(Lit.Double("1.4"))
    assertTree(templStat("1.40d"))(Lit.Double("1.40"))
    // NOTE: This fails under Scala Native:
    // [info] - Lit.Double *** FAILED ***
    // [info]   "Lit.Double(1.4[00000]d)" did not equal "Lit.Double(1.4[]d)" (SyntacticSuite.scala:321)
    // assertEquals(Lit.Double(1.40d).structure, "Lit.Double(1.4d)") // trailing 0 is lost
    // assertEquals(Lit.Double(1.4d).structure, "Lit.Double(1.4d)")
    // assertEquals(Lit.Double(1.40d).structure, "Lit.Double(1.4d)") // trailing 0 is lost
    def assertFiniteError(value: Double, what: String) = interceptMessage[InvariantFailedException](
      s"""|invariant failed:
          |when verifying java.lang.Double.isFinite(value)
          |found that java.lang.Double.isFinite(value) is false
          |where value = $what
          |""".stripMargin.lf2nl
    )(Lit.Double(value))
    assertFiniteError(Double.NaN, "NaN")
    assertFiniteError(Double.PositiveInfinity, "Infinity")
    assertFiniteError(Double.NegativeInfinity, "-Infinity")
  }

  test("Lit.Float") {
    assertTree(templStat("1.4f"))(Lit.Float("1.4"))
    assertTree(templStat("1.40f"))(Lit.Float("1.40"))
    def assertFiniteError(value: Float, what: String) = interceptMessage[InvariantFailedException](
      s"""|invariant failed:
          |when verifying java.lang.Float.isFinite(value)
          |found that java.lang.Float.isFinite(value) is false
          |where value = $what
          |""".stripMargin.lf2nl
    )(Lit.Float(value))
    assertFiniteError(Float.NaN, "NaN")
    assertFiniteError(Float.PositiveInfinity, "Infinity")
    assertFiniteError(Float.NegativeInfinity, "-Infinity")
  }

  test("context and view bounds") {
    assertEquals(templStat("class C[T: List, U <% Int]").reprint, "class C[T: List, U <% Int]")
    assertEquals(
      templStat("def m[T: List, U <% Int] = ???").reprint,
      "def m[T: List, U <% Int] = ???"
    )
  }

  test("some tricky parenthesization") {
    assertEquals(templStat("if (1) 2 else 3 + 4").reprint, "if (1) 2 else 3 + 4")
    assertEquals(templStat("(if (1) 2 else 3) + 4").reprint, "(if (1) 2 else 3) + 4")
    assertEquals(templStat("if (1) (if (2) 3) else 4").reprint, "if (1) (if (2) 3) else 4")
    assertEquals(templStat("if (1) if (2) 3 else () else 4").reprint, "if (1) (if (2) 3) else 4")
    assertEquals(
      templStat("if (1) 2 else 3 match { case _ => }").reprint,
      s"if (1) 2 else 3 match {$EOL  case _ =>$EOL}"
    )
    assertEquals(
      templStat("(if (1) 2 else 3) match { case _ => }").reprint,
      s"(if (1) 2 else 3) match {$EOL  case _ =>$EOL}"
    )
    assertEquals(templStat("unit.toCheck += (() => body)").reprint, "unit.toCheck += (() => body)")
    assertEquals(
      templStat("({ foo1; foo2 }).orElse(bar)").reprint,
      s"{$EOL  foo1$EOL  foo2$EOL}.orElse(bar)"
    )
    assertEquals(
      templStat("(foo match { case _ => }).orElse(bar)").reprint,
      s"(foo match {$EOL  case _ =>$EOL}).orElse(bar)"
    )
    assertEquals(
      templStat("foo || (if (cond) bar else baz)").reprint,
      "foo || (if (cond) bar else baz)"
    )
    assertEquals(
      templStat("foo && (bar match { case _ => })").reprint,
      s"foo && (bar match {$EOL  case _ =>$EOL})"
    )
    assertEquals(
      templStat("\"foo \" + (if (cond) bar else baz)").reprint,
      "\"foo \" + (if (cond) bar else baz)"
    )
    assertEquals(
      templStat("foo match { case bar @ (_: T1 | _: T2) => }").reprint,
      s"foo match {$EOL  case bar @ (_: T1 | _: T2) =>$EOL}"
    )
    assertEquals(
      templStat("foo match { case A + B / C => }").reprint,
      s"foo match {$EOL  case A + B / C =>$EOL}"
    )
    assertEquals(
      templStat("foo match { case (A + B) / C => }").reprint,
      s"foo match {$EOL  case (A + B) / C =>$EOL}"
    )
    assertEquals(
      templStat("foo match { case A + (B / C) => }").reprint,
      s"foo match {$EOL  case A + B / C =>$EOL}"
    )
    assertEquals(
      templStat("foo match { case bar :: Nil :: Nil => }").reprint,
      s"foo match {$EOL  case bar :: Nil :: Nil =>$EOL}"
    )
    assertEquals(
      templStat("foo match { case (bar :: Nil) :: Nil => }").reprint,
      s"foo match {$EOL  case (bar :: Nil) :: Nil =>$EOL}"
    )
    assertEquals(templStat("@(foo @foo) class Bar").reprint, "@(foo @foo) class Bar")
    assertEquals(templStat("(foo: Foo): @foo").reprint, "(foo: Foo): @foo")
    assertEquals(templStat("type T = A + B / C").reprint, "type T = A + B / C")
    assertEquals(templStat("type T = (A + B) / C").reprint, "type T = A + B / C")
    assertEquals(templStat("type T = A + (B / C)").reprint, "type T = A + (B / C)")
    assertEquals(templStat("type T = A :: B :: C").reprint, "type T = A :: B :: C")
    assertEquals(templStat("type T = (A :: B) :: C").reprint, "type T = (A :: B) :: C")
    assertEquals(
      templStat("foo match { case _: A | _: B => }").reprint,
      s"foo match {$EOL  case _: A | _: B =>$EOL}"
    )
    assertEquals(
      templStat("foo match { case _: A | _: B | _: C => }").reprint,
      s"foo match {$EOL  case _: A | _: B | _: C =>$EOL}"
    )
  }

  test("more trickiness") {
    assertEquals(templStat("def foo(bar_ : Int) = ???").reprint, "def foo(bar_ : Int) = ???")
    assertEquals(templStat("class C[T_ : Foo]").reprint, "class C[T_ : Foo]")
    assertEquals(templStat("val scala_ : NameType = ???").reprint, "val scala_ : NameType = ???")
  }

  test("class C extends (() => Int)") {
    assertEquals(templStat("class C extends (() => Int)").reprint, "class C extends (() => Int)")
  }

  test("package object e extends D") {
    assertEquals(topStat("package object e extends D").reprint, "package object e extends D")
  }

  test("trait C extends A with B with D") {
    assertEquals(
      templStat("trait C extends A with B with D").reprint,
      "trait C extends A with B with D"
    )
  }

  test("private sealed trait C extends A with B with D") {
    assertEquals(
      templStat("private sealed trait C extends A with B with D").reprint,
      "private sealed trait C extends A with B with D"
    )
  }

  test("object C extends A with B with D") {
    assertEquals(
      templStat("object C extends A with B with D").reprint,
      "object C extends A with B with D"
    )
  }

  test("private implicit object C extends A with B with D") {
    assertEquals(
      templStat("private implicit object C extends A with B with D").reprint,
      "private implicit object C extends A with B with D"
    )
  }

  test("abstract class C extends A with B with D") {
    assertEquals(
      templStat("abstract class C extends A with B with D").reprint,
      "abstract class C extends A with B with D"
    )
  }

  test("protected abstract class C extends A with B with D") {
    assertEquals(
      templStat("protected abstract class C extends A with B with D").reprint,
      "protected abstract class C extends A with B with D"
    )
  }

  test("new C with A with B with D") {
    assertEquals(templStat("new C with A with B with D").reprint, "new C with A with B with D")
  }

  test("class C(x: Int)(implicit y: String, z: Boolean)") {
    assertEquals(
      templStat("class C(x: Int)(implicit y: String, z: Boolean)").reprint,
      "class C(x: Int)(implicit y: String, z: Boolean)"
    )
  }

  test("#1837 class C(x: Int, implicit val|var y: String)") {
    assertEquals(
      templStat("class C(x: Int, implicit val y: String)").reprint,
      "class C(x: Int, implicit val y: String)"
    )
    assertEquals(
      templStat("class C(x: Int, implicit var y: String)").reprint,
      "class C(x: Int, implicit var y: String)"
    )
  }

  test("#1837 class C(<keyword> implicit val x: Int, y: String)(implicit z: Boolean)") {
    def checkSyntax(stat: String) = assertEquals(templStat(stat).reprint, stat)

    checkSyntax("class C(private implicit val x: Int, y: String)(implicit z: Boolean)")
    checkSyntax("class C(protected implicit val x: Int, y: String)(implicit z: Boolean)")
    checkSyntax("class C(final implicit val x: Int, y: String)(implicit z: Boolean)")
    checkSyntax("class C(override implicit val x: Int, y: String)(implicit z: Boolean) extends T")
  }

  test("class C(implicit override val x: Int, final implicit var y: String)") {
    checkTree(
      templStat("class C(implicit override val x: Int, final implicit var y: String)"),
      "class C(implicit override val x: Int, final var y: String)"
    ) {
      Defn.Class(
        Nil,
        pname("C"),
        Nil,
        ctorp(
          Mod.Implicit(),
          tparam(List(Mod.Override(), Mod.Implicit(), Mod.ValParam()), "x", "Int"),
          tparam(List(Mod.Final(), Mod.Implicit(), Mod.VarParam()), "y", "String")
        ),
        tplNoBody()
      )
    }
  }

  test("#1837 class C(private implicit val x: Int, implicit final val y: String, protected implicit var z: Boolean)") {
    checkTree(
      templStat("class C(private implicit val x: Int, implicit final val y: String, protected implicit var z: Boolean)"),
      "class C(implicit private val x: Int, final val y: String, protected var z: Boolean)"
    ) {
      Defn.Class(
        Nil,
        pname("C"),
        Nil,
        ctorp(
          Mod.Implicit(),
          tparam(List(Mod.Private(anon), Mod.Implicit(), Mod.ValParam()), "x", "Int"),
          tparam(List(Mod.Implicit(), Mod.Final(), Mod.ValParam()), "y", "String"),
          tparam(List(Mod.Protected(anon), Mod.Implicit(), Mod.VarParam()), "z", "Boolean")
        ),
        tplNoBody()
      )
    }
  }

  test("class C(var x: Int)") {
    assertEquals(templStat("class C(var x: Int)").reprint, "class C(var x: Int)")
  }

  test("private/protected within something") {
    assertEquals(
      templStat(
        """|
           |class C {
           |  private[this] val x = 1
           |  private[D] val y = 2
           |  protected[this] val z = 3
           |  protected[D] val w = 4
           |}
           |""".stripMargin
      ).reprint,
      """|class C {
         |  private[this] val x = 1
         |  private[D] val y = 2
         |  protected[this] val z = 3
         |  protected[D] val w = 4
         |}""".stripMargin.lf2nl
    )
  }

  test("case List(xs @ _*)") {
    val tree = pat("List(xs @ _*)")
    checkTree(tree, "List(xs @ _*)")(
      Pat.Extract(tname("List"), List(Pat.Bind(Pat.Var(tname("xs")), Pat.SeqWildcard())))
    )
  }

  test("case List[t](xs @ _*)") {
    val tree = pat("List[t](xs @ _*)")
    checkTree(tree, "List[t](xs @ _*)")(Pat.Extract(
      Term.ApplyType(tname("List"), List(Type.Var(pname("t")))),
      List(Pat.Bind(Pat.Var(tname("xs")), Pat.SeqWildcard()))
    ))
  }

  test("case List[_](xs @ _*)") {
    val tree = pat("List[_](xs @ _*)")
    checkTree(tree, "List[_](xs @ _*)")(Pat.Extract(
      Term.ApplyType(tname("List"), List(Type.Wildcard(Type.Bounds(None, None)))),
      List(Pat.Bind(Pat.Var(tname("xs")), Pat.SeqWildcard()))
    ))
  }

  test("package foo; class C; package baz { class D }") {
    val tree = source("package foo; class C; package baz { class D }")
    checkTree(tree, s"package foo${EOL}class C${EOL}package baz {$EOL  class D$EOL}")(Source(
      List(Pkg(
        tname("foo"),
        List(
          Defn.Class(Nil, pname("C"), Nil, ctor, tplNoBody()),
          Pkg(tname("baz"), List(Defn.Class(Nil, pname("D"), Nil, ctor, tplNoBody())))
        )
      ))
    ))
  }

  test("ammonite: package foo; class C; package baz { class D }") {
    val code = """
                 |package foo1; class C1; package baz1 { class D1 }
                 |@
                 |package foo2; class C2; package baz2 { class D2 }
                 |""".stripMargin
    val tree = ammonite(code)
    assertTree(tree)(MultiSource(List(
      Source(List(Pkg(
        tname("foo1"),
        List(
          Defn.Class(Nil, pname("C1"), Nil, ctor, tplNoBody()),
          Pkg(tname("baz1"), List(Defn.Class(Nil, pname("D1"), Nil, ctor, tplNoBody())))
        )
      ))),
      Source(List(Pkg(
        tname("foo2"),
        List(
          Defn.Class(Nil, pname("C2"), Nil, ctor, tplNoBody()),
          Pkg(tname("baz2"), List(Defn.Class(Nil, pname("D2"), Nil, ctor, tplNoBody())))
        )
      )))
    )))
    assertEquals(tree.syntax, code)
    assertEquals(
      tree.reprint,
      s"""package foo1
         |class C1
         |package baz1 {
         |  class D1
         |}
         |
         |@
         |
         |package foo2
         |class C2
         |package baz2 {
         |  class D2
         |}""".stripMargin.lf2nl
    )
  }

  test("case `x`") {
    val tree1 = pat("`x`")
    assertTree(tree1)(tname("x"))
    val tree2 = pat("f(`x`)")
    checkTree(tree2, "f(`x`)")(Pat.Extract(tname("f"), List(tname("x"))))
    val tree3 = pat("X")
    checkTree(tree3, "X")(tname("X"))
    val tree4 = pat("f(X)")
    checkTree(tree4, "f(X)")(Pat.Extract(tname("f"), List(tname("X"))))
  }

  test("case _: Int")(assertEquals(pat("_: Int").reprint, "_: Int"))

  test("case _: t")(assertEquals(pat("_: t").reprint, "_: t"))

  test("case _: F[t]")(assertEquals(pat("_: F[t]").reprint, "_: F[t]"))

  test("case _: F[_]")(assertEquals(pat("_: F[_]").reprint, "_: F[_]"))

  test("case _: F[?]")(assertEquals(pat("_: F[?]").reprint, "_: F[?]"))

  test("constructors") {
    val tree @ Defn.Class(_, _, _, primary, Template(_, _, _, List(secondary))) =
      templStat("class C(x: Int) { def this() = this(42) }")
    assertEquals(tree.reprint, "class C(x: Int) { def this() = this(42) }")
    assertEquals(primary.reprint, "(x: Int)")
    assertEquals(secondary.reprint, "def this() = this(42)")
    assertEquals(tree.toString, "class C(x: Int) { def this() = this(42) }")
    assertEquals(primary.toString, "def this(x: Int)")
    assertEquals(secondary.toString, "def this() = this(42)")
  }

  test("smart case printing - oneliner in one line") {
    val Term.Match(_, case1 :: Nil) = templStat("??? match { case x => x }")
    assertEquals(case1.reprint, "case x => x")
  }

  test("smart case printing - oneliner in multiple lines") {
    val Term.Match(_, case1 :: case2 :: Nil) =
      templStat("??? match { case x => x; case List(x, y) => println(x); println(y) }")
    assertSameLines(
      case1.reprint,
      """|case x =>
         |  x
         |""".stripMargin
    )
    assertEquals(
      case2.reprint,
      """|case List(x, y) =>
         |  println(x)
         |  println(y)""".stripMargin.lf2nl
    )
  }

  test("xml literals") {
    val tree = term("<foo>{bar}</foo>")
    checkTree(tree, "<foo>{\n  bar\n}</foo>")(
      Term.Xml(List(str("<foo>"), str("</foo>")), List(Term.Block(List(tname("bar")))))
    )
  }

  test("xml literals unit") {
    val tree = term("<foo>{}</foo>")
    checkTree(tree, "<foo>{}</foo>")(
      Term.Xml(List(str("<foo>"), str("</foo>")), List(Term.Block(Nil)))
    )
  }

  test("xml literals: pattern position") {
    assertEquals(pat("<a>{_*}</a>").show[Syntax], "<a>{_*}</a>")
    assertEquals(pat("<a>{ns @ _*}</a>").show[Syntax], "<a>{ns @ _*}</a>")
    assertEquals(pat("<a><b/>{ns @ _*}</a>").show[Syntax], "<a><b/>{ns @ _*}</a>")
  }

  test("interpolator unit") {
    val tree = term("""s"Hello${}World"""")
    checkTree(tree, """s"Hello${}World"""")(
      Term.Interpolate(tname("s"), List(str("Hello"), str("World")), List(Term.Block(Nil)))
    )
  }

  test("interpolator with $ character") {
    val tree = term("""s"$$$foo$$"""")
    checkTree(tree, """s"$$$foo$$"""")(
      Term.Interpolate(tname("s"), List(str("$"), str("$")), List(tname("foo")))
    )
  }

  Seq(
    ("${_}", "${\n  _\n}"),
    ("${x + y.map { _.length }.max}", "${\n  x + y.map {\n    _.length\n  }.max\n}"),
    ("${_a}", "${\n  _a\n}"),
    ("${_a}123", "${\n  _a\n}123"),
    ("${_a} 123", "${\n  _a\n} 123"),
    ("${_a}_123", "${\n  _a\n}_123"),
    ("${_a}+123", "${\n  _a\n}+123"),
    ("${++}", "${\n  ++\n}"),
    ("${++}123", "${\n  ++\n}123"),
    ("${++} 123", "${\n  ++\n} 123"),
    ("${++}_123", "${\n  ++\n}_123"),
    ("${++}+123", "${\n  ++\n}+123")
  ).foreach { case (codeInterp, termSyntaxInterp) =>
    test(
      s"term interpolator braces: test syntax/parsing consistency: $codeInterp -> $termSyntaxInterp"
    ) {
      def interp(str: String) = s"""s"${str.lf2nl}""""
      val syntax = interp(termSyntaxInterp)
      assertEquals(super.templStat(interp(codeInterp)).reprint, syntax)
      assertEquals(super.templStat(syntax).reprint, syntax)
    }
  }

  Seq(
    ("${_}", "${_}"),
    ("$_", "${_}"),
    ("${_a}", "${_a}"),
    ("${_a}123", "${_a}123"),
    ("${_a} 123", "${_a} 123"),
    ("${_a}_123", "${_a}_123"),
    ("${_a}+123", "${_a}+123"),
    ("${++}", "${++}"),
    ("${++}123", "${++}123"),
    ("${++} 123", "${++} 123"),
    ("${++}_123", "${++}_123"),
    ("${++}+123", "${++}+123")
  ).foreach { case (codeInterp, patSyntaxInterp) =>
    test(
      s"pat interpolator braces: test syntax/parsing consistency: $codeInterp -> $patSyntaxInterp"
    ) {
      def interp(str: String) = s"""s"${str.lf2nl}""""
      val syntax = interp(patSyntaxInterp)
      assertEquals(super.pat(interp(codeInterp)).reprint, syntax)
      assertEquals(super.pat(syntax).reprint, syntax)
    }
  }

  test("interpolator braces for term names beginning with '_'") {
    def interpolate(before: String, after: String): Term.Interpolate = Term.Interpolate(
      prefix = tname("s"),
      parts = List(str(before), str(after)),
      args = List(tname("_foo"))
    )
    assertWithOriginalSyntax(interpolate("", ""), """s"${_foo}"""", """s"${_foo}"""")
    assertWithOriginalSyntax(
      interpolate("bar", "baz"),
      """s"bar${_foo}baz"""",
      """s"bar${_foo}baz""""
    )
    assertWithOriginalSyntax(interpolate("[", "]"), """s"[${_foo}]"""", """s"[${_foo}]"""")
  }

  test("empty-arglist application") {
    val tree = term("foo.toString()")
    checkTree(tree, "foo.toString()")(Term.Apply(Term.Select(tname("foo"), tname("toString")), Nil))
  }

  test("type parameters with type bounds") {
    val Defn.Def(_, _, List(tree), _, _, _) = templStat("def foo[T <: Int] = ???")
    checkTree(tree, "T <: Int")(pparam("T", hiBound("Int")))

    assertTree(templStat("def foo[T <: Int] = ???")) {
      Defn.Def(Nil, tname("foo"), Type.ParamClause(tree :: Nil), Nil, None, tname("???"))
    }
  }

  test("Lit(()) - 1") {
    val lit @ Lit(()) = term("()")
    checkTree(lit, "()")(Lit.Unit())
  }

  test("Lit(()) - 2") {
    val Term.If(Term.Name("cond"), Lit(42), lit @ Lit.Unit()) = super.term("if (cond) 42")
    checkTree(lit, "()")(Lit.Unit())
  }

  test("Term.Apply(_, List(Term.Function(...))) #572, #574") {
    val tree1 = Term
      .Apply(tname("foo"), List(Term.Function(List(tparam(List(), "i", "Int")), Lit.Unit())))
    val tree2 = Term.Apply(
      tname("foo"),
      List(Term.Function(List(tparam(List(Mod.Implicit()), "i", "Int")), Lit.Unit()))
    )
    val tree3 = Term.Apply(tname("foo"), List(Term.Function(List(tparam(List(), "i")), Lit.Unit())))
    assertWithOriginalSyntax(tree1, "foo((i: Int) => ())", "foo((i: Int) => ())")
    assertWithOriginalSyntax(tree2, "foo { implicit i: Int => () }", "foo { implicit i: Int => () }")
    assertWithOriginalSyntax(tree3, "foo(i => ())", "foo(i => ())")
  }

  test("Pat.Interpolate syntax is correct #587") {
    val interpolate = Pat.Interpolate(
      tname("q"),
      List(str("object "), str(" { .."), str(" }")),
      List(Pat.Var(tname("name")), Pat.Var(tname("stats")))
    )
    assertWithOriginalSyntax(
      interpolate,
      """q"object ${name} { ..${stats} }"""",
      """q"object ${name} { ..${stats} }""""
    )
  }

  test("show[Structure] should uppercase long literals suffix: '2l' -> '2L'") {
    assertTree(templStat("foo(1l, 1L)"))(Term.Apply(tname("foo"), List(Lit.Long(1L), Lit.Long(1L))))
  }

  test("show[Structure] should lowercase float literals suffix: '0.01F' -> '0.01f'") {
    assertTree(templStat("foo(0.01f, 0.01F)"))(
      Term.Apply(tname("foo"), List(Lit.Float("0.01"), Lit.Float("0.01")))
    )
  }

  test("show[Structure] should lowercase double literals suffix: '0.01D' -> '0.01d'") {
    assertTree(templStat("foo(0.02d, 0.02D, 0.02)"))(
      Term.Apply(tname("foo"), List(Lit.Double("0.02"), Lit.Double("0.02"), Lit.Double("0.02")))
    )
  }

  test("#1864 Terms with leading numerics are backquoted") {
    checkStat("`123foo`")(tname("123foo"))
  }

  test("#1868 Term.Eta preserves structure") {
    checkStat("(x _).y")(Term.Select(Term.Eta(tname("x")), tname("y")))
    checkStat("x _")(Term.Eta(tname("x")))
  }

  test("#2106 unicode characters are properly escaped in string literals") {
    checkStat("\"xy\\u001az\"")(str("xy\u001az"))
    checkStat("\"þæö\"")(str("þæö"))
    checkStat("\">\"")(str(">"))
    checkStat("\"~\"")(str("~"))
    checkStat("\" \"")(str(" "))
    checkStat("\"=\"")(str("="))
    checkStat("\"\\u007f\"")(str("\u007f"))
    checkStat("\"\\u001f\"")(str("\u001f"))
    checkStat("\"\\\\\"")(str("\\"))
    checkStat("\"þæö\"")(str("\u00fe\u00e6\u00f6"))
    checkStat("\"ラーメン\"")(str("ラーメン"))
  }

  test("test repeat") {
    import Show.repeat
    assertEquals(repeat("<", ",", ">")("", "", "", "").toString, "")
    assertEquals(repeat("<", ",", ">")("x", "", "", "").toString, "<x>")
    assertEquals(repeat("<", ",", ">")("", "x", "", "").toString, "<x>")
    assertEquals(repeat("<", ",", ">")("", "", "x", "").toString, "<x>")
    assertEquals(repeat("<", ",", ">")("", "", "", "x").toString, "<x>")
    assertEquals(repeat("<", ",", ">")("x", "y", "", "").toString, "<x,y>")
    assertEquals(repeat("<", ",", ">")("x", "", "y", "").toString, "<x,y>")
    assertEquals(repeat("<", ",", ">")("x", "", "", "y").toString, "<x,y>")
    assertEquals(repeat("<", ",", ">")("", "x", "y", "").toString, "<x,y>")
    assertEquals(repeat("<", ",", ">")("", "x", "", "y").toString, "<x,y>")
    assertEquals(repeat("<", ",", ">")("", "", "x", "y").toString, "<x,y>")
  }

  test("backticked-keywords-scala3") {
    assertEquals(tname("enum").reprint, "enum")
    assertEquals(tname("given").reprint, "given")
    assertEquals(tname("export").reprint, "export")
    assertEquals(tname("then").reprint, "then")
    assertEquals(tname("?=>").reprint, "?=>")
  }

  test("#1063 good") {
    checkTree(term("foo(bar) { baz: _* }"))(Term.Apply(
      Term.Apply(tname("foo"), List(tname("bar"))),
      Term.Block(List(Term.Repeated(tname("baz")))) :: Nil
    ))
  }

  test("#1063 bad") {
    val thrown = intercept[ParseException](term("foo(bar) { val baz = qux; baz: _* }"))
    assertEquals(
      thrown.getMessage.substring(0, 52),
      "<input>:1: error: repeated argument not allowed here"
    )
  }

  test("#1384 char no unescaped LF") {
    val expr = "('\n')"
    val error = """|<input>:1: error: can't use unescaped LF in character literals
                   |('
                   |  ^""".stripMargin.lf2nl
    interceptMessage[ParseException](error)(super.term(expr))
  }

  test("#1384 char ok escaped LF") {
    val exprU = "'\\u000a'"
    val expr = s"('\\n', $exprU)"
    val syntax = "('\\n', '\\n')"
    val tree = super.term(expr)
    val charN = Lit.Char('\n')
    val origin = Origin.Parsed(new Origin.ParsedSource(Input.String(exprU)), 1, 2)
    val charU = charN.withOrigin(origin)
    checkTree(tree, expr)(Term.Tuple(List(charN, charU)))
    checkTree(tree.resetAllOrigins, syntax)(Term.Tuple(List(charN, charN)))
  }

  test("#2774 1") {
    val tree = tparam(Mod.Erased() :: Mod.Using() :: Nil, "foo", "Bar")
    assertSyntax(tree, "erased foo: Bar")(tree)
  }

  test("#2774 2") {
    val tree = Type.Function(
      List(Type.TypedParam(pname("e"), pname("Entry"), List(Mod.Erased()))),
      Type.Select(tname("e"), pname("Key"))
    )
    assertSyntax(tree, "(erased e: Entry) => e.Key")(tree)
  }

  test("#2774 3") {
    val tree = Type
      .Function(List(Type.FunctionArg(List(Mod.Erased()), pname("Ev")), pname("Int")), pname("Int"))
    assertSyntax(tree, "(erased Ev, Int) => Int")(tree)
  }

  test("#3065 embedded triple quotes") {
    val tree = str("\n\"\"\"")
    assertSyntax(tree, "\"\"\"\n\\\"\\\"\\\"\"\"\"")(tree)
  }

  test("#2046 new line separator required before some Term.Block") {
    assertEquals(
      blockStat("{{a}; val b = 1; val c = 2; {d}; {e}}").reprint,
      s"""|{
          |  {
          |    a
          |  }
          |  val b = 1
          |  val c = 2
          |
          |  {
          |    d
          |  }
          |  {
          |    e
          |  }
          |}""".stripMargin.lf2nl
    )
    assertEquals(
      blockStat("{if (a) while (b) for (c <- d) -e; {f}}").reprint,
      s"""|{
          |  if (a) while (b) for (c <- d) -e
          |
          |  {
          |    f
          |  }
          |}""".stripMargin.lf2nl
    )
  }

  test("#3610 Type.Block 1") {
    val tree = Type.Block(Nil, pname("Int"))
    assertEquals(tree.reprint, "Int")
  }

  test("#3610 Type.Block 2") {
    val tree = Type.Block(List(Decl.Type(Nil, pname("t"), Nil, noBounds)), pname("t"))
    assertEquals(tree.reprint, "type t; t")
  }

  test("#3610 Type.Block 3") {
    val tree = Type.Block(List(Defn.Type(Nil, pname("t"), Nil, pname("Int"))), pname("t"))
    assertEquals(tree.reprint, "type t = Int; t")
  }

}

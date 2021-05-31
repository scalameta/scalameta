package scala.meta.tests
package prettyprinters

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.trees._
import scala.meta.prettyprinters.Show

class SyntacticSuite extends scala.meta.tests.parsers.ParseSuite {
  override def term(code: String)(implicit dialect: Dialect) =
    super.term(code)(dialect).resetAllOrigins
  override def pat(code: String)(implicit dialect: Dialect) =
    super.pat(code)(dialect).resetAllOrigins
  override def tpe(code: String)(implicit dialect: Dialect) =
    super.tpe(code)(dialect).resetAllOrigins
  override def topStat(code: String)(implicit dialect: Dialect) =
    super.topStat(code)(dialect).resetAllOrigins
  override def templStat(code: String)(implicit dialect: Dialect) =
    super.templStat(code)(dialect).resetAllOrigins
  override def blockStat(code: String)(implicit dialect: Dialect) =
    super.blockStat(code)(dialect).resetAllOrigins
  override def caseClause(code: String)(implicit dialect: Dialect) =
    super.caseClause(code)(dialect).resetAllOrigins
  override def source(code: String)(implicit dialect: Dialect) =
    super.source(code)(dialect).resetAllOrigins
  implicit class XtensionResetOrigin[T <: Tree](tree: T) {
    // NOTE: Ensures that neither the given tree nor its subtrees have their origins set.
    // This is necessary to force prettyprinting as opposed to reusing original syntax.
    def resetAllOrigins: T = {
      tree.transform { case tree: Tree => tree.withOrigin(Origin.None) }.asInstanceOf[T]
    }
  }

  test("val x: Int (raw)") {
    val tree = templStat("val x: Int")
    assertEquals(
      tree.structure,
      "Decl.Val(Nil, List(Pat.Var(Term.Name(\"x\"))), Type.Name(\"Int\"))"
    )
  }

  test("val x: Int (code)") {
    val tree = templStat("val x: Int")
    assertEquals(tree.syntax, "val x: Int")
  }

  test("~(1 + 2) + ~x.y(z) + (~x).y(z)") {
    val tree = templStat("~(1 + 2) + ~x.y(z) + (~x).y(z)")
    assertEquals(tree.syntax, "~(1 + 2) + ~x.y(z) + (~x).y(z)")
  }

  test("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)") {
    val tree = templStat("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)")
    assertEquals(tree.syntax, "a + b + c && a + (b + c) && (a :: b :: c) && ((a :: b) :: c)")
  }

  test("(x map y).foo") {
    val tree = templStat("(x map y).foo")
    assertEquals(tree.syntax, "(x map y).foo")
  }

  test("multi-line string literals") {
    val tree = templStat("""{
      val x = QQQ
        x
      QQQ
    }""".replace("QQQ", "\"\"\""))

    assertSameLines(
      tree.syntax,
      """
    |{
    |  val x = QQQ
    |        x
    |      QQQ
    |}
    """.trim.stripMargin.replace("QQQ", "\"\"\"")
    )
  }

  test("string literals with newlines and double quotes") {
    val tree = templStat("""{
      val x = QQQ
        x
      QQQ
      val y = "\""
    }""".replace("QQQ", "\"\"\""))
    assertEquals(
      tree.structure,
      """Term.Block(List(Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.String("%n        x%n      ")), Defn.Val(Nil, List(Pat.Var(Term.Name("y"))), None, Lit.String("\""))))"""
        .replace("%n", "\\n")
    )
    assertSameLines(
      tree.syntax,
      """
    |{
    |  val x = QQQ
    |        x
    |      QQQ
    |  val y = "\""
    |}
    """.trim.stripMargin.replace("QQQ", "\"\"\"")
    )
  }

  test("interpolations") {
    val tree = templStat("""{
      val x = q"123 + $x + ${foo(123)} + 456"
      val y = QQQ
        $x
        $y
        ..$z
      QQQ
    }""".replace("QQQ", "\"\"\""))
    assertEquals(
      tree.structure,
      """Term.Block(List(Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Term.Interpolate(Term.Name("q"), List(Lit.String("123 + "), Lit.String(" + "), Lit.String(" + 456")), List(Term.Name("x"), Term.Apply(Term.Name("foo"), List(Lit.Int(123)))))), Defn.Val(Nil, List(Pat.Var(Term.Name("y"))), None, Lit.String("%n        $x%n        $y%n        ..$z%n      "))))"""
        .replace("%n", "\\n")
    )
    assertSameLines(
      tree.syntax,
      """
    |{
    |  val x = q"123 + $x + ${foo(123)} + 456"
    |  val y = QQQ
    |        $x
    |        $y
    |        ..$z
    |      QQQ
    |}
    """.trim.stripMargin.replace("QQQ", "\"\"\"")
    )
  }

  test("foo.bar(bar) { baz }") {
    val tree = templStat("foo.bar(bar) { baz }")
    assertEquals(
      tree.syntax,
      """
      |foo.bar(bar) {
      |  baz
      |}
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
  }

  test("Template.self stringifications") {
    assertEquals(templStat("new { val x = 2 }").syntax, "new { val x = 2 }")
    assertEquals(templStat("new { self => val x = 2 }").syntax, "new { self => val x = 2 }")
    assertEquals(
      templStat("new { self: Int => val x = 2 }").syntax,
      "new { self: Int => val x = 2 }"
    )
    assertEquals(
      templStat("""
      new {
        val x = 2
        val y = 3
      }
    """).syntax,
      """
      |new {
      |  val x = 2
      |  val y = 3
      |}
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
    assertEquals(
      templStat("""
      new { self =>
        val x = 2
        val y = 3
      }
    """).syntax,
      """
      |new { self =>
      |  val x = 2
      |  val y = 3
      |}
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
    assertEquals(
      templStat("""
      new { self: Int =>
        val x = 2
        val y = 3
      }
    """).syntax,
      """
      |new { self: Int =>
      |  val x = 2
      |  val y = 3
      |}
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
    assertEquals(templStat("class B { x: B => }").syntax, "class B { x: B => }")
  }

  test("new X") {
    assertEquals(templStat("new X").syntax, "new X")
    assertEquals(templStat("new X {}").syntax, "new X {}")
  }

  test("(new X).bar") {
    assertEquals(templStat("(new X).bar").syntax, "(new X).bar")
    assertEquals(templStat("new X {}.bar").syntax, "new X {}.bar")
    assertEquals(templStat("new X().bar").syntax, "new X().bar")
  }

  test("ascribe and annotate") {
    assertEquals(templStat("_: Int").syntax, "_: Int")
    assertEquals(templStat("(_: Int) + 2").syntax, "(_: Int) + 2")
    assertEquals(templStat("x: @foo").syntax, "x: @foo")
    assertEquals(templStat("(x: @foo) + 2").syntax, "(x: @foo) + 2")
  }

  test("compound types") {
    assertEquals(tpe("Foo").syntax, "Foo")
    assertEquals(tpe("Foo {}").syntax, "Foo {}")
    assertEquals(tpe("Foo { type T = Int }").syntax, "Foo { type T = Int }")
    assertEquals(
      tpe("Foo { type T = Int; type U <: String }").syntax,
      "Foo { type T = Int; type U <: String }"
    )
    assertEquals(tpe("Foo with Bar").syntax, "Foo with Bar")
    assertEquals(tpe("Foo with Bar {}").syntax, "Foo with Bar {}")
    assertEquals(tpe("Foo with Bar { type T = Int }").syntax, "Foo with Bar { type T = Int }")
    assertEquals(
      tpe("Foo with Bar { type T = Int; type U <: String }").syntax,
      "Foo with Bar { type T = Int; type U <: String }"
    )
  }

  test("infix types") {
    assertEquals(tpe("Foo + Bar").syntax, "Foo + Bar")
    assertEquals(tpe("Foo & Bar").syntax, "Foo & Bar")
    assertEquals(tpe("Foo | Bar").syntax, "Foo | Bar")
  }

  test("and types") {
    val Scala211 = null
    import scala.meta.dialects.Dotty
    assertEquals(tpe("Foo & Bar").syntax, "Foo & Bar")
  }

  test("or types") {
    val Scala211 = null
    import scala.meta.dialects.Dotty
    assertEquals(tpe("Foo | Bar").syntax, "Foo | Bar")
  }

  test("trait parameters") {
    val Scala211 = null
    import scala.meta.dialects.Dotty
    assertEquals(Dotty(q"trait T(a: Int)").syntax, "trait T(a: Int)")
  }

  test("literalTypes") {
    intercept[ParseException] {
      dialects.Scala211("val a : 42 = 42").parse[Stat].get.syntax
    }
    val Scala211 = null
    import scala.meta.dialects.Dotty
    assertEquals(q"val a: 42 = 42".syntax, "val a: 42 = 42")
    assertEquals(q"val a: 42L = 42L".syntax, "val a: 42L = 42L")
    assertEquals(q"val a: 42d = 42d".syntax, "val a: 42d = 42d")
    assertEquals(q"val a: 42.0d = 42.0d".syntax, "val a: 42.0d = 42.0d")
    assertEquals(q"val a: 42f = 42f".syntax, "val a: 42f = 42f")
    assertEquals(q"val a: 42.0f = 42.0f".syntax, "val a: 42.0f = 42.0f")
    assertEquals(q"val a: true = true".syntax, "val a: true = true")
    assertEquals(q"val a: false = false".syntax, "val a: false = false")
    assertEquals(
      dialects.Dotty("val a: \"42\" = \"42\"").parse[Stat].get.syntax,
      "val a: \"42\" = \"42\""
    )
    assertEquals(pat("_: 42").syntax, "_: 42")
    assertEquals(pat("_: 42f").syntax, "_: 42f")
    assertEquals(pat("_: 42d").syntax, "_: 42d")
    assertEquals(pat("_: 42.0f").syntax, "_: 42.0f")
    assertEquals(pat("_: 42.0d").syntax, "_: 42.0d")
    assertEquals(pat("_: 42L").syntax, "_: 42L")
    assertEquals(pat("_: true").syntax, "_: true")
    assertEquals(pat("_: false").syntax, "_: false")
  }

  test("packages") {
    assertEquals(source("package foo.bar; class C").syntax, s"package foo.bar${EOL}class C")
    assertEquals(
      source("package foo.bar; class C; class D").syntax,
      s"package foo.bar${EOL}class C${EOL}class D"
    )
    assertEquals(source("package foo.bar { class C }").syntax, s"package foo.bar${EOL}class C")
    assertEquals(
      source("package foo.bar { class C; class D }").syntax,
      s"package foo.bar${EOL}class C${EOL}class D"
    )
  }

  test("type parameter mods") {
    assertEquals(source("class C[@foo T]").syntax, "class C[@foo T]")
    assertEquals(source("class C[+T]").syntax, "class C[+T]")
    assertEquals(source("class C[@foo +T]").syntax, "class C[@foo +T]")
  }

  test("primary constructor mods") {
    assertEquals(source("class C").syntax, "class C")
    assertEquals(source("class C private").syntax, "class C private")
    assertEquals(source("class C @foo(x)").syntax, "class C @foo(x)")
    assertEquals(source("class C @foo(x) private").syntax, "class C @foo(x) private")
    assertEquals(source("class C(x: Int)").syntax, "class C(x: Int)")
    assertEquals(source("class C private (x: Int)").syntax, "class C private (x: Int)")
    assertEquals(source("class C @foo(x) (x: Int)").syntax, "class C @foo(x) (x: Int)")
    assertEquals(
      source("class C @foo(x) private (x: Int)").syntax,
      "class C @foo(x) private (x: Int)"
    )
  }

  test("parentheses in patterns") {
    assertEquals(
      templStat("x match { case (xs: List[Int]) :+ x => ??? }").syntax,
      """
      |x match {
      |  case (xs: List[Int]) :+ x => ???
      |}
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
  }

  test("List(x, y) :: z") {
    assertEquals(templStat("List(x, y) :: z").syntax, "List(x, y) :: z")
    assertEquals(
      templStat("x match { case List(x, y) :: z => ??? }").syntax,
      """
      |x match {
      |  case List(x, y) :: z => ???
      |}
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
  }

  test("secondary ctor - expr") {
    assertEquals(
      source("class C(x: Int) { def this() = this(2) }").syntax,
      "class C(x: Int) { def this() = this(2) }"
    )
  }

  test("secondary ctor - block") {
    assertEquals(
      source("class C(x: Int) { def this() { this(2); println(\"OBLIVION!!!\") } }").syntax,
      """
      |class C(x: Int) {
      |  def this() {
      |    this(2)
      |    println("OBLIVION!!!")
      |  }
      |}
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
  }

  test("case semicolons") {
    assertEquals(
      templStat("x match { case y => foo1; foo2 }").syntax,
      """
      |x match {
      |  case y =>
      |    foo1
      |    foo2
      |}
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
  }

  test("assorted literals") {
    assertEquals(templStat("true").syntax, "true")
    assertEquals(templStat("false").syntax, "false")
    assertEquals(templStat("0").syntax, "0")
    assertEquals(templStat("0l").syntax, "0L")
    assertEquals(templStat("0L").syntax, "0L")
    assertEquals(templStat("0f").syntax, "0f")
    assertEquals(templStat("0F").syntax, "0f")
    assertEquals(templStat("0.0f").syntax, "0.0f")
    assertEquals(templStat("0.0F").syntax, "0.0f")
    assertEquals(templStat("1.4f").syntax, "1.4f")
    assertEquals(templStat("1.40f").syntax, "1.40f")
    assertEquals(templStat("0.0").syntax, "0.0d")
    assertEquals(templStat("0d").syntax, "0d")
    assertEquals(templStat("0D").syntax, "0d")
    assertEquals(templStat("0.0d").syntax, "0.0d")
    assertEquals(templStat("0.0D").syntax, "0.0d")
    assertEquals(templStat("'0'").syntax, "'0'")
    assertEquals(templStat("\"0\"").syntax, "\"0\"")
    assertEquals(templStat("'zero").syntax, "'zero")
    assertEquals(templStat("null").syntax, "null")
    assertEquals(templStat("()").syntax, "()")
  }

  test("Lit.Double") {
    assertEquals(templStat("1.4d").structure, """Lit.Double(1.4d)""")
    assertEquals(templStat("1.40d").structure, """Lit.Double(1.40d)""")
    // NOTE: This fails under Scala Native:
    // [info] - Lit.Double *** FAILED ***
    // [info]   "Lit.Double(1.4[00000]d)" did not equal "Lit.Double(1.4[]d)" (SyntacticSuite.scala:321)
    // assertEquals(Lit.Double(1.40d).structure, "Lit.Double(1.4d)") // trailing 0 is lost
    // assertEquals(Lit.Double(1.4d).structure, "Lit.Double(1.4d)")
    // assertEquals(Lit.Double(1.40d).structure, "Lit.Double(1.4d)") // trailing 0 is lost
    assertEquals(Lit.Double(Double.NaN).syntax, "Double.NaN")
    assertEquals(Lit.Double(Double.PositiveInfinity).syntax, "Double.PositiveInfinity")
    assertEquals(Lit.Double(Double.NegativeInfinity).syntax, "Double.NegativeInfinity")
    assertEquals(Lit.Double(Double.NaN).structure, "Lit.Double(Double.NaN)")
    assertEquals(
      Lit.Double(Double.PositiveInfinity).structure,
      "Lit.Double(Double.PositiveInfinity)"
    )
    assertEquals(
      Lit.Double(Double.NegativeInfinity).structure,
      "Lit.Double(Double.NegativeInfinity)"
    )
  }

  test("Lit.Float") {
    assertEquals(templStat("1.4f").structure, """Lit.Float(1.4f)""")
    assertEquals(templStat("1.40f").structure, """Lit.Float(1.40f)""")
    assertEquals(Lit.Float(Float.NaN).syntax, "Float.NaN")
    assertEquals(Lit.Float(Float.PositiveInfinity).syntax, "Float.PositiveInfinity")
    assertEquals(Lit.Float(Float.NegativeInfinity).syntax, "Float.NegativeInfinity")
    assertEquals(Lit.Float(Float.NaN).structure, "Lit.Float(Float.NaN)")
    assertEquals(Lit.Float(Float.PositiveInfinity).structure, "Lit.Float(Float.PositiveInfinity)")
    assertEquals(Lit.Float(Float.NegativeInfinity).structure, "Lit.Float(Float.NegativeInfinity)")
  }

  test("context and view bounds") {
    assertEquals(templStat("class C[T: List, U <% Int]").syntax, "class C[T: List, U <% Int]")
    assertEquals(
      templStat("def m[T: List, U <% Int] = ???").syntax,
      "def m[T: List, U <% Int] = ???"
    )
  }

  test("some tricky parenthesization") {
    assertEquals(templStat("if (1) 2 else 3 + 4").syntax, "if (1) 2 else 3 + 4")
    assertEquals(templStat("(if (1) 2 else 3) + 4").syntax, "(if (1) 2 else 3) + 4")
    assertEquals(
      templStat("if (1) 2 else 3 match { case _ => }").syntax,
      s"if (1) 2 else 3 match {${EOL}  case _ =>${EOL}}"
    )
    assertEquals(
      templStat("(if (1) 2 else 3) match { case _ => }").syntax,
      s"(if (1) 2 else 3) match {${EOL}  case _ =>${EOL}}"
    )
    assertEquals(templStat("unit.toCheck += (() => body)").syntax, "unit.toCheck += (() => body)")
    assertEquals(
      templStat("({ foo1; foo2 }).orElse(bar)").syntax,
      s"{${EOL}  foo1${EOL}  foo2${EOL}}.orElse(bar)"
    )
    assertEquals(
      templStat("(foo match { case _ => }).orElse(bar)").syntax,
      s"(foo match {${EOL}  case _ =>${EOL}}).orElse(bar)"
    )
    assertEquals(
      templStat("foo || (if (cond) bar else baz)").syntax,
      "foo || (if (cond) bar else baz)"
    )
    assertEquals(
      templStat("foo && (bar match { case _ => })").syntax,
      s"foo && (bar match {${EOL}  case _ =>${EOL}})"
    )
    assertEquals(
      templStat("\"foo \" + (if (cond) bar else baz)").syntax,
      "\"foo \" + (if (cond) bar else baz)"
    )
    assertEquals(
      templStat("foo match { case bar @ (_: T1 | _: T2) => }").syntax,
      s"foo match {${EOL}  case bar @ (_: T1 | _: T2) =>${EOL}}"
    )
    assertEquals(
      templStat("foo match { case A + B / C => }").syntax,
      s"foo match {${EOL}  case A + B / C =>${EOL}}"
    )
    assertEquals(
      templStat("foo match { case (A + B) / C => }").syntax,
      s"foo match {${EOL}  case (A + B) / C =>${EOL}}"
    )
    assertEquals(
      templStat("foo match { case A + (B / C) => }").syntax,
      s"foo match {${EOL}  case A + B / C =>${EOL}}"
    )
    assertEquals(
      templStat("foo match { case bar :: Nil :: Nil => }").syntax,
      s"foo match {${EOL}  case bar :: Nil :: Nil =>${EOL}}"
    )
    assertEquals(
      templStat("foo match { case (bar :: Nil) :: Nil => }").syntax,
      s"foo match {${EOL}  case (bar :: Nil) :: Nil =>${EOL}}"
    )
    assertEquals(templStat("@(foo @foo) class Bar").syntax, "@(foo @foo) class Bar")
    assertEquals(templStat("(foo: Foo): @foo").syntax, "(foo: Foo): @foo")
    assertEquals(templStat("type T = A + B / C").syntax, "type T = A + B / C")
    assertEquals(templStat("type T = (A + B) / C").syntax, "type T = A + B / C")
    assertEquals(templStat("type T = A + (B / C)").syntax, "type T = A + (B / C)")
    assertEquals(templStat("type T = A :: B :: C").syntax, "type T = A :: B :: C")
    assertEquals(templStat("type T = (A :: B) :: C").syntax, "type T = (A :: B) :: C")
    assertEquals(
      templStat("foo match { case _: A | _: B => }").syntax,
      s"foo match {${EOL}  case _: A | _: B =>${EOL}}"
    )
    assertEquals(
      templStat("foo match { case _: A | _: B | _: C => }").syntax,
      s"foo match {${EOL}  case _: A | _: B | _: C =>${EOL}}"
    )
  }

  test("Type projections") {
    // Without lambda trick
    assertEquals(
      q"""class A { class B }
          type C = A#B
        """.syntax,
      """{
        |  class A { class B }
        |  type C = A#B
        |}""".stripMargin.split('\n').mkString(EOL)
    )
    // With lambda trick
    assertEquals(
      q"""
      def foo[F[_]]: Unit = ???
      foo[({ type T[A] = Either[Int, A] })#T]
        """.syntax,
      """{
        |  def foo[F[_]]: Unit = ???
        |  foo[({ type T[A] = Either[Int, A] })#T]
        |}""".stripMargin.split('\n').mkString(EOL)
    )
  }

  test("more trickiness") {
    assertEquals(templStat("def foo(bar_ : Int) = ???").syntax, "def foo(bar_ : Int) = ???")
    assertEquals(templStat("class C[T_ : Foo]").syntax, "class C[T_ : Foo]")
    assertEquals(templStat("val scala_ : NameType = ???").syntax, "val scala_ : NameType = ???")
  }

  test("class C extends (() => Int)") {
    assertEquals(templStat("class C extends (() => Int)").syntax, "class C extends (() => Int)")
  }

  test("package object e extends D") {
    assertEquals(topStat("package object e extends D").syntax, "package object e extends D")
    val q"package object $name $template" = q"package object e extends D"
    assertEquals(template.syntax, "extends D")
  }

  test("trait C extends A with B with D") {
    assertEquals(
      templStat("trait C extends A with B with D").syntax,
      "trait C extends A with B with D"
    )
    val q"trait $name $template" = q"trait C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("private sealed trait C extends A with B with D") {
    assertEquals(
      templStat("private sealed trait C extends A with B with D").syntax,
      "private sealed trait C extends A with B with D"
    )
    val q"private sealed trait $name $template" =
      q"private sealed trait C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("object C extends A with B with D") {
    assertEquals(
      templStat("object C extends A with B with D").syntax,
      "object C extends A with B with D"
    )
    val q"object $name $template" = q"object C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("private implicit object C extends A with B with D") {
    assertEquals(
      templStat("private implicit object C extends A with B with D").syntax,
      "private implicit object C extends A with B with D"
    )
    val q"private implicit object $name $template" =
      q"private implicit object C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("abstract class C extends A with B with D") {
    assertEquals(
      templStat(
        "abstract class C extends A with B with D"
      ).syntax,
      "abstract class C extends A with B with D"
    )
    val q"abstract class $name $template" = q"abstract class C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("protected abstract class C extends A with B with D") {
    assertEquals(
      templStat(
        "protected abstract class C extends A with B with D"
      ).syntax,
      "protected abstract class C extends A with B with D"
    )
    val q"protected abstract class $name $template" =
      q"protected abstract class C extends A with B with D"
    assertEquals(template.syntax, "extends A with B with D")
  }

  test("new C with A with B with D") {
    assertEquals(
      templStat(
        "new C with A with B with D"
      ).syntax,
      "new C with A with B with D"
    )
    val Term.NewAnonymous(template) = q"new C with A with B with D"
    assertEquals(template.syntax, "C with A with B with D")
  }

  test("class C(x: Int)(implicit y: String, z: Boolean)") {
    assertEquals(
      templStat("class C(x: Int)(implicit y: String, z: Boolean)").syntax,
      "class C(x: Int)(implicit y: String, z: Boolean)"
    )
  }

  test("#1837 class C(x: Int, implicit val|var y: String)") {
    assertEquals(
      templStat("class C(x: Int, implicit val y: String)").syntax,
      "class C(x: Int, implicit val y: String)"
    )
    assertEquals(
      templStat("class C(x: Int, implicit var y: String)").syntax,
      "class C(x: Int, implicit var y: String)"
    )
  }

  test("#1837 class C(<keyword> implicit val x: Int, y: String)(implicit z: Boolean)") {
    def checkSyntax(stat: String) = assertEquals(templStat(stat).syntax, stat)

    checkSyntax("class C(private implicit val x: Int, y: String)(implicit z: Boolean)")
    checkSyntax("class C(protected implicit val x: Int, y: String)(implicit z: Boolean)")
    checkSyntax("class C(final implicit val x: Int, y: String)(implicit z: Boolean)")
    checkSyntax("class C(override implicit val x: Int, y: String)(implicit z: Boolean) extends T")
  }

  test(
    "#1837 class C(private implicit val x: Int, implicit final val y: String, protected implicit var z: Boolean)"
  ) {
    assertEquals(
      templStat(
        "class C(private implicit val x: Int, implicit final val y: String, protected implicit var z: Boolean)"
      ).syntax,
      "class C(private implicit val x: Int, final val y: String, protected var z: Boolean)"
    )
  }

  test("class C(var x: Int)") {
    assertEquals(templStat("class C(var x: Int)").syntax, "class C(var x: Int)")
  }

  test("private/protected within something") {
    assertEquals(
      templStat("""
      class C {
        private[this] val x = 1
        private[D] val y = 2
        protected[this] val z = 3
        protected[D] val w = 4
      }
    """).syntax,
      """
        |class C {
        |  private[this] val x = 1
        |  private[D] val y = 2
        |  protected[this] val z = 3
        |  protected[D] val w = 4
        |}
    """.stripMargin.trim.split('\n').mkString(EOL)
    )
  }

  test("case List(xs @ _*)") {
    val tree = pat("List(xs @ _*)")
    assertEquals(
      tree.structure,
      "Pat.Extract(Term.Name(\"List\"), List(Pat.Bind(Pat.Var(Term.Name(\"xs\")), Pat.SeqWildcard())))"
    )
    assertEquals(tree.syntax, "List(xs @ _*)")
  }

  test("case List[t](xs @ _*)") {
    val tree = pat("List[t](xs @ _*)")
    assertEquals(
      tree.structure,
      "Pat.Extract(Term.ApplyType(Term.Name(\"List\"), List(Type.Var(Type.Name(\"t\")))), List(Pat.Bind(Pat.Var(Term.Name(\"xs\")), Pat.SeqWildcard())))"
    )
    assertEquals(tree.syntax, "List[t](xs @ _*)")
  }

  test("case List[_](xs @ _*)") {
    val tree = pat("List[_](xs @ _*)")
    assertEquals(
      tree.structure,
      "Pat.Extract(Term.ApplyType(Term.Name(\"List\"), List(Type.Placeholder(Type.Bounds(None, None)))), List(Pat.Bind(Pat.Var(Term.Name(\"xs\")), Pat.SeqWildcard())))"
    )
    assertEquals(tree.syntax, "List[_](xs @ _*)")
  }

  test("package foo; class C; package baz { class D }") {
    val tree = source("package foo; class C; package baz { class D }")
    assertNoDiff(
      tree.structure,
      """Source(List(Pkg(Term.Name("foo"), List(Defn.Class(Nil, Type.Name("C"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Self(Name(""), None), Nil, Nil)), Pkg(Term.Name("baz"), List(Defn.Class(Nil, Type.Name("D"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Self(Name(""), None), Nil, Nil))))))))"""
    )
    assertEquals(tree.syntax, s"package foo${EOL}class C${EOL}package baz {$EOL  class D${EOL}}")
  }

  test("case `x`") {
    val tree1 = pat("`x`")
    assertEquals(tree1.structure, "Term.Name(\"x\")")
    val tree2 = pat("f(`x`)")
    assertEquals(tree2.structure, "Pat.Extract(Term.Name(\"f\"), List(Term.Name(\"x\")))")
    assertEquals(tree2.syntax, "f(`x`)")
    val tree3 = pat("X")
    assertEquals(tree3.structure, "Term.Name(\"X\")")
    assertEquals(tree3.syntax, "X")
    val tree4 = pat("f(X)")
    assertEquals(tree4.structure, "Pat.Extract(Term.Name(\"f\"), List(Term.Name(\"X\")))")
    assertEquals(tree4.syntax, "f(X)")
  }

  test("case _: Int") {
    assertEquals(pat("_: Int").syntax, "_: Int")
  }

  test("case _: t") {
    assertEquals(pat("_: t").syntax, "_: t")
  }

  test("case _: F[t]") {
    assertEquals(pat("_: F[t]").syntax, "_: F[t]")
  }

  test("case _: F[_]") {
    assertEquals(pat("_: F[_]").syntax, "_: F[_]")
  }

  test("constructors") {
    val tree @ Defn.Class(_, _, _, primary, Template(_, _, _, List(secondary))) =
      templStat("class C(x: Int) { def this() = this(42) }")
    assertEquals(tree.syntax, "class C(x: Int) { def this() = this(42) }")
    assertEquals(primary.syntax, "(x: Int)")
    assertEquals(secondary.syntax, "def this() = this(42)")
    assertEquals(tree.toString, "class C(x: Int) { def this() = this(42) }")
    assertEquals(primary.toString, "def this(x: Int)")
    assertEquals(secondary.toString, "def this() = this(42)")
  }

  test("smart case printing - oneliner in one line") {
    val Term.Match(_, case1 :: Nil) = templStat("??? match { case x => x }")
    assertEquals(case1.toString, "case x => x")
  }

  test("smart case printing - oneliner in multiple lines") {
    val Term.Match(_, case1 :: case2 :: Nil) =
      templStat("??? match { case x => x; case List(x, y) => println(x); println(y) }")
    assertSameLines(
      case1.toString,
      """
      |case x =>
      |  x
    """.trim.stripMargin
    )
    assertEquals(
      case2.toString,
      """
      |case List(x, y) =>
      |  println(x)
      |  println(y)
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
  }

  test("xml literals") {
    val tree = term("<foo>{bar}</foo>")
    assertEquals(
      tree.structure,
      """Term.Xml(List(Lit.String("<foo>"), Lit.String("</foo>")), List(Term.Name("bar")))"""
    )
    assertEquals(tree.syntax, "<foo>{bar}</foo>")
  }

  test("xml literals unit") {
    val tree = term("<foo>{}</foo>")
    assertEquals(
      tree.structure,
      """Term.Xml(List(Lit.String("<foo>"), Lit.String("</foo>")), List(Term.Block(Nil)))"""
    )
    assertEquals(tree.syntax, "<foo>{{}}</foo>")
  }

  test("xml literals: pattern position") {
    assertEquals(pat("<a>{_*}</a>").show[Syntax], "<a>{_*}</a>")
    assertEquals(pat("<a>{ns @ _*}</a>").show[Syntax], "<a>{ns @ _*}</a>")
    assertEquals(pat("<a><b/>{ns @ _*}</a>").show[Syntax], "<a><b/>{ns @ _*}</a>")
  }

  test("interpolator unit") {
    val tree = term("""s"Hello${}World"""")
    assertEquals(
      tree.structure,
      """Term.Interpolate(Term.Name("s"), List(Lit.String("Hello"), Lit.String("World")), List(Term.Block(Nil)))"""
    )
    assertEquals(tree.syntax, """s"Hello${{}}World"""")
  }

  test("interpolator with $ character") {
    val tree = term("""s"$$$foo$$"""")
    assertEquals(
      tree.structure,
      """Term.Interpolate(Term.Name("s"), List(Lit.String("$"), Lit.String("$")), List(Term.Name("foo")))"""
    )
    assertEquals(tree.syntax, """s"$$$foo$$"""")
  }

  test("interpolator braces for operator identifiers") {
    assertEquals(q"""s"$${+++}bar"""".syntax, """s"$+++bar"""")
    assertEquals(q"""s"$${+++}_bar"""".syntax, """s"$+++_bar"""")
    assertEquals(q"""s"$${+++}123"""".syntax, """s"$+++123"""")
    assertEquals(q"""s"$${+++}***"""".syntax, """s"${+++}***"""")
    assertEquals(q"""s"$${+++} ***"""".syntax, """s"$+++ ***"""")
  }

  test("interpolator braces for plain identifiers") {
    assertEquals(q"""s"$${foo}bar"""".syntax, """s"${foo}bar"""")
    assertEquals(q"""s"$${foo}_bar"""".syntax, """s"${foo}_bar"""")
    assertEquals(q"""s"$${foo}123"""".syntax, """s"${foo}123"""")
    assertEquals(q"""s"$${foo}***"""".syntax, """s"$foo***"""")
    assertEquals(q"""s"$${foo} ***"""".syntax, """s"$foo ***"""")
  }

  test("interpolator braces for term names beginning with '_'") {
    def interpolate(before: String, after: String): Term.Interpolate =
      Term.Interpolate(
        prefix = Term.Name("s"),
        parts = List(Lit.String(before), Lit.String(after)),
        args = List(Term.Name("_foo"))
      )
    assertEquals(interpolate("", "").syntax, """s"${_foo}"""")
    assertEquals(interpolate("bar", "baz").syntax, """s"bar${_foo}baz"""")
    assertEquals(interpolate("[", "]").syntax, """s"[${_foo}]"""")
  }

  test("empty-arglist application") {
    val tree = term("foo.toString()")
    assertEquals(
      tree.structure,
      "Term.Apply(Term.Select(Term.Name(\"foo\"), Term.Name(\"toString\")), Nil)"
    )
    assertEquals(tree.syntax, "foo.toString()")
  }

  test("type parameters with type bounds") {
    val Defn.Def(_, _, List(tree), _, _, _) = templStat("def foo[T <: Int] = ???")
    assertEquals(
      tree.structure,
      "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, Some(Type.Name(\"Int\"))), Nil, Nil)"
    )
    assertEquals(tree.syntax, "T <: Int")
  }

  test("Lit(()) - 1") {
    val lit @ Lit(()) = term("()")
    assertEquals(lit.structure, "Lit.Unit(())")
    assertEquals(lit.syntax, "()")
  }

  test("Lit(()) - 2") {
    val Term.If(Term.Name("cond"), Lit(42), lit @ Lit.Unit()) = super.term("if (cond) 42")
    assertEquals(lit.structure, "Lit.Unit(())")
    assertEquals(lit.syntax, "")
  }

  test("Type.Function(Tuple, _) #557") {
    assertEquals(t"((a, b)) => c".syntax, "((a, b)) => c")
    assertEquals(t"((a, b), c) => c".syntax, "((a, b), c) => c")
  }

  test("Term.Apply(_, List(Term.Function(...))) #572, #574") {
    import scala.collection.immutable.List
    val tree1 = Term.Apply(
      Term.Name("foo"),
      List(
        Term.Function(
          List(Term.Param(List(), Term.Name("i"), Some(Type.Name("Int")), None)),
          Lit.Unit()
        )
      )
    )
    val tree2 = Term.Apply(
      Term.Name("foo"),
      List(
        Term.Function(
          List(Term.Param(List(Mod.Implicit()), Term.Name("i"), Some(Type.Name("Int")), None)),
          Lit.Unit()
        )
      )
    )
    val tree3 = Term.Apply(
      Term.Name("foo"),
      List(Term.Function(List(Term.Param(List(), Term.Name("i"), None, None)), Lit.Unit()))
    )
    assertEquals(tree1.syntax, "foo { (i: Int) => () }")
    assertEquals(tree2.syntax, "foo { implicit i: Int => () }")
    assertEquals(tree3.syntax, "foo(i => ())")
  }

  test("macro defs #581") {
    assertEquals(q"def f = macro g".syntax, "def f = macro g")
    assertEquals(q"def f: Int = macro g".syntax, "def f: Int = macro g")
  }

  test("Pat.Interpolate syntax is correct #587") {
    val interpolate = Pat.Interpolate(
      Term.Name("q"),
      List(Lit.String("object "), Lit.String(" { .."), Lit.String(" }")),
      List(Pat.Var(Term.Name("name")), Pat.Var(Term.Name("stats")))
    )
    assertEquals(interpolate.syntax, """q"object ${name} { ..${stats} }"""")
  }

  test("Importee.Rename") {
    assertEquals(q"import a.{b=>c}".syntax, "import a.{b => c}")
  }

  test("backquote importees when needed - scalafix #1337") {
    assertEquals(q"import a.`{ q }`".syntax, "import a.`{ q }`")
    assertEquals(q"import a.`macro`".syntax, "import a.`macro`")
  }

  test("show[Structure] should uppercase long literals suffix: '2l' -> '2L'") {
    assertEquals(
      templStat("foo(1l, 1L)").structure,
      """Term.Apply(Term.Name("foo"), List(Lit.Long(1L), Lit.Long(1L)))"""
    )
    assertEquals(q"val x = 1l".structure, q"val x = 1L".structure)
  }

  test("show[Structure] should lowercase float literals suffix: '0.01F' -> '0.01f'") {
    assertEquals(
      templStat("foo(0.01f, 0.01F)").structure,
      """Term.Apply(Term.Name("foo"), List(Lit.Float(0.01f), Lit.Float(0.01f)))"""
    )
    assertEquals(q"val x = 1f".structure, q"val x = 1F".structure)
  }

  test("show[Structure] should lowercase double literals suffix: '0.01D' -> '0.01d'") {
    assertEquals(
      templStat("foo(0.02d, 0.02D, 0.02)").structure,
      """Term.Apply(Term.Name("foo"), List(Lit.Double(0.02d), Lit.Double(0.02d), Lit.Double(0.02d)))"""
    )
    assertEquals(q"val x = 1d".structure, q"val x = 1D".structure)
    assertEquals(q"val x = 1.0d".structure, q"val x = 1.0".structure)
  }

  test("#931 val `a b` = 2") {
    assertEquals(q"val `a b` = 2".syntax, "val `a b` = 2")
  }

  test("#2097 val `macro` = 42") {
    assertEquals(q"val `macro` = 42".syntax, "val `macro` = 42")
  }

  test("#1661 Names outside ") {
    // Must start with either a letter or an operator
    assertEquals(q"val `foo` = 2".syntax, "val foo = 2")
    assertEquals(q"val `++++` = 2".syntax, "val ++++ = 2")
    assertEquals(q"val `_+` = 2".syntax, "val `_+` = 2")

    // Non-leading operators are accepted only after underscores
    assertEquals(q"val `a_+` = 2".syntax, "val a_+ = 2")
    assertEquals(q"val `a_a_+` = 2".syntax, "val a_a_+ = 2")

    // Operators must not be followed by non-operators
    assertEquals(q"val `+_a` = 2".syntax, "val `+_a` = 2")
    assertEquals(q"val `a_++` = 2".syntax, "val a_++ = 2")
    assertEquals(q"val `a_++a` = 2".syntax, "val `a_++a` = 2")

    // Lexical letters and digits can follow underscores
    assertEquals(q"val `_a` = 2".syntax, "val _a = 2")
    assertEquals(q"val `a_a` = 2".syntax, "val a_a = 2")

    // Non-operators must not be followed by operators
    assertEquals(q"val `a+` = 2".syntax, "val `a+` = 2")
    assertEquals(q"val `a-b` = 2".syntax, "val `a-b` = 2")
    assertEquals(q"val `a:b` = 2".syntax, "val `a:b` = 2")

    // Comments must be handled carefully
    assertEquals(q"val `/*` = 2".syntax, "val `/*` = 2")
    assertEquals(q"val `//` = 2".syntax, "val `//` = 2")
    assertEquals(q"val `a_//` = 2".syntax, "val `a_//` = 2")
  }

  test("#1817 ApplyInfix parentheses") {
    checkTree(q"list map (println)", "list map println")
    checkTree(q"list map (add(1))", "list map add(1)")
    checkTree(q"list map (add(_, 1))", "list map (add(_, 1))")
    checkTree(q"list map (bar:_*)", "list map (bar: _*)")
  }
  test("#1826 ApplyInfix parentheses on Select") {
    checkTree(q"list map (_.bar)", "list map (_.bar)")
    checkTree(q"list map (Foo.bar)", "list map Foo.bar")
  }
  test("1826 ApplyInfix parentheses on multiple Select") {
    checkTree(q"list map (_.foo.bar)", "list map (_.foo.bar)")
  }
  test("#1826 ApplyInfix parentheses on tuple") {
    checkTree(q"list map ((_, foo))", "list map ((_, foo))")
  }
  test("#1826 ApplyInfix parentheses on Apply") {
    checkTree(q"list map (_.->(foo))", "list map (_.->(foo))")
    checkTree(q"list map a.->(foo)", "list map a.->(foo)")
    checkTree(q"list map (_.diff(foo))", "list map (_.diff(foo))")
    checkTree(q"list map (_.diff.bar(foo))", "list map (_.diff.bar(foo))")
    checkTree(q"list map a.diff(foo)", "list map a.diff(foo)")
    checkTree(q"list map a.diff.bar(foo)", "list map a.diff.bar(foo)")
  }
  test("#1826 ApplyInfix parentheses on Function") {
    checkTree(q"list map (_ => foo)", "list map (_ => foo)")
  }
  test("#1826 ApplyInfix parentheses on ApplyInfix function") {
    checkTree(q"list map (_ diff foo)", "list map (_ diff foo)")
    // 'diff' has same precedence as 'map', so parentheses should be added
    checkTree(q"list map (a diff foo)", "list map (a diff foo)")
  }
  test("#1826 ApplyInfix parentheses on ApplyInfix operator") {
    checkTree(q"list map (_ -> foo)", "list map (_ -> foo)")
    // '->' has greater precendence than 'map', so parentheses are not needed
    checkTree(q"list map (a -> foo)", "list map a -> foo")
  }
  test("1826 ApplyInfix parentheses on Term.Match") {
    checkTree(
      q"list map (_ match { case 1 => 2})",
      s"list map (_ match {${EOL}  case 1 => 2${EOL}})"
    )
  }

  test("#1839 ApplyInfix parentheses on Term.Placeholder") {
    checkTree(q"list reduce (_ + _)", "list reduce (_ + _)")
    checkTree(q"list reduce (_ + (_))", "list reduce (_ + _)")
    checkTree(q"list reduce (_.foo + _.bar)", "list reduce (_.foo + _.bar)")
    checkTree(q"list reduce (_.a.b.c + _.d.e.f)", "list reduce (_.a.b.c + _.d.e.f)")
    checkTree(q"list reduce (_.a(foo) + _.b(bar))", "list reduce (_.a(foo) + _.b(bar))")
  }

  test("#1864 Terms with leading numerics are backquoted") {
    checkTree(Term.Name("123foo"), "`123foo`")
    checkTree(q""" val `123foo` = "hello" """, """val `123foo` = "hello"""")
  }

  test("#1868 Term.Eta preserves structure") {
    checkTree(Term.Select(Term.Eta(Term.Name("x")), Term.Name("y")), "(x _).y")
    checkTree(q"""(x _).y""", "(x _).y")
    checkTree(Term.Eta(Term.Name("x")), "x _")
    checkTree(q"""x _""", "x _")
  }

  test("#2106 unicode characters are properly escaped in string literals") {
    checkTree(Lit.String("xy\u001az"), "\"xy\\u001az\"")
    checkTree(Lit.String("þæö"), "\"þæö\"")
    checkTree(Lit.String(">"), "\">\"")
    checkTree(Lit.String("~"), "\"~\"")
    checkTree(Lit.String(" "), "\" \"")
    checkTree(Lit.String("="), "\"=\"")
    checkTree(Lit.String("\u007f"), "\"\\u007f\"")
    checkTree(Lit.String("\u001f"), "\"\\u001f\"")
    checkTree(Lit.String("\\"), "\"\\\\\"")
    checkTree(Lit.String("\u00fe\u00e6\u00f6"), "\"þæö\"")
    checkTree(Lit.String("ラーメン"), "\"ラーメン\"")
  }

  def checkTree(original: Tree, expected: String)(implicit loc: munit.Location): Unit = {
    assertNoDiff(original.syntax, expected)
    assertNoDiff(original.structure, expected.parse[Stat].get.structure)
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
    assertEquals(
      Term.Name("enum").syntax,
      "enum"
    )
    assertEquals(
      Term.Name("given").syntax,
      "given"
    )
    assertEquals(
      Term.Name("export").syntax,
      "export"
    )
    assertEquals(
      Term.Name("then").syntax,
      "then"
    )
    assertEquals(
      Term.Name("?=>").syntax,
      "?=>"
    )
  }
}

package scala.meta.tests
package prettyprinters

import scala.meta._
import scala.meta.internal.tokens._
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
  override def parseCase(code: String)(implicit dialect: Dialect) =
    super.parseCase(code)(dialect).resetAllOrigins
  override def source(code: String)(implicit dialect: Dialect) =
    super.source(code)(dialect).resetAllOrigins
  implicit class XtensionResetOrigin[T <: Tree](tree: T) {
    // NOTE: Ensures that neither the given tree nor its subtrees have their origins set.
    // This is necessary to force prettyprinting as opposed to reusing original syntax.
    def resetAllOrigins: T = {
      tree.transform { case tree: Tree => tree.withOrigin(Origin.None) }.asInstanceOf[T]
    }
  }

  import scala.meta.dialects.Scala211

  test("val x: Int (raw)") {
    val tree = templStat("val x: Int")
    assertTree(tree)(Decl.Val(Nil, List(Pat.Var(Term.Name("x"))), Type.Name("Int")))
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
    assertTree(tree)(
      Term.Block(
        List(
          Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.String("\n        x\n      ")),
          Defn.Val(Nil, List(Pat.Var(Term.Name("y"))), None, Lit.String("\""))
        )
      )
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
    assertTree(tree)(
      Term.Block(
        List(
          Defn.Val(
            Nil,
            List(Pat.Var(Term.Name("x"))),
            None,
            Term.Interpolate(
              Term.Name("q"),
              List(Lit.String("123 + "), Lit.String(" + "), Lit.String(" + 456")),
              List(Term.Name("x"), Term.Apply(Term.Name("foo"), List(Lit.Int(123))))
            )
          ),
          Defn.Val(
            Nil,
            List(Pat.Var(Term.Name("y"))),
            None,
            Lit.String("\n        $x\n        $y\n        ..$z\n      ")
          )
        )
      )
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
    import dialects.Scala3
    assertEquals(tpe("Foo & Bar").syntax, "Foo & Bar")
  }

  test("or types") {
    val Scala211 = null
    import dialects.Scala3
    assertEquals(tpe("Foo | Bar").syntax, "Foo | Bar")
  }

  test("trait parameters") {
    val Scala211 = null
    import dialects.Scala3
    assertEquals(Scala3(q"trait T(a: Int)").syntax, "trait T(a: Int)")
  }

  test("literalTypes") {
    intercept[ParseException] {
      dialects.Scala211("val a : 42 = 42").parse[Stat].get.syntax
    }
    val Scala211 = null
    import dialects.Scala3
    assertEquals(q"val a: 42 = 42".syntax, "val a: 42 = 42")
    assertEquals(q"val a: 42L = 42L".syntax, "val a: 42L = 42L")
    assertEquals(q"val a: 42d = 42d".syntax, "val a: 42d = 42d")
    assertEquals(q"val a: 42.0d = 42.0d".syntax, "val a: 42.0d = 42.0d")
    assertEquals(q"val a: 42f = 42f".syntax, "val a: 42f = 42f")
    assertEquals(q"val a: 42.0f = 42.0f".syntax, "val a: 42.0f = 42.0f")
    assertEquals(q"val a: true = true".syntax, "val a: true = true")
    assertEquals(q"val a: false = false".syntax, "val a: false = false")
    assertEquals(
      dialects.Scala3("val a: \"42\" = \"42\"").parse[Stat].get.syntax,
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

  test("type with a literal type param (#2725)") {
    val Scala211 = null
    import dialects.Scala3
    assertEquals(t"Foo[42]".syntax, "Foo[42]")
    assertEquals(t"Foo @@ 42".syntax, "Foo @@ 42")
    assertEquals(t"Foo[true]".syntax, "Foo[true]")
    assertEquals(q"val x = new Foo[42]".syntax, "val x = new Foo[42]")
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
    assertTree(templStat("1.4d"))(Lit.Double("1.4"))
    assertTree(templStat("1.40d"))(Lit.Double("1.40"))
    // NOTE: This fails under Scala Native:
    // [info] - Lit.Double *** FAILED ***
    // [info]   "Lit.Double(1.4[00000]d)" did not equal "Lit.Double(1.4[]d)" (SyntacticSuite.scala:321)
    // assertEquals(Lit.Double(1.40d).structure, "Lit.Double(1.4d)") // trailing 0 is lost
    // assertEquals(Lit.Double(1.4d).structure, "Lit.Double(1.4d)")
    // assertEquals(Lit.Double(1.40d).structure, "Lit.Double(1.4d)") // trailing 0 is lost
    assertEquals(Lit.Double(Double.NaN).syntax, "Double.NaN")
    assertEquals(Lit.Double(Double.PositiveInfinity).syntax, "Double.PositiveInfinity")
    assertEquals(Lit.Double(Double.NegativeInfinity).syntax, "Double.NegativeInfinity")
    assertTree(Lit.Double(Double.NaN))(Lit.Double(Double.NaN))
    assertTree(Lit.Double(Double.PositiveInfinity))(Lit.Double(Double.PositiveInfinity))
    assertTree(Lit.Double(Double.NegativeInfinity))(Lit.Double(Double.NegativeInfinity))
  }

  test("Lit.Float") {
    assertTree(templStat("1.4f"))(Lit.Float("1.4"))
    assertTree(templStat("1.40f"))(Lit.Float("1.40"))
    assertEquals(Lit.Float(Float.NaN).syntax, "Float.NaN")
    assertEquals(Lit.Float(Float.PositiveInfinity).syntax, "Float.PositiveInfinity")
    assertEquals(Lit.Float(Float.NegativeInfinity).syntax, "Float.NegativeInfinity")
    assertTree(Lit.Float(Float.NaN))(Lit.Float(Float.NaN))
    assertTree(Lit.Float(Float.PositiveInfinity))(Lit.Float(Float.PositiveInfinity))
    assertTree(Lit.Float(Float.NegativeInfinity))(Lit.Float(Float.NegativeInfinity))
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
    assertEquals(templStat("if (1) (if (2) 3) else 4").syntax, "if (1) if (2) 3 else 4")
    assertEquals(templStat("if (1) if (2) 3 else () else 4").syntax, "if (1) if (2) 3 else 4")
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

  test("class C(implicit override val x: Int, final implicit var y: String)") {
    checkTree(
      templStat("class C(implicit override val x: Int, final implicit var y: String)"),
      "class C(implicit override val x: Int, final var y: String)"
    ) {
      Defn.Class(
        Nil,
        Type.Name("C"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          Name(""),
          List(
            Term.ParamClause(
              List(
                Term.Param(
                  List(Mod.Override(), Mod.Implicit(), Mod.ValParam()),
                  Term.Name("x"),
                  Some(Type.Name("Int")),
                  None
                ),
                Term.Param(
                  List(Mod.Final(), Mod.Implicit(), Mod.VarParam()),
                  Term.Name("y"),
                  Some(Type.Name("String")),
                  None
                )
              ),
              Some(Mod.Implicit())
            )
          )
        ),
        Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
      )
    }
  }

  test(
    "#1837 class C(private implicit val x: Int, implicit final val y: String, protected implicit var z: Boolean)"
  ) {
    checkTree(
      templStat(
        "class C(private implicit val x: Int, implicit final val y: String, protected implicit var z: Boolean)"
      ),
      "class C(implicit private val x: Int, final val y: String, protected var z: Boolean)"
    ) {
      Defn.Class(
        Nil,
        Type.Name("C"),
        Type.ParamClause(Nil),
        Ctor.Primary(
          Nil,
          Name(""),
          Term.ParamClause(
            Term.Param(
              List(Mod.Private(Name("")), Mod.Implicit(), Mod.ValParam()),
              Term.Name("x"),
              Some(Type.Name("Int")),
              None
            ) :: Term.Param(
              List(Mod.Implicit(), Mod.Final(), Mod.ValParam()),
              Term.Name("y"),
              Some(Type.Name("String")),
              None
            ) :: Term.Param(
              List(Mod.Protected(Name("")), Mod.Implicit(), Mod.VarParam()),
              Term.Name("z"),
              Some(Type.Name("Boolean")),
              None
            ) :: Nil,
            Some(Mod.Implicit())
          ) :: Nil
        ),
        Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
      )
    }
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
    assertTree(tree)(
      Pat.Extract(Term.Name("List"), List(Pat.Bind(Pat.Var(Term.Name("xs")), Pat.SeqWildcard())))
    )
    assertEquals(tree.syntax, "List(xs @ _*)")
  }

  test("case List[t](xs @ _*)") {
    val tree = pat("List[t](xs @ _*)")
    assertTree(tree)(
      Pat.Extract(
        Term.ApplyType(Term.Name("List"), List(Type.Var(Type.Name("t")))),
        List(Pat.Bind(Pat.Var(Term.Name("xs")), Pat.SeqWildcard()))
      )
    )
    assertEquals(tree.syntax, "List[t](xs @ _*)")
  }

  test("case List[_](xs @ _*)") {
    val tree = pat("List[_](xs @ _*)")
    assertTree(tree)(
      Pat.Extract(
        Term.ApplyType(Term.Name("List"), List(Type.Wildcard(Type.Bounds(None, None)))),
        List(Pat.Bind(Pat.Var(Term.Name("xs")), Pat.SeqWildcard()))
      )
    )
    assertEquals(tree.syntax, "List[_](xs @ _*)")
  }

  test("case List[_](xs @ _*): scala31") {
    implicit val Scala211 = dialects.Scala31
    val tree = pat("List[_](xs @ _*)")
    assertTree(tree)(
      Pat.Extract(
        Term.ApplyType(Term.Name("List"), List(Type.Wildcard(Type.Bounds(None, None)))),
        List(Pat.Bind(Pat.Var(Term.Name("xs")), Pat.SeqWildcard()))
      )
    )
    assertEquals(tree.syntax, "List[_](xs @ _*)")
  }

  test("case List[_](xs @ _*): scala3future") {
    implicit val Scala211 = dialects.Scala3Future
    val tree = pat("List[_](xs @ _*)")
    assertTree(tree)(
      Pat.Extract(
        Term.ApplyType(Term.Name("List"), List(Type.AnonymousParam(None))),
        List(Pat.Bind(Pat.Var(Term.Name("xs")), Pat.SeqWildcard()))
      )
    )
    assertEquals(tree.syntax, "List[_](xs @ _*)")
  }

  test("package foo; class C; package baz { class D }") {
    val tree = source("package foo; class C; package baz { class D }")
    assertTree(tree)(
      Source(
        List(
          Pkg(
            Term.Name("foo"),
            List(
              Defn.Class(
                Nil,
                Type.Name("C"),
                Nil,
                Ctor.Primary(Nil, Name(""), Nil),
                Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
              ),
              Pkg(
                Term.Name("baz"),
                List(
                  Defn.Class(
                    Nil,
                    Type.Name("D"),
                    Nil,
                    Ctor.Primary(Nil, Name(""), Nil),
                    Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
                  )
                )
              )
            )
          )
        )
      )
    )
    assertEquals(tree.syntax, s"package foo${EOL}class C${EOL}package baz {$EOL  class D${EOL}}")
  }

  test("ammonite: package foo; class C; package baz { class D }") {
    val code = """
                 |package foo1; class C1; package baz1 { class D1 }
                 |@
                 |package foo2; class C2; package baz2 { class D2 }
                 |""".stripMargin
    val tree = ammonite(code)
    assertTree(tree)(
      MultiSource(
        List(
          Source(
            List(
              Pkg(
                Term.Name("foo1"),
                List(
                  Defn.Class(
                    Nil,
                    Type.Name("C1"),
                    Nil,
                    Ctor.Primary(Nil, Name(""), Nil),
                    Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
                  ),
                  Pkg(
                    Term.Name("baz1"),
                    List(
                      Defn.Class(
                        Nil,
                        Type.Name("D1"),
                        Nil,
                        Ctor.Primary(Nil, Name(""), Nil),
                        Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
                      )
                    )
                  )
                )
              )
            )
          ),
          Source(
            List(
              Pkg(
                Term.Name("foo2"),
                List(
                  Defn.Class(
                    Nil,
                    Type.Name("C2"),
                    Nil,
                    Ctor.Primary(Nil, Name(""), Nil),
                    Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
                  ),
                  Pkg(
                    Term.Name("baz2"),
                    List(
                      Defn.Class(
                        Nil,
                        Type.Name("D2"),
                        Nil,
                        Ctor.Primary(Nil, Name(""), Nil),
                        Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    assertEquals(tree.syntax, code)
    assertEquals(
      tree.resetAllOrigins.syntax,
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
         |}""".stripMargin.replace("\n", EOL)
    )
  }

  test("case `x`") {
    val tree1 = pat("`x`")
    assertTree(tree1)(Term.Name("x"))
    val tree2 = pat("f(`x`)")
    assertTree(tree2)(Pat.Extract(Term.Name("f"), List(Term.Name("x"))))
    assertEquals(tree2.syntax, "f(`x`)")
    val tree3 = pat("X")
    assertTree(tree3)(Term.Name("X"))
    assertEquals(tree3.syntax, "X")
    val tree4 = pat("f(X)")
    assertTree(tree4)(Pat.Extract(Term.Name("f"), List(Term.Name("X"))))
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

  test("case _: F[?]") {
    assertEquals(pat("_: F[?]").syntax, "_: F[?]")
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
    assertTree(tree)(
      Term.Xml(List(Lit.String("<foo>"), Lit.String("</foo>")), List(Term.Name("bar")))
    )
    assertEquals(tree.syntax, "<foo>{bar}</foo>")
  }

  test("xml literals unit") {
    val tree = term("<foo>{}</foo>")
    assertTree(tree)(
      Term.Xml(List(Lit.String("<foo>"), Lit.String("</foo>")), List(Term.Block(Nil)))
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
    assertTree(tree)(
      Term.Interpolate(
        Term.Name("s"),
        List(Lit.String("Hello"), Lit.String("World")),
        List(Term.Block(Nil))
      )
    )
    assertEquals(tree.syntax, """s"Hello${{}}World"""")
  }

  test("interpolator with $ character") {
    val tree = term("""s"$$$foo$$"""")
    assertTree(tree)(
      Term.Interpolate(
        Term.Name("s"),
        List(Lit.String("$"), Lit.String("$")),
        List(Term.Name("foo"))
      )
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
    assertTree(tree)(Term.Apply(Term.Select(Term.Name("foo"), Term.Name("toString")), Nil))
    assertEquals(tree.syntax, "foo.toString()")
  }

  test("type parameters with type bounds") {
    val Defn.Def(_, _, List(tree), _, _, _) = templStat("def foo[T <: Int] = ???")
    assertTree(tree)(
      Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, Some(Type.Name("Int"))), Nil, Nil)
    )
    assertEquals(tree.syntax, "T <: Int")

    assertTree(templStat("def foo[T <: Int] = ???")) {
      Defn.Def(
        Nil,
        Term.Name("foo"),
        Type.ParamClause(tree :: Nil),
        Nil,
        None,
        Term.Name("???")
      )
    }
  }

  test("Lit(()) - 1") {
    val lit @ Lit(()) = term("()")
    assertTree(lit)(Lit.Unit())
    assertEquals(lit.syntax, "()")
  }

  test("Lit(()) - 2") {
    val Term.If(Term.Name("cond"), Lit(42), lit @ Lit.Unit()) = super.term("if (cond) 42")
    assertTree(lit)(Lit.Unit())
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
    assertEquals(tree1.syntax, "foo((i: Int) => ())")
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
    assertTree(templStat("foo(1l, 1L)"))(
      Term.Apply(Term.Name("foo"), List(Lit.Long(1L), Lit.Long(1L)))
    )
    assertTree(q"val x = 1l")(q"val x = 1L")
  }

  test("show[Structure] should lowercase float literals suffix: '0.01F' -> '0.01f'") {
    assertTree(templStat("foo(0.01f, 0.01F)"))(
      Term.Apply(Term.Name("foo"), List(Lit.Float("0.01"), Lit.Float("0.01")))
    )
    assertTree(q"val x = 1f")(q"val x = 1F")
  }

  test("show[Structure] should lowercase double literals suffix: '0.01D' -> '0.01d'") {
    assertTree(templStat("foo(0.02d, 0.02D, 0.02)"))(
      Term.Apply(Term.Name("foo"), List(Lit.Double("0.02"), Lit.Double("0.02"), Lit.Double("0.02")))
    )
    assertTree(q"val x = 1d")(q"val x = 1D")
    assertTree(q"val x = 1.0d")(q"val x = 1.0")
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
    checkStat(
      s"list map (_ match {${EOL}  case 1 => 2${EOL}})"
    )(q"list map (_ match { case 1 => 2})")
  }

  test("#1839 ApplyInfix parentheses on Term.Placeholder") {
    checkStat("list reduce (_ + _)")(q"list reduce (_ + _)")
    checkStat("list reduce (_ + _)")(q"list reduce (_ + (_))")
    checkStat("list reduce (_ + (_: Int))")(q"list reduce (_ + (_: Int))")
    checkStat("list reduce (_.foo + _.bar)")(q"list reduce (_.foo + _.bar)")
    checkStat("list reduce (_.a.b.c + _.d.e.f)")(q"list reduce (_.a.b.c + _.d.e.f)")
    checkStat("list reduce (_.a(foo) + _.b(bar))")(q"list reduce (_.a(foo) + _.b(bar))")
  }

  test("#1864 Terms with leading numerics are backquoted") {
    checkStat("`123foo`")(Term.Name("123foo"))
    checkStat("""val `123foo` = "hello"""")(q""" val `123foo` = "hello" """)
  }

  test("#1868 Term.Eta preserves structure") {
    checkStat("(x _).y")(Term.Select(Term.Eta(Term.Name("x")), Term.Name("y")))
    checkStat("(x _).y")(q"""(x _).y""")
    checkStat("x _")(Term.Eta(Term.Name("x")))
    checkStat("x _")(q"""x _""")
  }

  test("#2106 unicode characters are properly escaped in string literals") {
    checkStat("\"xy\\u001az\"")(Lit.String("xy\u001az"))
    checkStat("\"þæö\"")(Lit.String("þæö"))
    checkStat("\">\"")(Lit.String(">"))
    checkStat("\"~\"")(Lit.String("~"))
    checkStat("\" \"")(Lit.String(" "))
    checkStat("\"=\"")(Lit.String("="))
    checkStat("\"\\u007f\"")(Lit.String("\u007f"))
    checkStat("\"\\u001f\"")(Lit.String("\u001f"))
    checkStat("\"\\\\\"")(Lit.String("\\"))
    checkStat("\"þæö\"")(Lit.String("\u00fe\u00e6\u00f6"))
    checkStat("\"ラーメン\"")(Lit.String("ラーメン"))
  }

  test("#2447 Pat.Bind on Term.Name") {
    checkStat("{\n  case x @ Y => x\n}")(q"{ case x @ Y => x }")
  }

  test("#2447 Pat.Bind on Term.Name backticks") {
    checkStat("{\n  case x @ `y` => x\n}")(q"{ case x @ `y` => x }")
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

  test("#1843 anonymous functions 1") {
    checkTree(q"list foo (_ fun (_.bar))")(
      Term.ApplyInfix(
        Term.Name("list"),
        Term.Name("foo"),
        Nil,
        List(
          Term.AnonymousFunction(
            Term.ApplyInfix(
              Term.Placeholder(),
              Term.Name("fun"),
              Nil,
              List(Term.AnonymousFunction(Term.Select(Term.Placeholder(), Term.Name("bar"))))
            )
          )
        )
      )
    )
  }

  test("#1843 anonymous functions 2") {
    checkTree(q"list foo (_ fun _.bar)")(
      Term.ApplyInfix(
        Term.Name("list"),
        Term.Name("foo"),
        Nil,
        List(
          Term.AnonymousFunction(
            Term.ApplyInfix(
              Term.Placeholder(),
              Term.Name("fun"),
              Nil,
              List(Term.Select(Term.Placeholder(), Term.Name("bar")))
            )
          )
        )
      )
    )
  }

  test("#2717 anonymous function with unary") {
    checkTree(q"xs span { !separates(_) }") {
      Term.ApplyInfix(
        Term.Name("xs"),
        Term.Name("span"),
        Nil,
        List(
          Term.Block(
            List(
              Term.AnonymousFunction(
                Term.ApplyUnary(
                  Term.Name("!"),
                  Term.Apply(Term.Name("separates"), List(Term.Placeholder()))
                )
              )
            )
          )
        )
      )
    }
  }

  test("anonymous function with new") {
    checkTree(q"foo map (new foo(_))") {
      Term.ApplyInfix(
        Term.Name("foo"),
        Term.Name("map"),
        Nil,
        List(
          Term.AnonymousFunction(
            Term.New(Init(Type.Name("foo"), Name(""), List(List(Term.Placeholder()))))
          )
        )
      )
    }
  }

  test("anonymous function with select") {
    checkTree(q"foo map (foo(_).bar)") {
      Term.ApplyInfix(
        Term.Name("foo"),
        Term.Name("map"),
        Nil,
        List(
          Term.AnonymousFunction(
            Term.Select(Term.Apply(Term.Name("foo"), List(Term.Placeholder())), Term.Name("bar"))
          )
        )
      )
    }
  }

  test("anonymous function with apply type") {
    checkTree(q"foo map (_.foo[A])") {
      Term.ApplyInfix(
        Term.Name("foo"),
        Term.Name("map"),
        Nil,
        List(
          Term.AnonymousFunction(
            Term.ApplyType(Term.Select(Term.Placeholder(), Term.Name("foo")), List(Type.Name("A")))
          )
        )
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

  test("#2699 method declaration with multiple named 'using' params") {
    assertEquals(
      templStat("def foo(x: Int)(using y: String, z: Boolean): String")(dialects.Scala3).syntax,
      "def foo(x: Int)(using y: String, z: Boolean): String"
    )
  }

  test("#2699 method definition with multiple named 'using' params") {
    assertEquals(
      templStat("def foo(x: Int)(using y: String, z: Boolean) = x")(dialects.Scala3).syntax,
      "def foo(x: Int)(using y: String, z: Boolean) = x"
    )
  }

  test("#2699 primary constructor with multiple named 'using' params") {
    assertEquals(
      templStat("class C(x: Int)(using y: String, z: Boolean)")(dialects.Scala3).syntax,
      "class C(x: Int)(using y: String, z: Boolean)"
    )
  }

  test("#2699 secondary constructor with multiple named 'using' params") {
    assertEquals(
      templStat(
        "class C(x: Int) { def this(x: String)(using y: String, z: Boolean) = this(x.toInt) }"
      )(dialects.Scala3).syntax,
      "class C(x: Int) { def this(x: String)(using y: String, z: Boolean) = this(x.toInt) }"
    )
  }

  test("#2699 method declaration with multiple anonymous 'using' params") {
    assertEquals(
      templStat("def foo(x: Int)(using String, Boolean): String")(dialects.Scala3).syntax,
      "def foo(x: Int)(using String, Boolean): String"
    )
  }

  test("#2699 method definition with multiple anonymous 'using' params") {
    assertEquals(
      templStat("def foo(x: Int)(using String, Boolean) = x")(dialects.Scala3).syntax,
      "def foo(x: Int)(using String, Boolean) = x"
    )
  }

  test("#2699 primary constructor with multiple anonymous 'using' params") {
    assertEquals(
      templStat("class C(x: Int)(using String, Boolean)")(dialects.Scala3).syntax,
      "class C(x: Int)(using String, Boolean)"
    )
  }

  test("#2699 secondary constructor with multiple anonymous 'using' params") {
    assertEquals(
      templStat(
        "class C(x: Int) { def this(x: String)(using String, Boolean) = this(x.toInt) }"
      )(dialects.Scala3).syntax,
      "class C(x: Int) { def this(x: String)(using String, Boolean) = this(x.toInt) }"
    )
  }

  test("#1596") {
    val tree: Term.Xml = q"<h1>a{b}</h1>"
    checkTree(tree)(
      Term.Xml(List(Lit.String("<h1>a"), Lit.String("</h1>")), List(Term.Name("b")))
    )
    val dialect = implicitly[Dialect]
    val Term.Xml(part1 :: part2 :: Nil, arg1 :: Nil) = tree

    val inputPart1 = Input.VirtualFile(path = "<InternalTrees.tokens>", value = "<h1>a")
    assertEquals(
      part1.tokens.toList,
      List(
        Token.Constant.String(inputPart1, dialect, 0, 5, "<h1>a")
      )
    )

    val inputArg1 = Input.VirtualFile(path = "<InternalTrees.tokens>", value = "b")
    assertEquals(
      arg1.tokens.toList,
      List(
        Token.BOF(inputArg1, dialect, 0),
        Token.Ident(inputArg1, dialect, 0, 1, "b"),
        Token.EOF(inputArg1, dialect, 1)
      )
    )

    val inputPart2 = Input.VirtualFile(path = "<InternalTrees.tokens>", value = "</h1>")
    assertEquals(
      part2.tokens.toList,
      List(
        Token.Constant.String(inputPart2, dialect, 0, 5, "</h1>")
      )
    )
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

  test("#1063 good") {
    checkTree(term("foo(bar) { baz: _* }"))(
      Term.Apply(
        Term.Apply(Term.Name("foo"), List(Term.Name("bar"))),
        List(Term.Block(List(Term.Repeated(Term.Name("baz")))))
      )
    )
  }

  test("#1063 bad") {
    val thrown = intercept[ParseException] {
      term("foo(bar) { val baz = qux; baz: _* }")
    }
    assertEquals(
      thrown.getMessage.substring(0, 52),
      "<input>:1: error: repeated argument not allowed here"
    )
  }

  test("#2708 term lassoc") {
    checkTree(q"""{
            () == ()
            (()) == (())
            () == () == ()
            (()) == (()) == (())
          }""")(
      Term.Block(
        List(
          Term.ApplyInfix(Lit.Unit(), Term.Name("=="), Nil, Nil),
          Term.ApplyInfix(
            Lit.Unit(),
            Term.Name("=="),
            Nil,
            List(Lit.Unit())
          ),
          Term.ApplyInfix(
            Term.ApplyInfix(Lit.Unit(), Term.Name("=="), Nil, Nil),
            Term.Name("=="),
            Nil,
            Nil
          ),
          Term.ApplyInfix(
            Term.ApplyInfix(
              Lit.Unit(),
              Term.Name("=="),
              Nil,
              List(Lit.Unit())
            ),
            Term.Name("=="),
            Nil,
            List(Lit.Unit())
          )
        )
      )
    )
  }

  test("#2708 term rassoc") {
    checkTree(q"""{
            () :: ()
            (()) :: (())
            () :: () :: ()
            (()) :: (()) :: (())
          }""")(
      Term.Block(
        List(
          Term.ApplyInfix(Lit.Unit(), Term.Name("::"), Nil, Nil),
          Term.ApplyInfix(
            Lit.Unit(),
            Term.Name("::"),
            Nil,
            List(Lit.Unit())
          ),
          Term.ApplyInfix(
            Lit.Unit(),
            Term.Name("::"),
            Nil,
            List(Term.ApplyInfix(Lit.Unit(), Term.Name("::"), Nil, Nil))
          ),
          Term.ApplyInfix(
            Lit.Unit(),
            Term.Name("::"),
            Nil,
            List(
              Term.ApplyInfix(
                Lit.Unit(),
                Term.Name("::"),
                Nil,
                List(Lit.Unit())
              )
            )
          )
        )
      )
    )
  }

  test("#2708 pat lassoc") {
    checkTree(q"""foo match {
            case () == () =>
            case (()) == (()) =>
            case () == () == () =>
            case (()) == (()) == (()) =>
          }""")(
      Term.Match(
        Term.Name("foo"),
        List(
          Case(Pat.ExtractInfix(Lit.Unit(), Term.Name("=="), Nil), None, Term.Block(Nil)),
          Case(
            Pat.ExtractInfix(
              Lit.Unit(),
              Term.Name("=="),
              List(Lit.Unit())
            ),
            None,
            Term.Block(Nil)
          ),
          Case(
            Pat.ExtractInfix(
              Pat.ExtractInfix(Lit.Unit(), Term.Name("=="), Nil),
              Term.Name("=="),
              Nil
            ),
            None,
            Term.Block(Nil)
          ),
          Case(
            Pat.ExtractInfix(
              Pat.ExtractInfix(
                Lit.Unit(),
                Term.Name("=="),
                List(Lit.Unit())
              ),
              Term.Name("=="),
              List(Lit.Unit())
            ),
            None,
            Term.Block(Nil)
          )
        ),
        Nil
      )
    )
  }

  test("#2708 pat rassoc") {
    checkTree(q"""foo match {
            case () :: () =>
            case (()) :: (()) =>
            case () :: () :: () =>
            case (()) :: (()) :: (()) =>
          }""")(
      Term.Match(
        Term.Name("foo"),
        List(
          Case(Pat.ExtractInfix(Lit.Unit(), Term.Name("::"), Nil), None, Term.Block(Nil)),
          Case(
            Pat.ExtractInfix(
              Lit.Unit(),
              Term.Name("::"),
              List(Lit.Unit())
            ),
            None,
            Term.Block(Nil)
          ),
          Case(
            Pat.ExtractInfix(
              Lit.Unit(),
              Term.Name("::"),
              List(Pat.ExtractInfix(Lit.Unit(), Term.Name("::"), Nil))
            ),
            None,
            Term.Block(Nil)
          ),
          Case(
            Pat.ExtractInfix(
              Lit.Unit(),
              Term.Name("::"),
              List(
                Pat.ExtractInfix(
                  Lit.Unit(),
                  Term.Name("::"),
                  List(Lit.Unit())
                )
              )
            ),
            None,
            Term.Block(Nil)
          )
        ),
        Nil
      )
    )
  }

  test("#1384 char no unescaped LF") {
    val expr = "('\n')"
    assertNoDiff(
      intercept[TokenizeException](super.term(expr)).getMessage,
      """|<input>:1: error: can't use unescaped LF in character literals
         |('
         |  ^""".stripMargin
    )
  }

  test("#1384 char ok escaped LF") {
    val exprU = "'\\u000a'"
    val expr = s"('\\n', $exprU)"
    val syntax = "('\\n', '\\n')"
    val tree = super.term(expr)
    val charN = Lit.Char('\n')
    val origin = Origin.Parsed(Input.String(exprU), implicitly[Dialect], TokenStreamPosition(1, 2))
    val charU = charN.withOrigin(origin)
    checkTree(tree, expr) {
      Term.Tuple(List(charN, charU))
    }
    checkTree(tree.resetAllOrigins, syntax) {
      Term.Tuple(List(charN, charN))
    }
  }

  test("pat infix: _ op (a | b)") {
    checkTree(p"_ op (a | b)", "_ op (a | b)") {
      Pat.ExtractInfix(
        Pat.Wildcard(),
        Term.Name("op"),
        List(Pat.Alternative(Pat.Var(Term.Name("a")), Pat.Var(Term.Name("b"))))
      )
    }
  }

  test("pat infix: _ * (a + b)") {
    checkTree(p"_ * (a + b)", "_ * (a + b)") {
      Pat.ExtractInfix(
        Pat.Wildcard(),
        Term.Name("*"),
        List(
          Pat.ExtractInfix(Pat.Var(Term.Name("a")), Term.Name("+"), List(Pat.Var(Term.Name("b"))))
        )
      )
    }
  }

  test("term infix: _ * (a + b)") {
    checkTree(q"_ * (a + b)", "_ * (a + b)") {
      Term.AnonymousFunction(
        Term.ApplyInfix(
          Term.Placeholder(),
          Term.Name("*"),
          Nil,
          List(Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("b"))))
        )
      )
    }
  }

  test("term infix: 1 + (2 / 3) * 4") {
    checkTree(q"1 + (2 / 3) * 4", "1 + 2 / 3 * 4") {
      Term.ApplyInfix(
        Lit.Int(1),
        Term.Name("+"),
        Nil,
        List(
          Term.ApplyInfix(
            Term.ApplyInfix(Lit.Int(2), Term.Name("/"), Nil, List(Lit.Int(3))),
            Term.Name("*"),
            Nil,
            List(Lit.Int(4))
          )
        )
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
        Lit.Int(1),
        Term.Name("+"),
        Nil,
        List(
          Term.ApplyInfix(
            Term.Block(List(Term.ApplyInfix(Lit.Int(2), Term.Name("/"), Nil, List(Lit.Int(3))))),
            Term.Name("*"),
            Nil,
            List(Lit.Int(4))
          )
        )
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
        Term.Block(List(Term.ApplyInfix(Lit.Int(2), Term.Name("/"), Nil, List(Lit.Int(3))))),
        Term.Name("+"),
        Nil,
        List(Lit.Int(4))
      )
    }
  }

  test("term anon func: foo.bar(_: Int, _: String)") {
    checkTree(q"foo.bar(_: Int, _: String)", "foo.bar(_: Int, _: String)") {
      Term.AnonymousFunction(
        Term.Apply(
          Term.Select(Term.Name("foo"), Term.Name("bar")),
          List(
            Term.Ascribe(Term.Placeholder(), Type.Name("Int")),
            Term.Ascribe(Term.Placeholder(), Type.Name("String"))
          )
        )
      )
    }
  }

  test("#3065 embedded triple quotes") {
    val tree = Lit.String("\n\"\"\"")
    assertSyntax(tree, "\"\"\"\n\\\"\\\"\\\"\"\"\"")(tree)
  }

}

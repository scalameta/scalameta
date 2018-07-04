package scala.meta.tests
package prettyprinters

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.trees._

class SyntacticSuite extends scala.meta.tests.parsers.ParseSuite {
  override def term(code: String)(implicit dialect: Dialect)       = super.term(code)(dialect).resetAllOrigins
  override def pat(code: String)(implicit dialect: Dialect)        = super.pat(code)(dialect).resetAllOrigins
  override def tpe(code: String)(implicit dialect: Dialect)        = super.tpe(code)(dialect).resetAllOrigins
  override def topStat(code: String)(implicit dialect: Dialect)    = super.topStat(code)(dialect).resetAllOrigins
  override def templStat(code: String)(implicit dialect: Dialect)  = super.templStat(code)(dialect).resetAllOrigins
  override def blockStat(code: String)(implicit dialect: Dialect)  = super.blockStat(code)(dialect).resetAllOrigins
  override def caseClause(code: String)(implicit dialect: Dialect) = super.caseClause(code)(dialect).resetAllOrigins
  override def source(code: String)(implicit dialect: Dialect)     = super.source(code)(dialect).resetAllOrigins
  implicit class XtensionResetOrigin[T <: Tree](tree: T) {
    // NOTE: Ensures that neither the given tree nor its subtrees have their origins set.
    // This is necessary to force prettyprinting as opposed to reusing original syntax.
    def resetAllOrigins: T = {
      tree.transform{ case tree: Tree => tree.withOrigin(Origin.None) }.asInstanceOf[T]
    }
  }

  test("val x: Int (raw)") {
    val tree = templStat("val x: Int")
    assert(tree.structure === "Decl.Val(Nil, List(Pat.Var(Term.Name(\"x\"))), Type.Name(\"Int\"))")
  }

  test("val x: Int (code)") {
    val tree = templStat("val x: Int")
    assert(tree.syntax === "val x: Int")
  }

  test("~(1 + 2) + ~x.y(z) + (~x).y(z)") {
    val tree = templStat("~(1 + 2) + ~x.y(z) + (~x).y(z)")
    assert(tree.syntax === "~(1 + 2) + ~x.y(z) + (~x).y(z)")
  }

  test("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)") {
    val tree = templStat("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)")
    assert(tree.syntax === "a + b + c && a + (b + c) && (a :: b :: c) && ((a :: b) :: c)")
  }

  test("(x map y).foo") {
    val tree = templStat("(x map y).foo")
    assert(tree.syntax === "(x map y).foo")
  }

  test("multi-line string literals") {
    val tree = templStat("""{
      val x = QQQ
        x
      QQQ
    }""".replace("QQQ", "\"\"\""))

    assertSameLines(tree.syntax, """
    |{
    |  val x = QQQ
    |        x
    |      QQQ
    |}
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("string literals with newlines and double quotes") {
    val tree = templStat("""{
      val x = QQQ
        x
      QQQ
      val y = "\""
    }""".replace("QQQ", "\"\"\""))
    assert(tree.structure === """Term.Block(List(Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.String("%n        x%n      ")), Defn.Val(Nil, List(Pat.Var(Term.Name("y"))), None, Lit.String("\""))))""".replace("%n", "\\n"))
    assertSameLines(tree.syntax, """
    |{
    |  val x = QQQ
    |        x
    |      QQQ
    |  val y = "\""
    |}
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
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
    assert(tree.structure === """Term.Block(List(Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Term.Interpolate(Term.Name("q"), List(Lit.String("123 + "), Lit.String(" + "), Lit.String(" + 456")), List(Term.Name("x"), Term.Apply(Term.Name("foo"), List(Lit.Int(123)))))), Defn.Val(Nil, List(Pat.Var(Term.Name("y"))), None, Lit.String("%n        $x%n        $y%n        ..$z%n      "))))""".replace("%n", "\\n"))
    assertSameLines(tree.syntax, """
    |{
    |  val x = q"123 + $x + ${foo(123)} + 456"
    |  val y = QQQ
    |        $x
    |        $y
    |        ..$z
    |      QQQ
    |}
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("foo.bar(bar) { baz }") {
    val tree = templStat("foo.bar(bar) { baz }")
    assert(tree.syntax === """
      |foo.bar(bar) {
      |  baz
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("Template.self stringifications") {
    assert(templStat("new { val x = 2 }").syntax === "new { val x = 2 }")
    assert(templStat("new { self => val x = 2 }").syntax === "new { self => val x = 2 }")
    assert(templStat("new { self: Int => val x = 2 }").syntax === "new { self: Int => val x = 2 }")
    assert(templStat("""
      new {
        val x = 2
        val y = 3
      }
    """).syntax === """
      |new {
      |  val x = 2
      |  val y = 3
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
    assert(templStat("""
      new { self =>
        val x = 2
        val y = 3
      }
    """).syntax === """
      |new { self =>
      |  val x = 2
      |  val y = 3
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
    assert(templStat("""
      new { self: Int =>
        val x = 2
        val y = 3
      }
    """).syntax === """
      |new { self: Int =>
      |  val x = 2
      |  val y = 3
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
    assert(templStat("class B { x: B => }").syntax === "class B { x: B => }")
  }

  test("new X") {
    assert(templStat("new X").syntax === "new X")
    assert(templStat("new X {}").syntax === "new X {}")
  }

  test("ascribe and annotate") {
    assert(templStat("_: Int").syntax === "_: Int")
    assert(templStat("(_: Int) + 2").syntax === "(_: Int) + 2")
    assert(templStat("x: @foo").syntax === "x: @foo")
    assert(templStat("(x: @foo) + 2").syntax === "(x: @foo) + 2")
  }

  test("compound types") {
    assert(tpe("Foo").syntax === "Foo")
    assert(tpe("Foo {}").syntax === "Foo {}")
    assert(tpe("Foo { type T = Int }").syntax === "Foo { type T = Int }")
    assert(tpe("Foo { type T = Int; type U <: String }").syntax === "Foo { type T = Int; type U <: String }")
    assert(tpe("Foo with Bar").syntax === "Foo with Bar")
    assert(tpe("Foo with Bar {}").syntax === "Foo with Bar {}")
    assert(tpe("Foo with Bar { type T = Int }").syntax === "Foo with Bar { type T = Int }")
    assert(tpe("Foo with Bar { type T = Int; type U <: String }").syntax === "Foo with Bar { type T = Int; type U <: String }")
  }

  test("infix types") {
    assert(tpe("Foo + Bar").syntax === "Foo + Bar")
    assert(tpe("Foo & Bar").syntax === "Foo & Bar")
    assert(tpe("Foo | Bar").syntax === "Foo | Bar")
  }

  test("and types") {
    val Scala211 = null
    import scala.meta.dialects.Dotty
    assert(tpe("Foo & Bar").syntax === "Foo & Bar")
  }

  test("or types") {
    val Scala211 = null
    import scala.meta.dialects.Dotty
    assert(tpe("Foo | Bar").syntax === "Foo | Bar")
  }

  test("trait parameters") {
    val Scala211 = null
    import scala.meta.dialects.Dotty
    assert(Dotty(q"trait T(a: Int)").syntax === "trait T(a: Int)")
  }

  test("literalTypes") {
    intercept[ParseException] {
      dialects.Scala211("val a : 42 = 42").parse[Stat].get.syntax
    }
    val Scala211 = null
    import scala.meta.dialects.Dotty
    assert(q"val a: 42 = 42".syntax === "val a: 42 = 42")
    assert(q"val a: 42L = 42L".syntax === "val a: 42L = 42L")
    assert(q"val a: 42d = 42d".syntax === "val a: 42d = 42d")
    assert(q"val a: 42.0d = 42.0d".syntax === "val a: 42.0d = 42.0d")
    assert(q"val a: 42f = 42f".syntax === "val a: 42f = 42f")
    assert(q"val a: 42.0f = 42.0f".syntax === "val a: 42.0f = 42.0f")
    assert(q"val a: true = true".syntax === "val a: true = true")
    assert(q"val a: false = false".syntax === "val a: false = false")
    assert(dialects.Dotty("val a: \"42\" = \"42\"").parse[Stat].get.syntax === "val a: \"42\" = \"42\"")
    assert(pat("_: 42").syntax === "_: 42")
    assert(pat("_: 42f").syntax === "_: 42f")
    assert(pat("_: 42d").syntax === "_: 42d")
    assert(pat("_: 42.0f").syntax === "_: 42.0f")
    assert(pat("_: 42.0d").syntax === "_: 42.0d")
    assert(pat("_: 42L").syntax === "_: 42L")
    assert(pat("_: true").syntax === "_: true")
    assert(pat("_: false").syntax === "_: false")
  }

  test("packages") {
    assert(source("package foo.bar; class C").syntax === s"package foo.bar${EOL}class C")
    assert(source("package foo.bar; class C; class D").syntax === s"package foo.bar${EOL}class C${EOL}class D")
    assert(source("package foo.bar { class C }").syntax === s"package foo.bar${EOL}class C")
    assert(source("package foo.bar { class C; class D }").syntax === s"package foo.bar${EOL}class C${EOL}class D")
  }

  test("type parameter mods") {
    assert(source("class C[@foo T]").syntax === "class C[@foo T]")
    assert(source("class C[+T]").syntax === "class C[+T]")
    assert(source("class C[@foo +T]").syntax === "class C[@foo +T]")
  }

  test("primary constructor mods") {
    assert(source("class C").syntax === "class C")
    assert(source("class C private").syntax === "class C private")
    assert(source("class C @foo(x)").syntax === "class C @foo(x)")
    assert(source("class C @foo(x) private").syntax === "class C @foo(x) private")
    assert(source("class C(x: Int)").syntax === "class C(x: Int)")
    assert(source("class C private (x: Int)").syntax === "class C private (x: Int)")
    assert(source("class C @foo(x) (x: Int)").syntax === "class C @foo(x) (x: Int)")
    assert(source("class C @foo(x) private (x: Int)").syntax === "class C @foo(x) private (x: Int)")
  }

  test("parentheses in patterns") {
    assert(templStat("x match { case (xs: List[Int]) :+ x => ??? }").syntax === """
      |x match {
      |  case (xs: List[Int]) :+ x => ???
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("List(x, y) :: z") {
    assert(templStat("List(x, y) :: z").syntax == "List(x, y) :: z")
    assert(templStat("x match { case List(x, y) :: z => ??? }").syntax === """
      |x match {
      |  case List(x, y) :: z => ???
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("secondary ctor - expr") {
    assert(source("class C(x: Int) { def this() = this(2) }").syntax === "class C(x: Int) { def this() = this(2) }")
  }

  test("secondary ctor - block") {
    assert(source("class C(x: Int) { def this() { this(2); println(\"OBLIVION!!!\") } }").syntax === """
      |class C(x: Int) {
      |  def this() {
      |    this(2)
      |    println("OBLIVION!!!")
      |  }
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("case semicolons") {
    assert(templStat("x match { case y => foo1; foo2 }").syntax === """
      |x match {
      |  case y =>
      |    foo1
      |    foo2
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("assorted literals") {
    assert(templStat("true").syntax === "true")
    assert(templStat("false").syntax === "false")
    assert(templStat("0").syntax === "0")
    assert(templStat("0l").syntax === "0L")
    assert(templStat("0L").syntax === "0L")
    assert(templStat("0f").syntax === "0f")
    assert(templStat("0F").syntax === "0f")
    assert(templStat("0.0f").syntax === "0.0f")
    assert(templStat("0.0F").syntax === "0.0f")
    assert(templStat("1.4f").syntax === "1.4f")
    assert(templStat("1.40f").syntax === "1.40f")
    assert(templStat("0.0").syntax === "0.0d")
    assert(templStat("0d").syntax === "0d")
    assert(templStat("0D").syntax === "0d")
    assert(templStat("0.0d").syntax === "0.0d")
    assert(templStat("0.0D").syntax === "0.0d")
    assert(templStat("'0'").syntax === "'0'")
    assert(templStat("\"0\"").syntax === "\"0\"")
    assert(templStat("'zero").syntax === "'zero")
    assert(templStat("null").syntax === "null")
    assert(templStat("()").syntax === "()")
  }

  test("Lit.Double") {
    assert(templStat("1.4d").structure == """Lit.Double(1.4d)""")
    assert(templStat("1.40d").structure == """Lit.Double(1.40d)""")
    // NOTE: This fails under Scala Native:
    // [info] - Lit.Double *** FAILED ***
    // [info]   "Lit.Double(1.4[00000]d)" did not equal "Lit.Double(1.4[]d)" (SyntacticSuite.scala:321)
    // assert(Lit.Double(1.40d).structure == "Lit.Double(1.4d)") // trailing 0 is lost
    // assert(Lit.Double(1.4d).structure == "Lit.Double(1.4d)")
    // assert(Lit.Double(1.40d).structure == "Lit.Double(1.4d)") // trailing 0 is lost
    assert(Lit.Double(Double.NaN).syntax == "Double.NaN")
    assert(Lit.Double(Double.PositiveInfinity).syntax == "Double.PositiveInfinity")
    assert(Lit.Double(Double.NegativeInfinity).syntax == "Double.NegativeInfinity")
    assert(Lit.Double(Double.NaN).structure == "Lit.Double(Double.NaN)")
    assert(Lit.Double(Double.PositiveInfinity).structure == "Lit.Double(Double.PositiveInfinity)")
    assert(Lit.Double(Double.NegativeInfinity).structure == "Lit.Double(Double.NegativeInfinity)")
  }

  test("Lit.Float") {
    assert(templStat("1.4f").structure == """Lit.Float(1.4f)""")
    assert(templStat("1.40f").structure == """Lit.Float(1.40f)""")
    assert(Lit.Float(Float.NaN).syntax == "Float.NaN")
    assert(Lit.Float(Float.PositiveInfinity).syntax == "Float.PositiveInfinity")
    assert(Lit.Float(Float.NegativeInfinity).syntax == "Float.NegativeInfinity")
    assert(Lit.Float(Float.NaN).structure == "Lit.Float(Float.NaN)")
    assert(Lit.Float(Float.PositiveInfinity).structure == "Lit.Float(Float.PositiveInfinity)")
    assert(Lit.Float(Float.NegativeInfinity).structure == "Lit.Float(Float.NegativeInfinity)")
  }

  test("context and view bounds") {
    assert(templStat("class C[T: List, U <% Int]").syntax === "class C[T: List, U <% Int]")
    assert(templStat("def m[T: List, U <% Int] = ???").syntax === "def m[T: List, U <% Int] = ???")
  }

  test("some tricky parenthesization") {
    assert(templStat("if (1) 2 else 3 + 4").syntax === "if (1) 2 else 3 + 4")
    assert(templStat("(if (1) 2 else 3) + 4").syntax === "(if (1) 2 else 3) + 4")
    assert(templStat("if (1) 2 else 3 match { case _ => }").syntax === s"if (1) 2 else 3 match {${EOL}  case _ =>${EOL}}")
    assert(templStat("(if (1) 2 else 3) match { case _ => }").syntax === s"(if (1) 2 else 3) match {${EOL}  case _ =>${EOL}}")
    assert(templStat("unit.toCheck += (() => body)").syntax === "unit.toCheck += (() => body)")
    assert(templStat("({ foo1; foo2 }).orElse(bar)").syntax === s"{${EOL}  foo1${EOL}  foo2${EOL}}.orElse(bar)")
    assert(templStat("(foo match { case _ => }).orElse(bar)").syntax === s"(foo match {${EOL}  case _ =>${EOL}}).orElse(bar)")
    assert(templStat("foo || (if (cond) bar else baz)").syntax === "foo || (if (cond) bar else baz)")
    assert(templStat("foo && (bar match { case _ => })").syntax === s"foo && (bar match {${EOL}  case _ =>${EOL}})")
    assert(templStat("\"foo \" + (if (cond) bar else baz)").syntax === "\"foo \" + (if (cond) bar else baz)")
    assert(templStat("foo match { case bar @ (_: T1 | _: T2) => }").syntax === s"foo match {${EOL}  case bar @ (_: T1 | _: T2) =>${EOL}}")
    assert(templStat("foo match { case A + B / C => }").syntax === s"foo match {${EOL}  case A + B / C =>${EOL}}")
    assert(templStat("foo match { case (A + B) / C => }").syntax === s"foo match {${EOL}  case (A + B) / C =>${EOL}}")
    assert(templStat("foo match { case A + (B / C) => }").syntax === s"foo match {${EOL}  case A + B / C =>${EOL}}")
    assert(templStat("foo match { case bar :: Nil :: Nil => }").syntax === s"foo match {${EOL}  case bar :: Nil :: Nil =>${EOL}}")
    assert(templStat("foo match { case (bar :: Nil) :: Nil => }").syntax === s"foo match {${EOL}  case (bar :: Nil) :: Nil =>${EOL}}")
    assert(templStat("@(foo @foo) class Bar").syntax === "@(foo @foo) class Bar")
    assert(templStat("(foo: Foo): @foo").syntax === "(foo: Foo): @foo")
    assert(templStat("type T = A + B / C").syntax === "type T = A + B / C")
    assert(templStat("type T = (A + B) / C").syntax === "type T = A + B / C")
    assert(templStat("type T = A + (B / C)").syntax === "type T = A + (B / C)")
    assert(templStat("type T = A :: B :: C").syntax === "type T = A :: B :: C")
    assert(templStat("type T = (A :: B) :: C").syntax === "type T = (A :: B) :: C")
    assert(templStat("foo match { case _: A | _: B => }").syntax === s"foo match {${EOL}  case _: A | _: B =>${EOL}}")
    assert(templStat("foo match { case _: A | _: B | _: C => }").syntax === s"foo match {${EOL}  case _: A | _: B | _: C =>${EOL}}")
  }

  test("Type projections") {
    // Without lambda trick
    assert(
      q"""class A { class B }
          type C = A#B
        """.syntax ===
        """{
            |  class A { class B }
            |  type C = A#B
            |}""".stripMargin.split('\n').mkString(EOL)
    )
    // With lambda trick
    assert(
      q"""
      def foo[F[_]]: Unit = ???
      foo[({ type T[A] = Either[Int, A] })#T]
        """.syntax ===
        """{
            |  def foo[F[_]]: Unit = ???
            |  foo[({ type T[A] = Either[Int, A] })#T]
            |}""".stripMargin.split('\n').mkString(EOL)
    )
  }

  test("more trickiness") {
    assert(templStat("def foo(bar_ : Int) = ???").syntax === "def foo(bar_ : Int) = ???")
    assert(templStat("class C[T_ : Foo]").syntax === "class C[T_ : Foo]")
    assert(templStat("val scala_ : NameType = ???").syntax === "val scala_ : NameType = ???")
  }

  test("class C extends (() => Int)") {
    assert(templStat("class C extends (() => Int)").syntax === "class C extends (() => Int)")
  }

  test("class C(x: Int)(implicit y: String, z: Boolean)") {
    assert(templStat("class C(x: Int)(implicit y: String, z: Boolean)").syntax === "class C(x: Int)(implicit y: String, z: Boolean)")
  }

  test("class C(var x: Int)") {
    assert(templStat("class C(var x: Int)").syntax === "class C(var x: Int)")
  }

  test("private/protected within something") {
    assert(templStat("""
      class C {
        private[this] val x = 1
        private[D] val y = 2
        protected[this] val z = 3
        protected[D] val w = 4
      }
    """).syntax === """
      |class C {
      |  private[this] val x = 1
      |  private[D] val y = 2
      |  protected[this] val z = 3
      |  protected[D] val w = 4
      |}
    """.stripMargin.trim.split('\n').mkString(EOL))
  }

  test("case List(xs @ _*)") {
    val tree = pat("List(xs @ _*)")
    assert(tree.structure === "Pat.Extract(Term.Name(\"List\"), List(Pat.Bind(Pat.Var(Term.Name(\"xs\")), Pat.SeqWildcard())))")
    assert(tree.syntax === "List(xs @ _*)")
  }

  test("case List[t](xs @ _*)") {
    val tree = pat("List[t](xs @ _*)")
    assert(tree.structure === "Pat.Extract(Term.ApplyType(Term.Name(\"List\"), List(Type.Var(Type.Name(\"t\")))), List(Pat.Bind(Pat.Var(Term.Name(\"xs\")), Pat.SeqWildcard())))")
    assert(tree.syntax === "List[t](xs @ _*)")
  }

  test("case List[_](xs @ _*)") {
    val tree = pat("List[_](xs @ _*)")
    assert(tree.structure === "Pat.Extract(Term.ApplyType(Term.Name(\"List\"), List(Type.Placeholder(Type.Bounds(None, None)))), List(Pat.Bind(Pat.Var(Term.Name(\"xs\")), Pat.SeqWildcard())))")
    assert(tree.syntax === "List[_](xs @ _*)")
  }

  test("package foo; class C; package baz { class D }") {
    val tree = source("package foo; class C; package baz { class D }")
    assert(tree.structure === "Source(List(Pkg(Term.Name(\"foo\"), List(Defn.Class(Nil, Type.Name(\"C\"), Nil, Ctor.Primary(Nil, Name(\"\"), Nil), Template(Nil, Nil, Self(Name(\"\"), None), Nil)), Pkg(Term.Name(\"baz\"), List(Defn.Class(Nil, Type.Name(\"D\"), Nil, Ctor.Primary(Nil, Name(\"\"), Nil), Template(Nil, Nil, Self(Name(\"\"), None), Nil))))))))")
    assert(tree.syntax === s"package foo${EOL}class C${EOL}package baz {$EOL  class D${EOL}}")
  }

  test("case `x`") {
    val tree1 = pat("`x`")
    assert(tree1.structure === "Term.Name(\"x\")")
    val tree2 = pat("f(`x`)")
    assert(tree2.structure === "Pat.Extract(Term.Name(\"f\"), List(Term.Name(\"x\")))")
    assert(tree2.syntax === "f(`x`)")
    val tree3 = pat("X")
    assert(tree3.structure === "Term.Name(\"X\")")
    assert(tree3.syntax === "X")
    val tree4 = pat("f(X)")
    assert(tree4.structure === "Pat.Extract(Term.Name(\"f\"), List(Term.Name(\"X\")))")
    assert(tree4.syntax === "f(X)")
  }

  test("case _: Int") {
    assert(pat("_: Int").syntax === "_: Int")
  }

  test("case _: t") {
    assert(pat("_: t").syntax === "_: t")
  }

  test("case _: F[t]") {
    assert(pat("_: F[t]").syntax === "_: F[t]")
  }

  test("case _: F[_]") {
    assert(pat("_: F[_]").syntax === "_: F[_]")
  }

  test("constructors") {
    val tree @ Defn.Class(_, _, _, primary, Template(_, _, _, List(secondary))) = templStat("class C(x: Int) { def this() = this(42) }")
    assert(tree.syntax === "class C(x: Int) { def this() = this(42) }")
    assert(primary.syntax === "(x: Int)")
    assert(secondary.syntax === "def this() = this(42)")
    assert(tree.toString === "class C(x: Int) { def this() = this(42) }")
    assert(primary.toString === "def this(x: Int)")
    assert(secondary.toString === "def this() = this(42)")
  }

  test("smart case printing - oneliner in one line") {
    val Term.Match(_, case1 :: Nil) = templStat("??? match { case x => x }")
    assert(case1.toString === "case x => x")
  }

  test("smart case printing - oneliner in multiple lines") {
    val Term.Match(_, case1 :: case2 :: Nil) = templStat("??? match { case x => x; case List(x, y) => println(x); println(y) }")
    assertSameLines(case1.toString, """
      |case x =>
      |  x
    """.trim.stripMargin)
    assert(case2.toString === """
      |case List(x, y) =>
      |  println(x)
      |  println(y)
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("xml literals") {
    val tree = term("<foo>{bar}</foo>")
    assert(tree.structure === """Term.Xml(List(Lit.String("<foo>"), Lit.String("</foo>")), List(Term.Name("bar")))""")
    assert(tree.syntax === "<foo>{bar}</foo>")
  }

  test("xml literals unit") {
    val tree = term("<foo>{}</foo>")
    assert(tree.structure == """Term.Xml(List(Lit.String("<foo>"), Lit.String("</foo>")), List(Term.Block(Nil)))""")
    assert(tree.syntax == "<foo>{{}}</foo>")
  }

  test("xml literals: pattern position") {
    assert(pat("<a>{_*}</a>").show[Syntax] == "<a>{_*}</a>")
    assert(pat("<a>{ns @ _*}</a>").show[Syntax] == "<a>{ns @ _*}</a>")
    assert(pat("<a><b/>{ns @ _*}</a>").show[Syntax] == "<a><b/>{ns @ _*}</a>")
  }

  test("interpolator unit") {
    val tree = term("""s"Hello${}World"""")
    assert(tree.structure == """Term.Interpolate(Term.Name("s"), List(Lit.String("Hello"), Lit.String("World")), List(Term.Block(Nil)))""")
    assert(tree.syntax == """s"Hello${{}}World"""")
  }

  test("empty-arglist application") {
    val tree = term("foo.toString()")
    assert(tree.structure === "Term.Apply(Term.Select(Term.Name(\"foo\"), Term.Name(\"toString\")), Nil)")
    assert(tree.syntax === "foo.toString()")
  }

  test("type parameters with type bounds") {
    val Defn.Def(_, _, List(tree), _, _, _) = templStat("def foo[T <: Int] = ???")
    assert(tree.structure === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, Some(Type.Name(\"Int\"))), Nil, Nil)")
    assert(tree.syntax === "T <: Int")
  }

  test("Lit(()) - 1") {
    val lit @ Lit(()) = term("()")
    assert(lit.structure === "Lit.Unit(())")
    assert(lit.syntax === "()")
  }

  test("Lit(()) - 2") {
    val Term.If(Term.Name("cond"), Lit(42), lit @ Lit.Unit()) = super.term("if (cond) 42")
    assert(lit.structure === "Lit.Unit(())")
    assert(lit.syntax === "")
  }

  test("Type.Function(Tuple, _) #557") {
    assert(t"((a, b)) => c".syntax    == "((a, b)) => c")
    assert(t"((a, b), c) => c".syntax == "((a, b), c) => c")
  }

  test("Term.Apply(_, List(Term.Function(...))) #572, #574") {
    import scala.collection.immutable.List
    val tree1 = Term.Apply(Term.Name("foo"), List(Term.Function(List(Term.Param(List(), Term.Name("i"), Some(Type.Name("Int")), None)), Lit.Unit())))
    val tree2 = Term.Apply(Term.Name("foo"), List(Term.Function(List(Term.Param(List(Mod.Implicit()), Term.Name("i"), Some(Type.Name("Int")), None)), Lit.Unit())))
    val tree3 = Term.Apply(Term.Name("foo"), List(Term.Function(List(Term.Param(List(), Term.Name("i"), None, None)), Lit.Unit())))
    assert(tree1.syntax == "foo { (i: Int) => () }")
    assert(tree2.syntax == "foo { implicit i: Int => () }")
    assert(tree3.syntax == "foo(i => ())")
  }

  test("macro defs #581") {
    assert(q"def f = macro g".syntax == "def f = macro g")
    assert(q"def f: Int = macro g".syntax == "def f: Int = macro g")
  }

  test("Pat.Interpolate syntax is correct #587") {
    val interpolate = Pat.Interpolate(
      Term.Name("q"),
      List(Lit.String("object "), Lit.String(" { .."), Lit.String(" }")),
      List(Pat.Var(Term.Name("name")), Pat.Var(Term.Name("stats")))
    )
    assert(interpolate.syntax === """q"object ${name} { ..${stats} }"""")
  }

  test("Importee.Rename") {
    assert(q"import a.{b=>c}".syntax == "import a.{b => c}")
  }

  test("show[Structure] should uppercase long literals suffix: '2l' -> '2L'") {
    assert(
      templStat("foo(1l, 1L)").structure ==
          """Term.Apply(Term.Name("foo"), List(Lit.Long(1L), Lit.Long(1L)))""")
    assert(q"val x = 1l".structure == q"val x = 1L".structure)
  }

  test("show[Structure] should lowercase float literals suffix: '0.01F' -> '0.01f'") {
    assert(
      templStat("foo(0.01f, 0.01F)").structure ==
          """Term.Apply(Term.Name("foo"), List(Lit.Float(0.01f), Lit.Float(0.01f)))""")
    assert(q"val x = 1f".structure == q"val x = 1F".structure)
  }

  test("show[Structure] should lowercase double literals suffix: '0.01D' -> '0.01d'") {
    assert(
      templStat("foo(0.02d, 0.02D, 0.02)").structure ==
          """Term.Apply(Term.Name("foo"), List(Lit.Double(0.02d), Lit.Double(0.02d), Lit.Double(0.02d)))""")
    assert(q"val x = 1d".structure == q"val x = 1D".structure)
    assert(q"val x = 1.0d".structure == q"val x = 1.0".structure)
  }

  test("#931 val `a b` = 2") {
    assert(q"val `a b` = 2".syntax == "val `a b` = 2")
  }

  test("#1661 Names outside ") {
    // Must start with either a letter or an operator
    assert(q"val `foo` = 2".syntax == "val foo = 2")
    assert(q"val `++++` = 2".syntax == "val ++++ = 2")
    assert(q"val `_+` = 2".syntax == "val `_+` = 2")

    // Non-leading operators are accepted only after underscores
    assert(q"val `a_+` = 2".syntax == "val a_+ = 2")
    assert(q"val `a_a_+` = 2".syntax == "val a_a_+ = 2")

    // Operators must not be followed by non-operators
    assert(q"val `+_a` = 2".syntax == "val `+_a` = 2")
    assert(q"val `a_++` = 2".syntax == "val a_++ = 2")
    assert(q"val `a_++a` = 2".syntax == "val `a_++a` = 2")

    // Lexical letters and digits can follow underscores
    assert(q"val `_a` = 2".syntax == "val _a = 2")
    assert(q"val `a_a` = 2".syntax == "val a_a = 2")

    // Non-operators must not be followed by operators
    assert(q"val `a+` = 2".syntax == "val `a+` = 2")
    assert(q"val `a-b` = 2".syntax == "val `a-b` = 2")
    assert(q"val `a:b` = 2".syntax == "val `a:b` = 2")

    // Comments must be handled carefully
    assert(q"val `/*` = 2".syntax == "val `/*` = 2")
    assert(q"val `//` = 2".syntax == "val `//` = 2")
    assert(q"val `a_//` = 2".syntax == "val `a_//` = 2")
  }
}

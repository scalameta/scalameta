import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class ScalaSuite extends ParseSuite {
  test("val x: Int (raw)") {
    val tree = templStat("val x: Int")
    assert(tree.show[Raw] === "Decl.Val(Nil, List(Pat.Var.Term(Term.Name(\"x\"))), Type.Name(\"Int\"))")
  }

  test("val x: Int (code)") {
    val tree = templStat("val x: Int")
    assert(tree.show[Code] === "val x: Int")
  }

  test("~(1 + 2) + ~x.y(z) + (~x).y(z)") {
    val tree = templStat("~(1 + 2) + ~x.y(z) + (~x).y(z)")
    assert(tree.show[Code] === "~(1 + 2) + ~x.y(z) + (~x).y(z)")
  }

  test("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)") {
    val tree = templStat("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)")
    assert(tree.show[Code] === "a + b + c && a + (b + c) && (a :: b :: c) && ((a :: b) :: c)")
  }

  test("(x map y).foo") {
    val tree = templStat("(x map y).foo")
    assert(tree.show[Code] === "(x map y).foo")
  }

  test("string literals with newlines and double quotes") {
    val tree = templStat("""{
      val x = QQQ
        x
      QQQ
      val y = "\""
    }""".replace("QQQ", "\"\"\""))
    assert(tree.show[Raw] === """Term.Block(List(Defn.Val(Nil, List(Pat.Var.Term(Term.Name("x"))), None, Lit.String("%n        x%n      ")), Defn.Val(Nil, List(Pat.Var.Term(Term.Name("y"))), None, Lit.String("\""))))""".replace("%n", escapedEOL))
    assert(tree.show[Code] === """
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
    assert(tree.show[Raw] === """Term.Block(List(Defn.Val(Nil, List(Pat.Var.Term(Term.Name("x"))), None, Term.Interpolate(Term.Name("q"), List(Lit.String("123 + "), Lit.String(" + "), Lit.String(" + 456")), List(Term.Name("x"), Term.Apply(Term.Name("foo"), List(Lit.Int(123)))))), Defn.Val(Nil, List(Pat.Var.Term(Term.Name("y"))), None, Lit.String("%n        $x%n        $y%n        ..$z%n      "))))""".replace("%n", escapedEOL))
    assert(tree.show[Code] === """
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
    assert(tree.show[Code] === """
      |foo.bar(bar) {
      |  baz
      |}
    """.trim.stripMargin)
  }

  test("Template.self stringifications") {
    assert(templStat("new { val x = 2 }").show[Code] === "new { val x = 2 }")
    assert(templStat("new { self => val x = 2 }").show[Code] === "new { self => val x = 2 }")
    assert(templStat("new { self: Int => val x = 2 }").show[Code] === "new { self: Int => val x = 2 }")
    assert(templStat("""
      new {
        val x = 2
        val y = 3
      }
    """).show[Code] === """
      |new {
      |  val x = 2
      |  val y = 3
      |}
    """.trim.stripMargin)
    assert(templStat("""
      new { self =>
        val x = 2
        val y = 3
      }
    """).show[Code] === """
      |new { self =>
      |  val x = 2
      |  val y = 3
      |}
    """.trim.stripMargin)
    assert(templStat("""
      new { self: Int =>
        val x = 2
        val y = 3
      }
    """).show[Code] === """
      |new { self: Int =>
      |  val x = 2
      |  val y = 3
      |}
    """.trim.stripMargin)
    assert(templStat("class B { x: B => }").show[Code] === "class B { x: B => }")
  }

  test("new X") {
    assert(templStat("new X").show[Code] === "new X")
    assert(templStat("new X {}").show[Code] === "new X {}")
  }

  test("ascribe and annotate") {
    assert(templStat("_: Int").show[Code] === "_: Int")
    assert(templStat("(_: Int) + 2").show[Code] === "(_: Int) + 2")
    assert(templStat("x: @foo").show[Code] === "x: @foo")
    assert(templStat("(x: @foo) + 2").show[Code] === "(x: @foo) + 2")
  }

  test("compound types") {
    assert(tpe("Foo").show[Code] === "Foo")
    assert(tpe("Foo {}").show[Code] === "Foo")
    assert(tpe("Foo { type T = Int }").show[Code] === "Foo { type T = Int }")
    assert(tpe("Foo { type T = Int; type U <: String }").show[Code] === "Foo { type T = Int; type U <: String }")
    assert(tpe("Foo with Bar").show[Code] === "Foo with Bar")
    // TODO: revisit this once we have trivia in place
    // assert(tpe("Foo with Bar {}").show[Code] === "Foo with Bar {}")
    assert(tpe("Foo with Bar {}").show[Code] === "Foo with Bar")
    assert(tpe("Foo with Bar { type T = Int }").show[Code] === "Foo with Bar { type T = Int }")
    assert(tpe("Foo with Bar { type T = Int; type U <: String }").show[Code] === "Foo with Bar { type T = Int; type U <: String }")
  }

  test("packages") {
    assert(source("package foo.bar; class C").show[Code] === s"package foo.bar${EOL}class C")
    assert(source("package foo.bar; class C; class D").show[Code] === s"package foo.bar${EOL}class C${EOL}class D")
    // TODO: revisit this once we have trivia in place
    // assert(source("package foo.bar { class C }").show[Code] === s"package foo.bar {${EOL}  class C${EOL}}")
    // assert(source("package foo.bar { class C; class D }").show[Code] === s"package foo.bar {${EOL}  class C${EOL}  class D${EOL}}")
    assert(source("package foo.bar { class C }").show[Code] === s"package foo.bar${EOL}class C")
    assert(source("package foo.bar { class C; class D }").show[Code] === s"package foo.bar${EOL}class C${EOL}class D")
  }

  test("type parameter mods") {
    assert(source("class C[@foo T]").show[Code] === "class C[@foo T]")
    assert(source("class C[+T]").show[Code] === "class C[+T]")
    assert(source("class C[@foo +T]").show[Code] === "class C[@foo +T]")
  }

  test("primary constructor mods") {
    assert(source("class C").show[Code] === "class C")
    assert(source("class C private").show[Code] === "class C private")
    assert(source("class C @foo(x)").show[Code] === "class C @foo(x)")
    assert(source("class C @foo(x) private").show[Code] === "class C @foo(x) private")
    assert(source("class C(x: Int)").show[Code] === "class C(x: Int)")
    assert(source("class C private (x: Int)").show[Code] === "class C private (x: Int)")
    assert(source("class C @foo(x) (x: Int)").show[Code] === "class C @foo(x) (x: Int)")
    assert(source("class C @foo(x) private (x: Int)").show[Code] === "class C @foo(x) private (x: Int)")
  }

  test("parentheses in patterns") {
    assert(templStat("x match { case (xs: List[Int]) :+ x => ??? }").show[Code] === """
      |x match {
      |  case (xs: List[Int]) :+ x => ???
      |}
    """.trim.stripMargin)
  }

  test("List(x, y) :: z") {
    assert(templStat("List(x, y) :: z").show[Code] == "List(x, y) :: z")
    assert(templStat("x match { case List(x, y) :: z => ??? }").show[Code] === """
      |x match {
      |  case List(x, y) :: z => ???
      |}
    """.trim.stripMargin)
  }

  test("secondary ctor - expr") {
    assert(source("class C(x: Int) { def this() = this(2) }").show[Code] === "class C(x: Int) { def this() = this(2) }")
  }

  test("secondary ctor - block") {
    assert(source("class C(x: Int) { def this() { this(2); println(\"OBLIVION!!!\") } }").show[Code] === """
      |class C(x: Int) {
      |  def this() {
      |    this(2)
      |    println("OBLIVION!!!")
      |  }
      |}
    """.trim.stripMargin)
  }

  test("case semicolons") {
    assert(templStat("x match { case y => foo1; foo2 }").show[Code] === """
      |x match {
      |  case y =>
      |    foo1
      |    foo2
      |}
    """.trim.stripMargin)
  }

  test("assorted literals") {
    assert(templStat("true").show[Code] === "true")
    assert(templStat("false").show[Code] === "false")
    assert(templStat("0").show[Code] === "0")
    assert(templStat("0l").show[Code] === "0L")
    assert(templStat("0L").show[Code] === "0L")
    assert(templStat("0f").show[Code] === "0.0f")
    assert(templStat("0F").show[Code] === "0.0f")
    assert(templStat("0.0").show[Code] === "0.0d")
    assert(templStat("0d").show[Code] === "0.0d")
    assert(templStat("0D").show[Code] === "0.0d")
    assert(templStat("'0'").show[Code] === "'0'")
    assert(templStat("\"0\"").show[Code] === "\"0\"")
    assert(templStat("'zero").show[Code] === "'zero")
    assert(templStat("null").show[Code] === "null")
    assert(templStat("()").show[Code] === "()")
  }

  test("context and view bounds") {
    assert(templStat("class C[T: List, U <% Int]").show[Code] === "class C[T: List, U <% Int]")
    assert(templStat("def m[T: List, U <% Int] = ???").show[Code] === "def m[T: List, U <% Int] = ???")
  }

  test("some tricky parenthesization") {
    assert(templStat("if (1) 2 else 3 + 4").show[Code] === "if (1) 2 else 3 + 4")
    assert(templStat("(if (1) 2 else 3) + 4").show[Code] === "(if (1) 2 else 3) + 4")
    assert(templStat("if (1) 2 else 3 match { case _ => }").show[Code] === s"if (1) 2 else 3 match {${EOL}  case _ =>${EOL}}")
    assert(templStat("(if (1) 2 else 3) match { case _ => }").show[Code] === s"(if (1) 2 else 3) match {${EOL}  case _ =>${EOL}}")
    assert(templStat("unit.toCheck += (() => body)").show[Code] === "unit.toCheck += (() => body)")
    assert(templStat("({ foo1; foo2 }).orElse(bar)").show[Code] === s"{${EOL}  foo1${EOL}  foo2${EOL}}.orElse(bar)")
    assert(templStat("(foo match { case _ => }).orElse(bar)").show[Code] === s"(foo match {${EOL}  case _ =>${EOL}}).orElse(bar)")
    assert(templStat("foo || (if (cond) bar else baz)").show[Code] === "foo || (if (cond) bar else baz)")
    assert(templStat("foo && (bar match { case _ => })").show[Code] === s"foo && (bar match {${EOL}  case _ =>${EOL}})")
    assert(templStat("\"foo \" + (if (cond) bar else baz)").show[Code] === "\"foo \" + (if (cond) bar else baz)")
    assert(templStat("foo match { case bar @ (_: T1 | _: T2) => }").show[Code] === s"foo match {${EOL}  case bar @ (_: T1 | _: T2) =>${EOL}}")
    assert(templStat("foo match { case A + B / C => }").show[Code] === s"foo match {${EOL}  case A + B / C =>${EOL}}")
    assert(templStat("foo match { case (A + B) / C => }").show[Code] === s"foo match {${EOL}  case (A + B) / C =>${EOL}}")
    assert(templStat("foo match { case A + (B / C) => }").show[Code] === s"foo match {${EOL}  case A + B / C =>${EOL}}")
    assert(templStat("foo match { case bar :: Nil :: Nil => }").show[Code] === s"foo match {${EOL}  case bar :: Nil :: Nil =>${EOL}}")
    assert(templStat("foo match { case (bar :: Nil) :: Nil => }").show[Code] === s"foo match {${EOL}  case (bar :: Nil) :: Nil =>${EOL}}")
    assert(templStat("@(foo @foo) class Bar").show[Code] === "@(foo @foo) class Bar")
    assert(templStat("(foo: Foo): @foo").show[Code] === "(foo: Foo): @foo")
    assert(templStat("type T = A + B / C").show[Code] === "type T = A + B / C")
    assert(templStat("type T = (A + B) / C").show[Code] === "type T = A + B / C")
    assert(templStat("type T = A + (B / C)").show[Code] === "type T = A + (B / C)")
    assert(templStat("type T = A :: B :: C").show[Code] === "type T = A :: B :: C")
    assert(templStat("type T = (A :: B) :: C").show[Code] === "type T = (A :: B) :: C")
    assert(templStat("foo match { case _: A | _: B => }").show[Code] === s"foo match {${EOL}  case _: A | _: B =>${EOL}}")
    assert(templStat("foo match { case _: A | _: B | _: C => }").show[Code] === s"foo match {${EOL}  case _: A | _: B | _: C =>${EOL}}")
  }

  test("more trickiness") {
    assert(templStat("def foo(bar_ : Int) = ???").show[Code] === "def foo(bar_ : Int) = ???")
    assert(templStat("class C[T_ : Foo]").show[Code] === "class C[T_ : Foo]")
    assert(templStat("val scala_ : NameType = ???").show[Code] === "val scala_ : NameType = ???")
  }

  test("class C extends (() => Int)") {
    assert(templStat("class C extends (() => Int)").show[Code] === "class C extends (() => Int)")
  }

  test("class C(x: Int)(implicit y: String, z: Boolean)") {
    assert(templStat("class C(x: Int)(implicit y: String, z: Boolean)").show[Code] === "class C(x: Int)(implicit y: String, z: Boolean)")
  }

  test("class C(var x: Int)") {
    assert(templStat("class C(var x: Int)").show[Code] === "class C(var x: Int)")
  }

  test("private/protected within something") {
    assert(templStat("""
      class C {
        private[this] val x = 1
        private[D] val y = 2
        protected[this] val z = 3
        protected[D] val w = 4
      }
    """).show[Code] === """
      |class C {
      |  private[this] val x = 1
      |  private[D] val y = 2
      |  protected[this] val z = 3
      |  protected[D] val w = 4
      |}
    """.stripMargin.trim)
  }

  test("case List(xs @ _*)") {
    val tree = pat("List(xs @ _*)")
    assert(tree.show[Raw] === "Pat.Extract(Term.Name(\"List\"), Nil, List(Pat.Bind(Pat.Var.Term(Term.Name(\"xs\")), Pat.Arg.SeqWildcard())))")
    assert(tree.show[Code] === "List(xs @ _*)")
  }

  test("package foo; class C; package baz { class D }") {
    val tree = source("package foo; class C; package baz { class D }")
    assert(tree.show[Raw] === "Source(List(Pkg(Term.Name(\"foo\"), List(Defn.Class(Nil, Type.Name(\"C\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), None)), Pkg(Term.Name(\"baz\"), List(Defn.Class(Nil, Type.Name(\"D\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), None))))))))")
    assert(tree.show[Code] === "package foo\nclass C\npackage baz {\n  class D\n}")
  }

  test("case `x`") {
    val tree1 = pat("`x`")
    assert(tree1.show[Raw] === "Term.Name(\"x\")")
    assert(tree1.show[Code] === "x")
    val tree2 = pat("f(`x`)")
    assert(tree2.show[Raw] === "Pat.Extract(Term.Name(\"f\"), Nil, List(Term.Name(\"x\")))")
    assert(tree2.show[Code] === "f(`x`)")
    val tree3 = pat("X")
    assert(tree3.show[Raw] === "Term.Name(\"X\")")
    assert(tree3.show[Code] === "X")
    val tree4 = pat("f(X)")
    assert(tree4.show[Raw] === "Pat.Extract(Term.Name(\"f\"), Nil, List(Term.Name(\"X\")))")
    assert(tree4.show[Code] === "f(X)")
  }

  test("case _: Int") {
    assert(pat("_: Int").show[Code] === "_: Int")
  }

  test("case _: t") {
    assert(pat("_: t").show[Code] === "_: t")
  }

  test("case _: F[t]") {
    assert(pat("_: F[t]").show[Code] === "_: F[t]")
  }

  test("case _: F[_]") {
    assert(pat("_: F[_]").show[Code] === "_: F[_]")
  }

  test("case _: (t Map u)") {
    assert(pat("_: (t Map u)").show[Code] === "_: (t Map u)")
  }

  test("constructors") {
    import scala.meta.internal.ast._
    val tree @ Defn.Class(_, _, _, primary, Template(_, _, _, Some(secondary :: Nil))) = templStat("class C(x: Int) { def this() = this(42) }")
    assert(tree.show[Code] === "class C(x: Int) { def this() = this(42) }")
    assert(primary.show[Code] === "(x: Int)")
    assert(secondary.show[Code] === "def this() = this(42)")
    assert(tree.toString === "class C(x: Int) { def this() = this(42) }")
    assert(primary.toString === "def this(x: Int)")
    assert(secondary.toString === "def this() = this(42)")
  }

  test("lazy printing") {
    import scala.meta.internal.ast._
    val emptyCtor = Ctor.Primary(Nil, Ctor.Name("this"), Nil)
    val lazyStats = templStat("class C") #:: ??? #:: Stream.empty
    val lazyTemplate = Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(lazyStats))
    val tree1 = Defn.Class(Nil, Type.Name("test"), Nil, emptyCtor, lazyTemplate)
    assert(tree1.toString === "class test { ... }")
    val tree2 = Defn.Trait(Nil, Type.Name("test"), Nil, emptyCtor, lazyTemplate)
    assert(tree2.toString === "trait test { ... }")
    val tree3 = Defn.Object(Nil, Term.Name("test"), emptyCtor, lazyTemplate)
    assert(tree3.toString === "object test { ... }")
    val tree4 = Pkg(Term.Name("test"), lazyStats)
    assert(tree4.toString === "package test { ... }")
    val tree5 = Pkg.Object(Nil, Term.Name("test"), emptyCtor, lazyTemplate)
    assert(tree5.toString === "package object test { ... }")
  }

  test("smart case printing - oneliner in one line") {
    import scala.meta.internal.ast._
    val Term.Match(_, case1 :: Nil) = templStat("??? match { case x => x }")
    assert(case1.toString === "case x => x")
  }

  test("smart case printing - oneliner in multiple lines") {
    import scala.meta.internal.ast._
    val Term.Match(_, case1 :: case2 :: Nil) = templStat("??? match { case x => x; case List(x, y) => println(x); println(y) }")
    assert(case1.toString === """
      |case x =>
      |  x
    """.trim.stripMargin)
    assert(case2.toString === """
      |case List(x, y) =>
      |  println(x)
      |  println(y)
    """.trim.stripMargin)
  }

  test("xml literals") {
    val tree = term("<foo>{bar}</foo>")
    assert(tree.show[Raw] === """Term.Interpolate(Term.Name("xml"), List(Lit.String("<foo>{bar}</foo>")), Nil)""")
    assert(tree.show[Code] === """ xml"<foo>{bar}</foo>" """.trim)
  }
}
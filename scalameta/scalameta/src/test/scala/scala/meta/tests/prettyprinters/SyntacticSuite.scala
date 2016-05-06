package scala.meta.tests
package prettyprinters

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.ast._

class SyntacticSuite extends scala.meta.tests.parsers.ParseSuite {
  override def term(code: String)(implicit dialect: Dialect) = super.term(code).resetAllOrigins
  override def pat(code: String)(implicit dialect: Dialect) = super.pat(code).resetAllOrigins
  override def tpe(code: String)(implicit dialect: Dialect) = super.tpe(code).resetAllOrigins
  override def topStat(code: String)(implicit dialect: Dialect) = super.topStat(code).resetAllOrigins
  override def templStat(code: String)(implicit dialect: Dialect) = super.templStat(code).resetAllOrigins
  override def blockStat(code: String)(implicit dialect: Dialect) = super.blockStat(code).resetAllOrigins
  override def caseClause(code: String)(implicit dialect: Dialect) = super.caseClause(code).resetAllOrigins
  override def source(code: String)(implicit dialect: Dialect) = super.source(code).resetAllOrigins
  implicit class XtensionResetOrigin[T <: Tree](tree: T) {
    // NOTE: Ensures that neither the given tree nor its subtrees have their origins set.
    // This is necessary to force prettyprinting as opposed to reusing original syntax.
    def resetAllOrigins: T = {
      tree.transform{ case tree: Tree => tree.withOrigin(Origin.None) }.asInstanceOf[T]
    }
  }

  test("val x: Int (raw)") {
    val tree = templStat("val x: Int")
    assert(tree.show[Structure] === "Decl.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"x\"))), Type.Name(\"Int\"))")
  }

  test("val x: Int (code)") {
    val tree = templStat("val x: Int")
    assert(tree.show[Syntax] === "val x: Int")
  }

  test("~(1 + 2) + ~x.y(z) + (~x).y(z)") {
    val tree = templStat("~(1 + 2) + ~x.y(z) + (~x).y(z)")
    assert(tree.show[Syntax] === "~(1 + 2) + ~x.y(z) + (~x).y(z)")
  }

  test("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)") {
    val tree = templStat("(a + b + c) && (a + (b + c)) && (a :: b :: c) && ((a :: b) :: c)")
    assert(tree.show[Syntax] === "a + b + c && a + (b + c) && (a :: b :: c) && ((a :: b) :: c)")
  }

  test("(x map y).foo") {
    val tree = templStat("(x map y).foo")
    assert(tree.show[Syntax] === "(x map y).foo")
  }

  test("string literals with newlines and double quotes") {
    val tree = templStat("""{
      val x = QQQ
        x
      QQQ
      val y = "\""
    }""".replace("QQQ", "\"\"\""))
    assert(tree.show[Structure] === """Term.Block(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("x"))), None, Lit("%n        x%n      ")), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("y"))), None, Lit("\""))))""".replace("%n", escapedEOL))
    assert(tree.show[Syntax] === """
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
    assert(tree.show[Structure] === """Term.Block(Seq(Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("x"))), None, Term.Interpolate(Term.Name("q"), Seq(Lit("123 + "), Lit(" + "), Lit(" + 456")), Seq(Term.Name("x"), Term.Apply(Term.Name("foo"), Seq(Lit(123)))))), Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("y"))), None, Lit("%n        $x%n        $y%n        ..$z%n      "))))""".replace("%n", escapedEOL))
    assert(tree.show[Syntax] === """
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
    assert(tree.show[Syntax] === """
      |foo.bar(bar) {
      |  baz
      |}
    """.trim.stripMargin)
  }

  test("Template.self stringifications") {
    assert(templStat("new { val x = 2 }").show[Syntax] === "new { val x = 2 }")
    assert(templStat("new { self => val x = 2 }").show[Syntax] === "new { self => val x = 2 }")
    assert(templStat("new { self: Int => val x = 2 }").show[Syntax] === "new { self: Int => val x = 2 }")
    assert(templStat("""
      new {
        val x = 2
        val y = 3
      }
    """).show[Syntax] === """
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
    """).show[Syntax] === """
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
    """).show[Syntax] === """
      |new { self: Int =>
      |  val x = 2
      |  val y = 3
      |}
    """.trim.stripMargin)
    assert(templStat("class B { x: B => }").show[Syntax] === "class B { x: B => }")
  }

  test("new X") {
    assert(templStat("new X").show[Syntax] === "new X")
    assert(templStat("new X {}").show[Syntax] === "new X {}")
  }

  test("ascribe and annotate") {
    assert(templStat("_: Int").show[Syntax] === "_: Int")
    assert(templStat("(_: Int) + 2").show[Syntax] === "(_: Int) + 2")
    assert(templStat("x: @foo").show[Syntax] === "x: @foo")
    assert(templStat("(x: @foo) + 2").show[Syntax] === "(x: @foo) + 2")
  }

  test("compound types") {
    assert(tpe("Foo").show[Syntax] === "Foo")
    assert(tpe("Foo {}").show[Syntax] === "Foo")
    assert(tpe("Foo { type T = Int }").show[Syntax] === "Foo { type T = Int }")
    assert(tpe("Foo { type T = Int; type U <: String }").show[Syntax] === "Foo { type T = Int; type U <: String }")
    assert(tpe("Foo with Bar").show[Syntax] === "Foo with Bar")
    // TODO: revisit this once we have trivia in place
    // assert(tpe("Foo with Bar {}").show[Syntax] === "Foo with Bar {}")
    assert(tpe("Foo with Bar {}").show[Syntax] === "Foo with Bar")
    assert(tpe("Foo with Bar { type T = Int }").show[Syntax] === "Foo with Bar { type T = Int }")
    assert(tpe("Foo with Bar { type T = Int; type U <: String }").show[Syntax] === "Foo with Bar { type T = Int; type U <: String }")
  }

  test("packages") {
    assert(source("package foo.bar; class C").show[Syntax] === s"package foo.bar${EOL}class C")
    assert(source("package foo.bar; class C; class D").show[Syntax] === s"package foo.bar${EOL}class C${EOL}class D")
    // TODO: revisit this once we have trivia in place
    // assert(source("package foo.bar { class C }").show[Syntax] === s"package foo.bar {${EOL}  class C${EOL}}")
    // assert(source("package foo.bar { class C; class D }").show[Syntax] === s"package foo.bar {${EOL}  class C${EOL}  class D${EOL}}")
    assert(source("package foo.bar { class C }").show[Syntax] === s"package foo.bar${EOL}class C")
    assert(source("package foo.bar { class C; class D }").show[Syntax] === s"package foo.bar${EOL}class C${EOL}class D")
  }

  test("type parameter mods") {
    assert(source("class C[@foo T]").show[Syntax] === "class C[@foo T]")
    assert(source("class C[+T]").show[Syntax] === "class C[+T]")
    assert(source("class C[@foo +T]").show[Syntax] === "class C[@foo +T]")
  }

  test("primary constructor mods") {
    assert(source("class C").show[Syntax] === "class C")
    assert(source("class C private").show[Syntax] === "class C private")
    assert(source("class C @foo(x)").show[Syntax] === "class C @foo(x)")
    assert(source("class C @foo(x) private").show[Syntax] === "class C @foo(x) private")
    assert(source("class C(x: Int)").show[Syntax] === "class C(x: Int)")
    assert(source("class C private (x: Int)").show[Syntax] === "class C private (x: Int)")
    assert(source("class C @foo(x) (x: Int)").show[Syntax] === "class C @foo(x) (x: Int)")
    assert(source("class C @foo(x) private (x: Int)").show[Syntax] === "class C @foo(x) private (x: Int)")
  }

  test("parentheses in patterns") {
    assert(templStat("x match { case (xs: List[Int]) :+ x => ??? }").show[Syntax] === """
      |x match {
      |  case (xs: List[Int]) :+ x => ???
      |}
    """.trim.stripMargin)
  }

  test("List(x, y) :: z") {
    assert(templStat("List(x, y) :: z").show[Syntax] == "List(x, y) :: z")
    assert(templStat("x match { case List(x, y) :: z => ??? }").show[Syntax] === """
      |x match {
      |  case List(x, y) :: z => ???
      |}
    """.trim.stripMargin)
  }

  test("secondary ctor - expr") {
    assert(source("class C(x: Int) { def this() = this(2) }").show[Syntax] === "class C(x: Int) { def this() = this(2) }")
  }

  test("secondary ctor - block") {
    assert(source("class C(x: Int) { def this() { this(2); println(\"OBLIVION!!!\") } }").show[Syntax] === """
      |class C(x: Int) {
      |  def this() {
      |    this(2)
      |    println("OBLIVION!!!")
      |  }
      |}
    """.trim.stripMargin)
  }

  test("case semicolons") {
    assert(templStat("x match { case y => foo1; foo2 }").show[Syntax] === """
      |x match {
      |  case y =>
      |    foo1
      |    foo2
      |}
    """.trim.stripMargin)
  }

  test("assorted literals") {
    assert(templStat("true").show[Syntax] === "true")
    assert(templStat("false").show[Syntax] === "false")
    assert(templStat("0").show[Syntax] === "0")
    assert(templStat("0l").show[Syntax] === "0L")
    assert(templStat("0L").show[Syntax] === "0L")
    assert(templStat("0f").show[Syntax] === "0.0F")
    assert(templStat("0F").show[Syntax] === "0.0F")
    assert(templStat("0.0").show[Syntax] === "0.0")
    assert(templStat("0d").show[Syntax] === "0.0")
    assert(templStat("0D").show[Syntax] === "0.0")
    assert(templStat("'0'").show[Syntax] === "'0'")
    assert(templStat("\"0\"").show[Syntax] === "\"0\"")
    assert(templStat("'zero").show[Syntax] === "'zero")
    assert(templStat("null").show[Syntax] === "null")
    assert(templStat("()").show[Syntax] === "()")
  }

  test("context and view bounds") {
    assert(templStat("class C[T: List, U <% Int]").show[Syntax] === "class C[T: List, U <% Int]")
    assert(templStat("def m[T: List, U <% Int] = ???").show[Syntax] === "def m[T: List, U <% Int] = ???")
  }

  test("some tricky parenthesization") {
    assert(templStat("if (1) 2 else 3 + 4").show[Syntax] === "if (1) 2 else 3 + 4")
    assert(templStat("(if (1) 2 else 3) + 4").show[Syntax] === "(if (1) 2 else 3) + 4")
    assert(templStat("if (1) 2 else 3 match { case _ => }").show[Syntax] === s"if (1) 2 else 3 match {${EOL}  case _ =>${EOL}}")
    assert(templStat("(if (1) 2 else 3) match { case _ => }").show[Syntax] === s"(if (1) 2 else 3) match {${EOL}  case _ =>${EOL}}")
    assert(templStat("unit.toCheck += (() => body)").show[Syntax] === "unit.toCheck += (() => body)")
    assert(templStat("({ foo1; foo2 }).orElse(bar)").show[Syntax] === s"{${EOL}  foo1${EOL}  foo2${EOL}}.orElse(bar)")
    assert(templStat("(foo match { case _ => }).orElse(bar)").show[Syntax] === s"(foo match {${EOL}  case _ =>${EOL}}).orElse(bar)")
    assert(templStat("foo || (if (cond) bar else baz)").show[Syntax] === "foo || (if (cond) bar else baz)")
    assert(templStat("foo && (bar match { case _ => })").show[Syntax] === s"foo && (bar match {${EOL}  case _ =>${EOL}})")
    assert(templStat("\"foo \" + (if (cond) bar else baz)").show[Syntax] === "\"foo \" + (if (cond) bar else baz)")
    assert(templStat("foo match { case bar @ (_: T1 | _: T2) => }").show[Syntax] === s"foo match {${EOL}  case bar @ (_: T1 | _: T2) =>${EOL}}")
    assert(templStat("foo match { case A + B / C => }").show[Syntax] === s"foo match {${EOL}  case A + B / C =>${EOL}}")
    assert(templStat("foo match { case (A + B) / C => }").show[Syntax] === s"foo match {${EOL}  case (A + B) / C =>${EOL}}")
    assert(templStat("foo match { case A + (B / C) => }").show[Syntax] === s"foo match {${EOL}  case A + B / C =>${EOL}}")
    assert(templStat("foo match { case bar :: Nil :: Nil => }").show[Syntax] === s"foo match {${EOL}  case bar :: Nil :: Nil =>${EOL}}")
    assert(templStat("foo match { case (bar :: Nil) :: Nil => }").show[Syntax] === s"foo match {${EOL}  case (bar :: Nil) :: Nil =>${EOL}}")
    assert(templStat("@(foo @foo) class Bar").show[Syntax] === "@(foo @foo) class Bar")
    assert(templStat("(foo: Foo): @foo").show[Syntax] === "(foo: Foo): @foo")
    assert(templStat("type T = A + B / C").show[Syntax] === "type T = A + B / C")
    assert(templStat("type T = (A + B) / C").show[Syntax] === "type T = A + B / C")
    assert(templStat("type T = A + (B / C)").show[Syntax] === "type T = A + (B / C)")
    assert(templStat("type T = A :: B :: C").show[Syntax] === "type T = A :: B :: C")
    assert(templStat("type T = (A :: B) :: C").show[Syntax] === "type T = (A :: B) :: C")
    assert(templStat("foo match { case _: A | _: B => }").show[Syntax] === s"foo match {${EOL}  case _: A | _: B =>${EOL}}")
    assert(templStat("foo match { case _: A | _: B | _: C => }").show[Syntax] === s"foo match {${EOL}  case _: A | _: B | _: C =>${EOL}}")
  }

  test("more trickiness") {
    assert(templStat("def foo(bar_ : Int) = ???").show[Syntax] === "def foo(bar_ : Int) = ???")
    assert(templStat("class C[T_ : Foo]").show[Syntax] === "class C[T_ : Foo]")
    assert(templStat("val scala_ : NameType = ???").show[Syntax] === "val scala_ : NameType = ???")
  }

  test("class C extends (() => Int)") {
    assert(templStat("class C extends (() => Int)").show[Syntax] === "class C extends (() => Int)")
  }

  test("class C(x: Int)(implicit y: String, z: Boolean)") {
    assert(templStat("class C(x: Int)(implicit y: String, z: Boolean)").show[Syntax] === "class C(x: Int)(implicit y: String, z: Boolean)")
  }

  test("class C(var x: Int)") {
    assert(templStat("class C(var x: Int)").show[Syntax] === "class C(var x: Int)")
  }

  test("private/protected within something") {
    assert(templStat("""
      class C {
        private[this] val x = 1
        private[D] val y = 2
        protected[this] val z = 3
        protected[D] val w = 4
      }
    """).show[Syntax] === """
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
    assert(tree.show[Structure] === "Pat.Extract(Term.Name(\"List\"), Nil, Seq(Pat.Bind(Pat.Var.Term(Term.Name(\"xs\")), Pat.Arg.SeqWildcard())))")
    assert(tree.show[Syntax] === "List(xs @ _*)")
  }

  test("case List[t](xs @ _*)") {
    val tree = pat("List[t](xs @ _*)")
    assert(tree.show[Structure] === "Pat.Extract(Term.Name(\"List\"), Seq(Pat.Var.Type(Type.Name(\"t\"))), Seq(Pat.Bind(Pat.Var.Term(Term.Name(\"xs\")), Pat.Arg.SeqWildcard())))")
    assert(tree.show[Syntax] === "List[t](xs @ _*)")
  }

  test("case List[_](xs @ _*)") {
    val tree = pat("List[_](xs @ _*)")
    assert(tree.show[Structure] === "Pat.Extract(Term.Name(\"List\"), Seq(Pat.Type.Wildcard()), Seq(Pat.Bind(Pat.Var.Term(Term.Name(\"xs\")), Pat.Arg.SeqWildcard())))")
    assert(tree.show[Syntax] === "List[_](xs @ _*)")
  }

  test("package foo; class C; package baz { class D }") {
    val tree = source("package foo; class C; package baz { class D }")
    assert(tree.show[Structure] === "Source(Seq(Pkg(Term.Name(\"foo\"), Seq(Defn.Class(Nil, Type.Name(\"C\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), None)), Pkg(Term.Name(\"baz\"), Seq(Defn.Class(Nil, Type.Name(\"D\"), Nil, Ctor.Primary(Nil, Ctor.Ref.Name(\"this\"), Nil), Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), None))))))))")
    assert(tree.show[Syntax] === "package foo\nclass C\npackage baz {\n  class D\n}")
  }

  test("case `x`") {
    val tree1 = pat("`x`")
    assert(tree1.show[Structure] === "Term.Name(\"x\")")
    val tree2 = pat("f(`x`)")
    assert(tree2.show[Structure] === "Pat.Extract(Term.Name(\"f\"), Nil, Seq(Term.Name(\"x\")))")
    assert(tree2.show[Syntax] === "f(`x`)")
    val tree3 = pat("X")
    assert(tree3.show[Structure] === "Term.Name(\"X\")")
    assert(tree3.show[Syntax] === "X")
    val tree4 = pat("f(X)")
    assert(tree4.show[Structure] === "Pat.Extract(Term.Name(\"f\"), Nil, Seq(Term.Name(\"X\")))")
    assert(tree4.show[Syntax] === "f(X)")
  }

  test("case _: Int") {
    assert(pat("_: Int").show[Syntax] === "_: Int")
  }

  test("case _: t") {
    assert(pat("_: t").show[Syntax] === "_: t")
  }

  test("case _: F[t]") {
    assert(pat("_: F[t]").show[Syntax] === "_: F[t]")
  }

  test("case _: F[_]") {
    assert(pat("_: F[_]").show[Syntax] === "_: F[_]")
  }

  test("case _: (t Map u)") {
    // TODO: fixme
    // assert(pat("_: (t Map u)").show[Syntax] === "_: (t Map u)")
  }

  test("constructors") {
    val tree @ Defn.Class(_, _, _, primary, Template(_, _, _, Some(secondary :: Nil))) = templStat("class C(x: Int) { def this() = this(42) }")
    assert(tree.show[Syntax] === "class C(x: Int) { def this() = this(42) }")
    assert(primary.show[Syntax] === "(x: Int)")
    assert(secondary.show[Syntax] === "def this() = this(42)")
    assert(tree.toString === "class C(x: Int) { def this() = this(42) }")
    assert(primary.toString === "def this(x: Int)")
    assert(secondary.toString === "def this() = this(42)")
  }

  test("lazy printing") {
    val emptyCtor = Ctor.Primary(Nil, Ctor.Name("this"), Nil)
    val lazyStats = templStat("class C") #:: ??? #:: Stream.empty
    val lazyTemplate = Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(lazyStats))
    val tree1 = Defn.Class(Nil, Type.Name("test"), Nil, emptyCtor, lazyTemplate)
    assert(tree1.toString === "class test { ... }")
    val tree2 = Defn.Trait(Nil, Type.Name("test"), Nil, emptyCtor, lazyTemplate)
    assert(tree2.toString === "trait test { ... }")
    val tree3 = Defn.Object(Nil, Term.Name("test"), lazyTemplate)
    assert(tree3.toString === "object test { ... }")
    val tree4 = Pkg(Term.Name("test"), lazyStats)
    assert(tree4.toString === "package test { ... }")
    val tree5 = Pkg.Object(Nil, Term.Name("test"), lazyTemplate)
    assert(tree5.toString === "package object test { ... }")
  }

  test("smart case printing - oneliner in one line") {
    val Term.Match(_, case1 :: Nil) = templStat("??? match { case x => x }")
    assert(case1.toString === "case x => x")
  }

  test("smart case printing - oneliner in multiple lines") {
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
    assert(tree.show[Structure] === """Term.Xml(Seq(Lit("<foo>{bar}</foo>")), Nil)""")
    assert(tree.show[Syntax] === "<foo>{bar}</foo>")
  }

  test("empty-arglist application") {
    val tree = term("foo.toString()")
    assert(tree.show[Structure] === "Term.Apply(Term.Select(Term.Name(\"foo\"), Term.Name(\"toString\")), Nil)")
    assert(tree.show[Syntax] === "foo.toString()")
  }

  test("type parameters with type bounds") {
    val Defn.Def(_, _, List(tree), _, _, _) = templStat("def foo[T <: Int] = ???")
    assert(tree.show[Structure] === "Type.Param(Nil, Type.Name(\"T\"), Nil, Type.Bounds(None, Some(Type.Name(\"Int\"))), Nil, Nil)")
    assert(tree.show[Syntax] === "T <: Int")
  }
}
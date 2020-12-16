package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.tests.parsers.BasePositionSuite

class TokensPositionSuite extends BasePositionSuite(dialects.Scala213) {
  check[Enumerator](
    "`a` <- b",
    """|Term.Name `a`
       |""".stripMargin
  )
  check[Enumerator]("a = 1")
  check[Enumerator]("if x")
  check[Case](
    "case `a` =>",
    """|Term.Name `a`
       |Term.Block case `a` =>@@
       |""".stripMargin
  )
  check[Case](
    "case `a` :: `b` :: _ =>",
    """|Pat.ExtractInfix `a` :: `b` :: _
       |Term.Name `a`
       |Pat.ExtractInfix `b` :: _
       |Term.Name `b`
       |Term.Block case `a` :: `b` :: _ =>@@
       |""".stripMargin
  )
  check[Case](
    "case a b `c` =>",
    """|Pat.ExtractInfix a b `c`
       |Term.Name `c`
       |Term.Block case a b `c` =>@@
       |""".stripMargin
  )
  check[Case](
    "case _ op (a | b) =>",
    """|Pat.ExtractInfix _ op (a | b)
       |Pat.Alternative (a | b)
       |Term.Block case _ op (a | b) =>@@
       |""".stripMargin
  )
  check[Case](
    "case x `.y` () =>",
    """|Pat.ExtractInfix x `.y` ()
       |Term.Name `.y`
       |Term.Block case x `.y` () =>@@
       |""".stripMargin
  )
  check[Case](
    "case a if p =>",
    """|Term.Block case a if p =>@@
       |""".stripMargin
  )
  check[Case]("case _ => ()")
  check[Case](
    "case _ => {}",
    """|Term.Block {}
       |""".stripMargin
  )

  check[Type]("B")
  check[Type]("a.B")
  check[Type]("a#B")
  check[Type]("this.type", "Term.This this")
  check[Type]("t.type")
  check[Type]("F[T]")
  check[Type]("K Map V")
  check[Type]("() => B")
  check[Type]("A => B")
  check[Type]("(A, B) => C")
  check[Type]("(A, B)")
  check[Type]("A with B")
  check[Type]("A & B")
  check[Type](
    "A { def f: B }",
    """|Decl.Def def f: B
       |""".stripMargin
  )
  check[Type]("A{}")
  check[Type](
    "{ def f: B }",
    """|Decl.Def def f: B
       |""".stripMargin
  )
  check[Type](
    "A forSome { type T }",
    """|Decl.Type type T
       |Type.Bounds A forSome { type T @@}
       |""".stripMargin
  )
  check[Type](
    "T @A",
    """|Mod.Annot @A
       |""".stripMargin
  )
  check[Type](
    "_",
    """|Type.Bounds _@@
       |""".stripMargin
  )
  check[Type](
    "_ >: A <: B",
    """|Type.Bounds >: A <: B
       |""".stripMargin
  )
  check[Type](
    "_ <: B",
    """|Type.Bounds <: B
       |""".stripMargin
  )
  check[Type](
    "_ >: A",
    """|Type.Bounds >: A
       |""".stripMargin
  )
  check[Type]("=> T")
  check[Type]("Any*")
  check[Stat](
    "def f[A <% B[A]]: C",
    """|Type.Param A <% B[A]
       |Type.Bounds def f[A @@<% B[A]]: C
       |Type.Apply B[A]
       |""".stripMargin
  )
  check[Stat](
    "def f[A: B]: C",
    """|Type.Param A: B
       |Type.Bounds def f[A@@: B]: C
       |""".stripMargin
  )
  check[Stat](
    "def f[A : B : C]: D",
    """|Type.Param A : B : C
       |Type.Bounds def f[A @@: B : C]: D
       |""".stripMargin
  )

  check[Stat](
    "(a): @A",
    """|Term.Name (a)
       |Mod.Annot @A
       |""".stripMargin
  )
  check[Stat](
    "(f)((((a))))",
    """|Term.Name (f)
       |Term.Name (((a)))
       |""".stripMargin
  )
  check[Stat](
    "(f)((a))",
    """|Term.Name (f)
       |Term.Name (a)
       |""".stripMargin
  )
  check[Stat](
    "(f)({ case a => a })",
    """|Term.Name (f)
       |Term.PartialFunction { case a => a }
       |Case case a => a
       |""".stripMargin
  )
  check[Stat](
    "(f)({ x })",
    """|Term.Name (f)
       |Term.Block { x }
       |""".stripMargin
  )
  check[Stat](
    "(a) op (b)",
    """|Term.Name (a)
       |Term.Name (b)
       |""".stripMargin
  )
  check[Stat](
    "(a, b) op (c, d)",
    """|Term.Tuple (a, b)
       |""".stripMargin
  )
  check[Stat](
    "(a, b) op ((c, d))",
    """|Term.Tuple (a, b)
       |Term.Tuple ((c, d))
       |""".stripMargin
  )
  check[Stat]("1 + 1")
  check[Stat]("a f ()")
  check[Stat](
    "a f (b)",
    """|Term.Name (b)
       |""".stripMargin
  )
  check[Stat](
    "(f) [A,B]",
    """|Term.Name (f)
       |""".stripMargin
  )
  check[Stat](
    "(f) [A]",
    """|Term.Name (f)
       |""".stripMargin
  )
  check[Stat](
    "- (a)",
    """|Term.Name (a)
       |""".stripMargin
  )
  check[Stat](
    "(a): (A)",
    """|Term.Name (a)
       |Type.Name (A)
       |""".stripMargin
  )
  check[Stat](
    "(a) = (b)",
    """|Term.Name (a)
       |Term.Name (b)
       |""".stripMargin
  )
  check[Stat](
    "{ (a); (b) }",
    """|Term.Name (a)
       |Term.Name (b)
       |""".stripMargin
  )
  check[Stat](
    "do {d} while (p)",
    """|Term.Block {d}
       |""".stripMargin
  )
  check[Stat](
    "(f) _",
    """|Term.Name (f)
       |""".stripMargin
  )
  check[Stat](
    "for { x <- xs } (f)",
    """|Enumerator.Generator x <- xs
       |Term.Name (f)
       |""".stripMargin
  )
  check[Stat](
    "for { x <- xs } yield (f)",
    """|Enumerator.Generator x <- xs
       |Term.Name (f)
       |""".stripMargin
  )
  check[Stat](
    "((a), (b)) => (a)",
    """|Term.Param (a)
       |Term.Name (a)
       |Term.Param (b)
       |Term.Name (b)
       |Term.Name (a)
       |""".stripMargin
  )
  check[Stat](
    "if (p) (t) else (f)",
    """|Term.Name (t)
       |Term.Name (f)
       |""".stripMargin
  )
  check[Stat](
    "if (p) (t)",
    """|Term.Name (t)
       |Lit.Unit if (p) (t)@@
       |""".stripMargin
  )
  check[Stat](
    "if (p) if (p2) t",
    """|Term.If if (p2) t
       |Lit.Unit if (p) if (p2) t@@
       |Lit.Unit if (p) if (p2) t@@
       |""".stripMargin
  )
  check[Stat](
    "if (p) {}",
    """|Term.Block {}
       |Lit.Unit if (p) {}@@
       |""".stripMargin
  )
  check[Stat](
    """ s"start ${(a)} end"""",
    """|Term.Name (a)
       |""".stripMargin
  )
  check[Stat](
    "(a) match { case x => x }",
    """|Term.Name (a)
       |Case case x => x
       |""".stripMargin
  )
  check[Stat](
    "new (A)",
    """|Init (A)
       |Type.Name (A)
       |""".stripMargin
  )
  check[Stat](
    "new (A){}",
    """|Template (A){}
       |Init (A)
       |Type.Name (A)
       |Self new (A){@@}
       |""".stripMargin
  )
  check[Stat](
    "def f(a: A = (da)): A",
    """|Term.Param a: A = (da)
       |Term.Name (da)
       |""".stripMargin
  )
  check[Stat](
    "{ case x => x; case y => y }",
    """|Case case x => x;
       |Case case y => y
       |""".stripMargin
  )
  check[Stat](
    "f((x): _*)",
    """|Term.Repeated (x): _*
       |Term.Name (x)
       |""".stripMargin
  )
  check[Stat](
    "return (a)",
    """|Term.Name (a)
       |""".stripMargin
  )
  check[Stat](
    "(a).b",
    """|Term.Name (a)
       |""".stripMargin
  )
  check[Stat](
    "a.super[B].c",
    """|Term.Super a.super[B]
       |""".stripMargin
  )
  check[Stat]("a.this")
  check[Stat](
    "throw (e)",
    """|Term.Name (e)
       |""".stripMargin
  )
  check[Stat](
    "try (f) catch { case x => x; case y => y } finally { }",
    """|Term.Name (f)
       |Case case x => x;
       |Case case y => y
       |Term.Block { }
       |""".stripMargin
  )
  check[Stat]("try ()")
  check[Stat](
    "try (true, false)",
    """|Term.Tuple (true, false)
       |""".stripMargin
  )
  check[Stat](
    "try {1 + 2}.toString",
    """|Term.Select {1 + 2}.toString
       |Term.Block {1 + 2}
       |Term.ApplyInfix 1 + 2
       |""".stripMargin
  )
  check[Stat](
    "try (a.b).c",
    """|Term.Select (a.b).c
       |Term.Select (a.b)
       |""".stripMargin
  )
  check[Stat](
    "try (f) catch (h) finally { }",
    """|Term.Name (f)
       |Term.Name (h)
       |Term.Block { }
       |""".stripMargin
  )
  check[Stat](
    "try (true, false) catch (h) finally (1, 2)",
    """|Term.Tuple (true, false)
       |Term.Name (h)
       |Term.Tuple (1, 2)
       |""".stripMargin
  )
  check[Stat](
    "try (a.b).c catch (h) finally (1, 2)",
    """|Term.Select (a.b).c
       |Term.Select (a.b)
       |Term.Name (h)
       |Term.Tuple (1, 2)
       |""".stripMargin
  )
  check[Stat](
    "((a), (b))",
    """|Term.Name (a)
       |Term.Name (b)
       |""".stripMargin
  )
  check[Stat](
    "while (p) {d}",
    """|Term.Block {d}
       |""".stripMargin
  )
  check[Stat]("<a>b{c}d</a>")
  check[Stat]("(x)")
  check[Stat]("(_)")

  check[Source](
    "package a",
    """|Pkg package a
       |""".stripMargin
  )
  check[Source](
    "package a.b",
    """|Pkg package a.b
       |Term.Select a.b
       |""".stripMargin
  )

  check[Stat](
    "import a.b",
    """|Importer a.b
       |""".stripMargin
  )
  check[Stat](
    "import a.b, c.d",
    """|Importer a.b
       |Importer c.d
       |""".stripMargin
  )
  check[Stat](
    "import a._",
    """|Importer a._
       |""".stripMargin
  )
  check[Stat](
    "import a.{ b, c }",
    """|Importer a.{ b, c }
       |""".stripMargin
  )
  check[Stat](
    "import a.{ b => c }",
    """|Importer a.{ b => c }
       |Importee.Rename b => c
       |""".stripMargin
  )
  check[Stat](
    "import a.{ b => _ }",
    """|Importer a.{ b => _ }
       |Importee.Unimport b => _
       |""".stripMargin
  )

  // Decl
  check[Stat]("val a: Int")
  check[Stat]("var b: Long")
  check[Stat]("def f: String")
  check[Stat](
    "type T",
    """|Type.Bounds type T@@
       |""".stripMargin
  )

  check[Stat](
    "class A { def this(a: A) = this() }",
    """|Ctor.Primary class A @@{ def this(a: A) = this() }
       |Template { def this(a: A) = this() }
       |Self class A { @@def this(a: A) = this() }
       |Ctor.Secondary def this(a: A) = this()
       |Name.Anonymous this
       |Init this()
       |Type.Singleton this
       |Term.This this
       |Name.Anonymous this
       |""".stripMargin
  )
  check[Stat]("def f = macro m")
  check[Stat](
    "class A private (b: B)",
    """|Ctor.Primary private (b: B)
       |Template class A private (b: B)@@
       |Self class A private (b: B)@@
       |""".stripMargin
  )

  check[Stat](
    "@tailrec def f = 1",
    """|Mod.Annot @tailrec
       |""".stripMargin
  )
  check[Stat](
    "@tailrec def f = 1",
    """|Mod.Annot @tailrec
       |""".stripMargin
  )
  check[Stat](
    "@a def b = 1",
    """|Mod.Annot @a
       |""".stripMargin
  )
  check[Stat](
    "@a(1) def b = 1",
    """|Mod.Annot @a(1)
       |Init a(1)
       |""".stripMargin
  )
  check[Stat](
    "@(a @b) def x = 1",
    """|Mod.Annot @(a @b)
       |Init (a @b)
       |Type.Annotate (a @b)
       |Mod.Annot @b
       |""".stripMargin
  )
  check[Stat](
    "@(a @b(1, 2)(3)) def x = 1",
    """|Mod.Annot @(a @b(1, 2)(3))
       |Init (a @b(1, 2)(3))
       |Type.Annotate (a @b(1, 2)(3))
       |Mod.Annot @b(1, 2)(3)
       |Init b(1, 2)(3)
       |""".stripMargin
  )
  check[Stat](
    "private[foo] val a = 1",
    """|Mod.Private private[foo]
       |""".stripMargin
  )
  check[Stat](
    "protected[foo] val a = 1",
    """|Mod.Protected protected[foo]
       |""".stripMargin
  )
  check[Stat]("implicit val a = 1")
  check[Stat]("final val a = 1")
  check[Stat](
    "sealed trait a",
    """|Ctor.Primary sealed trait a@@
       |Template sealed trait a@@
       |Self sealed trait a@@
       |""".stripMargin
  )
  check[Stat]("override def f = 1")
  check[Stat](
    "case object B",
    """|Template case object B@@
       |Self case object B@@
       |""".stripMargin
  )
  check[Stat](
    "abstract class A",
    """|Ctor.Primary abstract class A@@
       |Template abstract class A@@
       |Self abstract class A@@
       |""".stripMargin
  )
  check[Stat](
    "class A[+ T]",
    """|Type.Param + T
       |Mod.Covariant +
       |Type.Bounds class A[+ T@@]
       |Ctor.Primary class A[+ T]@@
       |Template class A[+ T]@@
       |Self class A[+ T]@@
       |""".stripMargin
  )
  check[Stat](
    "class A[- T]",
    """|Type.Param - T
       |Mod.Contravariant -
       |Type.Bounds class A[- T@@]
       |Ctor.Primary class A[- T]@@
       |Template class A[- T]@@
       |Self class A[- T]@@
       |""".stripMargin
  )
  check[Stat]("lazy val a = 1")
  check[Stat](
    "class A(val b: B)",
    """|Ctor.Primary (val b: B)
       |Term.Param val b: B
       |Mod.ValParam val
       |Template class A(val b: B)@@
       |Self class A(val b: B)@@
       |""".stripMargin
  )
  check[Stat](
    "class A(var b: B)",
    """|Ctor.Primary (var b: B)
       |Term.Param var b: B
       |Mod.VarParam var
       |Template class A(var b: B)@@
       |Self class A(var b: B)@@
       |""".stripMargin
  )

  check[Stat](
    "trait A { self: B => }",
    """|Ctor.Primary trait A @@{ self: B => }
       |Name.Anonymous {
       |Template { self: B => }
       |Self self: B
       |""".stripMargin
  )
  check[Stat](
    "trait A { _: B => }",
    """|Ctor.Primary trait A @@{ _: B => }
       |Name.Anonymous {
       |Template { _: B => }
       |Self _: B
       |Name.Anonymous _
       |""".stripMargin
  )
  check[Stat](
    "trait A { self => }",
    """|Ctor.Primary trait A @@{ self => }
       |Name.Anonymous {
       |Template { self => }
       |Self self
       |""".stripMargin
  )
  check[Stat](
    "trait A { this: B => }",
    """|Ctor.Primary trait A @@{ this: B => }
       |Name.Anonymous {
       |Template { this: B => }
       |Self this: B
       |Name.Anonymous this
       |""".stripMargin
  )

  check[Stat](
    "new A {}",
    """|Template A {}
       |Self new A {@@}
       |""".stripMargin
  )
  check[Stat](
    "new { val a = 1 } with A {}",
    """|Template { val a = 1 } with A {}
       |Defn.Val val a = 1
       |Self new { val a = 1 } with A {@@}
       |""".stripMargin
  )

  check[Case](
    "case `1` =>",
    """|Term.Name `1`
       |Term.Block case `1` =>@@
       |""".stripMargin
  )
  check[Pat]("a @ A")
  check[Pat]("_")
  check[Pat]("_*")
  check[Pat]("a | b")
  check[Pat]("(a, b)")
  check[Pat]("E(a, b)")
  check[Pat]("a E b")
  check[Pat]("""s"start ${(a)} end"""", "Pat.Var (a)")
  check[Pat]("<h1>a{b}c{d}e{f}g</h1>")
  check[Pat]("x: T")
  check[Pat]("y: T")
}

package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.tests.parsers.BasePositionSuite

class TokensPositionSuite extends BasePositionSuite(dialects.Scala213) {
  checkPositions[Enumerator](
    "`a` <- b",
    """|Term.Name `a`
       |""".stripMargin
  )
  checkPositions[Enumerator]("a = 1")
  checkPositions[Enumerator]("if x")
  checkPositions[Case](
    "case `a` =>",
    """|Term.Name `a`
       |Term.Block case `a` =>@@
       |""".stripMargin
  )
  checkPositions[Case](
    "case `a` :: `b` :: _ =>",
    """|Pat.ExtractInfix `a` :: `b` :: _
       |Term.Name `a`
       |Pat.ExtractInfix `b` :: _
       |Term.Name `b`
       |Term.Block case `a` :: `b` :: _ =>@@
       |""".stripMargin
  )
  checkPositions[Case](
    "case a b `c` =>",
    """|Pat.ExtractInfix a b `c`
       |Term.Name `c`
       |Term.Block case a b `c` =>@@
       |""".stripMargin
  )
  checkPositions[Case](
    "case _ op (a | b) =>",
    """|Pat.ExtractInfix _ op (a | b)
       |Pat.Alternative (a | b)
       |Term.Block case _ op (a | b) =>@@
       |""".stripMargin
  )
  checkPositions[Case](
    "case x `.y` () =>",
    """|Pat.ExtractInfix x `.y` ()
       |Term.Name `.y`
       |Term.Block case x `.y` () =>@@
       |""".stripMargin
  )
  checkPositions[Case](
    "case a if p =>",
    """|Term.Block case a if p =>@@
       |""".stripMargin
  )
  checkPositions[Case]("case _ => ()")
  checkPositions[Case](
    "case _ => {}",
    """|Term.Block {}
       |""".stripMargin
  )

  checkPositions[Type]("B")
  checkPositions[Type]("a.B")
  checkPositions[Type]("a#B")
  checkPositions[Type]("this.type", "Term.This this")
  checkPositions[Type]("t.type")
  checkPositions[Type]("F[T]")
  checkPositions[Type]("K Map V")
  checkPositions[Type]("() => B")
  checkPositions[Type]("A => B")
  checkPositions[Type]("(A, B) => C")
  checkPositions[Type]("(A, B)")
  checkPositions[Type]("A with B")
  checkPositions[Type]("A & B")
  checkPositions[Type](
    "A { def f: B }",
    """|Decl.Def def f: B
       |""".stripMargin
  )
  checkPositions[Type]("A{}")
  checkPositions[Type](
    "{ def f: B }",
    """|Decl.Def def f: B
       |""".stripMargin
  )
  checkPositions[Type](
    "A forSome { type T }",
    """|Decl.Type type T
       |Type.Bounds A forSome { type T @@}
       |""".stripMargin
  )
  checkPositions[Type](
    "T @A",
    """|Mod.Annot @A
       |""".stripMargin
  )
  checkPositions[Type](
    "_",
    """|Type.Bounds _@@
       |""".stripMargin
  )
  checkPositions[Type](
    "_ >: A <: B",
    """|Type.Bounds >: A <: B
       |""".stripMargin
  )
  checkPositions[Type](
    "_ <: B",
    """|Type.Bounds <: B
       |""".stripMargin
  )
  checkPositions[Type](
    "_ >: A",
    """|Type.Bounds >: A
       |""".stripMargin
  )
  checkPositions[Type]("=> T")
  checkPositions[Type]("Any*")
  checkPositions[Stat](
    "def f[A <% B[A]]: C",
    """|Type.Param A <% B[A]
       |Type.Bounds def f[A @@<% B[A]]: C
       |Type.Apply B[A]
       |""".stripMargin
  )
  checkPositions[Stat](
    "def f[A: B]: C",
    """|Type.Param A: B
       |Type.Bounds def f[A@@: B]: C
       |""".stripMargin
  )
  checkPositions[Stat](
    "def f[A : B : C]: D",
    """|Type.Param A : B : C
       |Type.Bounds def f[A @@: B : C]: D
       |""".stripMargin
  )

  checkPositions[Stat](
    "(a): @A",
    """|Term.Name (a)
       |Mod.Annot @A
       |""".stripMargin
  )
  checkPositions[Stat](
    "(f)((((a))))",
    """|Term.Name (f)
       |Term.Name (((a)))
       |""".stripMargin
  )
  checkPositions[Stat](
    "(f)((a))",
    """|Term.Name (f)
       |Term.Name (a)
       |""".stripMargin
  )
  checkPositions[Stat](
    "(f)({ case a => a })",
    """|Term.Name (f)
       |Term.PartialFunction { case a => a }
       |Case case a => a
       |""".stripMargin
  )
  checkPositions[Stat](
    "(f)({ x })",
    """|Term.Name (f)
       |Term.Block { x }
       |""".stripMargin
  )
  checkPositions[Stat](
    "(a) op (b)",
    """|Term.Name (a)
       |Term.Name (b)
       |""".stripMargin
  )
  checkPositions[Stat](
    "(a, b) op (c, d)",
    """|Term.Tuple (a, b)
       |""".stripMargin
  )
  checkPositions[Stat](
    "(a, b) op ((c, d))",
    """|Term.Tuple (a, b)
       |Term.Tuple ((c, d))
       |""".stripMargin
  )
  checkPositions[Stat]("1 + 1")
  checkPositions[Stat]("a f ()")
  checkPositions[Stat](
    "a f (b)",
    """|Term.Name (b)
       |""".stripMargin
  )
  checkPositions[Stat](
    "(f) [A,B]",
    """|Term.Name (f)
       |""".stripMargin
  )
  checkPositions[Stat](
    "(f) [A]",
    """|Term.Name (f)
       |""".stripMargin
  )
  checkPositions[Stat](
    "- (a)",
    """|Term.Name (a)
       |""".stripMargin
  )
  checkPositions[Stat](
    "(a): (A)",
    """|Term.Name (a)
       |Type.Name (A)
       |""".stripMargin
  )
  checkPositions[Stat](
    "(a) = (b)",
    """|Term.Name (a)
       |Term.Name (b)
       |""".stripMargin
  )
  checkPositions[Stat](
    "{ (a); (b) }",
    """|Term.Name (a)
       |Term.Name (b)
       |""".stripMargin
  )
  checkPositions[Stat](
    "do {d} while (p)",
    """|Term.Block {d}
       |""".stripMargin
  )
  checkPositions[Stat](
    "(f) _",
    """|Term.Name (f)
       |""".stripMargin
  )
  checkPositions[Stat](
    "for { x <- xs } (f)",
    """|Enumerator.Generator x <- xs
       |Term.Name (f)
       |""".stripMargin
  )
  checkPositions[Stat](
    "for { x <- xs } yield (f)",
    """|Enumerator.Generator x <- xs
       |Term.Name (f)
       |""".stripMargin
  )
  checkPositions[Stat](
    "((a), (b)) => (a)",
    """|Term.Param (a)
       |Term.Name (a)
       |Term.Param (b)
       |Term.Name (b)
       |Term.Name (a)
       |""".stripMargin
  )
  checkPositions[Stat](
    "if (p) (t) else (f)",
    """|Term.Name (t)
       |Term.Name (f)
       |""".stripMargin
  )
  checkPositions[Stat](
    "if (p) (t)",
    """|Term.Name (t)
       |Lit.Unit if (p) (t)@@
       |""".stripMargin
  )
  checkPositions[Stat](
    "if (p) if (p2) t",
    """|Term.If if (p2) t
       |Lit.Unit if (p) if (p2) t@@
       |Lit.Unit if (p) if (p2) t@@
       |""".stripMargin
  )
  checkPositions[Stat](
    "if (p) {}",
    """|Term.Block {}
       |Lit.Unit if (p) {}@@
       |""".stripMargin
  )
  checkPositions[Stat](
    """ s"start ${(a)} end"""",
    """|Term.Name (a)
       |""".stripMargin
  )
  checkPositions[Stat](
    "(a) match { case x => x }",
    """|Term.Name (a)
       |Case case x => x
       |""".stripMargin
  )
  checkPositions[Stat](
    "new (A)",
    """|Init (A)
       |Type.Name (A)
       |""".stripMargin
  )
  checkPositions[Stat](
    "new (A){}",
    """|Template (A){}
       |Init (A)
       |Type.Name (A)
       |Self new (A){@@}
       |""".stripMargin
  )
  checkPositions[Stat](
    "def f(a: A = (da)): A",
    """|Term.Param a: A = (da)
       |Term.Name (da)
       |""".stripMargin
  )
  checkPositions[Stat](
    "{ case x => x; case y => y }",
    """|Case case x => x;
       |Case case y => y
       |""".stripMargin
  )
  checkPositions[Stat](
    "f((x): _*)",
    """|Term.Repeated (x): _*
       |Term.Name (x)
       |""".stripMargin
  )
  checkPositions[Stat](
    "return (a)",
    """|Term.Name (a)
       |""".stripMargin
  )
  checkPositions[Stat](
    "(a).b",
    """|Term.Name (a)
       |""".stripMargin
  )
  checkPositions[Stat](
    "a.super[B].c",
    """|Term.Super a.super[B]
       |""".stripMargin
  )
  checkPositions[Stat]("a.this")
  checkPositions[Stat](
    "throw (e)",
    """|Term.Name (e)
       |""".stripMargin
  )
  checkPositions[Stat](
    "try (f) catch { case x => x; case y => y } finally { }",
    """|Term.Name (f)
       |Case case x => x;
       |Case case y => y
       |Term.Block { }
       |""".stripMargin
  )
  checkPositions[Stat]("try ()")
  checkPositions[Stat](
    "try (true, false)",
    """|Term.Tuple (true, false)
       |""".stripMargin
  )
  checkPositions[Stat](
    "try {1 + 2}.toString",
    """|Term.Select {1 + 2}.toString
       |Term.Block {1 + 2}
       |Term.ApplyInfix 1 + 2
       |""".stripMargin
  )
  checkPositions[Stat](
    "try (a.b).c",
    """|Term.Select (a.b).c
       |Term.Select (a.b)
       |""".stripMargin
  )
  checkPositions[Stat](
    "try (f) catch (h) finally { }",
    """|Term.Name (f)
       |Term.Name (h)
       |Term.Block { }
       |""".stripMargin
  )
  checkPositions[Stat](
    "try (true, false) catch (h) finally (1, 2)",
    """|Term.Tuple (true, false)
       |Term.Name (h)
       |Term.Tuple (1, 2)
       |""".stripMargin
  )
  checkPositions[Stat](
    "try (a.b).c catch (h) finally (1, 2)",
    """|Term.Select (a.b).c
       |Term.Select (a.b)
       |Term.Name (h)
       |Term.Tuple (1, 2)
       |""".stripMargin
  )
  checkPositions[Stat](
    "((a), (b))",
    """|Term.Name (a)
       |Term.Name (b)
       |""".stripMargin
  )
  checkPositions[Stat](
    "while (p) {d}",
    """|Term.Block {d}
       |""".stripMargin
  )
  checkPositions[Stat]("<a>b{c}d</a>")
  checkPositions[Stat]("(x)")
  checkPositions[Stat]("(_)")

  checkPositions[Source](
    "package a",
    """|Pkg package a
       |""".stripMargin
  )
  checkPositions[Source](
    "package a.b",
    """|Pkg package a.b
       |Term.Select a.b
       |""".stripMargin
  )

  checkPositions[Stat](
    "import a.b",
    """|Importer a.b
       |""".stripMargin
  )
  checkPositions[Stat](
    "import a.b, c.d",
    """|Importer a.b
       |Importer c.d
       |""".stripMargin
  )
  checkPositions[Stat](
    "import a._",
    """|Importer a._
       |""".stripMargin
  )
  checkPositions[Stat](
    "import a.{ b, c }",
    """|Importer a.{ b, c }
       |""".stripMargin
  )
  checkPositions[Stat](
    "import a.{ b => c }",
    """|Importer a.{ b => c }
       |Importee.Rename b => c
       |""".stripMargin
  )
  checkPositions[Stat](
    "import a.{ b => _ }",
    """|Importer a.{ b => _ }
       |Importee.Unimport b => _
       |""".stripMargin
  )

  // Decl
  checkPositions[Stat]("val a: Int")
  checkPositions[Stat]("var b: Long")
  checkPositions[Stat]("def f: String")
  checkPositions[Stat](
    "type T",
    """|Type.Bounds type T@@
       |""".stripMargin
  )

  checkPositions[Stat](
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
  checkPositions[Stat]("def f = macro m")
  checkPositions[Stat](
    "class A private (b: B)",
    """|Ctor.Primary private (b: B)
       |Template class A private (b: B)@@
       |Self class A private (b: B)@@
       |""".stripMargin
  )

  checkPositions[Stat](
    "@tailrec def f = 1",
    """|Mod.Annot @tailrec
       |""".stripMargin
  )
  checkPositions[Stat](
    "@tailrec def f = 1",
    """|Mod.Annot @tailrec
       |""".stripMargin
  )
  checkPositions[Stat](
    "@a def b = 1",
    """|Mod.Annot @a
       |""".stripMargin
  )
  checkPositions[Stat](
    "@a(1) def b = 1",
    """|Mod.Annot @a(1)
       |Init a(1)
       |""".stripMargin
  )
  checkPositions[Stat](
    "@(a @b) def x = 1",
    """|Mod.Annot @(a @b)
       |Init (a @b)
       |Type.Annotate (a @b)
       |Mod.Annot @b
       |""".stripMargin
  )
  checkPositions[Stat](
    "@(a @b(1, 2)(3)) def x = 1",
    """|Mod.Annot @(a @b(1, 2)(3))
       |Init (a @b(1, 2)(3))
       |Type.Annotate (a @b(1, 2)(3))
       |Mod.Annot @b(1, 2)(3)
       |Init b(1, 2)(3)
       |""".stripMargin
  )
  checkPositions[Stat](
    "private[foo] val a = 1",
    """|Mod.Private private[foo]
       |""".stripMargin
  )
  checkPositions[Stat](
    "protected[foo] val a = 1",
    """|Mod.Protected protected[foo]
       |""".stripMargin
  )
  checkPositions[Stat]("implicit val a = 1")
  checkPositions[Stat]("final val a = 1")
  checkPositions[Stat](
    "sealed trait a",
    """|Ctor.Primary sealed trait a@@
       |Template sealed trait a@@
       |Self sealed trait a@@
       |""".stripMargin
  )
  checkPositions[Stat]("override def f = 1")
  checkPositions[Stat](
    "case object B",
    """|Template case object B@@
       |Self case object B@@
       |""".stripMargin
  )
  checkPositions[Stat](
    "abstract class A",
    """|Ctor.Primary abstract class A@@
       |Template abstract class A@@
       |Self abstract class A@@
       |""".stripMargin
  )
  checkPositions[Stat](
    "class A[+ T]",
    """|Type.Param + T
       |Mod.Covariant +
       |Type.Bounds class A[+ T@@]
       |Ctor.Primary class A[+ T]@@
       |Template class A[+ T]@@
       |Self class A[+ T]@@
       |""".stripMargin
  )
  checkPositions[Stat](
    "class A[- T]",
    """|Type.Param - T
       |Mod.Contravariant -
       |Type.Bounds class A[- T@@]
       |Ctor.Primary class A[- T]@@
       |Template class A[- T]@@
       |Self class A[- T]@@
       |""".stripMargin
  )
  checkPositions[Stat]("lazy val a = 1")
  checkPositions[Stat](
    "class A(val b: B)",
    """|Ctor.Primary (val b: B)
       |Term.Param val b: B
       |Mod.ValParam val
       |Template class A(val b: B)@@
       |Self class A(val b: B)@@
       |""".stripMargin
  )
  checkPositions[Stat](
    "class A(var b: B)",
    """|Ctor.Primary (var b: B)
       |Term.Param var b: B
       |Mod.VarParam var
       |Template class A(var b: B)@@
       |Self class A(var b: B)@@
       |""".stripMargin
  )

  checkPositions[Stat](
    "trait A { self: B => }",
    """|Ctor.Primary trait A @@{ self: B => }
       |Template { self: B => }
       |Self self: B
       |""".stripMargin
  )
  checkPositions[Stat](
    "trait A { _: B => }",
    """|Ctor.Primary trait A @@{ _: B => }
       |Template { _: B => }
       |Self _: B
       |Name.Anonymous _
       |""".stripMargin
  )
  checkPositions[Stat](
    "trait A { self => }",
    """|Ctor.Primary trait A @@{ self => }
       |Template { self => }
       |Self self
       |""".stripMargin
  )
  checkPositions[Stat](
    "trait A { this: B => }",
    """|Ctor.Primary trait A @@{ this: B => }
       |Template { this: B => }
       |Self this: B
       |Name.Anonymous this
       |""".stripMargin
  )

  checkPositions[Stat](
    "new A {}",
    """|Template A {}
       |Self new A {@@}
       |""".stripMargin
  )
  checkPositions[Stat](
    "new { val a = 1 } with A {}",
    """|Template { val a = 1 } with A {}
       |Defn.Val val a = 1
       |Self new { val a = 1 } with A {@@}
       |""".stripMargin
  )

  checkPositions[Case](
    "case `1` =>",
    """|Term.Name `1`
       |Term.Block case `1` =>@@
       |""".stripMargin
  )
  checkPositions[Pat]("a @ A")
  checkPositions[Pat]("_")
  checkPositions[Pat]("_*")
  checkPositions[Pat]("a | b")
  checkPositions[Pat]("(a, b)")
  checkPositions[Pat]("E(a, b)")
  checkPositions[Pat]("a E b")
  checkPositions[Pat]("""s"start ${(a)} end"""", "Pat.Var (a)")
  checkPositions[Pat]("<h1>a{b}c{d}e{f}g</h1>")
  checkPositions[Pat]("x: T")
  checkPositions[Pat]("y: T")

  checkPositions[Stat](
    """|trait SampleTrait extends A {
       |  self: X with B with C =>
       |
       |  def foo: Boolean = true
       |}""".stripMargin,
    """|Ctor.Primary trait SampleTrait @@extends A {
       |Template extends A {
       |  self: X with B with C =>
       |
       |  def foo: Boolean = true
       |}
       |Self self: X with B with C
       |Type.With X with B with C
       |Type.With X with B
       |Defn.Def def foo: Boolean = true
       |""".stripMargin
  )

  checkPositions[Stat](
    """|object A {
       |  private [this] def foo: Int = ???
       |}
       |""".stripMargin,
    """|Template {
       |  private [this] def foo: Int = ???
       |}
       |Self   @@private [this] def foo: Int = ???
       |Defn.Def private [this] def foo: Int = ???
       |Mod.Private private [this]
       |Term.This this
       |Name.Anonymous this
       |""".stripMargin
  )

  checkPositions[Stat](
    """|(_: X) => 42
       |""".stripMargin,
    """|Term.Param (_: X)
       |Name.Anonymous _
       |""".stripMargin
  )

  checkPositions[Stat](
    """|_ => 42
       |""".stripMargin,
    """|Term.Param _
       |Name.Anonymous _
       |""".stripMargin
  )

  checkPositions[Stat](
    """|q"x $this x"
       |""".stripMargin,
    """|Term.This this
       |Name.Anonymous this
       |""".stripMargin
  )

}

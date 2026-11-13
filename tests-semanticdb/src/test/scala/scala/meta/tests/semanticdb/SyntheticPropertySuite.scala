package scala.meta.tests
package semanticdb

import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.{semanticdb => s}
import scala.meta.metap.Format

// Regression tests for #1492: compiler-synthesized symbols (members generated for
// `case` classes and `AnyVal` classes) must carry the SYNTHETIC property, while
// symbols written by the user must not.
class SyntheticPropertySuite extends SemanticdbSuite with MetacpSnippets {

  import s.Implicits._
  import s._

  private def info(doc: s.TextDocument, suffix: String): s.SymbolInformation =
    doc.symbols.filter(_.symbol.endsWith(suffix)) match {
      case Seq(info) => info
      case Nil => fail(
          s"no symbol ending with '$suffix' in:\n" + doc.symbols.map(_.symbol).sorted.mkString("\n"),
        )
      case many => fail(s"ambiguous suffix '$suffix' matched: " + many.map(_.symbol).mkString(", "))
    }

  private def assertSynthetic(doc: s.TextDocument, suffix: String): Unit =
    assert(info(doc, suffix).isSynthetic, s"expected '$suffix' to be SYNTHETIC")

  private def assertNotSynthetic(doc: s.TextDocument, suffix: String): Unit =
    assert(!info(doc, suffix).isSynthetic, s"expected '$suffix' to NOT be SYNTHETIC")

  // the symbols that `select` accepts, from metac and from metacp
  private def symbols(name: String, code: String, metac: String, metacp: String)(
      select: s.SymbolInformation => Boolean,
  )(implicit loc: munit.Location): Unit = test(name) {
    val classes = compile(code)
    def show(infos: Map[String, s.SymbolInformation]) = {
      val printerSymtab: PrinterSymtab = infos.get
      infos.values.filter(select).map(Print.info(Format.Compact, _, printerSymtab)).toSeq.sorted
        .mkString("\n")
    }
    assertNoDiff(show(symtab(classes)), metac, "metac")
    assertNoDiff(show(symtab(convert(classes))), metacp, "metacp")
  }

  // the symbols named `evidence$N`
  private def evidences(name: String, code: String, metac: String, metacp: String)(implicit
      loc: munit.Location,
  ): Unit = symbols(s"$name: evidences", code, metac, metacp)(_.displayName.startsWith("evidence$"))

  private def implicitParams(name: String, code: String, metac: String, metacp: String)(implicit
      loc: munit.Location,
  ): Unit =
    symbols(s"$name: implicit parameters", code, metac, metacp)(i => i.isParameter && i.isImplicit)

  targeted(
    """|package p
       |case class Foo(x: Int) {
       |  def bar: Int = x
       |}
       |""".stripMargin,
    { doc =>
      // positive: members the compiler synthesizes for case classes
      assertSynthetic(doc, "p/Foo#equals().")
      assertSynthetic(doc, "p/Foo#hashCode().")
      assertSynthetic(doc, "p/Foo#copy().")
      assertSynthetic(doc, "p/Foo#canEqual().")
      assertSynthetic(doc, "p/Foo#productArity().")
      assertSynthetic(doc, "p/Foo.apply().")
      assertSynthetic(doc, "p/Foo.unapply().")
      // negative: user-written declarations are not synthetic
      assertNotSynthetic(doc, "p/Foo#")
      assertNotSynthetic(doc, "p/Foo#bar().")
      assertNotSynthetic(doc, "p/Foo#x.")
    },
  )

  targeted(
    """|package q
       |class Meters(val value: Int) extends AnyVal
       |""".stripMargin,
    { doc =>
      // positive: equals/hashCode synthesized for value classes
      assertSynthetic(doc, "q/Meters#equals().")
      assertSynthetic(doc, "q/Meters#hashCode().")
      // negative: the user-declared value accessor is not synthetic
      assertNotSynthetic(doc, "q/Meters#")
      assertNotSynthetic(doc, "q/Meters#value.")
    },
  )

  targeted(
    // #1492: an explicit self binding is written in source, so even though scalac
    // marks the self symbol synthetic it must not get the SYNTHETIC property.
    """|package r
       |trait T { self => }
       |""".stripMargin,
    { doc =>
      val self = doc.symbols.find(_.isSelfParameter).getOrElse(fail("no self parameter symbol found"))
      assert(!self.isSynthetic, "explicit self parameter must NOT be SYNTHETIC")
    },
  )

  evidences(
    "context bounds",
    """|package p
       |class C[T: Ordering](t: T) {
       |  def k = implicitly[Ordering[T]]
       |  def m[U: Ordering](u: U) = u
       |}
       |trait A {
       |  def m[V: Ordering](v: V): V
       |  def n[W: Ordering: Numeric](w: W)(implicit i: Int) = w
       |}
       |object O {
       |  def l = {
       |    class D[X: Ordering](x: X) { def m[Y: Ordering](y: Y) = y }
       |    def g[Z: Ordering](z: Z) = z
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|local11 => synthetic implicit param evidence$8: Ordering[Z]
       |local2 => synthetic implicit param evidence$6: Ordering[X]
       |local7 => synthetic implicit param evidence$7: Ordering[Y]
       |p/A#m().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |p/A#n().(evidence$4) => synthetic implicit param evidence$4: Ordering[W]
       |p/A#n().(evidence$5) => synthetic implicit param evidence$5: Numeric[W]
       |p/C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/C#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/C#m().(evidence$2) => synthetic implicit param evidence$2: Ordering[U]
       |""".stripMargin,
    """|p/A#m().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |p/A#n().(evidence$4) => synthetic implicit param evidence$4: Ordering[W]
       |p/A#n().(evidence$5) => synthetic implicit param evidence$5: Numeric[W]
       |p/C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/C#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/C#m().(evidence$2) => synthetic implicit param evidence$2: Ordering[U]
       |""".stripMargin,
  )

  evidences(
    "view bounds",
    """|package p
       |class C[T <% Int](t: T) {
       |  def k = t + 1
       |  def m[U <% Int](u: U) = u
       |}
       |object O {
       |  def l = {
       |    class D[X <% Int](x: X)
       |    def g[Z <% Int](z: Z) = z
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|local2 => synthetic implicit param evidence$3: Function1[X, Int]
       |local7 => synthetic implicit param evidence$4: Function1[Z, Int]
       |p/C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Function1[T, Int]
       |p/C#evidence$1. => private[this] synthetic implicit val method evidence$1: Function1[T, Int]
       |p/C#m().(evidence$2) => synthetic implicit param evidence$2: Function1[U, Int]
       |""".stripMargin,
    """|p/C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Function1[T, Int]
       |p/C#evidence$1. => private[this] synthetic implicit val method evidence$1: Function1[T, Int]
       |p/C#m().(evidence$2) => synthetic implicit param evidence$2: Function1[U, Int]
       |""".stripMargin,
  )

  evidences(
    "context bounds of classes with synthetic methods",
    """|package p
       |case class K[T: Ordering](t: T)
       |class W(val x: Int) extends AnyVal { def m[U: Ordering](u: U) = u }
       |object O {
       |  implicit class I[V: Ordering](v: V)
       |}
       |""".stripMargin,
    """|p/K#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/K#copy().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/K#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/K.apply().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/O.I#`<init>`().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |p/O.I#evidence$3. => private[this] synthetic implicit val method evidence$3: Ordering[V]
       |p/O.I().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |p/W#m().(evidence$2) => synthetic implicit param evidence$2: Ordering[U]
       |""".stripMargin,
    """|p/K#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/K#copy().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/K#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/K.apply().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/O.I#`<init>`().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |p/O.I#evidence$3. => private[this] synthetic implicit val method evidence$3: Ordering[V]
       |p/O.I().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |p/W#m().(evidence$2) => synthetic implicit param evidence$2: Ordering[U]
       |""".stripMargin,
  )

  evidences(
    "context bounds of classes nested in a class",
    """|package p
       |class Out {
       |  class C[T: Ordering](t: T)
       |  implicit class I[U: Ordering](u: U)
       |  case class K[V: Ordering](v: V)
       |}
       |""".stripMargin,
    """|p/Out#C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/Out#C#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/Out#I#`<init>`().(evidence$2) => synthetic implicit param evidence$2: Ordering[U]
       |p/Out#I#evidence$2. => private[this] synthetic implicit val method evidence$2: Ordering[U]
       |p/Out#I().(evidence$2) => synthetic implicit param evidence$2: Ordering[U]
       |p/Out#K#`<init>`().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |p/Out#K#copy().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |p/Out#K#evidence$3. => private[this] synthetic implicit val method evidence$3: Ordering[V]
       |p/Out#K.apply().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |""".stripMargin,
    """|p/Out#C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/Out#C#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/Out#I#`<init>`().(evidence$2) => synthetic implicit param evidence$2: Ordering[U]
       |p/Out#I#evidence$2. => private[this] synthetic implicit val method evidence$2: Ordering[U]
       |p/Out#I().(evidence$2) => synthetic implicit param evidence$2: Ordering[U]
       |p/Out#K#`<init>`().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |p/Out#K#copy().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |p/Out#K#evidence$3. => private[this] synthetic implicit val method evidence$3: Ordering[V]
       |p/Out#K.apply().(evidence$3) => synthetic implicit param evidence$3: Ordering[V]
       |""".stripMargin,
  )

  evidences(
    "written parameters named as evidences",
    """|package p
       |class E[T](t: T)(implicit evidence$91: Ordering[T])
       |case class F[T](t: T)(implicit evidence$92: Ordering[T])
       |case class H[T: Ordering](t: T)(implicit evidence$93: Numeric[T])
       |object O {
       |  implicit class G[T](t: T)(implicit evidence$94: Ordering[T])
       |  def m[T](t: T)(implicit evidence$95: Ordering[T]) = t
       |}
       |""".stripMargin,
    """|p/E#`<init>`().(evidence$91) => implicit param evidence$91: Ordering[T]
       |p/E#evidence$91. => private[this] implicit val method evidence$91: Ordering[T]
       |p/F#`<init>`().(evidence$92) => implicit param evidence$92: Ordering[T]
       |p/F#copy().(evidence$92) => implicit param evidence$92: Ordering[T]
       |p/F#evidence$92. => private[this] implicit val method evidence$92: Ordering[T]
       |p/F.apply().(evidence$92) => implicit param evidence$92: Ordering[T]
       |p/H#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/H#`<init>`().(evidence$93) => implicit param evidence$93: Numeric[T]
       |p/H#copy().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/H#copy().(evidence$93) => implicit param evidence$93: Numeric[T]
       |p/H#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/H#evidence$93. => private[this] implicit val method evidence$93: Numeric[T]
       |p/H.apply().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/H.apply().(evidence$93) => implicit param evidence$93: Numeric[T]
       |p/O.G#`<init>`().(evidence$94) => implicit param evidence$94: Ordering[T]
       |p/O.G#evidence$94. => private[this] implicit val method evidence$94: Ordering[T]
       |p/O.G().(evidence$94) => implicit param evidence$94: Ordering[T]
       |p/O.m().(evidence$95) => implicit param evidence$95: Ordering[T]
       |""".stripMargin,
    """|p/E#`<init>`().(evidence$91) => implicit param evidence$91: Ordering[T]
       |p/E#evidence$91. => private[this] implicit val method evidence$91: Ordering[T]
       |p/F#`<init>`().(evidence$92) => implicit param evidence$92: Ordering[T]
       |p/F#copy().(evidence$92) => implicit param evidence$92: Ordering[T]
       |p/F#evidence$92. => private[this] implicit val method evidence$92: Ordering[T]
       |p/F.apply().(evidence$92) => implicit param evidence$92: Ordering[T]
       |p/H#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/H#`<init>`().(evidence$93) => implicit param evidence$93: Numeric[T]
       |p/H#copy().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/H#copy().(evidence$93) => implicit param evidence$93: Numeric[T]
       |p/H#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/H#evidence$93. => private[this] implicit val method evidence$93: Numeric[T]
       |p/H.apply().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/H.apply().(evidence$93) => implicit param evidence$93: Numeric[T]
       |p/O.G#`<init>`().(evidence$94) => implicit param evidence$94: Ordering[T]
       |p/O.G#evidence$94. => private[this] implicit val method evidence$94: Ordering[T]
       |p/O.G().(evidence$94) => implicit param evidence$94: Ordering[T]
       |p/O.m().(evidence$95) => implicit param evidence$95: Ordering[T]
       |""".stripMargin,
  )

  evidences(
    "written parameters named as evidences beside bounds",
    """|package p
       |class M[A <% Int: Ordering, B: Numeric](a: A, b: B)(implicit evidence$96: Int)
       |object O {
       |  def m[T: Ordering](t: T)(implicit evidence$97: Numeric[T]) = t
       |  implicit class I[T: Ordering](t: T)(implicit evidence$98: Numeric[T])
       |}
       |""".stripMargin,
    """|p/M#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Function1[A, Int]
       |p/M#`<init>`().(evidence$2) => synthetic implicit param evidence$2: Ordering[A]
       |p/M#`<init>`().(evidence$3) => synthetic implicit param evidence$3: Numeric[B]
       |p/M#`<init>`().(evidence$96) => implicit param evidence$96: Int
       |p/M#evidence$1. => private[this] synthetic implicit val method evidence$1: Function1[A, Int]
       |p/M#evidence$2. => private[this] synthetic implicit val method evidence$2: Ordering[A]
       |p/M#evidence$3. => private[this] synthetic implicit val method evidence$3: Numeric[B]
       |p/M#evidence$96. => private[this] implicit val method evidence$96: Int
       |p/O.I#`<init>`().(evidence$5) => synthetic implicit param evidence$5: Ordering[T]
       |p/O.I#`<init>`().(evidence$98) => implicit param evidence$98: Numeric[T]
       |p/O.I#evidence$5. => private[this] synthetic implicit val method evidence$5: Ordering[T]
       |p/O.I#evidence$98. => private[this] implicit val method evidence$98: Numeric[T]
       |p/O.I().(evidence$5) => synthetic implicit param evidence$5: Ordering[T]
       |p/O.I().(evidence$98) => implicit param evidence$98: Numeric[T]
       |p/O.m().(evidence$4) => synthetic implicit param evidence$4: Ordering[T]
       |p/O.m().(evidence$97) => implicit param evidence$97: Numeric[T]
       |""".stripMargin,
    """|p/M#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Function1[A, Int]
       |p/M#`<init>`().(evidence$2) => synthetic implicit param evidence$2: Ordering[A]
       |p/M#`<init>`().(evidence$3) => synthetic implicit param evidence$3: Numeric[B]
       |p/M#`<init>`().(evidence$96) => implicit param evidence$96: Int
       |p/M#evidence$1. => private[this] synthetic implicit val method evidence$1: Function1[A, Int]
       |p/M#evidence$2. => private[this] synthetic implicit val method evidence$2: Ordering[A]
       |p/M#evidence$3. => private[this] synthetic implicit val method evidence$3: Numeric[B]
       |p/M#evidence$96. => private[this] implicit val method evidence$96: Int
       |p/O.I#`<init>`().(evidence$5) => synthetic implicit param evidence$5: Ordering[T]
       |p/O.I#`<init>`().(evidence$98) => implicit param evidence$98: Numeric[T]
       |p/O.I#evidence$5. => private[this] synthetic implicit val method evidence$5: Ordering[T]
       |p/O.I#evidence$98. => private[this] implicit val method evidence$98: Numeric[T]
       |p/O.I().(evidence$5) => synthetic implicit param evidence$5: Ordering[T]
       |p/O.I().(evidence$98) => implicit param evidence$98: Numeric[T]
       |p/O.m().(evidence$4) => synthetic implicit param evidence$4: Ordering[T]
       |p/O.m().(evidence$97) => implicit param evidence$97: Numeric[T]
       |""".stripMargin,
  )

  evidences(
    "local written parameters named as evidences beside bounds",
    """|package p
       |object O {
       |  def l = {
       |    case class K[T: Ordering](t: T)(implicit evidence$97: Numeric[T])
       |    implicit class J[T: Ordering](t: T)(implicit evidence$98: Numeric[T])
       |    def n[T: Ordering](t: T)(implicit evidence$99: Numeric[T]) = t
       |    ()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|local17 => synthetic implicit param evidence$1: Ordering[T]
         |local18 => implicit param evidence$97: Numeric[T]
         |local2 => synthetic implicit param evidence$1: Ordering[T]
         |local24 => synthetic implicit param evidence$2: Ordering[T]
         |local25 => implicit param evidence$98: Numeric[T]
         |local3 => implicit param evidence$97: Numeric[T]
         |local30 => synthetic implicit param evidence$3: Ordering[T]
         |local31 => implicit param evidence$99: Numeric[T]
         |""".stripMargin
    else
      """|local18 => synthetic implicit param evidence$1: Ordering[T]
         |local19 => implicit param evidence$97: Numeric[T]
         |local2 => synthetic implicit param evidence$1: Ordering[T]
         |local26 => synthetic implicit param evidence$2: Ordering[T]
         |local27 => implicit param evidence$98: Numeric[T]
         |local3 => implicit param evidence$97: Numeric[T]
         |local32 => synthetic implicit param evidence$3: Ordering[T]
         |local33 => implicit param evidence$99: Numeric[T]
         |""".stripMargin,
    "",
  )

  evidences(
    "context bounds beside secondary constructors and backquoted names",
    """|package p
       |class `C c`[T: Ordering](t: T) {
       |  def this(t: T, i: Int) = this(t)
       |  @deprecated private def this(t: T, s: String) = this(t)
       |  def `m m`[U: Ordering](u: U) = u
       |}
       |case class `K k`[T: Ordering](t: T)
       |object O {
       |  def m = {
       |    class L[T <% Int](t: T) { def this(t: T, i: Int) = this(t) }
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|local2 => synthetic implicit param evidence$6: Function1[T, Int]
       |local8 => synthetic implicit param evidence$7: Function1[T, Int]
       |p/`C c`#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/`C c`#`<init>`(+1).(evidence$2) => synthetic implicit param evidence$2: Ordering[T]
       |p/`C c`#`<init>`(+2).(evidence$3) => synthetic implicit param evidence$3: Ordering[T]
       |p/`C c`#`m m`().(evidence$4) => synthetic implicit param evidence$4: Ordering[U]
       |p/`C c`#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/`K k`#`<init>`().(evidence$5) => synthetic implicit param evidence$5: Ordering[T]
       |p/`K k`#copy().(evidence$5) => synthetic implicit param evidence$5: Ordering[T]
       |p/`K k`#evidence$5. => private[this] synthetic implicit val method evidence$5: Ordering[T]
       |p/`K k`.apply().(evidence$5) => synthetic implicit param evidence$5: Ordering[T]
       |""".stripMargin,
    """|p/`C c`#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/`C c`#`<init>`(+1).(evidence$2) => synthetic implicit param evidence$2: Ordering[T]
       |p/`C c`#`<init>`(+2).(evidence$3) => synthetic implicit param evidence$3: Ordering[T]
       |p/`C c`#`m m`().(evidence$4) => synthetic implicit param evidence$4: Ordering[U]
       |p/`C c`#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/`K k`#`<init>`().(evidence$5) => synthetic implicit param evidence$5: Ordering[T]
       |p/`K k`#copy().(evidence$5) => synthetic implicit param evidence$5: Ordering[T]
       |p/`K k`#evidence$5. => private[this] synthetic implicit val method evidence$5: Ordering[T]
       |p/`K k`.apply().(evidence$5) => synthetic implicit param evidence$5: Ordering[T]
       |""".stripMargin,
  )

  // scalac 2.12 does not compile `this(t)` here
  if (!ScalaVersion.is212) evidences(
    "a secondary constructor with an implicit parameter",
    """|package p
       |class C[T: Ordering](t: T) {
       |  def this(t: T, i: Int)(implicit evidence$91: Numeric[T]) = this(t)
       |}
       |""".stripMargin,
    """|p/C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/C#`<init>`(+1).(evidence$2) => synthetic implicit param evidence$2: Ordering[T]
       |p/C#`<init>`(+1).(evidence$91) => implicit param evidence$91: Numeric[T]
       |p/C#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |""".stripMargin,
    """|p/C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/C#`<init>`(+1).(evidence$2) => synthetic implicit param evidence$2: Ordering[T]
       |p/C#`<init>`(+1).(evidence$91) => implicit param evidence$91: Numeric[T]
       |p/C#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |""".stripMargin,
  )

  // scalac 2.12 does not compile this secondary constructor
  if (!ScalaVersion.is212) evidences(
    "a secondary constructor with a parameter named as the class's evidence",
    """|package p
       |class C[T: Ordering](t: T) {
       |  def this(t: T, i: Int)(implicit evidence$1: Ordering[T]) = this(t)(evidence$1)
       |}
       |""".stripMargin,
    """|p/C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/C#`<init>`(+1).(evidence$1) => implicit param evidence$1: Ordering[T]
       |p/C#`<init>`(+1).(evidence$2) => synthetic implicit param evidence$2: Ordering[T]
       |p/C#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |""".stripMargin,
    """|p/C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/C#`<init>`(+1).(evidence$1) => implicit param evidence$1: Ordering[T]
       |p/C#`<init>`(+1).(evidence$2) => synthetic implicit param evidence$2: Ordering[T]
       |p/C#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |""".stripMargin,
  )

  evidences(
    "written copy and apply with parameters named as the class's evidence",
    """|package p
       |case class K[T: Ordering](t: T) {
       |  def copy(t: T)(implicit evidence$1: Ordering[T]): K[T] = new K(t)
       |}
       |object K { def apply[T](t: T)(implicit evidence$1: Ordering[T]): K[T] = new K(t) }
       |""".stripMargin,
    """|p/K#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/K#copy().(evidence$1) => implicit param evidence$1: Ordering[T]
       |p/K#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/K.apply().(evidence$1) => implicit param evidence$1: Ordering[T]
       |""".stripMargin,
    """|p/K#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/K#copy().(evidence$1) => implicit param evidence$1: Ordering[T]
       |p/K#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/K.apply().(evidence$1) => implicit param evidence$1: Ordering[T]
       |""".stripMargin,
  )

  evidences(
    "a written parameter named as the evidence of its result class",
    """|package p
       |class H[T: Ordering](t: T)
       |object H {
       |  def mkH[T](t: T)(implicit evidence$1: Ordering[T]): H[T] = new H(t)
       |  def n(evidence$2: Int) = evidence$2
       |}
       |""".stripMargin,
    """|p/H#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/H#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/H.mkH().(evidence$1) => implicit param evidence$1: Ordering[T]
       |p/H.n().(evidence$2) => param evidence$2: Int
       |""".stripMargin,
    """|p/H#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/H#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |p/H.mkH().(evidence$1) => implicit param evidence$1: Ordering[T]
       |p/H.n().(evidence$2) => param evidence$2: Int
       |""".stripMargin,
  )

  evidences(
    "written parameters named as evidences next to a bound's",
    """|package p
       |class K[A: Ordering](implicit evidence$2: Numeric[A])
       |class L[A: Ordering](implicit evidence$22: Numeric[A])
       |object O {
       |  def m[A: Ordering](implicit evidence$2: Numeric[A]): Int = 1
       |  def n[A: Ordering](implicit evidence$22: Numeric[A]): Int = 1
       |}
       |""".stripMargin,
    """|p/K#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[A]
       |p/K#`<init>`().(evidence$2) => implicit param evidence$2: Numeric[A]
       |p/K#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[A]
       |p/K#evidence$2. => private[this] implicit val method evidence$2: Numeric[A]
       |p/L#`<init>`().(evidence$2) => synthetic implicit param evidence$2: Ordering[A]
       |p/L#`<init>`().(evidence$22) => implicit param evidence$22: Numeric[A]
       |p/L#evidence$2. => private[this] synthetic implicit val method evidence$2: Ordering[A]
       |p/L#evidence$22. => private[this] implicit val method evidence$22: Numeric[A]
       |p/O.m().(evidence$2) => implicit param evidence$2: Numeric[A]
       |p/O.m().(evidence$3) => synthetic implicit param evidence$3: Ordering[A]
       |p/O.n().(evidence$22) => implicit param evidence$22: Numeric[A]
       |p/O.n().(evidence$4) => synthetic implicit param evidence$4: Ordering[A]
       |""".stripMargin,
    """|p/K#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[A]
       |p/K#`<init>`().(evidence$2) => implicit param evidence$2: Numeric[A]
       |p/K#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[A]
       |p/K#evidence$2. => private[this] implicit val method evidence$2: Numeric[A]
       |p/L#`<init>`().(evidence$2) => synthetic implicit param evidence$2: Ordering[A]
       |p/L#`<init>`().(evidence$22) => implicit param evidence$22: Numeric[A]
       |p/L#evidence$2. => private[this] synthetic implicit val method evidence$2: Ordering[A]
       |p/L#evidence$22. => private[this] implicit val method evidence$22: Numeric[A]
       |p/O.m().(evidence$2) => implicit param evidence$2: Numeric[A]
       |p/O.m().(evidence$3) => synthetic implicit param evidence$3: Ordering[A]
       |p/O.n().(evidence$22) => implicit param evidence$22: Numeric[A]
       |p/O.n().(evidence$4) => synthetic implicit param evidence$4: Ordering[A]
       |""".stripMargin,
  )

  implicitParams(
    "written parameters named as synthetic members",
    """|package p
       |case class K(x: Int)(implicit productPrefix: String)
       |case class C(x: Int)
       |object C { def mkC(implicit hashCode: Int): C = C(hashCode) }
       |class Out {
       |  case class In(x: Int)
       |  def mkIn(implicit toString: String): In = In(1)
       |}
       |""".stripMargin,
    """|p/C.mkC().(hashCode) => implicit param hashCode: Int
       |p/K#`<init>`().(productPrefix) => implicit param productPrefix: String
       |p/K#copy().(productPrefix) => implicit param productPrefix: String
       |p/K.apply().(productPrefix) => implicit param productPrefix: String
       |p/Out#mkIn().(toString) => implicit param toString: String
       |""".stripMargin,
    """|p/C.mkC().(hashCode) => implicit param hashCode: Int
       |p/K#`<init>`().(productPrefix) => implicit param productPrefix: String
       |p/K#copy().(productPrefix) => implicit param productPrefix: String
       |p/K.apply().(productPrefix) => implicit param productPrefix: String
       |p/Out#mkIn().(toString) => implicit param toString: String
       |""".stripMargin,
  )

  evidences(
    "a context bound read from an inner class",
    """|package p
       |class C[T: Ordering](t: T) {
       |  class In { def o = implicitly[Ordering[T]] }
       |}
       |""".stripMargin,
    """|p/C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/C#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |""".stripMargin,
    """|p/C#`<init>`().(evidence$1) => synthetic implicit param evidence$1: Ordering[T]
       |p/C#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |""".stripMargin,
  )

}

package scala.meta.tests.semanticdb

class ExistentialTypesSuite extends SemanticdbSuite {

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  symbols(
    "existential value in a local type alias",
    """|abstract class e1 {
       |  def m = {
       |    type E = xe.type forSome { val xe: AnyRef }
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|_empty_/e1# => abstract class e1 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e1#`<init>`(). => primary ctor <init>()
       |_empty_/e1#m(). => method m: Unit
       |  Unit => scala/Unit#
       |local0 => type E = xe.type forSome { type xe.type <: AnyRef with Singleton {} }
       |  xe.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local2 => method xeAnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    "existential value in a local type alias",
    """|abstract class e1 {
       |  def m = {
       |    type E = xe.type forSome { val xe: AnyRef }
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|[0:15..0:17): e1 <= _empty_/e1#
       |[0:18..0:18):  <= _empty_/e1#`<init>`().
       |[1:6..1:7): m <= _empty_/e1#m().
       |[2:9..2:10): E <= local0
       |[2:13..2:15): xe => local2
       |[2:35..2:37): xe <= local2
       |""".stripMargin,
  )

  symbols(
    "existential values in a type member, bounds and a method type parameter",
    """|class e2 {
       |  type E = xe.type forSome { val xe: AnyRef }
       |  type T <: Option[xt.type] forSome { val xt: AnyRef }
       |  type L >: Option[xl.type] forSome { val xl: AnyRef }
       |  def f[P <: Option[xp.type] forSome { val xp: AnyRef }] = 1
       |}
       |""".stripMargin,
    """|_empty_/e2# => class e2 extends AnyRef { +5 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e2#E# => type E = xe.type forSome { type xe.type <: AnyRef with Singleton {} }
       |  xe.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e2#L# => abstract type L >: Option[xl.type] forSome { type xl.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xl.type => local4
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e2#T# => abstract type T <: Option[xt.type] forSome { type xt.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xt.type => local2
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e2#`<init>`(). => primary ctor <init>()
       |_empty_/e2#f(). => method f[P <: Option[xp.type] forSome { type xp.type <: AnyRef with Singleton {} }]: Int
       |  P => _empty_/e2#f().[P]
       |  Option => scala/Option#
       |  xp.type => local6
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  Int => scala/Int#
       |_empty_/e2#f().[P] => typeparam P <: Option[xp.type] forSome { type xp.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xp.type => local6
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local1 => method xeAnyRef
       |  AnyRef => scala/AnyRef#
       |local3 => method xtAnyRef
       |  AnyRef => scala/AnyRef#
       |local5 => method xlAnyRef
       |  AnyRef => scala/AnyRef#
       |local7 => method xpAnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    "existential values in a type member, bounds and a method type parameter",
    """|class e2 {
       |  type E = xe.type forSome { val xe: AnyRef }
       |  type T <: Option[xt.type] forSome { val xt: AnyRef }
       |  type L >: Option[xl.type] forSome { val xl: AnyRef }
       |  def f[P <: Option[xp.type] forSome { val xp: AnyRef }] = 1
       |}
       |""".stripMargin,
    """|[0:6..0:8): e2 <= _empty_/e2#
       |[0:9..0:9):  <= _empty_/e2#`<init>`().
       |[1:7..1:8): E <= _empty_/e2#E#
       |[1:11..1:13): xe => local1
       |[1:33..1:35): xe <= local1
       |[2:7..2:8): T <= _empty_/e2#T#
       |[2:12..2:18): Option => scala/Option#
       |[2:19..2:21): xt => local3
       |[2:42..2:44): xt <= local3
       |[3:7..3:8): L <= _empty_/e2#L#
       |[3:12..3:18): Option => scala/Option#
       |[3:19..3:21): xl => local5
       |[3:42..3:44): xl <= local5
       |[4:6..4:7): f <= _empty_/e2#f().
       |[4:8..4:9): P <= _empty_/e2#f().[P]
       |[4:13..4:19): Option => scala/Option#
       |[4:20..4:22): xp => local7
       |[4:43..4:45): xp <= local7
       |""".stripMargin,
  )

  symbols(
    "existential values in type parameters of a class and a type alias",
    """|class e10[T <: Option[xt.type] forSome { val xt: AnyRef }]
       |trait e11 { type F[P <: Option[xf.type] forSome { val xf: AnyRef }] = P }
       |""".stripMargin,
    """|_empty_/e10# => class e10[T <: Option[xt.type] forSome { type xt.type <: AnyRef with Singleton {} }] extends AnyRef { +1 decls }
       |  T => _empty_/e10#[T]
       |  Option => scala/Option#
       |  xt.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e10#[T] => typeparam T <: Option[xt.type] forSome { type xt.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xt.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e10#`<init>`(). => primary ctor <init>()
       |_empty_/e11# => trait e11 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e11#F# => type F[P <: Option[xf.type] forSome { type xf.type <: AnyRef with Singleton {} }] = P
       |  P => _empty_/e11#F#[P]
       |  Option => scala/Option#
       |  xf.type => local2
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e11#F#[P] => typeparam P <: Option[xf.type] forSome { type xf.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xf.type => local2
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local1 => method xtAnyRef
       |  AnyRef => scala/AnyRef#
       |local3 => method xfAnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    "existential values in type parameters of a class and a type alias",
    """|class e10[T <: Option[xt.type] forSome { val xt: AnyRef }]
       |trait e11 { type F[P <: Option[xf.type] forSome { val xf: AnyRef }] = P }
       |""".stripMargin,
    """|[0:6..0:9): e10 <= _empty_/e10#
       |[0:10..0:11): T <= _empty_/e10#[T]
       |[0:15..0:21): Option => scala/Option#
       |[0:22..0:24): xt => local1
       |[0:45..0:47): xt <= local1
       |[0:58..0:58):  <= _empty_/e10#`<init>`().
       |[1:6..1:9): e11 <= _empty_/e11#
       |[1:17..1:18): F <= _empty_/e11#F#
       |[1:19..1:20): P <= _empty_/e11#F#[P]
       |[1:24..1:30): Option => scala/Option#
       |[1:31..1:33): xf => local3
       |[1:54..1:56): xf <= local3
       |[1:70..1:71): P => _empty_/e11#F#[P]
       |""".stripMargin,
  )

  symbols(
    "existential values in a value, a parameter and a refinement",
    """|class e3 {
       |  val v: Option[xv.type] forSome { val xv: AnyRef } = None
       |  def d(a: Option[xd.type] forSome { val xd: AnyRef }) = a
       |  type S = { def r: Option[xr.type] forSome { val xr: AnyRef } }
       |}
       |""".stripMargin,
    """|_empty_/e3# => class e3 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e3#S# => type S = AnyRef { def r: Option[xr.type] forSome { type xr.type <: AnyRef with Singleton {} } }
       |  AnyRef => scala/AnyRef#
       |  r => local4
       |  Option => scala/Option#
       |  xr.type => local5
       |  Singleton => scala/Singleton#
       |_empty_/e3#`<init>`(). => primary ctor <init>()
       |_empty_/e3#d(). => method d(a: Option[xd.type] forSome { type xd.type <: AnyRef with Singleton {} }): Option[AnyRef]
       |  a => _empty_/e3#d().(a)
       |  Option => scala/Option#
       |  xd.type => local2
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e3#d().(a) => param a: Option[xd.type] forSome { type xd.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xd.type => local2
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e3#v. => val method v: Option[xv.type] forSome { type xv.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xv.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local0 => val local xv: AnyRef
       |  AnyRef => scala/AnyRef#
       |local3 => val local xd: AnyRef
       |  AnyRef => scala/AnyRef#
       |local4 => abstract method r: Option[xr.type] forSome { type xr.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xr.type => local5
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local6 => val local xr: AnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    "existential values in a value, a parameter and a refinement",
    """|class e3 {
       |  val v: Option[xv.type] forSome { val xv: AnyRef } = None
       |  def d(a: Option[xd.type] forSome { val xd: AnyRef }) = a
       |  type S = { def r: Option[xr.type] forSome { val xr: AnyRef } }
       |}
       |""".stripMargin,
    """|[0:6..0:8): e3 <= _empty_/e3#
       |[0:9..0:9):  <= _empty_/e3#`<init>`().
       |[1:6..1:7): v <= _empty_/e3#v.
       |[1:9..1:15): Option => scala/Option#
       |[1:16..1:18): xv => local0
       |[1:39..1:41): xv <= local0
       |[1:54..1:58): None => scala/None.
       |[2:6..2:7): d <= _empty_/e3#d().
       |[2:8..2:9): a <= _empty_/e3#d().(a)
       |[2:11..2:17): Option => scala/Option#
       |[2:18..2:20): xd => local3
       |[2:41..2:43): xd <= local3
       |[2:57..2:58): a => _empty_/e3#d().(a)
       |[3:7..3:8): S <= _empty_/e3#S#
       |[3:17..3:18): r <= local4
       |[3:20..3:26): Option => scala/Option#
       |[3:27..3:29): xr => local6
       |[3:50..3:52): xr <= local6
       |""".stripMargin,
  )

  symbols(
    "existential values in type ascriptions",
    """|class e13 {
       |  def m = {
       |    val a = (None: Option[xa.type] forSome { val xa: AnyRef })
       |    a
       |  }
       |  val b = (None: Option[xb.type] forSome { val xb: AnyRef })
       |}
       |""".stripMargin,
    """|_empty_/e13# => class e13 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e13#`<init>`(). => primary ctor <init>()
       |_empty_/e13#b. => val method b: Option[AnyRef]
       |  Option => scala/Option#
       |  AnyRef => scala/AnyRef#
       |_empty_/e13#m(). => method m: Option[AnyRef]
       |  Option => scala/Option#
       |  AnyRef => scala/AnyRef#
       |local0 => val local xa: AnyRef
       |  AnyRef => scala/AnyRef#
       |local1 => val local a: Option[AnyRef]
       |  Option => scala/Option#
       |  AnyRef => scala/AnyRef#
       |local2 => val local xb: AnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    "existential values in type ascriptions",
    """|class e13 {
       |  def m = {
       |    val a = (None: Option[xa.type] forSome { val xa: AnyRef })
       |    a
       |  }
       |  val b = (None: Option[xb.type] forSome { val xb: AnyRef })
       |}
       |""".stripMargin,
    """|[0:6..0:9): e13 <= _empty_/e13#
       |[0:10..0:10):  <= _empty_/e13#`<init>`().
       |[1:6..1:7): m <= _empty_/e13#m().
       |[2:8..2:9): a <= local1
       |[2:13..2:17): None => scala/None.
       |[2:19..2:25): Option => scala/Option#
       |[2:26..2:28): xa => local0
       |[2:49..2:51): xa <= local0
       |[3:4..3:5): a => local1
       |[5:6..5:7): b <= _empty_/e13#b.
       |[5:11..5:15): None => scala/None.
       |[5:17..5:23): Option => scala/Option#
       |[5:24..5:26): xb => local2
       |[5:47..5:49): xb <= local2
       |""".stripMargin,
  )

  test("existential value in a self type: symbols") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class e4 {
         |  self: (Option[xs.type] forSome { val xs: AnyRef }) =>
         |}
         |""".stripMargin,
      "Symbols",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("existential value in a self type: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class e4 {
         |  self: (Option[xs.type] forSome { val xs: AnyRef }) =>
         |}
         |""".stripMargin,
      "Occurrences",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("existential values in self types of a trait and an object: symbols") {
    def load() = computeDatabaseSectionFromSnippet(
      """|trait e5 {
         |  self: (Option[xt.type] forSome { val xt: AnyRef }) =>
         |}
         |object o5 {
         |  self: (Option[xo.type] forSome { val xo: AnyRef }) =>
         |}
         |""".stripMargin,
      "Symbols",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("existential value in the self type of a local class: symbols") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object e6 {
         |  def m = {
         |    class c { self: (Option[xs.type] forSome { val xs: AnyRef }) => }
         |    1
         |  }
         |}
         |""".stripMargin,
      "Symbols",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("existential value in the self type of a local class: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object e6 {
         |  def m = {
         |    class c { self: (Option[xs.type] forSome { val xs: AnyRef }) => }
         |    1
         |  }
         |}
         |""".stripMargin,
      "Occurrences",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("existential values in self types of member classes: symbols") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object e16 {
         |  class c { self: (Option[xs.type] forSome { val xs: AnyRef }) => }
         |  class d { self: (Option[xd.type] forSome { val xd: AnyRef }) with Serializable => }
         |}
         |""".stripMargin,
      "Symbols",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("existential values in self types of member classes: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object e16 {
         |  class c { self: (Option[xs.type] forSome { val xs: AnyRef }) => }
         |  class d { self: (Option[xd.type] forSome { val xd: AnyRef }) with Serializable => }
         |}
         |""".stripMargin,
      "Occurrences",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("existential value in a compound self type: symbols") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class e7 {
         |  self: (Option[xs.type] forSome { val xs: AnyRef }) with Serializable =>
         |}
         |""".stripMargin,
      "Symbols",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("self reference under an existential self type: synthetics") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class e8 {
         |  self: (Option[xs.type] forSome { val xs: AnyRef }) =>
         |  def g = Some(self)
         |}
         |""".stripMargin,
      "Synthetics",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  symbols(
    "existential type in a self type",
    """|class e14 {
       |  self: (Option[T] forSome { type T <: AnyRef }) =>
       |}
       |""".stripMargin,
    """|_empty_/e14# => class e14 extends AnyRef { self: Option[T] forSome { type T <: AnyRef } => +1 decls }
       |  AnyRef => scala/AnyRef#
       |  Option => scala/Option#
       |  T => local0
       |_empty_/e14#`<init>`(). => primary ctor <init>()
       |local0 => abstract type T <: AnyRef
       |  AnyRef => scala/AnyRef#
       |local1 => selfparam self: Option[T] forSome { type T <: AnyRef }
       |  Option => scala/Option#
       |  T => local0
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    "existential type in a self type",
    """|class e14 {
       |  self: (Option[T] forSome { type T <: AnyRef }) =>
       |}
       |""".stripMargin,
    """|[0:6..0:9): e14 <= _empty_/e14#
       |[0:10..0:10):  <= _empty_/e14#`<init>`().
       |[1:2..1:6): self <= local1
       |[1:16..1:17): T => local0
       |[1:34..1:35): T <= local0
       |""".stripMargin,
  )

  test("singleton types in self types of member classes: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object e20 {
         |  val v: AnyRef = null
         |  class c { self: Option[v.type] => }
         |  class d { self: Option[v.type] with Serializable => }
         |  class f { self: v.type => }
         |}
         |""".stripMargin,
      "Occurrences",
    )
    assertNoDiff(
      load(),
      if (ScalaVersion.is212)
        """|[0:7..0:10): e20 <= _empty_/e20.
           |[1:6..1:7): v <= _empty_/e20.v.
           |[1:9..1:15): AnyRef => scala/AnyRef#
           |[2:8..2:9): c <= _empty_/e20.c#
           |[2:10..2:10):  <= _empty_/e20.c#`<init>`().
           |[2:12..2:16): self <= local0
           |[2:18..2:24): Option => scala/Option#
           |[3:8..3:9): d <= _empty_/e20.d#
           |[3:10..3:10):  <= _empty_/e20.d#`<init>`().
           |[3:12..3:16): self <= local1
           |[3:18..3:24): Option => scala/Option#
           |[3:25..3:26): v => _empty_/e20.v.
           |[3:38..3:50): Serializable => scala/Serializable#
           |[4:8..4:9): f <= _empty_/e20.f#
           |[4:10..4:10):  <= _empty_/e20.f#`<init>`().
           |[4:12..4:16): self <= local2
           |""".stripMargin
      else
        """|[0:7..0:10): e20 <= _empty_/e20.
           |[1:6..1:7): v <= _empty_/e20.v.
           |[1:9..1:15): AnyRef => scala/AnyRef#
           |[2:8..2:9): c <= _empty_/e20.c#
           |[2:10..2:10):  <= _empty_/e20.c#`<init>`().
           |[2:12..2:16): self <= local0
           |[2:18..2:24): Option => scala/Option#
           |[3:8..3:9): d <= _empty_/e20.d#
           |[3:10..3:10):  <= _empty_/e20.d#`<init>`().
           |[3:12..3:16): self <= local1
           |[3:18..3:24): Option => scala/Option#
           |[3:25..3:26): v => _empty_/e20.v.
           |[3:38..3:50): Serializable => scala/package.Serializable#
           |[4:8..4:9): f <= _empty_/e20.f#
           |[4:10..4:10):  <= _empty_/e20.f#`<init>`().
           |[4:12..4:16): self <= local2
           |""".stripMargin,
    )
  }

  symbols(
    "existential values in constructor parameters",
    """|class e21(b: Option[xb.type] forSome { val xb: AnyRef }, var c: Option[xc.type] forSome { val xc: AnyRef })
       |""".stripMargin,
    """|_empty_/e21# => class e21 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e21#`<init>`(). => primary ctor <init>(b: Option[xb.type] forSome { type xb.type <: AnyRef with Singleton {} }, var c: Option[xc.type] forSome { type xc.type <: AnyRef with Singleton {} })
       |  b => _empty_/e21#`<init>`().(b)
       |  Option => scala/Option#
       |  xb.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  c => _empty_/e21#`<init>`().(c)
       |  xc.type => local1
       |_empty_/e21#`<init>`().(b) => param b: Option[xb.type] forSome { type xb.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xb.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e21#`<init>`().(c) => var param c: Option[xc.type] forSome { type xc.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xc.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e21#`c_=`(). => var method c_=(x$1: Option[xc.type] forSome { type xc.type <: AnyRef with Singleton {} }): Unit
       |  x$1 => _empty_/e21#`c_=`().(x$1)
       |  Option => scala/Option#
       |  xc.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  Unit => scala/Unit#
       |_empty_/e21#`c_=`().(x$1) => synthetic param x$1: Option[xc.type] forSome { type xc.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xc.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e21#b. => private[this] val method b: Option[xb.type] forSome { type xb.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xb.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e21#c(). => var method c: Option[xc.type] forSome { type xc.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xc.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local2 => val local xb: AnyRef
       |  AnyRef => scala/AnyRef#
       |local3 => val local xc: AnyRef
       |  AnyRef => scala/AnyRef#
       |local4 => val local xb: AnyRef
       |  AnyRef => scala/AnyRef#
       |local5 => val local xc: AnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    "existential values in constructor parameters",
    """|class e21(b: Option[xb.type] forSome { val xb: AnyRef }, var c: Option[xc.type] forSome { val xc: AnyRef })
       |""".stripMargin,
    """|[0:6..0:9): e21 <= _empty_/e21#
       |[0:9..0:9):  <= _empty_/e21#`<init>`().
       |[0:10..0:11): b <= _empty_/e21#`<init>`().(b)
       |[0:10..0:11): b <= _empty_/e21#b.
       |[0:13..0:19): Option => scala/Option#
       |[0:20..0:22): xb => local2
       |[0:43..0:45): xb <= local2
       |[0:61..0:62): c <= _empty_/e21#`<init>`().(c)
       |[0:61..0:62): c <= _empty_/e21#c().
       |[0:64..0:70): Option => scala/Option#
       |[0:71..0:73): xc => local3
       |[0:94..0:96): xc <= local3
       |""".stripMargin,
  )

  test("existential value in a case class parameter: symbols") {
    def load() = computeDatabaseSectionFromSnippet(
      """|case class e24(b: Option[xb.type] forSome { val xb: AnyRef })
         |""".stripMargin,
      "Symbols",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("existential value in a case class parameter: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|case class e24(b: Option[xb.type] forSome { val xb: AnyRef })
         |""".stripMargin,
      "Occurrences",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  symbols(
    "existential value in a default parameter",
    """|class e25 {
       |  def m(b: Option[xb.type] forSome { val xb: AnyRef } = None) = b
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/e25# => class e25 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e25#`<init>`(). => primary ctor <init>()
         |_empty_/e25#m$default$1(). => synthetic method m$default$1: Option[xb.type] forSome { type xb.type <: AnyRef with Singleton {} }
         |  Option => scala/Option#
         |  xb.type => local0
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |_empty_/e25#m(). => method m(b: Option[xb.type] forSome { type xb.type <: AnyRef with Singleton {} }): Option[AnyRef]
         |  b => _empty_/e25#m().(b)
         |  Option => scala/Option#
         |  xb.type => local0
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |_empty_/e25#m().(b) => default param b: Option[xb.type] forSome { type xb.type <: AnyRef with Singleton {} }
         |  Option => scala/Option#
         |  xb.type => local0
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local1 => val local xb: AnyRef
         |  AnyRef => scala/AnyRef#
         |local2 => val local xb: AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin
    else
      """|_empty_/e25# => class e25 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e25#`<init>`(). => primary ctor <init>()
         |_empty_/e25#m$default$1(). => synthetic method m$default$1: Option[AnyRef] @uncheckedVariance
         |  Option => scala/Option#
         |  AnyRef => scala/AnyRef#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/e25#m(). => method m(b: Option[xb.type] forSome { type xb.type <: AnyRef with Singleton {} }): Option[AnyRef]
         |  b => _empty_/e25#m().(b)
         |  Option => scala/Option#
         |  xb.type => local0
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |_empty_/e25#m().(b) => default param b: Option[xb.type] forSome { type xb.type <: AnyRef with Singleton {} }
         |  Option => scala/Option#
         |  xb.type => local0
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local1 => val local xb: AnyRef
         |  AnyRef => scala/AnyRef#
         |local2 => val local xb: AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin,
  )

  occurrences(
    "existential value in a default parameter",
    """|class e25 {
       |  def m(b: Option[xb.type] forSome { val xb: AnyRef } = None) = b
       |}
       |""".stripMargin,
    """|[0:6..0:9): e25 <= _empty_/e25#
       |[0:10..0:10):  <= _empty_/e25#`<init>`().
       |[1:6..1:7): m <= _empty_/e25#m().
       |[1:8..1:9): b <= _empty_/e25#m().(b)
       |[1:11..1:17): Option => scala/Option#
       |[1:18..1:20): xb => local1
       |[1:41..1:43): xb <= local1
       |[1:56..1:60): None => scala/None.
       |[1:64..1:65): b => _empty_/e25#m().(b)
       |""".stripMargin,
  )

  symbols(
    "existential value in classOf",
    """|object e22 {
       |  val c = classOf[Option[xc.type] forSome { val xc: AnyRef }]
       |}
       |""".stripMargin,
    """|_empty_/e22. => final object e22 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e22.c. => val method c: Class[Option[xc.type] forSome { type xc.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  xc.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |""".stripMargin,
  )

  occurrences(
    "existential value in classOf",
    """|object e22 {
       |  val c = classOf[Option[xc.type] forSome { val xc: AnyRef }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e22 <= _empty_/e22.
       |[1:6..1:7): c <= _empty_/e22.c.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  symbols(
    "wildcard in an extractor pattern",
    """|object e23 {
       |  def f(x: Any) = x match { case l @ List(_) => l }
       |}
       |""".stripMargin,
    """|_empty_/e23. => final object e23 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e23.f(). => method f(x: Any): List[Any]
       |  x => _empty_/e23.f().(x)
       |  Any => scala/Any#
       |  List => scala/collection/immutable/List#
       |_empty_/e23.f().(x) => param x: Any
       |  Any => scala/Any#
       |local0 => val local l: List[local1]
       |  List => scala/collection/immutable/List#
       |  local1 => local1
       |""".stripMargin,
  )

  symbols(
    "existential types in classOf",
    """|object e26 {
       |  val c = classOf[Option[T] forSome { type T <: AnyRef; type U[V <: Int] }]
       |  val d = classOf[Option[Option[xd.type] forSome { val xd: Option[Int] }]]
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/e26. => final object e26 extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e26.c. => val method c: Class[Option[T] forSome { type T <: AnyRef; type U[V <: Int] => Any }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  T => local0
         |  AnyRef => scala/AnyRef#
         |  U => local1
         |  V => local2
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/e26.d. => val method d: Class[Option[Option[xd.type] forSome { type xd.type <: Option[Int] with Singleton {} }]]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  xd.type => local3
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |""".stripMargin
    else
      """|_empty_/e26. => final object e26 extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e26.c. => val method c: Class[Option[T] forSome { type T <: AnyRef; type U[V <: Int] }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  T => local0
         |  AnyRef => scala/AnyRef#
         |  U => local1
         |  V => local2
         |  Int => scala/Int#
         |_empty_/e26.d. => val method d: Class[Option[Option[xd.type] forSome { type xd.type <: Option[Int] with Singleton {} }]]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  xd.type => local3
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |""".stripMargin,
  )

  occurrences(
    "existential types in classOf",
    """|object e26 {
       |  val c = classOf[Option[T] forSome { type T <: AnyRef; type U[V <: Int] }]
       |  val d = classOf[Option[Option[xd.type] forSome { val xd: Option[Int] }]]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e26 <= _empty_/e26.
       |[1:6..1:7): c <= _empty_/e26.c.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): d <= _empty_/e26.d.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |""".stripMargin,
  )

  occurrences(
    "existential type with bounds in a value type",
    """|object e27 {
       |  val v: Option[T] forSome { type T >: Null <: AnyRef } = None
       |}
       |""".stripMargin,
    """|[0:7..0:10): e27 <= _empty_/e27.
       |[1:6..1:7): v <= _empty_/e27.v.
       |[1:9..1:15): Option => scala/Option#
       |[1:16..1:17): T => local0
       |[1:34..1:35): T <= local0
       |[1:58..1:62): None => scala/None.
       |""".stripMargin,
  )

  occurrences(
    "type member of an existential value in classOf",
    """|trait Foo { type T }
       |object e28 {
       |  val c = classOf[Option[x.T] forSome { val x: Foo }]
       |}
       |""".stripMargin,
    """|[0:6..0:9): Foo <= _empty_/Foo#
       |[0:17..0:18): T <= _empty_/Foo#T#
       |[1:7..1:10): e28 <= _empty_/e28.
       |[2:6..2:7): c <= _empty_/e28.c.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  symbols(
    "existential type bounded by an existential value in classOf",
    """|object e29 {
       |  val c = classOf[Option[T] forSome { type T <: x.type; val x: AnyRef }]
       |}
       |""".stripMargin,
    """|_empty_/e29. => final object e29 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e29.c. => val method c: Class[Option[T] forSome { type T <: x.type; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  T => local0
       |  x.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |""".stripMargin,
  )

  occurrences(
    "paths through existential values in classOf",
    """|trait Foo { type T }
       |trait Bar { val y: Foo }
       |object e30 {
       |  val c = classOf[Option[x.type#T] forSome { val x: Foo }]
       |  val d = classOf[Option[x.y.T] forSome { val x: Bar }]
       |}
       |""".stripMargin,
    """|[0:6..0:9): Foo <= _empty_/Foo#
       |[0:17..0:18): T <= _empty_/Foo#T#
       |[1:6..1:9): Bar <= _empty_/Bar#
       |[1:16..1:17): y <= _empty_/Bar#y.
       |[1:19..1:22): Foo => _empty_/Foo#
       |[2:7..2:10): e30 <= _empty_/e30.
       |[3:6..3:7): c <= _empty_/e30.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[4:6..4:7): d <= _empty_/e30.d.
       |[4:10..4:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  symbols(
    "existential values in the declarations of existential types in classOf",
    """|trait Foo { type T <: AnyRef }
       |object e31 {
       |  val a = classOf[Option[y.type] forSome { val x: AnyRef; val y: x.type }]
       |  val b = classOf[Option[T] forSome { type T >: x.type <: AnyRef; val x: AnyRef }]
       |  val c = classOf[Option[T] forSome { type T <: Option[x.type]; val x: AnyRef }]
       |  val d = classOf[Option[T] forSome { type T <: x.T; val x: Foo }]
       |  val e = classOf[Option[y.type] forSome { val x: Foo; val y: x.T }]
       |}
       |""".stripMargin,
    """|_empty_/Foo# => trait Foo extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/Foo#T# => abstract type T <: AnyRef
       |  AnyRef => scala/AnyRef#
       |_empty_/e31. => final object e31 extends AnyRef { +5 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e31.a. => val method a: Class[Option[y.type] forSome { type x.type <: AnyRef with Singleton {}; type y.type <: x.type with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  y.type => local0
       |  x.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e31.b. => val method b: Class[Option[T] forSome { type T >: x.type <: AnyRef; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  T => local2
       |  x.type => local3
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e31.c. => val method c: Class[Option[T] forSome { type T <: Option[x.type]; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  T => local4
       |  x.type => local5
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e31.d. => val method d: Class[Option[T] forSome { type T <: x.type#T; type x.type <: Foo with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  T => local6
       |  x.type => local7
       |  T => _empty_/Foo#T#
       |  Foo => _empty_/Foo#
       |  Singleton => scala/Singleton#
       |_empty_/e31.e. => val method e: Class[Option[y.type] forSome { type x.type <: Foo with Singleton {}; type y.type <: x.type#T with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  y.type => local8
       |  x.type => local9
       |  Foo => _empty_/Foo#
       |  Singleton => scala/Singleton#
       |  T => _empty_/Foo#T#
       |""".stripMargin,
  )

  occurrences(
    "existential values in the declarations of existential types in classOf",
    """|trait Foo { type T <: AnyRef }
       |object e31 {
       |  val a = classOf[Option[y.type] forSome { val x: AnyRef; val y: x.type }]
       |  val b = classOf[Option[T] forSome { type T >: x.type <: AnyRef; val x: AnyRef }]
       |  val c = classOf[Option[T] forSome { type T <: Option[x.type]; val x: AnyRef }]
       |  val d = classOf[Option[T] forSome { type T <: x.T; val x: Foo }]
       |  val e = classOf[Option[y.type] forSome { val x: Foo; val y: x.T }]
       |}
       |""".stripMargin,
    """|[0:6..0:9): Foo <= _empty_/Foo#
       |[0:17..0:18): T <= _empty_/Foo#T#
       |[0:22..0:28): AnyRef => scala/AnyRef#
       |[1:7..1:10): e31 <= _empty_/e31.
       |[2:6..2:7): a <= _empty_/e31.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[3:6..3:7): b <= _empty_/e31.b.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[4:6..4:7): c <= _empty_/e31.c.
       |[4:10..4:17): classOf => scala/Predef.classOf().
       |[5:6..5:7): d <= _empty_/e31.d.
       |[5:10..5:17): classOf => scala/Predef.classOf().
       |[6:6..6:7): e <= _empty_/e31.e.
       |[6:10..6:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  symbols(
    "existential value in a type parameter bound in classOf",
    """|object e32 {
       |  val c = classOf[Option[U[Int]] forSome { type U[V <: x.type]; val x: AnyRef }]
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/e32. => final object e32 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e32.c. => val method c: Class[Option[U[Int]] forSome { type U[V <: x.type] => Any; type x.type <: AnyRef with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  U => local0
         |  Int => scala/Int#
         |  V => local1
         |  x.type => local2
         |  Any => scala/Any#
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |""".stripMargin
    else
      """|_empty_/e32. => final object e32 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e32.c. => val method c: Class[Option[U[Int]] forSome { type U[V <: x.type]; type x.type <: AnyRef with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  U => local0
         |  Int => scala/Int#
         |  V => local1
         |  x.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |""".stripMargin,
  )

  occurrences(
    "existential value in a type parameter bound in classOf",
    """|object e32 {
       |  val c = classOf[Option[U[Int]] forSome { type U[V <: x.type]; val x: AnyRef }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e32 <= _empty_/e32.
       |[1:6..1:7): c <= _empty_/e32.c.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  symbols(
    "existential values in the declarations of existential types in type aliases",
    """|trait Foo { type T <: AnyRef }
       |class e33 {
       |  type A = Option[y.type] forSome { val x: AnyRef; val y: x.type }
       |  type B = Option[T] forSome { type T >: x.type <: AnyRef; val x: AnyRef }
       |  type C = Option[T] forSome { type T <: Option[x.type]; val x: AnyRef }
       |  type D = Option[T] forSome { type T <: x.T; val x: Foo }
       |  type E = Option[y.type] forSome { val x: Foo; val y: x.T }
       |}
       |""".stripMargin,
    """|_empty_/Foo# => trait Foo extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/Foo#T# => abstract type T <: AnyRef
       |  AnyRef => scala/AnyRef#
       |_empty_/e33# => class e33 extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e33#A# => type A = Option[y.type] forSome { type x.type <: AnyRef with Singleton {}; type y.type <: x.type with Singleton {} }
       |  Option => scala/Option#
       |  y.type => local0
       |  x.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e33#B# => type B = Option[T] forSome { type T >: x.type <: AnyRef; type x.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  T => local4
       |  x => local6
       |  AnyRef => scala/AnyRef#
       |  x.type => local5
       |  Singleton => scala/Singleton#
       |_empty_/e33#C# => type C = Option[T] forSome { type T <: Option[x.type]; type x.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  T => local7
       |  x => local9
       |  x.type => local8
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e33#D# => type D = Option[T] forSome { type T <: x.T; type x.type <: Foo with Singleton {} }
       |  Option => scala/Option#
       |  T => local10
       |  x => local12
       |  T => _empty_/Foo#T#
       |  x.type => local11
       |  Foo => _empty_/Foo#
       |  Singleton => scala/Singleton#
       |_empty_/e33#E# => type E = Option[y.type] forSome { type x.type <: Foo with Singleton {}; type y.type <: x.type#T with Singleton {} }
       |  Option => scala/Option#
       |  y.type => local13
       |  x.type => local14
       |  Foo => _empty_/Foo#
       |  Singleton => scala/Singleton#
       |  T => _empty_/Foo#T#
       |_empty_/e33#`<init>`(). => primary ctor <init>()
       |local2 => method yx.type
       |  x => local3
       |local3 => method xAnyRef
       |  AnyRef => scala/AnyRef#
       |local4 => abstract type T >: x.type <: AnyRef
       |  x => local6
       |  AnyRef => scala/AnyRef#
       |local6 => method xAnyRef
       |  AnyRef => scala/AnyRef#
       |local7 => abstract type T <: Option[x.type]
       |  Option => scala/Option#
       |  x => local9
       |local9 => method xAnyRef
       |  AnyRef => scala/AnyRef#
       |local10 => abstract type T <: x.T
       |  x => local12
       |  T => _empty_/Foo#T#
       |local12 => method xFoo
       |  Foo => _empty_/Foo#
       |local15 => method yx.T
       |  x => local16
       |  T => _empty_/Foo#T#
       |local16 => method xFoo
       |  Foo => _empty_/Foo#
       |""".stripMargin,
  )

  occurrences(
    "existential values in the declarations of existential types in type aliases",
    """|trait Foo { type T <: AnyRef }
       |class e33 {
       |  type A = Option[y.type] forSome { val x: AnyRef; val y: x.type }
       |  type B = Option[T] forSome { type T >: x.type <: AnyRef; val x: AnyRef }
       |  type C = Option[T] forSome { type T <: Option[x.type]; val x: AnyRef }
       |  type D = Option[T] forSome { type T <: x.T; val x: Foo }
       |  type E = Option[y.type] forSome { val x: Foo; val y: x.T }
       |}
       |""".stripMargin,
    """|[0:6..0:9): Foo <= _empty_/Foo#
       |[0:17..0:18): T <= _empty_/Foo#T#
       |[0:22..0:28): AnyRef => scala/AnyRef#
       |[1:6..1:9): e33 <= _empty_/e33#
       |[1:10..1:10):  <= _empty_/e33#`<init>`().
       |[2:7..2:8): A <= _empty_/e33#A#
       |[2:11..2:17): Option => scala/Option#
       |[2:18..2:19): y => local2
       |[2:40..2:41): x <= local3
       |[2:55..2:56): y <= local2
       |[2:58..2:59): x => local3
       |[3:7..3:8): B <= _empty_/e33#B#
       |[3:11..3:17): Option => scala/Option#
       |[3:18..3:19): T => local4
       |[3:36..3:37): T <= local4
       |[3:41..3:42): x => local6
       |[3:63..3:64): x <= local6
       |[4:7..4:8): C <= _empty_/e33#C#
       |[4:11..4:17): Option => scala/Option#
       |[4:18..4:19): T => local7
       |[4:36..4:37): T <= local7
       |[4:48..4:49): x => local9
       |[4:61..4:62): x <= local9
       |[5:7..5:8): D <= _empty_/e33#D#
       |[5:11..5:17): Option => scala/Option#
       |[5:18..5:19): T => local10
       |[5:36..5:37): T <= local10
       |[5:41..5:42): x => local12
       |[5:43..5:44): T => _empty_/Foo#T#
       |[5:50..5:51): x <= local12
       |[5:53..5:56): Foo => _empty_/Foo#
       |[6:7..6:8): E <= _empty_/e33#E#
       |[6:11..6:17): Option => scala/Option#
       |[6:18..6:19): y => local15
       |[6:40..6:41): x <= local16
       |[6:43..6:46): Foo => _empty_/Foo#
       |[6:52..6:53): y <= local15
       |[6:55..6:56): x => local16
       |[6:57..6:58): T => _empty_/Foo#T#
       |""".stripMargin,
  )

  symbols(
    "existential type bounded by an existential value in a value type",
    """|class e34 {
       |  val v: Option[T] forSome { type T <: x.type; val x: AnyRef } = None
       |}
       |""".stripMargin,
    """|_empty_/e34# => class e34 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e34#`<init>`(). => primary ctor <init>()
       |_empty_/e34#v. => val method v: Option[T] forSome { type T <: x.type; type x.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  T => local0
       |  x => local1
       |  x.type => local2
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local0 => abstract type T <: x.type
       |  x => local1
       |local1 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    "existential type bounded by an existential value in a value type",
    """|class e34 {
       |  val v: Option[T] forSome { type T <: x.type; val x: AnyRef } = None
       |}
       |""".stripMargin,
    """|[0:6..0:9): e34 <= _empty_/e34#
       |[0:10..0:10):  <= _empty_/e34#`<init>`().
       |[1:6..1:7): v <= _empty_/e34#v.
       |[1:9..1:15): Option => scala/Option#
       |[1:16..1:17): T => local0
       |[1:34..1:35): T <= local0
       |[1:39..1:40): x => local1
       |[1:51..1:52): x <= local1
       |[1:65..1:69): None => scala/None.
       |""".stripMargin,
  )

  test("existential type bounded by an existential value in a self type: symbols") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class e35 {
         |  self: (Option[T] forSome { type T <: x.type; val x: AnyRef }) =>
         |}
         |""".stripMargin,
      "Symbols",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("existential type bounded by an existential value in a self type: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class e35 {
         |  self: (Option[T] forSome { type T <: x.type; val x: AnyRef }) =>
         |}
         |""".stripMargin,
      "Occurrences",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  symbols(
    "nested existential types in classOf",
    """|object e36 {
       |  val a = classOf[(Option[(x.type, y.type)] forSome { val x: AnyRef }) forSome { val y: AnyRef }]
       |  val b = classOf[(Option[T] forSome { type T <: y.type }) forSome { val y: AnyRef }]
       |  val c = classOf[((Option[(x.type, y.type, z.type)] forSome { val x: AnyRef }) forSome { val y: AnyRef }) forSome { val z: AnyRef }]
       |}
       |""".stripMargin,
    """|_empty_/e36. => final object e36 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e36.a. => val method a: Class[Option[Tuple2[x.type, y.type]] forSome { type y.type <: AnyRef with Singleton {}; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  Tuple2 => scala/Tuple2#
       |  x.type => local0
       |  y.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e36.b. => val method b: Class[Option[T] forSome { type y.type <: AnyRef with Singleton {}; type T <: y.type }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  T => local2
       |  y.type => local3
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e36.c. => val method c: Class[Option[Tuple3[x.type, y.type, z.type]] forSome { type z.type <: AnyRef with Singleton {}; type y.type <: AnyRef with Singleton {}; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  Tuple3 => scala/Tuple3#
       |  x.type => local4
       |  y.type => local5
       |  z.type => local6
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |""".stripMargin,
  )

  occurrences(
    "nested existential types in classOf",
    """|object e36 {
       |  val a = classOf[(Option[(x.type, y.type)] forSome { val x: AnyRef }) forSome { val y: AnyRef }]
       |  val b = classOf[(Option[T] forSome { type T <: y.type }) forSome { val y: AnyRef }]
       |  val c = classOf[((Option[(x.type, y.type, z.type)] forSome { val x: AnyRef }) forSome { val y: AnyRef }) forSome { val z: AnyRef }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e36 <= _empty_/e36.
       |[1:6..1:7): a <= _empty_/e36.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/e36.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[3:6..3:7): c <= _empty_/e36.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  occurrences(
    "existential types in type arguments in classOf",
    """|object e37 {
       |  val a = classOf[Option[Option[x.type] forSome { val x: AnyRef }]]
       |  val b = classOf[Map[Option[x.type] forSome { val x: AnyRef }, Option[y.type] forSome { val y: AnyRef }]]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e37 <= _empty_/e37.
       |[1:6..1:7): a <= _empty_/e37.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[1:18..1:24): Option => scala/Option#
       |[2:6..2:7): b <= _empty_/e37.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:21): Map => scala/Predef.Map#
       |""".stripMargin,
  )

  symbols(
    "shadowed existential values",
    """|object e38 {
       |  val a = classOf[Option[Option[x.type] forSome { val x: AnyRef }] forSome { val x: Int }]
       |  val b = classOf[(Option[(x.type, x.type)] forSome { val x: AnyRef }) forSome { val x: Int }]
       |  type C = Option[Option[x.type] forSome { val x: AnyRef }] forSome { val x: Int }
       |}
       |""".stripMargin,
    """|_empty_/e38. => final object e38 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e38.C# => type C = Option[Option[x.type] forSome { type x.type <: AnyRef with Singleton {} }] forSome { type x.type <: Int with Singleton {} }
       |  Option => scala/Option#
       |  x.type => local4
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  x.type => local5
       |  Int => scala/Int#
       |_empty_/e38.a. => val method a: Class[Option[Option[x.type] forSome { type x.type <: AnyRef with Singleton {} }] forSome { type x.type <: Int with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  x.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  x.type => local1
       |  Int => scala/Int#
       |_empty_/e38.b. => val method b: Class[Option[Tuple2[x.type, x.type]] forSome { type x.type <: Int with Singleton {}; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  Tuple2 => scala/Tuple2#
       |  x.type => local2
       |  x.type => local3
       |  Int => scala/Int#
       |  Singleton => scala/Singleton#
       |  AnyRef => scala/AnyRef#
       |local6 => method xAnyRef
       |  AnyRef => scala/AnyRef#
       |local7 => method xInt
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "shadowed existential values",
    """|object e38 {
       |  val a = classOf[Option[Option[x.type] forSome { val x: AnyRef }] forSome { val x: Int }]
       |  val b = classOf[(Option[(x.type, x.type)] forSome { val x: AnyRef }) forSome { val x: Int }]
       |  type C = Option[Option[x.type] forSome { val x: AnyRef }] forSome { val x: Int }
       |}
       |""".stripMargin,
    """|[0:7..0:10): e38 <= _empty_/e38.
       |[1:6..1:7): a <= _empty_/e38.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/e38.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[3:7..3:8): C <= _empty_/e38.C#
       |[3:11..3:17): Option => scala/Option#
       |[3:18..3:24): Option => scala/Option#
       |[3:25..3:26): x => local6
       |[3:47..3:48): x <= local6
       |[3:74..3:75): x <= local7
       |""".stripMargin,
  )

  symbols(
    "unused existential values in classOf",
    """|object e39 {
       |  val a = classOf[Option[Int] forSome { val x: AnyRef }]
       |  val b = classOf[(Option[Int] forSome { val x: AnyRef }) forSome { val y: AnyRef }]
       |}
       |""".stripMargin,
    """|_empty_/e39. => final object e39 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e39.a. => val method a: Class[Option[Int] forSome { type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |  x.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e39.b. => val method b: Class[Option[Int] forSome { type y.type <: AnyRef with Singleton {}; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |  y.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  x.type => local2
       |""".stripMargin,
  )

  occurrences(
    "unused existential values in classOf",
    """|object e39 {
       |  val a = classOf[Option[Int] forSome { val x: AnyRef }]
       |  val b = classOf[(Option[Int] forSome { val x: AnyRef }) forSome { val y: AnyRef }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e39 <= _empty_/e39.
       |[1:6..1:7): a <= _empty_/e39.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/e39.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  symbols(
    "compound and refined types of existential values in classOf",
    """|object e40 {
       |  val a = classOf[Option[x.type] forSome { val x: Serializable with Cloneable }]
       |  val b = classOf[Option[x.type] forSome { val x: AnyRef with Serializable }]
       |  val c = classOf[Option[x.type] forSome { val x: AnyRef with Serializable with Cloneable }]
       |  val d = classOf[Option[x.type] forSome { val x: AnyRef { def f: Int } }]
       |  val e = classOf[Option[x.type] forSome { val x: AnyRef { val v: Int; type U <: Int } }]
       |  val f = classOf[Option[x.type] forSome { val x: Serializable with Cloneable { def f: Int } }]
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/e40. => final object e40 extends AnyRef { +6 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e40.a. => val method a: Class[Option[x.type] forSome { type x.type <: Serializable with Cloneable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local0
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.b. => val method b: Class[Option[x.type] forSome { type x.type <: AnyRef with Serializable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local1
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.c. => val method c: Class[Option[x.type] forSome { type x.type <: AnyRef with Serializable with Cloneable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local2
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.d. => val method d: Class[Option[x.type] forSome { type x.type <: AnyRef { def f: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local3
         |  AnyRef => scala/AnyRef#
         |  f => local4
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |_empty_/e40.e. => val method e: Class[Option[x.type] forSome { type x.type <: AnyRef { val def v: Int; type U <: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local5
         |  AnyRef => scala/AnyRef#
         |  v => local6
         |  Int => scala/Int#
         |  U => local7
         |  Singleton => scala/Singleton#
         |_empty_/e40.f. => val method f: Class[Option[x.type] forSome { type x.type <: Serializable with Cloneable { def f: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local8
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |  f => local9
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |""".stripMargin
    else
      """|_empty_/e40. => final object e40 extends AnyRef { +6 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e40.a. => val method a: Class[Option[x.type] forSome { type x.type <: Serializable with Cloneable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local0
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.b. => val method b: Class[Option[x.type] forSome { type x.type <: AnyRef with Serializable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local1
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/package.Serializable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.c. => val method c: Class[Option[x.type] forSome { type x.type <: AnyRef with Serializable with Cloneable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local2
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.d. => val method d: Class[Option[x.type] forSome { type x.type <: AnyRef { def f: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local3
         |  AnyRef => scala/AnyRef#
         |  f => local4
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |_empty_/e40.e. => val method e: Class[Option[x.type] forSome { type x.type <: AnyRef { val def v: Int; type U <: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local5
         |  AnyRef => scala/AnyRef#
         |  v => local6
         |  Int => scala/Int#
         |  U => local7
         |  Singleton => scala/Singleton#
         |_empty_/e40.f. => val method f: Class[Option[x.type] forSome { type x.type <: Serializable with Cloneable { def f: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local8
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |  f => local9
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |""".stripMargin,
  )

  occurrences(
    "compound and refined types of existential values in classOf",
    """|object e40 {
       |  val a = classOf[Option[x.type] forSome { val x: Serializable with Cloneable }]
       |  val b = classOf[Option[x.type] forSome { val x: AnyRef with Serializable }]
       |  val c = classOf[Option[x.type] forSome { val x: AnyRef with Serializable with Cloneable }]
       |  val d = classOf[Option[x.type] forSome { val x: AnyRef { def f: Int } }]
       |  val e = classOf[Option[x.type] forSome { val x: AnyRef { val v: Int; type U <: Int } }]
       |  val f = classOf[Option[x.type] forSome { val x: Serializable with Cloneable { def f: Int } }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e40 <= _empty_/e40.
       |[1:6..1:7): a <= _empty_/e40.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/e40.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[3:6..3:7): c <= _empty_/e40.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[4:6..4:7): d <= _empty_/e40.d.
       |[4:10..4:17): classOf => scala/Predef.classOf().
       |[5:6..5:7): e <= _empty_/e40.e.
       |[5:10..5:17): classOf => scala/Predef.classOf().
       |[6:6..6:7): f <= _empty_/e40.f.
       |[6:10..6:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  symbols(
    "compound and refined bounds of existential types in classOf",
    """|object e41 {
       |  val a = classOf[Option[T] forSome { type T <: Serializable with Cloneable }]
       |  val b = classOf[Option[T] forSome { type T >: Null <: AnyRef { def f: Int } }]
       |  val c = classOf[Option[T] forSome { type T <: AnyRef with Serializable with Cloneable }]
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/e41. => final object e41 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e41.a. => val method a: Class[Option[T] forSome { type T <: Serializable with Cloneable {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  T => local0
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |_empty_/e41.b. => val method b: Class[Option[T] forSome { type T >: Null <: AnyRef { def f: Int } }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  T => local1
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |  f => local2
         |  Int => scala/Int#
         |_empty_/e41.c. => val method c: Class[Option[T] forSome { type T <: AnyRef with Serializable with Cloneable {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  T => local3
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |""".stripMargin
    else
      """|_empty_/e41. => final object e41 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e41.a. => val method a: Class[Option[T] forSome { type T <: Serializable with Cloneable {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  T => local0
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |_empty_/e41.b. => val method b: Class[Option[T] forSome { type T >: Null <: AnyRef { def f: Int } }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  T => local1
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |  f => local2
         |  Int => scala/Int#
         |_empty_/e41.c. => val method c: Class[Option[T] forSome { type T <: AnyRef with Serializable with Cloneable {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  T => local3
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |""".stripMargin,
  )

  occurrences(
    "compound and refined bounds of existential types in classOf",
    """|object e41 {
       |  val a = classOf[Option[T] forSome { type T <: Serializable with Cloneable }]
       |  val b = classOf[Option[T] forSome { type T >: Null <: AnyRef { def f: Int } }]
       |  val c = classOf[Option[T] forSome { type T <: AnyRef with Serializable with Cloneable }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e41 <= _empty_/e41.
       |[1:6..1:7): a <= _empty_/e41.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/e41.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[3:6..3:7): c <= _empty_/e41.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  occurrences(
    "annotated types of existential values in classOf",
    """|object e42 {
       |  val a = classOf[Option[x.type] forSome { val x: AnyRef @unchecked }]
       |  val b = classOf[Option[T] forSome { type T <: AnyRef @unchecked }]
       |  val c = classOf[Option[x.type] forSome { val x: AnyRef @unchecked @deprecated }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e42 <= _empty_/e42.
       |[1:6..1:7): a <= _empty_/e42.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/e42.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[3:6..3:7): c <= _empty_/e42.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  occurrences(
    "compound and refined types of existential values in type aliases",
    """|object e43 {
       |  type A = Option[x.type] forSome { val x: Serializable with Cloneable }
       |  type B = Option[x.type] forSome { val x: AnyRef { def f: Int } }
       |  type C = Option[T] forSome { type T <: Serializable with Cloneable }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:10): e43 <= _empty_/e43.
         |[1:7..1:8): A <= _empty_/e43.A#
         |[1:11..1:17): Option => scala/Option#
         |[1:18..1:19): x => local1
         |[1:40..1:41): x <= local1
         |[1:43..1:55): Serializable => scala/Serializable#
         |[1:61..1:70): Cloneable => scala/Cloneable#
         |[2:7..2:8): B <= _empty_/e43.B#
         |[2:11..2:17): Option => scala/Option#
         |[2:18..2:19): x => local4
         |[2:40..2:41): x <= local4
         |[2:43..2:49): AnyRef => scala/AnyRef#
         |[2:56..2:57): f <= local3
         |[2:59..2:62): Int => scala/Int#
         |[3:7..3:8): C <= _empty_/e43.C#
         |[3:11..3:17): Option => scala/Option#
         |[3:18..3:19): T => local5
         |[3:36..3:37): T <= local5
         |[3:41..3:53): Serializable => scala/Serializable#
         |[3:59..3:68): Cloneable => scala/Cloneable#
         |""".stripMargin
    else
      """|[0:7..0:10): e43 <= _empty_/e43.
         |[1:7..1:8): A <= _empty_/e43.A#
         |[1:11..1:17): Option => scala/Option#
         |[1:18..1:19): x => local1
         |[1:40..1:41): x <= local1
         |[1:43..1:55): Serializable => scala/package.Serializable#
         |[1:61..1:70): Cloneable => scala/package.Cloneable#
         |[2:7..2:8): B <= _empty_/e43.B#
         |[2:11..2:17): Option => scala/Option#
         |[2:18..2:19): x => local4
         |[2:40..2:41): x <= local4
         |[2:43..2:49): AnyRef => scala/AnyRef#
         |[2:56..2:57): f <= local3
         |[2:59..2:62): Int => scala/Int#
         |[3:7..3:8): C <= _empty_/e43.C#
         |[3:11..3:17): Option => scala/Option#
         |[3:18..3:19): T => local5
         |[3:36..3:37): T <= local5
         |[3:41..3:53): Serializable => scala/package.Serializable#
         |[3:59..3:68): Cloneable => scala/package.Cloneable#
         |""".stripMargin,
  )

  occurrences(
    "existential values in a type application and isInstanceOf",
    """|object e44 {
       |  def g[A] = 1
       |  val a = g[Option[x.type] forSome { val x: AnyRef }]
       |  val b = (null: Any).isInstanceOf[Option[y.type] forSome { val y: AnyRef }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e44 <= _empty_/e44.
       |[1:6..1:7): g <= _empty_/e44.g().
       |[1:8..1:9): A <= _empty_/e44.g().[A]
       |[2:6..2:7): a <= _empty_/e44.a.
       |[2:10..2:11): g => _empty_/e44.g().
       |[2:12..2:18): Option => scala/Option#
       |[2:19..2:20): x => local0
       |[2:41..2:42): x <= local0
       |[3:6..3:7): b <= _empty_/e44.b.
       |[3:17..3:20): Any => scala/Any#
       |[3:22..3:34): isInstanceOf => scala/Any#isInstanceOf().
       |[3:35..3:41): Option => scala/Option#
       |[3:42..3:43): y => local1
       |[3:64..3:65): y <= local1
       |""".stripMargin,
  )

  occurrences(
    "existential value in the type argument of a constructor",
    """|class C[A]
       |object e45 { val c = new C[Option[y.type] forSome { val y: AnyRef }] }
       |""".stripMargin,
    """|[0:6..0:7): C <= _empty_/C#
       |[0:8..0:9): A <= _empty_/C#[A]
       |[0:10..0:10):  <= _empty_/C#`<init>`().
       |[1:7..1:10): e45 <= _empty_/e45.
       |[1:17..1:18): c <= _empty_/e45.c.
       |[1:25..1:26): C => _empty_/C#
       |[1:27..1:33): Option => scala/Option#
       |[1:34..1:35): y => local0
       |[1:56..1:57): y <= local0
       |[1:68..1:68):  => _empty_/C#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "existential value in classOf in an annotation",
    """|class ann(c: Class[_]) extends scala.annotation.StaticAnnotation
       |object e46 {
       |  @ann(classOf[Option[x.type] forSome { val x: AnyRef }]) def f = 1
       |}
       |""".stripMargin,
    """|[0:6..0:9): ann <= _empty_/ann#
       |[0:9..0:9):  <= _empty_/ann#`<init>`().
       |[0:10..0:11): c <= _empty_/ann#`<init>`().(c)
       |[0:10..0:11): c <= _empty_/ann#c.
       |[0:13..0:18): Class => scala/Predef.Class#
       |[0:19..0:20): _ <= local0
       |[0:31..0:36): scala => scala/
       |[0:37..0:47): annotation => scala/annotation/
       |[0:48..0:64): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[0:64..0:64):  => scala/annotation/Annotation#`<init>`().
       |[1:7..1:10): e46 <= _empty_/e46.
       |[2:3..2:6): ann => _empty_/ann#
       |[2:6..2:6):  => _empty_/ann#`<init>`().
       |[2:7..2:14): classOf => scala/Predef.classOf().
       |[2:62..2:63): f <= _empty_/e46.f().
       |""".stripMargin,
  )

  occurrences(
    "existential values in one declaration",
    """|object e47 {
       |  val a = classOf[Option[(x.type, y.type)] forSome { val x, y: AnyRef }]
       |  type B = Option[(x.type, y.type)] forSome { val x, y: AnyRef }
       |}
       |""".stripMargin,
    """|[0:7..0:10): e47 <= _empty_/e47.
       |[1:6..1:7): a <= _empty_/e47.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:7..2:8): B <= _empty_/e47.B#
       |[2:11..2:17): Option => scala/Option#
       |[2:19..2:20): x => local4
       |[2:27..2:28): y => local5
       |[2:50..2:51): x <= local4
       |[2:53..2:54): y <= local5
       |""".stripMargin,
  )

  occurrences(
    "existential value typed by a value",
    """|object e48 {
       |  def m(v: AnyRef) = classOf[Option[x.type] forSome { val x: v.type }]
       |  def n(v: AnyRef) = { type A = Option[x.type] forSome { val x: v.type }; 1 }
       |}
       |""".stripMargin,
    """|[0:7..0:10): e48 <= _empty_/e48.
       |[1:6..1:7): m <= _empty_/e48.m().
       |[1:8..1:9): v <= _empty_/e48.m().(v)
       |[1:11..1:17): AnyRef => scala/AnyRef#
       |[1:21..1:28): classOf => scala/Predef.classOf().
       |[2:6..2:7): n <= _empty_/e48.n().
       |[2:8..2:9): v <= _empty_/e48.n().(v)
       |[2:11..2:17): AnyRef => scala/AnyRef#
       |[2:28..2:29): A <= local1
       |[2:32..2:38): Option => scala/Option#
       |[2:39..2:40): x => local3
       |[2:61..2:62): x <= local3
       |[2:64..2:65): v => _empty_/e48.n().(v)
       |""".stripMargin,
  )

  occurrences(
    "existential type bounded by an existential value in classOf",
    """|object e49 {
       |  val c = classOf[Option[x.type] forSome { val x: AnyRef; type A <: x.type }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e49 <= _empty_/e49.
       |[1:6..1:7): c <= _empty_/e49.c.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  test("existential value in a self type with two traits: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|trait e50 { self: (Option[x.type] forSome { val x: AnyRef }) with Serializable with Cloneable => }
         |""".stripMargin,
      "Occurrences",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  test("self reference in a local class with an existential self type: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object e51 {
         |  def g = { class L { self: (Option[x.type] forSome { val x: AnyRef }) => def h = self }; 1 }
         |}
         |""".stripMargin,
      "Occurrences",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(err.getMessage, s"""unsupported type $tpe: NullaryMethodType(TypeRef(ThisType(scala), TypeName("AnyRef"), List()))""")
  }

  occurrences(
    "existential values in classOf of a wildcard, a function and a tuple",
    """|object e52 {
       |  val a = classOf[Map[x.type, _] forSome { val x: AnyRef }]
       |  val b = classOf[(Option[x.type] forSome { val x: AnyRef }) => Int]
       |  val c = classOf[(Option[x.type] forSome { val x: AnyRef }, Int)]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e52 <= _empty_/e52.
       |[1:6..1:7): a <= _empty_/e52.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/e52.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:64..2:67): Int => scala/Int#
       |[3:6..3:7): c <= _empty_/e52.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:61..3:64): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "infix types in classOf",
    """|object e53 {
       |  val a = classOf[Int Either String]
       |  val b = classOf[Option[x.type] forSome { val x: Int Either String }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e53 <= _empty_/e53.
       |[1:6..1:7): a <= _empty_/e53.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/e53.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  symbols(
    "methods with parameters in refinements of existential values",
    """|object e54 {
       |  val a = classOf[Option[x.type] forSome { val x: AnyRef { def f(a: Int): Int; def g[A](b: A)(c: Int): A } }]
       |  type B = Option[x.type] forSome { val x: AnyRef { def f(a: Int): Int; def g[A](b: A)(c: Int): A } }
       |}
       |""".stripMargin,
    """|_empty_/e54. => final object e54 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e54.B# => type B = Option[x.type] forSome { type x.type <: AnyRef { def f(a: Int): Int; def g[A](b: A)(c: Int): A } with Singleton {} }
       |  Option => scala/Option#
       |  x.type => local7
       |  AnyRef => scala/AnyRef#
       |  f => local8
       |  a => local9
       |  Int => scala/Int#
       |  g => local10
       |  A => local12
       |  b => local11
       |  c => local13
       |  Singleton => scala/Singleton#
       |_empty_/e54.a. => val method a: Class[Option[x.type] forSome { type x.type <: AnyRef { def f(a: Int): Int; def g[A](b: A)(c: Int): A } with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  x.type => local0
       |  AnyRef => scala/AnyRef#
       |  f => local1
       |  a => local2
       |  Int => scala/Int#
       |  g => local3
       |  A => local5
       |  b => local4
       |  c => local6
       |  Singleton => scala/Singleton#
       |local8 => abstract method f(a: Int): Int
       |  a => local9
       |  Int => scala/Int#
       |local9 => param a: Int
       |  Int => scala/Int#
       |local10 => abstract method g[A](b: A)(c: Int): A
       |  A => local12
       |  b => local11
       |  c => local13
       |  Int => scala/Int#
       |local11 => param b: A
       |  A => local12
       |local12 => typeparam A
       |local13 => param c: Int
       |  Int => scala/Int#
       |local14 => method xAnyRef { def f(a: Int): Int; def g[A](b: A)(c: Int): A }
       |  AnyRef => scala/AnyRef#
       |  f => local8
       |  a => local9
       |  Int => scala/Int#
       |  g => local10
       |  A => local12
       |  b => local11
       |  c => local13
       |""".stripMargin,
  )

  occurrences(
    "methods with parameters in refinements of existential values",
    """|object e54 {
       |  val a = classOf[Option[x.type] forSome { val x: AnyRef { def f(a: Int): Int; def g[A](b: A)(c: Int): A } }]
       |  type B = Option[x.type] forSome { val x: AnyRef { def f(a: Int): Int; def g[A](b: A)(c: Int): A } }
       |}
       |""".stripMargin,
    """|[0:7..0:10): e54 <= _empty_/e54.
       |[1:6..1:7): a <= _empty_/e54.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:7..2:8): B <= _empty_/e54.B#
       |[2:11..2:17): Option => scala/Option#
       |[2:18..2:19): x => local14
       |[2:40..2:41): x <= local14
       |[2:43..2:49): AnyRef => scala/AnyRef#
       |[2:56..2:57): f <= local8
       |[2:58..2:59): a <= local9
       |[2:61..2:64): Int => scala/Int#
       |[2:67..2:70): Int => scala/Int#
       |[2:76..2:77): g <= local10
       |[2:78..2:79): A <= local12
       |[2:81..2:82): b <= local11
       |[2:84..2:85): A => local12
       |[2:87..2:88): c <= local13
       |[2:90..2:93): Int => scala/Int#
       |[2:96..2:97): A => local12
       |""".stripMargin,
  )

  occurrences(
    "parenthesized compound types of existential values in classOf",
    """|object e55 {
       |  val a = classOf[Option[x.type] forSome { val x: (AnyRef with Serializable) with Cloneable }]
       |  val b = classOf[Option[x.type] forSome { val x: AnyRef with (Serializable with Cloneable) }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e55 <= _empty_/e55.
       |[1:6..1:7): a <= _empty_/e55.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/e55.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  occurrences(
    "existential value in classOf with a context bound and an implicit parameter",
    """|object e56 {
       |  val a = classOf[Option[x.type] forSome { val x: AnyRef { def f[A: Ordering](b: A)(implicit c: Int): A; def g[B <% Int](d: B): B } }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e56 <= _empty_/e56.
       |[1:6..1:7): a <= _empty_/e56.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  occurrences(
    "existential value in classOf with bounds after the last parameter",
    """|object e57 {
       |  val a = classOf[Option[x.type] forSome { val x: AnyRef { def f[A: Ordering](b: A): A; def g[B: Ordering]: B; def h[C <% Int: Ordering](c: C)(d: Int): C } }]
       |}
       |""".stripMargin,
    """|[0:7..0:10): e57 <= _empty_/e57.
       |[1:6..1:7): a <= _empty_/e57.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  occurrences(
    "overloaded methods in refinements of existential values in classOf",
    """|object p1 {
       |  val b = classOf[Option[x.type] forSome { val x: AnyRef { def f(a: Int): Int; def f(b: String): String } }]
       |  val c = classOf[Option[x.type] forSome { val x: AnyRef { def g(a: Int)(b: Int): Int; def g(c: Long): Long } }]
       |}
       |""".stripMargin,
    """|[0:7..0:9): p1 <= _empty_/p1.
       |[1:6..1:7): b <= _empty_/p1.b.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): c <= _empty_/p1.c.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  occurrences(
    "variable, type alias, by-name and repeated parameters in a refinement of an existential value in classOf",
    """|object p2 {
       |  val a = classOf[Option[x.type] forSome { val x: AnyRef { var v: Int; type T = String; def g(a: => Long, b: Double*): Unit } }]
       |}
       |""".stripMargin,
    """|[0:7..0:9): p2 <= _empty_/p2.
       |[1:6..1:7): a <= _empty_/p2.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  test("overloaded methods in a refinement of an existential value in a self type: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class p3 {
         |  self: (Option[x.type] forSome { val x: AnyRef { def f(a: Int): Int; def f(b: String): String } }) =>
         |}
         |""".stripMargin,
      "Occurrences",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(
      err.getMessage,
      s"""unsupported type $tpe{def f(a: Int): Int; def f(b: String): String}: NullaryMethodType(RefinedType(List(TypeRef(ThisType(scala), TypeName("AnyRef"), List())), Scope(TermName("f"), TermName("f"))))""",
    )
  }

  symbols(
    "existential values in a type parameter and class parameters",
    """|class e9[T <: Option[xt.type] forSome { val xt: AnyRef }](
       |    p: Option[xp.type] forSome { val xp: AnyRef },
       |    val q: Option[xq.type] forSome { val xq: AnyRef },
       |)
       |""".stripMargin,
    """|_empty_/e9# => class e9[T <: Option[xt.type] forSome { type xt.type <: AnyRef with Singleton {} }] extends AnyRef { +3 decls }
       |  T => _empty_/e9#[T]
       |  Option => scala/Option#
       |  xt.type => local2
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e9#[T] => typeparam T <: Option[xt.type] forSome { type xt.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xt.type => local2
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e9#`<init>`(). => primary ctor <init>(p: Option[xp.type] forSome { type xp.type <: AnyRef with Singleton {} }, val q: Option[xq.type] forSome { type xq.type <: AnyRef with Singleton {} })
       |  p => _empty_/e9#`<init>`().(p)
       |  Option => scala/Option#
       |  xp.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  q => _empty_/e9#`<init>`().(q)
       |  xq.type => local1
       |_empty_/e9#`<init>`().(p) => param p: Option[xp.type] forSome { type xp.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xp.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e9#`<init>`().(q) => val param q: Option[xq.type] forSome { type xq.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xq.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e9#p. => private[this] val method p: Option[xp.type] forSome { type xp.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xp.type => local0
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e9#q. => val method q: Option[xq.type] forSome { type xq.type <: AnyRef with Singleton {} }
       |  Option => scala/Option#
       |  xq.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local3 => method xtAnyRef
       |  AnyRef => scala/AnyRef#
       |local4 => val local xp: AnyRef
       |  AnyRef => scala/AnyRef#
       |local5 => val local xq: AnyRef
       |  AnyRef => scala/AnyRef#
       |local6 => val local xp: AnyRef
       |  AnyRef => scala/AnyRef#
       |local7 => val local xq: AnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    "existential values in a type parameter and class parameters",
    """|class e9[T <: Option[xt.type] forSome { val xt: AnyRef }](
       |    p: Option[xp.type] forSome { val xp: AnyRef },
       |    val q: Option[xq.type] forSome { val xq: AnyRef },
       |)
       |""".stripMargin,
    """|[0:6..0:8): e9 <= _empty_/e9#
       |[0:9..0:10): T <= _empty_/e9#[T]
       |[0:14..0:20): Option => scala/Option#
       |[0:21..0:23): xt => local3
       |[0:44..0:46): xt <= local3
       |[0:57..0:57):  <= _empty_/e9#`<init>`().
       |[1:4..1:5): p <= _empty_/e9#`<init>`().(p)
       |[1:4..1:5): p <= _empty_/e9#p.
       |[1:7..1:13): Option => scala/Option#
       |[1:14..1:16): xp => local4
       |[1:37..1:39): xp <= local4
       |[2:8..2:9): q <= _empty_/e9#`<init>`().(q)
       |[2:8..2:9): q <= _empty_/e9#q.
       |[2:11..2:17): Option => scala/Option#
       |[2:18..2:20): xq => local5
       |[2:41..2:43): xq <= local5
       |""".stripMargin,
  )

  occurrences(
    "val and def of one name in a refinement in classOf",
    """|object r1 {
       |  val a = classOf[Option[x.type] forSome { val x: AnyRef { val f: Int; def f(a: Int): Int } }]
       |  val b = classOf[Option[x.type] forSome { val x: AnyRef { def f(a: Int): Int; val f: Int } }]
       |  val c = classOf[Option[x.type] forSome { val x: AnyRef { var f: Int; def f(a: Int): Int } }]
       |}
       |""".stripMargin,
    """|[0:7..0:9): r1 <= _empty_/r1.
       |[1:6..1:7): a <= _empty_/r1.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/r1.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[3:6..3:7): c <= _empty_/r1.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  occurrences(
    "wildcard before an existential value in classOf",
    """|object r2 {
       |  val a = classOf[Map[_, x.type] forSome { val x: AnyRef }]
       |  val b = classOf[Option[x.y.type] forSome { val x: AnyRef { val y: AnyRef } }]
       |  val c = classOf[Option[x.type] forSome { val x: AnyRef { type F[A] = List[A]; def g: AnyRef { def h: Int } } }]
       |}
       |""".stripMargin,
    """|[0:7..0:9): r2 <= _empty_/r2.
       |[1:6..1:7): a <= _empty_/r2.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[2:6..2:7): b <= _empty_/r2.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[3:6..3:7): c <= _empty_/r2.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |""".stripMargin,
  )

  test("val and def of one name in a refinement in a self type: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class r3 {
         |  self: (Option[x.type] forSome { val x: AnyRef { def f(a: Int): Int; val f: Int } }) =>
         |}
         |""".stripMargin,
      "Occurrences",
    )
    val err = intercept[RuntimeException](load())
    val tpe = if (ScalaVersion.is212) "=> AnyRef" else "AnyRef"
    assertEquals(
      err.getMessage,
      s"""unsupported type $tpe{def f(a: Int): Int; val f: Int}: NullaryMethodType(RefinedType(List(TypeRef(ThisType(scala), TypeName("AnyRef"), List())), Scope(TermName("f"), TermName("f"))))""",
    )
  }

}

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
       |local2 => val local xe: AnyRef
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
       |[2:39..2:45): AnyRef => scala/AnyRef#
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
       |local1 => val local xe: AnyRef
       |  AnyRef => scala/AnyRef#
       |local3 => val local xt: AnyRef
       |  AnyRef => scala/AnyRef#
       |local5 => val local xl: AnyRef
       |  AnyRef => scala/AnyRef#
       |local7 => val local xp: AnyRef
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
       |[1:37..1:43): AnyRef => scala/AnyRef#
       |[2:7..2:8): T <= _empty_/e2#T#
       |[2:12..2:18): Option => scala/Option#
       |[2:19..2:21): xt => local3
       |[2:42..2:44): xt <= local3
       |[2:46..2:52): AnyRef => scala/AnyRef#
       |[3:7..3:8): L <= _empty_/e2#L#
       |[3:12..3:18): Option => scala/Option#
       |[3:19..3:21): xl => local5
       |[3:42..3:44): xl <= local5
       |[3:46..3:52): AnyRef => scala/AnyRef#
       |[4:6..4:7): f <= _empty_/e2#f().
       |[4:8..4:9): P <= _empty_/e2#f().[P]
       |[4:13..4:19): Option => scala/Option#
       |[4:20..4:22): xp => local7
       |[4:43..4:45): xp <= local7
       |[4:47..4:53): AnyRef => scala/AnyRef#
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
       |local1 => val local xt: AnyRef
       |  AnyRef => scala/AnyRef#
       |local3 => val local xf: AnyRef
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
       |[0:49..0:55): AnyRef => scala/AnyRef#
       |[0:58..0:58):  <= _empty_/e10#`<init>`().
       |[1:6..1:9): e11 <= _empty_/e11#
       |[1:17..1:18): F <= _empty_/e11#F#
       |[1:19..1:20): P <= _empty_/e11#F#[P]
       |[1:24..1:30): Option => scala/Option#
       |[1:31..1:33): xf => local3
       |[1:54..1:56): xf <= local3
       |[1:58..1:64): AnyRef => scala/AnyRef#
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
       |[1:43..1:49): AnyRef => scala/AnyRef#
       |[1:54..1:58): None => scala/None.
       |[2:6..2:7): d <= _empty_/e3#d().
       |[2:8..2:9): a <= _empty_/e3#d().(a)
       |[2:11..2:17): Option => scala/Option#
       |[2:18..2:20): xd => local3
       |[2:41..2:43): xd <= local3
       |[2:45..2:51): AnyRef => scala/AnyRef#
       |[2:57..2:58): a => _empty_/e3#d().(a)
       |[3:7..3:8): S <= _empty_/e3#S#
       |[3:17..3:18): r <= local4
       |[3:20..3:26): Option => scala/Option#
       |[3:27..3:29): xr => local6
       |[3:50..3:52): xr <= local6
       |[3:54..3:60): AnyRef => scala/AnyRef#
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
       |[2:53..2:59): AnyRef => scala/AnyRef#
       |[3:4..3:5): a => local1
       |[5:6..5:7): b <= _empty_/e13#b.
       |[5:11..5:15): None => scala/None.
       |[5:17..5:23): Option => scala/Option#
       |[5:24..5:26): xb => local2
       |[5:47..5:49): xb <= local2
       |[5:51..5:57): AnyRef => scala/AnyRef#
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
    assertNoDiff(
      load(),
      """|_empty_/e4# => class e4 extends AnyRef { self: Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} } => +1 decls }
         |  AnyRef => scala/AnyRef#
         |  Option => scala/Option#
         |  xs.type => local0
         |  Singleton => scala/Singleton#
         |_empty_/e4#`<init>`(). => primary ctor <init>()
         |local1 => val local xs: AnyRef
         |  AnyRef => scala/AnyRef#
         |local2 => selfparam self: Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} }
         |  Option => scala/Option#
         |  xs.type => local0
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |""".stripMargin,
    )
  }

  test("existential value in a self type: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class e4 {
         |  self: (Option[xs.type] forSome { val xs: AnyRef }) =>
         |}
         |""".stripMargin,
      "Occurrences",
    )
    assertNoDiff(
      load(),
      """|[0:6..0:8): e4 <= _empty_/e4#
         |[0:9..0:9):  <= _empty_/e4#`<init>`().
         |[1:2..1:6): self <= local2
         |[1:9..1:15): Option => scala/Option#
         |[1:16..1:18): xs => local1
         |[1:39..1:41): xs <= local1
         |[1:43..1:49): AnyRef => scala/AnyRef#
         |""".stripMargin,
    )
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
    assertNoDiff(
      load(),
      """|_empty_/e5# => trait e5 extends AnyRef { self: Option[xt.type] forSome { type xt.type <: AnyRef with Singleton {} } =>  }
         |  AnyRef => scala/AnyRef#
         |  Option => scala/Option#
         |  xt.type => local0
         |  Singleton => scala/Singleton#
         |_empty_/o5. => final object o5 extends AnyRef { self: Option[xo.type] forSome { type xo.type <: AnyRef with Singleton {} } =>  }
         |  AnyRef => scala/AnyRef#
         |  Option => scala/Option#
         |  xo.type => local3
         |  Singleton => scala/Singleton#
         |local1 => val local xt: AnyRef
         |  AnyRef => scala/AnyRef#
         |local2 => selfparam self: Option[xt.type] forSome { type xt.type <: AnyRef with Singleton {} }
         |  Option => scala/Option#
         |  xt.type => local0
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local4 => val local xo: AnyRef
         |  AnyRef => scala/AnyRef#
         |local5 => selfparam self: Option[xo.type] forSome { type xo.type <: AnyRef with Singleton {} }
         |  Option => scala/Option#
         |  xo.type => local3
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |""".stripMargin,
    )
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
    assertNoDiff(
      load(),
      if (ScalaVersion.is212)
        """|_empty_/e6. => final object e6 extends AnyRef { +1 decls }
           |  AnyRef => scala/AnyRef#
           |_empty_/e6.m(). => method m: Int
           |  Int => scala/Int#
           |local0 => class c extends AnyRef { self: Option[e6.this.xs.type] forSome { type xs.type <: AnyRef with Singleton {} } => +1 decls }
           |  AnyRef => scala/AnyRef#
           |  Option => scala/Option#
           |  e6 => _empty_/e6.
           |  xs => local1
           |  xs.type => local2
           |  Singleton => scala/Singleton#
           |local1 => val local xs: AnyRef
           |  AnyRef => scala/AnyRef#
           |local3 => primary ctor <init>()
           |local4 => selfparam self: Option[e6.this.xs.type] forSome { type xs.type <: AnyRef with Singleton {} }
           |  Option => scala/Option#
           |  e6 => _empty_/e6.
           |  xs => local1
           |  xs.type => local2
           |  AnyRef => scala/AnyRef#
           |  Singleton => scala/Singleton#
           |""".stripMargin
      else
        """|_empty_/e6. => final object e6 extends AnyRef { +1 decls }
           |  AnyRef => scala/AnyRef#
           |_empty_/e6.m(). => method m: Int
           |  Int => scala/Int#
           |local0 => class c extends AnyRef { self: Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} } => +1 decls }
           |  AnyRef => scala/AnyRef#
           |  Option => scala/Option#
           |  xs.type => local1
           |  Singleton => scala/Singleton#
           |local2 => primary ctor <init>()
           |local3 => val local xs: AnyRef
           |  AnyRef => scala/AnyRef#
           |local4 => selfparam self: Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} }
           |  Option => scala/Option#
           |  xs.type => local1
           |  AnyRef => scala/AnyRef#
           |  Singleton => scala/Singleton#
           |""".stripMargin,
    )
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
    assertNoDiff(
      load(),
      if (ScalaVersion.is212)
        """|[0:7..0:9): e6 <= _empty_/e6.
           |[1:6..1:7): m <= _empty_/e6.m().
           |[2:10..2:11): c <= local0
           |[2:12..2:12):  <= local3
           |[2:14..2:18): self <= local4
           |[2:21..2:27): Option => scala/Option#
           |[2:28..2:30): xs => local1
           |[2:51..2:53): xs <= local1
           |[2:55..2:61): AnyRef => scala/AnyRef#
           |""".stripMargin
      else
        """|[0:7..0:9): e6 <= _empty_/e6.
           |[1:6..1:7): m <= _empty_/e6.m().
           |[2:10..2:11): c <= local0
           |[2:12..2:12):  <= local2
           |[2:14..2:18): self <= local4
           |[2:21..2:27): Option => scala/Option#
           |[2:28..2:30): xs => local3
           |[2:51..2:53): xs <= local3
           |[2:55..2:61): AnyRef => scala/AnyRef#
           |""".stripMargin,
    )
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
    assertNoDiff(
      load(),
      if (ScalaVersion.is212)
        """|_empty_/e16. => final object e16 extends AnyRef { +2 decls }
           |  AnyRef => scala/AnyRef#
           |_empty_/e16.c# => class c extends AnyRef { self: Option[e16.this.xs.type] forSome { type xs.type <: AnyRef with Singleton {} } => +1 decls }
           |  AnyRef => scala/AnyRef#
           |  Option => scala/Option#
           |  e16 => _empty_/e16.
           |  xs => local0
           |  xs.type => local1
           |  Singleton => scala/Singleton#
           |_empty_/e16.c#`<init>`(). => primary ctor <init>()
           |_empty_/e16.d# => class d extends AnyRef { self: Option[e16.this.xd.type] forSome { type xd.type <: AnyRef with Singleton {} } with Serializable {} => +1 decls }
           |  AnyRef => scala/AnyRef#
           |  Option => scala/Option#
           |  e16 => _empty_/e16.
           |  xd => local3
           |  xd.type => local4
           |  Singleton => scala/Singleton#
           |  Serializable => scala/Serializable#
           |_empty_/e16.d#`<init>`(). => primary ctor <init>()
           |local0 => val local xs: AnyRef
           |  AnyRef => scala/AnyRef#
           |local2 => selfparam self: Option[e16.this.xs.type] forSome { type xs.type <: AnyRef with Singleton {} }
           |  Option => scala/Option#
           |  e16 => _empty_/e16.
           |  xs => local0
           |  xs.type => local1
           |  AnyRef => scala/AnyRef#
           |  Singleton => scala/Singleton#
           |local3 => val local xd: AnyRef
           |  AnyRef => scala/AnyRef#
           |local5 => selfparam self: Option[e16.this.xd.type] forSome { type xd.type <: AnyRef with Singleton {} } with Serializable {}
           |  Option => scala/Option#
           |  e16 => _empty_/e16.
           |  xd => local3
           |  xd.type => local4
           |  AnyRef => scala/AnyRef#
           |  Singleton => scala/Singleton#
           |  Serializable => scala/Serializable#
           |""".stripMargin
      else
        """|_empty_/e16. => final object e16 extends AnyRef { +2 decls }
           |  AnyRef => scala/AnyRef#
           |_empty_/e16.c# => class c extends AnyRef { self: Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} } => +1 decls }
           |  AnyRef => scala/AnyRef#
           |  Option => scala/Option#
           |  xs.type => local0
           |  Singleton => scala/Singleton#
           |_empty_/e16.c#`<init>`(). => primary ctor <init>()
           |_empty_/e16.d# => class d extends AnyRef { self: Option[xd.type] forSome { type xd.type <: AnyRef with Singleton {} } with Serializable {} => +1 decls }
           |  AnyRef => scala/AnyRef#
           |  Option => scala/Option#
           |  xd.type => local3
           |  Singleton => scala/Singleton#
           |  Serializable => scala/package.Serializable#
           |_empty_/e16.d#`<init>`(). => primary ctor <init>()
           |local1 => val local xs: AnyRef
           |  AnyRef => scala/AnyRef#
           |local2 => selfparam self: Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} }
           |  Option => scala/Option#
           |  xs.type => local0
           |  AnyRef => scala/AnyRef#
           |  Singleton => scala/Singleton#
           |local4 => val local xd: AnyRef
           |  AnyRef => scala/AnyRef#
           |local5 => selfparam self: Option[xd.type] forSome { type xd.type <: AnyRef with Singleton {} } with Serializable {}
           |  Option => scala/Option#
           |  xd.type => local3
           |  AnyRef => scala/AnyRef#
           |  Singleton => scala/Singleton#
           |  Serializable => scala/package.Serializable#
           |""".stripMargin,
    )
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
    assertNoDiff(
      load(),
      if (ScalaVersion.is212)
        """|[0:7..0:10): e16 <= _empty_/e16.
           |[1:8..1:9): c <= _empty_/e16.c#
           |[1:10..1:10):  <= _empty_/e16.c#`<init>`().
           |[1:12..1:16): self <= local2
           |[1:19..1:25): Option => scala/Option#
           |[1:26..1:28): xs => local0
           |[1:49..1:51): xs <= local0
           |[1:53..1:59): AnyRef => scala/AnyRef#
           |[2:8..2:9): d <= _empty_/e16.d#
           |[2:10..2:10):  <= _empty_/e16.d#`<init>`().
           |[2:12..2:16): self <= local5
           |[2:19..2:25): Option => scala/Option#
           |[2:26..2:28): xd => local3
           |[2:49..2:51): xd <= local3
           |[2:53..2:59): AnyRef => scala/AnyRef#
           |[2:68..2:80): Serializable => scala/Serializable#
           |""".stripMargin
      else
        """|[0:7..0:10): e16 <= _empty_/e16.
           |[1:8..1:9): c <= _empty_/e16.c#
           |[1:10..1:10):  <= _empty_/e16.c#`<init>`().
           |[1:12..1:16): self <= local2
           |[1:19..1:25): Option => scala/Option#
           |[1:26..1:28): xs => local1
           |[1:49..1:51): xs <= local1
           |[1:53..1:59): AnyRef => scala/AnyRef#
           |[2:8..2:9): d <= _empty_/e16.d#
           |[2:10..2:10):  <= _empty_/e16.d#`<init>`().
           |[2:12..2:16): self <= local5
           |[2:19..2:25): Option => scala/Option#
           |[2:26..2:28): xd => local4
           |[2:49..2:51): xd <= local4
           |[2:53..2:59): AnyRef => scala/AnyRef#
           |[2:68..2:80): Serializable => scala/package.Serializable#
           |""".stripMargin,
    )
  }

  test("existential value in a compound self type: symbols") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class e7 {
         |  self: (Option[xs.type] forSome { val xs: AnyRef }) with Serializable =>
         |}
         |""".stripMargin,
      "Symbols",
    )
    assertNoDiff(
      load(),
      if (ScalaVersion.is212)
        """|_empty_/e7# => class e7 extends AnyRef { self: Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} } with Serializable {} => +1 decls }
           |  AnyRef => scala/AnyRef#
           |  Option => scala/Option#
           |  xs.type => local0
           |  Singleton => scala/Singleton#
           |  Serializable => scala/Serializable#
           |_empty_/e7#`<init>`(). => primary ctor <init>()
           |local1 => val local xs: AnyRef
           |  AnyRef => scala/AnyRef#
           |local2 => selfparam self: Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} } with Serializable {}
           |  Option => scala/Option#
           |  xs.type => local0
           |  AnyRef => scala/AnyRef#
           |  Singleton => scala/Singleton#
           |  Serializable => scala/Serializable#
           |""".stripMargin
      else
        """|_empty_/e7# => class e7 extends AnyRef { self: Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} } with Serializable {} => +1 decls }
           |  AnyRef => scala/AnyRef#
           |  Option => scala/Option#
           |  xs.type => local0
           |  Singleton => scala/Singleton#
           |  Serializable => scala/package.Serializable#
           |_empty_/e7#`<init>`(). => primary ctor <init>()
           |local1 => val local xs: AnyRef
           |  AnyRef => scala/AnyRef#
           |local2 => selfparam self: Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} } with Serializable {}
           |  Option => scala/Option#
           |  xs.type => local0
           |  AnyRef => scala/AnyRef#
           |  Singleton => scala/Singleton#
           |  Serializable => scala/package.Serializable#
           |""".stripMargin,
    )
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
    assertNoDiff(
      load(),
      """|[2:10..2:14): Some => *.apply[e8 with Option[xs.type] forSome { type xs.type <: AnyRef with Singleton {} } {}]
         |  apply => scala/Some.apply().
         |  e8 => _empty_/e8#
         |  Option => scala/Option#
         |  xs.type => local0
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |""".stripMargin,
    )
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
       |[1:9..1:15): Option => scala/Option#
       |[1:16..1:17): T => local0
       |[1:34..1:35): T <= local0
       |[1:39..1:45): AnyRef => scala/AnyRef#
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
           |[2:25..2:26): v => _empty_/e20.v.
           |[3:8..3:9): d <= _empty_/e20.d#
           |[3:10..3:10):  <= _empty_/e20.d#`<init>`().
           |[3:12..3:16): self <= local1
           |[3:18..3:24): Option => scala/Option#
           |[3:25..3:26): v => _empty_/e20.v.
           |[3:38..3:50): Serializable => scala/Serializable#
           |[4:8..4:9): f <= _empty_/e20.f#
           |[4:10..4:10):  <= _empty_/e20.f#`<init>`().
           |[4:12..4:16): self <= local2
           |[4:18..4:19): v => _empty_/e20.v.
           |""".stripMargin
      else
        """|[0:7..0:10): e20 <= _empty_/e20.
           |[1:6..1:7): v <= _empty_/e20.v.
           |[1:9..1:15): AnyRef => scala/AnyRef#
           |[2:8..2:9): c <= _empty_/e20.c#
           |[2:10..2:10):  <= _empty_/e20.c#`<init>`().
           |[2:12..2:16): self <= local0
           |[2:18..2:24): Option => scala/Option#
           |[2:25..2:26): v => _empty_/e20.v.
           |[3:8..3:9): d <= _empty_/e20.d#
           |[3:10..3:10):  <= _empty_/e20.d#`<init>`().
           |[3:12..3:16): self <= local1
           |[3:18..3:24): Option => scala/Option#
           |[3:25..3:26): v => _empty_/e20.v.
           |[3:38..3:50): Serializable => scala/package.Serializable#
           |[4:8..4:9): f <= _empty_/e20.f#
           |[4:10..4:10):  <= _empty_/e20.f#`<init>`().
           |[4:12..4:16): self <= local2
           |[4:18..4:19): v => _empty_/e20.v.
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
       |[0:47..0:53): AnyRef => scala/AnyRef#
       |[0:61..0:62): c <= _empty_/e21#`<init>`().(c)
       |[0:61..0:62): c <= _empty_/e21#c().
       |[0:64..0:70): Option => scala/Option#
       |[0:71..0:73): xc => local3
       |[0:94..0:96): xc <= local3
       |[0:98..0:104): AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  test("existential value in a case class parameter: symbols") {
    def load() = computeDatabaseSectionFromSnippet(
      """|case class e24(b: Option[xb.type] forSome { val xb: AnyRef })
         |""".stripMargin,
      "Symbols",
    )
    assertNoDiff(
      load().linesIterator.filter(_.startsWith("local")).mkString("\n"),
      """|local1 => val local xb: AnyRef
         |local2 => synthetic val local e24$1: e24
         |""".stripMargin,
    )
  }

  test("existential value in a case class parameter: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|case class e24(b: Option[xb.type] forSome { val xb: AnyRef })
         |""".stripMargin,
      "Occurrences",
    )
    assertNoDiff(
      load(),
      """|[0:11..0:14): e24 <= _empty_/e24#
         |[0:14..0:14):  <= _empty_/e24#`<init>`().
         |[0:15..0:16): b <= _empty_/e24#`<init>`().(b)
         |[0:15..0:16): b <= _empty_/e24#b.
         |[0:18..0:24): Option => scala/Option#
         |[0:25..0:27): xb => local1
         |[0:48..0:50): xb <= local1
         |[0:52..0:58): AnyRef => scala/AnyRef#
         |""".stripMargin,
    )
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
       |[1:45..1:51): AnyRef => scala/AnyRef#
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
       |  xc.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local0 => val local xc: AnyRef
       |  AnyRef => scala/AnyRef#
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:27): xc => local0
       |[1:48..1:50): xc <= local0
       |[1:52..1:58): AnyRef => scala/AnyRef#
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
         |  xd.type => local4
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |local0 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local1 => abstract type U[V <: Int] => Any
         |  V => local2
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local2 => typeparam V <: Int
         |  Int => scala/Int#
         |local3 => val local xd: Option[Int]
         |  Option => scala/Option#
         |  Int => scala/Int#
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
         |  xd.type => local4
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |local0 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local1 => abstract type U[V <: Int]
         |  V => local2
         |  Int => scala/Int#
         |local2 => typeparam V <: Int
         |  Int => scala/Int#
         |local3 => val local xd: Option[Int]
         |  Option => scala/Option#
         |  Int => scala/Int#
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:26): T => local0
       |[1:43..1:44): T <= local0
       |[1:48..1:54): AnyRef => scala/AnyRef#
       |[1:61..1:62): U <= local1
       |[1:63..1:64): V <= local2
       |[1:68..1:71): Int => scala/Int#
       |[2:6..2:7): d <= _empty_/e26.d.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:31): Option => scala/Option#
       |[2:32..2:34): xd => local3
       |[2:55..2:57): xd <= local3
       |[2:59..2:65): Option => scala/Option#
       |[2:66..2:69): Int => scala/Int#
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
       |[1:39..1:43): Null => scala/Null#
       |[1:47..1:53): AnyRef => scala/AnyRef#
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
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:26): x => local0
       |[2:27..2:28): T => _empty_/Foo#T#
       |[2:44..2:45): x <= local0
       |[2:47..2:50): Foo => _empty_/Foo#
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
       |[3:18..3:24): Option => scala/Option#
       |[3:25..3:26): x => local0
       |[3:32..3:33): T => _empty_/Foo#T#
       |[3:49..3:50): x <= local0
       |[3:52..3:55): Foo => _empty_/Foo#
       |[4:6..4:7): d <= _empty_/e30.d.
       |[4:10..4:17): classOf => scala/Predef.classOf().
       |[4:18..4:24): Option => scala/Option#
       |[4:25..4:26): x => local2
       |[4:27..4:28): y => _empty_/Bar#y.
       |[4:29..4:30): T => _empty_/Foo#T#
       |[4:46..4:47): x <= local2
       |[4:49..4:52): Bar => _empty_/Bar#
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
       |  y.type => local2
       |  x.type => local3
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e31.b. => val method b: Class[Option[T] forSome { type T >: x.type <: AnyRef; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  T => local4
       |  x => local5
       |  AnyRef => scala/AnyRef#
       |  x.type => local6
       |  Singleton => scala/Singleton#
       |_empty_/e31.c. => val method c: Class[Option[T] forSome { type T <: Option[x.type]; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  T => local7
       |  x => local8
       |  x.type => local9
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e31.d. => val method d: Class[Option[T] forSome { type T <: x.T; type x.type <: Foo with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  T => local10
       |  x => local11
       |  T => _empty_/Foo#T#
       |  x.type => local12
       |  Foo => _empty_/Foo#
       |  Singleton => scala/Singleton#
       |_empty_/e31.e. => val method e: Class[Option[y.type] forSome { type x.type <: Foo with Singleton {}; type y.type <: x.type#T with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  y.type => local15
       |  x.type => local16
       |  Foo => _empty_/Foo#
       |  Singleton => scala/Singleton#
       |  T => _empty_/Foo#T#
       |local0 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |local1 => val local y: x.type
       |  x => local0
       |local4 => abstract type T >: x.type <: AnyRef
       |  x => local5
       |  AnyRef => scala/AnyRef#
       |local5 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |local7 => abstract type T <: Option[x.type]
       |  Option => scala/Option#
       |  x => local8
       |local8 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |local10 => abstract type T <: x.T
       |  x => local11
       |  T => _empty_/Foo#T#
       |local11 => val local x: Foo
       |  Foo => _empty_/Foo#
       |local13 => val local x: Foo
       |  Foo => _empty_/Foo#
       |local14 => val local y: x.T
       |  x => local13
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
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:26): y => local1
       |[2:47..2:48): x <= local0
       |[2:50..2:56): AnyRef => scala/AnyRef#
       |[2:62..2:63): y <= local1
       |[2:65..2:66): x => local0
       |[3:6..3:7): b <= _empty_/e31.b.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:24): Option => scala/Option#
       |[3:25..3:26): T => local4
       |[3:43..3:44): T <= local4
       |[3:48..3:49): x => local5
       |[3:58..3:64): AnyRef => scala/AnyRef#
       |[3:70..3:71): x <= local5
       |[3:73..3:79): AnyRef => scala/AnyRef#
       |[4:6..4:7): c <= _empty_/e31.c.
       |[4:10..4:17): classOf => scala/Predef.classOf().
       |[4:18..4:24): Option => scala/Option#
       |[4:25..4:26): T => local7
       |[4:43..4:44): T <= local7
       |[4:48..4:54): Option => scala/Option#
       |[4:55..4:56): x => local8
       |[4:68..4:69): x <= local8
       |[4:71..4:77): AnyRef => scala/AnyRef#
       |[5:6..5:7): d <= _empty_/e31.d.
       |[5:10..5:17): classOf => scala/Predef.classOf().
       |[5:18..5:24): Option => scala/Option#
       |[5:25..5:26): T => local10
       |[5:43..5:44): T <= local10
       |[5:48..5:49): x => local11
       |[5:50..5:51): T => _empty_/Foo#T#
       |[5:57..5:58): x <= local11
       |[5:60..5:63): Foo => _empty_/Foo#
       |[6:6..6:7): e <= _empty_/e31.e.
       |[6:10..6:17): classOf => scala/Predef.classOf().
       |[6:18..6:24): Option => scala/Option#
       |[6:25..6:26): y => local14
       |[6:47..6:48): x <= local13
       |[6:50..6:53): Foo => _empty_/Foo#
       |[6:59..6:60): y <= local14
       |[6:62..6:63): x => local13
       |[6:64..6:65): T => _empty_/Foo#T#
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
         |  x => local2
         |  Any => scala/Any#
         |  x.type => local3
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local0 => abstract type U[V <: x.type] => Any
         |  V => local1
         |  x => local2
         |  Any => scala/Any#
         |local1 => typeparam V <: x.type
         |  x => local2
         |local2 => val local x: AnyRef
         |  AnyRef => scala/AnyRef#
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
         |  x => local2
         |  x.type => local3
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local0 => abstract type U[V <: x.type]
         |  V => local1
         |  x => local2
         |local1 => typeparam V <: x.type
         |  x => local2
         |local2 => val local x: AnyRef
         |  AnyRef => scala/AnyRef#
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:26): U => local0
       |[1:27..1:30): Int => scala/Int#
       |[1:48..1:49): U <= local0
       |[1:50..1:51): V <= local1
       |[1:55..1:56): x => local2
       |[1:68..1:69): x <= local2
       |[1:71..1:77): AnyRef => scala/AnyRef#
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
       |local2 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |local3 => val local y: x.type
       |  x => local2
       |local4 => abstract type T >: x.type <: AnyRef
       |  x => local6
       |  AnyRef => scala/AnyRef#
       |local6 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |local7 => abstract type T <: Option[x.type]
       |  Option => scala/Option#
       |  x => local9
       |local9 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |local10 => abstract type T <: x.T
       |  x => local12
       |  T => _empty_/Foo#T#
       |local12 => val local x: Foo
       |  Foo => _empty_/Foo#
       |local15 => val local x: Foo
       |  Foo => _empty_/Foo#
       |local16 => val local y: x.T
       |  x => local15
       |  T => _empty_/Foo#T#
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
       |[2:18..2:19): y => local3
       |[2:40..2:41): x <= local2
       |[2:43..2:49): AnyRef => scala/AnyRef#
       |[2:55..2:56): y <= local3
       |[2:58..2:59): x => local2
       |[3:7..3:8): B <= _empty_/e33#B#
       |[3:11..3:17): Option => scala/Option#
       |[3:18..3:19): T => local4
       |[3:36..3:37): T <= local4
       |[3:41..3:42): x => local6
       |[3:51..3:57): AnyRef => scala/AnyRef#
       |[3:63..3:64): x <= local6
       |[3:66..3:72): AnyRef => scala/AnyRef#
       |[4:7..4:8): C <= _empty_/e33#C#
       |[4:11..4:17): Option => scala/Option#
       |[4:18..4:19): T => local7
       |[4:36..4:37): T <= local7
       |[4:41..4:47): Option => scala/Option#
       |[4:48..4:49): x => local9
       |[4:61..4:62): x <= local9
       |[4:64..4:70): AnyRef => scala/AnyRef#
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
       |[6:18..6:19): y => local16
       |[6:40..6:41): x <= local15
       |[6:43..6:46): Foo => _empty_/Foo#
       |[6:52..6:53): y <= local16
       |[6:55..6:56): x => local15
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
       |[1:54..1:60): AnyRef => scala/AnyRef#
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
    assertNoDiff(
      load(),
      """|_empty_/e35# => class e35 extends AnyRef { self: Option[T] forSome { type T <: x.type; type x.type <: AnyRef with Singleton {} } => +1 decls }
         |  AnyRef => scala/AnyRef#
         |  Option => scala/Option#
         |  T => local0
         |  x => local2
         |  x.type => local1
         |  Singleton => scala/Singleton#
         |_empty_/e35#`<init>`(). => primary ctor <init>()
         |local0 => abstract type T <: x.type
         |  x => local2
         |local2 => val local x: AnyRef
         |  AnyRef => scala/AnyRef#
         |local3 => selfparam self: Option[T] forSome { type T <: x.type; type x.type <: AnyRef with Singleton {} }
         |  Option => scala/Option#
         |  T => local0
         |  x => local2
         |  x.type => local1
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |""".stripMargin,
    )
  }

  test("existential type bounded by an existential value in a self type: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|class e35 {
         |  self: (Option[T] forSome { type T <: x.type; val x: AnyRef }) =>
         |}
         |""".stripMargin,
      "Occurrences",
    )
    assertNoDiff(
      load(),
      """|[0:6..0:9): e35 <= _empty_/e35#
         |[0:10..0:10):  <= _empty_/e35#`<init>`().
         |[1:2..1:6): self <= local3
         |[1:9..1:15): Option => scala/Option#
         |[1:16..1:17): T => local0
         |[1:34..1:35): T <= local0
         |[1:39..1:40): x => local2
         |[1:51..1:52): x <= local2
         |[1:54..1:60): AnyRef => scala/AnyRef#
         |""".stripMargin,
    )
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
       |  x.type => local2
       |  y.type => local3
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e36.b. => val method b: Class[Option[T] forSome { type y.type <: AnyRef with Singleton {}; type T <: y.type }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  T => local5
       |  y.type => local6
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  y => local4
       |_empty_/e36.c. => val method c: Class[Option[Tuple3[x.type, y.type, z.type]] forSome { type z.type <: AnyRef with Singleton {}; type y.type <: AnyRef with Singleton {}; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  Tuple3 => scala/Tuple3#
       |  x.type => local10
       |  y.type => local11
       |  z.type => local12
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local0 => val local y: AnyRef
       |  AnyRef => scala/AnyRef#
       |local1 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |local4 => val local y: AnyRef
       |  AnyRef => scala/AnyRef#
       |local5 => abstract type T <: y.type
       |  y => local4
       |local7 => val local z: AnyRef
       |  AnyRef => scala/AnyRef#
       |local8 => val local y: AnyRef
       |  AnyRef => scala/AnyRef#
       |local9 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
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
       |[1:19..1:25): Option => scala/Option#
       |[1:27..1:28): x => local1
       |[1:35..1:36): y => local0
       |[1:58..1:59): x <= local1
       |[1:61..1:67): AnyRef => scala/AnyRef#
       |[1:85..1:86): y <= local0
       |[1:88..1:94): AnyRef => scala/AnyRef#
       |[2:6..2:7): b <= _empty_/e36.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:19..2:25): Option => scala/Option#
       |[2:26..2:27): T => local5
       |[2:44..2:45): T <= local5
       |[2:49..2:50): y => local4
       |[2:73..2:74): y <= local4
       |[2:76..2:82): AnyRef => scala/AnyRef#
       |[3:6..3:7): c <= _empty_/e36.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:20..3:26): Option => scala/Option#
       |[3:28..3:29): x => local9
       |[3:36..3:37): y => local8
       |[3:44..3:45): z => local7
       |[3:67..3:68): x <= local9
       |[3:70..3:76): AnyRef => scala/AnyRef#
       |[3:94..3:95): y <= local8
       |[3:97..3:103): AnyRef => scala/AnyRef#
       |[3:121..3:122): z <= local7
       |[3:124..3:130): AnyRef => scala/AnyRef#
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
       |[1:25..1:31): Option => scala/Option#
       |[1:32..1:33): x => local0
       |[1:54..1:55): x <= local0
       |[1:57..1:63): AnyRef => scala/AnyRef#
       |[2:6..2:7): b <= _empty_/e37.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:21): Map => scala/Predef.Map#
       |[2:22..2:28): Option => scala/Option#
       |[2:29..2:30): x => local2
       |[2:51..2:52): x <= local2
       |[2:54..2:60): AnyRef => scala/AnyRef#
       |[2:64..2:70): Option => scala/Option#
       |[2:71..2:72): y => local3
       |[2:93..2:94): y <= local3
       |[2:96..2:102): AnyRef => scala/AnyRef#
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
       |  x.type => local8
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  x.type => local9
       |  Int => scala/Int#
       |_empty_/e38.a. => val method a: Class[Option[Option[x.type] forSome { type x.type <: AnyRef with Singleton {} }] forSome { type x.type <: Int with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  x.type => local2
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  x.type => local3
       |  Int => scala/Int#
       |_empty_/e38.b. => val method b: Class[Option[Tuple2[x.type, x.type]] forSome { type x.type <: Int with Singleton {}; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  Tuple2 => scala/Tuple2#
       |  x.type => local6
       |  x.type => local7
       |  Int => scala/Int#
       |  Singleton => scala/Singleton#
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |local4 => val local x: Int
       |  Int => scala/Int#
       |local5 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |local10 => val local x: Int
       |  Int => scala/Int#
       |local11 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:31): Option => scala/Option#
       |[1:32..1:33): x => local1
       |[1:54..1:55): x <= local1
       |[1:57..1:63): AnyRef => scala/AnyRef#
       |[1:81..1:82): x <= local0
       |[1:84..1:87): Int => scala/Int#
       |[2:6..2:7): b <= _empty_/e38.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:19..2:25): Option => scala/Option#
       |[2:27..2:28): x => local5
       |[2:35..2:36): x => local5
       |[2:58..2:59): x <= local5
       |[2:61..2:67): AnyRef => scala/AnyRef#
       |[2:85..2:86): x <= local4
       |[2:88..2:91): Int => scala/Int#
       |[3:7..3:8): C <= _empty_/e38.C#
       |[3:11..3:17): Option => scala/Option#
       |[3:18..3:24): Option => scala/Option#
       |[3:25..3:26): x => local11
       |[3:47..3:48): x <= local11
       |[3:50..3:56): AnyRef => scala/AnyRef#
       |[3:74..3:75): x <= local10
       |[3:77..3:80): Int => scala/Int#
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
       |  x.type => local1
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |_empty_/e39.b. => val method b: Class[Option[Int] forSome { type y.type <: AnyRef with Singleton {}; type x.type <: AnyRef with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |  y.type => local4
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |  x.type => local5
       |local0 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
       |local2 => val local y: AnyRef
       |  AnyRef => scala/AnyRef#
       |local3 => val local x: AnyRef
       |  AnyRef => scala/AnyRef#
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:28): Int => scala/Int#
       |[1:44..1:45): x <= local0
       |[1:47..1:53): AnyRef => scala/AnyRef#
       |[2:6..2:7): b <= _empty_/e39.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:19..2:25): Option => scala/Option#
       |[2:26..2:29): Int => scala/Int#
       |[2:45..2:46): x <= local3
       |[2:48..2:54): AnyRef => scala/AnyRef#
       |[2:72..2:73): y <= local2
       |[2:75..2:81): AnyRef => scala/AnyRef#
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
         |  x.type => local1
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.b. => val method b: Class[Option[x.type] forSome { type x.type <: AnyRef with Serializable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local3
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.c. => val method c: Class[Option[x.type] forSome { type x.type <: AnyRef with Serializable with Cloneable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local5
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.d. => val method d: Class[Option[x.type] forSome { type x.type <: AnyRef { def f: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local8
         |  AnyRef => scala/AnyRef#
         |  f => local7
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |_empty_/e40.e. => val method e: Class[Option[x.type] forSome { type x.type <: AnyRef { val def v: Int; type U <: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local12
         |  AnyRef => scala/AnyRef#
         |  v => local10
         |  Int => scala/Int#
         |  U => local11
         |  Singleton => scala/Singleton#
         |_empty_/e40.f. => val method f: Class[Option[x.type] forSome { type x.type <: Serializable with Cloneable { def f: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local15
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |  f => local14
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |local0 => val local x: Serializable with Cloneable {}
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |local2 => val local x: AnyRef with Serializable {}
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |local4 => val local x: AnyRef with Serializable with Cloneable {}
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |local6 => val local x: AnyRef { def f: Int }
         |  AnyRef => scala/AnyRef#
         |  f => local7
         |  Int => scala/Int#
         |local7 => abstract method f: Int
         |  Int => scala/Int#
         |local9 => val local x: AnyRef { val def v: Int; type U <: Int }
         |  AnyRef => scala/AnyRef#
         |  v => local10
         |  Int => scala/Int#
         |  U => local11
         |local10 => abstract val method v: Int
         |  Int => scala/Int#
         |local11 => abstract type U <: Int
         |  Int => scala/Int#
         |local13 => val local x: Serializable with Cloneable { def f: Int }
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |  f => local14
         |  Int => scala/Int#
         |local14 => abstract method f: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/e40. => final object e40 extends AnyRef { +6 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e40.a. => val method a: Class[Option[x.type] forSome { type x.type <: Serializable with Cloneable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local1
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.b. => val method b: Class[Option[x.type] forSome { type x.type <: AnyRef with Serializable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local3
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/package.Serializable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.c. => val method c: Class[Option[x.type] forSome { type x.type <: AnyRef with Serializable with Cloneable {} with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local5
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |  Singleton => scala/Singleton#
         |_empty_/e40.d. => val method d: Class[Option[x.type] forSome { type x.type <: AnyRef { def f: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local8
         |  AnyRef => scala/AnyRef#
         |  f => local7
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |_empty_/e40.e. => val method e: Class[Option[x.type] forSome { type x.type <: AnyRef { val def v: Int; type U <: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local12
         |  AnyRef => scala/AnyRef#
         |  v => local10
         |  Int => scala/Int#
         |  U => local11
         |  Singleton => scala/Singleton#
         |_empty_/e40.f. => val method f: Class[Option[x.type] forSome { type x.type <: Serializable with Cloneable { def f: Int } with Singleton {} }]
         |  Class => java/lang/Class#
         |  Option => scala/Option#
         |  x.type => local15
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |  f => local14
         |  Int => scala/Int#
         |  Singleton => scala/Singleton#
         |local0 => val local x: Serializable with Cloneable {}
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |local2 => val local x: AnyRef with Serializable {}
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/package.Serializable#
         |local4 => val local x: AnyRef with Serializable with Cloneable {}
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |local6 => val local x: AnyRef { def f: Int }
         |  AnyRef => scala/AnyRef#
         |  f => local7
         |  Int => scala/Int#
         |local7 => abstract method f: Int
         |  Int => scala/Int#
         |local9 => val local x: AnyRef { val def v: Int; type U <: Int }
         |  AnyRef => scala/AnyRef#
         |  v => local10
         |  Int => scala/Int#
         |  U => local11
         |local10 => abstract val method v: Int
         |  Int => scala/Int#
         |local11 => abstract type U <: Int
         |  Int => scala/Int#
         |local13 => val local x: Serializable with Cloneable { def f: Int }
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |  f => local14
         |  Int => scala/Int#
         |local14 => abstract method f: Int
         |  Int => scala/Int#
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
    if (ScalaVersion.is212)
      """|[0:7..0:10): e40 <= _empty_/e40.
         |[1:6..1:7): a <= _empty_/e40.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:24): Option => scala/Option#
         |[1:25..1:26): x => local0
         |[1:47..1:48): x <= local0
         |[1:50..1:62): Serializable => scala/Serializable#
         |[1:68..1:77): Cloneable => scala/Cloneable#
         |[2:6..2:7): b <= _empty_/e40.b.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:24): Option => scala/Option#
         |[2:25..2:26): x => local2
         |[2:47..2:48): x <= local2
         |[2:50..2:56): AnyRef => scala/AnyRef#
         |[2:62..2:74): Serializable => scala/Serializable#
         |[3:6..3:7): c <= _empty_/e40.c.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:24): Option => scala/Option#
         |[3:25..3:26): x => local4
         |[3:47..3:48): x <= local4
         |[3:50..3:56): AnyRef => scala/AnyRef#
         |[3:62..3:74): Serializable => scala/Serializable#
         |[3:80..3:89): Cloneable => scala/Cloneable#
         |[4:6..4:7): d <= _empty_/e40.d.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:24): Option => scala/Option#
         |[4:25..4:26): x => local6
         |[4:47..4:48): x <= local6
         |[4:50..4:56): AnyRef => scala/AnyRef#
         |[4:63..4:64): f <= local7
         |[4:66..4:69): Int => scala/Int#
         |[5:6..5:7): e <= _empty_/e40.e.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:24): Option => scala/Option#
         |[5:25..5:26): x => local9
         |[5:47..5:48): x <= local9
         |[5:50..5:56): AnyRef => scala/AnyRef#
         |[5:63..5:64): v <= local10
         |[5:66..5:69): Int => scala/Int#
         |[5:76..5:77): U <= local11
         |[5:81..5:84): Int => scala/Int#
         |[6:6..6:7): f <= _empty_/e40.f.
         |[6:10..6:17): classOf => scala/Predef.classOf().
         |[6:18..6:24): Option => scala/Option#
         |[6:25..6:26): x => local13
         |[6:47..6:48): x <= local13
         |[6:50..6:62): Serializable => scala/Serializable#
         |[6:68..6:77): Cloneable => scala/Cloneable#
         |[6:84..6:85): f <= local14
         |[6:87..6:90): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:7..0:10): e40 <= _empty_/e40.
         |[1:6..1:7): a <= _empty_/e40.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:24): Option => scala/Option#
         |[1:25..1:26): x => local0
         |[1:47..1:48): x <= local0
         |[1:50..1:62): Serializable => scala/package.Serializable#
         |[1:68..1:77): Cloneable => scala/package.Cloneable#
         |[2:6..2:7): b <= _empty_/e40.b.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:24): Option => scala/Option#
         |[2:25..2:26): x => local2
         |[2:47..2:48): x <= local2
         |[2:50..2:56): AnyRef => scala/AnyRef#
         |[2:62..2:74): Serializable => scala/package.Serializable#
         |[3:6..3:7): c <= _empty_/e40.c.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:24): Option => scala/Option#
         |[3:25..3:26): x => local4
         |[3:47..3:48): x <= local4
         |[3:50..3:56): AnyRef => scala/AnyRef#
         |[3:62..3:74): Serializable => scala/package.Serializable#
         |[3:80..3:89): Cloneable => scala/package.Cloneable#
         |[4:6..4:7): d <= _empty_/e40.d.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:24): Option => scala/Option#
         |[4:25..4:26): x => local6
         |[4:47..4:48): x <= local6
         |[4:50..4:56): AnyRef => scala/AnyRef#
         |[4:63..4:64): f <= local7
         |[4:66..4:69): Int => scala/Int#
         |[5:6..5:7): e <= _empty_/e40.e.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:24): Option => scala/Option#
         |[5:25..5:26): x => local9
         |[5:47..5:48): x <= local9
         |[5:50..5:56): AnyRef => scala/AnyRef#
         |[5:63..5:64): v <= local10
         |[5:66..5:69): Int => scala/Int#
         |[5:76..5:77): U <= local11
         |[5:81..5:84): Int => scala/Int#
         |[6:6..6:7): f <= _empty_/e40.f.
         |[6:10..6:17): classOf => scala/Predef.classOf().
         |[6:18..6:24): Option => scala/Option#
         |[6:25..6:26): x => local13
         |[6:47..6:48): x <= local13
         |[6:50..6:62): Serializable => scala/package.Serializable#
         |[6:68..6:77): Cloneable => scala/package.Cloneable#
         |[6:84..6:85): f <= local14
         |[6:87..6:90): Int => scala/Int#
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
         |local0 => abstract type T <: Serializable with Cloneable {}
         |  Serializable => scala/Serializable#
         |  Cloneable => scala/Cloneable#
         |local1 => abstract type T >: Null <: AnyRef { def f: Int }
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |  f => local2
         |  Int => scala/Int#
         |local2 => abstract method f: Int
         |  Int => scala/Int#
         |local3 => abstract type T <: AnyRef with Serializable with Cloneable {}
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
         |local0 => abstract type T <: Serializable with Cloneable {}
         |  Serializable => scala/package.Serializable#
         |  Cloneable => scala/package.Cloneable#
         |local1 => abstract type T >: Null <: AnyRef { def f: Int }
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |  f => local2
         |  Int => scala/Int#
         |local2 => abstract method f: Int
         |  Int => scala/Int#
         |local3 => abstract type T <: AnyRef with Serializable with Cloneable {}
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
    if (ScalaVersion.is212)
      """|[0:7..0:10): e41 <= _empty_/e41.
         |[1:6..1:7): a <= _empty_/e41.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:24): Option => scala/Option#
         |[1:25..1:26): T => local0
         |[1:43..1:44): T <= local0
         |[1:48..1:60): Serializable => scala/Serializable#
         |[1:66..1:75): Cloneable => scala/Cloneable#
         |[2:6..2:7): b <= _empty_/e41.b.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:24): Option => scala/Option#
         |[2:25..2:26): T => local1
         |[2:43..2:44): T <= local1
         |[2:48..2:52): Null => scala/Null#
         |[2:56..2:62): AnyRef => scala/AnyRef#
         |[2:69..2:70): f <= local2
         |[2:72..2:75): Int => scala/Int#
         |[3:6..3:7): c <= _empty_/e41.c.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:24): Option => scala/Option#
         |[3:25..3:26): T => local3
         |[3:43..3:44): T <= local3
         |[3:48..3:54): AnyRef => scala/AnyRef#
         |[3:60..3:72): Serializable => scala/Serializable#
         |[3:78..3:87): Cloneable => scala/Cloneable#
         |""".stripMargin
    else
      """|[0:7..0:10): e41 <= _empty_/e41.
         |[1:6..1:7): a <= _empty_/e41.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:24): Option => scala/Option#
         |[1:25..1:26): T => local0
         |[1:43..1:44): T <= local0
         |[1:48..1:60): Serializable => scala/package.Serializable#
         |[1:66..1:75): Cloneable => scala/package.Cloneable#
         |[2:6..2:7): b <= _empty_/e41.b.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:24): Option => scala/Option#
         |[2:25..2:26): T => local1
         |[2:43..2:44): T <= local1
         |[2:48..2:52): Null => scala/Null#
         |[2:56..2:62): AnyRef => scala/AnyRef#
         |[2:69..2:70): f <= local2
         |[2:72..2:75): Int => scala/Int#
         |[3:6..3:7): c <= _empty_/e41.c.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:24): Option => scala/Option#
         |[3:25..3:26): T => local3
         |[3:43..3:44): T <= local3
         |[3:48..3:54): AnyRef => scala/AnyRef#
         |[3:60..3:72): Serializable => scala/package.Serializable#
         |[3:78..3:87): Cloneable => scala/package.Cloneable#
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:26): x => local0
       |[1:47..1:48): x <= local0
       |[1:50..1:56): AnyRef => scala/AnyRef#
       |[1:58..1:67): unchecked => scala/unchecked#
       |[2:6..2:7): b <= _empty_/e42.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:26): T => local2
       |[2:43..2:44): T <= local2
       |[2:48..2:54): AnyRef => scala/AnyRef#
       |[2:56..2:65): unchecked => scala/unchecked#
       |[3:6..3:7): c <= _empty_/e42.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:24): Option => scala/Option#
       |[3:25..3:26): x => local3
       |[3:47..3:48): x <= local3
       |[3:50..3:56): AnyRef => scala/AnyRef#
       |[3:58..3:67): unchecked => scala/unchecked#
       |[3:69..3:79): deprecated => scala/deprecated#
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
       |[2:44..2:50): AnyRef => scala/AnyRef#
       |[3:6..3:7): b <= _empty_/e44.b.
       |[3:17..3:20): Any => scala/Any#
       |[3:22..3:34): isInstanceOf => scala/Any#isInstanceOf().
       |[3:35..3:41): Option => scala/Option#
       |[3:42..3:43): y => local1
       |[3:64..3:65): y <= local1
       |[3:67..3:73): AnyRef => scala/AnyRef#
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
       |[1:59..1:65): AnyRef => scala/AnyRef#
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
       |[2:15..2:21): Option => scala/Option#
       |[2:22..2:23): x => local1
       |[2:44..2:45): x <= local1
       |[2:47..2:53): AnyRef => scala/AnyRef#
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
       |[1:18..1:24): Option => scala/Option#
       |[1:26..1:27): x => local0
       |[1:34..1:35): y => local1
       |[1:57..1:58): x <= local0
       |[1:60..1:61): y <= local1
       |[1:63..1:69): AnyRef => scala/AnyRef#
       |[2:7..2:8): B <= _empty_/e47.B#
       |[2:11..2:17): Option => scala/Option#
       |[2:19..2:20): x => local6
       |[2:27..2:28): y => local7
       |[2:50..2:51): x <= local6
       |[2:53..2:54): y <= local7
       |[2:56..2:62): AnyRef => scala/AnyRef#
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
       |[1:29..1:35): Option => scala/Option#
       |[1:36..1:37): x => local1
       |[1:58..1:59): x <= local1
       |[1:61..1:62): v => _empty_/e48.m().(v)
       |[2:6..2:7): n <= _empty_/e48.n().
       |[2:8..2:9): v <= _empty_/e48.n().(v)
       |[2:11..2:17): AnyRef => scala/AnyRef#
       |[2:28..2:29): A <= local2
       |[2:32..2:38): Option => scala/Option#
       |[2:39..2:40): x => local4
       |[2:61..2:62): x <= local4
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:26): x => local0
       |[1:47..1:48): x <= local0
       |[1:50..1:56): AnyRef => scala/AnyRef#
       |[1:63..1:64): A <= local1
       |[1:68..1:69): x => local0
       |""".stripMargin,
  )

  test("existential value in a self type with two traits: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|trait e50 { self: (Option[x.type] forSome { val x: AnyRef }) with Serializable with Cloneable => }
         |""".stripMargin,
      "Occurrences",
    )
    assertNoDiff(
      load(),
      if (ScalaVersion.is212)
        """|[0:6..0:9): e50 <= _empty_/e50#
           |[0:12..0:16): self <= local2
           |[0:19..0:25): Option => scala/Option#
           |[0:26..0:27): x => local1
           |[0:48..0:49): x <= local1
           |[0:51..0:57): AnyRef => scala/AnyRef#
           |[0:66..0:78): Serializable => scala/Serializable#
           |[0:84..0:93): Cloneable => scala/Cloneable#
           |""".stripMargin
      else
        """|[0:6..0:9): e50 <= _empty_/e50#
           |[0:12..0:16): self <= local2
           |[0:19..0:25): Option => scala/Option#
           |[0:26..0:27): x => local1
           |[0:48..0:49): x <= local1
           |[0:51..0:57): AnyRef => scala/AnyRef#
           |[0:66..0:78): Serializable => scala/package.Serializable#
           |[0:84..0:93): Cloneable => scala/package.Cloneable#
           |""".stripMargin,
    )
  }

  test("self reference in a local class with an existential self type: occurrences") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object e51 {
         |  def g = { class L { self: (Option[x.type] forSome { val x: AnyRef }) => def h = self }; 1 }
         |}
         |""".stripMargin,
      "Occurrences",
    )
    assertNoDiff(
      load(),
      if (ScalaVersion.is212)
        """|[0:7..0:10): e51 <= _empty_/e51.
           |[1:6..1:7): g <= _empty_/e51.g().
           |[1:18..1:19): L <= local0
           |[1:20..1:20):  <= local3
           |[1:22..1:26): self <= local5
           |[1:29..1:35): Option => scala/Option#
           |[1:36..1:37): x => local1
           |[1:58..1:59): x <= local1
           |[1:61..1:67): AnyRef => scala/AnyRef#
           |[1:78..1:79): h <= local4
           |[1:82..1:86): self => local5
           |""".stripMargin
      else
        """|[0:7..0:10): e51 <= _empty_/e51.
           |[1:6..1:7): g <= _empty_/e51.g().
           |[1:18..1:19): L <= local0
           |[1:20..1:20):  <= local2
           |[1:22..1:26): self <= local5
           |[1:29..1:35): Option => scala/Option#
           |[1:36..1:37): x => local4
           |[1:58..1:59): x <= local4
           |[1:61..1:67): AnyRef => scala/AnyRef#
           |[1:78..1:79): h <= local3
           |[1:82..1:86): self => local5
           |""".stripMargin,
    )
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
       |[1:18..1:21): Map => scala/Predef.Map#
       |[1:22..1:23): x => local0
       |[1:30..1:31): _ <= local1
       |[1:47..1:48): x <= local0
       |[1:50..1:56): AnyRef => scala/AnyRef#
       |[2:6..2:7): b <= _empty_/e52.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:19..2:25): Option => scala/Option#
       |[2:26..2:27): x => local3
       |[2:48..2:49): x <= local3
       |[2:51..2:57): AnyRef => scala/AnyRef#
       |[2:64..2:67): Int => scala/Int#
       |[3:6..3:7): c <= _empty_/e52.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:19..3:25): Option => scala/Option#
       |[3:26..3:27): x => local5
       |[3:48..3:49): x <= local5
       |[3:51..3:57): AnyRef => scala/AnyRef#
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
       |[1:18..1:21): Int => scala/Int#
       |[1:22..1:28): Either => scala/package.Either#
       |[1:29..1:35): String => scala/Predef.String#
       |[2:6..2:7): b <= _empty_/e53.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:26): x => local0
       |[2:47..2:48): x <= local0
       |[2:50..2:53): Int => scala/Int#
       |[2:54..2:60): Either => scala/package.Either#
       |[2:61..2:67): String => scala/Predef.String#
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
       |  x.type => local8
       |  AnyRef => scala/AnyRef#
       |  f => local9
       |  a => local10
       |  Int => scala/Int#
       |  g => local11
       |  A => local13
       |  b => local12
       |  c => local14
       |  Singleton => scala/Singleton#
       |_empty_/e54.a. => val method a: Class[Option[x.type] forSome { type x.type <: AnyRef { def f(a: Int): Int; def g[A](b: A)(c: Int): A } with Singleton {} }]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  x.type => local7
       |  AnyRef => scala/AnyRef#
       |  f => local1
       |  a => local2
       |  Int => scala/Int#
       |  g => local3
       |  A => local5
       |  b => local4
       |  c => local6
       |  Singleton => scala/Singleton#
       |local0 => val local x: AnyRef { def f(a: Int): Int; def g[A](b: A)(c: Int): A }
       |  AnyRef => scala/AnyRef#
       |  f => local1
       |  a => local2
       |  Int => scala/Int#
       |  g => local3
       |  A => local5
       |  b => local4
       |  c => local6
       |local1 => abstract method f(a: Int): Int
       |  a => local2
       |  Int => scala/Int#
       |local2 => param a: Int
       |  Int => scala/Int#
       |local3 => abstract method g[A](b: A)(c: Int): A
       |  A => local5
       |  b => local4
       |  c => local6
       |  Int => scala/Int#
       |local4 => param b: A
       |  A => local5
       |local5 => typeparam A
       |local6 => param c: Int
       |  Int => scala/Int#
       |local9 => abstract method f(a: Int): Int
       |  a => local10
       |  Int => scala/Int#
       |local10 => param a: Int
       |  Int => scala/Int#
       |local11 => abstract method g[A](b: A)(c: Int): A
       |  A => local13
       |  b => local12
       |  c => local14
       |  Int => scala/Int#
       |local12 => param b: A
       |  A => local13
       |local13 => typeparam A
       |local14 => param c: Int
       |  Int => scala/Int#
       |local15 => val local x: AnyRef { def f(a: Int): Int; def g[A](b: A)(c: Int): A }
       |  AnyRef => scala/AnyRef#
       |  f => local9
       |  a => local10
       |  Int => scala/Int#
       |  g => local11
       |  A => local13
       |  b => local12
       |  c => local14
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:26): x => local0
       |[1:47..1:48): x <= local0
       |[1:50..1:56): AnyRef => scala/AnyRef#
       |[1:63..1:64): f <= local1
       |[1:65..1:66): a <= local2
       |[1:68..1:71): Int => scala/Int#
       |[1:74..1:77): Int => scala/Int#
       |[1:83..1:84): g <= local3
       |[1:85..1:86): A <= local5
       |[1:88..1:89): b <= local4
       |[1:91..1:92): A => local5
       |[1:94..1:95): c <= local6
       |[1:97..1:100): Int => scala/Int#
       |[1:103..1:104): A => local5
       |[2:7..2:8): B <= _empty_/e54.B#
       |[2:11..2:17): Option => scala/Option#
       |[2:18..2:19): x => local15
       |[2:40..2:41): x <= local15
       |[2:43..2:49): AnyRef => scala/AnyRef#
       |[2:56..2:57): f <= local9
       |[2:58..2:59): a <= local10
       |[2:61..2:64): Int => scala/Int#
       |[2:67..2:70): Int => scala/Int#
       |[2:76..2:77): g <= local11
       |[2:78..2:79): A <= local13
       |[2:81..2:82): b <= local12
       |[2:84..2:85): A => local13
       |[2:87..2:88): c <= local14
       |[2:90..2:93): Int => scala/Int#
       |[2:96..2:97): A => local13
       |""".stripMargin,
  )

  occurrences(
    "parenthesized compound types of existential values in classOf",
    """|object e55 {
       |  val a = classOf[Option[x.type] forSome { val x: (AnyRef with Serializable) with Cloneable }]
       |  val b = classOf[Option[x.type] forSome { val x: AnyRef with (Serializable with Cloneable) }]
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:10): e55 <= _empty_/e55.
         |[1:6..1:7): a <= _empty_/e55.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:24): Option => scala/Option#
         |[1:25..1:26): x => local0
         |[1:47..1:48): x <= local0
         |[1:51..1:57): AnyRef => scala/AnyRef#
         |[1:63..1:75): Serializable => scala/Serializable#
         |[1:82..1:91): Cloneable => scala/Cloneable#
         |[2:6..2:7): b <= _empty_/e55.b.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:24): Option => scala/Option#
         |[2:25..2:26): x => local2
         |[2:47..2:48): x <= local2
         |[2:50..2:56): AnyRef => scala/AnyRef#
         |[2:63..2:75): Serializable => scala/Serializable#
         |[2:81..2:90): Cloneable => scala/Cloneable#
         |""".stripMargin
    else
      """|[0:7..0:10): e55 <= _empty_/e55.
         |[1:6..1:7): a <= _empty_/e55.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:24): Option => scala/Option#
         |[1:25..1:26): x => local0
         |[1:47..1:48): x <= local0
         |[1:51..1:57): AnyRef => scala/AnyRef#
         |[1:63..1:75): Serializable => scala/package.Serializable#
         |[1:82..1:91): Cloneable => scala/package.Cloneable#
         |[2:6..2:7): b <= _empty_/e55.b.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:24): Option => scala/Option#
         |[2:25..2:26): x => local2
         |[2:47..2:48): x <= local2
         |[2:50..2:56): AnyRef => scala/AnyRef#
         |[2:63..2:75): Serializable => scala/package.Serializable#
         |[2:81..2:90): Cloneable => scala/package.Cloneable#
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:26): x => local0
       |[1:47..1:48): x <= local0
       |[1:50..1:56): AnyRef => scala/AnyRef#
       |[1:63..1:64): f <= local1
       |[1:65..1:66): A <= local3
       |[1:68..1:76): Ordering => scala/package.Ordering#
       |[1:78..1:79): b <= local2
       |[1:81..1:82): A => local3
       |[1:93..1:94): c <= local5
       |[1:96..1:99): Int => scala/Int#
       |[1:102..1:103): A => local3
       |[1:109..1:110): g <= local6
       |[1:111..1:112): B <= local8
       |[1:116..1:119): Int => scala/Int#
       |[1:121..1:122): d <= local7
       |[1:124..1:125): B => local8
       |[1:128..1:129): B => local8
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:26): x => local0
       |[1:47..1:48): x <= local0
       |[1:50..1:56): AnyRef => scala/AnyRef#
       |[1:63..1:64): f <= local1
       |[1:65..1:66): A <= local3
       |[1:68..1:76): Ordering => scala/package.Ordering#
       |[1:78..1:79): b <= local2
       |[1:81..1:82): A => local3
       |[1:85..1:86): A => local3
       |[1:92..1:93): g <= local5
       |[1:94..1:95): B <= local7
       |[1:97..1:105): Ordering => scala/package.Ordering#
       |[1:108..1:109): B => local7
       |[1:115..1:116): h <= local8
       |[1:117..1:118): C <= local10
       |[1:122..1:125): Int => scala/Int#
       |[1:127..1:135): Ordering => scala/package.Ordering#
       |[1:137..1:138): c <= local9
       |[1:140..1:141): C => local10
       |[1:143..1:144): d <= local11
       |[1:146..1:149): Int => scala/Int#
       |[1:152..1:153): C => local10
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:26): x => local0
       |[1:47..1:48): x <= local0
       |[1:50..1:56): AnyRef => scala/AnyRef#
       |[1:63..1:64): f <= local1
       |[1:65..1:66): a <= local2
       |[1:68..1:71): Int => scala/Int#
       |[1:74..1:77): Int => scala/Int#
       |[1:83..1:84): f <= local3
       |[1:85..1:86): b <= local4
       |[1:88..1:94): String => scala/Predef.String#
       |[1:97..1:103): String => scala/Predef.String#
       |[2:6..2:7): c <= _empty_/p1.c.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:26): x => local6
       |[2:47..2:48): x <= local6
       |[2:50..2:56): AnyRef => scala/AnyRef#
       |[2:63..2:64): g <= local7
       |[2:65..2:66): a <= local8
       |[2:68..2:71): Int => scala/Int#
       |[2:73..2:74): b <= local9
       |[2:76..2:79): Int => scala/Int#
       |[2:82..2:85): Int => scala/Int#
       |[2:91..2:92): g <= local10
       |[2:93..2:94): c <= local11
       |[2:96..2:100): Long => scala/Long#
       |[2:103..2:107): Long => scala/Long#
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:26): x => local0
       |[1:47..1:48): x <= local0
       |[1:50..1:56): AnyRef => scala/AnyRef#
       |[1:63..1:64): v <= local1
       |[1:66..1:69): Int => scala/Int#
       |[1:76..1:77): T <= local4
       |[1:80..1:86): String => scala/Predef.String#
       |[1:92..1:93): g <= local5
       |[1:94..1:95): a <= local6
       |[1:100..1:104): Long => scala/Long#
       |[1:106..1:107): b <= local7
       |[1:109..1:115): Double => scala/Double#
       |[1:119..1:123): Unit => scala/Unit#
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
    assertNoDiff(
      load(),
      """|[0:6..0:8): p3 <= _empty_/p3#
         |[0:9..0:9):  <= _empty_/p3#`<init>`().
         |[1:2..1:6): self <= local6
         |[1:9..1:15): Option => scala/Option#
         |[1:16..1:17): x => local5
         |[1:38..1:39): x <= local5
         |[1:41..1:47): AnyRef => scala/AnyRef#
         |[1:54..1:55): f <= local1
         |[1:56..1:57): a <= local2
         |[1:59..1:62): Int => scala/Int#
         |[1:65..1:68): Int => scala/Int#
         |[1:74..1:75): f <= local3
         |[1:76..1:77): b <= local4
         |[1:79..1:85): String => scala/Predef.String#
         |[1:88..1:94): String => scala/Predef.String#
         |""".stripMargin,
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
       |local3 => val local xt: AnyRef
       |  AnyRef => scala/AnyRef#
       |local4 => val local xp: AnyRef
       |  AnyRef => scala/AnyRef#
       |local5 => val local xq: AnyRef
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
       |[0:48..0:54): AnyRef => scala/AnyRef#
       |[0:57..0:57):  <= _empty_/e9#`<init>`().
       |[1:4..1:5): p <= _empty_/e9#`<init>`().(p)
       |[1:4..1:5): p <= _empty_/e9#p.
       |[1:7..1:13): Option => scala/Option#
       |[1:14..1:16): xp => local4
       |[1:37..1:39): xp <= local4
       |[1:41..1:47): AnyRef => scala/AnyRef#
       |[2:8..2:9): q <= _empty_/e9#`<init>`().(q)
       |[2:8..2:9): q <= _empty_/e9#q.
       |[2:11..2:17): Option => scala/Option#
       |[2:18..2:20): xq => local5
       |[2:41..2:43): xq <= local5
       |[2:45..2:51): AnyRef => scala/AnyRef#
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
       |[1:18..1:24): Option => scala/Option#
       |[1:25..1:26): x => local0
       |[1:47..1:48): x <= local0
       |[1:50..1:56): AnyRef => scala/AnyRef#
       |[1:63..1:64): f <= local1
       |[1:66..1:69): Int => scala/Int#
       |[1:75..1:76): f <= local2
       |[1:77..1:78): a <= local3
       |[1:80..1:83): Int => scala/Int#
       |[1:86..1:89): Int => scala/Int#
       |[2:6..2:7): b <= _empty_/r1.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:26): x => local5
       |[2:47..2:48): x <= local5
       |[2:50..2:56): AnyRef => scala/AnyRef#
       |[2:63..2:64): f <= local6
       |[2:65..2:66): a <= local7
       |[2:68..2:71): Int => scala/Int#
       |[2:74..2:77): Int => scala/Int#
       |[2:83..2:84): f <= local8
       |[2:86..2:89): Int => scala/Int#
       |[3:6..3:7): c <= _empty_/r1.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:24): Option => scala/Option#
       |[3:25..3:26): x => local10
       |[3:47..3:48): x <= local10
       |[3:50..3:56): AnyRef => scala/AnyRef#
       |[3:63..3:64): f <= local11
       |[3:66..3:69): Int => scala/Int#
       |[3:75..3:76): f <= local14
       |[3:77..3:78): a <= local15
       |[3:80..3:83): Int => scala/Int#
       |[3:86..3:89): Int => scala/Int#
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
       |[1:18..1:21): Map => scala/Predef.Map#
       |[1:22..1:23): _ <= local1
       |[1:25..1:26): x => local0
       |[1:47..1:48): x <= local0
       |[1:50..1:56): AnyRef => scala/AnyRef#
       |[2:6..2:7): b <= _empty_/r2.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:26): x => local3
       |[2:27..2:28): y => local4
       |[2:49..2:50): x <= local3
       |[2:52..2:58): AnyRef => scala/AnyRef#
       |[2:65..2:66): y <= local4
       |[2:68..2:74): AnyRef => scala/AnyRef#
       |[3:6..3:7): c <= _empty_/r2.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:24): Option => scala/Option#
       |[3:25..3:26): x => local6
       |[3:47..3:48): x <= local6
       |[3:50..3:56): AnyRef => scala/AnyRef#
       |[3:64..3:65): F <= local7
       |[3:66..3:67): A <= local8
       |[3:71..3:75): List => scala/package.List#
       |[3:76..3:77): A => local8
       |[3:84..3:85): g <= local9
       |[3:87..3:93): AnyRef => scala/AnyRef#
       |[3:100..3:101): h <= local10
       |[3:103..3:106): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "val and def of one name in a refinement in a self type",
    """|class r3 {
       |  self: (Option[x.type] forSome { val x: AnyRef { def f(a: Int): Int; val f: Int } }) =>
       |}
       |""".stripMargin,
    """|[0:6..0:8): r3 <= _empty_/r3#
       |[0:9..0:9):  <= _empty_/r3#`<init>`().
       |[1:2..1:6): self <= local5
       |[1:9..1:15): Option => scala/Option#
       |[1:16..1:17): x => local4
       |[1:38..1:39): x <= local4
       |[1:41..1:47): AnyRef => scala/AnyRef#
       |[1:54..1:55): f <= local1
       |[1:56..1:57): a <= local2
       |[1:59..1:62): Int => scala/Int#
       |[1:65..1:68): Int => scala/Int#
       |[1:74..1:75): f <= local3
       |[1:77..1:80): Int => scala/Int#
       |""".stripMargin,
  )

}

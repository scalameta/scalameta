package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1808
class Issue1808Suite extends SemanticdbSuite {

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  occurrences(
    "self in a type",
    "class D { self => type T = Int; def m: self.T = ??? }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:23..0:24): T <= _empty_/D#T#
       |[0:27..0:30): Int => scala/Int#
       |[0:36..0:37): m <= _empty_/D#m().
       |[0:39..0:43): self => local0
       |[0:44..0:45): T => _empty_/D#T#
       |[0:48..0:51): ??? => scala/Predef.`???`().
       |""".stripMargin,
  )

  symbols(
    "self in a type",
    "class D { self => type T = Int; def m: self.T = ??? }",
    """|_empty_/D# => class D extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/D#T# => type T = Int
       |  Int => scala/Int#
       |_empty_/D#`<init>`(). => primary ctor <init>()
       |_empty_/D#m(). => method m: T
       |  T => _empty_/D#T#
       |local0 => selfparam self
       |""".stripMargin,
  )

  occurrences(
    "self in a term",
    "class D { self => val x = 1; def m = self.x }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): x <= _empty_/D#x.
       |[0:33..0:34): m <= _empty_/D#m().
       |[0:37..0:41): self => local0
       |[0:42..0:43): x => _empty_/D#x.
       |""".stripMargin,
  )

  occurrences(
    "self as an argument",
    "class D { self => def m = println(self) }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): m <= _empty_/D#m().
       |[0:26..0:33): println => scala/Predef.println(+1).
       |[0:34..0:38): self => local0
       |""".stripMargin,
  )

  occurrences(
    "self as a result",
    "class D { self => def m: D = self }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): m <= _empty_/D#m().
       |[0:25..0:26): D => _empty_/D#
       |[0:29..0:33): self => local0
       |""".stripMargin,
  )

  occurrences(
    "self in a singleton type",
    "class D { self => def m: self.type = self }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): m <= _empty_/D#m().
       |[0:25..0:29): self => local0
       |[0:37..0:41): self => local0
       |""".stripMargin,
  )

  symbols(
    "self in a singleton type",
    "class D { self => def m: self.type = self }",
    """|_empty_/D# => class D extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/D#`<init>`(). => primary ctor <init>()
       |_empty_/D#m(). => method m: D.this.type
       |  D => _empty_/D#
       |local0 => selfparam self
       |""".stripMargin,
  )

  occurrences(
    "self with a type",
    "trait A { def a = 1 }; class D { self: A => def m = self.a }",
    """|[0:6..0:7): A <= _empty_/A#
       |[0:8..0:8):  <= _empty_/A#`<init>`().
       |[0:14..0:15): a <= _empty_/A#a().
       |[0:29..0:30): D <= _empty_/D#
       |[0:31..0:31):  <= _empty_/D#`<init>`().
       |[0:33..0:37): self <= local0
       |[0:39..0:40): A => _empty_/A#
       |[0:48..0:49): m <= _empty_/D#m().
       |[0:52..0:56): self => local0
       |[0:57..0:58): a => _empty_/A#a().
       |""".stripMargin,
  )

  occurrences(
    "self with a type, alone",
    "trait A; class D { self: A => def m: A = self }",
    """|[0:6..0:7): A <= _empty_/A#
       |[0:15..0:16): D <= _empty_/D#
       |[0:17..0:17):  <= _empty_/D#`<init>`().
       |[0:19..0:23): self <= local0
       |[0:25..0:26): A => _empty_/A#
       |[0:34..0:35): m <= _empty_/D#m().
       |[0:37..0:38): A => _empty_/A#
       |[0:41..0:45): self => local0
       |""".stripMargin,
  )

  occurrences(
    "self with a type, member type",
    "trait A { type T }; class D { self: A => def m: self.T = ??? }",
    """|[0:6..0:7): A <= _empty_/A#
       |[0:15..0:16): T <= _empty_/A#T#
       |[0:26..0:27): D <= _empty_/D#
       |[0:28..0:28):  <= _empty_/D#`<init>`().
       |[0:30..0:34): self <= local0
       |[0:36..0:37): A => _empty_/A#
       |[0:45..0:46): m <= _empty_/D#m().
       |[0:48..0:52): self => local0
       |[0:53..0:54): T => _empty_/A#T#
       |[0:57..0:60): ??? => scala/Predef.`???`().
       |""".stripMargin,
  )

  symbols(
    "self with a type, member type",
    "trait A { type T }; class D { self: A => def m: self.T = ??? }",
    """|_empty_/A# => trait A extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/A#T# => abstract type T
       |_empty_/D# => class D extends AnyRef { self: A => +2 decls }
       |  AnyRef => scala/AnyRef#
       |  A => _empty_/A#
       |_empty_/D#`<init>`(). => primary ctor <init>()
       |_empty_/D#m(). => method m: D.this.T
       |  D => _empty_/D#
       |  T => _empty_/A#T#
       |local0 => selfparam self: A
       |  A => _empty_/A#
       |""".stripMargin,
  )

  occurrences(
    "self in a trait",
    "trait D { self => val x = 1; def m = self.x }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): x <= _empty_/D#x.
       |[0:33..0:34): m <= _empty_/D#m().
       |[0:37..0:41): self => local0
       |[0:42..0:43): x => _empty_/D#x.
       |""".stripMargin,
  )

  occurrences(
    "self in an object",
    "object D { self => val x = 1; def m = self.x }",
    """|[0:7..0:8): D <= _empty_/D.
       |[0:11..0:15): self <= local0
       |[0:23..0:24): x <= _empty_/D.x.
       |[0:34..0:35): m <= _empty_/D.m().
       |[0:38..0:42): self => local0
       |[0:43..0:44): x => _empty_/D.x.
       |""".stripMargin,
  )

  occurrences(
    "self in an anonymous class",
    "object O { val a = new AnyRef { self => def m = self } }",
    """|[0:7..0:8): O <= _empty_/O.
       |[0:15..0:16): a <= _empty_/O.a.
       |[0:23..0:23):  <= local0
       |[0:23..0:29): AnyRef => scala/AnyRef#
       |[0:29..0:29):  => java/lang/Object#`<init>`().
       |[0:32..0:36): self <= local2
       |[0:44..0:45): m <= local1
       |[0:48..0:52): self => local2
       |""".stripMargin,
  )

  occurrences(
    "self in a local class",
    "object O { def f = { class L { self => def m = self }; 1 } }",
    """|[0:7..0:8): O <= _empty_/O.
       |[0:15..0:16): f <= _empty_/O.f().
       |[0:27..0:28): L <= local0
       |[0:29..0:29):  <= local1
       |[0:31..0:35): self <= local3
       |[0:43..0:44): m <= local2
       |[0:47..0:51): self => local3
       |""".stripMargin,
  )

  occurrences(
    "outer self in a nested class",
    "class D { outer => val x = 1; class E { def m = outer.x } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:15): outer <= local0
       |[0:23..0:24): x <= _empty_/D#x.
       |[0:36..0:37): E <= _empty_/D#E#
       |[0:38..0:38):  <= _empty_/D#E#`<init>`().
       |[0:44..0:45): m <= _empty_/D#E#m().
       |[0:48..0:53): outer => local0
       |[0:54..0:55): x => _empty_/D#x.
       |""".stripMargin,
  )

  occurrences(
    "outer self in a nested class, type",
    "class D { outer => type T = Int; class E { def m: outer.T = ??? } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:15): outer <= local0
       |[0:24..0:25): T <= _empty_/D#T#
       |[0:28..0:31): Int => scala/Int#
       |[0:39..0:40): E <= _empty_/D#E#
       |[0:41..0:41):  <= _empty_/D#E#`<init>`().
       |[0:47..0:48): m <= _empty_/D#E#m().
       |[0:50..0:55): outer => local0
       |[0:56..0:57): T => _empty_/D#T#
       |[0:60..0:63): ??? => scala/Predef.`???`().
       |""".stripMargin,
  )

  symbols(
    "outer self in a nested class, type",
    "class D { outer => type T = Int; class E { def m: outer.T = ??? } }",
    """|_empty_/D# => class D extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/D#E# => class E extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/D#E#`<init>`(). => primary ctor <init>()
       |_empty_/D#E#m(). => method m: T
       |  T => _empty_/D#T#
       |_empty_/D#T# => type T = Int
       |  Int => scala/Int#
       |_empty_/D#`<init>`(). => primary ctor <init>()
       |local0 => selfparam outer
       |""".stripMargin,
  )

  occurrences(
    "two selfs",
    "class D { outer => class E { inner => def m = (outer, inner) } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:15): outer <= local0
       |[0:25..0:26): E <= _empty_/D#E#
       |[0:27..0:27):  <= _empty_/D#E#`<init>`().
       |[0:29..0:34): inner <= local1
       |[0:42..0:43): m <= _empty_/D#E#m().
       |[0:47..0:52): outer => local0
       |[0:54..0:59): inner => local1
       |""".stripMargin,
  )

  occurrences(
    "this and self",
    "class D { self => val x = 1; def a = this.x; def b = self.x; def c = D.this.x }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): x <= _empty_/D#x.
       |[0:33..0:34): a <= _empty_/D#a().
       |[0:42..0:43): x => _empty_/D#x.
       |[0:49..0:50): b <= _empty_/D#b().
       |[0:53..0:57): self => local0
       |[0:58..0:59): x => _empty_/D#x.
       |[0:65..0:66): c <= _empty_/D#c().
       |[0:69..0:70): D => _empty_/D#
       |[0:76..0:77): x => _empty_/D#x.
       |""".stripMargin,
  )

  occurrences(
    "self named as the class",
    "class D { D => val x = 1; def m = D.x; def n = D.this.x }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:11): D <= local0
       |[0:19..0:20): x <= _empty_/D#x.
       |[0:30..0:31): m <= _empty_/D#m().
       |[0:34..0:35): D => local0
       |[0:36..0:37): x => _empty_/D#x.
       |[0:43..0:44): n <= _empty_/D#n().
       |[0:47..0:48): D => _empty_/D#
       |[0:54..0:55): x => _empty_/D#x.
       |""".stripMargin,
  )

  occurrences(
    "self in an import",
    "class D { self => object o { val x = 1 }; def m = { import self.o.x; x } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:25..0:26): o <= _empty_/D#o.
       |[0:33..0:34): x <= _empty_/D#o.x.
       |[0:46..0:47): m <= _empty_/D#m().
       |[0:59..0:63): self => local0
       |[0:64..0:65): o => _empty_/D#o.
       |[0:66..0:67): x => _empty_/D#o.x.
       |[0:69..0:70): x => _empty_/D#o.x.
       |""".stripMargin,
  )

  occurrences(
    "self as a stable pattern",
    "class D { self => def m(a: Any) = a match { case `self` => 1 } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): m <= _empty_/D#m().
       |[0:24..0:25): a <= _empty_/D#m().(a)
       |[0:27..0:30): Any => scala/Any#
       |[0:34..0:35): a => _empty_/D#m().(a)
       |[0:49..0:55): `self` => local0
       |""".stripMargin,
  )

  occurrences(
    "anonymous self with a type",
    "trait A { def a = 1 }; class D { this: A => def m = this.a }",
    """|[0:6..0:7): A <= _empty_/A#
       |[0:8..0:8):  <= _empty_/A#`<init>`().
       |[0:14..0:15): a <= _empty_/A#a().
       |[0:29..0:30): D <= _empty_/D#
       |[0:31..0:31):  <= _empty_/D#`<init>`().
       |[0:39..0:40): A => _empty_/A#
       |[0:48..0:49): m <= _empty_/D#m().
       |[0:57..0:58): a => _empty_/A#a().
       |""".stripMargin,
  )

  occurrences(
    "parameter shadows self",
    "class D { self => def m(self: Int) = self }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): m <= _empty_/D#m().
       |[0:24..0:28): self <= _empty_/D#m().(self)
       |[0:30..0:33): Int => scala/Int#
       |[0:37..0:41): self => _empty_/D#m().(self)
       |""".stripMargin,
  )

  occurrences(
    "inherited member named as self",
    "trait A { def self = 1 }; class D extends A { self => def m = self }",
    """|[0:6..0:7): A <= _empty_/A#
       |[0:8..0:8):  <= _empty_/A#`<init>`().
       |[0:14..0:18): self <= _empty_/A#self().
       |[0:32..0:33): D <= _empty_/D#
       |[0:34..0:34):  <= _empty_/D#`<init>`().
       |[0:42..0:43): A => _empty_/A#
       |[0:43..0:43):  => java/lang/Object#`<init>`().
       |[0:46..0:50): self <= local0
       |[0:58..0:59): m <= _empty_/D#m().
       |[0:62..0:66): self => _empty_/A#self().
       |""".stripMargin,
  )

  occurrences(
    "member named as self",
    "class D { self => def self = 1; def m = self }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:26): self <= _empty_/D#self().
       |[0:36..0:37): m <= _empty_/D#m().
       |[0:40..0:44): self => _empty_/D#self().
       |""".stripMargin,
  )

  occurrences(
    "local shadows self",
    "class D { self => def m = { val self = 1; self } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): m <= _empty_/D#m().
       |[0:32..0:36): self <= local1
       |[0:42..0:46): self => local1
       |""".stripMargin,
  )

  occurrences(
    "inner member shadows outer self",
    "class D { self => class E { def self = 1; def m = self } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:24..0:25): E <= _empty_/D#E#
       |[0:26..0:26):  <= _empty_/D#E#`<init>`().
       |[0:32..0:36): self <= _empty_/D#E#self().
       |[0:46..0:47): m <= _empty_/D#E#m().
       |[0:50..0:54): self => _empty_/D#E#self().
       |""".stripMargin,
  )

  occurrences(
    "inner object named as outer self",
    "class D { s => class E { object s { val x = 1 }; def m = s.x } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:11): s <= local0
       |[0:21..0:22): E <= _empty_/D#E#
       |[0:23..0:23):  <= _empty_/D#E#`<init>`().
       |[0:32..0:33): s <= _empty_/D#E#s.
       |[0:40..0:41): x <= _empty_/D#E#s.x.
       |[0:53..0:54): m <= _empty_/D#E#m().
       |[0:57..0:58): s => _empty_/D#E#s.
       |[0:59..0:60): x => _empty_/D#E#s.x.
       |""".stripMargin,
  )

  occurrences(
    "self in interpolation",
    """class D { self => def m = s"$self" }""",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): m <= _empty_/D#m().
       |[0:26..0:27): s => scala/StringContext#s().
       |[0:29..0:33): self => local0
       |""".stripMargin,
  )

  occurrences(
    "typed self in anonymous class",
    "trait A { def a = 1 }; object O { val v = new A { self: A => def m = self.a } }",
    """|[0:6..0:7): A <= _empty_/A#
       |[0:8..0:8):  <= _empty_/A#`<init>`().
       |[0:14..0:15): a <= _empty_/A#a().
       |[0:30..0:31): O <= _empty_/O.
       |[0:38..0:39): v <= _empty_/O.v.
       |[0:46..0:46):  <= local0
       |[0:46..0:47): A => _empty_/A#
       |[0:47..0:47):  => java/lang/Object#`<init>`().
       |[0:50..0:54): self <= local2
       |[0:56..0:57): A => _empty_/A#
       |[0:65..0:66): m <= local1
       |[0:69..0:73): self => local2
       |[0:74..0:75): a => _empty_/A#a().
       |""".stripMargin,
  )

  occurrences(
    "this in anonymous class",
    "object O { val v = new AnyRef { def m = this } }",
    """|[0:7..0:8): O <= _empty_/O.
       |[0:15..0:16): v <= _empty_/O.v.
       |[0:23..0:23):  <= local0
       |[0:23..0:29): AnyRef => scala/AnyRef#
       |[0:29..0:29):  => java/lang/Object#`<init>`().
       |[0:36..0:37): m <= local1
       |""".stripMargin,
  )

  occurrences(
    "object self named as object",
    "object D { D => val x = 1; def m = D.x }",
    """|[0:7..0:8): D <= _empty_/D.
       |[0:11..0:12): D <= local0
       |[0:20..0:21): x <= _empty_/D.x.
       |[0:31..0:32): m <= _empty_/D.m().
       |[0:35..0:36): D => local0
       |[0:37..0:38): x => _empty_/D.x.
       |""".stripMargin,
  )

  occurrences(
    "implicit conversion of self",
    """object C { implicit def c(d: D): String = "" }; import C._; class D { self => def m = self.length }""",
    """|[0:7..0:8): C <= _empty_/C.
       |[0:24..0:25): c <= _empty_/C.c().
       |[0:26..0:27): d <= _empty_/C.c().(d)
       |[0:29..0:30): D => _empty_/D#
       |[0:33..0:39): String => scala/Predef.String#
       |[0:55..0:56): C => _empty_/C.
       |[0:66..0:67): D <= _empty_/D#
       |[0:68..0:68):  <= _empty_/D#`<init>`().
       |[0:70..0:74): self <= local0
       |[0:82..0:83): m <= _empty_/D#m().
       |[0:86..0:90): self => local0
       |[0:91..0:97): length => java/lang/String#length().
       |""".stripMargin,
  )

  occurrences(
    "case class self",
    "case class D(x: Int) { self => def m = self.copy() }",
    """|[0:11..0:12): D <= _empty_/D#
       |[0:12..0:12):  <= _empty_/D#`<init>`().
       |[0:13..0:14): x <= _empty_/D#`<init>`().(x)
       |[0:13..0:14): x <= _empty_/D#x.
       |[0:16..0:19): Int => scala/Int#
       |[0:23..0:27): self <= local0
       |[0:35..0:36): m <= _empty_/D#m().
       |[0:39..0:43): self => local0
       |[0:44..0:48): copy => _empty_/D#copy().
       |""".stripMargin,
  )

  occurrences(
    "self in nested anonymous class",
    "class D { self => val f = new AnyRef { def m = self } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): f <= _empty_/D#f.
       |[0:30..0:30):  <= local1
       |[0:30..0:36): AnyRef => scala/AnyRef#
       |[0:36..0:36):  => java/lang/Object#`<init>`().
       |[0:43..0:44): m <= local2
       |[0:47..0:51): self => local0
       |""".stripMargin,
  )

  occurrences(
    "self in default parameter",
    "class D { self => def m(d: D = self) = d }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): m <= _empty_/D#m().
       |[0:24..0:25): d <= _empty_/D#m().(d)
       |[0:27..0:28): D => _empty_/D#
       |[0:31..0:35): self => local0
       |[0:39..0:40): d => _empty_/D#m().(d)
       |""".stripMargin,
  )

  occurrences(
    "inherited member named as class self",
    "trait A { def D = 1 }; class D extends A { D => def m = D }",
    """|[0:6..0:7): A <= _empty_/A#
       |[0:8..0:8):  <= _empty_/A#`<init>`().
       |[0:14..0:15): D <= _empty_/A#D().
       |[0:29..0:30): D <= _empty_/D#
       |[0:31..0:31):  <= _empty_/D#`<init>`().
       |[0:39..0:40): A => _empty_/A#
       |[0:40..0:40):  => java/lang/Object#`<init>`().
       |[0:43..0:44): D <= local0
       |[0:52..0:53): m <= _empty_/D#m().
       |[0:56..0:57): D => _empty_/A#D().
       |""".stripMargin,
  )

  occurrences(
    "inner class self shadows outer self",
    "class D { s => val x = 1; class E { s => val y = 2; def m = s.y } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:11): s <= local0
       |[0:19..0:20): x <= _empty_/D#x.
       |[0:32..0:33): E <= _empty_/D#E#
       |[0:34..0:34):  <= _empty_/D#E#`<init>`().
       |[0:36..0:37): s <= local1
       |[0:45..0:46): y <= _empty_/D#E#y.
       |[0:56..0:57): m <= _empty_/D#E#m().
       |[0:60..0:61): s => local1
       |[0:62..0:63): y => _empty_/D#E#y.
       |""".stripMargin,
  )

  occurrences(
    "self type of anonymous class",
    "class D { self => def m = new D { inner => def n: (D, D) = (self, inner) } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:22..0:23): m <= _empty_/D#m().
       |[0:30..0:30):  <= local2
       |[0:30..0:31): D => _empty_/D#
       |[0:31..0:31):  => _empty_/D#`<init>`().
       |[0:34..0:39): inner <= local3
       |[0:47..0:48): n <= local1
       |[0:51..0:52): D => _empty_/D#
       |[0:54..0:55): D => _empty_/D#
       |[0:60..0:64): self => local0
       |[0:66..0:71): inner => local3
       |""".stripMargin,
  )

  occurrences(
    "member object named as self of nested",
    "class D { s => object s2; class E { s => def m = s } }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:11): s <= local0
       |[0:22..0:24): s2 <= _empty_/D#s2.
       |[0:32..0:33): E <= _empty_/D#E#
       |[0:34..0:34):  <= _empty_/D#E#`<init>`().
       |[0:36..0:37): s <= local1
       |[0:45..0:46): m <= _empty_/D#E#m().
       |[0:49..0:50): s => local1
       |""".stripMargin,
  )

  occurrences(
    "self in a type projection",
    "class D { self => class E; def m: self.E = ??? ; def n: D#E = m }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:14): self <= local0
       |[0:24..0:25): E <= _empty_/D#E#
       |[0:25..0:25):  <= _empty_/D#E#`<init>`().
       |[0:31..0:32): m <= _empty_/D#m().
       |[0:34..0:38): self => local0
       |[0:39..0:40): E => _empty_/D#E#
       |[0:43..0:46): ??? => scala/Predef.`???`().
       |[0:53..0:54): n <= _empty_/D#n().
       |[0:56..0:57): D => _empty_/D#
       |[0:58..0:59): E => _empty_/D#E#
       |[0:62..0:63): m => _empty_/D#m().
       |""".stripMargin,
  )

  occurrences(
    "generic self",
    "class D[T] { self: D[T] => def m = self }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:9): T <= _empty_/D#[T]
       |[0:11..0:11):  <= _empty_/D#`<init>`().
       |[0:13..0:17): self <= local0
       |[0:19..0:20): D => _empty_/D#
       |[0:21..0:22): T => _empty_/D#[T]
       |[0:31..0:32): m <= _empty_/D#m().
       |[0:35..0:39): self => local0
       |""".stripMargin,
  )

  occurrences(
    "self in a super call argument",
    "class B(x: Any); class D extends B(1) { self => val y = new B(self) {} }",
    """|[0:6..0:7): B <= _empty_/B#
       |[0:7..0:7):  <= _empty_/B#`<init>`().
       |[0:8..0:9): x <= _empty_/B#`<init>`().(x)
       |[0:8..0:9): x <= _empty_/B#x.
       |[0:11..0:14): Any => scala/Any#
       |[0:23..0:24): D <= _empty_/D#
       |[0:25..0:25):  <= _empty_/D#`<init>`().
       |[0:33..0:34): B => _empty_/B#
       |[0:34..0:34):  => _empty_/B#`<init>`().
       |[0:40..0:44): self <= local0
       |[0:52..0:53): y <= _empty_/D#y.
       |[0:60..0:60):  <= local1
       |[0:60..0:61): B => _empty_/B#
       |[0:61..0:61):  => _empty_/B#`<init>`().
       |[0:62..0:66): self => local0
       |""".stripMargin,
  )

  occurrences(
    "outer anonymous self",
    "object O { val v = new AnyRef { self => class E { def m = self } } }",
    """|[0:7..0:8): O <= _empty_/O.
       |[0:15..0:16): v <= _empty_/O.v.
       |[0:23..0:23):  <= local0
       |[0:23..0:29): AnyRef => scala/AnyRef#
       |[0:29..0:29):  => java/lang/Object#`<init>`().
       |[0:32..0:36): self <= local2
       |[0:46..0:47): E <= local1
       |[0:48..0:48):  <= local3
       |[0:54..0:55): m <= local4
       |[0:58..0:62): self => local2
       |""".stripMargin,
  )

  occurrences(
    "backquoted self",
    "class D { `my self` => def m = `my self` }",
    """|[0:6..0:7): D <= _empty_/D#
       |[0:8..0:8):  <= _empty_/D#`<init>`().
       |[0:10..0:19): `my self` <= local0
       |[0:27..0:28): m <= _empty_/D#m().
       |[0:31..0:40): `my self` => local0
       |""".stripMargin,
  )

  occurrences(
    "object self named as object, from outside",
    "object O { O => val x = 1; type T = Int }; object U { def m: O.T = O.x; import O._; def n = x }",
    """|[0:7..0:8): O <= _empty_/O.
       |[0:11..0:12): O <= local0
       |[0:20..0:21): x <= _empty_/O.x.
       |[0:32..0:33): T <= _empty_/O.T#
       |[0:36..0:39): Int => scala/Int#
       |[0:50..0:51): U <= _empty_/U.
       |[0:58..0:59): m <= _empty_/U.m().
       |[0:61..0:62): O => _empty_/O.
       |[0:63..0:64): T => _empty_/O.T#
       |[0:67..0:68): O => _empty_/O.
       |[0:69..0:70): x => _empty_/O.x.
       |[0:79..0:80): O => _empty_/O.
       |[0:88..0:89): n <= _empty_/U.n().
       |[0:92..0:93): x => _empty_/O.x.
       |""".stripMargin,
  )

  occurrences(
    "object self named as object, type from outside",
    "object O { O => type T = Int }; object U { def m: O.T = ??? }",
    """|[0:7..0:8): O <= _empty_/O.
       |[0:11..0:12): O <= local0
       |[0:21..0:22): T <= _empty_/O.T#
       |[0:25..0:28): Int => scala/Int#
       |[0:39..0:40): U <= _empty_/U.
       |[0:47..0:48): m <= _empty_/U.m().
       |[0:50..0:51): O => _empty_/O.
       |[0:52..0:53): T => _empty_/O.T#
       |[0:56..0:59): ??? => scala/Predef.`???`().
       |""".stripMargin,
  )

  occurrences(
    "nested object self named as object, from outside",
    "object P { object O { O => type T = Int; val x = 1 } }; object U { def m: P.O.T = P.O.x }",
    """|[0:7..0:8): P <= _empty_/P.
       |[0:18..0:19): O <= _empty_/P.O.
       |[0:22..0:23): O <= local0
       |[0:32..0:33): T <= _empty_/P.O.T#
       |[0:36..0:39): Int => scala/Int#
       |[0:45..0:46): x <= _empty_/P.O.x.
       |[0:63..0:64): U <= _empty_/U.
       |[0:71..0:72): m <= _empty_/U.m().
       |[0:74..0:75): P => _empty_/P.
       |[0:76..0:77): O => _empty_/P.O.
       |[0:78..0:79): T => _empty_/P.O.T#
       |[0:82..0:83): P => _empty_/P.
       |[0:84..0:85): O => _empty_/P.O.
       |[0:86..0:87): x => _empty_/P.O.x.
       |""".stripMargin,
  )

  occurrences(
    "super with self named as class",
    "trait A { def a = 1 }; class D extends A { D => def m = D.super.a }",
    """|[0:6..0:7): A <= _empty_/A#
       |[0:8..0:8):  <= _empty_/A#`<init>`().
       |[0:14..0:15): a <= _empty_/A#a().
       |[0:29..0:30): D <= _empty_/D#
       |[0:31..0:31):  <= _empty_/D#`<init>`().
       |[0:39..0:40): A => _empty_/A#
       |[0:40..0:40):  => java/lang/Object#`<init>`().
       |[0:43..0:44): D <= local0
       |[0:52..0:53): m <= _empty_/D#m().
       |[0:56..0:57): D => _empty_/D#
       |[0:63..0:63):  => _empty_/A#
       |[0:64..0:65): a => _empty_/A#a().
       |""".stripMargin,
  )

  occurrences(
    "package object self named as package",
    "package object p { p => val x = 1 }; object U { def m = p.x }",
    """|[0:15..0:16): p <= p/package.
       |[0:19..0:20): p <= local0
       |[0:28..0:29): x <= p/package.x.
       |[0:44..0:45): U <= _empty_/U.
       |[0:52..0:53): m <= _empty_/U.m().
       |[0:56..0:57): p => p/
       |[0:58..0:59): x => p/package.x.
       |""".stripMargin,
  )

  occurrences(
    "object self named as object, singleton type from outside",
    "object O { O => }; object U { def m: O.type = O }",
    """|[0:7..0:8): O <= _empty_/O.
       |[0:11..0:12): O <= local0
       |[0:26..0:27): U <= _empty_/U.
       |[0:34..0:35): m <= _empty_/U.m().
       |[0:37..0:38): O => _empty_/O.
       |[0:46..0:47): O => _empty_/O.
       |""".stripMargin,
  )

}

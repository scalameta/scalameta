package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac.TyperOps

class NameRewriteSuite extends SemanticdbSuite {

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  private def synthetics(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: synthetics", code, expected, "Synthetics")

  occurrences(
    "assignments to vars and setters",
    """|object n1 {
       |  class C { var x = 1; x = 2; this.x = 3; x_=(4); x += 1 }
       |  val c = new C
       |  c.x = 5
       |  c.x += 1
       |  c.x_=(6)
       |  class D { def y = 1; def y_=(v: Int) = () }
       |  val d = new D
       |  d.y = 2
       |  d.y += 1
       |  def m = { var v = 1; v = 2; v += 1; v -= 1; v }
       |  class L { def ::(i: Int) = this; def +(i: Int) = this }
       |  var s = new L
       |  s += 2
       |  s ::= 2
       |}
       |""".stripMargin,
    """|[0:7..0:9): n1 <= _empty_/n1.
       |[1:8..1:9): C <= _empty_/n1.C#
       |[1:10..1:10):  <= _empty_/n1.C#`<init>`().
       |[1:16..1:17): x <= _empty_/n1.C#x().
       |[1:23..1:24): x => _empty_/n1.C#`x_=`().
       |[1:35..1:36): x => _empty_/n1.C#`x_=`().
       |[1:42..1:45): x_= => _empty_/n1.C#`x_=`().
       |[1:50..1:51): x => _empty_/n1.C#`x_=`().
       |[1:52..1:54): += => scala/Int#`+`(+4).
       |[2:6..2:7): c <= _empty_/n1.c.
       |[2:14..2:15): C => _empty_/n1.C#
       |[2:15..2:15):  => _empty_/n1.C#`<init>`().
       |[3:2..3:3): c => _empty_/n1.c.
       |[3:4..3:5): x => _empty_/n1.C#`x_=`().
       |[4:2..4:3): c => _empty_/n1.c.
       |[4:4..4:5): x => _empty_/n1.C#`x_=`().
       |[4:6..4:8): += => scala/Int#`+`(+4).
       |[5:2..5:3): c => _empty_/n1.c.
       |[5:4..5:7): x_= => _empty_/n1.C#`x_=`().
       |[6:8..6:9): D <= _empty_/n1.D#
       |[6:10..6:10):  <= _empty_/n1.D#`<init>`().
       |[6:16..6:17): y <= _empty_/n1.D#y().
       |[6:27..6:30): y_= <= _empty_/n1.D#`y_=`().
       |[6:31..6:32): v <= _empty_/n1.D#`y_=`().(v)
       |[6:34..6:37): Int => scala/Int#
       |[7:6..7:7): d <= _empty_/n1.d.
       |[7:14..7:15): D => _empty_/n1.D#
       |[7:15..7:15):  => _empty_/n1.D#`<init>`().
       |[8:2..8:3): d => _empty_/n1.d.
       |[8:4..8:5): y => _empty_/n1.D#`y_=`().
       |[9:2..9:3): d => _empty_/n1.d.
       |[9:4..9:5): y => _empty_/n1.D#`y_=`().
       |[9:6..9:8): += => scala/Int#`+`(+4).
       |[10:6..10:7): m <= _empty_/n1.m().
       |[10:16..10:17): v <= local0
       |[10:23..10:24): v => local0
       |[10:30..10:31): v => local0
       |[10:32..10:34): += => scala/Int#`+`(+4).
       |[10:38..10:39): v => local0
       |[10:40..10:42): -= => scala/Int#`-`(+3).
       |[10:46..10:47): v => local0
       |[11:8..11:9): L <= _empty_/n1.L#
       |[11:10..11:10):  <= _empty_/n1.L#`<init>`().
       |[11:16..11:18): :: <= _empty_/n1.L#`::`().
       |[11:19..11:20): i <= _empty_/n1.L#`::`().(i)
       |[11:22..11:25): Int => scala/Int#
       |[11:39..11:40): + <= _empty_/n1.L#`+`().
       |[11:41..11:42): i <= _empty_/n1.L#`+`().(i)
       |[11:44..11:47): Int => scala/Int#
       |[12:6..12:7): s <= _empty_/n1.s().
       |[12:14..12:15): L => _empty_/n1.L#
       |[12:15..12:15):  => _empty_/n1.L#`<init>`().
       |[13:2..13:3): s => _empty_/n1.`s_=`().
       |[13:4..13:6): += => _empty_/n1.L#`+`().
       |[14:2..14:3): s => _empty_/n1.`s_=`().
       |[14:4..14:7): ::= => _empty_/n1.L#`::`().
       |""".stripMargin,
  )

  occurrences(
    "updates and assignment operators on applications",
    """|object n2 {
       |  val a = Array(1)
       |  a(0) = 2
       |  a(0) += 1
       |  a.update(0, 4)
       |  class M { def apply(i: Int) = i; def update(i: Int, v: Int) = () }
       |  val m = new M
       |  m(1) = 3
       |  m(1) += 1
       |  class B { def +=(i: Int) = this; def -=(i: Int) = this; def ++=(i: Int) = this }
       |  val b = new B
       |  b += 2
       |  b -= 2
       |  b ++= 3
       |}
       |""".stripMargin,
    """|[0:7..0:9): n2 <= _empty_/n2.
       |[1:6..1:7): a <= _empty_/n2.a.
       |[1:10..1:15): Array => scala/Array.
       |[2:2..2:3): a => _empty_/n2.a.
       |[3:2..3:3): a => _empty_/n2.a.
       |[3:7..3:9): += => scala/Int#`+`(+4).
       |[4:2..4:3): a => _empty_/n2.a.
       |[4:4..4:10): update => scala/Array#update().
       |[5:8..5:9): M <= _empty_/n2.M#
       |[5:10..5:10):  <= _empty_/n2.M#`<init>`().
       |[5:16..5:21): apply <= _empty_/n2.M#apply().
       |[5:22..5:23): i <= _empty_/n2.M#apply().(i)
       |[5:25..5:28): Int => scala/Int#
       |[5:32..5:33): i => _empty_/n2.M#apply().(i)
       |[5:39..5:45): update <= _empty_/n2.M#update().
       |[5:46..5:47): i <= _empty_/n2.M#update().(i)
       |[5:49..5:52): Int => scala/Int#
       |[5:54..5:55): v <= _empty_/n2.M#update().(v)
       |[5:57..5:60): Int => scala/Int#
       |[6:6..6:7): m <= _empty_/n2.m.
       |[6:14..6:15): M => _empty_/n2.M#
       |[6:15..6:15):  => _empty_/n2.M#`<init>`().
       |[7:2..7:3): m => _empty_/n2.m.
       |[8:2..8:3): m => _empty_/n2.m.
       |[8:7..8:9): += => scala/Int#`+`(+4).
       |[9:8..9:9): B <= _empty_/n2.B#
       |[9:10..9:10):  <= _empty_/n2.B#`<init>`().
       |[9:16..9:18): += <= _empty_/n2.B#`+=`().
       |[9:19..9:20): i <= _empty_/n2.B#`+=`().(i)
       |[9:22..9:25): Int => scala/Int#
       |[9:39..9:41): -= <= _empty_/n2.B#`-=`().
       |[9:42..9:43): i <= _empty_/n2.B#`-=`().(i)
       |[9:45..9:48): Int => scala/Int#
       |[9:62..9:65): ++= <= _empty_/n2.B#`++=`().
       |[9:66..9:67): i <= _empty_/n2.B#`++=`().(i)
       |[9:69..9:72): Int => scala/Int#
       |[10:6..10:7): b <= _empty_/n2.b.
       |[10:14..10:15): B => _empty_/n2.B#
       |[10:15..10:15):  => _empty_/n2.B#`<init>`().
       |[11:2..11:3): b => _empty_/n2.b.
       |[11:4..11:6): += => _empty_/n2.B#`+=`().
       |[12:2..12:3): b => _empty_/n2.b.
       |[12:4..12:6): -= => _empty_/n2.B#`-=`().
       |[13:2..13:3): b => _empty_/n2.b.
       |[13:4..13:7): ++= => _empty_/n2.B#`++=`().
       |""".stripMargin,
  )

  occurrences(
    "applications of objects, values and functions",
    """|object n4 {
       |  case class Foo(a: Int)
       |  Foo(1)
       |  val f = Foo
       |  f(1)
       |  val g = (i: Int) => i
       |  g(1)
       |  g.apply(2)
       |  Option(1)
       |  List()
       |  List[Int]()
       |  object A { def apply() = 1; def apply(i: Int) = i }
       |  A()
       |  A(1)
       |  class B { def apply(i: Int) = i }
       |  val b = new B
       |  b(1)
       |  new B()(1)
       |}
       |""".stripMargin,
    """|[0:7..0:9): n4 <= _empty_/n4.
       |[1:13..1:16): Foo <= _empty_/n4.Foo#
       |[1:16..1:16):  <= _empty_/n4.Foo#`<init>`().
       |[1:17..1:18): a <= _empty_/n4.Foo#`<init>`().(a)
       |[1:17..1:18): a <= _empty_/n4.Foo#a.
       |[1:20..1:23): Int => scala/Int#
       |[2:2..2:5): Foo => _empty_/n4.Foo.
       |[3:6..3:7): f <= _empty_/n4.f.
       |[3:10..3:13): Foo => _empty_/n4.Foo.
       |[4:2..4:3): f => _empty_/n4.f.
       |[5:6..5:7): g <= _empty_/n4.g.
       |[5:11..5:12): i <= local0
       |[5:14..5:17): Int => scala/Int#
       |[5:22..5:23): i => local0
       |[6:2..6:3): g => _empty_/n4.g.
       |[7:2..7:3): g => _empty_/n4.g.
       |[7:4..7:9): apply => scala/Function1#apply().
       |[8:2..8:8): Option => scala/Option.
       |[9:2..9:6): List => scala/collection/immutable/Nil.
       |[10:2..10:6): List => scala/collection/immutable/Nil.
       |[10:7..10:10): Int => scala/Int#
       |[11:9..11:10): A <= _empty_/n4.A.
       |[11:17..11:22): apply <= _empty_/n4.A.apply().
       |[11:34..11:39): apply <= _empty_/n4.A.apply(+1).
       |[11:40..11:41): i <= _empty_/n4.A.apply(+1).(i)
       |[11:43..11:46): Int => scala/Int#
       |[11:50..11:51): i => _empty_/n4.A.apply(+1).(i)
       |[12:2..12:3): A => _empty_/n4.A.
       |[13:2..13:3): A => _empty_/n4.A.
       |[14:8..14:9): B <= _empty_/n4.B#
       |[14:10..14:10):  <= _empty_/n4.B#`<init>`().
       |[14:16..14:21): apply <= _empty_/n4.B#apply().
       |[14:22..14:23): i <= _empty_/n4.B#apply().(i)
       |[14:25..14:28): Int => scala/Int#
       |[14:32..14:33): i => _empty_/n4.B#apply().(i)
       |[15:6..15:7): b <= _empty_/n4.b.
       |[15:14..15:15): B => _empty_/n4.B#
       |[15:15..15:15):  => _empty_/n4.B#`<init>`().
       |[16:2..16:3): b => _empty_/n4.b.
       |[17:6..17:7): B => _empty_/n4.B#
       |[17:7..17:7):  => _empty_/n4.B#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "selections on a Dynamic",
    """|import scala.language.dynamics
       |class n5d extends Dynamic {
       |  def selectDynamic(n: String) = 1
       |  def applyDynamic(n: String)(as: Any*) = 1
       |  def applyDynamicNamed(n: String)(kvs: (String, Any)*) = 1
       |  def updateDynamic(n: String)(v: Any) = ()
       |}
       |object n5 {
       |  val d = new n5d
       |  d.foo
       |  d.bar(1)
       |  d.baz = 2
       |  d.qux(a = 1)
       |}
       |""".stripMargin,
    """|[0:7..0:12): scala => scala/
       |[0:13..0:21): language => scala/language.
       |[0:22..0:30): dynamics => scala/language.dynamics.
       |[1:6..1:9): n5d <= _empty_/n5d#
       |[1:10..1:10):  <= _empty_/n5d#`<init>`().
       |[1:18..1:25): Dynamic => scala/Dynamic#
       |[1:25..1:25):  => java/lang/Object#`<init>`().
       |[2:6..2:19): selectDynamic <= _empty_/n5d#selectDynamic().
       |[2:20..2:21): n <= _empty_/n5d#selectDynamic().(n)
       |[2:23..2:29): String => scala/Predef.String#
       |[3:6..3:18): applyDynamic <= _empty_/n5d#applyDynamic().
       |[3:19..3:20): n <= _empty_/n5d#applyDynamic().(n)
       |[3:22..3:28): String => scala/Predef.String#
       |[3:30..3:32): as <= _empty_/n5d#applyDynamic().(as)
       |[3:34..3:37): Any => scala/Any#
       |[4:6..4:23): applyDynamicNamed <= _empty_/n5d#applyDynamicNamed().
       |[4:24..4:25): n <= _empty_/n5d#applyDynamicNamed().(n)
       |[4:27..4:33): String => scala/Predef.String#
       |[4:35..4:38): kvs <= _empty_/n5d#applyDynamicNamed().(kvs)
       |[4:41..4:47): String => scala/Predef.String#
       |[4:49..4:52): Any => scala/Any#
       |[5:6..5:19): updateDynamic <= _empty_/n5d#updateDynamic().
       |[5:20..5:21): n <= _empty_/n5d#updateDynamic().(n)
       |[5:23..5:29): String => scala/Predef.String#
       |[5:31..5:32): v <= _empty_/n5d#updateDynamic().(v)
       |[5:34..5:37): Any => scala/Any#
       |[7:7..7:9): n5 <= _empty_/n5.
       |[8:6..8:7): d <= _empty_/n5.d.
       |[8:14..8:17): n5d => _empty_/n5d#
       |[8:17..8:17):  => _empty_/n5d#`<init>`().
       |[9:2..9:3): d => _empty_/n5.d.
       |[10:2..10:3): d => _empty_/n5.d.
       |[11:2..11:3): d => _empty_/n5.d.
       |[12:2..12:3): d => _empty_/n5.d.
       |""".stripMargin,
  )

  occurrences(
    "string interpolators",
    """|object n6 {
       |  val x = 1
       |  s"a $x"
       |  raw"\n$x"
       |  implicit class Ctx(sc: StringContext) { def my(args: Any*) = 1 }
       |  my"a $x"
       |}
       |""".stripMargin,
    """|[0:7..0:9): n6 <= _empty_/n6.
       |[1:6..1:7): x <= _empty_/n6.x.
       |[2:2..2:3): s => scala/StringContext#s().
       |[2:7..2:8): x => _empty_/n6.x.
       |[3:2..3:5): raw => scala/StringContext#raw().
       |[3:9..3:10): x => _empty_/n6.x.
       |[4:17..4:20): Ctx <= _empty_/n6.Ctx#
       |[4:20..4:20):  <= _empty_/n6.Ctx#`<init>`().
       |[4:21..4:23): sc <= _empty_/n6.Ctx#`<init>`().(sc)
       |[4:21..4:23): sc <= _empty_/n6.Ctx#sc.
       |[4:25..4:38): StringContext => scala/StringContext#
       |[4:46..4:48): my <= _empty_/n6.Ctx#my().
       |[4:49..4:53): args <= _empty_/n6.Ctx#my().(args)
       |[4:55..4:58): Any => scala/Any#
       |[5:2..5:4): my => _empty_/n6.Ctx#my().
       |[5:8..5:9): x => _empty_/n6.x.
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "a string interpolator in a pattern",
    """|object n6p {
       |  "a-b" match { case s"$a-$b" => a + b }
       |}
       |""".stripMargin,
    """|[0:7..0:10): n6p <= _empty_/n6p.
       |[1:21..1:22): s => scala/StringContext#s.
       |[1:24..1:25): a <= local0
       |[1:27..1:28): b <= local1
       |[1:33..1:34): a => local0
       |[1:35..1:36): + => java/lang/String#`+`().
       |[1:37..1:38): b => local1
       |""".stripMargin,
  )

  occurrences(
    "references through renamed imports",
    """|object n7a { def b = 1; class B; object O; type T = Int; case class K(i: Int); var v = 1 }
       |object n7 {
       |  import n7a.{b => c, B => D, O => Q, T => U, K => L, v => w}
       |  c
       |  new D
       |  val d: D = null
       |  Q
       |  val u: U = 1
       |  L(1)
       |  w = 2
       |  w += 1
       |}
       |""".stripMargin,
    """|[0:7..0:10): n7a <= _empty_/n7a.
       |[0:17..0:18): b <= _empty_/n7a.b().
       |[0:30..0:31): B <= _empty_/n7a.B#
       |[0:31..0:31):  <= _empty_/n7a.B#`<init>`().
       |[0:40..0:41): O <= _empty_/n7a.O.
       |[0:48..0:49): T <= _empty_/n7a.T#
       |[0:52..0:55): Int => scala/Int#
       |[0:68..0:69): K <= _empty_/n7a.K#
       |[0:69..0:69):  <= _empty_/n7a.K#`<init>`().
       |[0:70..0:71): i <= _empty_/n7a.K#`<init>`().(i)
       |[0:70..0:71): i <= _empty_/n7a.K#i.
       |[0:73..0:76): Int => scala/Int#
       |[0:83..0:84): v <= _empty_/n7a.v().
       |[1:7..1:9): n7 <= _empty_/n7.
       |[2:9..2:12): n7a => _empty_/n7a.
       |[2:14..2:15): b => _empty_/n7a.b().
       |[2:22..2:23): B => _empty_/n7a.B#
       |[2:30..2:31): O => _empty_/n7a.O.
       |[2:38..2:39): T => _empty_/n7a.T#
       |[2:46..2:47): K => _empty_/n7a.K#
       |[2:46..2:47): K => _empty_/n7a.K.
       |[2:54..2:55): v => _empty_/n7a.v().
       |[3:2..3:3): c => _empty_/n7a.b().
       |[4:6..4:7): D => _empty_/n7a.B#
       |[4:7..4:7):  => _empty_/n7a.B#`<init>`().
       |[5:6..5:7): d <= _empty_/n7.d.
       |[5:9..5:10): D => _empty_/n7a.B#
       |[6:2..6:3): Q => _empty_/n7a.O.
       |[7:6..7:7): u <= _empty_/n7.u.
       |[7:9..7:10): U => _empty_/n7a.T#
       |[8:2..8:3): L => _empty_/n7a.K.
       |[9:2..9:3): w => _empty_/n7a.`v_=`().
       |[10:2..10:3): w => _empty_/n7a.`v_=`().
       |[10:4..10:6): += => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  occurrences(
    "references through a self alias",
    """|class n8 { self =>
       |  val x = 1
       |  self.x
       |  this.x
       |  n8.this.x
       |}
       |trait n8t { this: n8 => def y = x }
       |""".stripMargin,
    """|[0:6..0:8): n8 <= _empty_/n8#
       |[0:9..0:9):  <= _empty_/n8#`<init>`().
       |[0:11..0:15): self <= local0
       |[1:6..1:7): x <= _empty_/n8#x.
       |[2:2..2:6): self => local0
       |[2:7..2:8): x => _empty_/n8#x.
       |[3:7..3:8): x => _empty_/n8#x.
       |[4:2..4:4): n8 => _empty_/n8#
       |[4:10..4:11): x => _empty_/n8#x.
       |[6:6..6:9): n8t <= _empty_/n8t#
       |[6:10..6:10):  <= _empty_/n8t#`<init>`().
       |[6:18..6:20): n8 => _empty_/n8#
       |[6:28..6:29): y <= _empty_/n8t#y().
       |[6:32..6:33): x => _empty_/n8#x.
       |""".stripMargin,
  )

  occurrences(
    "implicit conversions and numeric widening",
    """|object n9 {
       |  implicit class Rich(i: Int) { def twice = i * 2 }
       |  1.twice
       |  val one = 1
       |  one.twice
       |  implicit def i2s(i: Int): String = ""
       |  val t: String = one
       |  val big: BigInt = one
       |  val lg: Long = one
       |  val db: Double = one
       |}
       |""".stripMargin,
    """|[0:7..0:9): n9 <= _empty_/n9.
       |[1:17..1:21): Rich <= _empty_/n9.Rich#
       |[1:21..1:21):  <= _empty_/n9.Rich#`<init>`().
       |[1:22..1:23): i <= _empty_/n9.Rich#`<init>`().(i)
       |[1:22..1:23): i <= _empty_/n9.Rich#i.
       |[1:25..1:28): Int => scala/Int#
       |[1:36..1:41): twice <= _empty_/n9.Rich#twice().
       |[1:44..1:45): i => _empty_/n9.Rich#i.
       |[1:46..1:47): * => scala/Int#`*`(+3).
       |[2:4..2:9): twice => _empty_/n9.Rich#twice().
       |[3:6..3:9): one <= _empty_/n9.one.
       |[4:2..4:5): one => _empty_/n9.one.
       |[4:6..4:11): twice => _empty_/n9.Rich#twice().
       |[5:15..5:18): i2s <= _empty_/n9.i2s().
       |[5:19..5:20): i <= _empty_/n9.i2s().(i)
       |[5:22..5:25): Int => scala/Int#
       |[5:28..5:34): String => scala/Predef.String#
       |[6:6..6:7): t <= _empty_/n9.t.
       |[6:9..6:15): String => scala/Predef.String#
       |[6:18..6:21): one => _empty_/n9.one.
       |[7:6..7:9): big <= _empty_/n9.big.
       |[7:11..7:17): BigInt => scala/package.BigInt#
       |[7:20..7:23): one => _empty_/n9.one.
       |[8:6..8:8): lg <= _empty_/n9.lg.
       |[8:10..8:14): Long => scala/Long#
       |[8:17..8:20): one => _empty_/n9.one.
       |[9:6..9:8): db <= _empty_/n9.db.
       |[9:10..9:16): Double => scala/Double#
       |[9:19..9:22): one => _empty_/n9.one.
       |""".stripMargin,
  )

  occurrences(
    "super calls",
    """|class n10a { def m = 1 }
       |trait n10t extends n10a { override def m = 2 }
       |class n10 extends n10a with n10t { override def m = super.m + super[n10t].m }
       |""".stripMargin,
    """|[0:6..0:10): n10a <= _empty_/n10a#
       |[0:11..0:11):  <= _empty_/n10a#`<init>`().
       |[0:17..0:18): m <= _empty_/n10a#m().
       |[1:6..1:10): n10t <= _empty_/n10t#
       |[1:11..1:11):  <= _empty_/n10t#`<init>`().
       |[1:19..1:23): n10a => _empty_/n10a#
       |[1:39..1:40): m <= _empty_/n10t#m().
       |[2:6..2:9): n10 <= _empty_/n10#
       |[2:10..2:10):  <= _empty_/n10#`<init>`().
       |[2:18..2:22): n10a => _empty_/n10a#
       |[2:22..2:22):  => _empty_/n10a#`<init>`().
       |[2:28..2:32): n10t => _empty_/n10t#
       |[2:48..2:49): m <= _empty_/n10#m().
       |[2:52..2:52):  => _empty_/n10#
       |[2:57..2:57):  => _empty_/n10t#
       |[2:58..2:59): m => _empty_/n10t#m().
       |[2:60..2:61): + => scala/Int#`+`(+4).
       |[2:62..2:62):  => _empty_/n10#
       |[2:68..2:72): n10t => _empty_/n10t#
       |[2:74..2:75): m => _empty_/n10t#m().
       |""".stripMargin,
  )

  occurrences(
    "named and default arguments",
    """|object n11 {
       |  def f(a: Int = 1, b: Int = 2) = a
       |  f(b = 3)
       |  f()
       |  case class K(a: Int = 1, b: Int = 2)
       |  K(b = 1).copy(a = 2)
       |  new K(b = 3)
       |}
       |""".stripMargin,
    """|[0:7..0:10): n11 <= _empty_/n11.
       |[1:6..1:7): f <= _empty_/n11.f().
       |[1:8..1:9): a <= _empty_/n11.f().(a)
       |[1:11..1:14): Int => scala/Int#
       |[1:20..1:21): b <= _empty_/n11.f().(b)
       |[1:23..1:26): Int => scala/Int#
       |[1:34..1:35): a => _empty_/n11.f().(a)
       |[2:2..2:3): f => _empty_/n11.f().
       |[2:4..2:5): b => _empty_/n11.f().(b)
       |[3:2..3:3): f => _empty_/n11.f().
       |[4:13..4:14): K <= _empty_/n11.K#
       |[4:14..4:14):  <= _empty_/n11.K#`<init>`().
       |[4:15..4:16): a <= _empty_/n11.K#`<init>`().(a)
       |[4:15..4:16): a <= _empty_/n11.K#a.
       |[4:18..4:21): Int => scala/Int#
       |[4:27..4:28): b <= _empty_/n11.K#`<init>`().(b)
       |[4:27..4:28): b <= _empty_/n11.K#b.
       |[4:30..4:33): Int => scala/Int#
       |[5:2..5:3): K => _empty_/n11.K.
       |[5:4..5:5): b => _empty_/n11.K.apply().(b)
       |[5:11..5:15): copy => _empty_/n11.K#copy().
       |[5:16..5:17): a => _empty_/n11.K#copy().(a)
       |[6:6..6:7): K => _empty_/n11.K#
       |[6:7..6:7):  => _empty_/n11.K#`<init>`().
       |[6:8..6:9): b => _empty_/n11.K#`<init>`().(b)
       |""".stripMargin,
  )

  occurrences(
    "macro calls",
    """|object n15 {
       |  val n = sourcecode.Name.generate
       |  val l = sourcecode.Line.generate
       |}
       |""".stripMargin,
    """|[0:7..0:10): n15 <= _empty_/n15.
       |[1:6..1:7): n <= _empty_/n15.n.
       |[1:10..1:20): sourcecode => sourcecode/
       |[1:21..1:25): Name => sourcecode/Name.
       |[1:26..1:34): generate => sourcecode/NameMacros#generate().
       |[2:6..2:7): l <= _empty_/n15.l.
       |[2:10..2:20): sourcecode => sourcecode/
       |[2:21..2:25): Line => sourcecode/Line.
       |[2:26..2:34): generate => sourcecode/LineMacros#generate().
       |""".stripMargin,
  )

  occurrences(
    "assignments to explicit setters through renamed imports",
    """|object n14a { private var u = 0; def v = u; def v_=(i: Int): Unit = u = i }
       |object n14 {
       |  import n14a.{v => w}
       |  w = 2
       |  w += 1
       |}
       |""".stripMargin,
    """|[0:7..0:11): n14a <= _empty_/n14a.
       |[0:26..0:27): u <= _empty_/n14a.u().
       |[0:37..0:38): v <= _empty_/n14a.v().
       |[0:41..0:42): u => _empty_/n14a.u().
       |[0:48..0:51): v_= <= _empty_/n14a.`v_=`().
       |[0:52..0:53): i <= _empty_/n14a.`v_=`().(i)
       |[0:55..0:58): Int => scala/Int#
       |[0:61..0:65): Unit => scala/Unit#
       |[0:68..0:69): u => _empty_/n14a.`u_=`().
       |[0:72..0:73): i => _empty_/n14a.`v_=`().(i)
       |[1:7..1:10): n14 <= _empty_/n14.
       |[2:9..2:13): n14a => _empty_/n14a.
       |[2:15..2:16): v => _empty_/n14a.v().
       |[3:2..3:3): w => _empty_/n14a.`v_=`().
       |[4:2..4:3): w => _empty_/n14a.`v_=`().
       |[4:4..4:6): += => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  occurrences(
    "empty lists through a qualified or an explicit apply",
    """|object n16 {
       |  List.apply()
       |  scala.List()
       |  scala.collection.immutable.List[Int]()
       |}
       |""".stripMargin,
    """|[0:7..0:10): n16 <= _empty_/n16.
       |[1:7..1:12): apply => scala/collection/immutable/Nil.
       |[2:8..2:12): List => scala/collection/immutable/Nil.
       |[3:29..3:33): List => scala/collection/immutable/Nil.
       |[3:34..3:37): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "empty lists through a qualified name",
    """|object n17 {
       |  val a = scala.collection.immutable.Nil
       |  import scala.collection.immutable
       |  val b = immutable.Nil
       |}
       |""".stripMargin,
    """|[0:7..0:10): n17 <= _empty_/n17.
       |[1:6..1:7): a <= _empty_/n17.a.
       |[1:10..1:15): scala => scala/
       |[1:16..1:26): collection => scala/collection/
       |[1:27..1:36): immutable => scala/collection/immutable/
       |[1:37..1:40): Nil => scala/collection/immutable/Nil.
       |[2:9..2:14): scala => scala/
       |[2:15..2:25): collection => scala/collection/
       |[2:26..2:35): immutable => scala/collection/immutable/
       |[3:6..3:7): b <= _empty_/n17.b.
       |[3:10..3:19): immutable => scala/collection/immutable/
       |[3:20..3:23): Nil => scala/collection/immutable/Nil.
       |""".stripMargin,
  )

  occurrences(
    "empty lists through an import, a pattern and a qualified `List`",
    """|import scala.collection.immutable
       |object n18 {
       |  def f(xs: List[Int]) = xs match {
       |    case immutable.Nil => 0
       |    case _ => 1
       |  }
       |  val a = scala.List()
       |  val b = immutable.List[Int]()
       |  import scala.collection.immutable.Nil
       |  val c = Nil
       |}
       |""".stripMargin,
    """|[0:7..0:12): scala => scala/
       |[0:13..0:23): collection => scala/collection/
       |[0:24..0:33): immutable => scala/collection/immutable/
       |[1:7..1:10): n18 <= _empty_/n18.
       |[2:6..2:7): f <= _empty_/n18.f().
       |[2:8..2:10): xs <= _empty_/n18.f().(xs)
       |[2:12..2:16): List => scala/package.List#
       |[2:17..2:20): Int => scala/Int#
       |[2:25..2:27): xs => _empty_/n18.f().(xs)
       |[3:9..3:18): immutable => scala/collection/immutable/
       |[3:19..3:22): Nil => scala/collection/immutable/Nil.
       |[6:6..6:7): a <= _empty_/n18.a.
       |[6:16..6:20): List => scala/collection/immutable/Nil.
       |[7:6..7:7): b <= _empty_/n18.b.
       |[7:20..7:24): List => scala/collection/immutable/Nil.
       |[7:25..7:28): Int => scala/Int#
       |[8:9..8:14): scala => scala/
       |[8:15..8:25): collection => scala/collection/
       |[8:26..8:35): immutable => scala/collection/immutable/
       |[8:36..8:39): Nil => scala/collection/immutable/Nil.
       |[9:6..9:7): c <= _empty_/n18.c.
       |[9:10..9:13): Nil => scala/collection/immutable/Nil.
       |""".stripMargin,
  )

  occurrences(
    "constant references and folded expressions",
    """|object n19 {
       |  final val K = 1
       |  final val S = "a"
       |  final val T = true
       |  val a = K
       |  val b = K + 1
       |  val c = -K
       |  val d = n19.K
       |  val e = K.toLong
       |  val f: Long = K
       |  val g = S + K
       |  val h = !T
       |  val i = (K: Int)
       |  val j: S.type = S
       |  val k = Int.MaxValue
       |  val l = Integer.MAX_VALUE
       |  val m = java.lang.Math.PI
       |  import n19.{K => J}
       |  val n = J * 2
       |}
       |""".stripMargin,
    """|[0:7..0:10): n19 <= _empty_/n19.
       |[1:12..1:13): K <= _empty_/n19.K.
       |[2:12..2:13): S <= _empty_/n19.S.
       |[3:12..3:13): T <= _empty_/n19.T.
       |[4:6..4:7): a <= _empty_/n19.a.
       |[4:10..4:11): K => _empty_/n19.K.
       |[5:6..5:7): b <= _empty_/n19.b.
       |[5:10..5:11): K => _empty_/n19.K.
       |[5:12..5:13): + => scala/Int#`+`(+4).
       |[6:6..6:7): c <= _empty_/n19.c.
       |[6:10..6:11): - => scala/Int#`unary_-`().
       |[6:11..6:12): K => _empty_/n19.K.
       |[7:6..7:7): d <= _empty_/n19.d.
       |[7:10..7:13): n19 => _empty_/n19.
       |[7:14..7:15): K => _empty_/n19.K.
       |[8:6..8:7): e <= _empty_/n19.e.
       |[8:10..8:11): K => _empty_/n19.K.
       |[8:12..8:18): toLong => scala/Int#toLong().
       |[9:6..9:7): f <= _empty_/n19.f.
       |[9:9..9:13): Long => scala/Long#
       |[9:16..9:17): K => _empty_/n19.K.
       |[10:6..10:7): g <= _empty_/n19.g.
       |[10:10..10:11): S => _empty_/n19.S.
       |[10:12..10:13): + => java/lang/String#`+`().
       |[10:14..10:15): K => _empty_/n19.K.
       |[11:6..11:7): h <= _empty_/n19.h.
       |[11:10..11:11): ! => scala/Boolean#`unary_!`().
       |[11:11..11:12): T => _empty_/n19.T.
       |[12:6..12:7): i <= _empty_/n19.i.
       |[12:11..12:12): K => _empty_/n19.K.
       |[12:14..12:17): Int => scala/Int#
       |[13:6..13:7): j <= _empty_/n19.j.
       |[13:9..13:10): S => _empty_/n19.S.
       |[13:18..13:19): S => _empty_/n19.S.
       |[14:6..14:7): k <= _empty_/n19.k.
       |[14:10..14:13): Int => scala/Int.
       |[14:14..14:22): MaxValue => scala/Int.MaxValue.
       |[15:6..15:7): l <= _empty_/n19.l.
       |[15:10..15:17): Integer => java/lang/Integer#
       |[15:18..15:27): MAX_VALUE => java/lang/Integer#MAX_VALUE.
       |[16:6..16:7): m <= _empty_/n19.m.
       |[16:10..16:14): java => java/
       |[16:15..16:19): lang => java/lang/
       |[16:20..16:24): Math => java/lang/Math#
       |[16:25..16:27): PI => java/lang/Math#PI.
       |[17:9..17:12): n19 => _empty_/n19.
       |[17:14..17:15): K => _empty_/n19.K.
       |[18:6..18:7): n <= _empty_/n19.n.
       |[18:10..18:11): J => _empty_/n19.K.
       |[18:12..18:13): * => scala/Int#`*`(+3).
       |""".stripMargin,
  )

  occurrences(
    "constants in patterns",
    """|object n20 {
       |  final val K = 1
       |  object O { final val K = 2 }
       |  def f(x: Int) = x match { case K => 0; case O.K => 1; case _ => 2 }
       |}
       |""".stripMargin,
    """|[0:7..0:10): n20 <= _empty_/n20.
       |[1:12..1:13): K <= _empty_/n20.K.
       |[2:9..2:10): O <= _empty_/n20.O.
       |[2:23..2:24): K <= _empty_/n20.O.K.
       |[3:6..3:7): f <= _empty_/n20.f().
       |[3:8..3:9): x <= _empty_/n20.f().(x)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): x => _empty_/n20.f().(x)
       |[3:33..3:34): K => _empty_/n20.K.
       |[3:46..3:47): O => _empty_/n20.O.
       |[3:48..3:49): K => _empty_/n20.O.K.
       |""".stripMargin,
  )

  occurrences(
    "right-associative operators on constants",
    """|object n21 {
       |  final val K = 1
       |  final val S = "a"
       |  val x = 1
       |  class R { def +:(i: Int) = this; def ::(i: => Int) = this }
       |  val r = new R
       |  val a = K :: Nil
       |  val b = (K + 1) :: Nil
       |  val c = 1 :: Nil
       |  val d = x :: Nil
       |  val e = S +: Seq()
       |  val f = Int.MaxValue :: Nil
       |  val g = K +: r
       |  val h = K :: r
       |  val i = x :: r
       |  val j = (K: Int) :: Nil
       |  val k = -K :: Nil
       |  val l = n21.K :: Nil
       |  val m = K :: K :: Nil
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n21 <= _empty_/n21.
             |[1:12..1:13): K <= _empty_/n21.K.
             |[2:12..2:13): S <= _empty_/n21.S.
             |[3:6..3:7): x <= _empty_/n21.x.
             |[4:8..4:9): R <= _empty_/n21.R#
             |[4:10..4:10):  <= _empty_/n21.R#`<init>`().
             |[4:16..4:18): +: <= _empty_/n21.R#`+:`().
             |[4:19..4:20): i <= _empty_/n21.R#`+:`().(i)
             |[4:22..4:25): Int => scala/Int#
             |[4:39..4:41): :: <= _empty_/n21.R#`::`().
             |[4:42..4:43): i <= _empty_/n21.R#`::`().(i)
             |[4:48..4:51): Int => scala/Int#
             |[5:6..5:7): r <= _empty_/n21.r.
             |[5:14..5:15): R => _empty_/n21.R#
             |[5:15..5:15):  => _empty_/n21.R#`<init>`().
             |[6:6..6:7): a <= _empty_/n21.a.
             |[6:10..6:11): K => _empty_/n21.K.
             |[6:12..6:14): :: => scala/collection/immutable/List#`::`().
             |[6:15..6:18): Nil => scala/collection/immutable/Nil.
             |[7:6..7:7): b <= _empty_/n21.b.
             |[7:11..7:12): K => _empty_/n21.K.
             |[7:13..7:14): + => scala/Int#`+`(+4).
             |[7:18..7:20): :: => scala/collection/immutable/List#`::`().
             |[7:21..7:24): Nil => scala/collection/immutable/Nil.
             |[8:6..8:7): c <= _empty_/n21.c.
             |[8:12..8:14): :: => scala/collection/immutable/List#`::`().
             |[8:15..8:18): Nil => scala/collection/immutable/Nil.
             |[9:6..9:7): d <= _empty_/n21.d.
             |[9:10..9:11): x => _empty_/n21.x.
             |[9:12..9:14): :: => scala/collection/immutable/List#`::`().
             |[9:15..9:18): Nil => scala/collection/immutable/Nil.
             |[10:6..10:7): e <= _empty_/n21.e.
             |[10:10..10:11): S => _empty_/n21.S.
             |[10:12..10:14): +: => scala/collection/SeqLike#`+:`().
             |[10:15..10:18): Seq => scala/collection/Seq.
             |[11:6..11:7): f <= _empty_/n21.f.
             |[11:10..11:13): Int => scala/Int.
             |[11:14..11:22): MaxValue => scala/Int.MaxValue.
             |[11:23..11:25): :: => scala/collection/immutable/List#`::`().
             |[11:26..11:29): Nil => scala/collection/immutable/Nil.
             |[12:6..12:7): g <= _empty_/n21.g.
             |[12:10..12:11): K => _empty_/n21.K.
             |[12:12..12:14): +: => _empty_/n21.R#`+:`().
             |[12:15..12:16): r => _empty_/n21.r.
             |[13:6..13:7): h <= _empty_/n21.h.
             |[13:10..13:11): K => _empty_/n21.K.
             |[13:12..13:14): :: => _empty_/n21.R#`::`().
             |[13:15..13:16): r => _empty_/n21.r.
             |[14:6..14:7): i <= _empty_/n21.i.
             |[14:10..14:11): x => _empty_/n21.x.
             |[14:12..14:14): :: => _empty_/n21.R#`::`().
             |[14:15..14:16): r => _empty_/n21.r.
             |[15:6..15:7): j <= _empty_/n21.j.
             |[15:11..15:12): K => _empty_/n21.K.
             |[15:14..15:17): Int => scala/Int#
             |[15:19..15:21): :: => scala/collection/immutable/List#`::`().
             |[15:22..15:25): Nil => scala/collection/immutable/Nil.
             |[16:6..16:7): k <= _empty_/n21.k.
             |[16:10..16:11): - => scala/Int#`unary_-`().
             |[16:11..16:12): K => _empty_/n21.K.
             |[16:13..16:15): :: => scala/collection/immutable/List#`::`().
             |[16:16..16:19): Nil => scala/collection/immutable/Nil.
             |[17:6..17:7): l <= _empty_/n21.l.
             |[17:10..17:13): n21 => _empty_/n21.
             |[17:14..17:15): K => _empty_/n21.K.
             |[17:16..17:18): :: => scala/collection/immutable/List#`::`().
             |[17:19..17:22): Nil => scala/collection/immutable/Nil.
             |[18:6..18:7): m <= _empty_/n21.m.
             |[18:10..18:11): K => _empty_/n21.K.
             |[18:12..18:14): :: => scala/collection/immutable/List#`::`().
             |[18:15..18:16): K => _empty_/n21.K.
             |[18:17..18:19): :: => scala/collection/immutable/List#`::`().
             |[18:20..18:23): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n21 <= _empty_/n21.
         |[1:12..1:13): K <= _empty_/n21.K.
         |[2:12..2:13): S <= _empty_/n21.S.
         |[3:6..3:7): x <= _empty_/n21.x.
         |[4:8..4:9): R <= _empty_/n21.R#
         |[4:10..4:10):  <= _empty_/n21.R#`<init>`().
         |[4:16..4:18): +: <= _empty_/n21.R#`+:`().
         |[4:19..4:20): i <= _empty_/n21.R#`+:`().(i)
         |[4:22..4:25): Int => scala/Int#
         |[4:39..4:41): :: <= _empty_/n21.R#`::`().
         |[4:42..4:43): i <= _empty_/n21.R#`::`().(i)
         |[4:48..4:51): Int => scala/Int#
         |[5:6..5:7): r <= _empty_/n21.r.
         |[5:14..5:15): R => _empty_/n21.R#
         |[5:15..5:15):  => _empty_/n21.R#`<init>`().
         |[6:6..6:7): a <= _empty_/n21.a.
         |[6:10..6:11): K => _empty_/n21.K.
         |[6:12..6:14): :: => scala/collection/immutable/List#`::`().
         |[6:15..6:18): Nil => scala/package.Nil.
         |[7:6..7:7): b <= _empty_/n21.b.
         |[7:11..7:12): K => _empty_/n21.K.
         |[7:13..7:14): + => scala/Int#`+`(+4).
         |[7:18..7:20): :: => scala/collection/immutable/List#`::`().
         |[7:21..7:24): Nil => scala/package.Nil.
         |[8:6..8:7): c <= _empty_/n21.c.
         |[8:12..8:14): :: => scala/collection/immutable/List#`::`().
         |[8:15..8:18): Nil => scala/package.Nil.
         |[9:6..9:7): d <= _empty_/n21.d.
         |[9:10..9:11): x => _empty_/n21.x.
         |[9:12..9:14): :: => scala/collection/immutable/List#`::`().
         |[9:15..9:18): Nil => scala/package.Nil.
         |[10:6..10:7): e <= _empty_/n21.e.
         |[10:10..10:11): S => _empty_/n21.S.
         |[10:12..10:14): +: => scala/collection/SeqOps#`+:`().
         |[10:15..10:18): Seq => scala/package.Seq.
         |[11:6..11:7): f <= _empty_/n21.f.
         |[11:10..11:13): Int => scala/Int.
         |[11:14..11:22): MaxValue => scala/Int.MaxValue.
         |[11:23..11:25): :: => scala/collection/immutable/List#`::`().
         |[11:26..11:29): Nil => scala/package.Nil.
         |[12:6..12:7): g <= _empty_/n21.g.
         |[12:10..12:11): K => _empty_/n21.K.
         |[12:12..12:14): +: => _empty_/n21.R#`+:`().
         |[12:15..12:16): r => _empty_/n21.r.
         |[13:6..13:7): h <= _empty_/n21.h.
         |[13:10..13:11): K => _empty_/n21.K.
         |[13:12..13:14): :: => _empty_/n21.R#`::`().
         |[13:15..13:16): r => _empty_/n21.r.
         |[14:6..14:7): i <= _empty_/n21.i.
         |[14:10..14:11): x => _empty_/n21.x.
         |[14:12..14:14): :: => _empty_/n21.R#`::`().
         |[14:15..14:16): r => _empty_/n21.r.
         |[15:6..15:7): j <= _empty_/n21.j.
         |[15:11..15:12): K => _empty_/n21.K.
         |[15:14..15:17): Int => scala/Int#
         |[15:19..15:21): :: => scala/collection/immutable/List#`::`().
         |[15:22..15:25): Nil => scala/package.Nil.
         |[16:6..16:7): k <= _empty_/n21.k.
         |[16:10..16:11): - => scala/Int#`unary_-`().
         |[16:11..16:12): K => _empty_/n21.K.
         |[16:13..16:15): :: => scala/collection/immutable/List#`::`().
         |[16:16..16:19): Nil => scala/package.Nil.
         |[17:6..17:7): l <= _empty_/n21.l.
         |[17:10..17:13): n21 => _empty_/n21.
         |[17:14..17:15): K => _empty_/n21.K.
         |[17:16..17:18): :: => scala/collection/immutable/List#`::`().
         |[17:19..17:22): Nil => scala/package.Nil.
         |[18:6..18:7): m <= _empty_/n21.m.
         |[18:10..18:11): K => _empty_/n21.K.
         |[18:12..18:14): :: => scala/collection/immutable/List#`::`().
         |[18:15..18:16): K => _empty_/n21.K.
         |[18:17..18:19): :: => scala/collection/immutable/List#`::`().
         |[18:20..18:23): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  if (ScalaVersion.is213) occurrences(
    "lazy list conses on constants",
    """|object n22 {
       |  final val K = 1
       |  val x = 1
       |  val a = K #:: LazyList.empty
       |  val b = x #:: LazyList.empty
       |}
       |""".stripMargin,
    """|[0:7..0:10): n22 <= _empty_/n22.
       |[1:12..1:13): K <= _empty_/n22.K.
       |[2:6..2:7): x <= _empty_/n22.x.
       |[3:6..3:7): a <= _empty_/n22.a.
       |[3:10..3:11): K => _empty_/n22.K.
       |[3:12..3:15): #:: => scala/collection/immutable/LazyList.Deferrer#`#::`().
       |[3:16..3:24): LazyList => scala/package.LazyList.
       |[3:25..3:30): empty => scala/collection/immutable/LazyList.empty().
       |[4:6..4:7): b <= _empty_/n22.b.
       |[4:10..4:11): x => _empty_/n22.x.
       |[4:12..4:15): #:: => scala/collection/immutable/LazyList.Deferrer#`#::`().
       |[4:16..4:24): LazyList => scala/package.LazyList.
       |[4:25..4:30): empty => scala/collection/immutable/LazyList.empty().
       |""".stripMargin,
  )

  occurrences(
    "references to `List`, `Seq` and `Nil` through the scala package",
    """|object n23 {
       |  val a = List(1)
       |  val b = Seq(1)
       |  val c = Nil
       |  val d = scala.List(1)
       |  val e = scala.Seq(1)
       |  val f = scala.Nil
       |  val g = List.empty[Int]
       |  val h = Vector(1)
       |  val i: List[Int] = Nil
       |  val j = List
       |  val k = Seq
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n23 <= _empty_/n23.
             |[1:6..1:7): a <= _empty_/n23.a.
             |[1:10..1:14): List => scala/collection/immutable/List.
             |[2:6..2:7): b <= _empty_/n23.b.
             |[2:10..2:13): Seq => scala/collection/Seq.
             |[3:6..3:7): c <= _empty_/n23.c.
             |[3:10..3:13): Nil => scala/collection/immutable/Nil.
             |[4:6..4:7): d <= _empty_/n23.d.
             |[4:10..4:15): scala => scala/
             |[4:16..4:20): List => scala/collection/immutable/List.
             |[5:6..5:7): e <= _empty_/n23.e.
             |[5:10..5:15): scala => scala/
             |[5:16..5:19): Seq => scala/collection/Seq.
             |[6:6..6:7): f <= _empty_/n23.f.
             |[6:10..6:15): scala => scala/
             |[6:16..6:19): Nil => scala/collection/immutable/Nil.
             |[7:6..7:7): g <= _empty_/n23.g.
             |[7:10..7:14): List => scala/collection/immutable/List.
             |[7:15..7:20): empty => scala/collection/immutable/List.empty().
             |[7:21..7:24): Int => scala/Int#
             |[8:6..8:7): h <= _empty_/n23.h.
             |[8:10..8:16): Vector => scala/package.Vector.
             |[9:6..9:7): i <= _empty_/n23.i.
             |[9:9..9:13): List => scala/package.List#
             |[9:14..9:17): Int => scala/Int#
             |[9:21..9:24): Nil => scala/collection/immutable/Nil.
             |[10:6..10:7): j <= _empty_/n23.j.
             |[10:10..10:14): List => scala/collection/immutable/List.
             |[11:6..11:7): k <= _empty_/n23.k.
             |[11:10..11:13): Seq => scala/collection/Seq.
             |""".stripMargin
      },
      """|[0:7..0:10): n23 <= _empty_/n23.
         |[1:6..1:7): a <= _empty_/n23.a.
         |[1:10..1:14): List => scala/package.List.
         |[2:6..2:7): b <= _empty_/n23.b.
         |[2:10..2:13): Seq => scala/package.Seq.
         |[3:6..3:7): c <= _empty_/n23.c.
         |[3:10..3:13): Nil => scala/package.Nil.
         |[4:6..4:7): d <= _empty_/n23.d.
         |[4:10..4:15): scala => scala/
         |[4:16..4:20): List => scala/package.List.
         |[5:6..5:7): e <= _empty_/n23.e.
         |[5:10..5:15): scala => scala/
         |[5:16..5:19): Seq => scala/package.Seq.
         |[6:6..6:7): f <= _empty_/n23.f.
         |[6:10..6:15): scala => scala/
         |[6:16..6:19): Nil => scala/package.Nil.
         |[7:6..7:7): g <= _empty_/n23.g.
         |[7:10..7:14): List => scala/package.List.
         |[7:15..7:20): empty => scala/collection/immutable/List.empty().
         |[7:21..7:24): Int => scala/Int#
         |[8:6..8:7): h <= _empty_/n23.h.
         |[8:10..8:16): Vector => scala/package.Vector.
         |[9:6..9:7): i <= _empty_/n23.i.
         |[9:9..9:13): List => scala/package.List#
         |[9:14..9:17): Int => scala/Int#
         |[9:21..9:24): Nil => scala/package.Nil.
         |[10:6..10:7): j <= _empty_/n23.j.
         |[10:10..10:14): List => scala/package.List.
         |[11:6..11:7): k <= _empty_/n23.k.
         |[11:10..11:13): Seq => scala/package.Seq.
         |""".stripMargin,
    ),
  )

  occurrences(
    "empty collections",
    """|object n24 {
       |  val a = Seq()
       |  val b = Array[Int]()
       |  val c = Vector()
       |  val d = Set()
       |  val e = Map()
       |  val f = Option.empty[Int]
       |  val g = List[Int]()
       |  val h = scala.collection.immutable.List.apply[Int]()
       |  def m(xs: List[Int]) = xs match { case List() => 0; case _ => 1 }
       |  def v(xs: Int*) = xs
       |  v(List(): _*)
       |  val i = List(List())
       |  val j = (List(), List())
       |  val k = List() ++ List()
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n24 <= _empty_/n24.
             |[1:6..1:7): a <= _empty_/n24.a.
             |[1:10..1:13): Seq => scala/collection/Seq.
             |[2:6..2:7): b <= _empty_/n24.b.
             |[2:10..2:15): Array => scala/Array.
             |[2:16..2:19): Int => scala/Int#
             |[3:6..3:7): c <= _empty_/n24.c.
             |[3:10..3:16): Vector => scala/package.Vector.
             |[4:6..4:7): d <= _empty_/n24.d.
             |[4:10..4:13): Set => scala/Predef.Set.
             |[5:6..5:7): e <= _empty_/n24.e.
             |[5:10..5:13): Map => scala/Predef.Map.
             |[6:6..6:7): f <= _empty_/n24.f.
             |[6:10..6:16): Option => scala/Option.
             |[6:17..6:22): empty => scala/Option.empty().
             |[6:23..6:26): Int => scala/Int#
             |[7:6..7:7): g <= _empty_/n24.g.
             |[7:10..7:14): List => scala/collection/immutable/Nil.
             |[7:15..7:18): Int => scala/Int#
             |[8:6..8:7): h <= _empty_/n24.h.
             |[8:42..8:47): apply => scala/collection/immutable/Nil.
             |[8:48..8:51): Int => scala/Int#
             |[9:6..9:7): m <= _empty_/n24.m().
             |[9:8..9:10): xs <= _empty_/n24.m().(xs)
             |[9:12..9:16): List => scala/package.List#
             |[9:17..9:20): Int => scala/Int#
             |[9:25..9:27): xs => _empty_/n24.m().(xs)
             |[9:41..9:45): List => scala/collection/immutable/List.
             |[10:6..10:7): v <= _empty_/n24.v().
             |[10:8..10:10): xs <= _empty_/n24.v().(xs)
             |[10:12..10:15): Int => scala/Int#
             |[10:20..10:22): xs => _empty_/n24.v().(xs)
             |[11:2..11:3): v => _empty_/n24.v().
             |[11:4..11:8): List => scala/collection/immutable/Nil.
             |[12:6..12:7): i <= _empty_/n24.i.
             |[12:10..12:14): List => scala/collection/immutable/List.
             |[12:15..12:19): List => scala/collection/immutable/Nil.
             |[13:6..13:7): j <= _empty_/n24.j.
             |[13:11..13:15): List => scala/collection/immutable/Nil.
             |[13:19..13:23): List => scala/collection/immutable/Nil.
             |[14:6..14:7): k <= _empty_/n24.k.
             |[14:10..14:14): List => scala/collection/immutable/Nil.
             |[14:17..14:19): ++ => scala/collection/immutable/List#`++`().
             |[14:20..14:24): List => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n24 <= _empty_/n24.
         |[1:6..1:7): a <= _empty_/n24.a.
         |[1:10..1:13): Seq => scala/package.Seq.
         |[2:6..2:7): b <= _empty_/n24.b.
         |[2:10..2:15): Array => scala/Array.
         |[2:16..2:19): Int => scala/Int#
         |[3:6..3:7): c <= _empty_/n24.c.
         |[3:10..3:16): Vector => scala/package.Vector.
         |[4:6..4:7): d <= _empty_/n24.d.
         |[4:10..4:13): Set => scala/Predef.Set.
         |[5:6..5:7): e <= _empty_/n24.e.
         |[5:10..5:13): Map => scala/Predef.Map.
         |[6:6..6:7): f <= _empty_/n24.f.
         |[6:10..6:16): Option => scala/Option.
         |[6:17..6:22): empty => scala/Option.empty().
         |[6:23..6:26): Int => scala/Int#
         |[7:6..7:7): g <= _empty_/n24.g.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:18): Int => scala/Int#
         |[8:6..8:7): h <= _empty_/n24.h.
         |[8:42..8:47): apply => scala/collection/immutable/Nil.
         |[8:48..8:51): Int => scala/Int#
         |[9:6..9:7): m <= _empty_/n24.m().
         |[9:8..9:10): xs <= _empty_/n24.m().(xs)
         |[9:12..9:16): List => scala/package.List#
         |[9:17..9:20): Int => scala/Int#
         |[9:25..9:27): xs => _empty_/n24.m().(xs)
         |[9:41..9:45): List => scala/package.List.
         |[10:6..10:7): v <= _empty_/n24.v().
         |[10:8..10:10): xs <= _empty_/n24.v().(xs)
         |[10:12..10:15): Int => scala/Int#
         |[10:20..10:22): xs => _empty_/n24.v().(xs)
         |[11:2..11:3): v => _empty_/n24.v().
         |[11:4..11:8): List => scala/collection/immutable/Nil.
         |[12:6..12:7): i <= _empty_/n24.i.
         |[12:10..12:14): List => scala/package.List.
         |[12:15..12:19): List => scala/collection/immutable/Nil.
         |[13:6..13:7): j <= _empty_/n24.j.
         |[13:11..13:15): List => scala/collection/immutable/Nil.
         |[13:19..13:23): List => scala/collection/immutable/Nil.
         |[14:6..14:7): k <= _empty_/n24.k.
         |[14:10..14:14): List => scala/collection/immutable/Nil.
         |[14:17..14:19): ++ => scala/collection/IterableOps#`++`().
         |[14:20..14:24): List => scala/collection/immutable/Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "constants in arguments of annotations",
    """|object n25 {
       |  final val S = "a"
       |  final val L = 1L
       |  @SuppressWarnings(Array("b", S)) def a = 1
       |  @java.lang.SuppressWarnings(value = Array(S)) def b = 1
       |  @SuppressWarnings(scala.Array(n25.S)) def c = 1
       |  @deprecated(S, S) def d = 1
       |  @SerialVersionUID(L) class X
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n25 <= _empty_/n25.
             |[1:12..1:13): S <= _empty_/n25.S.
             |[2:12..2:13): L <= _empty_/n25.L.
             |[3:3..3:19): SuppressWarnings => java/lang/SuppressWarnings#
             |[3:39..3:40): a <= _empty_/n25.a().
             |[4:3..4:7): java => java/
             |[4:8..4:12): lang => java/lang/
             |[4:13..4:29): SuppressWarnings => java/lang/SuppressWarnings#
             |[4:30..4:35): value => java/lang/SuppressWarnings#value().
             |[4:52..4:53): b <= _empty_/n25.b().
             |[5:3..5:19): SuppressWarnings => java/lang/SuppressWarnings#
             |[5:20..5:25): scala => scala/
             |[5:26..5:31): Array => scala/Array.
             |[5:32..5:35): n25 => _empty_/n25.
             |[5:36..5:37): S => _empty_/n25.S.
             |[5:44..5:45): c <= _empty_/n25.c().
             |[6:3..6:13): deprecated => scala/deprecated#
             |[6:13..6:13):  => scala/deprecated#`<init>`().
             |[6:14..6:15): S => _empty_/n25.S.
             |[6:17..6:18): S => _empty_/n25.S.
             |[6:24..6:25): d <= _empty_/n25.d().
             |[7:3..7:19): SerialVersionUID => scala/SerialVersionUID#
             |[7:19..7:19):  => scala/SerialVersionUID#`<init>`().
             |[7:29..7:30): X <= _empty_/n25.X#
             |[7:30..7:30):  <= _empty_/n25.X#`<init>`().
             |""".stripMargin
      },
      """|[0:7..0:10): n25 <= _empty_/n25.
         |[1:12..1:13): S <= _empty_/n25.S.
         |[2:12..2:13): L <= _empty_/n25.L.
         |[3:3..3:19): SuppressWarnings => java/lang/SuppressWarnings#
         |[3:39..3:40): a <= _empty_/n25.a().
         |[4:3..4:7): java => java/
         |[4:8..4:12): lang => java/lang/
         |[4:13..4:29): SuppressWarnings => java/lang/SuppressWarnings#
         |[4:30..4:35): value => java/lang/SuppressWarnings#value().
         |[4:52..4:53): b <= _empty_/n25.b().
         |[5:3..5:19): SuppressWarnings => java/lang/SuppressWarnings#
         |[5:20..5:25): scala => scala/
         |[5:26..5:31): Array => scala/Array.
         |[5:32..5:35): n25 => _empty_/n25.
         |[5:36..5:37): S => _empty_/n25.S.
         |[5:44..5:45): c <= _empty_/n25.c().
         |[6:3..6:13): deprecated => scala/deprecated#
         |[6:13..6:13):  => scala/deprecated#`<init>`().
         |[6:14..6:15): S => _empty_/n25.S.
         |[6:17..6:18): S => _empty_/n25.S.
         |[6:24..6:25): d <= _empty_/n25.d().
         |[7:3..7:19): SerialVersionUID => scala/SerialVersionUID#
         |[7:19..7:19):  => scala/SerialVersionUID#`<init>`().
         |[7:20..7:21): L => _empty_/n25.L.
         |[7:29..7:30): X <= _empty_/n25.X#
         |[7:30..7:30):  <= _empty_/n25.X#`<init>`().
         |""".stripMargin,
    ),
  )

  private val n77 =
    """|import issue1672._
       |object n77 {
       |  final val S = "a"
       |  final val I = 1
       |  class Q
       |  @SuppressWarnings(Array(S)) def a = 1
       |  @Shade(Color.RED) def b = 1
       |  @Shade(issue1672.Color.RED) def c = 1
       |  @Colors(Array(Color.RED)) def d = 1
       |  @Outer(new Inner(S)) def e = 1
       |  @Outer(value = new Inner(value = S)) def f = 1
       |  @Num(I + 1) def g = 1
       |  @Num(n77.I + 1) def h = 1
       |  @SuppressWarnings(Array(S + "x")) def i = 1
       |  @example.Issue4505J.AnnotationWithClass(classOf[String]) def j = 1
       |  @example.Issue4505J.AnnotationWithClass(classOf[Q]) def k = 1
       |  val o = (Array(S), Color.RED, I + 1, n77.I + 1, S + "x", classOf[String], classOf[Q])
       |}
       |""".stripMargin

  occurrences(
    "names in arguments of Java annotations",
    n77,
    """|[0:7..0:16): issue1672 => issue1672/
       |[1:7..1:10): n77 <= _empty_/n77.
       |[2:12..2:13): S <= _empty_/n77.S.
       |[3:12..3:13): I <= _empty_/n77.I.
       |[4:8..4:9): Q <= _empty_/n77.Q#
       |[4:9..4:9):  <= _empty_/n77.Q#`<init>`().
       |[5:3..5:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[5:34..5:35): a <= _empty_/n77.a().
       |[6:3..6:8): Shade => issue1672/Shade#
       |[6:24..6:25): b <= _empty_/n77.b().
       |[7:3..7:8): Shade => issue1672/Shade#
       |[7:9..7:18): issue1672 => issue1672/
       |[7:19..7:24): Color => issue1672/Color#
       |[7:25..7:28): RED => issue1672/Color#RED.
       |[7:34..7:35): c <= _empty_/n77.c().
       |[8:3..8:9): Colors => issue1672/Colors#
       |[8:32..8:33): d <= _empty_/n77.d().
       |[9:3..9:8): Outer => issue1672/Outer#
       |[9:13..9:18): Inner => issue1672/Inner#
       |[9:27..9:28): e <= _empty_/n77.e().
       |[10:3..10:8): Outer => issue1672/Outer#
       |[10:9..10:14): value => issue1672/Outer#value().
       |[10:21..10:26): Inner => issue1672/Inner#
       |[10:27..10:32): value => issue1672/Inner#value().
       |[10:43..10:44): f <= _empty_/n77.f().
       |[11:3..11:6): Num => issue1672/Num#
       |[11:18..11:19): g <= _empty_/n77.g().
       |[12:3..12:6): Num => issue1672/Num#
       |[12:7..12:10): n77 => _empty_/n77.
       |[12:11..12:12): I => _empty_/n77.I.
       |[12:22..12:23): h <= _empty_/n77.h().
       |[13:3..13:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[13:40..13:41): i <= _empty_/n77.i().
       |[14:3..14:10): example => example/
       |[14:11..14:21): Issue4505J => example/Issue4505J#
       |[14:22..14:41): AnnotationWithClass => example/Issue4505J#AnnotationWithClass#
       |[14:63..14:64): j <= _empty_/n77.j().
       |[15:3..15:10): example => example/
       |[15:11..15:21): Issue4505J => example/Issue4505J#
       |[15:22..15:41): AnnotationWithClass => example/Issue4505J#AnnotationWithClass#
       |[15:58..15:59): k <= _empty_/n77.k().
       |[16:6..16:7): o <= _empty_/n77.o.
       |[16:11..16:16): Array => scala/Array.
       |[16:17..16:18): S => _empty_/n77.S.
       |[16:21..16:26): Color => issue1672/Color#
       |[16:27..16:30): RED => issue1672/Color#RED.
       |[16:32..16:33): I => _empty_/n77.I.
       |[16:34..16:35): + => scala/Int#`+`(+4).
       |[16:39..16:42): n77 => _empty_/n77.
       |[16:43..16:44): I => _empty_/n77.I.
       |[16:45..16:46): + => scala/Int#`+`(+4).
       |[16:50..16:51): S => _empty_/n77.S.
       |[16:52..16:53): + => java/lang/String#`+`().
       |[16:59..16:66): classOf => scala/Predef.classOf().
       |[16:67..16:73): String => scala/Predef.String#
       |[16:76..16:83): classOf => scala/Predef.classOf().
       |[16:84..16:85): Q => _empty_/n77.Q#
       |""".stripMargin,
  )

  synthetics(
    "names in arguments of Java annotations",
    n77,
    """|[16:11..16:16): Array => *.apply[String]
       |  apply => scala/Array.apply().
       |  String => java/lang/String#
       |[16:11..16:19): Array(S) => *((`macro-expandee` : ClassTag[String]))
       |  ClassTag => scala/reflect/ClassTag#
       |  String => java/lang/String#
       |""".stripMargin,
  )

  occurrences(
    "places of a Java annotation",
    """|object n78 {
       |  final val S = "a"
       |  @SuppressWarnings(Array(S)) class C(@SuppressWarnings(Array(S)) val p: Int)
       |  @SuppressWarnings(Array(S)) object O
       |  @SuppressWarnings(Array(S)) val v = 1
       |  def m(@SuppressWarnings(Array(S)) x: Int) = x
       |  def t[@SuppressWarnings(Array(S)) T] = 1
       |  def l = { @SuppressWarnings(Array(S)) def k = 1; k }
       |  def u: Int @SuppressWarnings(Array(S)) = 1
       |}
       |""".stripMargin,
    """|[0:7..0:10): n78 <= _empty_/n78.
       |[1:12..1:13): S <= _empty_/n78.S.
       |[2:3..2:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[2:36..2:37): C <= _empty_/n78.C#
       |[2:37..2:37):  <= _empty_/n78.C#`<init>`().
       |[2:39..2:55): SuppressWarnings => java/lang/SuppressWarnings#
       |[2:70..2:71): p <= _empty_/n78.C#`<init>`().(p)
       |[2:70..2:71): p <= _empty_/n78.C#p.
       |[2:73..2:76): Int => scala/Int#
       |[3:3..3:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[3:37..3:38): O <= _empty_/n78.O.
       |[4:3..4:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[4:34..4:35): v <= _empty_/n78.v.
       |[5:6..5:7): m <= _empty_/n78.m().
       |[5:9..5:25): SuppressWarnings => java/lang/SuppressWarnings#
       |[5:36..5:37): x <= _empty_/n78.m().(x)
       |[5:39..5:42): Int => scala/Int#
       |[5:46..5:47): x => _empty_/n78.m().(x)
       |[6:6..6:7): t <= _empty_/n78.t().
       |[6:9..6:25): SuppressWarnings => java/lang/SuppressWarnings#
       |[6:36..6:37): T <= _empty_/n78.t().[T]
       |[7:6..7:7): l <= _empty_/n78.l().
       |[7:13..7:29): SuppressWarnings => java/lang/SuppressWarnings#
       |[7:44..7:45): k <= local0
       |[7:51..7:52): k => local0
       |[8:6..8:7): u <= _empty_/n78.u().
       |[8:9..8:12): Int => scala/Int#
       |[8:14..8:30): SuppressWarnings => java/lang/SuppressWarnings#
       |""".stripMargin,
  )

  private val n79 =
    """|object n79a { final val S = "a"; final val T = "b" }
       |object n79 {
       |  import n79a.{S => R, T}
       |  @SuppressWarnings(Array(R, T)) def a = 1
       |  val o = Array(R, T)
       |}
       |""".stripMargin

  occurrences(
    "renamed imports in arguments of Java annotations",
    n79,
    """|[0:7..0:11): n79a <= _empty_/n79a.
       |[0:24..0:25): S <= _empty_/n79a.S.
       |[0:43..0:44): T <= _empty_/n79a.T.
       |[1:7..1:10): n79 <= _empty_/n79.
       |[2:9..2:13): n79a => _empty_/n79a.
       |[2:15..2:16): S => _empty_/n79a.S.
       |[2:23..2:24): T => _empty_/n79a.T.
       |[3:3..3:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[3:37..3:38): a <= _empty_/n79.a().
       |[4:6..4:7): o <= _empty_/n79.o.
       |[4:10..4:15): Array => scala/Array.
       |[4:16..4:17): R => _empty_/n79a.S.
       |[4:19..4:20): T => _empty_/n79a.T.
       |""".stripMargin,
  )

  synthetics(
    "renamed imports in arguments of Java annotations",
    n79,
    """|[4:10..4:15): Array => *.apply[String]
       |  apply => scala/Array.apply().
       |  String => java/lang/String#
       |[4:10..4:21): Array(R, T) => *((`macro-expandee` : ClassTag[String]))
       |  ClassTag => scala/reflect/ClassTag#
       |  String => java/lang/String#
       |""".stripMargin,
  )

  occurrences(
    "constants in a Scala classfile annotation",
    """|object n80 {
       |  final val L = 1L
       |  @SerialVersionUID(value = L) class X
       |  @SerialVersionUID(n80.L) class Y
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n80 <= _empty_/n80.
             |[1:12..1:13): L <= _empty_/n80.L.
             |[2:3..2:19): SerialVersionUID => scala/SerialVersionUID#
             |[2:19..2:19):  => scala/SerialVersionUID#`<init>`().
             |[2:20..2:25): value => scala/SerialVersionUID#`<init>`().(value)
             |[2:37..2:38): X <= _empty_/n80.X#
             |[2:38..2:38):  <= _empty_/n80.X#`<init>`().
             |[3:3..3:19): SerialVersionUID => scala/SerialVersionUID#
             |[3:19..3:19):  => scala/SerialVersionUID#`<init>`().
             |[3:20..3:23): n80 => _empty_/n80.
             |[3:24..3:25): L => _empty_/n80.L.
             |[3:33..3:34): Y <= _empty_/n80.Y#
             |[3:34..3:34):  <= _empty_/n80.Y#`<init>`().
             |""".stripMargin
      },
      """|[0:7..0:10): n80 <= _empty_/n80.
         |[1:12..1:13): L <= _empty_/n80.L.
         |[2:3..2:19): SerialVersionUID => scala/SerialVersionUID#
         |[2:19..2:19):  => scala/SerialVersionUID#`<init>`().
         |[2:20..2:25): value => scala/SerialVersionUID#`<init>`().(value)
         |[2:28..2:29): L => _empty_/n80.L.
         |[2:37..2:38): X <= _empty_/n80.X#
         |[2:38..2:38):  <= _empty_/n80.X#`<init>`().
         |[3:3..3:19): SerialVersionUID => scala/SerialVersionUID#
         |[3:19..3:19):  => scala/SerialVersionUID#`<init>`().
         |[3:20..3:23): n80 => _empty_/n80.
         |[3:24..3:25): L => _empty_/n80.L.
         |[3:33..3:34): Y <= _empty_/n80.Y#
         |[3:34..3:34):  <= _empty_/n80.Y#`<init>`().
         |""".stripMargin,
    ),
  )

  occurrences(
    "nested Java annotations in an array and references to elements",
    """|import issue1672._
       |object n81 {
       |  final val S = "a"
       |  @Inners(Array(new Inner(S), new Inner(value = "b"))) def a = 1
       |  def b(s: SuppressWarnings) = s.value
       |  def c(o: Outer) = o.value.value()
       |}
       |""".stripMargin,
    """|[0:7..0:16): issue1672 => issue1672/
       |[1:7..1:10): n81 <= _empty_/n81.
       |[2:12..2:13): S <= _empty_/n81.S.
       |[3:3..3:9): Inners => issue1672/Inners#
       |[3:20..3:25): Inner => issue1672/Inner#
       |[3:34..3:39): Inner => issue1672/Inner#
       |[3:40..3:45): value => issue1672/Inner#value().
       |[3:59..3:60): a <= _empty_/n81.a().
       |[4:6..4:7): b <= _empty_/n81.b().
       |[4:8..4:9): s <= _empty_/n81.b().(s)
       |[4:11..4:27): SuppressWarnings => java/lang/SuppressWarnings#
       |[4:31..4:32): s => _empty_/n81.b().(s)
       |[4:33..4:38): value => java/lang/SuppressWarnings#value().
       |[5:6..5:7): c <= _empty_/n81.c().
       |[5:8..5:9): o <= _empty_/n81.c().(o)
       |[5:11..5:16): Outer => issue1672/Outer#
       |[5:20..5:21): o => _empty_/n81.c().(o)
       |[5:22..5:27): value => issue1672/Outer#value().
       |[5:28..5:33): value => issue1672/Inner#value().
       |""".stripMargin,
  )

  occurrences(
    "Java annotations in blocks of a template body",
    """|object n82 {
       |  final val S = "a"
       |  def t(n: String)(b: => Int) = b
       |  locally { @SuppressWarnings(Array(S)) def k = 1; k }
       |  t("x") { @SuppressWarnings(Array(S)) def k = 1; k }
       |}
       |""".stripMargin,
    """|[0:7..0:10): n82 <= _empty_/n82.
       |[1:12..1:13): S <= _empty_/n82.S.
       |[2:6..2:7): t <= _empty_/n82.t().
       |[2:8..2:9): n <= _empty_/n82.t().(n)
       |[2:11..2:17): String => scala/Predef.String#
       |[2:19..2:20): b <= _empty_/n82.t().(b)
       |[2:25..2:28): Int => scala/Int#
       |[2:32..2:33): b => _empty_/n82.t().(b)
       |[3:2..3:9): locally => scala/Predef.locally().
       |[3:13..3:29): SuppressWarnings => java/lang/SuppressWarnings#
       |[3:44..3:45): k <= local0
       |[3:51..3:52): k => local0
       |[4:2..4:3): t => _empty_/n82.t().
       |[4:12..4:28): SuppressWarnings => java/lang/SuppressWarnings#
       |[4:43..4:44): k <= local1
       |[4:50..4:51): k => local1
       |""".stripMargin,
  )

  occurrences(
    "a polymorphic Scala annotation without type arguments",
    """|import java.io.IOException
       |object n83 {
       |  @throws(classOf[IOException]) def a = 1
       |  @throws(classOf[java.io.IOException]) def b = 1
       |}
       |""".stripMargin,
    """|[0:7..0:11): java => java/
       |[0:12..0:14): io => java/io/
       |[0:15..0:26): IOException => java/io/IOException#
       |[1:7..1:10): n83 <= _empty_/n83.
       |[2:3..2:9): throws => scala/throws#
       |[2:9..2:9):  => scala/throws#`<init>`(+1).
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:29): IOException => java/io/IOException#
       |[2:36..2:37): a <= _empty_/n83.a().
       |[3:3..3:9): throws => scala/throws#
       |[3:9..3:9):  => scala/throws#`<init>`(+1).
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:22): java => java/
       |[3:23..3:25): io => java/io/
       |[3:26..3:37): IOException => java/io/IOException#
       |[3:44..3:45): b <= _empty_/n83.b().
       |""".stripMargin,
  )

  occurrences(
    "a named argument and a type argument in Java annotations",
    """|object n84 {
       |  final val S = "a"
       |  def u: Int @SuppressWarnings(value = Array(S)) = 1
       |  @SuppressWarnings(Array[String](S)) def a = 1
       |  import issue1672._
       |  def v: Int @Outer(value = new Inner(value = S)) = 1
       |}
       |""".stripMargin,
    """|[0:7..0:10): n84 <= _empty_/n84.
       |[1:12..1:13): S <= _empty_/n84.S.
       |[2:6..2:7): u <= _empty_/n84.u().
       |[2:9..2:12): Int => scala/Int#
       |[2:14..2:30): SuppressWarnings => java/lang/SuppressWarnings#
       |[2:31..2:36): value => java/lang/SuppressWarnings#value().
       |[3:3..3:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[3:42..3:43): a <= _empty_/n84.a().
       |[4:9..4:18): issue1672 => issue1672/
       |[5:6..5:7): v <= _empty_/n84.v().
       |[5:9..5:12): Int => scala/Int#
       |[5:14..5:19): Outer => issue1672/Outer#
       |[5:20..5:25): value => issue1672/Outer#value().
       |[5:32..5:37): Inner => issue1672/Inner#
       |[5:38..5:43): value => issue1672/Inner#value().
       |""".stripMargin,
  )

  private val n85 =
    """|object n85 {
       |  final val S = "a"
       |  class C[T](x: T)
       |  @SuppressWarnings(Array(S)) def a = new C(new C(new C(1)))
       |  val b = new n85.C(new C[C[Array[String]]](new C(Array(S))))
       |}
       |""".stripMargin

  occurrences(
    "nested constructor calls beside a Java annotation",
    n85,
    """|[0:7..0:10): n85 <= _empty_/n85.
       |[1:12..1:13): S <= _empty_/n85.S.
       |[2:8..2:9): C <= _empty_/n85.C#
       |[2:10..2:11): T <= _empty_/n85.C#[T]
       |[2:12..2:12):  <= _empty_/n85.C#`<init>`().
       |[2:13..2:14): x <= _empty_/n85.C#`<init>`().(x)
       |[2:13..2:14): x <= _empty_/n85.C#x.
       |[2:16..2:17): T => _empty_/n85.C#[T]
       |[3:3..3:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[3:34..3:35): a <= _empty_/n85.a().
       |[3:42..3:43): C => _empty_/n85.C#
       |[3:43..3:43):  => _empty_/n85.C#`<init>`().
       |[3:48..3:49): C => _empty_/n85.C#
       |[3:49..3:49):  => _empty_/n85.C#`<init>`().
       |[3:54..3:55): C => _empty_/n85.C#
       |[3:55..3:55):  => _empty_/n85.C#`<init>`().
       |[4:6..4:7): b <= _empty_/n85.b.
       |[4:14..4:17): n85 => _empty_/n85.
       |[4:18..4:19): C => _empty_/n85.C#
       |[4:19..4:19):  => _empty_/n85.C#`<init>`().
       |[4:24..4:25): C => _empty_/n85.C#
       |[4:26..4:27): C => _empty_/n85.C#
       |[4:28..4:33): Array => scala/Array#
       |[4:34..4:40): String => scala/Predef.String#
       |[4:43..4:43):  => _empty_/n85.C#`<init>`().
       |[4:48..4:49): C => _empty_/n85.C#
       |[4:49..4:49):  => _empty_/n85.C#`<init>`().
       |[4:50..4:55): Array => scala/Array.
       |[4:56..4:57): S => _empty_/n85.S.
       |""".stripMargin,
  )

  synthetics(
    "nested constructor calls beside a Java annotation",
    n85,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[4:50..4:55): Array => *.apply[String]
             |  apply => scala/Array.apply().
             |  String => java/lang/String#
             |[4:50..4:58): Array(S) => *((`macro-expandee` : ClassTag[String]))
             |  ClassTag => scala/reflect/ClassTag#
             |  String => java/lang/String#
             |""".stripMargin
      },
      """|[4:50..4:55): Array => *.apply[String]
         |  apply => scala/Array.apply().
         |  String => scala/Predef.String#
         |[4:50..4:58): Array(S) => *((`macro-expandee` : ClassTag[String]))
         |  ClassTag => scala/reflect/ClassTag#
         |  String => scala/Predef.String#
         |""".stripMargin,
    ),
  )

  occurrences(
    "a Java annotation on a case class parameter",
    """|object n86 {
       |  final val S = "a"
       |  case class K(@SuppressWarnings(Array(S)) p: Int)
       |}
       |""".stripMargin,
    """|[0:7..0:10): n86 <= _empty_/n86.
       |[1:12..1:13): S <= _empty_/n86.S.
       |[2:13..2:14): K <= _empty_/n86.K#
       |[2:14..2:14):  <= _empty_/n86.K#`<init>`().
       |[2:16..2:32): SuppressWarnings => java/lang/SuppressWarnings#
       |[2:43..2:44): p <= _empty_/n86.K#`<init>`().(p)
       |[2:43..2:44): p <= _empty_/n86.K#p.
       |[2:46..2:49): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "string interpolators on constants",
    """|object n26 {
       |  final val K = 1
       |  val a = s"a"
       |  val b = s"$K"
       |  val c = raw"a"
       |  val d = s"${K + 1}"
       |}
       |""".stripMargin,
    """|[0:7..0:10): n26 <= _empty_/n26.
       |[1:12..1:13): K <= _empty_/n26.K.
       |[2:6..2:7): a <= _empty_/n26.a.
       |[2:10..2:11): s => scala/StringContext#s().
       |[3:6..3:7): b <= _empty_/n26.b.
       |[3:10..3:11): s => scala/StringContext#s().
       |[3:13..3:14): K => _empty_/n26.K.
       |[4:6..4:7): c <= _empty_/n26.c.
       |[4:10..4:13): raw => scala/StringContext#raw().
       |[5:6..5:7): d <= _empty_/n26.d.
       |[5:10..5:11): s => scala/StringContext#s().
       |[5:14..5:15): K => _empty_/n26.K.
       |[5:16..5:17): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  occurrences(
    "`List`, `Seq` and `Nil` through `_root_` and imports of the scala package",
    """|object n27 {
       |  val a = _root_.scala.List(1)
       |  val b = _root_.scala.Seq(1)
       |  val c = _root_.scala.Nil
       |  val d = _root_.scala.List()
       |  val e = scala.Seq()
       |  val f = scala.`package`.List(1)
       |  val g = scala.`package`.Nil
       |  object i1 { import scala.List; val a = List(1); val b = List }
       |  object i2 { import scala.Seq; val a = Seq(1); val b = Seq }
       |  object i3 { import scala.Nil; val a = Nil }
       |  object i4 { import scala._; val a = List(1); val b = Seq(1); val c = Nil }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n27 <= _empty_/n27.
             |[1:6..1:7): a <= _empty_/n27.a.
             |[1:17..1:22): scala => scala/
             |[1:23..1:27): List => scala/collection/immutable/List.
             |[2:6..2:7): b <= _empty_/n27.b.
             |[2:17..2:22): scala => scala/
             |[2:23..2:26): Seq => scala/collection/Seq.
             |[3:6..3:7): c <= _empty_/n27.c.
             |[3:17..3:22): scala => scala/
             |[3:23..3:26): Nil => scala/collection/immutable/Nil.
             |[4:6..4:7): d <= _empty_/n27.d.
             |[4:23..4:27): List => scala/collection/immutable/Nil.
             |[5:6..5:7): e <= _empty_/n27.e.
             |[5:10..5:15): scala => scala/
             |[5:16..5:19): Seq => scala/collection/Seq.
             |[6:6..6:7): f <= _empty_/n27.f.
             |[6:10..6:15): scala => scala/
             |[6:16..6:25): `package` => scala/package.
             |[6:26..6:30): List => scala/package.List.
             |[7:6..7:7): g <= _empty_/n27.g.
             |[7:10..7:15): scala => scala/
             |[7:16..7:25): `package` => scala/package.
             |[7:26..7:29): Nil => scala/package.Nil.
             |[8:9..8:11): i1 <= _empty_/n27.i1.
             |[8:21..8:26): scala => scala/
             |[8:27..8:31): List => scala/package.List#
             |[8:27..8:31): List => scala/package.List.
             |[8:37..8:38): a <= _empty_/n27.i1.a.
             |[8:41..8:45): List => scala/collection/immutable/List.
             |[8:54..8:55): b <= _empty_/n27.i1.b.
             |[8:58..8:62): List => scala/collection/immutable/List.
             |[9:9..9:11): i2 <= _empty_/n27.i2.
             |[9:21..9:26): scala => scala/
             |[9:27..9:30): Seq => scala/package.Seq#
             |[9:27..9:30): Seq => scala/package.Seq.
             |[9:36..9:37): a <= _empty_/n27.i2.a.
             |[9:40..9:43): Seq => scala/collection/Seq.
             |[9:52..9:53): b <= _empty_/n27.i2.b.
             |[9:56..9:59): Seq => scala/collection/Seq.
             |[10:9..10:11): i3 <= _empty_/n27.i3.
             |[10:21..10:26): scala => scala/
             |[10:27..10:30): Nil => scala/package.Nil.
             |[10:36..10:37): a <= _empty_/n27.i3.a.
             |[10:40..10:43): Nil => scala/collection/immutable/Nil.
             |[11:9..11:11): i4 <= _empty_/n27.i4.
             |[11:21..11:26): scala => scala/
             |[11:34..11:35): a <= _empty_/n27.i4.a.
             |[11:38..11:42): List => scala/collection/immutable/List.
             |[11:51..11:52): b <= _empty_/n27.i4.b.
             |[11:55..11:58): Seq => scala/collection/Seq.
             |[11:67..11:68): c <= _empty_/n27.i4.c.
             |[11:71..11:74): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n27 <= _empty_/n27.
         |[1:6..1:7): a <= _empty_/n27.a.
         |[1:17..1:22): scala => scala/
         |[1:23..1:27): List => scala/package.List.
         |[2:6..2:7): b <= _empty_/n27.b.
         |[2:17..2:22): scala => scala/
         |[2:23..2:26): Seq => scala/package.Seq.
         |[3:6..3:7): c <= _empty_/n27.c.
         |[3:17..3:22): scala => scala/
         |[3:23..3:26): Nil => scala/package.Nil.
         |[4:6..4:7): d <= _empty_/n27.d.
         |[4:23..4:27): List => scala/collection/immutable/Nil.
         |[5:6..5:7): e <= _empty_/n27.e.
         |[5:10..5:15): scala => scala/
         |[5:16..5:19): Seq => scala/package.Seq.
         |[6:6..6:7): f <= _empty_/n27.f.
         |[6:10..6:15): scala => scala/
         |[6:16..6:25): `package` => scala/package.
         |[6:26..6:30): List => scala/package.List.
         |[7:6..7:7): g <= _empty_/n27.g.
         |[7:10..7:15): scala => scala/
         |[7:16..7:25): `package` => scala/package.
         |[7:26..7:29): Nil => scala/package.Nil.
         |[8:9..8:11): i1 <= _empty_/n27.i1.
         |[8:21..8:26): scala => scala/
         |[8:27..8:31): List => scala/package.List#
         |[8:27..8:31): List => scala/package.List.
         |[8:37..8:38): a <= _empty_/n27.i1.a.
         |[8:41..8:45): List => scala/package.List.
         |[8:54..8:55): b <= _empty_/n27.i1.b.
         |[8:58..8:62): List => scala/package.List.
         |[9:9..9:11): i2 <= _empty_/n27.i2.
         |[9:21..9:26): scala => scala/
         |[9:27..9:30): Seq => scala/package.Seq#
         |[9:27..9:30): Seq => scala/package.Seq.
         |[9:36..9:37): a <= _empty_/n27.i2.a.
         |[9:40..9:43): Seq => scala/package.Seq.
         |[9:52..9:53): b <= _empty_/n27.i2.b.
         |[9:56..9:59): Seq => scala/package.Seq.
         |[10:9..10:11): i3 <= _empty_/n27.i3.
         |[10:21..10:26): scala => scala/
         |[10:27..10:30): Nil => scala/package.Nil.
         |[10:36..10:37): a <= _empty_/n27.i3.a.
         |[10:40..10:43): Nil => scala/package.Nil.
         |[11:9..11:11): i4 <= _empty_/n27.i4.
         |[11:21..11:26): scala => scala/
         |[11:34..11:35): a <= _empty_/n27.i4.a.
         |[11:38..11:42): List => scala/package.List.
         |[11:51..11:52): b <= _empty_/n27.i4.b.
         |[11:55..11:58): Seq => scala/package.Seq.
         |[11:67..11:68): c <= _empty_/n27.i4.c.
         |[11:71..11:74): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "renamed imports of `List`, `Seq` and `Nil`",
    """|object n28 {
       |  import scala.{List => L, Seq => S, Nil => N}
       |  val a = L(1)
       |  val b = S(1)
       |  val c = N
       |  val d = L
       |  val e = L.empty[Int]
       |  val f = L()
       |  def g(xs: List[Int]) = xs match {
       |    case L(x) => x
       |    case N => 0
       |    case x :: N => x
       |    case _ => 1
       |  }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n28 <= _empty_/n28.
             |[1:9..1:14): scala => scala/
             |[1:16..1:20): List => scala/package.List#
             |[1:16..1:20): List => scala/package.List.
             |[1:27..1:30): Seq => scala/package.Seq#
             |[1:27..1:30): Seq => scala/package.Seq.
             |[1:37..1:40): Nil => scala/package.Nil.
             |[2:6..2:7): a <= _empty_/n28.a.
             |[2:10..2:11): L => scala/collection/immutable/List.
             |[3:6..3:7): b <= _empty_/n28.b.
             |[3:10..3:11): S => scala/collection/Seq.
             |[4:6..4:7): c <= _empty_/n28.c.
             |[4:10..4:11): N => scala/collection/immutable/Nil.
             |[5:6..5:7): d <= _empty_/n28.d.
             |[5:10..5:11): L => scala/collection/immutable/List.
             |[6:6..6:7): e <= _empty_/n28.e.
             |[6:10..6:11): L => scala/collection/immutable/List.
             |[6:12..6:17): empty => scala/collection/immutable/List.empty().
             |[6:18..6:21): Int => scala/Int#
             |[7:6..7:7): f <= _empty_/n28.f.
             |[7:10..7:11): L => scala/collection/immutable/Nil.
             |[8:6..8:7): g <= _empty_/n28.g().
             |[8:8..8:10): xs <= _empty_/n28.g().(xs)
             |[8:12..8:16): List => scala/package.List#
             |[8:17..8:20): Int => scala/Int#
             |[8:25..8:27): xs => _empty_/n28.g().(xs)
             |[9:9..9:10): L => scala/collection/immutable/List.
             |[9:11..9:12): x <= local0
             |[9:17..9:18): x => local0
             |[10:9..10:10): N => scala/collection/immutable/Nil.
             |[11:9..11:10): x <= local1
             |[11:11..11:13): :: => scala/package.`::`.
             |[11:14..11:15): N => scala/collection/immutable/Nil.
             |[11:19..11:20): x => local1
             |""".stripMargin
      },
      """|[0:7..0:10): n28 <= _empty_/n28.
         |[1:9..1:14): scala => scala/
         |[1:16..1:20): List => scala/package.List#
         |[1:16..1:20): List => scala/package.List.
         |[1:27..1:30): Seq => scala/package.Seq#
         |[1:27..1:30): Seq => scala/package.Seq.
         |[1:37..1:40): Nil => scala/package.Nil.
         |[2:6..2:7): a <= _empty_/n28.a.
         |[2:10..2:11): L => scala/package.List.
         |[3:6..3:7): b <= _empty_/n28.b.
         |[3:10..3:11): S => scala/package.Seq.
         |[4:6..4:7): c <= _empty_/n28.c.
         |[4:10..4:11): N => scala/package.Nil.
         |[5:6..5:7): d <= _empty_/n28.d.
         |[5:10..5:11): L => scala/package.List.
         |[6:6..6:7): e <= _empty_/n28.e.
         |[6:10..6:11): L => scala/package.List.
         |[6:12..6:17): empty => scala/collection/immutable/List.empty().
         |[6:18..6:21): Int => scala/Int#
         |[7:6..7:7): f <= _empty_/n28.f.
         |[7:10..7:11): L => scala/collection/immutable/Nil.
         |[8:6..8:7): g <= _empty_/n28.g().
         |[8:8..8:10): xs <= _empty_/n28.g().(xs)
         |[8:12..8:16): List => scala/package.List#
         |[8:17..8:20): Int => scala/Int#
         |[8:25..8:27): xs => _empty_/n28.g().(xs)
         |[9:9..9:10): L => scala/package.List.
         |[9:11..9:12): x <= local0
         |[9:17..9:18): x => local0
         |[10:9..10:10): N => scala/package.Nil.
         |[11:9..11:10): x <= local1
         |[11:11..11:13): :: => scala/package.`::`.
         |[11:14..11:15): N => scala/package.Nil.
         |[11:19..11:20): x => local1
         |""".stripMargin,
    ),
  )

  occurrences(
    "`List`, `Seq` and `Nil` through their own objects",
    """|object n29 {
       |  val a = scala.collection.immutable.List(1)
       |  val b = scala.collection.Seq(1)
       |  val c = scala.collection.immutable.List.empty[Int]
       |  object i1 {
       |    import scala.collection.immutable.{List, Nil}
       |    import scala.collection.Seq
       |    val a = List(1)
       |    val b = Seq(1)
       |    val c = Nil
       |    val d = List.empty[Int]
       |    def f(xs: List[Int]) = xs match { case List(x) => x; case Nil => 0; case _ => 1 }
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:10): n29 <= _empty_/n29.
       |[1:6..1:7): a <= _empty_/n29.a.
       |[1:10..1:15): scala => scala/
       |[1:16..1:26): collection => scala/collection/
       |[1:27..1:36): immutable => scala/collection/immutable/
       |[1:37..1:41): List => scala/collection/immutable/List.
       |[2:6..2:7): b <= _empty_/n29.b.
       |[2:10..2:15): scala => scala/
       |[2:16..2:26): collection => scala/collection/
       |[2:27..2:30): Seq => scala/collection/Seq.
       |[3:6..3:7): c <= _empty_/n29.c.
       |[3:10..3:15): scala => scala/
       |[3:16..3:26): collection => scala/collection/
       |[3:27..3:36): immutable => scala/collection/immutable/
       |[3:37..3:41): List => scala/collection/immutable/List.
       |[3:42..3:47): empty => scala/collection/immutable/List.empty().
       |[3:48..3:51): Int => scala/Int#
       |[4:9..4:11): i1 <= _empty_/n29.i1.
       |[5:11..5:16): scala => scala/
       |[5:17..5:27): collection => scala/collection/
       |[5:28..5:37): immutable => scala/collection/immutable/
       |[5:39..5:43): List => scala/collection/immutable/List#
       |[5:39..5:43): List => scala/collection/immutable/List.
       |[5:45..5:48): Nil => scala/collection/immutable/Nil.
       |[6:11..6:16): scala => scala/
       |[6:17..6:27): collection => scala/collection/
       |[6:28..6:31): Seq => scala/collection/Seq#
       |[6:28..6:31): Seq => scala/collection/Seq.
       |[7:8..7:9): a <= _empty_/n29.i1.a.
       |[7:12..7:16): List => scala/collection/immutable/List.
       |[8:8..8:9): b <= _empty_/n29.i1.b.
       |[8:12..8:15): Seq => scala/collection/Seq.
       |[9:8..9:9): c <= _empty_/n29.i1.c.
       |[9:12..9:15): Nil => scala/collection/immutable/Nil.
       |[10:8..10:9): d <= _empty_/n29.i1.d.
       |[10:12..10:16): List => scala/collection/immutable/List.
       |[10:17..10:22): empty => scala/collection/immutable/List.empty().
       |[10:23..10:26): Int => scala/Int#
       |[11:8..11:9): f <= _empty_/n29.i1.f().
       |[11:10..11:12): xs <= _empty_/n29.i1.f().(xs)
       |[11:14..11:18): List => scala/collection/immutable/List#
       |[11:19..11:22): Int => scala/Int#
       |[11:27..11:29): xs => _empty_/n29.i1.f().(xs)
       |[11:43..11:47): List => scala/collection/immutable/List.
       |[11:48..11:49): x <= local0
       |[11:54..11:55): x => local0
       |[11:62..11:65): Nil => scala/collection/immutable/Nil.
       |""".stripMargin,
  )

  occurrences(
    "user objects named `List`, `Seq` and `Nil`",
    """|object n30 {
       |  object List { def apply(i: Int) = i; def empty = 0; def unapply(i: Int) = Some(i) }
       |  object Seq { def apply(i: Int) = i }
       |  object Nil
       |  val a = List(1)
       |  val b = Seq(1)
       |  val c = Nil
       |  val d = List.empty
       |  val e = List
       |  def f(i: Int) = i match { case List(x) => x }
       |  def g(x: Any) = x match { case Nil => 0; case _ => 1 }
       |}
       |""".stripMargin,
    """|[0:7..0:10): n30 <= _empty_/n30.
       |[1:9..1:13): List <= _empty_/n30.List.
       |[1:20..1:25): apply <= _empty_/n30.List.apply().
       |[1:26..1:27): i <= _empty_/n30.List.apply().(i)
       |[1:29..1:32): Int => scala/Int#
       |[1:36..1:37): i => _empty_/n30.List.apply().(i)
       |[1:43..1:48): empty <= _empty_/n30.List.empty().
       |[1:58..1:65): unapply <= _empty_/n30.List.unapply().
       |[1:66..1:67): i <= _empty_/n30.List.unapply().(i)
       |[1:69..1:72): Int => scala/Int#
       |[1:76..1:80): Some => scala/Some.
       |[1:81..1:82): i => _empty_/n30.List.unapply().(i)
       |[2:9..2:12): Seq <= _empty_/n30.Seq.
       |[2:19..2:24): apply <= _empty_/n30.Seq.apply().
       |[2:25..2:26): i <= _empty_/n30.Seq.apply().(i)
       |[2:28..2:31): Int => scala/Int#
       |[2:35..2:36): i => _empty_/n30.Seq.apply().(i)
       |[3:9..3:12): Nil <= _empty_/n30.Nil.
       |[4:6..4:7): a <= _empty_/n30.a.
       |[4:10..4:14): List => _empty_/n30.List.
       |[5:6..5:7): b <= _empty_/n30.b.
       |[5:10..5:13): Seq => _empty_/n30.Seq.
       |[6:6..6:7): c <= _empty_/n30.c.
       |[6:10..6:13): Nil => _empty_/n30.Nil.
       |[7:6..7:7): d <= _empty_/n30.d.
       |[7:10..7:14): List => _empty_/n30.List.
       |[7:15..7:20): empty => _empty_/n30.List.empty().
       |[8:6..8:7): e <= _empty_/n30.e.
       |[8:10..8:14): List => _empty_/n30.List.
       |[9:6..9:7): f <= _empty_/n30.f().
       |[9:8..9:9): i <= _empty_/n30.f().(i)
       |[9:11..9:14): Int => scala/Int#
       |[9:18..9:19): i => _empty_/n30.f().(i)
       |[9:33..9:37): List => _empty_/n30.List.
       |[9:38..9:39): x <= local0
       |[9:44..9:45): x => local0
       |[10:6..10:7): g <= _empty_/n30.g().
       |[10:8..10:9): x <= _empty_/n30.g().(x)
       |[10:11..10:14): Any => scala/Any#
       |[10:18..10:19): x => _empty_/n30.g().(x)
       |[10:33..10:36): Nil => _empty_/n30.Nil.
       |""".stripMargin,
  )

  occurrences(
    "members of `List`, `Seq` and `Nil`",
    """|object n31 {
       |  val a = Seq.empty[Int]
       |  val b = Nil.length
       |  val c = scala.List.empty[Int]
       |  val d = List.fill(1)(1)
       |  val e = Seq.apply(1)
       |  val f = List.apply(1)
       |  val g = 1 :: Nil
       |  val h = Nil.::(1)
       |  val i = scala.Nil.isEmpty
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n31 <= _empty_/n31.
             |[1:6..1:7): a <= _empty_/n31.a.
             |[1:10..1:13): Seq => scala/collection/Seq.
             |[1:14..1:19): empty => scala/collection/generic/GenericCompanion#empty().
             |[1:20..1:23): Int => scala/Int#
             |[2:6..2:7): b <= _empty_/n31.b.
             |[2:10..2:13): Nil => scala/collection/immutable/Nil.
             |[2:14..2:20): length => scala/collection/LinearSeqOptimized#length().
             |[3:6..3:7): c <= _empty_/n31.c.
             |[3:10..3:15): scala => scala/
             |[3:16..3:20): List => scala/collection/immutable/List.
             |[3:21..3:26): empty => scala/collection/immutable/List.empty().
             |[3:27..3:30): Int => scala/Int#
             |[4:6..4:7): d <= _empty_/n31.d.
             |[4:10..4:14): List => scala/collection/immutable/List.
             |[4:15..4:19): fill => scala/collection/generic/GenTraversableFactory#fill().
             |[5:6..5:7): e <= _empty_/n31.e.
             |[5:10..5:13): Seq => scala/collection/Seq.
             |[5:14..5:19): apply => scala/collection/generic/GenericCompanion#apply().
             |[6:6..6:7): f <= _empty_/n31.f.
             |[6:10..6:14): List => scala/collection/immutable/List.
             |[6:15..6:20): apply => scala/collection/immutable/List.apply().
             |[7:6..7:7): g <= _empty_/n31.g.
             |[7:12..7:14): :: => scala/collection/immutable/List#`::`().
             |[7:15..7:18): Nil => scala/collection/immutable/Nil.
             |[8:6..8:7): h <= _empty_/n31.h.
             |[8:10..8:13): Nil => scala/collection/immutable/Nil.
             |[8:14..8:16): :: => scala/collection/immutable/List#`::`().
             |[9:6..9:7): i <= _empty_/n31.i.
             |[9:10..9:15): scala => scala/
             |[9:16..9:19): Nil => scala/collection/immutable/Nil.
             |[9:20..9:27): isEmpty => scala/collection/immutable/Nil.isEmpty().
             |""".stripMargin
      },
      """|[0:7..0:10): n31 <= _empty_/n31.
         |[1:6..1:7): a <= _empty_/n31.a.
         |[1:10..1:13): Seq => scala/package.Seq.
         |[1:14..1:19): empty => scala/collection/SeqFactory.Delegate#empty().
         |[1:20..1:23): Int => scala/Int#
         |[2:6..2:7): b <= _empty_/n31.b.
         |[2:10..2:13): Nil => scala/package.Nil.
         |[2:14..2:20): length => scala/collection/immutable/List#length().
         |[3:6..3:7): c <= _empty_/n31.c.
         |[3:10..3:15): scala => scala/
         |[3:16..3:20): List => scala/package.List.
         |[3:21..3:26): empty => scala/collection/immutable/List.empty().
         |[3:27..3:30): Int => scala/Int#
         |[4:6..4:7): d <= _empty_/n31.d.
         |[4:10..4:14): List => scala/package.List.
         |[4:15..4:19): fill => scala/collection/StrictOptimizedSeqFactory#fill().
         |[5:6..5:7): e <= _empty_/n31.e.
         |[5:10..5:13): Seq => scala/package.Seq.
         |[5:14..5:19): apply => scala/collection/SeqFactory.Delegate#apply().
         |[6:6..6:7): f <= _empty_/n31.f.
         |[6:10..6:14): List => scala/package.List.
         |[6:15..6:20): apply => scala/collection/IterableFactory#apply().
         |[7:6..7:7): g <= _empty_/n31.g.
         |[7:12..7:14): :: => scala/collection/immutable/List#`::`().
         |[7:15..7:18): Nil => scala/package.Nil.
         |[8:6..8:7): h <= _empty_/n31.h.
         |[8:10..8:13): Nil => scala/package.Nil.
         |[8:14..8:16): :: => scala/collection/immutable/List#`::`().
         |[9:6..9:7): i <= _empty_/n31.i.
         |[9:10..9:15): scala => scala/
         |[9:16..9:19): Nil => scala/package.Nil.
         |[9:20..9:27): isEmpty => scala/collection/immutable/List#isEmpty().
         |""".stripMargin,
    ),
  )

  occurrences(
    "patterns on `List`, `Seq` and `Nil`",
    """|object n32 {
       |  def f(xs: List[Int]) = xs match {
       |    case List(x) => x
       |    case Nil => 0
       |    case h :: Nil => h
       |    case scala.List(x, _) => x
       |    case _ :: scala.Nil => 1
       |    case scala.Nil => 2
       |    case _ => 3
       |  }
       |  def g(xs: Seq[Int]) = xs match {
       |    case Seq(x) => x
       |    case scala.Seq(x, _) => x
       |    case Seq() => 0
       |    case _ => 1
       |  }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n32 <= _empty_/n32.
             |[1:6..1:7): f <= _empty_/n32.f().
             |[1:8..1:10): xs <= _empty_/n32.f().(xs)
             |[1:12..1:16): List => scala/package.List#
             |[1:17..1:20): Int => scala/Int#
             |[1:25..1:27): xs => _empty_/n32.f().(xs)
             |[2:9..2:13): List => scala/collection/immutable/List.
             |[2:14..2:15): x <= local0
             |[2:20..2:21): x => local0
             |[3:9..3:12): Nil => scala/collection/immutable/Nil.
             |[4:9..4:10): h <= local1
             |[4:11..4:13): :: => scala/package.`::`.
             |[4:14..4:17): Nil => scala/collection/immutable/Nil.
             |[4:21..4:22): h => local1
             |[5:9..5:14): scala => scala/
             |[5:15..5:19): List => scala/collection/immutable/List.
             |[5:20..5:21): x <= local2
             |[5:29..5:30): x => local2
             |[6:11..6:13): :: => scala/package.`::`.
             |[6:14..6:19): scala => scala/
             |[6:20..6:23): Nil => scala/collection/immutable/Nil.
             |[7:9..7:14): scala => scala/
             |[7:15..7:18): Nil => scala/collection/immutable/Nil.
             |[10:6..10:7): g <= _empty_/n32.g().
             |[10:8..10:10): xs <= _empty_/n32.g().(xs)
             |[10:12..10:15): Seq => scala/package.Seq#
             |[10:16..10:19): Int => scala/Int#
             |[10:24..10:26): xs => _empty_/n32.g().(xs)
             |[11:9..11:12): Seq => scala/collection/Seq.
             |[11:13..11:14): x <= local3
             |[11:19..11:20): x => local3
             |[12:9..12:14): scala => scala/
             |[12:15..12:18): Seq => scala/collection/Seq.
             |[12:19..12:20): x <= local4
             |[12:28..12:29): x => local4
             |[13:9..13:12): Seq => scala/collection/Seq.
             |""".stripMargin
      },
      """|[0:7..0:10): n32 <= _empty_/n32.
         |[1:6..1:7): f <= _empty_/n32.f().
         |[1:8..1:10): xs <= _empty_/n32.f().(xs)
         |[1:12..1:16): List => scala/package.List#
         |[1:17..1:20): Int => scala/Int#
         |[1:25..1:27): xs => _empty_/n32.f().(xs)
         |[2:9..2:13): List => scala/package.List.
         |[2:14..2:15): x <= local0
         |[2:20..2:21): x => local0
         |[3:9..3:12): Nil => scala/package.Nil.
         |[4:9..4:10): h <= local1
         |[4:11..4:13): :: => scala/package.`::`.
         |[4:14..4:17): Nil => scala/package.Nil.
         |[4:21..4:22): h => local1
         |[5:9..5:14): scala => scala/
         |[5:15..5:19): List => scala/package.List.
         |[5:20..5:21): x <= local2
         |[5:29..5:30): x => local2
         |[6:11..6:13): :: => scala/package.`::`.
         |[6:14..6:19): scala => scala/
         |[6:20..6:23): Nil => scala/package.Nil.
         |[7:9..7:14): scala => scala/
         |[7:15..7:18): Nil => scala/package.Nil.
         |[10:6..10:7): g <= _empty_/n32.g().
         |[10:8..10:10): xs <= _empty_/n32.g().(xs)
         |[10:12..10:15): Seq => scala/package.Seq#
         |[10:16..10:19): Int => scala/Int#
         |[10:24..10:26): xs => _empty_/n32.g().(xs)
         |[11:9..11:12): Seq => scala/package.Seq.
         |[11:13..11:14): x <= local3
         |[11:19..11:20): x => local3
         |[12:9..12:14): scala => scala/
         |[12:15..12:18): Seq => scala/package.Seq.
         |[12:19..12:20): x <= local4
         |[12:28..12:29): x => local4
         |[13:9..13:12): Seq => scala/package.Seq.
         |""".stripMargin,
    ),
  )

  occurrences(
    "`List`, `Seq` and `Nil` in types",
    """|object n33 {
       |  val a: List.type = List
       |  val b: Seq.type = Seq
       |  val c: Nil.type = Nil
       |  val d: scala.Nil.type = scala.Nil
       |  val e: List[Nil.type] = Nil :: Nil
       |  val f = classOf[Seq[Nil.type]]
       |  val g = implicitly[List.type =:= List.type]
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n33 <= _empty_/n33.
             |[1:6..1:7): a <= _empty_/n33.a.
             |[1:9..1:13): List => scala/collection/immutable/List.
             |[1:21..1:25): List => scala/collection/immutable/List.
             |[2:6..2:7): b <= _empty_/n33.b.
             |[2:9..2:12): Seq => scala/collection/Seq.
             |[2:20..2:23): Seq => scala/collection/Seq.
             |[3:6..3:7): c <= _empty_/n33.c.
             |[3:9..3:12): Nil => scala/collection/immutable/Nil.
             |[3:20..3:23): Nil => scala/collection/immutable/Nil.
             |[4:6..4:7): d <= _empty_/n33.d.
             |[4:9..4:14): scala => scala/
             |[4:15..4:18): Nil => scala/collection/immutable/Nil.
             |[4:26..4:31): scala => scala/
             |[4:32..4:35): Nil => scala/collection/immutable/Nil.
             |[5:6..5:7): e <= _empty_/n33.e.
             |[5:9..5:13): List => scala/package.List#
             |[5:14..5:17): Nil => scala/collection/immutable/Nil.
             |[5:26..5:29): Nil => scala/collection/immutable/Nil.
             |[5:30..5:32): :: => scala/collection/immutable/List#`::`().
             |[5:33..5:36): Nil => scala/collection/immutable/Nil.
             |[6:6..6:7): f <= _empty_/n33.f.
             |[6:10..6:17): classOf => scala/Predef.classOf().
             |[6:18..6:21): Seq => scala/package.Seq#
             |[6:22..6:25): Nil => scala/collection/immutable/Nil.
             |[7:6..7:7): g <= _empty_/n33.g.
             |[7:10..7:20): implicitly => scala/Predef.implicitly().
             |[7:21..7:25): List => scala/collection/immutable/List.
             |[7:31..7:34): =:= => scala/Predef.`=:=`#
             |[7:35..7:39): List => scala/collection/immutable/List.
             |""".stripMargin
      },
      """|[0:7..0:10): n33 <= _empty_/n33.
         |[1:6..1:7): a <= _empty_/n33.a.
         |[1:9..1:13): List => scala/package.List.
         |[1:21..1:25): List => scala/package.List.
         |[2:6..2:7): b <= _empty_/n33.b.
         |[2:9..2:12): Seq => scala/package.Seq.
         |[2:20..2:23): Seq => scala/package.Seq.
         |[3:6..3:7): c <= _empty_/n33.c.
         |[3:9..3:12): Nil => scala/package.Nil.
         |[3:20..3:23): Nil => scala/package.Nil.
         |[4:6..4:7): d <= _empty_/n33.d.
         |[4:9..4:14): scala => scala/
         |[4:15..4:18): Nil => scala/package.Nil.
         |[4:26..4:31): scala => scala/
         |[4:32..4:35): Nil => scala/package.Nil.
         |[5:6..5:7): e <= _empty_/n33.e.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:17): Nil => scala/package.Nil.
         |[5:26..5:29): Nil => scala/package.Nil.
         |[5:30..5:32): :: => scala/collection/immutable/List#`::`().
         |[5:33..5:36): Nil => scala/package.Nil.
         |[6:6..6:7): f <= _empty_/n33.f.
         |[6:10..6:17): classOf => scala/Predef.classOf().
         |[6:18..6:21): Seq => scala/package.Seq#
         |[6:22..6:25): Nil => scala/package.Nil.
         |[7:6..7:7): g <= _empty_/n33.g.
         |[7:10..7:20): implicitly => scala/Predef.implicitly().
         |[7:21..7:25): List => scala/package.List.
         |[7:31..7:34): =:= => scala/`=:=`#
         |[7:35..7:39): List => scala/package.List.
         |""".stripMargin,
    ),
  )

  occurrences(
    "`List`, `Seq` and `Nil` in types without a tree",
    """|object n34 {
       |  trait F[A, B]
       |  val a = classOf[Seq[List.type]]
       |  val b = classOf[Seq[scala.Nil.type]]
       |  def c[A: ({ type L[X] = F[Nil.type, X] })#L] = 1
       |  def d[A <: Nil.type] = 1
       |  val e: { def x: Nil.type } = null
       |  trait E { this: Nil.type => }
       |  val f: PartialFunction[Nil.type, Int] = { case n: Nil.type => 1 }
       |  val g = (n: Nil.type) => n
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n34 <= _empty_/n34.
             |[1:8..1:9): F <= _empty_/n34.F#
             |[1:10..1:11): A <= _empty_/n34.F#[A]
             |[1:13..1:14): B <= _empty_/n34.F#[B]
             |[2:6..2:7): a <= _empty_/n34.a.
             |[2:10..2:17): classOf => scala/Predef.classOf().
             |[2:18..2:21): Seq => scala/package.Seq#
             |[2:22..2:26): List => scala/collection/immutable/List.
             |[3:6..3:7): b <= _empty_/n34.b.
             |[3:10..3:17): classOf => scala/Predef.classOf().
             |[3:18..3:21): Seq => scala/package.Seq#
             |[3:22..3:27): scala => scala/
             |[3:28..3:31): Nil => scala/collection/immutable/Nil.
             |[4:6..4:7): c <= _empty_/n34.c().
             |[4:8..4:9): A <= _empty_/n34.c().[A]
             |[4:11..4:11):  <= _empty_/n34.c().(evidence$1)
             |[4:19..4:20): L <= local0
             |[4:21..4:22): X <= local1
             |[4:26..4:27): F => _empty_/n34.F#
             |[4:28..4:31): Nil => scala/collection/immutable/Nil.
             |[4:38..4:39): X => local1
             |[4:44..4:45): L => local0
             |[5:6..5:7): d <= _empty_/n34.d().
             |[5:8..5:9): A <= _empty_/n34.d().[A]
             |[5:13..5:16): Nil => scala/collection/immutable/Nil.
             |[6:6..6:7): e <= _empty_/n34.e.
             |[6:15..6:16): x <= local2
             |[6:18..6:21): Nil => scala/collection/immutable/Nil.
             |[7:8..7:9): E <= _empty_/n34.E#
             |[7:18..7:21): Nil => scala/collection/immutable/Nil.
             |[8:6..8:7): f <= _empty_/n34.f.
             |[8:9..8:24): PartialFunction => scala/PartialFunction#
             |[8:25..8:28): Nil => scala/collection/immutable/Nil.
             |[8:35..8:38): Int => scala/Int#
             |[8:42..8:42):  <= local3
             |[8:49..8:50): n <= local10
             |[8:52..8:55): Nil => scala/collection/immutable/Nil.
             |[9:6..9:7): g <= _empty_/n34.g.
             |[9:11..9:12): n <= local12
             |[9:14..9:17): Nil => scala/collection/immutable/Nil.
             |[9:27..9:28): n => local12
             |""".stripMargin
      },
      """|[0:7..0:10): n34 <= _empty_/n34.
         |[1:8..1:9): F <= _empty_/n34.F#
         |[1:10..1:11): A <= _empty_/n34.F#[A]
         |[1:13..1:14): B <= _empty_/n34.F#[B]
         |[2:6..2:7): a <= _empty_/n34.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:21): Seq => scala/package.Seq#
         |[2:22..2:26): List => scala/package.List.
         |[3:6..3:7): b <= _empty_/n34.b.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:21): Seq => scala/package.Seq#
         |[3:22..3:27): scala => scala/
         |[3:28..3:31): Nil => scala/package.Nil.
         |[4:6..4:7): c <= _empty_/n34.c().
         |[4:8..4:9): A <= _empty_/n34.c().[A]
         |[4:11..4:11):  <= _empty_/n34.c().(evidence$1)
         |[4:19..4:20): L <= local0
         |[4:21..4:22): X <= local1
         |[4:26..4:27): F => _empty_/n34.F#
         |[4:28..4:31): Nil => scala/package.Nil.
         |[4:38..4:39): X => local1
         |[4:44..4:45): L => local0
         |[5:6..5:7): d <= _empty_/n34.d().
         |[5:8..5:9): A <= _empty_/n34.d().[A]
         |[5:13..5:16): Nil => scala/package.Nil.
         |[6:6..6:7): e <= _empty_/n34.e.
         |[6:15..6:16): x <= local2
         |[6:18..6:21): Nil => scala/package.Nil.
         |[7:8..7:9): E <= _empty_/n34.E#
         |[7:18..7:21): Nil => scala/package.Nil.
         |[8:6..8:7): f <= _empty_/n34.f.
         |[8:9..8:24): PartialFunction => scala/PartialFunction#
         |[8:25..8:28): Nil => scala/package.Nil.
         |[8:35..8:38): Int => scala/Int#
         |[8:42..8:42):  <= local3
         |[8:49..8:50): n <= local10
         |[8:52..8:55): Nil => scala/package.Nil.
         |[9:6..9:7): g <= _empty_/n34.g.
         |[9:11..9:12): n <= local12
         |[9:14..9:17): Nil => scala/package.Nil.
         |[9:27..9:28): n => local12
         |""".stripMargin,
    ),
  )

  if (!ScalaVersion.is212) occurrences(
    "`List`, `Seq` and `Nil` through a renamed scala package",
    """|object n35 {
       |  import _root_.{scala => s}
       |  val a = s.List(1)
       |  val b = s.Seq(1)
       |  val c = s.Nil
       |  val d = s.List.empty[Int]
       |}
       |""".stripMargin,
    """|[0:7..0:10): n35 <= _empty_/n35.
       |[1:9..1:15): _root_ => _root_/
       |[1:17..1:22): scala => scala/
       |[2:6..2:7): a <= _empty_/n35.a.
       |[2:10..2:11): s => scala/
       |[2:12..2:16): List => scala/package.List.
       |[3:6..3:7): b <= _empty_/n35.b.
       |[3:10..3:11): s => scala/
       |[3:12..3:15): Seq => scala/package.Seq.
       |[4:6..4:7): c <= _empty_/n35.c.
       |[4:10..4:11): s => scala/
       |[4:12..4:15): Nil => scala/package.Nil.
       |[5:6..5:7): d <= _empty_/n35.d.
       |[5:10..5:11): s => scala/
       |[5:12..5:16): List => scala/package.List.
       |[5:17..5:22): empty => scala/collection/immutable/List.empty().
       |[5:23..5:26): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "backquoted `List`, `Seq` and `Nil`",
    """|object n36 {
       |  val a = `List`(1)
       |  val b = `Seq`(1)
       |  val c = `Nil`
       |  val d = scala.`Nil`
       |  val e = scala.`List`(1)
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n36 <= _empty_/n36.
             |[1:6..1:7): a <= _empty_/n36.a.
             |[1:10..1:16): `List` => scala/collection/immutable/List.
             |[2:6..2:7): b <= _empty_/n36.b.
             |[2:10..2:15): `Seq` => scala/collection/Seq.
             |[3:6..3:7): c <= _empty_/n36.c.
             |[3:10..3:15): `Nil` => scala/collection/immutable/Nil.
             |[4:6..4:7): d <= _empty_/n36.d.
             |[4:10..4:15): scala => scala/
             |[4:16..4:21): `Nil` => scala/collection/immutable/Nil.
             |[5:6..5:7): e <= _empty_/n36.e.
             |[5:10..5:15): scala => scala/
             |[5:16..5:22): `List` => scala/collection/immutable/List.
             |""".stripMargin
      },
      """|[0:7..0:10): n36 <= _empty_/n36.
         |[1:6..1:7): a <= _empty_/n36.a.
         |[1:10..1:16): `List` => scala/package.List.
         |[2:6..2:7): b <= _empty_/n36.b.
         |[2:10..2:15): `Seq` => scala/package.Seq.
         |[3:6..3:7): c <= _empty_/n36.c.
         |[3:10..3:15): `Nil` => scala/package.Nil.
         |[4:6..4:7): d <= _empty_/n36.d.
         |[4:10..4:15): scala => scala/
         |[4:16..4:21): `Nil` => scala/package.Nil.
         |[5:6..5:7): e <= _empty_/n36.e.
         |[5:10..5:15): scala => scala/
         |[5:16..5:22): `List` => scala/package.List.
         |""".stripMargin,
    ),
  )

  occurrences(
    "`List`, `Seq` and `Nil` inside package scala",
    """|package scala
       |object n37 {
       |  val a = List(1)
       |  val b = Seq(1)
       |  val c = Nil
       |  val d = scala.List(1)
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:8..0:13): scala <= scala/
             |[1:7..1:10): n37 <= scala/n37.
             |[2:6..2:7): a <= scala/n37.a.
             |[2:10..2:14): List => scala/collection/immutable/List.
             |[3:6..3:7): b <= scala/n37.b.
             |[3:10..3:13): Seq => scala/collection/Seq.
             |[4:6..4:7): c <= scala/n37.c.
             |[4:10..4:13): Nil => scala/collection/immutable/Nil.
             |[5:6..5:7): d <= scala/n37.d.
             |[5:10..5:15): scala => scala/
             |[5:16..5:20): List => scala/collection/immutable/List.
             |""".stripMargin
      },
      """|[0:8..0:13): scala <= scala/
         |[1:7..1:10): n37 <= scala/n37.
         |[2:6..2:7): a <= scala/n37.a.
         |[2:10..2:14): List => scala/package.List.
         |[3:6..3:7): b <= scala/n37.b.
         |[3:10..3:13): Seq => scala/package.Seq.
         |[4:6..4:7): c <= scala/n37.c.
         |[4:10..4:13): Nil => scala/package.Nil.
         |[5:6..5:7): d <= scala/n37.d.
         |[5:10..5:15): scala => scala/
         |[5:16..5:20): List => scala/package.List.
         |""".stripMargin,
    ),
  )

  occurrences(
    "renamed `List`, `Seq` and `Nil` in nested scopes",
    """|object n38 {
       |  import scala.{List => L, Seq => S, Nil => N}
       |  object i1 { val a = L(1); val b = S(1); val c = N }
       |  def f = { val a = L(1); { val b = N; b } }
       |  class C { import scala.{List => M}; def g = { val a = M(1); a } }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n38 <= _empty_/n38.
             |[1:9..1:14): scala => scala/
             |[1:16..1:20): List => scala/package.List#
             |[1:16..1:20): List => scala/package.List.
             |[1:27..1:30): Seq => scala/package.Seq#
             |[1:27..1:30): Seq => scala/package.Seq.
             |[1:37..1:40): Nil => scala/package.Nil.
             |[2:9..2:11): i1 <= _empty_/n38.i1.
             |[2:18..2:19): a <= _empty_/n38.i1.a.
             |[2:22..2:23): L => scala/collection/immutable/List.
             |[2:32..2:33): b <= _empty_/n38.i1.b.
             |[2:36..2:37): S => scala/collection/Seq.
             |[2:46..2:47): c <= _empty_/n38.i1.c.
             |[2:50..2:51): N => scala/collection/immutable/Nil.
             |[3:6..3:7): f <= _empty_/n38.f().
             |[3:16..3:17): a <= local0
             |[3:20..3:21): L => scala/collection/immutable/List.
             |[3:32..3:33): b <= local1
             |[3:36..3:37): N => scala/collection/immutable/Nil.
             |[3:39..3:40): b => local1
             |[4:8..4:9): C <= _empty_/n38.C#
             |[4:10..4:10):  <= _empty_/n38.C#`<init>`().
             |[4:19..4:24): scala => scala/
             |[4:26..4:30): List => scala/package.List#
             |[4:26..4:30): List => scala/package.List.
             |[4:42..4:43): g <= _empty_/n38.C#g().
             |[4:52..4:53): a <= local2
             |[4:56..4:57): M => scala/collection/immutable/List.
             |[4:62..4:63): a => local2
             |""".stripMargin
      },
      """|[0:7..0:10): n38 <= _empty_/n38.
         |[1:9..1:14): scala => scala/
         |[1:16..1:20): List => scala/package.List#
         |[1:16..1:20): List => scala/package.List.
         |[1:27..1:30): Seq => scala/package.Seq#
         |[1:27..1:30): Seq => scala/package.Seq.
         |[1:37..1:40): Nil => scala/package.Nil.
         |[2:9..2:11): i1 <= _empty_/n38.i1.
         |[2:18..2:19): a <= _empty_/n38.i1.a.
         |[2:22..2:23): L => scala/package.List.
         |[2:32..2:33): b <= _empty_/n38.i1.b.
         |[2:36..2:37): S => scala/package.Seq.
         |[2:46..2:47): c <= _empty_/n38.i1.c.
         |[2:50..2:51): N => scala/package.Nil.
         |[3:6..3:7): f <= _empty_/n38.f().
         |[3:16..3:17): a <= local0
         |[3:20..3:21): L => scala/package.List.
         |[3:32..3:33): b <= local1
         |[3:36..3:37): N => scala/package.Nil.
         |[3:39..3:40): b => local1
         |[4:8..4:9): C <= _empty_/n38.C#
         |[4:10..4:10):  <= _empty_/n38.C#`<init>`().
         |[4:19..4:24): scala => scala/
         |[4:26..4:30): List => scala/package.List#
         |[4:26..4:30): List => scala/package.List.
         |[4:42..4:43): g <= _empty_/n38.C#g().
         |[4:52..4:53): a <= local2
         |[4:56..4:57): M => scala/package.List.
         |[4:62..4:63): a => local2
         |""".stripMargin,
    ),
  )

  occurrences(
    "imports from `List`, `Seq` and `Nil`",
    """|object n39 {
       |  object i1 { import List.empty; val a = empty[Int] }
       |  object i2 { import scala.Seq._; val a = fill(1)(0) }
       |  object i3 { import Nil.{isEmpty => e}; val a = e }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n39 <= _empty_/n39.
             |[1:9..1:11): i1 <= _empty_/n39.i1.
             |[1:21..1:25): List => scala/collection/immutable/List.
             |[1:26..1:31): empty => scala/collection/immutable/List.empty().
             |[1:37..1:38): a <= _empty_/n39.i1.a.
             |[1:41..1:46): empty => scala/collection/immutable/List.empty().
             |[1:47..1:50): Int => scala/Int#
             |[2:9..2:11): i2 <= _empty_/n39.i2.
             |[2:21..2:26): scala => scala/
             |[2:27..2:30): Seq => scala/collection/Seq.
             |[2:38..2:39): a <= _empty_/n39.i2.a.
             |[2:42..2:46): fill => scala/collection/generic/GenTraversableFactory#fill().
             |[3:9..3:11): i3 <= _empty_/n39.i3.
             |[3:21..3:24): Nil => scala/collection/immutable/Nil.
             |[3:26..3:33): isEmpty => scala/collection/immutable/Nil.isEmpty().
             |[3:45..3:46): a <= _empty_/n39.i3.a.
             |[3:49..3:50): e => scala/collection/immutable/Nil.isEmpty().
             |""".stripMargin
      },
      """|[0:7..0:10): n39 <= _empty_/n39.
         |[1:9..1:11): i1 <= _empty_/n39.i1.
         |[1:21..1:25): List => scala/package.List.
         |[1:26..1:31): empty => scala/collection/immutable/List.empty().
         |[1:37..1:38): a <= _empty_/n39.i1.a.
         |[1:41..1:46): empty => scala/collection/immutable/List.empty().
         |[1:47..1:50): Int => scala/Int#
         |[2:9..2:11): i2 <= _empty_/n39.i2.
         |[2:21..2:26): scala => scala/
         |[2:27..2:30): Seq => scala/package.Seq.
         |[2:38..2:39): a <= _empty_/n39.i2.a.
         |[2:42..2:46): fill => scala/collection/IterableFactory#fill().
         |[3:9..3:11): i3 <= _empty_/n39.i3.
         |[3:21..3:24): Nil => scala/package.Nil.
         |[3:26..3:33): isEmpty => scala/collection/immutable/List#isEmpty().
         |[3:45..3:46): a <= _empty_/n39.i3.a.
         |[3:49..3:50): e => scala/collection/immutable/List#isEmpty().
         |""".stripMargin,
    ),
  )

  occurrences(
    "renamed and imported `Nil` in types",
    """|object n40 {
       |  object i1 {
       |    import scala.{Nil => N}
       |    val a = classOf[Seq[N.type]]
       |    trait E { this: N.type => }
       |  }
       |  object i2 {
       |    import scala.collection.immutable.{Nil => N}
       |    val a = classOf[Seq[N.type]]
       |  }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n40 <= _empty_/n40.
             |[1:9..1:11): i1 <= _empty_/n40.i1.
             |[2:11..2:16): scala => scala/
             |[2:18..2:21): Nil => scala/package.Nil.
             |[3:8..3:9): a <= _empty_/n40.i1.a.
             |[3:12..3:19): classOf => scala/Predef.classOf().
             |[3:20..3:23): Seq => scala/package.Seq#
             |[3:24..3:25): N => scala/collection/immutable/Nil.
             |[4:10..4:11): E <= _empty_/n40.i1.E#
             |[4:20..4:21): N => scala/collection/immutable/Nil.
             |[6:9..6:11): i2 <= _empty_/n40.i2.
             |[7:11..7:16): scala => scala/
             |[7:17..7:27): collection => scala/collection/
             |[7:28..7:37): immutable => scala/collection/immutable/
             |[7:39..7:42): Nil => scala/collection/immutable/Nil.
             |[8:8..8:9): a <= _empty_/n40.i2.a.
             |[8:12..8:19): classOf => scala/Predef.classOf().
             |[8:20..8:23): Seq => scala/package.Seq#
             |[8:24..8:25): N => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n40 <= _empty_/n40.
         |[1:9..1:11): i1 <= _empty_/n40.i1.
         |[2:11..2:16): scala => scala/
         |[2:18..2:21): Nil => scala/package.Nil.
         |[3:8..3:9): a <= _empty_/n40.i1.a.
         |[3:12..3:19): classOf => scala/Predef.classOf().
         |[3:20..3:23): Seq => scala/package.Seq#
         |[3:24..3:25): N => scala/package.Nil.
         |[4:10..4:11): E <= _empty_/n40.i1.E#
         |[4:20..4:21): N => scala/package.Nil.
         |[6:9..6:11): i2 <= _empty_/n40.i2.
         |[7:11..7:16): scala => scala/
         |[7:17..7:27): collection => scala/collection/
         |[7:28..7:37): immutable => scala/collection/immutable/
         |[7:39..7:42): Nil => scala/collection/immutable/Nil.
         |[8:8..8:9): a <= _empty_/n40.i2.a.
         |[8:12..8:19): classOf => scala/Predef.classOf().
         |[8:20..8:23): Seq => scala/package.Seq#
         |[8:24..8:25): N => scala/collection/immutable/Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "qualified `List`, `Seq` and `Nil` in types",
    """|object n41 {
       |  val a = classOf[Seq[scala.List.type]]
       |  val b = classOf[Seq[scala.Seq.type]]
       |  val c = classOf[Seq[_root_.scala.Nil.type]]
       |  val d = classOf[Seq[scala.collection.immutable.Nil.type]]
       |  trait E { this: scala.Nil.type => }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n41 <= _empty_/n41.
             |[1:6..1:7): a <= _empty_/n41.a.
             |[1:10..1:17): classOf => scala/Predef.classOf().
             |[1:18..1:21): Seq => scala/package.Seq#
             |[1:22..1:27): scala => scala/
             |[1:28..1:32): List => scala/collection/immutable/List.
             |[2:6..2:7): b <= _empty_/n41.b.
             |[2:10..2:17): classOf => scala/Predef.classOf().
             |[2:18..2:21): Seq => scala/package.Seq#
             |[2:22..2:27): scala => scala/
             |[2:28..2:31): Seq => scala/collection/Seq.
             |[3:6..3:7): c <= _empty_/n41.c.
             |[3:10..3:17): classOf => scala/Predef.classOf().
             |[3:18..3:21): Seq => scala/package.Seq#
             |[3:22..3:28): _root_ => _root_/
             |[3:29..3:34): scala => scala/
             |[3:35..3:38): Nil => scala/collection/immutable/Nil.
             |[4:6..4:7): d <= _empty_/n41.d.
             |[4:10..4:17): classOf => scala/Predef.classOf().
             |[4:18..4:21): Seq => scala/package.Seq#
             |[4:22..4:27): scala => scala/
             |[4:28..4:38): collection => scala/collection/
             |[4:39..4:48): immutable => scala/collection/immutable/
             |[4:49..4:52): Nil => scala/collection/immutable/Nil.
             |[5:8..5:9): E <= _empty_/n41.E#
             |[5:18..5:23): scala => scala/
             |[5:24..5:27): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n41 <= _empty_/n41.
         |[1:6..1:7): a <= _empty_/n41.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:21): Seq => scala/package.Seq#
         |[1:22..1:27): scala => scala/
         |[1:28..1:32): List => scala/package.List.
         |[2:6..2:7): b <= _empty_/n41.b.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:21): Seq => scala/package.Seq#
         |[2:22..2:27): scala => scala/
         |[2:28..2:31): Seq => scala/package.Seq.
         |[3:6..3:7): c <= _empty_/n41.c.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:21): Seq => scala/package.Seq#
         |[3:22..3:28): _root_ => _root_/
         |[3:29..3:34): scala => scala/
         |[3:35..3:38): Nil => scala/package.Nil.
         |[4:6..4:7): d <= _empty_/n41.d.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:21): Seq => scala/package.Seq#
         |[4:22..4:27): scala => scala/
         |[4:28..4:38): collection => scala/collection/
         |[4:39..4:48): immutable => scala/collection/immutable/
         |[4:49..4:52): Nil => scala/collection/immutable/Nil.
         |[5:8..5:9): E <= _empty_/n41.E#
         |[5:18..5:23): scala => scala/
         |[5:24..5:27): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "`Nil` in an existential bound",
    """|object n42 {
       |  val a: List[T] forSome { type T <: scala.Nil.type } = Nil
       |  val b: List[T] forSome { type T <: Nil.type } = Nil
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n42 <= _empty_/n42.
             |[1:6..1:7): a <= _empty_/n42.a.
             |[1:9..1:13): List => scala/package.List#
             |[1:14..1:15): T => local0
             |[1:32..1:33): T <= local0
             |[1:37..1:42): scala => scala/
             |[1:43..1:46): Nil => scala/collection/immutable/Nil.
             |[1:56..1:59): Nil => scala/collection/immutable/Nil.
             |[2:6..2:7): b <= _empty_/n42.b.
             |[2:9..2:13): List => scala/package.List#
             |[2:14..2:15): T => local1
             |[2:32..2:33): T <= local1
             |[2:37..2:40): Nil => scala/collection/immutable/Nil.
             |[2:50..2:53): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n42 <= _empty_/n42.
         |[1:6..1:7): a <= _empty_/n42.a.
         |[1:9..1:13): List => scala/package.List#
         |[1:14..1:15): T => local0
         |[1:32..1:33): T <= local0
         |[1:37..1:42): scala => scala/
         |[1:43..1:46): Nil => scala/package.Nil.
         |[1:56..1:59): Nil => scala/package.Nil.
         |[2:6..2:7): b <= _empty_/n42.b.
         |[2:9..2:13): List => scala/package.List#
         |[2:14..2:15): T => local1
         |[2:32..2:33): T <= local1
         |[2:37..2:40): Nil => scala/package.Nil.
         |[2:50..2:53): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "`Seq` through a scala package renamed from collection",
    """|object n43 {
       |  import scala.{collection => scala}
       |  val a = classOf[Seq[scala.Seq.type]]
       |}
       |""".stripMargin,
    """|[0:7..0:10): n43 <= _empty_/n43.
       |[1:9..1:14): scala => scala/
       |[1:16..1:26): collection => scala/collection/
       |[2:6..2:7): a <= _empty_/n43.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:21): Seq => scala/package.Seq#
       |[2:22..2:27): scala => scala/collection/
       |[2:28..2:31): Seq => scala/collection/Seq.
       |""".stripMargin,
  )

  occurrences(
    "renames of `List`, `Seq` and `Nil` to one name in nested scopes",
    """|object n44 {
       |  object o1 {
       |    import scala.collection.immutable.{List => L, Nil => N}
       |    import scala.collection.{Seq => S}
       |    object i {
       |      import scala.{List => L, Seq => S, Nil => N}
       |      val a = classOf[Seq[N.type]]
       |      val b: N.type = N
       |      val c = N
       |      val d = classOf[Seq[L.type]]
       |      val e: L.type = L
       |      val f = L(1)
       |      val g = classOf[Seq[S.type]]
       |      val h: S.type = S
       |      val k = S(1)
       |    }
       |  }
       |  object o2 {
       |    import scala.{List => L, Seq => S, Nil => N}
       |    object i {
       |      import scala.collection.immutable.{List => L, Nil => N}
       |      import scala.collection.{Seq => S}
       |      val a = classOf[Seq[N.type]]
       |      val b: N.type = N
       |      val c = N
       |      val d = classOf[Seq[L.type]]
       |      val e: L.type = L
       |      val f = L(1)
       |      val g = classOf[Seq[S.type]]
       |      val h: S.type = S
       |      val k = S(1)
       |    }
       |  }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n44 <= _empty_/n44.
             |[1:9..1:11): o1 <= _empty_/n44.o1.
             |[2:11..2:16): scala => scala/
             |[2:17..2:27): collection => scala/collection/
             |[2:28..2:37): immutable => scala/collection/immutable/
             |[2:39..2:43): List => scala/collection/immutable/List#
             |[2:39..2:43): List => scala/collection/immutable/List.
             |[2:50..2:53): Nil => scala/collection/immutable/Nil.
             |[3:11..3:16): scala => scala/
             |[3:17..3:27): collection => scala/collection/
             |[3:29..3:32): Seq => scala/collection/Seq#
             |[3:29..3:32): Seq => scala/collection/Seq.
             |[4:11..4:12): i <= _empty_/n44.o1.i.
             |[5:13..5:18): scala => scala/
             |[5:20..5:24): List => scala/package.List#
             |[5:20..5:24): List => scala/package.List.
             |[5:31..5:34): Seq => scala/package.Seq#
             |[5:31..5:34): Seq => scala/package.Seq.
             |[5:41..5:44): Nil => scala/package.Nil.
             |[6:10..6:11): a <= _empty_/n44.o1.i.a.
             |[6:14..6:21): classOf => scala/Predef.classOf().
             |[6:22..6:25): Seq => scala/package.Seq#
             |[6:26..6:27): N => scala/collection/immutable/Nil.
             |[7:10..7:11): b <= _empty_/n44.o1.i.b.
             |[7:13..7:14): N => scala/collection/immutable/Nil.
             |[7:22..7:23): N => scala/collection/immutable/Nil.
             |[8:10..8:11): c <= _empty_/n44.o1.i.c.
             |[8:14..8:15): N => scala/collection/immutable/Nil.
             |[9:10..9:11): d <= _empty_/n44.o1.i.d.
             |[9:14..9:21): classOf => scala/Predef.classOf().
             |[9:22..9:25): Seq => scala/package.Seq#
             |[9:26..9:27): L => scala/collection/immutable/List.
             |[10:10..10:11): e <= _empty_/n44.o1.i.e.
             |[10:13..10:14): L => scala/collection/immutable/List.
             |[10:22..10:23): L => scala/collection/immutable/List.
             |[11:10..11:11): f <= _empty_/n44.o1.i.f.
             |[11:14..11:15): L => scala/collection/immutable/List.
             |[12:10..12:11): g <= _empty_/n44.o1.i.g.
             |[12:14..12:21): classOf => scala/Predef.classOf().
             |[12:22..12:25): Seq => scala/package.Seq#
             |[12:26..12:27): S => scala/collection/Seq.
             |[13:10..13:11): h <= _empty_/n44.o1.i.h.
             |[13:13..13:14): S => scala/collection/Seq.
             |[13:22..13:23): S => scala/collection/Seq.
             |[14:10..14:11): k <= _empty_/n44.o1.i.k.
             |[14:14..14:15): S => scala/collection/Seq.
             |[17:9..17:11): o2 <= _empty_/n44.o2.
             |[18:11..18:16): scala => scala/
             |[18:18..18:22): List => scala/package.List#
             |[18:18..18:22): List => scala/package.List.
             |[18:29..18:32): Seq => scala/package.Seq#
             |[18:29..18:32): Seq => scala/package.Seq.
             |[18:39..18:42): Nil => scala/package.Nil.
             |[19:11..19:12): i <= _empty_/n44.o2.i.
             |[20:13..20:18): scala => scala/
             |[20:19..20:29): collection => scala/collection/
             |[20:30..20:39): immutable => scala/collection/immutable/
             |[20:41..20:45): List => scala/collection/immutable/List#
             |[20:41..20:45): List => scala/collection/immutable/List.
             |[20:52..20:55): Nil => scala/collection/immutable/Nil.
             |[21:13..21:18): scala => scala/
             |[21:19..21:29): collection => scala/collection/
             |[21:31..21:34): Seq => scala/collection/Seq#
             |[21:31..21:34): Seq => scala/collection/Seq.
             |[22:10..22:11): a <= _empty_/n44.o2.i.a.
             |[22:14..22:21): classOf => scala/Predef.classOf().
             |[22:22..22:25): Seq => scala/package.Seq#
             |[22:26..22:27): N => scala/collection/immutable/Nil.
             |[23:10..23:11): b <= _empty_/n44.o2.i.b.
             |[23:13..23:14): N => scala/collection/immutable/Nil.
             |[23:22..23:23): N => scala/collection/immutable/Nil.
             |[24:10..24:11): c <= _empty_/n44.o2.i.c.
             |[24:14..24:15): N => scala/collection/immutable/Nil.
             |[25:10..25:11): d <= _empty_/n44.o2.i.d.
             |[25:14..25:21): classOf => scala/Predef.classOf().
             |[25:22..25:25): Seq => scala/package.Seq#
             |[25:26..25:27): L => scala/collection/immutable/List.
             |[26:10..26:11): e <= _empty_/n44.o2.i.e.
             |[26:13..26:14): L => scala/collection/immutable/List.
             |[26:22..26:23): L => scala/collection/immutable/List.
             |[27:10..27:11): f <= _empty_/n44.o2.i.f.
             |[27:14..27:15): L => scala/collection/immutable/List.
             |[28:10..28:11): g <= _empty_/n44.o2.i.g.
             |[28:14..28:21): classOf => scala/Predef.classOf().
             |[28:22..28:25): Seq => scala/package.Seq#
             |[28:26..28:27): S => scala/collection/Seq.
             |[29:10..29:11): h <= _empty_/n44.o2.i.h.
             |[29:13..29:14): S => scala/collection/Seq.
             |[29:22..29:23): S => scala/collection/Seq.
             |[30:10..30:11): k <= _empty_/n44.o2.i.k.
             |[30:14..30:15): S => scala/collection/Seq.
             |""".stripMargin
      },
      """|[0:7..0:10): n44 <= _empty_/n44.
         |[1:9..1:11): o1 <= _empty_/n44.o1.
         |[2:11..2:16): scala => scala/
         |[2:17..2:27): collection => scala/collection/
         |[2:28..2:37): immutable => scala/collection/immutable/
         |[2:39..2:43): List => scala/collection/immutable/List#
         |[2:39..2:43): List => scala/collection/immutable/List.
         |[2:50..2:53): Nil => scala/collection/immutable/Nil.
         |[3:11..3:16): scala => scala/
         |[3:17..3:27): collection => scala/collection/
         |[3:29..3:32): Seq => scala/collection/Seq#
         |[3:29..3:32): Seq => scala/collection/Seq.
         |[4:11..4:12): i <= _empty_/n44.o1.i.
         |[5:13..5:18): scala => scala/
         |[5:20..5:24): List => scala/package.List#
         |[5:20..5:24): List => scala/package.List.
         |[5:31..5:34): Seq => scala/package.Seq#
         |[5:31..5:34): Seq => scala/package.Seq.
         |[5:41..5:44): Nil => scala/package.Nil.
         |[6:10..6:11): a <= _empty_/n44.o1.i.a.
         |[6:14..6:21): classOf => scala/Predef.classOf().
         |[6:22..6:25): Seq => scala/package.Seq#
         |[6:26..6:27): N => scala/package.Nil.
         |[7:10..7:11): b <= _empty_/n44.o1.i.b.
         |[7:13..7:14): N => scala/package.Nil.
         |[7:22..7:23): N => scala/package.Nil.
         |[8:10..8:11): c <= _empty_/n44.o1.i.c.
         |[8:14..8:15): N => scala/package.Nil.
         |[9:10..9:11): d <= _empty_/n44.o1.i.d.
         |[9:14..9:21): classOf => scala/Predef.classOf().
         |[9:22..9:25): Seq => scala/package.Seq#
         |[9:26..9:27): L => scala/package.List.
         |[10:10..10:11): e <= _empty_/n44.o1.i.e.
         |[10:13..10:14): L => scala/package.List.
         |[10:22..10:23): L => scala/package.List.
         |[11:10..11:11): f <= _empty_/n44.o1.i.f.
         |[11:14..11:15): L => scala/package.List.
         |[12:10..12:11): g <= _empty_/n44.o1.i.g.
         |[12:14..12:21): classOf => scala/Predef.classOf().
         |[12:22..12:25): Seq => scala/package.Seq#
         |[12:26..12:27): S => scala/package.Seq.
         |[13:10..13:11): h <= _empty_/n44.o1.i.h.
         |[13:13..13:14): S => scala/package.Seq.
         |[13:22..13:23): S => scala/package.Seq.
         |[14:10..14:11): k <= _empty_/n44.o1.i.k.
         |[14:14..14:15): S => scala/package.Seq.
         |[17:9..17:11): o2 <= _empty_/n44.o2.
         |[18:11..18:16): scala => scala/
         |[18:18..18:22): List => scala/package.List#
         |[18:18..18:22): List => scala/package.List.
         |[18:29..18:32): Seq => scala/package.Seq#
         |[18:29..18:32): Seq => scala/package.Seq.
         |[18:39..18:42): Nil => scala/package.Nil.
         |[19:11..19:12): i <= _empty_/n44.o2.i.
         |[20:13..20:18): scala => scala/
         |[20:19..20:29): collection => scala/collection/
         |[20:30..20:39): immutable => scala/collection/immutable/
         |[20:41..20:45): List => scala/collection/immutable/List#
         |[20:41..20:45): List => scala/collection/immutable/List.
         |[20:52..20:55): Nil => scala/collection/immutable/Nil.
         |[21:13..21:18): scala => scala/
         |[21:19..21:29): collection => scala/collection/
         |[21:31..21:34): Seq => scala/collection/Seq#
         |[21:31..21:34): Seq => scala/collection/Seq.
         |[22:10..22:11): a <= _empty_/n44.o2.i.a.
         |[22:14..22:21): classOf => scala/Predef.classOf().
         |[22:22..22:25): Seq => scala/package.Seq#
         |[22:26..22:27): N => scala/collection/immutable/Nil.
         |[23:10..23:11): b <= _empty_/n44.o2.i.b.
         |[23:13..23:14): N => scala/collection/immutable/Nil.
         |[23:22..23:23): N => scala/collection/immutable/Nil.
         |[24:10..24:11): c <= _empty_/n44.o2.i.c.
         |[24:14..24:15): N => scala/collection/immutable/Nil.
         |[25:10..25:11): d <= _empty_/n44.o2.i.d.
         |[25:14..25:21): classOf => scala/Predef.classOf().
         |[25:22..25:25): Seq => scala/package.Seq#
         |[25:26..25:27): L => scala/collection/immutable/List.
         |[26:10..26:11): e <= _empty_/n44.o2.i.e.
         |[26:13..26:14): L => scala/collection/immutable/List.
         |[26:22..26:23): L => scala/collection/immutable/List.
         |[27:10..27:11): f <= _empty_/n44.o2.i.f.
         |[27:14..27:15): L => scala/collection/immutable/List.
         |[28:10..28:11): g <= _empty_/n44.o2.i.g.
         |[28:14..28:21): classOf => scala/Predef.classOf().
         |[28:22..28:25): Seq => scala/package.Seq#
         |[28:26..28:27): S => scala/collection/Seq.
         |[29:10..29:11): h <= _empty_/n44.o2.i.h.
         |[29:13..29:14): S => scala/collection/Seq.
         |[29:22..29:23): S => scala/collection/Seq.
         |[30:10..30:11): k <= _empty_/n44.o2.i.k.
         |[30:14..30:15): S => scala/collection/Seq.
         |""".stripMargin,
    ),
  )

  if (ScalaVersion.is212) occurrences(
    "type selections through `List` and `Seq`",
    """|object n45 {
       |  val a = classOf[scala.List.GenericCanBuildFrom[Int]]
       |  val b = classOf[List.GenericCanBuildFrom[Int]]
       |  val c = classOf[scala.Seq.GenericCanBuildFrom[Int]]
       |  val d = classOf[Seq.GenericCanBuildFrom[Int]]
       |  val e: scala.List.GenericCanBuildFrom[Int] = null
       |  val f: Seq.GenericCanBuildFrom[Int] = null
       |  type T = List.GenericCanBuildFrom[Int]
       |  val g = classOf[scala.collection.immutable.List.GenericCanBuildFrom[Int]]
       |  object i {
       |    import scala.{List => L, Seq => S}
       |    val a = classOf[L.GenericCanBuildFrom[Int]]
       |    val b: S.GenericCanBuildFrom[Int] = null
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:10): n45 <= _empty_/n45.
       |[1:6..1:7): a <= _empty_/n45.a.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[1:18..1:23): scala => scala/
       |[1:24..1:28): List => scala/collection/immutable/List.
       |[1:29..1:48): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[1:49..1:52): Int => scala/Int#
       |[2:6..2:7): b <= _empty_/n45.b.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:22): List => scala/collection/immutable/List.
       |[2:23..2:42): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[2:43..2:46): Int => scala/Int#
       |[3:6..3:7): c <= _empty_/n45.c.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:23): scala => scala/
       |[3:24..3:27): Seq => scala/collection/Seq.
       |[3:28..3:47): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[3:48..3:51): Int => scala/Int#
       |[4:6..4:7): d <= _empty_/n45.d.
       |[4:10..4:17): classOf => scala/Predef.classOf().
       |[4:18..4:21): Seq => scala/collection/Seq.
       |[4:22..4:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[4:42..4:45): Int => scala/Int#
       |[5:6..5:7): e <= _empty_/n45.e.
       |[5:9..5:14): scala => scala/
       |[5:15..5:19): List => scala/collection/immutable/List.
       |[5:20..5:39): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[5:40..5:43): Int => scala/Int#
       |[6:6..6:7): f <= _empty_/n45.f.
       |[6:9..6:12): Seq => scala/collection/Seq.
       |[6:13..6:32): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[6:33..6:36): Int => scala/Int#
       |[7:7..7:8): T <= _empty_/n45.T#
       |[7:11..7:15): List => scala/collection/immutable/List.
       |[7:16..7:35): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[7:36..7:39): Int => scala/Int#
       |[8:6..8:7): g <= _empty_/n45.g.
       |[8:10..8:17): classOf => scala/Predef.classOf().
       |[8:18..8:23): scala => scala/
       |[8:24..8:34): collection => scala/collection/
       |[8:35..8:44): immutable => scala/collection/immutable/
       |[8:45..8:49): List => scala/collection/immutable/List.
       |[8:50..8:69): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[8:70..8:73): Int => scala/Int#
       |[9:9..9:10): i <= _empty_/n45.i.
       |[10:11..10:16): scala => scala/
       |[10:18..10:22): List => scala/package.List#
       |[10:18..10:22): List => scala/package.List.
       |[10:29..10:32): Seq => scala/package.Seq#
       |[10:29..10:32): Seq => scala/package.Seq.
       |[11:8..11:9): a <= _empty_/n45.i.a.
       |[11:12..11:19): classOf => scala/Predef.classOf().
       |[11:20..11:21): L => scala/collection/immutable/List.
       |[11:22..11:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[11:42..11:45): Int => scala/Int#
       |[12:8..12:9): b <= _empty_/n45.i.b.
       |[12:11..12:12): S => scala/collection/Seq.
       |[12:13..12:32): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[12:33..12:36): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "a user-defined alias of an object in a package object",
    """|package n46 {
       |  object Foo { val member = 1 }
       |  package object p { val Bar = Foo }
       |  object o1 {
       |    import p.Bar
       |    val a = Bar
       |    val b = Bar.member
       |    val c: Bar.type = Bar
       |    val d = classOf[Seq[Bar.type]]
       |    val e = p.Bar
       |  }
       |  object o2 {
       |    import p.{Bar => B}
       |    val a = B
       |    val b = B.member
       |    val c: B.type = B
       |    val d = classOf[Seq[B.type]]
       |  }
       |}
       |""".stripMargin,
    """|[0:8..0:11): n46 <= n46/
       |[1:9..1:12): Foo <= n46/Foo.
       |[1:19..1:25): member <= n46/Foo.member.
       |[2:17..2:18): p <= n46/p/package.
       |[2:25..2:28): Bar <= n46/p/package.Bar.
       |[2:31..2:34): Foo => n46/Foo.
       |[3:9..3:11): o1 <= n46/o1.
       |[4:11..4:12): p => n46/p/
       |[4:13..4:16): Bar => n46/p/package.Bar.
       |[5:8..5:9): a <= n46/o1.a.
       |[5:12..5:15): Bar => n46/p/package.Bar.
       |[6:8..6:9): b <= n46/o1.b.
       |[6:12..6:15): Bar => n46/p/package.Bar.
       |[6:16..6:22): member => n46/Foo.member.
       |[7:8..7:9): c <= n46/o1.c.
       |[7:11..7:14): Bar => n46/p/package.Bar.
       |[7:22..7:25): Bar => n46/p/package.Bar.
       |[8:8..8:9): d <= n46/o1.d.
       |[8:12..8:19): classOf => scala/Predef.classOf().
       |[8:20..8:23): Seq => scala/package.Seq#
       |[8:24..8:27): Bar => n46/p/package.Bar.
       |[9:8..9:9): e <= n46/o1.e.
       |[9:12..9:13): p => n46/p/
       |[9:14..9:17): Bar => n46/p/package.Bar.
       |[11:9..11:11): o2 <= n46/o2.
       |[12:11..12:12): p => n46/p/
       |[12:14..12:17): Bar => n46/p/package.Bar.
       |[13:8..13:9): a <= n46/o2.a.
       |[13:12..13:13): B => n46/p/package.Bar.
       |[14:8..14:9): b <= n46/o2.b.
       |[14:12..14:13): B => n46/p/package.Bar.
       |[14:14..14:20): member => n46/Foo.member.
       |[15:8..15:9): c <= n46/o2.c.
       |[15:11..15:12): B => n46/p/package.Bar.
       |[15:20..15:21): B => n46/p/package.Bar.
       |[16:8..16:9): d <= n46/o2.d.
       |[16:12..16:19): classOf => scala/Predef.classOf().
       |[16:20..16:23): Seq => scala/package.Seq#
       |[16:24..16:25): B => n46/p/package.Bar.
       |""".stripMargin,
  )

  occurrences(
    "renames of `List`, `Seq` and `Nil` to the name of another alias",
    """|object n47 {
       |  object o1 {
       |    import scala.{Seq => List}
       |    val a = List(1)
       |    val b = List.empty
       |    val c = classOf[Seq[List.type]]
       |    val d: List.type = List
       |    def f(x: Any) = x match { case List(y) => y }
       |  }
       |  object o2 {
       |    import scala.{Nil => List, List => Seq}
       |    val a = List
       |    val b = Seq(1)
       |    val c = classOf[Seq[List.type]]
       |    def f(x: Any) = x match { case List => 1; case Seq(y) => y }
       |  }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n47 <= _empty_/n47.
             |[1:9..1:11): o1 <= _empty_/n47.o1.
             |[2:11..2:16): scala => scala/
             |[2:18..2:21): Seq => scala/package.Seq#
             |[2:18..2:21): Seq => scala/package.Seq.
             |[3:8..3:9): a <= _empty_/n47.o1.a.
             |[3:12..3:16): List => scala/collection/Seq.
             |[4:8..4:9): b <= _empty_/n47.o1.b.
             |[4:12..4:16): List => scala/collection/Seq.
             |[4:17..4:22): empty => scala/collection/generic/GenericCompanion#empty().
             |[5:8..5:9): c <= _empty_/n47.o1.c.
             |[5:12..5:19): classOf => scala/Predef.classOf().
             |[5:20..5:23): Seq => scala/package.Seq#
             |[5:24..5:28): List => scala/collection/Seq.
             |[6:8..6:9): d <= _empty_/n47.o1.d.
             |[6:11..6:15): List => scala/collection/Seq.
             |[6:23..6:27): List => scala/collection/Seq.
             |[7:8..7:9): f <= _empty_/n47.o1.f().
             |[7:10..7:11): x <= _empty_/n47.o1.f().(x)
             |[7:13..7:16): Any => scala/Any#
             |[7:20..7:21): x => _empty_/n47.o1.f().(x)
             |[7:35..7:39): List => scala/collection/Seq.
             |[7:40..7:41): y <= local1
             |[7:46..7:47): y => local1
             |[9:9..9:11): o2 <= _empty_/n47.o2.
             |[10:11..10:16): scala => scala/
             |[10:18..10:21): Nil => scala/package.Nil.
             |[10:31..10:35): List => scala/package.List#
             |[10:31..10:35): List => scala/package.List.
             |[11:8..11:9): a <= _empty_/n47.o2.a.
             |[11:12..11:16): List => scala/collection/immutable/Nil.
             |[12:8..12:9): b <= _empty_/n47.o2.b.
             |[12:12..12:15): Seq => scala/collection/immutable/List.
             |[13:8..13:9): c <= _empty_/n47.o2.c.
             |[13:12..13:19): classOf => scala/Predef.classOf().
             |[13:20..13:23): Seq => scala/package.List#
             |[13:24..13:28): List => scala/collection/immutable/Nil.
             |[14:8..14:9): f <= _empty_/n47.o2.f().
             |[14:10..14:11): x <= _empty_/n47.o2.f().(x)
             |[14:13..14:16): Any => scala/Any#
             |[14:20..14:21): x => _empty_/n47.o2.f().(x)
             |[14:35..14:39): List => scala/collection/immutable/Nil.
             |[14:51..14:54): Seq => scala/collection/immutable/List.
             |[14:55..14:56): y <= local3
             |[14:61..14:62): y => local3
             |""".stripMargin
      },
      """|[0:7..0:10): n47 <= _empty_/n47.
         |[1:9..1:11): o1 <= _empty_/n47.o1.
         |[2:11..2:16): scala => scala/
         |[2:18..2:21): Seq => scala/package.Seq#
         |[2:18..2:21): Seq => scala/package.Seq.
         |[3:8..3:9): a <= _empty_/n47.o1.a.
         |[3:12..3:16): List => scala/package.Seq.
         |[4:8..4:9): b <= _empty_/n47.o1.b.
         |[4:12..4:16): List => scala/package.Seq.
         |[4:17..4:22): empty => scala/collection/SeqFactory.Delegate#empty().
         |[5:8..5:9): c <= _empty_/n47.o1.c.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:23): Seq => scala/package.Seq#
         |[5:24..5:28): List => scala/package.Seq.
         |[6:8..6:9): d <= _empty_/n47.o1.d.
         |[6:11..6:15): List => scala/package.Seq.
         |[6:23..6:27): List => scala/package.Seq.
         |[7:8..7:9): f <= _empty_/n47.o1.f().
         |[7:10..7:11): x <= _empty_/n47.o1.f().(x)
         |[7:13..7:16): Any => scala/Any#
         |[7:20..7:21): x => _empty_/n47.o1.f().(x)
         |[7:35..7:39): List => scala/package.Seq.
         |[7:40..7:41): y <= local1
         |[7:46..7:47): y => local1
         |[9:9..9:11): o2 <= _empty_/n47.o2.
         |[10:11..10:16): scala => scala/
         |[10:18..10:21): Nil => scala/package.Nil.
         |[10:31..10:35): List => scala/package.List#
         |[10:31..10:35): List => scala/package.List.
         |[11:8..11:9): a <= _empty_/n47.o2.a.
         |[11:12..11:16): List => scala/package.Nil.
         |[12:8..12:9): b <= _empty_/n47.o2.b.
         |[12:12..12:15): Seq => scala/package.List.
         |[13:8..13:9): c <= _empty_/n47.o2.c.
         |[13:12..13:19): classOf => scala/Predef.classOf().
         |[13:20..13:23): Seq => scala/package.List#
         |[13:24..13:28): List => scala/package.Nil.
         |[14:8..14:9): f <= _empty_/n47.o2.f().
         |[14:10..14:11): x <= _empty_/n47.o2.f().(x)
         |[14:13..14:16): Any => scala/Any#
         |[14:20..14:21): x => _empty_/n47.o2.f().(x)
         |[14:35..14:39): List => scala/package.Nil.
         |[14:51..14:54): Seq => scala/package.List.
         |[14:55..14:56): y <= local3
         |[14:61..14:62): y => local3
         |""".stripMargin,
    ),
  )

  occurrences(
    "renamed `List`, `Seq` and `Nil` in a method body and in constructor defaults",
    """|object n48 {
       |  def m = {
       |    import scala.{List => L, Seq => S, Nil => N}
       |    val a = L(1)
       |    val b = S.empty
       |    val c: N.type = N
       |    classOf[Seq[N.type]]
       |  }
       |  import scala.collection.immutable.{List => L, Nil => N}
       |  import scala.collection.{Seq => S}
       |  object i {
       |    object K
       |    import scala.{List => L, Seq => S, Nil => N}
       |    class K(
       |        a: Any = L(1),
       |        b: Any = S.empty,
       |        c: N.type = N,
       |        d: Any = classOf[Seq[L.type]],
       |    )
       |    def f(a: Any = L(1), b: N.type = N) = a
       |  }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n48 <= _empty_/n48.
             |[1:6..1:7): m <= _empty_/n48.m().
             |[2:11..2:16): scala => scala/
             |[2:18..2:22): List => scala/package.List#
             |[2:18..2:22): List => scala/package.List.
             |[2:29..2:32): Seq => scala/package.Seq#
             |[2:29..2:32): Seq => scala/package.Seq.
             |[2:39..2:42): Nil => scala/package.Nil.
             |[3:8..3:9): a <= local0
             |[3:12..3:13): L => scala/collection/immutable/List.
             |[4:8..4:9): b <= local1
             |[4:12..4:13): S => scala/collection/Seq.
             |[4:14..4:19): empty => scala/collection/generic/GenericCompanion#empty().
             |[5:8..5:9): c <= local2
             |[5:11..5:12): N => scala/collection/immutable/Nil.
             |[5:20..5:21): N => scala/collection/immutable/Nil.
             |[6:4..6:11): classOf => scala/Predef.classOf().
             |[6:12..6:15): Seq => scala/package.Seq#
             |[6:16..6:17): N => scala/collection/immutable/Nil.
             |[8:9..8:14): scala => scala/
             |[8:15..8:25): collection => scala/collection/
             |[8:26..8:35): immutable => scala/collection/immutable/
             |[8:37..8:41): List => scala/collection/immutable/List#
             |[8:37..8:41): List => scala/collection/immutable/List.
             |[8:48..8:51): Nil => scala/collection/immutable/Nil.
             |[9:9..9:14): scala => scala/
             |[9:15..9:25): collection => scala/collection/
             |[9:27..9:30): Seq => scala/collection/Seq#
             |[9:27..9:30): Seq => scala/collection/Seq.
             |[10:9..10:10): i <= _empty_/n48.i.
             |[11:11..11:12): K <= _empty_/n48.i.K.
             |[12:11..12:16): scala => scala/
             |[12:18..12:22): List => scala/package.List#
             |[12:18..12:22): List => scala/package.List.
             |[12:29..12:32): Seq => scala/package.Seq#
             |[12:29..12:32): Seq => scala/package.Seq.
             |[12:39..12:42): Nil => scala/package.Nil.
             |[13:10..13:11): K <= _empty_/n48.i.K#
             |[13:11..13:11):  <= _empty_/n48.i.K#`<init>`().
             |[14:8..14:9): a <= _empty_/n48.i.K#`<init>`().(a)
             |[14:8..14:9): a <= _empty_/n48.i.K#a.
             |[14:11..14:14): Any => scala/Any#
             |[14:17..14:18): L => scala/collection/immutable/List.
             |[15:8..15:9): b <= _empty_/n48.i.K#`<init>`().(b)
             |[15:8..15:9): b <= _empty_/n48.i.K#b.
             |[15:11..15:14): Any => scala/Any#
             |[15:17..15:18): S => scala/collection/Seq.
             |[15:19..15:24): empty => scala/collection/generic/GenericCompanion#empty().
             |[16:8..16:9): c <= _empty_/n48.i.K#`<init>`().(c)
             |[16:8..16:9): c <= _empty_/n48.i.K#c.
             |[16:11..16:12): N => scala/collection/immutable/Nil.
             |[16:20..16:21): N => scala/collection/immutable/Nil.
             |[17:8..17:9): d <= _empty_/n48.i.K#`<init>`().(d)
             |[17:8..17:9): d <= _empty_/n48.i.K#d.
             |[17:11..17:14): Any => scala/Any#
             |[17:17..17:24): classOf => scala/Predef.classOf().
             |[17:25..17:28): Seq => scala/package.Seq#
             |[17:29..17:30): L => scala/collection/immutable/List.
             |[19:8..19:9): f <= _empty_/n48.i.f().
             |[19:10..19:11): a <= _empty_/n48.i.f().(a)
             |[19:13..19:16): Any => scala/Any#
             |[19:19..19:20): L => scala/collection/immutable/List.
             |[19:25..19:26): b <= _empty_/n48.i.f().(b)
             |[19:28..19:29): N => scala/collection/immutable/Nil.
             |[19:37..19:38): N => scala/collection/immutable/Nil.
             |[19:42..19:43): a => _empty_/n48.i.f().(a)
             |""".stripMargin
      },
      """|[0:7..0:10): n48 <= _empty_/n48.
         |[1:6..1:7): m <= _empty_/n48.m().
         |[2:11..2:16): scala => scala/
         |[2:18..2:22): List => scala/package.List#
         |[2:18..2:22): List => scala/package.List.
         |[2:29..2:32): Seq => scala/package.Seq#
         |[2:29..2:32): Seq => scala/package.Seq.
         |[2:39..2:42): Nil => scala/package.Nil.
         |[3:8..3:9): a <= local0
         |[3:12..3:13): L => scala/package.List.
         |[4:8..4:9): b <= local1
         |[4:12..4:13): S => scala/package.Seq.
         |[4:14..4:19): empty => scala/collection/SeqFactory.Delegate#empty().
         |[5:8..5:9): c <= local2
         |[5:11..5:12): N => scala/package.Nil.
         |[5:20..5:21): N => scala/package.Nil.
         |[6:4..6:11): classOf => scala/Predef.classOf().
         |[6:12..6:15): Seq => scala/package.Seq#
         |[6:16..6:17): N => scala/package.Nil.
         |[8:9..8:14): scala => scala/
         |[8:15..8:25): collection => scala/collection/
         |[8:26..8:35): immutable => scala/collection/immutable/
         |[8:37..8:41): List => scala/collection/immutable/List#
         |[8:37..8:41): List => scala/collection/immutable/List.
         |[8:48..8:51): Nil => scala/collection/immutable/Nil.
         |[9:9..9:14): scala => scala/
         |[9:15..9:25): collection => scala/collection/
         |[9:27..9:30): Seq => scala/collection/Seq#
         |[9:27..9:30): Seq => scala/collection/Seq.
         |[10:9..10:10): i <= _empty_/n48.i.
         |[11:11..11:12): K <= _empty_/n48.i.K.
         |[12:11..12:16): scala => scala/
         |[12:18..12:22): List => scala/package.List#
         |[12:18..12:22): List => scala/package.List.
         |[12:29..12:32): Seq => scala/package.Seq#
         |[12:29..12:32): Seq => scala/package.Seq.
         |[12:39..12:42): Nil => scala/package.Nil.
         |[13:10..13:11): K <= _empty_/n48.i.K#
         |[13:11..13:11):  <= _empty_/n48.i.K#`<init>`().
         |[14:8..14:9): a <= _empty_/n48.i.K#`<init>`().(a)
         |[14:8..14:9): a <= _empty_/n48.i.K#a.
         |[14:11..14:14): Any => scala/Any#
         |[14:17..14:18): L => scala/collection/immutable/List.
         |[15:8..15:9): b <= _empty_/n48.i.K#`<init>`().(b)
         |[15:8..15:9): b <= _empty_/n48.i.K#b.
         |[15:11..15:14): Any => scala/Any#
         |[15:17..15:18): S => scala/collection/Seq.
         |[15:19..15:24): empty => scala/collection/SeqFactory.Delegate#empty().
         |[16:8..16:9): c <= _empty_/n48.i.K#`<init>`().(c)
         |[16:8..16:9): c <= _empty_/n48.i.K#c.
         |[16:11..16:12): N => scala/collection/immutable/Nil.
         |[16:20..16:21): N => scala/collection/immutable/Nil.
         |[17:8..17:9): d <= _empty_/n48.i.K#`<init>`().(d)
         |[17:8..17:9): d <= _empty_/n48.i.K#d.
         |[17:11..17:14): Any => scala/Any#
         |[17:17..17:24): classOf => scala/Predef.classOf().
         |[17:25..17:28): Seq => scala/package.Seq#
         |[17:29..17:30): L => scala/package.List.
         |[19:8..19:9): f <= _empty_/n48.i.f().
         |[19:10..19:11): a <= _empty_/n48.i.f().(a)
         |[19:13..19:16): Any => scala/Any#
         |[19:19..19:20): L => scala/package.List.
         |[19:25..19:26): b <= _empty_/n48.i.f().(b)
         |[19:28..19:29): N => scala/package.Nil.
         |[19:37..19:38): N => scala/package.Nil.
         |[19:42..19:43): a => _empty_/n48.i.f().(a)
         |""".stripMargin,
    ),
  )

  occurrences(
    "a renamed user alias of `List`",
    """|object n49 {
       |  object o { val L = scala.collection.immutable.List }
       |  import o.{L => M}
       |  val a: M.type = M
       |  val b = M(1)
       |  val c = M.empty
       |  val d = classOf[Seq[M.type]]
       |}
       |""".stripMargin,
    """|[0:7..0:10): n49 <= _empty_/n49.
       |[1:9..1:10): o <= _empty_/n49.o.
       |[1:17..1:18): L <= _empty_/n49.o.L.
       |[1:21..1:26): scala => scala/
       |[1:27..1:37): collection => scala/collection/
       |[1:38..1:47): immutable => scala/collection/immutable/
       |[1:48..1:52): List => scala/collection/immutable/List.
       |[2:9..2:10): o => _empty_/n49.o.
       |[2:12..2:13): L => _empty_/n49.o.L.
       |[3:6..3:7): a <= _empty_/n49.a.
       |[3:9..3:10): M => _empty_/n49.o.L.
       |[3:18..3:19): M => _empty_/n49.o.L.
       |[4:6..4:7): b <= _empty_/n49.b.
       |[4:10..4:11): M => _empty_/n49.o.L.
       |[5:6..5:7): c <= _empty_/n49.c.
       |[5:10..5:11): M => _empty_/n49.o.L.
       |[5:12..5:17): empty => scala/collection/immutable/List.empty().
       |[6:6..6:7): d <= _empty_/n49.d.
       |[6:10..6:17): classOf => scala/Predef.classOf().
       |[6:18..6:21): Seq => scala/package.Seq#
       |[6:22..6:23): M => _empty_/n49.o.L.
       |""".stripMargin,
  )

  occurrences(
    "a renamed `Nil` in an existential bound",
    """|object n50 {
       |  import scala.{Nil => N}
       |  import scala.collection.immutable.{Nil => M}
       |  val a: List[T] forSome { type T <: N.type } = N
       |  val b: List[T] forSome { type T <: M.type } = M
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n50 <= _empty_/n50.
             |[1:9..1:14): scala => scala/
             |[1:16..1:19): Nil => scala/package.Nil.
             |[2:9..2:14): scala => scala/
             |[2:15..2:25): collection => scala/collection/
             |[2:26..2:35): immutable => scala/collection/immutable/
             |[2:37..2:40): Nil => scala/collection/immutable/Nil.
             |[3:6..3:7): a <= _empty_/n50.a.
             |[3:9..3:13): List => scala/package.List#
             |[3:14..3:15): T => local0
             |[3:32..3:33): T <= local0
             |[3:37..3:38): N => scala/collection/immutable/Nil.
             |[3:48..3:49): N => scala/collection/immutable/Nil.
             |[4:6..4:7): b <= _empty_/n50.b.
             |[4:9..4:13): List => scala/package.List#
             |[4:14..4:15): T => local1
             |[4:32..4:33): T <= local1
             |[4:37..4:38): M => scala/collection/immutable/Nil.
             |[4:48..4:49): M => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n50 <= _empty_/n50.
         |[1:9..1:14): scala => scala/
         |[1:16..1:19): Nil => scala/package.Nil.
         |[2:9..2:14): scala => scala/
         |[2:15..2:25): collection => scala/collection/
         |[2:26..2:35): immutable => scala/collection/immutable/
         |[2:37..2:40): Nil => scala/collection/immutable/Nil.
         |[3:6..3:7): a <= _empty_/n50.a.
         |[3:9..3:13): List => scala/package.List#
         |[3:14..3:15): T => local0
         |[3:32..3:33): T <= local0
         |[3:37..3:38): N => scala/package.Nil.
         |[3:48..3:49): N => scala/package.Nil.
         |[4:6..4:7): b <= _empty_/n50.b.
         |[4:9..4:13): List => scala/package.List#
         |[4:14..4:15): T => local1
         |[4:32..4:33): T <= local1
         |[4:37..4:38): M => scala/collection/immutable/Nil.
         |[4:48..4:49): M => scala/collection/immutable/Nil.
         |""".stripMargin,
    ),
  )

  if (ScalaVersion.is212) occurrences(
    "type selections through `List` and `Seq` in self types and parents",
    """|object n51 {
       |  trait A { this: scala.List.GenericCanBuildFrom[Int] => }
       |  trait B { this: Seq.GenericCanBuildFrom[Int] => }
       |  class C extends scala.List.GenericCanBuildFrom[Int]
       |  class D extends Seq.GenericCanBuildFrom[Int]
       |  val e = new scala.Seq.GenericCanBuildFrom[Int]
       |  object i {
       |    import scala.{List => L}
       |    trait A { this: L.GenericCanBuildFrom[Int] => }
       |    class C extends L.GenericCanBuildFrom[Int]
       |  }
       |  object u {
       |    object o { val L = scala.collection.immutable.List }
       |    import o.{L => M}
       |    val a = classOf[M.GenericCanBuildFrom[Int]]
       |    class C extends M.GenericCanBuildFrom[Int]
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:10): n51 <= _empty_/n51.
       |[1:8..1:9): A <= _empty_/n51.A#
       |[1:18..1:23): scala => scala/
       |[1:24..1:28): List => scala/collection/immutable/List.
       |[1:29..1:48): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[1:49..1:52): Int => scala/Int#
       |[2:8..2:9): B <= _empty_/n51.B#
       |[2:18..2:21): Seq => scala/collection/Seq.
       |[2:22..2:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[2:42..2:45): Int => scala/Int#
       |[3:8..3:9): C <= _empty_/n51.C#
       |[3:10..3:10):  <= _empty_/n51.C#`<init>`().
       |[3:18..3:23): scala => scala/
       |[3:24..3:28): List => scala/collection/immutable/List.
       |[3:29..3:48): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[3:49..3:52): Int => scala/Int#
       |[3:53..3:53):  => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#`<init>`().
       |[4:8..4:9): D <= _empty_/n51.D#
       |[4:10..4:10):  <= _empty_/n51.D#`<init>`().
       |[4:18..4:21): Seq => scala/collection/Seq.
       |[4:22..4:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[4:42..4:45): Int => scala/Int#
       |[4:46..4:46):  => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#`<init>`().
       |[5:6..5:7): e <= _empty_/n51.e.
       |[5:14..5:19): scala => scala/
       |[5:20..5:23): Seq => scala/collection/Seq.
       |[5:24..5:43): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[5:44..5:47): Int => scala/Int#
       |[5:48..5:48):  => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#`<init>`().
       |[6:9..6:10): i <= _empty_/n51.i.
       |[7:11..7:16): scala => scala/
       |[7:18..7:22): List => scala/package.List#
       |[7:18..7:22): List => scala/package.List.
       |[8:10..8:11): A <= _empty_/n51.i.A#
       |[8:20..8:21): L => scala/collection/immutable/List.
       |[8:22..8:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[8:42..8:45): Int => scala/Int#
       |[9:10..9:11): C <= _empty_/n51.i.C#
       |[9:12..9:12):  <= _empty_/n51.i.C#`<init>`().
       |[9:20..9:21): L => scala/collection/immutable/List.
       |[9:22..9:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[9:42..9:45): Int => scala/Int#
       |[9:46..9:46):  => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#`<init>`().
       |[11:9..11:10): u <= _empty_/n51.u.
       |[12:11..12:12): o <= _empty_/n51.u.o.
       |[12:19..12:20): L <= _empty_/n51.u.o.L.
       |[12:23..12:28): scala => scala/
       |[12:29..12:39): collection => scala/collection/
       |[12:40..12:49): immutable => scala/collection/immutable/
       |[12:50..12:54): List => scala/collection/immutable/List.
       |[13:11..13:12): o => _empty_/n51.u.o.
       |[13:14..13:15): L => _empty_/n51.u.o.L.
       |[14:8..14:9): a <= _empty_/n51.u.a.
       |[14:12..14:19): classOf => scala/Predef.classOf().
       |[14:20..14:21): M => _empty_/n51.u.o.L.
       |[14:22..14:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[14:42..14:45): Int => scala/Int#
       |[15:10..15:11): C <= _empty_/n51.u.C#
       |[15:12..15:12):  <= _empty_/n51.u.C#`<init>`().
       |[15:20..15:21): M => _empty_/n51.u.o.L.
       |[15:22..15:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[15:42..15:45): Int => scala/Int#
       |[15:46..15:46):  => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "patterns on `List`, `Seq` and `Nil` through `_root_`",
    """|object n52 {
       |  def f(x: Any) = x match {
       |    case _root_.scala.Nil => 1
       |    case _root_.scala.List(y) => 2
       |    case _ :: _root_.scala.Nil => 3
       |    case _root_.scala.Seq(y, _) => 4
       |    case _root_.scala.collection.immutable.Nil => 5
       |  }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n52 <= _empty_/n52.
             |[1:6..1:7): f <= _empty_/n52.f().
             |[1:8..1:9): x <= _empty_/n52.f().(x)
             |[1:11..1:14): Any => scala/Any#
             |[1:18..1:19): x => _empty_/n52.f().(x)
             |[2:16..2:21): scala => scala/
             |[2:22..2:25): Nil => scala/collection/immutable/Nil.
             |[3:16..3:21): scala => scala/
             |[3:22..3:26): List => scala/collection/immutable/List.
             |[3:27..3:28): y <= local1
             |[4:11..4:13): :: => scala/package.`::`.
             |[4:21..4:26): scala => scala/
             |[4:27..4:30): Nil => scala/collection/immutable/Nil.
             |[5:16..5:21): scala => scala/
             |[5:22..5:25): Seq => scala/collection/Seq.
             |[5:26..5:27): y <= local3
             |[6:16..6:21): scala => scala/
             |[6:22..6:32): collection => scala/collection/
             |[6:33..6:42): immutable => scala/collection/immutable/
             |[6:43..6:46): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n52 <= _empty_/n52.
         |[1:6..1:7): f <= _empty_/n52.f().
         |[1:8..1:9): x <= _empty_/n52.f().(x)
         |[1:11..1:14): Any => scala/Any#
         |[1:18..1:19): x => _empty_/n52.f().(x)
         |[2:16..2:21): scala => scala/
         |[2:22..2:25): Nil => scala/package.Nil.
         |[3:16..3:21): scala => scala/
         |[3:22..3:26): List => scala/package.List.
         |[3:27..3:28): y <= local1
         |[4:11..4:13): :: => scala/package.`::`.
         |[4:21..4:26): scala => scala/
         |[4:27..4:30): Nil => scala/package.Nil.
         |[5:16..5:21): scala => scala/
         |[5:22..5:25): Seq => scala/package.Seq.
         |[5:26..5:27): y <= local3
         |[6:16..6:21): scala => scala/
         |[6:22..6:32): collection => scala/collection/
         |[6:33..6:42): immutable => scala/collection/immutable/
         |[6:43..6:46): Nil => scala/collection/immutable/Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "right-associative operators on constants of other origins",
    """|object n53 {
       |  object O { final val K = 1; final val S = "a" }
       |  import O.{K => J}
       |  type I = Int
       |  final val A: I = 1
       |  final val K = 1
       |  final val C = 'c'
       |  final val T = true
       |  val a = O.K :: Nil
       |  val b = J :: Nil
       |  val c = A :: Nil
       |  val d = (K) :: Nil
       |  val e = ((K + 1)) :: Nil
       |  val f = classOf[String] :: Nil
       |  val g = java.lang.Integer.MAX_VALUE :: Nil
       |  val h = "a" + K :: Nil
       |  val i = K.toLong :: Nil
       |  val j = O.S +: Seq()
       |  val k = C :: Nil
       |  val l = !T :: Nil
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n53 <= _empty_/n53.
             |[1:9..1:10): O <= _empty_/n53.O.
             |[1:23..1:24): K <= _empty_/n53.O.K.
             |[1:40..1:41): S <= _empty_/n53.O.S.
             |[2:9..2:10): O => _empty_/n53.O.
             |[2:12..2:13): K => _empty_/n53.O.K.
             |[3:7..3:8): I <= _empty_/n53.I#
             |[3:11..3:14): Int => scala/Int#
             |[4:12..4:13): A <= _empty_/n53.A.
             |[4:15..4:16): I => _empty_/n53.I#
             |[5:12..5:13): K <= _empty_/n53.K.
             |[6:12..6:13): C <= _empty_/n53.C.
             |[7:12..7:13): T <= _empty_/n53.T.
             |[8:6..8:7): a <= _empty_/n53.a.
             |[8:10..8:11): O => _empty_/n53.O.
             |[8:12..8:13): K => _empty_/n53.O.K.
             |[8:14..8:16): :: => scala/collection/immutable/List#`::`().
             |[8:17..8:20): Nil => scala/collection/immutable/Nil.
             |[9:6..9:7): b <= _empty_/n53.b.
             |[9:10..9:11): J => _empty_/n53.O.K.
             |[9:12..9:14): :: => scala/collection/immutable/List#`::`().
             |[9:15..9:18): Nil => scala/collection/immutable/Nil.
             |[10:6..10:7): c <= _empty_/n53.c.
             |[10:10..10:11): A => _empty_/n53.A.
             |[10:12..10:14): :: => scala/collection/immutable/List#`::`().
             |[10:15..10:18): Nil => scala/collection/immutable/Nil.
             |[11:6..11:7): d <= _empty_/n53.d.
             |[11:11..11:12): K => _empty_/n53.K.
             |[11:14..11:16): :: => scala/collection/immutable/List#`::`().
             |[11:17..11:20): Nil => scala/collection/immutable/Nil.
             |[12:6..12:7): e <= _empty_/n53.e.
             |[12:12..12:13): K => _empty_/n53.K.
             |[12:14..12:15): + => scala/Int#`+`(+4).
             |[12:20..12:22): :: => scala/collection/immutable/List#`::`().
             |[12:23..12:26): Nil => scala/collection/immutable/Nil.
             |[13:6..13:7): f <= _empty_/n53.f.
             |[13:10..13:17): classOf => scala/Predef.classOf().
             |[13:18..13:24): String => scala/Predef.String#
             |[13:26..13:28): :: => scala/collection/immutable/List#`::`().
             |[13:29..13:32): Nil => scala/collection/immutable/Nil.
             |[14:6..14:7): g <= _empty_/n53.g.
             |[14:10..14:14): java => java/
             |[14:15..14:19): lang => java/lang/
             |[14:20..14:27): Integer => java/lang/Integer#
             |[14:28..14:37): MAX_VALUE => java/lang/Integer#MAX_VALUE.
             |[14:38..14:40): :: => scala/collection/immutable/List#`::`().
             |[14:41..14:44): Nil => scala/collection/immutable/Nil.
             |[15:6..15:7): h <= _empty_/n53.h.
             |[15:14..15:15): + => java/lang/String#`+`().
             |[15:16..15:17): K => _empty_/n53.K.
             |[15:18..15:20): :: => scala/collection/immutable/List#`::`().
             |[15:21..15:24): Nil => scala/collection/immutable/Nil.
             |[16:6..16:7): i <= _empty_/n53.i.
             |[16:10..16:11): K => _empty_/n53.K.
             |[16:12..16:18): toLong => scala/Int#toLong().
             |[16:19..16:21): :: => scala/collection/immutable/List#`::`().
             |[16:22..16:25): Nil => scala/collection/immutable/Nil.
             |[17:6..17:7): j <= _empty_/n53.j.
             |[17:10..17:11): O => _empty_/n53.O.
             |[17:12..17:13): S => _empty_/n53.O.S.
             |[17:14..17:16): +: => scala/collection/SeqLike#`+:`().
             |[17:17..17:20): Seq => scala/collection/Seq.
             |[18:6..18:7): k <= _empty_/n53.k.
             |[18:10..18:11): C => _empty_/n53.C.
             |[18:12..18:14): :: => scala/collection/immutable/List#`::`().
             |[18:15..18:18): Nil => scala/collection/immutable/Nil.
             |[19:6..19:7): l <= _empty_/n53.l.
             |[19:10..19:11): ! => scala/Boolean#`unary_!`().
             |[19:11..19:12): T => _empty_/n53.T.
             |[19:13..19:15): :: => scala/collection/immutable/List#`::`().
             |[19:16..19:19): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n53 <= _empty_/n53.
         |[1:9..1:10): O <= _empty_/n53.O.
         |[1:23..1:24): K <= _empty_/n53.O.K.
         |[1:40..1:41): S <= _empty_/n53.O.S.
         |[2:9..2:10): O => _empty_/n53.O.
         |[2:12..2:13): K => _empty_/n53.O.K.
         |[3:7..3:8): I <= _empty_/n53.I#
         |[3:11..3:14): Int => scala/Int#
         |[4:12..4:13): A <= _empty_/n53.A.
         |[4:15..4:16): I => _empty_/n53.I#
         |[5:12..5:13): K <= _empty_/n53.K.
         |[6:12..6:13): C <= _empty_/n53.C.
         |[7:12..7:13): T <= _empty_/n53.T.
         |[8:6..8:7): a <= _empty_/n53.a.
         |[8:10..8:11): O => _empty_/n53.O.
         |[8:12..8:13): K => _empty_/n53.O.K.
         |[8:14..8:16): :: => scala/collection/immutable/List#`::`().
         |[8:17..8:20): Nil => scala/package.Nil.
         |[9:6..9:7): b <= _empty_/n53.b.
         |[9:10..9:11): J => _empty_/n53.O.K.
         |[9:12..9:14): :: => scala/collection/immutable/List#`::`().
         |[9:15..9:18): Nil => scala/package.Nil.
         |[10:6..10:7): c <= _empty_/n53.c.
         |[10:10..10:11): A => _empty_/n53.A.
         |[10:12..10:14): :: => scala/collection/immutable/List#`::`().
         |[10:15..10:18): Nil => scala/package.Nil.
         |[11:6..11:7): d <= _empty_/n53.d.
         |[11:11..11:12): K => _empty_/n53.K.
         |[11:14..11:16): :: => scala/collection/immutable/List#`::`().
         |[11:17..11:20): Nil => scala/package.Nil.
         |[12:6..12:7): e <= _empty_/n53.e.
         |[12:12..12:13): K => _empty_/n53.K.
         |[12:14..12:15): + => scala/Int#`+`(+4).
         |[12:20..12:22): :: => scala/collection/immutable/List#`::`().
         |[12:23..12:26): Nil => scala/package.Nil.
         |[13:6..13:7): f <= _empty_/n53.f.
         |[13:10..13:17): classOf => scala/Predef.classOf().
         |[13:18..13:24): String => scala/Predef.String#
         |[13:26..13:28): :: => scala/collection/immutable/List#`::`().
         |[13:29..13:32): Nil => scala/package.Nil.
         |[14:6..14:7): g <= _empty_/n53.g.
         |[14:10..14:14): java => java/
         |[14:15..14:19): lang => java/lang/
         |[14:20..14:27): Integer => java/lang/Integer#
         |[14:28..14:37): MAX_VALUE => java/lang/Integer#MAX_VALUE.
         |[14:38..14:40): :: => scala/collection/immutable/List#`::`().
         |[14:41..14:44): Nil => scala/package.Nil.
         |[15:6..15:7): h <= _empty_/n53.h.
         |[15:14..15:15): + => java/lang/String#`+`().
         |[15:16..15:17): K => _empty_/n53.K.
         |[15:18..15:20): :: => scala/collection/immutable/List#`::`().
         |[15:21..15:24): Nil => scala/package.Nil.
         |[16:6..16:7): i <= _empty_/n53.i.
         |[16:10..16:11): K => _empty_/n53.K.
         |[16:12..16:18): toLong => scala/Int#toLong().
         |[16:19..16:21): :: => scala/collection/immutable/List#`::`().
         |[16:22..16:25): Nil => scala/package.Nil.
         |[17:6..17:7): j <= _empty_/n53.j.
         |[17:10..17:11): O => _empty_/n53.O.
         |[17:12..17:13): S => _empty_/n53.O.S.
         |[17:14..17:16): +: => scala/collection/SeqOps#`+:`().
         |[17:17..17:20): Seq => scala/package.Seq.
         |[18:6..18:7): k <= _empty_/n53.k.
         |[18:10..18:11): C => _empty_/n53.C.
         |[18:12..18:14): :: => scala/collection/immutable/List#`::`().
         |[18:15..18:18): Nil => scala/package.Nil.
         |[19:6..19:7): l <= _empty_/n53.l.
         |[19:10..19:11): ! => scala/Boolean#`unary_!`().
         |[19:11..19:12): T => _empty_/n53.T.
         |[19:13..19:15): :: => scala/collection/immutable/List#`::`().
         |[19:16..19:19): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "right-associative operator kinds on constants",
    """|object n54 {
       |  final val K = 1
       |  class R { def ->:[A](a: A) = this; def *:(i: => Int) = this }
       |  val r = new R
       |  val buf = scala.collection.mutable.ListBuffer.empty[Int]
       |  val a = (K :: Nil) ::: Nil
       |  val b = List(K) ::: Nil
       |  val c = K +=: buf
       |  val d = K ->: r
       |  val e = K ->:[Int] r
       |  val f = K *: r
       |  val g = Nil.::(K)
       |  val h = K ::[Int] Nil
       |  val i = K +: Vector()
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n54 <= _empty_/n54.
             |[1:12..1:13): K <= _empty_/n54.K.
             |[2:8..2:9): R <= _empty_/n54.R#
             |[2:10..2:10):  <= _empty_/n54.R#`<init>`().
             |[2:16..2:19): ->: <= _empty_/n54.R#`->:`().
             |[2:20..2:21): A <= _empty_/n54.R#`->:`().[A]
             |[2:23..2:24): a <= _empty_/n54.R#`->:`().(a)
             |[2:26..2:27): A => _empty_/n54.R#`->:`().[A]
             |[2:41..2:43): *: <= _empty_/n54.R#`*:`().
             |[2:44..2:45): i <= _empty_/n54.R#`*:`().(i)
             |[2:50..2:53): Int => scala/Int#
             |[3:6..3:7): r <= _empty_/n54.r.
             |[3:14..3:15): R => _empty_/n54.R#
             |[3:15..3:15):  => _empty_/n54.R#`<init>`().
             |[4:6..4:9): buf <= _empty_/n54.buf.
             |[4:12..4:17): scala => scala/
             |[4:18..4:28): collection => scala/collection/
             |[4:29..4:36): mutable => scala/collection/mutable/
             |[4:37..4:47): ListBuffer => scala/collection/mutable/ListBuffer.
             |[4:48..4:53): empty => scala/collection/generic/GenericCompanion#empty().
             |[4:54..4:57): Int => scala/Int#
             |[5:6..5:7): a <= _empty_/n54.a.
             |[5:11..5:12): K => _empty_/n54.K.
             |[5:13..5:15): :: => scala/collection/immutable/List#`::`().
             |[5:16..5:19): Nil => scala/collection/immutable/Nil.
             |[5:21..5:24): ::: => scala/collection/immutable/List#`:::`().
             |[5:25..5:28): Nil => scala/collection/immutable/Nil.
             |[6:6..6:7): b <= _empty_/n54.b.
             |[6:10..6:14): List => scala/collection/immutable/List.
             |[6:15..6:16): K => _empty_/n54.K.
             |[6:18..6:21): ::: => scala/collection/immutable/List#`:::`().
             |[6:22..6:25): Nil => scala/collection/immutable/Nil.
             |[7:6..7:7): c <= _empty_/n54.c.
             |[7:10..7:11): K => _empty_/n54.K.
             |[7:12..7:15): +=: => scala/collection/mutable/ListBuffer#`+=:`().
             |[7:16..7:19): buf => _empty_/n54.buf.
             |[8:6..8:7): d <= _empty_/n54.d.
             |[8:10..8:11): K => _empty_/n54.K.
             |[8:12..8:15): ->: => _empty_/n54.R#`->:`().
             |[8:16..8:17): r => _empty_/n54.r.
             |[9:6..9:7): e <= _empty_/n54.e.
             |[9:10..9:11): K => _empty_/n54.K.
             |[9:12..9:15): ->: => _empty_/n54.R#`->:`().
             |[9:16..9:19): Int => scala/Int#
             |[9:21..9:22): r => _empty_/n54.r.
             |[10:6..10:7): f <= _empty_/n54.f.
             |[10:10..10:11): K => _empty_/n54.K.
             |[10:12..10:14): *: => _empty_/n54.R#`*:`().
             |[10:15..10:16): r => _empty_/n54.r.
             |[11:6..11:7): g <= _empty_/n54.g.
             |[11:10..11:13): Nil => scala/collection/immutable/Nil.
             |[11:14..11:16): :: => scala/collection/immutable/List#`::`().
             |[11:17..11:18): K => _empty_/n54.K.
             |[12:6..12:7): h <= _empty_/n54.h.
             |[12:10..12:11): K => _empty_/n54.K.
             |[12:12..12:14): :: => scala/collection/immutable/List#`::`().
             |[12:15..12:18): Int => scala/Int#
             |[12:20..12:23): Nil => scala/collection/immutable/Nil.
             |[13:6..13:7): i <= _empty_/n54.i.
             |[13:10..13:11): K => _empty_/n54.K.
             |[13:12..13:14): +: => scala/collection/immutable/Vector#`+:`().
             |[13:15..13:21): Vector => scala/package.Vector.
             |""".stripMargin
      },
      """|[0:7..0:10): n54 <= _empty_/n54.
         |[1:12..1:13): K <= _empty_/n54.K.
         |[2:8..2:9): R <= _empty_/n54.R#
         |[2:10..2:10):  <= _empty_/n54.R#`<init>`().
         |[2:16..2:19): ->: <= _empty_/n54.R#`->:`().
         |[2:20..2:21): A <= _empty_/n54.R#`->:`().[A]
         |[2:23..2:24): a <= _empty_/n54.R#`->:`().(a)
         |[2:26..2:27): A => _empty_/n54.R#`->:`().[A]
         |[2:41..2:43): *: <= _empty_/n54.R#`*:`().
         |[2:44..2:45): i <= _empty_/n54.R#`*:`().(i)
         |[2:50..2:53): Int => scala/Int#
         |[3:6..3:7): r <= _empty_/n54.r.
         |[3:14..3:15): R => _empty_/n54.R#
         |[3:15..3:15):  => _empty_/n54.R#`<init>`().
         |[4:6..4:9): buf <= _empty_/n54.buf.
         |[4:12..4:17): scala => scala/
         |[4:18..4:28): collection => scala/collection/
         |[4:29..4:36): mutable => scala/collection/mutable/
         |[4:37..4:47): ListBuffer => scala/collection/mutable/ListBuffer.
         |[4:48..4:53): empty => scala/collection/mutable/ListBuffer.empty().
         |[4:54..4:57): Int => scala/Int#
         |[5:6..5:7): a <= _empty_/n54.a.
         |[5:11..5:12): K => _empty_/n54.K.
         |[5:13..5:15): :: => scala/collection/immutable/List#`::`().
         |[5:16..5:19): Nil => scala/package.Nil.
         |[5:21..5:24): ::: => scala/collection/immutable/List#`:::`().
         |[5:25..5:28): Nil => scala/package.Nil.
         |[6:6..6:7): b <= _empty_/n54.b.
         |[6:10..6:14): List => scala/package.List.
         |[6:15..6:16): K => _empty_/n54.K.
         |[6:18..6:21): ::: => scala/collection/immutable/List#`:::`().
         |[6:22..6:25): Nil => scala/package.Nil.
         |[7:6..7:7): c <= _empty_/n54.c.
         |[7:10..7:11): K => _empty_/n54.K.
         |[7:12..7:15): +=: => scala/collection/mutable/Buffer#`+=:`().
         |[7:16..7:19): buf => _empty_/n54.buf.
         |[8:6..8:7): d <= _empty_/n54.d.
         |[8:10..8:11): K => _empty_/n54.K.
         |[8:12..8:15): ->: => _empty_/n54.R#`->:`().
         |[8:16..8:17): r => _empty_/n54.r.
         |[9:6..9:7): e <= _empty_/n54.e.
         |[9:10..9:11): K => _empty_/n54.K.
         |[9:12..9:15): ->: => _empty_/n54.R#`->:`().
         |[9:16..9:19): Int => scala/Int#
         |[9:21..9:22): r => _empty_/n54.r.
         |[10:6..10:7): f <= _empty_/n54.f.
         |[10:10..10:11): K => _empty_/n54.K.
         |[10:12..10:14): *: => _empty_/n54.R#`*:`().
         |[10:15..10:16): r => _empty_/n54.r.
         |[11:6..11:7): g <= _empty_/n54.g.
         |[11:10..11:13): Nil => scala/package.Nil.
         |[11:14..11:16): :: => scala/collection/immutable/List#`::`().
         |[11:17..11:18): K => _empty_/n54.K.
         |[12:6..12:7): h <= _empty_/n54.h.
         |[12:10..12:11): K => _empty_/n54.K.
         |[12:12..12:14): :: => scala/collection/immutable/List#`::`().
         |[12:15..12:18): Int => scala/Int#
         |[12:20..12:23): Nil => scala/package.Nil.
         |[13:6..13:7): i <= _empty_/n54.i.
         |[13:10..13:11): K => _empty_/n54.K.
         |[13:12..13:14): +: => scala/collection/SeqOps#`+:`().
         |[13:15..13:21): Vector => scala/package.Vector.
         |""".stripMargin,
    ),
  )

  occurrences(
    "right-associative operators on constants in other places",
    """|object n55 {
       |  final val K = 1
       |  def f(l: List[Int]) = l
       |  def g = { K :: Nil; K +: Vector(); () }
       |  val a = f(K :: Nil)
       |  val b = f(l = K :: Nil)
       |  var l = List(1)
       |  def h = { l ::= K; l +:= K }
       |  def m(x: Any) = x match { case K :: _ => 1; case _ => 2 }
       |  val c = if (true) K :: Nil else Nil
       |  val d = List(K :: Nil)
       |  def n = K :: Nil
       |  lazy val e = K :: Nil
       |  val o = for (i <- K :: Nil) yield i
       |  class B(l: List[Int])
       |  class D extends B(K :: Nil)
       |  def p(l: List[Int] = K :: Nil) = l
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n55 <= _empty_/n55.
             |[1:12..1:13): K <= _empty_/n55.K.
             |[2:6..2:7): f <= _empty_/n55.f().
             |[2:8..2:9): l <= _empty_/n55.f().(l)
             |[2:11..2:15): List => scala/package.List#
             |[2:16..2:19): Int => scala/Int#
             |[2:24..2:25): l => _empty_/n55.f().(l)
             |[3:6..3:7): g <= _empty_/n55.g().
             |[3:12..3:13): K => _empty_/n55.K.
             |[3:14..3:16): :: => scala/collection/immutable/List#`::`().
             |[3:17..3:20): Nil => scala/collection/immutable/Nil.
             |[3:22..3:23): K => _empty_/n55.K.
             |[3:24..3:26): +: => scala/collection/immutable/Vector#`+:`().
             |[3:27..3:33): Vector => scala/package.Vector.
             |[4:6..4:7): a <= _empty_/n55.a.
             |[4:10..4:11): f => _empty_/n55.f().
             |[4:12..4:13): K => _empty_/n55.K.
             |[4:14..4:16): :: => scala/collection/immutable/List#`::`().
             |[4:17..4:20): Nil => scala/collection/immutable/Nil.
             |[5:6..5:7): b <= _empty_/n55.b.
             |[5:10..5:11): f => _empty_/n55.f().
             |[5:12..5:13): l => _empty_/n55.f().(l)
             |[5:16..5:17): K => _empty_/n55.K.
             |[5:18..5:20): :: => scala/collection/immutable/List#`::`().
             |[5:21..5:24): Nil => scala/collection/immutable/Nil.
             |[6:6..6:7): l <= _empty_/n55.l().
             |[6:10..6:14): List => scala/collection/immutable/List.
             |[7:6..7:7): h <= _empty_/n55.h().
             |[7:12..7:13): l => _empty_/n55.`l_=`().
             |[7:14..7:17): ::= => scala/collection/immutable/List#`::`().
             |[7:18..7:19): K => _empty_/n55.K.
             |[7:21..7:22): l => _empty_/n55.`l_=`().
             |[7:23..7:26): +:= => scala/collection/immutable/List#`+:`().
             |[7:27..7:28): K => _empty_/n55.K.
             |[8:6..8:7): m <= _empty_/n55.m().
             |[8:8..8:9): x <= _empty_/n55.m().(x)
             |[8:11..8:14): Any => scala/Any#
             |[8:18..8:19): x => _empty_/n55.m().(x)
             |[8:33..8:34): K => _empty_/n55.K.
             |[8:35..8:37): :: => scala/package.`::`.
             |[9:6..9:7): c <= _empty_/n55.c.
             |[9:20..9:21): K => _empty_/n55.K.
             |[9:22..9:24): :: => scala/collection/immutable/List#`::`().
             |[9:25..9:28): Nil => scala/collection/immutable/Nil.
             |[9:34..9:37): Nil => scala/collection/immutable/Nil.
             |[10:6..10:7): d <= _empty_/n55.d.
             |[10:10..10:14): List => scala/collection/immutable/List.
             |[10:15..10:16): K => _empty_/n55.K.
             |[10:17..10:19): :: => scala/collection/immutable/List#`::`().
             |[10:20..10:23): Nil => scala/collection/immutable/Nil.
             |[11:6..11:7): n <= _empty_/n55.n().
             |[11:10..11:11): K => _empty_/n55.K.
             |[11:12..11:14): :: => scala/collection/immutable/List#`::`().
             |[11:15..11:18): Nil => scala/collection/immutable/Nil.
             |[12:11..12:12): e <= _empty_/n55.e.
             |[12:15..12:16): K => _empty_/n55.K.
             |[12:17..12:19): :: => scala/collection/immutable/List#`::`().
             |[12:20..12:23): Nil => scala/collection/immutable/Nil.
             |[13:6..13:7): o <= _empty_/n55.o.
             |[13:15..13:16): i <= local0
             |[13:20..13:21): K => _empty_/n55.K.
             |[13:22..13:24): :: => scala/collection/immutable/List#`::`().
             |[13:25..13:28): Nil => scala/collection/immutable/Nil.
             |[13:36..13:37): i => local0
             |[14:8..14:9): B <= _empty_/n55.B#
             |[14:9..14:9):  <= _empty_/n55.B#`<init>`().
             |[14:10..14:11): l <= _empty_/n55.B#`<init>`().(l)
             |[14:10..14:11): l <= _empty_/n55.B#l.
             |[14:13..14:17): List => scala/package.List#
             |[14:18..14:21): Int => scala/Int#
             |[15:8..15:9): D <= _empty_/n55.D#
             |[15:10..15:10):  <= _empty_/n55.D#`<init>`().
             |[15:18..15:19): B => _empty_/n55.B#
             |[15:19..15:19):  => _empty_/n55.B#`<init>`().
             |[15:20..15:21): K => _empty_/n55.K.
             |[15:22..15:24): :: => scala/collection/immutable/List#`::`().
             |[15:25..15:28): Nil => scala/collection/immutable/Nil.
             |[16:6..16:7): p <= _empty_/n55.p().
             |[16:8..16:9): l <= _empty_/n55.p().(l)
             |[16:11..16:15): List => scala/package.List#
             |[16:16..16:19): Int => scala/Int#
             |[16:23..16:24): K => _empty_/n55.K.
             |[16:25..16:27): :: => scala/collection/immutable/List#`::`().
             |[16:28..16:31): Nil => scala/collection/immutable/Nil.
             |[16:35..16:36): l => _empty_/n55.p().(l)
             |""".stripMargin
      },
      """|[0:7..0:10): n55 <= _empty_/n55.
         |[1:12..1:13): K <= _empty_/n55.K.
         |[2:6..2:7): f <= _empty_/n55.f().
         |[2:8..2:9): l <= _empty_/n55.f().(l)
         |[2:11..2:15): List => scala/package.List#
         |[2:16..2:19): Int => scala/Int#
         |[2:24..2:25): l => _empty_/n55.f().(l)
         |[3:6..3:7): g <= _empty_/n55.g().
         |[3:12..3:13): K => _empty_/n55.K.
         |[3:14..3:16): :: => scala/collection/immutable/List#`::`().
         |[3:17..3:20): Nil => scala/package.Nil.
         |[3:22..3:23): K => _empty_/n55.K.
         |[3:24..3:26): +: => scala/collection/SeqOps#`+:`().
         |[3:27..3:33): Vector => scala/package.Vector.
         |[4:6..4:7): a <= _empty_/n55.a.
         |[4:10..4:11): f => _empty_/n55.f().
         |[4:12..4:13): K => _empty_/n55.K.
         |[4:14..4:16): :: => scala/collection/immutable/List#`::`().
         |[4:17..4:20): Nil => scala/package.Nil.
         |[5:6..5:7): b <= _empty_/n55.b.
         |[5:10..5:11): f => _empty_/n55.f().
         |[5:12..5:13): l => _empty_/n55.f().(l)
         |[5:16..5:17): K => _empty_/n55.K.
         |[5:18..5:20): :: => scala/collection/immutable/List#`::`().
         |[5:21..5:24): Nil => scala/package.Nil.
         |[6:6..6:7): l <= _empty_/n55.l().
         |[6:10..6:14): List => scala/package.List.
         |[7:6..7:7): h <= _empty_/n55.h().
         |[7:12..7:13): l => _empty_/n55.`l_=`().
         |[7:14..7:17): ::= => scala/collection/immutable/List#`::`().
         |[7:18..7:19): K => _empty_/n55.K.
         |[7:21..7:22): l => _empty_/n55.`l_=`().
         |[7:23..7:26): +:= => scala/collection/SeqOps#`+:`().
         |[7:27..7:28): K => _empty_/n55.K.
         |[8:6..8:7): m <= _empty_/n55.m().
         |[8:8..8:9): x <= _empty_/n55.m().(x)
         |[8:11..8:14): Any => scala/Any#
         |[8:18..8:19): x => _empty_/n55.m().(x)
         |[8:33..8:34): K => _empty_/n55.K.
         |[8:35..8:37): :: => scala/package.`::`.
         |[9:6..9:7): c <= _empty_/n55.c.
         |[9:20..9:21): K => _empty_/n55.K.
         |[9:22..9:24): :: => scala/collection/immutable/List#`::`().
         |[9:25..9:28): Nil => scala/package.Nil.
         |[9:34..9:37): Nil => scala/package.Nil.
         |[10:6..10:7): d <= _empty_/n55.d.
         |[10:10..10:14): List => scala/package.List.
         |[10:15..10:16): K => _empty_/n55.K.
         |[10:17..10:19): :: => scala/collection/immutable/List#`::`().
         |[10:20..10:23): Nil => scala/package.Nil.
         |[11:6..11:7): n <= _empty_/n55.n().
         |[11:10..11:11): K => _empty_/n55.K.
         |[11:12..11:14): :: => scala/collection/immutable/List#`::`().
         |[11:15..11:18): Nil => scala/package.Nil.
         |[12:11..12:12): e <= _empty_/n55.e.
         |[12:15..12:16): K => _empty_/n55.K.
         |[12:17..12:19): :: => scala/collection/immutable/List#`::`().
         |[12:20..12:23): Nil => scala/package.Nil.
         |[13:6..13:7): o <= _empty_/n55.o.
         |[13:15..13:16): i <= local0
         |[13:20..13:21): K => _empty_/n55.K.
         |[13:22..13:24): :: => scala/collection/immutable/List#`::`().
         |[13:25..13:28): Nil => scala/package.Nil.
         |[13:36..13:37): i => local0
         |[14:8..14:9): B <= _empty_/n55.B#
         |[14:9..14:9):  <= _empty_/n55.B#`<init>`().
         |[14:10..14:11): l <= _empty_/n55.B#`<init>`().(l)
         |[14:10..14:11): l <= _empty_/n55.B#l.
         |[14:13..14:17): List => scala/package.List#
         |[14:18..14:21): Int => scala/Int#
         |[15:8..15:9): D <= _empty_/n55.D#
         |[15:10..15:10):  <= _empty_/n55.D#`<init>`().
         |[15:18..15:19): B => _empty_/n55.B#
         |[15:19..15:19):  => _empty_/n55.B#`<init>`().
         |[15:20..15:21): K => _empty_/n55.K.
         |[15:22..15:24): :: => scala/collection/immutable/List#`::`().
         |[15:25..15:28): Nil => scala/package.Nil.
         |[16:6..16:7): p <= _empty_/n55.p().
         |[16:8..16:9): l <= _empty_/n55.p().(l)
         |[16:11..16:15): List => scala/package.List#
         |[16:16..16:19): Int => scala/Int#
         |[16:23..16:24): K => _empty_/n55.K.
         |[16:25..16:27): :: => scala/collection/immutable/List#`::`().
         |[16:28..16:31): Nil => scala/package.Nil.
         |[16:35..16:36): l => _empty_/n55.p().(l)
         |""".stripMargin,
    ),
  )

  occurrences(
    "named and default arguments with constants",
    """|object n56 {
       |  final val K = 1
       |  val x = 1
       |  def f(a: Int, b: Int) = a
       |  def g(a: Int = K + 1, b: Int = 0) = a
       |  case class P(a: Int, b: Int)
       |  val a = f(b = K + 1, a = x)
       |  val b = f(b = Int.MaxValue, a = -K)
       |  val c = g(b = K)
       |  val d = g()
       |  val e = P(1, 2).copy(b = K + 1)
       |}
       |""".stripMargin,
    """|[0:7..0:10): n56 <= _empty_/n56.
       |[1:12..1:13): K <= _empty_/n56.K.
       |[2:6..2:7): x <= _empty_/n56.x.
       |[3:6..3:7): f <= _empty_/n56.f().
       |[3:8..3:9): a <= _empty_/n56.f().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/n56.f().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/n56.f().(a)
       |[4:6..4:7): g <= _empty_/n56.g().
       |[4:8..4:9): a <= _empty_/n56.g().(a)
       |[4:11..4:14): Int => scala/Int#
       |[4:17..4:18): K => _empty_/n56.K.
       |[4:19..4:20): + => scala/Int#`+`(+4).
       |[4:24..4:25): b <= _empty_/n56.g().(b)
       |[4:27..4:30): Int => scala/Int#
       |[4:38..4:39): a => _empty_/n56.g().(a)
       |[5:13..5:14): P <= _empty_/n56.P#
       |[5:14..5:14):  <= _empty_/n56.P#`<init>`().
       |[5:15..5:16): a <= _empty_/n56.P#`<init>`().(a)
       |[5:15..5:16): a <= _empty_/n56.P#a.
       |[5:18..5:21): Int => scala/Int#
       |[5:23..5:24): b <= _empty_/n56.P#`<init>`().(b)
       |[5:23..5:24): b <= _empty_/n56.P#b.
       |[5:26..5:29): Int => scala/Int#
       |[6:6..6:7): a <= _empty_/n56.a.
       |[6:10..6:11): f => _empty_/n56.f().
       |[6:12..6:13): b => _empty_/n56.f().(b)
       |[6:16..6:17): K => _empty_/n56.K.
       |[6:18..6:19): + => scala/Int#`+`(+4).
       |[6:23..6:24): a => _empty_/n56.f().(a)
       |[6:27..6:28): x => _empty_/n56.x.
       |[7:6..7:7): b <= _empty_/n56.b.
       |[7:10..7:11): f => _empty_/n56.f().
       |[7:12..7:13): b => _empty_/n56.f().(b)
       |[7:16..7:19): Int => scala/Int.
       |[7:20..7:28): MaxValue => scala/Int.MaxValue.
       |[7:30..7:31): a => _empty_/n56.f().(a)
       |[7:34..7:35): - => scala/Int#`unary_-`().
       |[7:35..7:36): K => _empty_/n56.K.
       |[8:6..8:7): c <= _empty_/n56.c.
       |[8:10..8:11): g => _empty_/n56.g().
       |[8:12..8:13): b => _empty_/n56.g().(b)
       |[8:16..8:17): K => _empty_/n56.K.
       |[9:6..9:7): d <= _empty_/n56.d.
       |[9:10..9:11): g => _empty_/n56.g().
       |[10:6..10:7): e <= _empty_/n56.e.
       |[10:10..10:11): P => _empty_/n56.P.
       |[10:18..10:22): copy => _empty_/n56.P#copy().
       |[10:23..10:24): b => _empty_/n56.P#copy().(b)
       |[10:27..10:28): K => _empty_/n56.K.
       |[10:29..10:30): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  occurrences(
    "interpolators with constants",
    """|object n57 {
       |  final val K = 1
       |  final val S = "a"
       |  val a = s"$K"
       |  val b = s"a${K + 1}b$S"
       |  val c = raw"$S"
       |}
       |""".stripMargin,
    """|[0:7..0:10): n57 <= _empty_/n57.
       |[1:12..1:13): K <= _empty_/n57.K.
       |[2:12..2:13): S <= _empty_/n57.S.
       |[3:6..3:7): a <= _empty_/n57.a.
       |[3:10..3:11): s => scala/StringContext#s().
       |[3:13..3:14): K => _empty_/n57.K.
       |[4:6..4:7): b <= _empty_/n57.b.
       |[4:10..4:11): s => scala/StringContext#s().
       |[4:15..4:16): K => _empty_/n57.K.
       |[4:17..4:18): + => scala/Int#`+`(+4).
       |[4:23..4:24): S => _empty_/n57.S.
       |[5:6..5:7): c <= _empty_/n57.c.
       |[5:10..5:13): raw => scala/StringContext#raw().
       |[5:15..5:16): S => _empty_/n57.S.
       |""".stripMargin,
  )

  occurrences(
    "right-associative operators on folded and adapted constants",
    """|object n58 {
       |  class A(l: List[Any]) extends scala.annotation.StaticAnnotation
       |  final val K = 1
       |  final val S = "a"
       |  val a = "a" + S :: Nil
       |  val b = K + 1L :: Nil
       |  val c = K :: (Nil: List[Long])
       |  val d = { K } :: Nil
       |  val e = 1 + 1 :: Nil
       |  val f = s"${K :: Nil}"
       |  @A(K :: Nil) val g = 1
       |  val h = S + "b" :: Nil
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n58 <= _empty_/n58.
             |[1:8..1:9): A <= _empty_/n58.A#
             |[1:9..1:9):  <= _empty_/n58.A#`<init>`().
             |[1:10..1:11): l <= _empty_/n58.A#`<init>`().(l)
             |[1:10..1:11): l <= _empty_/n58.A#l.
             |[1:13..1:17): List => scala/package.List#
             |[1:18..1:21): Any => scala/Any#
             |[1:32..1:37): scala => scala/
             |[1:38..1:48): annotation => scala/annotation/
             |[1:49..1:65): StaticAnnotation => scala/annotation/StaticAnnotation#
             |[1:65..1:65):  => scala/annotation/Annotation#`<init>`().
             |[2:12..2:13): K <= _empty_/n58.K.
             |[3:12..3:13): S <= _empty_/n58.S.
             |[4:6..4:7): a <= _empty_/n58.a.
             |[4:14..4:15): + => java/lang/String#`+`().
             |[4:16..4:17): S => _empty_/n58.S.
             |[4:18..4:20): :: => scala/collection/immutable/List#`::`().
             |[4:21..4:24): Nil => scala/collection/immutable/Nil.
             |[5:6..5:7): b <= _empty_/n58.b.
             |[5:10..5:11): K => _empty_/n58.K.
             |[5:12..5:13): + => scala/Int#`+`(+5).
             |[5:17..5:19): :: => scala/collection/immutable/List#`::`().
             |[5:20..5:23): Nil => scala/collection/immutable/Nil.
             |[6:6..6:7): c <= _empty_/n58.c.
             |[6:10..6:11): K => _empty_/n58.K.
             |[6:12..6:14): :: => scala/collection/immutable/List#`::`().
             |[6:16..6:19): Nil => scala/collection/immutable/Nil.
             |[6:21..6:25): List => scala/package.List#
             |[6:26..6:30): Long => scala/Long#
             |[7:6..7:7): d <= _empty_/n58.d.
             |[7:12..7:13): K => _empty_/n58.K.
             |[7:16..7:18): :: => scala/collection/immutable/List#`::`().
             |[7:19..7:22): Nil => scala/collection/immutable/Nil.
             |[8:6..8:7): e <= _empty_/n58.e.
             |[8:12..8:13): + => scala/Int#`+`(+4).
             |[8:16..8:18): :: => scala/collection/immutable/List#`::`().
             |[8:19..8:22): Nil => scala/collection/immutable/Nil.
             |[9:6..9:7): f <= _empty_/n58.f.
             |[9:10..9:11): s => scala/StringContext#s().
             |[9:14..9:15): K => _empty_/n58.K.
             |[9:16..9:18): :: => scala/collection/immutable/List#`::`().
             |[9:19..9:22): Nil => scala/collection/immutable/Nil.
             |[10:3..10:4): A => _empty_/n58.A#
             |[10:4..10:4):  => _empty_/n58.A#`<init>`().
             |[10:5..10:6): K => _empty_/n58.K.
             |[10:7..10:9): :: => scala/collection/immutable/List#`::`().
             |[10:10..10:13): Nil => scala/collection/immutable/Nil.
             |[10:19..10:20): g <= _empty_/n58.g.
             |[11:6..11:7): h <= _empty_/n58.h.
             |[11:10..11:11): S => _empty_/n58.S.
             |[11:12..11:13): + => java/lang/String#`+`().
             |[11:18..11:20): :: => scala/collection/immutable/List#`::`().
             |[11:21..11:24): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n58 <= _empty_/n58.
         |[1:8..1:9): A <= _empty_/n58.A#
         |[1:9..1:9):  <= _empty_/n58.A#`<init>`().
         |[1:10..1:11): l <= _empty_/n58.A#`<init>`().(l)
         |[1:10..1:11): l <= _empty_/n58.A#l.
         |[1:13..1:17): List => scala/package.List#
         |[1:18..1:21): Any => scala/Any#
         |[1:32..1:37): scala => scala/
         |[1:38..1:48): annotation => scala/annotation/
         |[1:49..1:65): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:65..1:65):  => scala/annotation/Annotation#`<init>`().
         |[2:12..2:13): K <= _empty_/n58.K.
         |[3:12..3:13): S <= _empty_/n58.S.
         |[4:6..4:7): a <= _empty_/n58.a.
         |[4:14..4:15): + => java/lang/String#`+`().
         |[4:16..4:17): S => _empty_/n58.S.
         |[4:18..4:20): :: => scala/collection/immutable/List#`::`().
         |[4:21..4:24): Nil => scala/package.Nil.
         |[5:6..5:7): b <= _empty_/n58.b.
         |[5:10..5:11): K => _empty_/n58.K.
         |[5:12..5:13): + => scala/Int#`+`(+5).
         |[5:17..5:19): :: => scala/collection/immutable/List#`::`().
         |[5:20..5:23): Nil => scala/package.Nil.
         |[6:6..6:7): c <= _empty_/n58.c.
         |[6:10..6:11): K => _empty_/n58.K.
         |[6:12..6:14): :: => scala/collection/immutable/List#`::`().
         |[6:16..6:19): Nil => scala/package.Nil.
         |[6:21..6:25): List => scala/package.List#
         |[6:26..6:30): Long => scala/Long#
         |[7:6..7:7): d <= _empty_/n58.d.
         |[7:12..7:13): K => _empty_/n58.K.
         |[7:16..7:18): :: => scala/collection/immutable/List#`::`().
         |[7:19..7:22): Nil => scala/package.Nil.
         |[8:6..8:7): e <= _empty_/n58.e.
         |[8:12..8:13): + => scala/Int#`+`(+4).
         |[8:16..8:18): :: => scala/collection/immutable/List#`::`().
         |[8:19..8:22): Nil => scala/package.Nil.
         |[9:6..9:7): f <= _empty_/n58.f.
         |[9:10..9:11): s => scala/StringContext#s().
         |[9:14..9:15): K => _empty_/n58.K.
         |[9:16..9:18): :: => scala/collection/immutable/List#`::`().
         |[9:19..9:22): Nil => scala/package.Nil.
         |[10:3..10:4): A => _empty_/n58.A#
         |[10:4..10:4):  => _empty_/n58.A#`<init>`().
         |[10:5..10:6): K => _empty_/n58.K.
         |[10:7..10:9): :: => scala/collection/immutable/List#`::`().
         |[10:10..10:13): Nil => scala/package.Nil.
         |[10:19..10:20): g <= _empty_/n58.g.
         |[11:6..11:7): h <= _empty_/n58.h.
         |[11:10..11:11): S => _empty_/n58.S.
         |[11:12..11:13): + => java/lang/String#`+`().
         |[11:18..11:20): :: => scala/collection/immutable/List#`::`().
         |[11:21..11:24): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  if (ScalaVersion.is213) occurrences(
    "right-associative operators on literal-typed constants",
    """|object n59 {
       |  final val K = 1
       |  final val L: 1 = 1
       |  val a = (K: 1) :: Nil
       |  val b = L :: Nil
       |}
       |""".stripMargin,
    """|[0:7..0:10): n59 <= _empty_/n59.
       |[1:12..1:13): K <= _empty_/n59.K.
       |[2:12..2:13): L <= _empty_/n59.L.
       |[3:6..3:7): a <= _empty_/n59.a.
       |[3:11..3:12): K => _empty_/n59.K.
       |[3:17..3:19): :: => scala/collection/immutable/List#`::`().
       |[3:20..3:23): Nil => scala/package.Nil.
       |[4:6..4:7): b <= _empty_/n59.b.
       |[4:10..4:11): L => _empty_/n59.L.
       |[4:12..4:14): :: => scala/collection/immutable/List#`::`().
       |[4:15..4:18): Nil => scala/package.Nil.
       |""".stripMargin,
  )

  occurrences(
    "pattern definitions with right-associative operators on constants",
    """|object n60 {
       |  final val K = 1
       |  val (p, q) = (K :: Nil, 1)
       |  def m = { val (r, s) = (K :: Nil, 1); r }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n60 <= _empty_/n60.
             |[1:12..1:13): K <= _empty_/n60.K.
             |[2:7..2:8): p <= _empty_/n60.p.
             |[2:10..2:11): q <= _empty_/n60.q.
             |[2:16..2:17): K => _empty_/n60.K.
             |[2:18..2:20): :: => scala/collection/immutable/List#`::`().
             |[2:21..2:24): Nil => scala/collection/immutable/Nil.
             |[3:6..3:7): m <= _empty_/n60.m().
             |[3:17..3:18): r <= local0
             |[3:20..3:21): s <= local1
             |[3:26..3:27): K => _empty_/n60.K.
             |[3:28..3:30): :: => scala/collection/immutable/List#`::`().
             |[3:31..3:34): Nil => scala/collection/immutable/Nil.
             |[3:40..3:41): r => local0
             |""".stripMargin
      },
      """|[0:7..0:10): n60 <= _empty_/n60.
         |[1:12..1:13): K <= _empty_/n60.K.
         |[2:7..2:8): p <= _empty_/n60.p.
         |[2:10..2:11): q <= _empty_/n60.q.
         |[2:16..2:17): K => _empty_/n60.K.
         |[2:18..2:20): :: => scala/collection/immutable/List#`::`().
         |[2:21..2:24): Nil => scala/package.Nil.
         |[3:6..3:7): m <= _empty_/n60.m().
         |[3:17..3:18): r <= local0
         |[3:20..3:21): s <= local1
         |[3:26..3:27): K => _empty_/n60.K.
         |[3:28..3:30): :: => scala/collection/immutable/List#`::`().
         |[3:31..3:34): Nil => scala/package.Nil.
         |[3:40..3:41): r => local0
         |""".stripMargin,
    ),
  )

  occurrences(
    "right-associative operators on constants in named arguments and defaults",
    """|object n62 {
       |  final val K = 1
       |  val x = 2
       |  def f(a: Int, b: List[Int]) = b
       |  val a = f(b = K :: Nil, a = x)
       |  def m = { def p(l: List[Int] = K :: Nil) = l; p() }
       |  class C(l: List[Int] = K :: Nil)
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n62 <= _empty_/n62.
             |[1:12..1:13): K <= _empty_/n62.K.
             |[2:6..2:7): x <= _empty_/n62.x.
             |[3:6..3:7): f <= _empty_/n62.f().
             |[3:8..3:9): a <= _empty_/n62.f().(a)
             |[3:11..3:14): Int => scala/Int#
             |[3:16..3:17): b <= _empty_/n62.f().(b)
             |[3:19..3:23): List => scala/package.List#
             |[3:24..3:27): Int => scala/Int#
             |[3:32..3:33): b => _empty_/n62.f().(b)
             |[4:6..4:7): a <= _empty_/n62.a.
             |[4:10..4:11): f => _empty_/n62.f().
             |[4:12..4:13): b => _empty_/n62.f().(b)
             |[4:16..4:17): K => _empty_/n62.K.
             |[4:18..4:20): :: => scala/collection/immutable/List#`::`().
             |[4:21..4:24): Nil => scala/collection/immutable/Nil.
             |[4:26..4:27): a => _empty_/n62.f().(a)
             |[4:30..4:31): x => _empty_/n62.x.
             |[5:6..5:7): m <= _empty_/n62.m().
             |[5:16..5:17): p <= local0
             |[5:18..5:19): l <= local1
             |[5:21..5:25): List => scala/package.List#
             |[5:26..5:29): Int => scala/Int#
             |[5:33..5:34): K => _empty_/n62.K.
             |[5:35..5:37): :: => scala/collection/immutable/List#`::`().
             |[5:38..5:41): Nil => scala/collection/immutable/Nil.
             |[5:45..5:46): l => local1
             |[5:48..5:49): p => local0
             |[6:8..6:9): C <= _empty_/n62.C#
             |[6:9..6:9):  <= _empty_/n62.C#`<init>`().
             |[6:10..6:11): l <= _empty_/n62.C#`<init>`().(l)
             |[6:10..6:11): l <= _empty_/n62.C#l.
             |[6:13..6:17): List => scala/package.List#
             |[6:18..6:21): Int => scala/Int#
             |[6:25..6:26): K => _empty_/n62.K.
             |[6:27..6:29): :: => scala/collection/immutable/List#`::`().
             |[6:30..6:33): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n62 <= _empty_/n62.
         |[1:12..1:13): K <= _empty_/n62.K.
         |[2:6..2:7): x <= _empty_/n62.x.
         |[3:6..3:7): f <= _empty_/n62.f().
         |[3:8..3:9): a <= _empty_/n62.f().(a)
         |[3:11..3:14): Int => scala/Int#
         |[3:16..3:17): b <= _empty_/n62.f().(b)
         |[3:19..3:23): List => scala/package.List#
         |[3:24..3:27): Int => scala/Int#
         |[3:32..3:33): b => _empty_/n62.f().(b)
         |[4:6..4:7): a <= _empty_/n62.a.
         |[4:10..4:11): f => _empty_/n62.f().
         |[4:12..4:13): b => _empty_/n62.f().(b)
         |[4:16..4:17): K => _empty_/n62.K.
         |[4:18..4:20): :: => scala/collection/immutable/List#`::`().
         |[4:21..4:24): Nil => scala/package.Nil.
         |[4:26..4:27): a => _empty_/n62.f().(a)
         |[4:30..4:31): x => _empty_/n62.x.
         |[5:6..5:7): m <= _empty_/n62.m().
         |[5:16..5:17): p <= local0
         |[5:18..5:19): l <= local1
         |[5:21..5:25): List => scala/package.List#
         |[5:26..5:29): Int => scala/Int#
         |[5:33..5:34): K => _empty_/n62.K.
         |[5:35..5:37): :: => scala/collection/immutable/List#`::`().
         |[5:38..5:41): Nil => scala/package.Nil.
         |[5:45..5:46): l => local1
         |[5:48..5:49): p => local0
         |[6:8..6:9): C <= _empty_/n62.C#
         |[6:9..6:9):  <= _empty_/n62.C#`<init>`().
         |[6:10..6:11): l <= _empty_/n62.C#`<init>`().(l)
         |[6:10..6:11): l <= _empty_/n62.C#l.
         |[6:13..6:17): List => scala/package.List#
         |[6:18..6:21): Int => scala/Int#
         |[6:25..6:26): K => _empty_/n62.K.
         |[6:27..6:29): :: => scala/collection/immutable/List#`::`().
         |[6:30..6:33): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "right-associative operators on constants in annotations of a class and a method",
    """|object n63 {
       |  final val K = 1
       |  class A(l: List[Any]) extends scala.annotation.StaticAnnotation
       |  @A(K :: Nil) class C
       |  @A(K :: Nil) def m = 1
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n63 <= _empty_/n63.
             |[1:12..1:13): K <= _empty_/n63.K.
             |[2:8..2:9): A <= _empty_/n63.A#
             |[2:9..2:9):  <= _empty_/n63.A#`<init>`().
             |[2:10..2:11): l <= _empty_/n63.A#`<init>`().(l)
             |[2:10..2:11): l <= _empty_/n63.A#l.
             |[2:13..2:17): List => scala/package.List#
             |[2:18..2:21): Any => scala/Any#
             |[2:32..2:37): scala => scala/
             |[2:38..2:48): annotation => scala/annotation/
             |[2:49..2:65): StaticAnnotation => scala/annotation/StaticAnnotation#
             |[2:65..2:65):  => scala/annotation/Annotation#`<init>`().
             |[3:3..3:4): A => _empty_/n63.A#
             |[3:4..3:4):  => _empty_/n63.A#`<init>`().
             |[3:5..3:6): K => _empty_/n63.K.
             |[3:7..3:9): :: => scala/collection/immutable/List#`::`().
             |[3:10..3:13): Nil => scala/collection/immutable/Nil.
             |[3:21..3:22): C <= _empty_/n63.C#
             |[3:22..3:22):  <= _empty_/n63.C#`<init>`().
             |[4:3..4:4): A => _empty_/n63.A#
             |[4:4..4:4):  => _empty_/n63.A#`<init>`().
             |[4:5..4:6): K => _empty_/n63.K.
             |[4:7..4:9): :: => scala/collection/immutable/List#`::`().
             |[4:10..4:13): Nil => scala/collection/immutable/Nil.
             |[4:19..4:20): m <= _empty_/n63.m().
             |""".stripMargin
      },
      """|[0:7..0:10): n63 <= _empty_/n63.
         |[1:12..1:13): K <= _empty_/n63.K.
         |[2:8..2:9): A <= _empty_/n63.A#
         |[2:9..2:9):  <= _empty_/n63.A#`<init>`().
         |[2:10..2:11): l <= _empty_/n63.A#`<init>`().(l)
         |[2:10..2:11): l <= _empty_/n63.A#l.
         |[2:13..2:17): List => scala/package.List#
         |[2:18..2:21): Any => scala/Any#
         |[2:32..2:37): scala => scala/
         |[2:38..2:48): annotation => scala/annotation/
         |[2:49..2:65): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[2:65..2:65):  => scala/annotation/Annotation#`<init>`().
         |[3:3..3:4): A => _empty_/n63.A#
         |[3:4..3:4):  => _empty_/n63.A#`<init>`().
         |[3:5..3:6): K => _empty_/n63.K.
         |[3:7..3:9): :: => scala/collection/immutable/List#`::`().
         |[3:10..3:13): Nil => scala/package.Nil.
         |[3:21..3:22): C <= _empty_/n63.C#
         |[3:22..3:22):  <= _empty_/n63.C#`<init>`().
         |[4:3..4:4): A => _empty_/n63.A#
         |[4:4..4:4):  => _empty_/n63.A#`<init>`().
         |[4:5..4:6): K => _empty_/n63.K.
         |[4:7..4:9): :: => scala/collection/immutable/List#`::`().
         |[4:10..4:13): Nil => scala/package.Nil.
         |[4:19..4:20): m <= _empty_/n63.m().
         |""".stripMargin,
    ),
  )

  private val n64 =
    """|object n64 {
       |  final val K = 1
       |  val a = { val y = K; K } :: Nil
       |}
       |""".stripMargin

  occurrences(
    "right-associative operators on constant blocks with a statement",
    n64,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n64 <= _empty_/n64.
             |[1:12..1:13): K <= _empty_/n64.K.
             |[2:6..2:7): a <= _empty_/n64.a.
             |[2:16..2:17): y <= local0
             |[2:20..2:21): K => _empty_/n64.K.
             |[2:23..2:24): K => _empty_/n64.K.
             |[2:27..2:29): :: => scala/collection/immutable/List#`::`().
             |[2:30..2:33): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n64 <= _empty_/n64.
         |[1:12..1:13): K <= _empty_/n64.K.
         |[2:6..2:7): a <= _empty_/n64.a.
         |[2:16..2:17): y <= local0
         |[2:20..2:21): K => _empty_/n64.K.
         |[2:23..2:24): K => _empty_/n64.K.
         |[2:27..2:29): :: => scala/collection/immutable/List#`::`().
         |[2:30..2:33): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  symbols(
    "right-associative operators on constant blocks with a statement",
    n64,
    """|_empty_/n64. => final object n64 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/n64.K. => final val method K: 1
       |_empty_/n64.a. => val method a: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "right-associative operators on constants under a view, a guard and a lambda",
    """|object n65 {
       |  final val K = 1
       |  class R { def +:(s: String) = this }
       |  implicit def v(i: Int): String = i.toString
       |  val r = new R
       |  val a = K +: r
       |  val b: PartialFunction[Int, Int] = { case x if (K :: Nil).nonEmpty => x }
       |  val c = List(1).map(_ => K :: Nil)
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n65 <= _empty_/n65.
             |[1:12..1:13): K <= _empty_/n65.K.
             |[2:8..2:9): R <= _empty_/n65.R#
             |[2:10..2:10):  <= _empty_/n65.R#`<init>`().
             |[2:16..2:18): +: <= _empty_/n65.R#`+:`().
             |[2:19..2:20): s <= _empty_/n65.R#`+:`().(s)
             |[2:22..2:28): String => scala/Predef.String#
             |[3:15..3:16): v <= _empty_/n65.v().
             |[3:17..3:18): i <= _empty_/n65.v().(i)
             |[3:20..3:23): Int => scala/Int#
             |[3:26..3:32): String => scala/Predef.String#
             |[3:35..3:36): i => _empty_/n65.v().(i)
             |[3:37..3:45): toString => scala/Any#toString().
             |[4:6..4:7): r <= _empty_/n65.r.
             |[4:14..4:15): R => _empty_/n65.R#
             |[4:15..4:15):  => _empty_/n65.R#`<init>`().
             |[5:6..5:7): a <= _empty_/n65.a.
             |[5:10..5:11): K => _empty_/n65.K.
             |[5:12..5:14): +: => _empty_/n65.R#`+:`().
             |[5:15..5:16): r => _empty_/n65.r.
             |[6:6..6:7): b <= _empty_/n65.b.
             |[6:9..6:24): PartialFunction => scala/PartialFunction#
             |[6:25..6:28): Int => scala/Int#
             |[6:30..6:33): Int => scala/Int#
             |[6:37..6:37):  <= local0
             |[6:44..6:45): x <= local7
             |[6:50..6:51): K => _empty_/n65.K.
             |[6:52..6:54): :: => scala/collection/immutable/List#`::`().
             |[6:55..6:58): Nil => scala/collection/immutable/Nil.
             |[6:60..6:68): nonEmpty => scala/collection/TraversableOnce#nonEmpty().
             |[6:72..6:73): x => local7
             |[7:6..7:7): c <= _empty_/n65.c.
             |[7:10..7:14): List => scala/collection/immutable/List.
             |[7:18..7:21): map => scala/collection/immutable/List#map().
             |[7:22..7:23): _ <= local9
             |[7:27..7:28): K => _empty_/n65.K.
             |[7:29..7:31): :: => scala/collection/immutable/List#`::`().
             |[7:32..7:35): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n65 <= _empty_/n65.
         |[1:12..1:13): K <= _empty_/n65.K.
         |[2:8..2:9): R <= _empty_/n65.R#
         |[2:10..2:10):  <= _empty_/n65.R#`<init>`().
         |[2:16..2:18): +: <= _empty_/n65.R#`+:`().
         |[2:19..2:20): s <= _empty_/n65.R#`+:`().(s)
         |[2:22..2:28): String => scala/Predef.String#
         |[3:15..3:16): v <= _empty_/n65.v().
         |[3:17..3:18): i <= _empty_/n65.v().(i)
         |[3:20..3:23): Int => scala/Int#
         |[3:26..3:32): String => scala/Predef.String#
         |[3:35..3:36): i => _empty_/n65.v().(i)
         |[3:37..3:45): toString => scala/Any#toString().
         |[4:6..4:7): r <= _empty_/n65.r.
         |[4:14..4:15): R => _empty_/n65.R#
         |[4:15..4:15):  => _empty_/n65.R#`<init>`().
         |[5:6..5:7): a <= _empty_/n65.a.
         |[5:10..5:11): K => _empty_/n65.K.
         |[5:12..5:14): +: => _empty_/n65.R#`+:`().
         |[5:15..5:16): r => _empty_/n65.r.
         |[6:6..6:7): b <= _empty_/n65.b.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:28): Int => scala/Int#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:37):  <= local0
         |[6:44..6:45): x <= local7
         |[6:50..6:51): K => _empty_/n65.K.
         |[6:52..6:54): :: => scala/collection/immutable/List#`::`().
         |[6:55..6:58): Nil => scala/package.Nil.
         |[6:60..6:68): nonEmpty => scala/collection/IterableOnceOps#nonEmpty().
         |[6:72..6:73): x => local7
         |[7:6..7:7): c <= _empty_/n65.c.
         |[7:10..7:14): List => scala/package.List.
         |[7:18..7:21): map => scala/collection/immutable/List#map().
         |[7:22..7:23): _ <= local9
         |[7:27..7:28): K => _empty_/n65.K.
         |[7:29..7:31): :: => scala/collection/immutable/List#`::`().
         |[7:32..7:35): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "right-associative operators and patterns on constants in one source",
    """|object n66 {
       |  final val K = 1
       |  def m(l: List[Int]) = { val a = K :: Nil; l match { case K :: _ => a; case _ => Nil } }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n66 <= _empty_/n66.
             |[1:12..1:13): K <= _empty_/n66.K.
             |[2:6..2:7): m <= _empty_/n66.m().
             |[2:8..2:9): l <= _empty_/n66.m().(l)
             |[2:11..2:15): List => scala/package.List#
             |[2:16..2:19): Int => scala/Int#
             |[2:30..2:31): a <= local0
             |[2:34..2:35): K => _empty_/n66.K.
             |[2:36..2:38): :: => scala/collection/immutable/List#`::`().
             |[2:39..2:42): Nil => scala/collection/immutable/Nil.
             |[2:44..2:45): l => _empty_/n66.m().(l)
             |[2:59..2:60): K => _empty_/n66.K.
             |[2:61..2:63): :: => scala/package.`::`.
             |[2:69..2:70): a => local0
             |[2:82..2:85): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n66 <= _empty_/n66.
         |[1:12..1:13): K <= _empty_/n66.K.
         |[2:6..2:7): m <= _empty_/n66.m().
         |[2:8..2:9): l <= _empty_/n66.m().(l)
         |[2:11..2:15): List => scala/package.List#
         |[2:16..2:19): Int => scala/Int#
         |[2:30..2:31): a <= local0
         |[2:34..2:35): K => _empty_/n66.K.
         |[2:36..2:38): :: => scala/collection/immutable/List#`::`().
         |[2:39..2:42): Nil => scala/package.Nil.
         |[2:44..2:45): l => _empty_/n66.m().(l)
         |[2:59..2:60): K => _empty_/n66.K.
         |[2:61..2:63): :: => scala/package.`::`.
         |[2:69..2:70): a => local0
         |[2:82..2:85): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "right-associative operators on a macro that expands to a constant",
    """|object n67 {
       |  final val K = 1
       |  val a = example.ConstantExpansion.one(K) :: Nil
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n67 <= _empty_/n67.
             |[1:12..1:13): K <= _empty_/n67.K.
             |[2:6..2:7): a <= _empty_/n67.a.
             |[2:10..2:17): example => example/
             |[2:18..2:35): ConstantExpansion => example/ConstantExpansion.
             |[2:36..2:39): one => example/ConstantExpansion.one().
             |[2:40..2:41): K => _empty_/n67.K.
             |[2:43..2:45): :: => scala/collection/immutable/List#`::`().
             |[2:46..2:49): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n67 <= _empty_/n67.
         |[1:12..1:13): K <= _empty_/n67.K.
         |[2:6..2:7): a <= _empty_/n67.a.
         |[2:10..2:17): example => example/
         |[2:18..2:35): ConstantExpansion => example/ConstantExpansion.
         |[2:36..2:39): one => example/ConstantExpansion.one().
         |[2:40..2:41): K => _empty_/n67.K.
         |[2:43..2:45): :: => scala/collection/immutable/List#`::`().
         |[2:46..2:49): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  private val n68 =
    """|object n68 {
       |  object O { final val J = 1 }
       |  final val K = 1
       |  val a = { def f = 1; K } :: Nil
       |  val b = { import O.J; J } :: Nil
       |  val c = { class C; K } :: Nil
       |  val d = { type T = Int; K } :: Nil
       |  val e = { object P; K } :: Nil
       |  final val g = { def f = 1; K }
       |  val h = identity({ import O.J; J })
       |  val m = example.ConstantExpansion.one(K) :: Nil
       |}
       |""".stripMargin

  occurrences(
    "right-associative operators on constant blocks with definitions and a macro",
    n68,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n68 <= _empty_/n68.
             |[1:9..1:10): O <= _empty_/n68.O.
             |[1:23..1:24): J <= _empty_/n68.O.J.
             |[2:12..2:13): K <= _empty_/n68.K.
             |[3:6..3:7): a <= _empty_/n68.a.
             |[3:16..3:17): f <= local0
             |[3:23..3:24): K => _empty_/n68.K.
             |[3:27..3:29): :: => scala/collection/immutable/List#`::`().
             |[3:30..3:33): Nil => scala/collection/immutable/Nil.
             |[4:6..4:7): b <= _empty_/n68.b.
             |[4:19..4:20): O => _empty_/n68.O.
             |[4:21..4:22): J => _empty_/n68.O.J.
             |[4:24..4:25): J => _empty_/n68.O.J.
             |[4:28..4:30): :: => scala/collection/immutable/List#`::`().
             |[4:31..4:34): Nil => scala/collection/immutable/Nil.
             |[5:6..5:7): c <= _empty_/n68.c.
             |[5:18..5:19): C <= local1
             |[5:19..5:19):  <= local2
             |[5:21..5:22): K => _empty_/n68.K.
             |[5:25..5:27): :: => scala/collection/immutable/List#`::`().
             |[5:28..5:31): Nil => scala/collection/immutable/Nil.
             |[6:6..6:7): d <= _empty_/n68.d.
             |[6:17..6:18): T <= local3
             |[6:21..6:24): Int => scala/Int#
             |[6:26..6:27): K => _empty_/n68.K.
             |[6:30..6:32): :: => scala/collection/immutable/List#`::`().
             |[6:33..6:36): Nil => scala/collection/immutable/Nil.
             |[7:6..7:7): e <= _empty_/n68.e.
             |[7:19..7:20): P <= local4
             |[7:22..7:23): K => _empty_/n68.K.
             |[7:26..7:28): :: => scala/collection/immutable/List#`::`().
             |[7:29..7:32): Nil => scala/collection/immutable/Nil.
             |[8:12..8:13): g <= _empty_/n68.g.
             |[8:22..8:23): f <= local5
             |[8:29..8:30): K => _empty_/n68.K.
             |[9:6..9:7): h <= _empty_/n68.h.
             |[9:10..9:18): identity => scala/Predef.identity().
             |[9:28..9:29): O => _empty_/n68.O.
             |[9:30..9:31): J => _empty_/n68.O.J.
             |[9:33..9:34): J => _empty_/n68.O.J.
             |[10:6..10:7): m <= _empty_/n68.m.
             |[10:10..10:17): example => example/
             |[10:18..10:35): ConstantExpansion => example/ConstantExpansion.
             |[10:36..10:39): one => example/ConstantExpansion.one().
             |[10:40..10:41): K => _empty_/n68.K.
             |[10:43..10:45): :: => scala/collection/immutable/List#`::`().
             |[10:46..10:49): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n68 <= _empty_/n68.
         |[1:9..1:10): O <= _empty_/n68.O.
         |[1:23..1:24): J <= _empty_/n68.O.J.
         |[2:12..2:13): K <= _empty_/n68.K.
         |[3:6..3:7): a <= _empty_/n68.a.
         |[3:16..3:17): f <= local0
         |[3:23..3:24): K => _empty_/n68.K.
         |[3:27..3:29): :: => scala/collection/immutable/List#`::`().
         |[3:30..3:33): Nil => scala/package.Nil.
         |[4:6..4:7): b <= _empty_/n68.b.
         |[4:19..4:20): O => _empty_/n68.O.
         |[4:21..4:22): J => _empty_/n68.O.J.
         |[4:24..4:25): J => _empty_/n68.O.J.
         |[4:28..4:30): :: => scala/collection/immutable/List#`::`().
         |[4:31..4:34): Nil => scala/package.Nil.
         |[5:6..5:7): c <= _empty_/n68.c.
         |[5:18..5:19): C <= local1
         |[5:19..5:19):  <= local2
         |[5:21..5:22): K => _empty_/n68.K.
         |[5:25..5:27): :: => scala/collection/immutable/List#`::`().
         |[5:28..5:31): Nil => scala/package.Nil.
         |[6:6..6:7): d <= _empty_/n68.d.
         |[6:17..6:18): T <= local3
         |[6:21..6:24): Int => scala/Int#
         |[6:26..6:27): K => _empty_/n68.K.
         |[6:30..6:32): :: => scala/collection/immutable/List#`::`().
         |[6:33..6:36): Nil => scala/package.Nil.
         |[7:6..7:7): e <= _empty_/n68.e.
         |[7:19..7:20): P <= local4
         |[7:22..7:23): K => _empty_/n68.K.
         |[7:26..7:28): :: => scala/collection/immutable/List#`::`().
         |[7:29..7:32): Nil => scala/package.Nil.
         |[8:12..8:13): g <= _empty_/n68.g.
         |[8:22..8:23): f <= local5
         |[8:29..8:30): K => _empty_/n68.K.
         |[9:6..9:7): h <= _empty_/n68.h.
         |[9:10..9:18): identity => scala/Predef.identity().
         |[9:28..9:29): O => _empty_/n68.O.
         |[9:30..9:31): J => _empty_/n68.O.J.
         |[9:33..9:34): J => _empty_/n68.O.J.
         |[10:6..10:7): m <= _empty_/n68.m.
         |[10:10..10:17): example => example/
         |[10:18..10:35): ConstantExpansion => example/ConstantExpansion.
         |[10:36..10:39): one => example/ConstantExpansion.one().
         |[10:40..10:41): K => _empty_/n68.K.
         |[10:43..10:45): :: => scala/collection/immutable/List#`::`().
         |[10:46..10:49): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  symbols(
    "right-associative operators on constant blocks with definitions and a macro",
    n68,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|_empty_/n68. => final object n68 extends AnyRef { +10 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/n68.K. => final val method K: 1
             |_empty_/n68.O. => final object O extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/n68.O.J. => final val method J: 1
             |_empty_/n68.a. => val method a: List[Int]
             |  List => scala/collection/immutable/List#
             |  Int => scala/Int#
             |_empty_/n68.b. => val method b: List[Int]
             |  List => scala/collection/immutable/List#
             |  Int => scala/Int#
             |_empty_/n68.c. => val method c: List[Int]
             |  List => scala/collection/immutable/List#
             |  Int => scala/Int#
             |_empty_/n68.d. => val method d: List[Int]
             |  List => scala/collection/immutable/List#
             |  Int => scala/Int#
             |_empty_/n68.e. => val method e: List[Int]
             |  List => scala/collection/immutable/List#
             |  Int => scala/Int#
             |_empty_/n68.g. => final val method g: Int
             |  Int => scala/Int#
             |_empty_/n68.h. => val method h: Int
             |  Int => scala/Int#
             |_empty_/n68.m. => val method m: List[Int]
             |  List => scala/collection/immutable/List#
             |  Int => scala/Int#
             |local0 => method f: Int
             |  Int => scala/Int#
             |local1 => class C extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |local2 => primary ctor <init>()
             |local3 => type T = Int
             |  Int => scala/Int#
             |local4 => final object P extends AnyRef
             |  AnyRef => scala/AnyRef#
             |local5 => method f: Int
             |  Int => scala/Int#
             |""".stripMargin
      },
      """|_empty_/n68. => final object n68 extends AnyRef { +10 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/n68.K. => final val method K: 1
         |_empty_/n68.O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/n68.O.J. => final val method J: 1
         |_empty_/n68.a. => val method a: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/n68.b. => val method b: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/n68.c. => val method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/n68.d. => val method d: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/n68.e. => val method e: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/n68.g. => final val method g: 1
         |_empty_/n68.h. => val method h: Int
         |  Int => scala/Int#
         |_empty_/n68.m. => val method m: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |local0 => method f: Int
         |  Int => scala/Int#
         |local1 => class C extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |local2 => primary ctor <init>()
         |local3 => type T = Int
         |  Int => scala/Int#
         |local4 => final object P extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local5 => method f: Int
         |  Int => scala/Int#
         |""".stripMargin,
    ),
  )

  synthetics(
    "right-associative operators on constant blocks with definitions and a macro",
    n68,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[3:27..3:33): :: Nil => *[Int]
             |  Int => scala/Int#
             |[4:28..4:34): :: Nil => *[Int]
             |  Int => scala/Int#
             |[5:25..5:31): :: Nil => *[Int]
             |  Int => scala/Int#
             |[6:30..6:36): :: Nil => *[Int]
             |  Int => scala/Int#
             |[7:26..7:32): :: Nil => *[Int]
             |  Int => scala/Int#
             |[9:10..9:18): identity => *[Int]
             |  Int => scala/Int#
             |[10:10..10:39): example.ConstantExpansion.one => *[Int]
             |  Int => scala/Int#
             |[10:43..10:49): :: Nil => *[Int]
             |  Int => scala/Int#
             |""".stripMargin
      },
      """|[3:27..3:33): :: Nil => *[Int]
         |  Int => scala/Int#
         |[4:28..4:34): :: Nil => *[Int]
         |  Int => scala/Int#
         |[5:25..5:31): :: Nil => *[Int]
         |  Int => scala/Int#
         |[6:30..6:36): :: Nil => *[Int]
         |  Int => scala/Int#
         |[7:26..7:32): :: Nil => *[Int]
         |  Int => scala/Int#
         |[9:10..9:18): identity => *[Int]
         |  Int => scala/Int#
         |[10:10..10:39): example.ConstantExpansion.one => *[Int]
         |  Int => scala/Int#
         |[10:43..10:49): :: Nil => *[Int]
         |  Int => scala/Int#
         |""".stripMargin,
    ),
  )

  private val n69 =
    """|object n69 {
       |  final val K = 1
       |  class R { def +:(s: String) = this }
       |  implicit def v(i: Int): String = i.toString
       |  val r = new R
       |  val x = 2
       |  val a = K +: r
       |  val b = x +: r
       |  val c = 3 +: r
       |  val d = r.+:(K)
       |}
       |""".stripMargin

  occurrences(
    "right-associative operators under an implicit view",
    n69,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n69 <= _empty_/n69.
             |[1:12..1:13): K <= _empty_/n69.K.
             |[2:8..2:9): R <= _empty_/n69.R#
             |[2:10..2:10):  <= _empty_/n69.R#`<init>`().
             |[2:16..2:18): +: <= _empty_/n69.R#`+:`().
             |[2:19..2:20): s <= _empty_/n69.R#`+:`().(s)
             |[2:22..2:28): String => scala/Predef.String#
             |[3:15..3:16): v <= _empty_/n69.v().
             |[3:17..3:18): i <= _empty_/n69.v().(i)
             |[3:20..3:23): Int => scala/Int#
             |[3:26..3:32): String => scala/Predef.String#
             |[3:35..3:36): i => _empty_/n69.v().(i)
             |[3:37..3:45): toString => scala/Any#toString().
             |[4:6..4:7): r <= _empty_/n69.r.
             |[4:14..4:15): R => _empty_/n69.R#
             |[4:15..4:15):  => _empty_/n69.R#`<init>`().
             |[5:6..5:7): x <= _empty_/n69.x.
             |[6:6..6:7): a <= _empty_/n69.a.
             |[6:10..6:11): K => _empty_/n69.K.
             |[6:12..6:14): +: => _empty_/n69.R#`+:`().
             |[6:15..6:16): r => _empty_/n69.r.
             |[7:6..7:7): b <= _empty_/n69.b.
             |[7:10..7:11): x => _empty_/n69.x.
             |[7:12..7:14): +: => _empty_/n69.R#`+:`().
             |[7:15..7:16): r => _empty_/n69.r.
             |[8:6..8:7): c <= _empty_/n69.c.
             |[8:12..8:14): +: => _empty_/n69.R#`+:`().
             |[8:15..8:16): r => _empty_/n69.r.
             |[9:6..9:7): d <= _empty_/n69.d.
             |[9:10..9:11): r => _empty_/n69.r.
             |[9:12..9:14): +: => _empty_/n69.R#`+:`().
             |[9:15..9:16): K => _empty_/n69.K.
             |""".stripMargin
      },
      """|[0:7..0:10): n69 <= _empty_/n69.
         |[1:12..1:13): K <= _empty_/n69.K.
         |[2:8..2:9): R <= _empty_/n69.R#
         |[2:10..2:10):  <= _empty_/n69.R#`<init>`().
         |[2:16..2:18): +: <= _empty_/n69.R#`+:`().
         |[2:19..2:20): s <= _empty_/n69.R#`+:`().(s)
         |[2:22..2:28): String => scala/Predef.String#
         |[3:15..3:16): v <= _empty_/n69.v().
         |[3:17..3:18): i <= _empty_/n69.v().(i)
         |[3:20..3:23): Int => scala/Int#
         |[3:26..3:32): String => scala/Predef.String#
         |[3:35..3:36): i => _empty_/n69.v().(i)
         |[3:37..3:45): toString => scala/Any#toString().
         |[4:6..4:7): r <= _empty_/n69.r.
         |[4:14..4:15): R => _empty_/n69.R#
         |[4:15..4:15):  => _empty_/n69.R#`<init>`().
         |[5:6..5:7): x <= _empty_/n69.x.
         |[6:6..6:7): a <= _empty_/n69.a.
         |[6:10..6:11): K => _empty_/n69.K.
         |[6:12..6:14): +: => _empty_/n69.R#`+:`().
         |[6:15..6:16): r => _empty_/n69.r.
         |[7:6..7:7): b <= _empty_/n69.b.
         |[7:10..7:11): x => _empty_/n69.x.
         |[7:12..7:14): +: => _empty_/n69.R#`+:`().
         |[7:15..7:16): r => _empty_/n69.r.
         |[8:6..8:7): c <= _empty_/n69.c.
         |[8:12..8:14): +: => _empty_/n69.R#`+:`().
         |[8:15..8:16): r => _empty_/n69.r.
         |[9:6..9:7): d <= _empty_/n69.d.
         |[9:10..9:11): r => _empty_/n69.r.
         |[9:12..9:14): +: => _empty_/n69.R#`+:`().
         |[9:15..9:16): K => _empty_/n69.K.
         |""".stripMargin,
    ),
  )

  synthetics(
    "right-associative operators under an implicit view",
    n69,
    """|[6:10..6:11): K => v(*)
       |  v => _empty_/n69.v().
       |[7:10..7:11): x => v(*)
       |  v => _empty_/n69.v().
       |[8:10..8:11): 3 => v(*)
       |  v => _empty_/n69.v().
       |[9:15..9:16): K => v(*)
       |  v => _empty_/n69.v().
       |""".stripMargin,
  )

  private val n70 =
    """|object n70 {
       |  object O { final val K = 1 }
       |  val a = example.ConstantExpansion.one({ val q = 2; q }) :: { val w = 1; List(w) }
       |  val b = example.ConstantExpansion.one({ val q = 2; q }) :: Nil
       |  val c = example.ConstantExpansion.one({ import O.{K => J}; J }) :: Nil
       |}
       |""".stripMargin

  occurrences(
    "right-associative operators on a macro with a block argument",
    n70,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n70 <= _empty_/n70.
             |[1:9..1:10): O <= _empty_/n70.O.
             |[1:23..1:24): K <= _empty_/n70.O.K.
             |[2:6..2:7): a <= _empty_/n70.a.
             |[2:10..2:17): example => example/
             |[2:18..2:35): ConstantExpansion => example/ConstantExpansion.
             |[2:36..2:39): one => example/ConstantExpansion.one().
             |[2:46..2:47): q <= local0
             |[2:53..2:54): q => local0
             |[2:58..2:60): :: => scala/collection/immutable/List#`::`().
             |[2:67..2:68): w <= local1
             |[2:74..2:78): List => scala/collection/immutable/List.
             |[2:79..2:80): w => local1
             |[3:6..3:7): b <= _empty_/n70.b.
             |[3:10..3:17): example => example/
             |[3:18..3:35): ConstantExpansion => example/ConstantExpansion.
             |[3:36..3:39): one => example/ConstantExpansion.one().
             |[3:46..3:47): q <= local2
             |[3:53..3:54): q => local2
             |[3:58..3:60): :: => scala/collection/immutable/List#`::`().
             |[3:61..3:64): Nil => scala/collection/immutable/Nil.
             |[4:6..4:7): c <= _empty_/n70.c.
             |[4:10..4:17): example => example/
             |[4:18..4:35): ConstantExpansion => example/ConstantExpansion.
             |[4:36..4:39): one => example/ConstantExpansion.one().
             |[4:49..4:50): O => _empty_/n70.O.
             |[4:52..4:53): K => _empty_/n70.O.K.
             |[4:61..4:62): J => _empty_/n70.O.K.
             |[4:66..4:68): :: => scala/collection/immutable/List#`::`().
             |[4:69..4:72): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:7..0:10): n70 <= _empty_/n70.
         |[1:9..1:10): O <= _empty_/n70.O.
         |[1:23..1:24): K <= _empty_/n70.O.K.
         |[2:6..2:7): a <= _empty_/n70.a.
         |[2:10..2:17): example => example/
         |[2:18..2:35): ConstantExpansion => example/ConstantExpansion.
         |[2:36..2:39): one => example/ConstantExpansion.one().
         |[2:46..2:47): q <= local1
         |[2:53..2:54): q => local1
         |[2:58..2:60): :: => scala/collection/immutable/List#`::`().
         |[2:67..2:68): w <= local0
         |[2:74..2:78): List => scala/package.List.
         |[2:79..2:80): w => local0
         |[3:6..3:7): b <= _empty_/n70.b.
         |[3:10..3:17): example => example/
         |[3:18..3:35): ConstantExpansion => example/ConstantExpansion.
         |[3:36..3:39): one => example/ConstantExpansion.one().
         |[3:46..3:47): q <= local2
         |[3:53..3:54): q => local2
         |[3:58..3:60): :: => scala/collection/immutable/List#`::`().
         |[3:61..3:64): Nil => scala/package.Nil.
         |[4:6..4:7): c <= _empty_/n70.c.
         |[4:10..4:17): example => example/
         |[4:18..4:35): ConstantExpansion => example/ConstantExpansion.
         |[4:36..4:39): one => example/ConstantExpansion.one().
         |[4:49..4:50): O => _empty_/n70.O.
         |[4:52..4:53): K => _empty_/n70.O.K.
         |[4:61..4:62): J => _empty_/n70.O.K.
         |[4:66..4:68): :: => scala/collection/immutable/List#`::`().
         |[4:69..4:72): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  symbols(
    "right-associative operators on a macro with a block argument",
    n70,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|_empty_/n70. => final object n70 extends AnyRef { +4 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/n70.O. => final object O extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/n70.O.K. => final val method K: 1
             |_empty_/n70.a. => val method a: List[Int]
             |  List => scala/collection/immutable/List#
             |  Int => scala/Int#
             |_empty_/n70.b. => val method b: List[Int]
             |  List => scala/collection/immutable/List#
             |  Int => scala/Int#
             |_empty_/n70.c. => val method c: List[Int]
             |  List => scala/collection/immutable/List#
             |  Int => scala/Int#
             |local0 => val local q: Int
             |  Int => scala/Int#
             |local1 => val local w: Int
             |  Int => scala/Int#
             |local2 => val local q: Int
             |  Int => scala/Int#
             |""".stripMargin
      },
      """|_empty_/n70. => final object n70 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/n70.O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/n70.O.K. => final val method K: 1
         |_empty_/n70.a. => val method a: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/n70.b. => val method b: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/n70.c. => val method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |local0 => val local w: Int
         |  Int => scala/Int#
         |local1 => val local q: Int
         |  Int => scala/Int#
         |local2 => val local q: Int
         |  Int => scala/Int#
         |""".stripMargin,
    ),
  )

  synthetics(
    "right-associative operators on a macro with a block argument",
    n70,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[2:10..2:39): example.ConstantExpansion.one => *[Int]
             |  Int => scala/Int#
             |[2:58..2:83): :: { val w = 1; List(w) } => *[Int]
             |  Int => scala/Int#
             |[2:74..2:78): List => *.apply[Int]
             |  apply => scala/collection/immutable/List.apply().
             |  Int => scala/Int#
             |[3:10..3:39): example.ConstantExpansion.one => *[Int]
             |  Int => scala/Int#
             |[3:58..3:64): :: Nil => *[Int]
             |  Int => scala/Int#
             |[4:10..4:39): example.ConstantExpansion.one => *[Int]
             |  Int => scala/Int#
             |[4:66..4:72): :: Nil => *[Int]
             |  Int => scala/Int#
             |""".stripMargin
      },
      """|[2:10..2:39): example.ConstantExpansion.one => *[Int]
         |  Int => scala/Int#
         |[2:58..2:83): :: { val w = 1; List(w) } => *[Int]
         |  Int => scala/Int#
         |[2:74..2:78): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[3:10..3:39): example.ConstantExpansion.one => *[Int]
         |  Int => scala/Int#
         |[3:58..3:64): :: Nil => *[Int]
         |  Int => scala/Int#
         |[4:10..4:39): example.ConstantExpansion.one => *[Int]
         |  Int => scala/Int#
         |[4:66..4:72): :: Nil => *[Int]
         |  Int => scala/Int#
         |""".stripMargin,
    ),
  )

  private val n71 =
    """|object n71 {
       |  final val K = 1
       |  def m(a: List[Int] = example.ConstantExpansion.one(K) :: Nil) = a
       |  val b: PartialFunction[Int, Int] = {
       |    case x if (example.ConstantExpansion.one(K) :: Nil).nonEmpty => x
       |  }
       |}
       |""".stripMargin

  occurrences(
    "right-associative operators on a macro in a default value and a guard",
    n71,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n71 <= _empty_/n71.
             |[1:12..1:13): K <= _empty_/n71.K.
             |[2:6..2:7): m <= _empty_/n71.m().
             |[2:8..2:9): a <= _empty_/n71.m().(a)
             |[2:11..2:15): List => scala/package.List#
             |[2:16..2:19): Int => scala/Int#
             |[2:23..2:30): example => example/
             |[2:31..2:48): ConstantExpansion => example/ConstantExpansion.
             |[2:49..2:52): one => example/ConstantExpansion.one().
             |[2:53..2:54): K => _empty_/n71.K.
             |[2:56..2:58): :: => scala/collection/immutable/List#`::`().
             |[2:59..2:62): Nil => scala/collection/immutable/Nil.
             |[2:66..2:67): a => _empty_/n71.m().(a)
             |[3:6..3:7): b <= _empty_/n71.b.
             |[3:9..3:24): PartialFunction => scala/PartialFunction#
             |[3:25..3:28): Int => scala/Int#
             |[3:30..3:33): Int => scala/Int#
             |[3:37..3:37):  <= local0
             |[4:9..4:10): x <= local7
             |[4:15..4:22): example => example/
             |[4:23..4:40): ConstantExpansion => example/ConstantExpansion.
             |[4:41..4:44): one => example/ConstantExpansion.one().
             |[4:45..4:46): K => _empty_/n71.K.
             |[4:48..4:50): :: => scala/collection/immutable/List#`::`().
             |[4:51..4:54): Nil => scala/collection/immutable/Nil.
             |[4:56..4:64): nonEmpty => scala/collection/TraversableOnce#nonEmpty().
             |[4:68..4:69): x => local7
             |""".stripMargin
      },
      """|[0:7..0:10): n71 <= _empty_/n71.
         |[1:12..1:13): K <= _empty_/n71.K.
         |[2:6..2:7): m <= _empty_/n71.m().
         |[2:8..2:9): a <= _empty_/n71.m().(a)
         |[2:11..2:15): List => scala/package.List#
         |[2:16..2:19): Int => scala/Int#
         |[2:23..2:30): example => example/
         |[2:31..2:48): ConstantExpansion => example/ConstantExpansion.
         |[2:49..2:52): one => example/ConstantExpansion.one().
         |[2:53..2:54): K => _empty_/n71.K.
         |[2:56..2:58): :: => scala/collection/immutable/List#`::`().
         |[2:59..2:62): Nil => scala/package.Nil.
         |[2:66..2:67): a => _empty_/n71.m().(a)
         |[3:6..3:7): b <= _empty_/n71.b.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:28): Int => scala/Int#
         |[3:30..3:33): Int => scala/Int#
         |[3:37..3:37):  <= local0
         |[4:9..4:10): x <= local7
         |[4:15..4:22): example => example/
         |[4:23..4:40): ConstantExpansion => example/ConstantExpansion.
         |[4:41..4:44): one => example/ConstantExpansion.one().
         |[4:45..4:46): K => _empty_/n71.K.
         |[4:48..4:50): :: => scala/collection/immutable/List#`::`().
         |[4:51..4:54): Nil => scala/package.Nil.
         |[4:56..4:64): nonEmpty => scala/collection/IterableOnceOps#nonEmpty().
         |[4:68..4:69): x => local7
         |""".stripMargin,
    ),
  )

  synthetics(
    "right-associative operators on a macro in a default value and a guard",
    n71,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[2:23..2:52): example.ConstantExpansion.one => *[Int]
             |  Int => scala/Int#
             |[2:56..2:62): :: Nil => *[Int]
             |  Int => scala/Int#
             |[3:37..5:3): {
             |    case x if (example.ConstantExpansion.one(K) :: Nil).nonEmpty => x
             |  } => *.apply
             |  apply => scala/Function1#apply().
             |[4:15..4:44): example.ConstantExpansion.one => *[Int]
             |  Int => scala/Int#
             |[4:48..4:54): :: Nil => *[Int]
             |  Int => scala/Int#
             |""".stripMargin
      },
      """|[2:23..2:52): example.ConstantExpansion.one => *[Int]
         |  Int => scala/Int#
         |[2:56..2:62): :: Nil => *[Int]
         |  Int => scala/Int#
         |[3:37..5:3): {
         |    case x if (example.ConstantExpansion.one(K) :: Nil).nonEmpty => x
         |  } => *.apply
         |  apply => scala/Function1#apply().
         |[4:15..4:44): example.ConstantExpansion.one => *[Int]
         |  Int => scala/Int#
         |[4:48..4:54): :: Nil => *[Int]
         |  Int => scala/Int#
         |""".stripMargin,
    ),
  )

  private val n73 =
    """|object n73 {
       |  class R { def +:(s: String) = this }
       |  implicit def v(i: Int): String = i.toString
       |  object o { val x = 1 }
       |  def f(i: Int) = i
       |  val r = new R
       |  val x = 2
       |  val a = o.x +: r
       |  val b = f(1) +: r
       |  val c = (x + 1) +: r
       |  val d = { x } +: r
       |  val e = 1 +: x +: r
       |}
       |""".stripMargin

  occurrences(
    "right-associative operators on expressions under a view",
    n73,
    """|[0:7..0:10): n73 <= _empty_/n73.
       |[1:8..1:9): R <= _empty_/n73.R#
       |[1:10..1:10):  <= _empty_/n73.R#`<init>`().
       |[1:16..1:18): +: <= _empty_/n73.R#`+:`().
       |[1:19..1:20): s <= _empty_/n73.R#`+:`().(s)
       |[1:22..1:28): String => scala/Predef.String#
       |[2:15..2:16): v <= _empty_/n73.v().
       |[2:17..2:18): i <= _empty_/n73.v().(i)
       |[2:20..2:23): Int => scala/Int#
       |[2:26..2:32): String => scala/Predef.String#
       |[2:35..2:36): i => _empty_/n73.v().(i)
       |[2:37..2:45): toString => scala/Any#toString().
       |[3:9..3:10): o <= _empty_/n73.o.
       |[3:17..3:18): x <= _empty_/n73.o.x.
       |[4:6..4:7): f <= _empty_/n73.f().
       |[4:8..4:9): i <= _empty_/n73.f().(i)
       |[4:11..4:14): Int => scala/Int#
       |[4:18..4:19): i => _empty_/n73.f().(i)
       |[5:6..5:7): r <= _empty_/n73.r.
       |[5:14..5:15): R => _empty_/n73.R#
       |[5:15..5:15):  => _empty_/n73.R#`<init>`().
       |[6:6..6:7): x <= _empty_/n73.x.
       |[7:6..7:7): a <= _empty_/n73.a.
       |[7:10..7:11): o => _empty_/n73.o.
       |[7:12..7:13): x => _empty_/n73.o.x.
       |[7:14..7:16): +: => _empty_/n73.R#`+:`().
       |[7:17..7:18): r => _empty_/n73.r.
       |[8:6..8:7): b <= _empty_/n73.b.
       |[8:10..8:11): f => _empty_/n73.f().
       |[8:15..8:17): +: => _empty_/n73.R#`+:`().
       |[8:18..8:19): r => _empty_/n73.r.
       |[9:6..9:7): c <= _empty_/n73.c.
       |[9:11..9:12): x => _empty_/n73.x.
       |[9:13..9:14): + => scala/Int#`+`(+4).
       |[9:18..9:20): +: => _empty_/n73.R#`+:`().
       |[9:21..9:22): r => _empty_/n73.r.
       |[10:6..10:7): d <= _empty_/n73.d.
       |[10:12..10:13): x => _empty_/n73.x.
       |[10:16..10:18): +: => _empty_/n73.R#`+:`().
       |[10:19..10:20): r => _empty_/n73.r.
       |[11:6..11:7): e <= _empty_/n73.e.
       |[11:12..11:14): +: => _empty_/n73.R#`+:`().
       |[11:15..11:16): x => _empty_/n73.x.
       |[11:17..11:19): +: => _empty_/n73.R#`+:`().
       |[11:20..11:21): r => _empty_/n73.r.
       |""".stripMargin,
  )

  synthetics(
    "right-associative operators on expressions under a view",
    n73,
    """|[7:10..7:13): o.x => v(*)
       |  v => _empty_/n73.v().
       |[8:10..8:14): f(1) => v(*)
       |  v => _empty_/n73.v().
       |[9:10..9:17): (x + 1) => v(*)
       |  v => _empty_/n73.v().
       |[10:12..10:13): x => v(*)
       |  v => _empty_/n73.v().
       |[11:10..11:11): 1 => v(*)
       |  v => _empty_/n73.v().
       |[11:15..11:16): x => v(*)
       |  v => _empty_/n73.v().
       |""".stripMargin,
  )

  private val n74 =
    """|object n74 {
       |  class C
       |  implicit val c: C = new C
       |  class R { def +:(s: String) = this }
       |  implicit def w(i: Int)(implicit c: C): String = i.toString
       |  implicit def l(xs: List[Int]): String = xs.toString
       |  val r = new R
       |  val x = 2
       |  val a = x +: r
       |  val b = List(1) +: r
       |}
       |""".stripMargin

  occurrences(
    "right-associative operators under a view with arguments",
    n74,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n74 <= _empty_/n74.
             |[1:8..1:9): C <= _empty_/n74.C#
             |[1:9..1:9):  <= _empty_/n74.C#`<init>`().
             |[2:15..2:16): c <= _empty_/n74.c.
             |[2:18..2:19): C => _empty_/n74.C#
             |[2:26..2:27): C => _empty_/n74.C#
             |[2:27..2:27):  => _empty_/n74.C#`<init>`().
             |[3:8..3:9): R <= _empty_/n74.R#
             |[3:10..3:10):  <= _empty_/n74.R#`<init>`().
             |[3:16..3:18): +: <= _empty_/n74.R#`+:`().
             |[3:19..3:20): s <= _empty_/n74.R#`+:`().(s)
             |[3:22..3:28): String => scala/Predef.String#
             |[4:15..4:16): w <= _empty_/n74.w().
             |[4:17..4:18): i <= _empty_/n74.w().(i)
             |[4:20..4:23): Int => scala/Int#
             |[4:34..4:35): c <= _empty_/n74.w().(c)
             |[4:37..4:38): C => _empty_/n74.C#
             |[4:41..4:47): String => scala/Predef.String#
             |[4:50..4:51): i => _empty_/n74.w().(i)
             |[4:52..4:60): toString => scala/Any#toString().
             |[5:15..5:16): l <= _empty_/n74.l().
             |[5:17..5:19): xs <= _empty_/n74.l().(xs)
             |[5:21..5:25): List => scala/package.List#
             |[5:26..5:29): Int => scala/Int#
             |[5:33..5:39): String => scala/Predef.String#
             |[5:42..5:44): xs => _empty_/n74.l().(xs)
             |[5:45..5:53): toString => scala/collection/SeqLike#toString().
             |[6:6..6:7): r <= _empty_/n74.r.
             |[6:14..6:15): R => _empty_/n74.R#
             |[6:15..6:15):  => _empty_/n74.R#`<init>`().
             |[7:6..7:7): x <= _empty_/n74.x.
             |[8:6..8:7): a <= _empty_/n74.a.
             |[8:10..8:11): x => _empty_/n74.x.
             |[8:12..8:14): +: => _empty_/n74.R#`+:`().
             |[8:15..8:16): r => _empty_/n74.r.
             |[9:6..9:7): b <= _empty_/n74.b.
             |[9:10..9:14): List => scala/collection/immutable/List.
             |[9:18..9:20): +: => _empty_/n74.R#`+:`().
             |[9:21..9:22): r => _empty_/n74.r.
             |""".stripMargin
      },
      """|[0:7..0:10): n74 <= _empty_/n74.
         |[1:8..1:9): C <= _empty_/n74.C#
         |[1:9..1:9):  <= _empty_/n74.C#`<init>`().
         |[2:15..2:16): c <= _empty_/n74.c.
         |[2:18..2:19): C => _empty_/n74.C#
         |[2:26..2:27): C => _empty_/n74.C#
         |[2:27..2:27):  => _empty_/n74.C#`<init>`().
         |[3:8..3:9): R <= _empty_/n74.R#
         |[3:10..3:10):  <= _empty_/n74.R#`<init>`().
         |[3:16..3:18): +: <= _empty_/n74.R#`+:`().
         |[3:19..3:20): s <= _empty_/n74.R#`+:`().(s)
         |[3:22..3:28): String => scala/Predef.String#
         |[4:15..4:16): w <= _empty_/n74.w().
         |[4:17..4:18): i <= _empty_/n74.w().(i)
         |[4:20..4:23): Int => scala/Int#
         |[4:34..4:35): c <= _empty_/n74.w().(c)
         |[4:37..4:38): C => _empty_/n74.C#
         |[4:41..4:47): String => scala/Predef.String#
         |[4:50..4:51): i => _empty_/n74.w().(i)
         |[4:52..4:60): toString => scala/Any#toString().
         |[5:15..5:16): l <= _empty_/n74.l().
         |[5:17..5:19): xs <= _empty_/n74.l().(xs)
         |[5:21..5:25): List => scala/package.List#
         |[5:26..5:29): Int => scala/Int#
         |[5:33..5:39): String => scala/Predef.String#
         |[5:42..5:44): xs => _empty_/n74.l().(xs)
         |[5:45..5:53): toString => scala/collection/Seq#toString().
         |[6:6..6:7): r <= _empty_/n74.r.
         |[6:14..6:15): R => _empty_/n74.R#
         |[6:15..6:15):  => _empty_/n74.R#`<init>`().
         |[7:6..7:7): x <= _empty_/n74.x.
         |[8:6..8:7): a <= _empty_/n74.a.
         |[8:10..8:11): x => _empty_/n74.x.
         |[8:12..8:14): +: => _empty_/n74.R#`+:`().
         |[8:15..8:16): r => _empty_/n74.r.
         |[9:6..9:7): b <= _empty_/n74.b.
         |[9:10..9:14): List => scala/package.List.
         |[9:18..9:20): +: => _empty_/n74.R#`+:`().
         |[9:21..9:22): r => _empty_/n74.r.
         |""".stripMargin,
    ),
  )

  synthetics(
    "right-associative operators under a view with arguments",
    n74,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[8:10..8:11): x => w(*)(c)
             |  w => _empty_/n74.w().
             |  c => _empty_/n74.c.
             |[9:10..9:14): List => *.apply[Int]
             |  apply => scala/collection/immutable/List.apply().
             |  Int => scala/Int#
             |[9:10..9:17): List(1) => l(*)
             |  l => _empty_/n74.l().
             |""".stripMargin
      },
      """|[8:10..8:11): x => w(*)(c)
         |  w => _empty_/n74.w().
         |  c => _empty_/n74.c.
         |[9:10..9:14): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[9:10..9:17): List(1) => l(*)
         |  l => _empty_/n74.l().
         |""".stripMargin,
    ),
  )

  private val n75 =
    """|object n75 {
       |  class C
       |  implicit val c: C = new C
       |  class R { def +:(s: String)(implicit c: C) = this; def +(s: String) = this }
       |  class Q
       |  implicit class RQ(q: Q) { def +:(s: String) = q }
       |  implicit def v(i: Int): String = i.toString
       |  val r = new R
       |  val q = new Q
       |  val x = 2
       |  var s = new R
       |  val a = "s" +: r
       |  val b = x +: r
       |  val d = "s" +: q
       |  val e = x +: q
       |  val f = r + x
       |  s +:= x
       |}
       |""".stripMargin

  occurrences(
    "right-associative operators beside a view on the left operand",
    n75,
    """|[0:7..0:10): n75 <= _empty_/n75.
       |[1:8..1:9): C <= _empty_/n75.C#
       |[1:9..1:9):  <= _empty_/n75.C#`<init>`().
       |[2:15..2:16): c <= _empty_/n75.c.
       |[2:18..2:19): C => _empty_/n75.C#
       |[2:26..2:27): C => _empty_/n75.C#
       |[2:27..2:27):  => _empty_/n75.C#`<init>`().
       |[3:8..3:9): R <= _empty_/n75.R#
       |[3:10..3:10):  <= _empty_/n75.R#`<init>`().
       |[3:16..3:18): +: <= _empty_/n75.R#`+:`().
       |[3:19..3:20): s <= _empty_/n75.R#`+:`().(s)
       |[3:22..3:28): String => scala/Predef.String#
       |[3:39..3:40): c <= _empty_/n75.R#`+:`().(c)
       |[3:42..3:43): C => _empty_/n75.C#
       |[3:57..3:58): + <= _empty_/n75.R#`+`().
       |[3:59..3:60): s <= _empty_/n75.R#`+`().(s)
       |[3:62..3:68): String => scala/Predef.String#
       |[4:8..4:9): Q <= _empty_/n75.Q#
       |[4:9..4:9):  <= _empty_/n75.Q#`<init>`().
       |[5:17..5:19): RQ <= _empty_/n75.RQ#
       |[5:19..5:19):  <= _empty_/n75.RQ#`<init>`().
       |[5:20..5:21): q <= _empty_/n75.RQ#`<init>`().(q)
       |[5:20..5:21): q <= _empty_/n75.RQ#q.
       |[5:23..5:24): Q => _empty_/n75.Q#
       |[5:32..5:34): +: <= _empty_/n75.RQ#`+:`().
       |[5:35..5:36): s <= _empty_/n75.RQ#`+:`().(s)
       |[5:38..5:44): String => scala/Predef.String#
       |[5:48..5:49): q => _empty_/n75.RQ#q.
       |[6:15..6:16): v <= _empty_/n75.v().
       |[6:17..6:18): i <= _empty_/n75.v().(i)
       |[6:20..6:23): Int => scala/Int#
       |[6:26..6:32): String => scala/Predef.String#
       |[6:35..6:36): i => _empty_/n75.v().(i)
       |[6:37..6:45): toString => scala/Any#toString().
       |[7:6..7:7): r <= _empty_/n75.r.
       |[7:14..7:15): R => _empty_/n75.R#
       |[7:15..7:15):  => _empty_/n75.R#`<init>`().
       |[8:6..8:7): q <= _empty_/n75.q.
       |[8:14..8:15): Q => _empty_/n75.Q#
       |[8:15..8:15):  => _empty_/n75.Q#`<init>`().
       |[9:6..9:7): x <= _empty_/n75.x.
       |[10:6..10:7): s <= _empty_/n75.s().
       |[10:14..10:15): R => _empty_/n75.R#
       |[10:15..10:15):  => _empty_/n75.R#`<init>`().
       |[11:6..11:7): a <= _empty_/n75.a.
       |[11:14..11:16): +: => _empty_/n75.R#`+:`().
       |[11:17..11:18): r => _empty_/n75.r.
       |[12:6..12:7): b <= _empty_/n75.b.
       |[12:10..12:11): x => _empty_/n75.x.
       |[12:12..12:14): +: => _empty_/n75.R#`+:`().
       |[12:15..12:16): r => _empty_/n75.r.
       |[13:6..13:7): d <= _empty_/n75.d.
       |[13:14..13:16): +: => _empty_/n75.RQ#`+:`().
       |[13:17..13:18): q => _empty_/n75.q.
       |[14:6..14:7): e <= _empty_/n75.e.
       |[14:10..14:11): x => _empty_/n75.x.
       |[14:12..14:14): +: => _empty_/n75.RQ#`+:`().
       |[14:15..14:16): q => _empty_/n75.q.
       |[15:6..15:7): f <= _empty_/n75.f.
       |[15:10..15:11): r => _empty_/n75.r.
       |[15:12..15:13): + => _empty_/n75.R#`+`().
       |[15:14..15:15): x => _empty_/n75.x.
       |[16:2..16:3): s => _empty_/n75.`s_=`().
       |[16:4..16:7): +:= => _empty_/n75.R#`+:`().
       |[16:8..16:9): x => _empty_/n75.x.
       |""".stripMargin,
  )

  synthetics(
    "right-associative operators beside a view on the left operand",
    n75,
    """|[11:10..11:18): "s" +: r => *(c)
       |  c => _empty_/n75.c.
       |[12:10..12:11): x => v(*)
       |  v => _empty_/n75.v().
       |[12:10..12:16): x +: r => *(c)
       |  c => _empty_/n75.c.
       |[13:17..13:18): q => RQ(*)
       |  RQ => _empty_/n75.RQ().
       |[14:10..14:11): x => v(*)
       |  v => _empty_/n75.v().
       |[14:15..14:16): q => RQ(*)
       |  RQ => _empty_/n75.RQ().
       |[15:14..15:15): x => v(*)
       |  v => _empty_/n75.v().
       |[16:2..16:9): s +:= x => *(c)
       |  c => _empty_/n75.c.
       |[16:8..16:9): x => v(*)
       |  v => _empty_/n75.v().
       |""".stripMargin,
  )

  private val n76 =
    """|object n76 {
       |  class C
       |  implicit val c: C = new C
       |  class R { def +:(s: String) = this }
       |  class B { def +:(s: => String)(implicit c: C) = this }
       |  implicit def toS(r: R): String = ""
       |  implicit def v(i: Int): String = i.toString
       |  def f(s: String) = s
       |  final val K = 1
       |  val r = new R
       |  val q = new B
       |  val x = 2
       |  val a: String = x +: r
       |  val b: String = K +: r
       |  val d = f(x +: r)
       |  val g: Seq[Int] = 1 +: Array(2)
       |  val h = x +: q
       |  val i = { println(); x } +: r
       |  val j = { println(); x } +: q
       |  class B2 { def +:(i: => Int)(implicit c: C) = this }
       |  val k = x +: new B2
       |  val l = "s" +: q
       |  val m = collection.mutable.Map[String, Int]()
       |  def n() = "a"
       |  m(n()) += 1
       |}
       |""".stripMargin

  occurrences(
    "right-associative operators under a view on the result",
    n76,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): n76 <= _empty_/n76.
             |[1:8..1:9): C <= _empty_/n76.C#
             |[1:9..1:9):  <= _empty_/n76.C#`<init>`().
             |[2:15..2:16): c <= _empty_/n76.c.
             |[2:18..2:19): C => _empty_/n76.C#
             |[2:26..2:27): C => _empty_/n76.C#
             |[2:27..2:27):  => _empty_/n76.C#`<init>`().
             |[3:8..3:9): R <= _empty_/n76.R#
             |[3:10..3:10):  <= _empty_/n76.R#`<init>`().
             |[3:16..3:18): +: <= _empty_/n76.R#`+:`().
             |[3:19..3:20): s <= _empty_/n76.R#`+:`().(s)
             |[3:22..3:28): String => scala/Predef.String#
             |[4:8..4:9): B <= _empty_/n76.B#
             |[4:10..4:10):  <= _empty_/n76.B#`<init>`().
             |[4:16..4:18): +: <= _empty_/n76.B#`+:`().
             |[4:19..4:20): s <= _empty_/n76.B#`+:`().(s)
             |[4:25..4:31): String => scala/Predef.String#
             |[4:42..4:43): c <= _empty_/n76.B#`+:`().(c)
             |[4:45..4:46): C => _empty_/n76.C#
             |[5:15..5:18): toS <= _empty_/n76.toS().
             |[5:19..5:20): r <= _empty_/n76.toS().(r)
             |[5:22..5:23): R => _empty_/n76.R#
             |[5:26..5:32): String => scala/Predef.String#
             |[6:15..6:16): v <= _empty_/n76.v().
             |[6:17..6:18): i <= _empty_/n76.v().(i)
             |[6:20..6:23): Int => scala/Int#
             |[6:26..6:32): String => scala/Predef.String#
             |[6:35..6:36): i => _empty_/n76.v().(i)
             |[6:37..6:45): toString => scala/Any#toString().
             |[7:6..7:7): f <= _empty_/n76.f().
             |[7:8..7:9): s <= _empty_/n76.f().(s)
             |[7:11..7:17): String => scala/Predef.String#
             |[7:21..7:22): s => _empty_/n76.f().(s)
             |[8:12..8:13): K <= _empty_/n76.K.
             |[9:6..9:7): r <= _empty_/n76.r.
             |[9:14..9:15): R => _empty_/n76.R#
             |[9:15..9:15):  => _empty_/n76.R#`<init>`().
             |[10:6..10:7): q <= _empty_/n76.q.
             |[10:14..10:15): B => _empty_/n76.B#
             |[10:15..10:15):  => _empty_/n76.B#`<init>`().
             |[11:6..11:7): x <= _empty_/n76.x.
             |[12:6..12:7): a <= _empty_/n76.a.
             |[12:9..12:15): String => scala/Predef.String#
             |[12:18..12:19): x => _empty_/n76.x.
             |[12:20..12:22): +: => _empty_/n76.R#`+:`().
             |[12:23..12:24): r => _empty_/n76.r.
             |[13:6..13:7): b <= _empty_/n76.b.
             |[13:9..13:15): String => scala/Predef.String#
             |[13:18..13:19): K => _empty_/n76.K.
             |[13:20..13:22): +: => _empty_/n76.R#`+:`().
             |[13:23..13:24): r => _empty_/n76.r.
             |[14:6..14:7): d <= _empty_/n76.d.
             |[14:10..14:11): f => _empty_/n76.f().
             |[14:12..14:13): x => _empty_/n76.x.
             |[14:14..14:16): +: => _empty_/n76.R#`+:`().
             |[14:17..14:18): r => _empty_/n76.r.
             |[15:6..15:7): g <= _empty_/n76.g.
             |[15:9..15:12): Seq => scala/package.Seq#
             |[15:13..15:16): Int => scala/Int#
             |[15:22..15:24): +: => scala/collection/SeqLike#`+:`().
             |[15:25..15:30): Array => scala/Array.
             |[16:6..16:7): h <= _empty_/n76.h.
             |[16:10..16:11): x => _empty_/n76.x.
             |[16:12..16:14): +: => _empty_/n76.B#`+:`().
             |[16:15..16:16): q => _empty_/n76.q.
             |[17:6..17:7): i <= _empty_/n76.i.
             |[17:12..17:19): println => scala/Predef.println().
             |[17:23..17:24): x => _empty_/n76.x.
             |[17:27..17:29): +: => _empty_/n76.R#`+:`().
             |[17:30..17:31): r => _empty_/n76.r.
             |[18:6..18:7): j <= _empty_/n76.j.
             |[18:12..18:19): println => scala/Predef.println().
             |[18:23..18:24): x => _empty_/n76.x.
             |[18:27..18:29): +: => _empty_/n76.B#`+:`().
             |[18:30..18:31): q => _empty_/n76.q.
             |[19:8..19:10): B2 <= _empty_/n76.B2#
             |[19:11..19:11):  <= _empty_/n76.B2#`<init>`().
             |[19:17..19:19): +: <= _empty_/n76.B2#`+:`().
             |[19:20..19:21): i <= _empty_/n76.B2#`+:`().(i)
             |[19:26..19:29): Int => scala/Int#
             |[19:40..19:41): c <= _empty_/n76.B2#`+:`().(c)
             |[19:43..19:44): C => _empty_/n76.C#
             |[20:6..20:7): k <= _empty_/n76.k.
             |[20:10..20:11): x => _empty_/n76.x.
             |[20:12..20:14): +: => _empty_/n76.B2#`+:`().
             |[20:19..20:21): B2 => _empty_/n76.B2#
             |[20:21..20:21):  => _empty_/n76.B2#`<init>`().
             |[21:6..21:7): l <= _empty_/n76.l.
             |[21:14..21:16): +: => _empty_/n76.B#`+:`().
             |[21:17..21:18): q => _empty_/n76.q.
             |[22:6..22:7): m <= _empty_/n76.m.
             |[22:10..22:20): collection => scala/collection/
             |[22:21..22:28): mutable => scala/collection/mutable/
             |[22:29..22:32): Map => scala/collection/mutable/Map.
             |[22:33..22:39): String => scala/Predef.String#
             |[22:41..22:44): Int => scala/Int#
             |[23:6..23:7): n <= _empty_/n76.n().
             |[24:2..24:3): m => _empty_/n76.m.
             |[24:4..24:5): n => _empty_/n76.n().
             |[24:9..24:11): += => scala/Int#`+`(+4).
             |""".stripMargin
      },
      """|[0:7..0:10): n76 <= _empty_/n76.
         |[1:8..1:9): C <= _empty_/n76.C#
         |[1:9..1:9):  <= _empty_/n76.C#`<init>`().
         |[2:15..2:16): c <= _empty_/n76.c.
         |[2:18..2:19): C => _empty_/n76.C#
         |[2:26..2:27): C => _empty_/n76.C#
         |[2:27..2:27):  => _empty_/n76.C#`<init>`().
         |[3:8..3:9): R <= _empty_/n76.R#
         |[3:10..3:10):  <= _empty_/n76.R#`<init>`().
         |[3:16..3:18): +: <= _empty_/n76.R#`+:`().
         |[3:19..3:20): s <= _empty_/n76.R#`+:`().(s)
         |[3:22..3:28): String => scala/Predef.String#
         |[4:8..4:9): B <= _empty_/n76.B#
         |[4:10..4:10):  <= _empty_/n76.B#`<init>`().
         |[4:16..4:18): +: <= _empty_/n76.B#`+:`().
         |[4:19..4:20): s <= _empty_/n76.B#`+:`().(s)
         |[4:25..4:31): String => scala/Predef.String#
         |[4:42..4:43): c <= _empty_/n76.B#`+:`().(c)
         |[4:45..4:46): C => _empty_/n76.C#
         |[5:15..5:18): toS <= _empty_/n76.toS().
         |[5:19..5:20): r <= _empty_/n76.toS().(r)
         |[5:22..5:23): R => _empty_/n76.R#
         |[5:26..5:32): String => scala/Predef.String#
         |[6:15..6:16): v <= _empty_/n76.v().
         |[6:17..6:18): i <= _empty_/n76.v().(i)
         |[6:20..6:23): Int => scala/Int#
         |[6:26..6:32): String => scala/Predef.String#
         |[6:35..6:36): i => _empty_/n76.v().(i)
         |[6:37..6:45): toString => scala/Any#toString().
         |[7:6..7:7): f <= _empty_/n76.f().
         |[7:8..7:9): s <= _empty_/n76.f().(s)
         |[7:11..7:17): String => scala/Predef.String#
         |[7:21..7:22): s => _empty_/n76.f().(s)
         |[8:12..8:13): K <= _empty_/n76.K.
         |[9:6..9:7): r <= _empty_/n76.r.
         |[9:14..9:15): R => _empty_/n76.R#
         |[9:15..9:15):  => _empty_/n76.R#`<init>`().
         |[10:6..10:7): q <= _empty_/n76.q.
         |[10:14..10:15): B => _empty_/n76.B#
         |[10:15..10:15):  => _empty_/n76.B#`<init>`().
         |[11:6..11:7): x <= _empty_/n76.x.
         |[12:6..12:7): a <= _empty_/n76.a.
         |[12:9..12:15): String => scala/Predef.String#
         |[12:18..12:19): x => _empty_/n76.x.
         |[12:20..12:22): +: => _empty_/n76.R#`+:`().
         |[12:23..12:24): r => _empty_/n76.r.
         |[13:6..13:7): b <= _empty_/n76.b.
         |[13:9..13:15): String => scala/Predef.String#
         |[13:18..13:19): K => _empty_/n76.K.
         |[13:20..13:22): +: => _empty_/n76.R#`+:`().
         |[13:23..13:24): r => _empty_/n76.r.
         |[14:6..14:7): d <= _empty_/n76.d.
         |[14:10..14:11): f => _empty_/n76.f().
         |[14:12..14:13): x => _empty_/n76.x.
         |[14:14..14:16): +: => _empty_/n76.R#`+:`().
         |[14:17..14:18): r => _empty_/n76.r.
         |[15:6..15:7): g <= _empty_/n76.g.
         |[15:9..15:12): Seq => scala/package.Seq#
         |[15:13..15:16): Int => scala/Int#
         |[15:22..15:24): +: => scala/collection/ArrayOps#`+:`().
         |[15:25..15:30): Array => scala/Array.
         |[16:6..16:7): h <= _empty_/n76.h.
         |[16:10..16:11): x => _empty_/n76.x.
         |[16:12..16:14): +: => _empty_/n76.B#`+:`().
         |[16:15..16:16): q => _empty_/n76.q.
         |[17:6..17:7): i <= _empty_/n76.i.
         |[17:12..17:19): println => scala/Predef.println().
         |[17:23..17:24): x => _empty_/n76.x.
         |[17:27..17:29): +: => _empty_/n76.R#`+:`().
         |[17:30..17:31): r => _empty_/n76.r.
         |[18:6..18:7): j <= _empty_/n76.j.
         |[18:12..18:19): println => scala/Predef.println().
         |[18:23..18:24): x => _empty_/n76.x.
         |[18:27..18:29): +: => _empty_/n76.B#`+:`().
         |[18:30..18:31): q => _empty_/n76.q.
         |[19:8..19:10): B2 <= _empty_/n76.B2#
         |[19:11..19:11):  <= _empty_/n76.B2#`<init>`().
         |[19:17..19:19): +: <= _empty_/n76.B2#`+:`().
         |[19:20..19:21): i <= _empty_/n76.B2#`+:`().(i)
         |[19:26..19:29): Int => scala/Int#
         |[19:40..19:41): c <= _empty_/n76.B2#`+:`().(c)
         |[19:43..19:44): C => _empty_/n76.C#
         |[20:6..20:7): k <= _empty_/n76.k.
         |[20:10..20:11): x => _empty_/n76.x.
         |[20:12..20:14): +: => _empty_/n76.B2#`+:`().
         |[20:19..20:21): B2 => _empty_/n76.B2#
         |[20:21..20:21):  => _empty_/n76.B2#`<init>`().
         |[21:6..21:7): l <= _empty_/n76.l.
         |[21:14..21:16): +: => _empty_/n76.B#`+:`().
         |[21:17..21:18): q => _empty_/n76.q.
         |[22:6..22:7): m <= _empty_/n76.m.
         |[22:10..22:20): collection => scala/collection/
         |[22:21..22:28): mutable => scala/collection/mutable/
         |[22:29..22:32): Map => scala/collection/mutable/Map.
         |[22:33..22:39): String => scala/Predef.String#
         |[22:41..22:44): Int => scala/Int#
         |[23:6..23:7): n <= _empty_/n76.n().
         |[24:2..24:3): m => _empty_/n76.m.
         |[24:4..24:5): n => _empty_/n76.n().
         |[24:9..24:11): += => scala/Int#`+`(+4).
         |""".stripMargin,
    ),
  )

  synthetics(
    "right-associative operators under a view on the result",
    n76,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[12:18..12:19): x => v(*)
             |  v => _empty_/n76.v().
             |[12:18..12:24): x +: r => toS(*)
             |  toS => _empty_/n76.toS().
             |[13:18..13:19): K => v(*)
             |  v => _empty_/n76.v().
             |[13:18..13:24): K +: r => toS(*)
             |  toS => _empty_/n76.toS().
             |[14:12..14:13): x => v(*)
             |  v => _empty_/n76.v().
             |[14:12..14:18): x +: r => toS(*)
             |  toS => _empty_/n76.toS().
             |[15:20..15:33): 1 +: Array(2) => *(fallbackCanBuildFrom(dummyImplicit))
             |  fallbackCanBuildFrom => scala/FallbackArrayBuilding#fallbackCanBuildFrom().
             |  dummyImplicit => scala/Predef.DummyImplicit.dummyImplicit().
             |[15:22..15:33): +: Array(2) => *[Int, Seq[Int]]
             |  Int => scala/Int#
             |  Seq => scala/package.Seq#
             |[15:25..15:30): Array => *.apply
             |  apply => scala/Array.apply(+5).
             |[15:25..15:33): Array(2) => intArrayOps(*)
             |  intArrayOps => scala/Predef.intArrayOps().
             |[16:10..16:11): x => v(*)
             |  v => _empty_/n76.v().
             |[16:10..16:16): x +: q => *(c)
             |  c => _empty_/n76.c.
             |[17:10..17:26): { println(); x } => v(*)
             |  v => _empty_/n76.v().
             |[18:10..18:26): { println(); x } => v(*)
             |  v => _empty_/n76.v().
             |[18:10..18:31): { println(); x } +: q => *(c)
             |  c => _empty_/n76.c.
             |[20:10..20:21): x +: new B2 => *(c)
             |  c => _empty_/n76.c.
             |[21:10..21:18): "s" +: q => *(c)
             |  c => _empty_/n76.c.
             |[22:10..22:32): collection.mutable.Map => *.apply
             |  apply => scala/collection/generic/GenMapFactory#apply().
             |[24:2..24:3): m => *.apply
             |  apply => scala/collection/MapLike#apply().
             |""".stripMargin
      },
      """|[12:18..12:19): x => v(*)
         |  v => _empty_/n76.v().
         |[12:18..12:24): x +: r => toS(*)
         |  toS => _empty_/n76.toS().
         |[13:18..13:19): K => v(*)
         |  v => _empty_/n76.v().
         |[13:18..13:24): K +: r => toS(*)
         |  toS => _empty_/n76.toS().
         |[14:12..14:13): x => v(*)
         |  v => _empty_/n76.v().
         |[14:12..14:18): x +: r => toS(*)
         |  toS => _empty_/n76.toS().
         |[15:20..15:33): 1 +: Array(2) => Predef.copyArrayToImmutableIndexedSeq[Int](*)
         |  Predef => scala/Predef.
         |  copyArrayToImmutableIndexedSeq => scala/LowPriorityImplicits2#copyArrayToImmutableIndexedSeq().
         |  Int => scala/Int#
         |[15:20..15:33): 1 +: Array(2) => *((`macro-expandee` : ClassTag[Int]))
         |  ClassTag => scala/reflect/ClassTag#
         |  Int => scala/Int#
         |[15:22..15:33): +: Array(2) => *[Int]
         |  Int => scala/Int#
         |[15:25..15:30): Array => *.apply
         |  apply => scala/Array.apply(+5).
         |[15:25..15:33): Array(2) => intArrayOps(*)
         |  intArrayOps => scala/Predef.intArrayOps().
         |[16:10..16:11): x => v(*)
         |  v => _empty_/n76.v().
         |[16:10..16:16): x +: q => *(c)
         |  c => _empty_/n76.c.
         |[17:10..17:26): { println(); x } => v(*)
         |  v => _empty_/n76.v().
         |[18:10..18:26): { println(); x } => v(*)
         |  v => _empty_/n76.v().
         |[18:10..18:31): { println(); x } +: q => *(c)
         |  c => _empty_/n76.c.
         |[20:10..20:21): x +: new B2 => *(c)
         |  c => _empty_/n76.c.
         |[21:10..21:18): "s" +: q => *(c)
         |  c => _empty_/n76.c.
         |[22:10..22:32): collection.mutable.Map => *.apply
         |  apply => scala/collection/MapFactory.Delegate#apply().
         |[24:2..24:3): m => *.apply
         |  apply => scala/collection/MapOps#apply().
         |""".stripMargin,
    ),
  )

  test("synthetic locals of a match after patmat") {
    computeDatabaseFromSnippet(
      """|object n72 {
         |  def m(x: Any) = x match { case i: Int => i }
         |}
         |""".stripMargin,
    )
    val unit = g.currentRun.units.toList.last
    g.currentRun.phaseNamed("patmat").asInstanceOf[g.GlobalPhase].apply(unit)
    val locals = unit.body.collect { case t: g.ValDef if t.symbol.isSynthetic => t.symbol }
    assertNotEquals(locals, Nil)
    assertEquals(locals.filter(_.attachments.contains[TyperOps#ValDefRhs]), Nil)
  }

}

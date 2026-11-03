package scala.meta.tests.semanticdb

class NameRewriteSuite extends SemanticdbSuite {

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

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

}

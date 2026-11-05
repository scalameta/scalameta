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
         |[6:12..6:14): :: => scala/collection/immutable/List#`::`().
         |[6:15..6:18): Nil => scala/package.Nil.
         |[7:6..7:7): b <= _empty_/n21.b.
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
         |[10:12..10:14): +: => scala/collection/SeqOps#`+:`().
         |[10:15..10:18): Seq => scala/package.Seq.
         |[11:6..11:7): f <= _empty_/n21.f.
         |[11:23..11:25): :: => scala/collection/immutable/List#`::`().
         |[11:26..11:29): Nil => scala/package.Nil.
         |[12:6..12:7): g <= _empty_/n21.g.
         |[12:12..12:14): +: => _empty_/n21.R#`+:`().
         |[12:15..12:16): r => _empty_/n21.r.
         |[13:6..13:7): h <= _empty_/n21.h.
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
         |[16:13..16:15): :: => scala/collection/immutable/List#`::`().
         |[16:16..16:19): Nil => scala/package.Nil.
         |[17:6..17:7): l <= _empty_/n21.l.
         |[17:16..17:18): :: => scala/collection/immutable/List#`::`().
         |[17:19..17:22): Nil => scala/package.Nil.
         |[18:6..18:7): m <= _empty_/n21.m.
         |[18:12..18:14): :: => scala/collection/immutable/List#`::`().
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
             |[4:16..4:20): List => scala/collection/immutable/List.
             |[5:6..5:7): e <= _empty_/n23.e.
             |[5:16..5:19): Seq => scala/collection/Seq.
             |[6:6..6:7): f <= _empty_/n23.f.
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
             |[1:23..1:27): List => scala/collection/immutable/List.
             |[2:6..2:7): b <= _empty_/n27.b.
             |[2:23..2:26): Seq => scala/collection/Seq.
             |[3:6..3:7): c <= _empty_/n27.c.
             |[3:23..3:26): Nil => scala/collection/immutable/Nil.
             |[4:6..4:7): d <= _empty_/n27.d.
             |[4:23..4:27): List => scala/collection/immutable/Nil.
             |[5:6..5:7): e <= _empty_/n27.e.
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
             |[3:6..3:7): b <= _empty_/n28.b.
             |[4:6..4:7): c <= _empty_/n28.c.
             |[5:6..5:7): d <= _empty_/n28.d.
             |[6:6..6:7): e <= _empty_/n28.e.
             |[6:12..6:17): empty => scala/collection/immutable/List.empty().
             |[6:18..6:21): Int => scala/Int#
             |[7:6..7:7): f <= _empty_/n28.f.
             |[7:10..7:11): L => scala/collection/immutable/Nil.
             |[8:6..8:7): g <= _empty_/n28.g().
             |[8:8..8:10): xs <= _empty_/n28.g().(xs)
             |[8:12..8:16): List => scala/package.List#
             |[8:17..8:20): Int => scala/Int#
             |[8:25..8:27): xs => _empty_/n28.g().(xs)
             |[9:11..9:12): x <= local0
             |[9:17..9:18): x => local0
             |[11:9..11:10): x <= local1
             |[11:11..11:13): :: => scala/package.`::`.
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
             |[5:15..5:19): List => scala/collection/immutable/List.
             |[5:20..5:21): x <= local2
             |[5:29..5:30): x => local2
             |[6:11..6:13): :: => scala/package.`::`.
             |[6:20..6:23): Nil => scala/collection/immutable/Nil.
             |[7:15..7:18): Nil => scala/collection/immutable/Nil.
             |[10:6..10:7): g <= _empty_/n32.g().
             |[10:8..10:10): xs <= _empty_/n32.g().(xs)
             |[10:12..10:15): Seq => scala/package.Seq#
             |[10:16..10:19): Int => scala/Int#
             |[10:24..10:26): xs => _empty_/n32.g().(xs)
             |[11:9..11:12): Seq => scala/collection/Seq.
             |[11:13..11:14): x <= local3
             |[11:19..11:20): x => local3
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
             |[4:15..4:18): Nil => scala/collection/immutable/Nil.
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
             |[4:16..4:21): `Nil` => scala/collection/immutable/Nil.
             |[5:6..5:7): e <= _empty_/n36.e.
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
             |[2:32..2:33): b <= _empty_/n38.i1.b.
             |[2:46..2:47): c <= _empty_/n38.i1.c.
             |[3:6..3:7): f <= _empty_/n38.f().
             |[3:16..3:17): a <= local0
             |[3:32..3:33): b <= local1
             |[3:39..3:40): b => local1
             |[4:8..4:9): C <= _empty_/n38.C#
             |[4:10..4:10):  <= _empty_/n38.C#`<init>`().
             |[4:19..4:24): scala => scala/
             |[4:26..4:30): List => scala/package.List#
             |[4:26..4:30): List => scala/package.List.
             |[4:42..4:43): g <= _empty_/n38.C#g().
             |[4:52..4:53): a <= local2
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
             |[4:10..4:11): E <= _empty_/n40.i1.E#
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
             |[1:28..1:32): List => scala/collection/immutable/List.
             |[2:6..2:7): b <= _empty_/n41.b.
             |[2:10..2:17): classOf => scala/Predef.classOf().
             |[2:18..2:21): Seq => scala/package.Seq#
             |[2:28..2:31): Seq => scala/collection/Seq.
             |[3:6..3:7): c <= _empty_/n41.c.
             |[3:10..3:17): classOf => scala/Predef.classOf().
             |[3:18..3:21): Seq => scala/package.Seq#
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
             |[8:10..8:11): c <= _empty_/n44.o1.i.c.
             |[9:10..9:11): d <= _empty_/n44.o1.i.d.
             |[9:14..9:21): classOf => scala/Predef.classOf().
             |[9:22..9:25): Seq => scala/package.Seq#
             |[9:26..9:27): L => scala/collection/immutable/List.
             |[10:10..10:11): e <= _empty_/n44.o1.i.e.
             |[11:10..11:11): f <= _empty_/n44.o1.i.f.
             |[12:10..12:11): g <= _empty_/n44.o1.i.g.
             |[12:14..12:21): classOf => scala/Predef.classOf().
             |[12:22..12:25): Seq => scala/package.Seq#
             |[12:26..12:27): S => scala/collection/Seq.
             |[13:10..13:11): h <= _empty_/n44.o1.i.h.
             |[14:10..14:11): k <= _empty_/n44.o1.i.k.
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
       |[3:24..3:27): Seq => scala/collection/Seq.
       |[3:28..3:47): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[3:48..3:51): Int => scala/Int#
       |[4:6..4:7): d <= _empty_/n45.d.
       |[4:10..4:17): classOf => scala/Predef.classOf().
       |[4:18..4:21): Seq => scala/collection/Seq.
       |[4:22..4:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[4:42..4:45): Int => scala/Int#
       |[5:6..5:7): e <= _empty_/n45.e.
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
       |[11:22..11:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[11:42..11:45): Int => scala/Int#
       |[12:8..12:9): b <= _empty_/n45.i.b.
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
             |[4:8..4:9): b <= _empty_/n47.o1.b.
             |[4:17..4:22): empty => scala/collection/generic/GenericCompanion#empty().
             |[5:8..5:9): c <= _empty_/n47.o1.c.
             |[5:12..5:19): classOf => scala/Predef.classOf().
             |[5:20..5:23): Seq => scala/package.Seq#
             |[6:8..6:9): d <= _empty_/n47.o1.d.
             |[7:8..7:9): f <= _empty_/n47.o1.f().
             |[7:10..7:11): x <= _empty_/n47.o1.f().(x)
             |[7:13..7:16): Any => scala/Any#
             |[7:20..7:21): x => _empty_/n47.o1.f().(x)
             |[7:40..7:41): y <= local1
             |[7:46..7:47): y => local1
             |[9:9..9:11): o2 <= _empty_/n47.o2.
             |[10:11..10:16): scala => scala/
             |[10:18..10:21): Nil => scala/package.Nil.
             |[10:31..10:35): List => scala/package.List#
             |[10:31..10:35): List => scala/package.List.
             |[11:8..11:9): a <= _empty_/n47.o2.a.
             |[12:8..12:9): b <= _empty_/n47.o2.b.
             |[13:8..13:9): c <= _empty_/n47.o2.c.
             |[13:12..13:19): classOf => scala/Predef.classOf().
             |[13:20..13:23): Seq => scala/package.List#
             |[14:8..14:9): f <= _empty_/n47.o2.f().
             |[14:10..14:11): x <= _empty_/n47.o2.f().(x)
             |[14:13..14:16): Any => scala/Any#
             |[14:20..14:21): x => _empty_/n47.o2.f().(x)
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
             |[4:8..4:9): b <= local1
             |[4:14..4:19): empty => scala/collection/generic/GenericCompanion#empty().
             |[5:8..5:9): c <= local2
             |[6:4..6:11): classOf => scala/Predef.classOf().
             |[6:12..6:15): Seq => scala/package.Seq#
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
             |[19:25..19:26): b <= _empty_/n48.i.f().(b)
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
       |[5:20..5:23): Seq => scala/collection/Seq.
       |[5:24..5:43): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[5:44..5:47): Int => scala/Int#
       |[5:48..5:48):  => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#`<init>`().
       |[6:9..6:10): i <= _empty_/n51.i.
       |[7:11..7:16): scala => scala/
       |[7:18..7:22): List => scala/package.List#
       |[7:18..7:22): List => scala/package.List.
       |[8:10..8:11): A <= _empty_/n51.i.A#
       |[8:22..8:41): GenericCanBuildFrom => scala/collection/generic/GenTraversableFactory#GenericCanBuildFrom#
       |[8:42..8:45): Int => scala/Int#
       |[9:10..9:11): C <= _empty_/n51.i.C#
       |[9:12..9:12):  <= _empty_/n51.i.C#`<init>`().
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
             |[2:22..2:25): Nil => scala/collection/immutable/Nil.
             |[3:22..3:26): List => scala/collection/immutable/List.
             |[3:27..3:28): y <= local1
             |[4:11..4:13): :: => scala/package.`::`.
             |[4:27..4:30): Nil => scala/collection/immutable/Nil.
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

}

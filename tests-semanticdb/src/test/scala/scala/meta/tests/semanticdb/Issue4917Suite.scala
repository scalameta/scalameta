package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/pull/4917
class Issue4917Suite extends SemanticdbSuite {

  private val classfile =
    """|package a
       |class C {
       |  val memberVal = new advanced.Structural().s1
       |  val memberValX = memberVal.x
       |  def memberDef = new advanced.Structural().s1
       |  val memberDefX = memberDef.x
       |  val chainX = new advanced.Structural().s1.x
       |  val anonX = new advanced.Structural().s2.x
       |  val methodM = new advanced.Structural().s3.m(1)
       |  def m(): Unit = {
       |    val localVal = new advanced.Structural().s1
       |    val localX = localVal.x
       |  }
       |}
       |""".stripMargin

  private val sameFile =
    """|package b
       |class C {
       |  val memberVal = new { val x = 1; def m(i: Int): Int = i }
       |  val memberValX = memberVal.x
       |  val methodM = memberVal.m(1)
       |  val typeVal: { type T = Int } = ???
       |  val typeT: typeVal.T = 1
       |  def m(): Unit = {
       |    val localVal = new { val x = 1 }
       |    val localX = localVal.x
       |  }
       |}
       |""".stripMargin

  occurrences(
    classfile,
    """|[0:8..0:9): a <= a/
       |[1:6..1:7): C <= a/C#
       |[1:8..1:8):  <= a/C#`<init>`().
       |[2:6..2:15): memberVal <= a/C#memberVal.
       |[2:22..2:30): advanced => advanced/
       |[2:31..2:41): Structural => advanced/Structural#
       |[2:41..2:41):  => advanced/Structural#`<init>`().
       |[2:44..2:46): s1 => advanced/Structural#s1().
       |[3:6..3:16): memberValX <= a/C#memberValX.
       |[3:19..3:28): memberVal => a/C#memberVal.
       |[4:6..4:15): memberDef <= a/C#memberDef().
       |[4:22..4:30): advanced => advanced/
       |[4:31..4:41): Structural => advanced/Structural#
       |[4:41..4:41):  => advanced/Structural#`<init>`().
       |[4:44..4:46): s1 => advanced/Structural#s1().
       |[5:6..5:16): memberDefX <= a/C#memberDefX.
       |[5:19..5:28): memberDef => a/C#memberDef().
       |[6:6..6:12): chainX <= a/C#chainX.
       |[6:19..6:27): advanced => advanced/
       |[6:28..6:38): Structural => advanced/Structural#
       |[6:38..6:38):  => advanced/Structural#`<init>`().
       |[6:41..6:43): s1 => advanced/Structural#s1().
       |[7:6..7:11): anonX <= a/C#anonX.
       |[7:18..7:26): advanced => advanced/
       |[7:27..7:37): Structural => advanced/Structural#
       |[7:37..7:37):  => advanced/Structural#`<init>`().
       |[7:40..7:42): s2 => advanced/Structural#s2().
       |[8:6..8:13): methodM <= a/C#methodM.
       |[8:20..8:28): advanced => advanced/
       |[8:29..8:39): Structural => advanced/Structural#
       |[8:39..8:39):  => advanced/Structural#`<init>`().
       |[8:42..8:44): s3 => advanced/Structural#s3().
       |[9:6..9:7): m <= a/C#m().
       |[9:11..9:15): Unit => scala/Unit#
       |[10:8..10:16): localVal <= local1
       |[10:23..10:31): advanced => advanced/
       |[10:32..10:42): Structural => advanced/Structural#
       |[10:42..10:42):  => advanced/Structural#`<init>`().
       |[10:45..10:47): s1 => advanced/Structural#s1().
       |[11:8..11:14): localX <= local2
       |[11:17..11:25): localVal => local1
       |""".stripMargin,
  )

  symbols(
    // local numbers continue across compiles of one text, so compile another
    classfile.replace("package a", "package c"),
    """|c/C# => class C extends AnyRef { +9 decls }
       |  AnyRef => scala/AnyRef#
       |c/C#`<init>`(). => primary ctor <init>()
       |c/C#anonX. => val method anonX: Int
       |  Int => scala/Int#
       |c/C#chainX. => val method chainX: Int
       |  Int => scala/Int#
       |c/C#m(). => method m(): Unit
       |  Unit => scala/Unit#
       |c/C#memberDef(). => method memberDef: AnyRef { val def x: Int }
       |  AnyRef => scala/AnyRef#
       |  x => local0
       |  Int => scala/Int#
       |c/C#memberDefX. => val method memberDefX: Int
       |  Int => scala/Int#
       |c/C#memberVal. => val method memberVal: AnyRef { val def x: Int }
       |  AnyRef => scala/AnyRef#
       |  x => local0
       |  Int => scala/Int#
       |c/C#memberValX. => val method memberValX: Int
       |  Int => scala/Int#
       |c/C#methodM. => val method methodM: Int
       |  Int => scala/Int#
       |local1 => val local localVal: AnyRef { val def x: Int }
       |  AnyRef => scala/AnyRef#
       |  x => local0
       |  Int => scala/Int#
       |local2 => val local localX: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    sameFile,
    """|[0:8..0:9): b <= b/
       |[1:6..1:7): C <= b/C#
       |[1:8..1:8):  <= b/C#`<init>`().
       |[2:6..2:15): memberVal <= b/C#memberVal.
       |[2:22..2:22):  <= local0
       |[2:28..2:29): x <= local1
       |[2:39..2:40): m <= local2
       |[2:41..2:42): i <= local3
       |[2:44..2:47): Int => scala/Int#
       |[2:50..2:53): Int => scala/Int#
       |[2:56..2:57): i => local3
       |[3:6..3:16): memberValX <= b/C#memberValX.
       |[3:19..3:28): memberVal => b/C#memberVal.
       |[3:29..3:30): x => local1
       |[4:6..4:13): methodM <= b/C#methodM.
       |[4:16..4:25): memberVal => b/C#memberVal.
       |[4:26..4:27): m => local2
       |[5:6..5:13): typeVal <= b/C#typeVal.
       |[5:22..5:23): T <= local4
       |[5:26..5:29): Int => scala/Int#
       |[5:34..5:37): ??? => scala/Predef.`???`().
       |[6:6..6:11): typeT <= b/C#typeT.
       |[6:13..6:20): typeVal => b/C#typeVal.
       |[6:21..6:22): T => local4
       |[7:6..7:7): m <= b/C#m().
       |[7:11..7:15): Unit => scala/Unit#
       |[8:8..8:16): localVal <= local7
       |[8:23..8:23):  <= local5
       |[8:29..8:30): x <= local6
       |[9:8..9:14): localX <= local8
       |[9:17..9:25): localVal => local7
       |[9:26..9:27): x => local6
       |""".stripMargin,
  )

}

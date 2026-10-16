package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1829
class Issue1829Suite extends SemanticdbSuite {

  occurrences(
    """|package a
       |import scala.language.implicitConversions
       |class Ops[A](target: A) {
       |  def map(fn: A => A): A = fn(target)
       |}
       |object ops {
       |  implicit def toOps[A](target: A)(implicit tc: Ops[A]): Ops[A] = new Ops[A](target)
       |}
       |object Main {
       |  import ops._
       |  implicit val intOps: Ops[Int] = ???
       |  def number: Int = 42
       |  for {
       |    n <- number
       |  } yield n + 1
       |}
       |""".stripMargin,
    """|[0:8..0:9): a <= a/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:41): implicitConversions => scala/language.implicitConversions.
       |[2:6..2:9): Ops <= a/Ops#
       |[2:10..2:11): A <= a/Ops#[A]
       |[2:12..2:12):  <= a/Ops#`<init>`().
       |[2:13..2:19): target <= a/Ops#`<init>`().(target)
       |[2:13..2:19): target <= a/Ops#target.
       |[2:21..2:22): A => a/Ops#[A]
       |[3:6..3:9): map <= a/Ops#map().
       |[3:10..3:12): fn <= a/Ops#map().(fn)
       |[3:14..3:15): A => a/Ops#[A]
       |[3:19..3:20): A => a/Ops#[A]
       |[3:23..3:24): A => a/Ops#[A]
       |[3:27..3:29): fn => a/Ops#map().(fn)
       |[3:30..3:36): target => a/Ops#target.
       |[5:7..5:10): ops <= a/ops.
       |[6:15..6:20): toOps <= a/ops.toOps().
       |[6:21..6:22): A <= a/ops.toOps().[A]
       |[6:24..6:30): target <= a/ops.toOps().(target)
       |[6:32..6:33): A => a/ops.toOps().[A]
       |[6:44..6:46): tc <= a/ops.toOps().(tc)
       |[6:48..6:51): Ops => a/Ops#
       |[6:52..6:53): A => a/ops.toOps().[A]
       |[6:57..6:60): Ops => a/Ops#
       |[6:61..6:62): A => a/ops.toOps().[A]
       |[6:70..6:73): Ops => a/Ops#
       |[6:74..6:75): A => a/ops.toOps().[A]
       |[6:76..6:76):  => a/Ops#`<init>`().
       |[6:77..6:83): target => a/ops.toOps().(target)
       |[8:7..8:11): Main <= a/Main.
       |[9:9..9:12): ops => a/ops.
       |[10:15..10:21): intOps <= a/Main.intOps.
       |[10:23..10:26): Ops => a/Ops#
       |[10:27..10:30): Int => scala/Int#
       |[10:34..10:37): ??? => scala/Predef.`???`().
       |[11:6..11:12): number <= a/Main.number().
       |[11:14..11:17): Int => scala/Int#
       |[13:4..13:5): n <= local0
       |[13:9..13:15): number => a/Main.number().
       |[14:10..14:11): n => local0
       |[14:12..14:13): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  occurrences(
    """|package b
       |import scala.language.implicitConversions
       |object Main {
       |  class Ops[A](a: A) { def parT: Option[Int] = None }
       |  implicit def toPar[A](a: A): Ops[A] = new Ops(a)
       |  def logins(): Int = 0
       |  def transactions(): Option[Int] =
       |    for {
       |      logins <- Option(logins())
       |      t <- logins.parT
       |    } yield t
       |}
       |""".stripMargin,
    """|[0:8..0:9): b <= b/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:41): implicitConversions => scala/language.implicitConversions.
       |[2:7..2:11): Main <= b/Main.
       |[3:8..3:11): Ops <= b/Main.Ops#
       |[3:12..3:13): A <= b/Main.Ops#[A]
       |[3:14..3:14):  <= b/Main.Ops#`<init>`().
       |[3:15..3:16): a <= b/Main.Ops#`<init>`().(a)
       |[3:15..3:16): a <= b/Main.Ops#a.
       |[3:18..3:19): A => b/Main.Ops#[A]
       |[3:27..3:31): parT <= b/Main.Ops#parT().
       |[3:33..3:39): Option => scala/Option#
       |[3:40..3:43): Int => scala/Int#
       |[3:47..3:51): None => scala/None.
       |[4:15..4:20): toPar <= b/Main.toPar().
       |[4:21..4:22): A <= b/Main.toPar().[A]
       |[4:24..4:25): a <= b/Main.toPar().(a)
       |[4:27..4:28): A => b/Main.toPar().[A]
       |[4:31..4:34): Ops => b/Main.Ops#
       |[4:35..4:36): A => b/Main.toPar().[A]
       |[4:44..4:47): Ops => b/Main.Ops#
       |[4:47..4:47):  => b/Main.Ops#`<init>`().
       |[4:48..4:49): a => b/Main.toPar().(a)
       |[5:6..5:12): logins <= b/Main.logins().
       |[5:16..5:19): Int => scala/Int#
       |[6:6..6:18): transactions <= b/Main.transactions().
       |[6:22..6:28): Option => scala/Option#
       |[6:29..6:32): Int => scala/Int#
       |[8:6..8:12): logins <= local0
       |[8:16..8:22): Option => scala/Option.
       |[8:23..8:29): logins => b/Main.logins().
       |[9:6..9:7): t <= local1
       |[9:11..9:17): logins => local0
       |[9:18..9:22): parT => b/Main.Ops#parT().
       |[10:12..10:13): t => local1
       |""".stripMargin,
  )

}

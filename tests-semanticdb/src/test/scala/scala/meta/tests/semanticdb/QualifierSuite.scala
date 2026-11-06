package scala.meta.tests.semanticdb

class QualifierSuite extends SemanticdbSuite {

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  occurrences(
    "a package path",
    """|package qs1a.qs1b { class C; object o }
       |object qs1 {
       |  val a = classOf[qs1a.qs1b.C]
       |  val b = List[qs1a.qs1b.C]()
       |  val c = classOf[List[X] forSome { type X <: qs1a.qs1b.C }]
       |  val d: List[X] forSome { type X <: qs1a.qs1b.C } = null
       |  val e: PartialFunction[qs1a.qs1b.C, Int] = (x: qs1a.qs1b.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: qs1a.qs1b.C }]()
       |  val g = List[qs1a.qs1b.o.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:12): qs1a => qs1a/
         |[0:13..0:17): qs1b => qs1a/qs1b/
         |[0:26..0:27): C <= qs1a/qs1b/C#
         |[0:27..0:27):  <= qs1a/qs1b/C#`<init>`().
         |[0:36..0:37): o <= qs1a/qs1b/o.
         |[1:7..1:10): qs1 <= _empty_/qs1.
         |[2:6..2:7): a <= _empty_/qs1.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:22): qs1a => qs1a/
         |[2:23..2:27): qs1b => qs1a/qs1b/
         |[2:28..2:29): C => qs1a/qs1b/C#
         |[3:6..3:7): b <= _empty_/qs1.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:19): qs1a => qs1a/
         |[3:20..3:24): qs1b => qs1a/qs1b/
         |[3:25..3:26): C => qs1a/qs1b/C#
         |[4:6..4:7): c <= _empty_/qs1.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:50): qs1a => qs1a/
         |[4:51..4:55): qs1b => qs1a/qs1b/
         |[4:56..4:57): C => qs1a/qs1b/C#
         |[5:6..5:7): d <= _empty_/qs1.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:41): qs1a => qs1a/
         |[5:42..5:46): qs1b => qs1a/qs1b/
         |[5:47..5:48): C => qs1a/qs1b/C#
         |[6:6..6:7): e <= _empty_/qs1.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): qs1a => qs1a/
         |[6:30..6:34): qs1b => qs1a/qs1b/
         |[6:35..6:36): C => qs1a/qs1b/C#
         |[6:38..6:41): Int => scala/Int#
         |[6:45..6:88): (x: qs1a.qs1b.C) => x match { case _ => 1 } <= local2
         |[6:46..6:47): x <= local9
         |[6:49..6:53): qs1a => qs1a/
         |[6:54..6:58): qs1b => qs1a/qs1b/
         |[6:59..6:60): C => qs1a/qs1b/C#
         |[6:65..6:66): x => local9
         |[7:6..7:7): f <= _empty_/qs1.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:35): qs1a => qs1a/
         |[7:36..7:40): qs1b => qs1a/qs1b/
         |[7:41..7:42): C => qs1a/qs1b/C#
         |[8:6..8:7): g <= _empty_/qs1.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:19): qs1a => qs1a/
         |[8:20..8:24): qs1b => qs1a/qs1b/
         |[8:25..8:26): o => qs1a/qs1b/o.
         |""".stripMargin
    else
      """|[0:8..0:12): qs1a => qs1a/
         |[0:13..0:17): qs1b => qs1a/qs1b/
         |[0:26..0:27): C <= qs1a/qs1b/C#
         |[0:27..0:27):  <= qs1a/qs1b/C#`<init>`().
         |[0:36..0:37): o <= qs1a/qs1b/o.
         |[1:7..1:10): qs1 <= _empty_/qs1.
         |[2:6..2:7): a <= _empty_/qs1.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:22): qs1a => qs1a/
         |[2:23..2:27): qs1b => qs1a/qs1b/
         |[2:28..2:29): C => qs1a/qs1b/C#
         |[3:6..3:7): b <= _empty_/qs1.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:19): qs1a => qs1a/
         |[3:20..3:24): qs1b => qs1a/qs1b/
         |[3:25..3:26): C => qs1a/qs1b/C#
         |[4:6..4:7): c <= _empty_/qs1.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:50): qs1a => qs1a/
         |[4:51..4:55): qs1b => qs1a/qs1b/
         |[4:56..4:57): C => qs1a/qs1b/C#
         |[5:6..5:7): d <= _empty_/qs1.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:41): qs1a => qs1a/
         |[5:42..5:46): qs1b => qs1a/qs1b/
         |[5:47..5:48): C => qs1a/qs1b/C#
         |[6:6..6:7): e <= _empty_/qs1.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): qs1a => qs1a/
         |[6:30..6:34): qs1b => qs1a/qs1b/
         |[6:35..6:36): C => qs1a/qs1b/C#
         |[6:38..6:41): Int => scala/Int#
         |[6:45..6:88): (x: qs1a.qs1b.C) => x match { case _ => 1 } <= local2
         |[6:46..6:47): x <= local9
         |[6:65..6:66): x => local9
         |[7:6..7:7): f <= _empty_/qs1.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:35): qs1a => qs1a/
         |[7:36..7:40): qs1b => qs1a/qs1b/
         |[7:41..7:42): C => qs1a/qs1b/C#
         |[8:6..8:7): g <= _empty_/qs1.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:19): qs1a => qs1a/
         |[8:20..8:24): qs1b => qs1a/qs1b/
         |[8:25..8:26): o => qs1a/qs1b/o.
         |""".stripMargin,
  )

  occurrences(
    "a package path through _root_",
    """|package qs2a.qs2b { class C; object o }
       |object qs2 {
       |  val a = classOf[_root_.qs2a.qs2b.C]
       |  val b = List[_root_.qs2a.qs2b.C]()
       |  val c = classOf[List[X] forSome { type X <: _root_.qs2a.qs2b.C }]
       |  val d: List[X] forSome { type X <: _root_.qs2a.qs2b.C } = null
       |  val e: PartialFunction[_root_.qs2a.qs2b.C, Int] = (x: _root_.qs2a.qs2b.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: _root_.qs2a.qs2b.C }]()
       |  val g = List[_root_.qs2a.qs2b.o.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:12): qs2a => qs2a/
         |[0:13..0:17): qs2b => qs2a/qs2b/
         |[0:26..0:27): C <= qs2a/qs2b/C#
         |[0:27..0:27):  <= qs2a/qs2b/C#`<init>`().
         |[0:36..0:37): o <= qs2a/qs2b/o.
         |[1:7..1:10): qs2 <= _empty_/qs2.
         |[2:6..2:7): a <= _empty_/qs2.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:24): _root_ => _root_/
         |[2:25..2:29): qs2a => qs2a/
         |[2:30..2:34): qs2b => qs2a/qs2b/
         |[2:35..2:36): C => qs2a/qs2b/C#
         |[3:6..3:7): b <= _empty_/qs2.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:21): _root_ => _root_/
         |[3:22..3:26): qs2a => qs2a/
         |[3:27..3:31): qs2b => qs2a/qs2b/
         |[3:32..3:33): C => qs2a/qs2b/C#
         |[4:6..4:7): c <= _empty_/qs2.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:52): _root_ => _root_/
         |[4:53..4:57): qs2a => qs2a/
         |[4:58..4:62): qs2b => qs2a/qs2b/
         |[4:63..4:64): C => qs2a/qs2b/C#
         |[5:6..5:7): d <= _empty_/qs2.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:43): _root_ => _root_/
         |[5:44..5:48): qs2a => qs2a/
         |[5:49..5:53): qs2b => qs2a/qs2b/
         |[5:54..5:55): C => qs2a/qs2b/C#
         |[6:6..6:7): e <= _empty_/qs2.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:32..6:36): qs2a => qs2a/
         |[6:37..6:41): qs2b => qs2a/qs2b/
         |[6:42..6:43): C => qs2a/qs2b/C#
         |[6:45..6:48): Int => scala/Int#
         |[6:52..6:102): (x: _root_.qs2a.qs2b.C) => x match { case _ => 1 } <= local2
         |[6:53..6:54): x <= local9
         |[6:56..6:62): _root_ => _root_/
         |[6:63..6:67): qs2a => qs2a/
         |[6:68..6:72): qs2b => qs2a/qs2b/
         |[6:73..6:74): C => qs2a/qs2b/C#
         |[6:79..6:80): x => local9
         |[7:6..7:7): f <= _empty_/qs2.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:37): _root_ => _root_/
         |[7:38..7:42): qs2a => qs2a/
         |[7:43..7:47): qs2b => qs2a/qs2b/
         |[7:48..7:49): C => qs2a/qs2b/C#
         |[8:6..8:7): g <= _empty_/qs2.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): _root_ => _root_/
         |[8:22..8:26): qs2a => qs2a/
         |[8:27..8:31): qs2b => qs2a/qs2b/
         |[8:32..8:33): o => qs2a/qs2b/o.
         |""".stripMargin
    else
      """|[0:8..0:12): qs2a => qs2a/
         |[0:13..0:17): qs2b => qs2a/qs2b/
         |[0:26..0:27): C <= qs2a/qs2b/C#
         |[0:27..0:27):  <= qs2a/qs2b/C#`<init>`().
         |[0:36..0:37): o <= qs2a/qs2b/o.
         |[1:7..1:10): qs2 <= _empty_/qs2.
         |[2:6..2:7): a <= _empty_/qs2.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:24): _root_ => _root_/
         |[2:25..2:29): qs2a => qs2a/
         |[2:30..2:34): qs2b => qs2a/qs2b/
         |[2:35..2:36): C => qs2a/qs2b/C#
         |[3:6..3:7): b <= _empty_/qs2.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:21): _root_ => _root_/
         |[3:22..3:26): qs2a => qs2a/
         |[3:27..3:31): qs2b => qs2a/qs2b/
         |[3:32..3:33): C => qs2a/qs2b/C#
         |[4:6..4:7): c <= _empty_/qs2.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:52): _root_ => _root_/
         |[4:53..4:57): qs2a => qs2a/
         |[4:58..4:62): qs2b => qs2a/qs2b/
         |[4:63..4:64): C => qs2a/qs2b/C#
         |[5:6..5:7): d <= _empty_/qs2.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:43): _root_ => _root_/
         |[5:44..5:48): qs2a => qs2a/
         |[5:49..5:53): qs2b => qs2a/qs2b/
         |[5:54..5:55): C => qs2a/qs2b/C#
         |[6:6..6:7): e <= _empty_/qs2.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:32..6:36): qs2a => qs2a/
         |[6:37..6:41): qs2b => qs2a/qs2b/
         |[6:42..6:43): C => qs2a/qs2b/C#
         |[6:45..6:48): Int => scala/Int#
         |[6:52..6:102): (x: _root_.qs2a.qs2b.C) => x match { case _ => 1 } <= local2
         |[6:53..6:54): x <= local9
         |[6:79..6:80): x => local9
         |[7:6..7:7): f <= _empty_/qs2.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:37): _root_ => _root_/
         |[7:38..7:42): qs2a => qs2a/
         |[7:43..7:47): qs2b => qs2a/qs2b/
         |[7:48..7:49): C => qs2a/qs2b/C#
         |[8:6..8:7): g <= _empty_/qs2.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): _root_ => _root_/
         |[8:22..8:26): qs2a => qs2a/
         |[8:27..8:31): qs2b => qs2a/qs2b/
         |[8:32..8:33): o => qs2a/qs2b/o.
         |""".stripMargin,
  )

  occurrences(
    "a member of a package object",
    """|package object qs3p { class C; object o }
       |object qs3 {
       |  val a = classOf[qs3p.C]
       |  val b = List[qs3p.C]()
       |  val c = classOf[List[X] forSome { type X <: qs3p.C }]
       |  val d: List[X] forSome { type X <: qs3p.C } = null
       |  val e: PartialFunction[qs3p.C, Int] = (x: qs3p.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: qs3p.C }]()
       |  val g = List[qs3p.o.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:15..0:19): qs3p <= qs3p/package.
         |[0:28..0:29): C <= qs3p/package.C#
         |[0:29..0:29):  <= qs3p/package.C#`<init>`().
         |[0:38..0:39): o <= qs3p/package.o.
         |[1:7..1:10): qs3 <= _empty_/qs3.
         |[2:6..2:7): a <= _empty_/qs3.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:22): qs3p => qs3p/
         |[2:23..2:24): C => qs3p/package.C#
         |[3:6..3:7): b <= _empty_/qs3.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:19): qs3p => qs3p/
         |[3:20..3:21): C => qs3p/package.C#
         |[4:6..4:7): c <= _empty_/qs3.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:50): qs3p => qs3p/
         |[4:51..4:52): C => qs3p/package.C#
         |[5:6..5:7): d <= _empty_/qs3.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:41): qs3p => qs3p/
         |[5:42..5:43): C => qs3p/package.C#
         |[6:6..6:7): e <= _empty_/qs3.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): qs3p => qs3p/
         |[6:30..6:31): C => qs3p/package.C#
         |[6:33..6:36): Int => scala/Int#
         |[6:40..6:78): (x: qs3p.C) => x match { case _ => 1 } <= local2
         |[6:41..6:42): x <= local9
         |[6:44..6:48): qs3p => qs3p/
         |[6:49..6:50): C => qs3p/package.C#
         |[6:55..6:56): x => local9
         |[7:6..7:7): f <= _empty_/qs3.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:35): qs3p => qs3p/
         |[7:36..7:37): C => qs3p/package.C#
         |[8:6..8:7): g <= _empty_/qs3.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:19): qs3p => qs3p/
         |[8:20..8:21): o => qs3p/package.o.
         |""".stripMargin
    else
      """|[0:15..0:19): qs3p <= qs3p/package.
         |[0:28..0:29): C <= qs3p/package.C#
         |[0:29..0:29):  <= qs3p/package.C#`<init>`().
         |[0:38..0:39): o <= qs3p/package.o.
         |[1:7..1:10): qs3 <= _empty_/qs3.
         |[2:6..2:7): a <= _empty_/qs3.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:22): qs3p => qs3p/
         |[2:23..2:24): C => qs3p/package.C#
         |[3:6..3:7): b <= _empty_/qs3.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:19): qs3p => qs3p/
         |[3:20..3:21): C => qs3p/package.C#
         |[4:6..4:7): c <= _empty_/qs3.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:50): qs3p => qs3p/
         |[4:51..4:52): C => qs3p/package.C#
         |[5:6..5:7): d <= _empty_/qs3.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:41): qs3p => qs3p/
         |[5:42..5:43): C => qs3p/package.C#
         |[6:6..6:7): e <= _empty_/qs3.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): qs3p => qs3p/
         |[6:30..6:31): C => qs3p/package.C#
         |[6:33..6:36): Int => scala/Int#
         |[6:40..6:78): (x: qs3p.C) => x match { case _ => 1 } <= local2
         |[6:41..6:42): x <= local9
         |[6:55..6:56): x => local9
         |[7:6..7:7): f <= _empty_/qs3.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:35): qs3p => qs3p/
         |[7:36..7:37): C => qs3p/package.C#
         |[8:6..8:7): g <= _empty_/qs3.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:19): qs3p => qs3p/
         |[8:20..8:21): o => qs3p/package.o.
         |""".stripMargin,
  )

  occurrences(
    "a member of the package object of a nested package",
    """|package qs4a { package object qs4b { class C; object o } }
       |object qs4 {
       |  val a = classOf[qs4a.qs4b.C]
       |  val b = List[qs4a.qs4b.C]()
       |  val c = classOf[List[X] forSome { type X <: qs4a.qs4b.C }]
       |  val d: List[X] forSome { type X <: qs4a.qs4b.C } = null
       |  val e: PartialFunction[qs4a.qs4b.C, Int] = (x: qs4a.qs4b.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: qs4a.qs4b.C }]()
       |  val g = List[qs4a.qs4b.o.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:12): qs4a <= qs4a/
         |[0:30..0:34): qs4b <= qs4a/qs4b/package.
         |[0:43..0:44): C <= qs4a/qs4b/package.C#
         |[0:44..0:44):  <= qs4a/qs4b/package.C#`<init>`().
         |[0:53..0:54): o <= qs4a/qs4b/package.o.
         |[1:7..1:10): qs4 <= _empty_/qs4.
         |[2:6..2:7): a <= _empty_/qs4.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:22): qs4a => qs4a/
         |[2:23..2:27): qs4b => qs4a/qs4b/
         |[2:28..2:29): C => qs4a/qs4b/package.C#
         |[3:6..3:7): b <= _empty_/qs4.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:19): qs4a => qs4a/
         |[3:20..3:24): qs4b => qs4a/qs4b/
         |[3:25..3:26): C => qs4a/qs4b/package.C#
         |[4:6..4:7): c <= _empty_/qs4.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:50): qs4a => qs4a/
         |[4:51..4:55): qs4b => qs4a/qs4b/
         |[4:56..4:57): C => qs4a/qs4b/package.C#
         |[5:6..5:7): d <= _empty_/qs4.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:41): qs4a => qs4a/
         |[5:42..5:46): qs4b => qs4a/qs4b/
         |[5:47..5:48): C => qs4a/qs4b/package.C#
         |[6:6..6:7): e <= _empty_/qs4.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): qs4a => qs4a/
         |[6:30..6:34): qs4b => qs4a/qs4b/
         |[6:35..6:36): C => qs4a/qs4b/package.C#
         |[6:38..6:41): Int => scala/Int#
         |[6:45..6:88): (x: qs4a.qs4b.C) => x match { case _ => 1 } <= local2
         |[6:46..6:47): x <= local9
         |[6:49..6:53): qs4a => qs4a/
         |[6:54..6:58): qs4b => qs4a/qs4b/
         |[6:59..6:60): C => qs4a/qs4b/package.C#
         |[6:65..6:66): x => local9
         |[7:6..7:7): f <= _empty_/qs4.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:35): qs4a => qs4a/
         |[7:36..7:40): qs4b => qs4a/qs4b/
         |[7:41..7:42): C => qs4a/qs4b/package.C#
         |[8:6..8:7): g <= _empty_/qs4.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:19): qs4a => qs4a/
         |[8:20..8:24): qs4b => qs4a/qs4b/
         |[8:25..8:26): o => qs4a/qs4b/package.o.
         |""".stripMargin
    else
      """|[0:8..0:12): qs4a <= qs4a/
         |[0:30..0:34): qs4b <= qs4a/qs4b/package.
         |[0:43..0:44): C <= qs4a/qs4b/package.C#
         |[0:44..0:44):  <= qs4a/qs4b/package.C#`<init>`().
         |[0:53..0:54): o <= qs4a/qs4b/package.o.
         |[1:7..1:10): qs4 <= _empty_/qs4.
         |[2:6..2:7): a <= _empty_/qs4.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:22): qs4a => qs4a/
         |[2:23..2:27): qs4b => qs4a/qs4b/
         |[2:28..2:29): C => qs4a/qs4b/package.C#
         |[3:6..3:7): b <= _empty_/qs4.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:19): qs4a => qs4a/
         |[3:20..3:24): qs4b => qs4a/qs4b/
         |[3:25..3:26): C => qs4a/qs4b/package.C#
         |[4:6..4:7): c <= _empty_/qs4.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:50): qs4a => qs4a/
         |[4:51..4:55): qs4b => qs4a/qs4b/
         |[4:56..4:57): C => qs4a/qs4b/package.C#
         |[5:6..5:7): d <= _empty_/qs4.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:41): qs4a => qs4a/
         |[5:42..5:46): qs4b => qs4a/qs4b/
         |[5:47..5:48): C => qs4a/qs4b/package.C#
         |[6:6..6:7): e <= _empty_/qs4.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): qs4a => qs4a/
         |[6:30..6:34): qs4b => qs4a/qs4b/
         |[6:35..6:36): C => qs4a/qs4b/package.C#
         |[6:38..6:41): Int => scala/Int#
         |[6:45..6:88): (x: qs4a.qs4b.C) => x match { case _ => 1 } <= local2
         |[6:46..6:47): x <= local9
         |[6:65..6:66): x => local9
         |[7:6..7:7): f <= _empty_/qs4.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:35): qs4a => qs4a/
         |[7:36..7:40): qs4b => qs4a/qs4b/
         |[7:41..7:42): C => qs4a/qs4b/package.C#
         |[8:6..8:7): g <= _empty_/qs4.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:19): qs4a => qs4a/
         |[8:20..8:24): qs4b => qs4a/qs4b/
         |[8:25..8:26): o => qs4a/qs4b/package.o.
         |""".stripMargin,
  )

  occurrences(
    "a member of a package object through _root_",
    """|package object qs5p { class C; object o }
       |object qs5 {
       |  val a = classOf[_root_.qs5p.C]
       |  val b = List[_root_.qs5p.C]()
       |  val c = classOf[List[X] forSome { type X <: _root_.qs5p.C }]
       |  val d: List[X] forSome { type X <: _root_.qs5p.C } = null
       |  val e: PartialFunction[_root_.qs5p.C, Int] = (x: _root_.qs5p.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: _root_.qs5p.C }]()
       |  val g = List[_root_.qs5p.o.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:15..0:19): qs5p <= qs5p/package.
         |[0:28..0:29): C <= qs5p/package.C#
         |[0:29..0:29):  <= qs5p/package.C#`<init>`().
         |[0:38..0:39): o <= qs5p/package.o.
         |[1:7..1:10): qs5 <= _empty_/qs5.
         |[2:6..2:7): a <= _empty_/qs5.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:24): _root_ => _root_/
         |[2:25..2:29): qs5p => qs5p/
         |[2:30..2:31): C => qs5p/package.C#
         |[3:6..3:7): b <= _empty_/qs5.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:21): _root_ => _root_/
         |[3:22..3:26): qs5p => qs5p/
         |[3:27..3:28): C => qs5p/package.C#
         |[4:6..4:7): c <= _empty_/qs5.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:52): _root_ => _root_/
         |[4:53..4:57): qs5p => qs5p/
         |[4:58..4:59): C => qs5p/package.C#
         |[5:6..5:7): d <= _empty_/qs5.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:43): _root_ => _root_/
         |[5:44..5:48): qs5p => qs5p/
         |[5:49..5:50): C => qs5p/package.C#
         |[6:6..6:7): e <= _empty_/qs5.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:32..6:36): qs5p => qs5p/
         |[6:37..6:38): C => qs5p/package.C#
         |[6:40..6:43): Int => scala/Int#
         |[6:47..6:92): (x: _root_.qs5p.C) => x match { case _ => 1 } <= local2
         |[6:48..6:49): x <= local9
         |[6:51..6:57): _root_ => _root_/
         |[6:58..6:62): qs5p => qs5p/
         |[6:63..6:64): C => qs5p/package.C#
         |[6:69..6:70): x => local9
         |[7:6..7:7): f <= _empty_/qs5.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:37): _root_ => _root_/
         |[7:38..7:42): qs5p => qs5p/
         |[7:43..7:44): C => qs5p/package.C#
         |[8:6..8:7): g <= _empty_/qs5.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): _root_ => _root_/
         |[8:22..8:26): qs5p => qs5p/
         |[8:27..8:28): o => qs5p/package.o.
         |""".stripMargin
    else
      """|[0:15..0:19): qs5p <= qs5p/package.
         |[0:28..0:29): C <= qs5p/package.C#
         |[0:29..0:29):  <= qs5p/package.C#`<init>`().
         |[0:38..0:39): o <= qs5p/package.o.
         |[1:7..1:10): qs5 <= _empty_/qs5.
         |[2:6..2:7): a <= _empty_/qs5.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:24): _root_ => _root_/
         |[2:25..2:29): qs5p => qs5p/
         |[2:30..2:31): C => qs5p/package.C#
         |[3:6..3:7): b <= _empty_/qs5.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:21): _root_ => _root_/
         |[3:22..3:26): qs5p => qs5p/
         |[3:27..3:28): C => qs5p/package.C#
         |[4:6..4:7): c <= _empty_/qs5.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:52): _root_ => _root_/
         |[4:53..4:57): qs5p => qs5p/
         |[4:58..4:59): C => qs5p/package.C#
         |[5:6..5:7): d <= _empty_/qs5.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:43): _root_ => _root_/
         |[5:44..5:48): qs5p => qs5p/
         |[5:49..5:50): C => qs5p/package.C#
         |[6:6..6:7): e <= _empty_/qs5.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:32..6:36): qs5p => qs5p/
         |[6:37..6:38): C => qs5p/package.C#
         |[6:40..6:43): Int => scala/Int#
         |[6:47..6:92): (x: _root_.qs5p.C) => x match { case _ => 1 } <= local2
         |[6:48..6:49): x <= local9
         |[6:69..6:70): x => local9
         |[7:6..7:7): f <= _empty_/qs5.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:37): _root_ => _root_/
         |[7:38..7:42): qs5p => qs5p/
         |[7:43..7:44): C => qs5p/package.C#
         |[8:6..8:7): g <= _empty_/qs5.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): _root_ => _root_/
         |[8:22..8:26): qs5p => qs5p/
         |[8:27..8:28): o => qs5p/package.o.
         |""".stripMargin,
  )

  occurrences(
    "a member of a package object inside the package object and its package",
    """|package object qs34p {
       |  class C
       |  val a1 = classOf[qs34p.C]
       |  val b1 = List[qs34p.C]()
       |  val c1 = classOf[List[X] forSome { type X <: qs34p.C }]
       |  val e1: PartialFunction[qs34p.C, Int] = (x: qs34p.C) => x match { case _ => 1 }
       |  val a2 = classOf[qs34p.`package`.C]
       |  val b2 = List[qs34p.`package`.C]()
       |  val c2 = classOf[List[X] forSome { type X <: qs34p.`package`.C }]
       |  val e2: PartialFunction[qs34p.`package`.C, Int] = (x: qs34p.`package`.C) => x match { case _ => 1 }
       |  object i {
       |    val a = classOf[qs34p.C]
       |    val b = List[qs34p.C]()
       |  }
       |}
       |package qs34p {
       |  object j {
       |    val a = classOf[qs34p.C]
       |    val b = List[qs34p.C]()
       |    val c = classOf[List[X] forSome { type X <: qs34p.C }]
       |    val e: PartialFunction[qs34p.C, Int] = (x: qs34p.C) => x match { case _ => 1 }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:15..0:20): qs34p <= qs34p/package.
         |[1:8..1:9): C <= qs34p/package.C#
         |[1:9..1:9):  <= qs34p/package.C#`<init>`().
         |[2:6..2:8): a1 <= qs34p/package.a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:19..2:24): qs34p => qs34p/
         |[2:25..2:26): C => qs34p/package.C#
         |[3:6..3:8): b1 <= qs34p/package.b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:16..3:21): qs34p => qs34p/
         |[3:22..3:23): C => qs34p/package.C#
         |[4:6..4:8): c1 <= qs34p/package.c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:47..4:52): qs34p => qs34p/
         |[4:53..4:54): C => qs34p/package.C#
         |[5:6..5:8): e1 <= qs34p/package.e1.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:31): qs34p => qs34p/
         |[5:32..5:33): C => qs34p/package.C#
         |[5:35..5:38): Int => scala/Int#
         |[5:42..5:81): (x: qs34p.C) => x match { case _ => 1 } <= local1
         |[5:43..5:44): x <= local8
         |[5:46..5:51): qs34p => qs34p/
         |[5:52..5:53): C => qs34p/package.C#
         |[5:58..5:59): x => local8
         |[6:6..6:8): a2 <= qs34p/package.a2.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:19..6:24): qs34p => qs34p/
         |[6:25..6:34): `package` => qs34p/package.
         |[6:35..6:36): C => qs34p/package.C#
         |[7:6..7:8): b2 <= qs34p/package.b2.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:21): qs34p => qs34p/
         |[7:22..7:31): `package` => qs34p/package.
         |[7:32..7:33): C => qs34p/package.C#
         |[8:6..8:8): c2 <= qs34p/package.c2.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:19..8:23): List => scala/package.List#
         |[8:24..8:25): X => local9
         |[8:42..8:43): X <= local9
         |[8:47..8:52): qs34p => qs34p/
         |[8:53..8:62): `package` => qs34p/package.
         |[8:63..8:64): C => qs34p/package.C#
         |[9:6..9:8): e2 <= qs34p/package.e2.
         |[9:10..9:25): PartialFunction => scala/PartialFunction#
         |[9:26..9:31): qs34p => qs34p/
         |[9:32..9:41): `package` => qs34p/package.
         |[9:42..9:43): C => qs34p/package.C#
         |[9:45..9:48): Int => scala/Int#
         |[9:52..9:101): (x: qs34p.`package`.C) => x match { case _ => 1 } <= local10
         |[9:53..9:54): x <= local17
         |[9:56..9:61): qs34p => qs34p/
         |[9:62..9:71): `package` => qs34p/package.
         |[9:72..9:73): C => qs34p/package.C#
         |[9:78..9:79): x => local17
         |[10:9..10:10): i <= qs34p/package.i.
         |[11:8..11:9): a <= qs34p/package.i.a.
         |[11:12..11:19): classOf => scala/Predef.classOf().
         |[11:20..11:25): qs34p => qs34p/
         |[11:26..11:27): C => qs34p/package.C#
         |[12:8..12:9): b <= qs34p/package.i.b.
         |[12:12..12:16): List => scala/collection/immutable/Nil.
         |[12:17..12:22): qs34p => qs34p/
         |[12:23..12:24): C => qs34p/package.C#
         |[15:8..15:13): qs34p <= qs34p/
         |[16:9..16:10): j <= qs34p/j.
         |[17:8..17:9): a <= qs34p/j.a.
         |[17:12..17:19): classOf => scala/Predef.classOf().
         |[17:20..17:25): qs34p => qs34p/
         |[17:26..17:27): C => qs34p/package.C#
         |[18:8..18:9): b <= qs34p/j.b.
         |[18:12..18:16): List => scala/collection/immutable/Nil.
         |[18:17..18:22): qs34p => qs34p/
         |[18:23..18:24): C => qs34p/package.C#
         |[19:8..19:9): c <= qs34p/j.c.
         |[19:12..19:19): classOf => scala/Predef.classOf().
         |[19:20..19:24): List => scala/package.List#
         |[19:25..19:26): X => local18
         |[19:43..19:44): X <= local18
         |[19:48..19:53): qs34p => qs34p/
         |[19:54..19:55): C => qs34p/package.C#
         |[20:8..20:9): e <= qs34p/j.e.
         |[20:11..20:26): PartialFunction => scala/PartialFunction#
         |[20:27..20:32): qs34p => qs34p/
         |[20:33..20:34): C => qs34p/package.C#
         |[20:36..20:39): Int => scala/Int#
         |[20:43..20:82): (x: qs34p.C) => x match { case _ => 1 } <= local19
         |[20:44..20:45): x <= local26
         |[20:47..20:52): qs34p => qs34p/
         |[20:53..20:54): C => qs34p/package.C#
         |[20:59..20:60): x => local26
         |""".stripMargin
    else
      """|[0:15..0:20): qs34p <= qs34p/package.
         |[1:8..1:9): C <= qs34p/package.C#
         |[1:9..1:9):  <= qs34p/package.C#`<init>`().
         |[2:6..2:8): a1 <= qs34p/package.a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:19..2:24): qs34p => qs34p/
         |[2:25..2:26): C => qs34p/package.C#
         |[3:6..3:8): b1 <= qs34p/package.b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:16..3:21): qs34p => qs34p/
         |[3:22..3:23): C => qs34p/package.C#
         |[4:6..4:8): c1 <= qs34p/package.c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:47..4:52): qs34p => qs34p/
         |[4:53..4:54): C => qs34p/package.C#
         |[5:6..5:8): e1 <= qs34p/package.e1.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:31): qs34p => qs34p/
         |[5:32..5:33): C => qs34p/package.C#
         |[5:35..5:38): Int => scala/Int#
         |[5:42..5:81): (x: qs34p.C) => x match { case _ => 1 } <= local1
         |[5:43..5:44): x <= local8
         |[5:58..5:59): x => local8
         |[6:6..6:8): a2 <= qs34p/package.a2.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:19..6:24): qs34p => qs34p/
         |[6:25..6:34): `package` => qs34p/package.
         |[6:35..6:36): C => qs34p/package.C#
         |[7:6..7:8): b2 <= qs34p/package.b2.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:21): qs34p => qs34p/
         |[7:22..7:31): `package` => qs34p/package.
         |[7:32..7:33): C => qs34p/package.C#
         |[8:6..8:8): c2 <= qs34p/package.c2.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:19..8:23): List => scala/package.List#
         |[8:24..8:25): X => local9
         |[8:42..8:43): X <= local9
         |[8:47..8:52): qs34p => qs34p/
         |[8:53..8:62): `package` => qs34p/package.
         |[8:63..8:64): C => qs34p/package.C#
         |[9:6..9:8): e2 <= qs34p/package.e2.
         |[9:10..9:25): PartialFunction => scala/PartialFunction#
         |[9:26..9:31): qs34p => qs34p/
         |[9:32..9:41): `package` => qs34p/package.
         |[9:42..9:43): C => qs34p/package.C#
         |[9:45..9:48): Int => scala/Int#
         |[9:52..9:101): (x: qs34p.`package`.C) => x match { case _ => 1 } <= local10
         |[9:53..9:54): x <= local17
         |[9:78..9:79): x => local17
         |[10:9..10:10): i <= qs34p/package.i.
         |[11:8..11:9): a <= qs34p/package.i.a.
         |[11:12..11:19): classOf => scala/Predef.classOf().
         |[11:20..11:25): qs34p => qs34p/
         |[11:26..11:27): C => qs34p/package.C#
         |[12:8..12:9): b <= qs34p/package.i.b.
         |[12:12..12:16): List => scala/collection/immutable/Nil.
         |[12:17..12:22): qs34p => qs34p/
         |[12:23..12:24): C => qs34p/package.C#
         |[15:8..15:13): qs34p <= qs34p/
         |[16:9..16:10): j <= qs34p/j.
         |[17:8..17:9): a <= qs34p/j.a.
         |[17:12..17:19): classOf => scala/Predef.classOf().
         |[17:20..17:25): qs34p => qs34p/
         |[17:26..17:27): C => qs34p/package.C#
         |[18:8..18:9): b <= qs34p/j.b.
         |[18:12..18:16): List => scala/collection/immutable/Nil.
         |[18:17..18:22): qs34p => qs34p/
         |[18:23..18:24): C => qs34p/package.C#
         |[19:8..19:9): c <= qs34p/j.c.
         |[19:12..19:19): classOf => scala/Predef.classOf().
         |[19:20..19:24): List => scala/package.List#
         |[19:25..19:26): X => local18
         |[19:43..19:44): X <= local18
         |[19:48..19:53): qs34p => qs34p/
         |[19:54..19:55): C => qs34p/package.C#
         |[20:8..20:9): e <= qs34p/j.e.
         |[20:11..20:26): PartialFunction => scala/PartialFunction#
         |[20:27..20:32): qs34p => qs34p/
         |[20:33..20:34): C => qs34p/package.C#
         |[20:36..20:39): Int => scala/Int#
         |[20:43..20:82): (x: qs34p.C) => x match { case _ => 1 } <= local19
         |[20:44..20:45): x <= local26
         |[20:59..20:60): x => local26
         |""".stripMargin,
  )

  occurrences(
    "a type through an object of a package object, inside the package object and its package",
    """|package object qs48p {
       |  object O { class C }
       |  val a1 = classOf[qs48p.O.C]
       |  val b1 = List[qs48p.O.C]()
       |  val c1 = classOf[List[X] forSome { type X <: qs48p.O.C }]
       |  val e1: PartialFunction[qs48p.O.C, Int] = (x: qs48p.O.C) => x match { case _ => 1 }
       |  val a2 = classOf[O.C]
       |  val b2 = List[O.C]()
       |  val c2 = classOf[List[X] forSome { type X <: O.C }]
       |  val e2: PartialFunction[O.C, Int] = (x: O.C) => x match { case _ => 1 }
       |}
       |package qs48p {
       |  object j {
       |    val a1 = classOf[qs48p.O.C]
       |    val b1 = List[qs48p.O.C]()
       |    val a2 = classOf[O.C]
       |    val b2 = List[O.C]()
       |  }
       |}
       |object qs48 {
       |  val a = classOf[qs48p.O.C]
       |  val b = List[qs48p.O.C]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:15..0:20): qs48p <= qs48p/package.
         |[1:9..1:10): O <= qs48p/package.O.
         |[1:19..1:20): C <= qs48p/package.O.C#
         |[1:20..1:20):  <= qs48p/package.O.C#`<init>`().
         |[2:6..2:8): a1 <= qs48p/package.a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:19..2:24): qs48p => qs48p/
         |[2:25..2:26): O => qs48p/package.O.
         |[2:27..2:28): C => qs48p/package.O.C#
         |[3:6..3:8): b1 <= qs48p/package.b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:16..3:21): qs48p => qs48p/
         |[3:22..3:23): O => qs48p/package.O.
         |[3:24..3:25): C => qs48p/package.O.C#
         |[4:6..4:8): c1 <= qs48p/package.c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:47..4:52): qs48p => qs48p/
         |[4:53..4:54): O => qs48p/package.O.
         |[4:55..4:56): C => qs48p/package.O.C#
         |[5:6..5:8): e1 <= qs48p/package.e1.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:31): qs48p => qs48p/
         |[5:32..5:33): O => qs48p/package.O.
         |[5:34..5:35): C => qs48p/package.O.C#
         |[5:37..5:40): Int => scala/Int#
         |[5:44..5:85): (x: qs48p.O.C) => x match { case _ => 1 } <= local1
         |[5:45..5:46): x <= local8
         |[5:48..5:53): qs48p => qs48p/
         |[5:54..5:55): O => qs48p/package.O.
         |[5:56..5:57): C => qs48p/package.O.C#
         |[5:62..5:63): x => local8
         |[6:6..6:8): a2 <= qs48p/package.a2.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:19..6:20): O => qs48p/package.O.
         |[6:21..6:22): C => qs48p/package.O.C#
         |[7:6..7:8): b2 <= qs48p/package.b2.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:17): O => qs48p/package.O.
         |[7:18..7:19): C => qs48p/package.O.C#
         |[8:6..8:8): c2 <= qs48p/package.c2.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:19..8:23): List => scala/package.List#
         |[8:24..8:25): X => local9
         |[8:42..8:43): X <= local9
         |[8:47..8:48): O => qs48p/package.O.
         |[8:49..8:50): C => qs48p/package.O.C#
         |[9:6..9:8): e2 <= qs48p/package.e2.
         |[9:10..9:25): PartialFunction => scala/PartialFunction#
         |[9:26..9:27): O => qs48p/package.O.
         |[9:28..9:29): C => qs48p/package.O.C#
         |[9:31..9:34): Int => scala/Int#
         |[9:38..9:73): (x: O.C) => x match { case _ => 1 } <= local10
         |[9:39..9:40): x <= local17
         |[9:42..9:43): O => qs48p/package.O.
         |[9:44..9:45): C => qs48p/package.O.C#
         |[9:50..9:51): x => local17
         |[11:8..11:13): qs48p <= qs48p/
         |[12:9..12:10): j <= qs48p/j.
         |[13:8..13:10): a1 <= qs48p/j.a1.
         |[13:13..13:20): classOf => scala/Predef.classOf().
         |[13:21..13:26): qs48p => qs48p/
         |[13:27..13:28): O => qs48p/package.O.
         |[13:29..13:30): C => qs48p/package.O.C#
         |[14:8..14:10): b1 <= qs48p/j.b1.
         |[14:13..14:17): List => scala/collection/immutable/Nil.
         |[14:18..14:23): qs48p => qs48p/
         |[14:24..14:25): O => qs48p/package.O.
         |[14:26..14:27): C => qs48p/package.O.C#
         |[15:8..15:10): a2 <= qs48p/j.a2.
         |[15:13..15:20): classOf => scala/Predef.classOf().
         |[15:21..15:22): O => qs48p/package.O.
         |[15:23..15:24): C => qs48p/package.O.C#
         |[16:8..16:10): b2 <= qs48p/j.b2.
         |[16:13..16:17): List => scala/collection/immutable/Nil.
         |[16:18..16:19): O => qs48p/package.O.
         |[16:20..16:21): C => qs48p/package.O.C#
         |[19:7..19:11): qs48 <= _empty_/qs48.
         |[20:6..20:7): a <= _empty_/qs48.a.
         |[20:10..20:17): classOf => scala/Predef.classOf().
         |[20:18..20:23): qs48p => qs48p/
         |[20:24..20:25): O => qs48p/package.O.
         |[20:26..20:27): C => qs48p/package.O.C#
         |[21:6..21:7): b <= _empty_/qs48.b.
         |[21:10..21:14): List => scala/collection/immutable/Nil.
         |[21:15..21:20): qs48p => qs48p/
         |[21:21..21:22): O => qs48p/package.O.
         |[21:23..21:24): C => qs48p/package.O.C#
         |""".stripMargin
    else
      """|[0:15..0:20): qs48p <= qs48p/package.
         |[1:9..1:10): O <= qs48p/package.O.
         |[1:19..1:20): C <= qs48p/package.O.C#
         |[1:20..1:20):  <= qs48p/package.O.C#`<init>`().
         |[2:6..2:8): a1 <= qs48p/package.a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:19..2:24): qs48p => qs48p/
         |[2:25..2:26): O => qs48p/package.O.
         |[2:27..2:28): C => qs48p/package.O.C#
         |[3:6..3:8): b1 <= qs48p/package.b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:16..3:21): qs48p => qs48p/
         |[3:22..3:23): O => qs48p/package.O.
         |[3:24..3:25): C => qs48p/package.O.C#
         |[4:6..4:8): c1 <= qs48p/package.c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:47..4:52): qs48p => qs48p/
         |[4:53..4:54): O => qs48p/package.O.
         |[4:55..4:56): C => qs48p/package.O.C#
         |[5:6..5:8): e1 <= qs48p/package.e1.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:31): qs48p => qs48p/
         |[5:32..5:33): O => qs48p/package.O.
         |[5:34..5:35): C => qs48p/package.O.C#
         |[5:37..5:40): Int => scala/Int#
         |[5:44..5:85): (x: qs48p.O.C) => x match { case _ => 1 } <= local1
         |[5:45..5:46): x <= local8
         |[5:62..5:63): x => local8
         |[6:6..6:8): a2 <= qs48p/package.a2.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:19..6:20): O => qs48p/package.O.
         |[6:21..6:22): C => qs48p/package.O.C#
         |[7:6..7:8): b2 <= qs48p/package.b2.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:17): O => qs48p/package.O.
         |[7:18..7:19): C => qs48p/package.O.C#
         |[8:6..8:8): c2 <= qs48p/package.c2.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:19..8:23): List => scala/package.List#
         |[8:24..8:25): X => local9
         |[8:42..8:43): X <= local9
         |[8:47..8:48): O => qs48p/package.O.
         |[8:49..8:50): C => qs48p/package.O.C#
         |[9:6..9:8): e2 <= qs48p/package.e2.
         |[9:10..9:25): PartialFunction => scala/PartialFunction#
         |[9:26..9:27): O => qs48p/package.O.
         |[9:28..9:29): C => qs48p/package.O.C#
         |[9:31..9:34): Int => scala/Int#
         |[9:38..9:73): (x: O.C) => x match { case _ => 1 } <= local10
         |[9:39..9:40): x <= local17
         |[9:50..9:51): x => local17
         |[11:8..11:13): qs48p <= qs48p/
         |[12:9..12:10): j <= qs48p/j.
         |[13:8..13:10): a1 <= qs48p/j.a1.
         |[13:13..13:20): classOf => scala/Predef.classOf().
         |[13:21..13:26): qs48p => qs48p/
         |[13:27..13:28): O => qs48p/package.O.
         |[13:29..13:30): C => qs48p/package.O.C#
         |[14:8..14:10): b1 <= qs48p/j.b1.
         |[14:13..14:17): List => scala/collection/immutable/Nil.
         |[14:18..14:23): qs48p => qs48p/
         |[14:24..14:25): O => qs48p/package.O.
         |[14:26..14:27): C => qs48p/package.O.C#
         |[15:8..15:10): a2 <= qs48p/j.a2.
         |[15:13..15:20): classOf => scala/Predef.classOf().
         |[15:21..15:22): O => qs48p/package.O.
         |[15:23..15:24): C => qs48p/package.O.C#
         |[16:8..16:10): b2 <= qs48p/j.b2.
         |[16:13..16:17): List => scala/collection/immutable/Nil.
         |[16:18..16:19): O => qs48p/package.O.
         |[16:20..16:21): C => qs48p/package.O.C#
         |[19:7..19:11): qs48 <= _empty_/qs48.
         |[20:6..20:7): a <= _empty_/qs48.a.
         |[20:10..20:17): classOf => scala/Predef.classOf().
         |[20:18..20:23): qs48p => qs48p/
         |[20:24..20:25): O => qs48p/package.O.
         |[20:26..20:27): C => qs48p/package.O.C#
         |[21:6..21:7): b <= _empty_/qs48.b.
         |[21:10..21:14): List => scala/collection/immutable/Nil.
         |[21:15..21:20): qs48p => qs48p/
         |[21:21..21:22): O => qs48p/package.O.
         |[21:23..21:24): C => qs48p/package.O.C#
         |""".stripMargin,
  )

  occurrences(
    "a type through this of a package object",
    """|package object qs78 {
       |  object O { class C; trait Tr; class P[A](a: A); trait TC[A] }
       |  val a = classOf[`package`.this.O.C]
       |  val b = List[`package`.this.O.C]()
       |  val c = classOf[List[X] forSome { type X <: `package`.this.O.C }]
       |  val d: List[X] forSome { type X <: `package`.this.O.C } = null
       |  val e: PartialFunction[`package`.this.O.C, Int] = (x: `package`.this.O.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: `package`.this.O.C }]()
       |  val g = List[`package`.this.type]()
       |  class I { s: `package`.this.O.Tr => }
       |  class J extends `package`.this.O.P(1)
       |  val h = List[AnyRef { def m[A: `package`.this.O.TC]: Int }]()
       |  class K {
       |    val a = classOf[`package`.this.O.C]
       |    val b = List[`package`.this.O.C]()
       |    val c = classOf[List[X] forSome { type X <: `package`.this.O.C }]
       |    val d: List[X] forSome { type X <: `package`.this.O.C } = null
       |    val e: PartialFunction[`package`.this.O.C, Int] = (x: `package`.this.O.C) => x match { case _ => 1 }
       |    val f = List[AnyRef { def m: `package`.this.O.C }]()
       |    val g = List[`package`.this.type]()
       |    class I { s: `package`.this.O.Tr => }
       |    class J extends `package`.this.O.P(1)
       |    val h = List[AnyRef { def m[A: `package`.this.O.TC]: Int }]()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:15..0:19): qs78 <= qs78/package.
         |[1:9..1:10): O <= qs78/package.O.
         |[1:19..1:20): C <= qs78/package.O.C#
         |[1:20..1:20):  <= qs78/package.O.C#`<init>`().
         |[1:28..1:30): Tr <= qs78/package.O.Tr#
         |[1:38..1:39): P <= qs78/package.O.P#
         |[1:40..1:41): A <= qs78/package.O.P#[A]
         |[1:42..1:42):  <= qs78/package.O.P#`<init>`().
         |[1:43..1:44): a <= qs78/package.O.P#`<init>`().(a)
         |[1:43..1:44): a <= qs78/package.O.P#a.
         |[1:46..1:47): A => qs78/package.O.P#[A]
         |[1:56..1:58): TC <= qs78/package.O.TC#
         |[1:59..1:60): A <= qs78/package.O.TC#[A]
         |[2:6..2:7): a <= qs78/package.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:27): `package` => qs78/package.
         |[2:33..2:34): O => qs78/package.O.
         |[2:35..2:36): C => qs78/package.O.C#
         |[3:6..3:7): b <= qs78/package.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:24): `package` => qs78/package.
         |[3:30..3:31): O => qs78/package.O.
         |[3:32..3:33): C => qs78/package.O.C#
         |[4:6..4:7): c <= qs78/package.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:55): `package` => qs78/package.
         |[4:61..4:62): O => qs78/package.O.
         |[4:63..4:64): C => qs78/package.O.C#
         |[5:6..5:7): d <= qs78/package.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:46): `package` => qs78/package.
         |[5:52..5:53): O => qs78/package.O.
         |[5:54..5:55): C => qs78/package.O.C#
         |[6:6..6:7): e <= qs78/package.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:34): `package` => qs78/package.
         |[6:40..6:41): O => qs78/package.O.
         |[6:42..6:43): C => qs78/package.O.C#
         |[6:45..6:48): Int => scala/Int#
         |[6:52..6:102): (x: `package`.this.O.C) => x match { case _ => 1 } <= local2
         |[6:53..6:54): x <= local9
         |[6:56..6:65): `package` => qs78/package.
         |[6:71..6:72): O => qs78/package.O.
         |[6:73..6:74): C => qs78/package.O.C#
         |[6:79..6:80): x => local9
         |[7:6..7:7): f <= qs78/package.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:40): `package` => qs78/package.
         |[7:46..7:47): O => qs78/package.O.
         |[7:48..7:49): C => qs78/package.O.C#
         |[8:6..8:7): g <= qs78/package.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:24): `package` => qs78/package.
         |[9:8..9:9): I <= qs78/package.I#
         |[9:10..9:10):  <= qs78/package.I#`<init>`().
         |[9:12..9:13): s <= local11
         |[9:15..9:24): `package` => qs78/package.
         |[9:30..9:31): O => qs78/package.O.
         |[9:32..9:34): Tr => qs78/package.O.Tr#
         |[10:8..10:9): J <= qs78/package.J#
         |[10:10..10:10):  <= qs78/package.J#`<init>`().
         |[10:33..10:34): O => qs78/package.O.
         |[10:35..10:36): P => qs78/package.O.P#
         |[10:36..10:36):  => qs78/package.O.P#`<init>`().
         |[11:6..11:7): h <= qs78/package.h.
         |[11:10..11:14): List => scala/collection/immutable/Nil.
         |[11:15..11:21): AnyRef => java/lang/Object#
         |[11:28..11:29): m <= local12
         |[11:30..11:31): A <= local14
         |[11:33..11:33):  <= local13
         |[11:33..11:42): `package` => qs78/package.
         |[11:48..11:49): O => qs78/package.O.
         |[11:50..11:52): TC => qs78/package.O.TC#
         |[11:55..11:58): Int => scala/Int#
         |[12:8..12:9): K <= qs78/package.K#
         |[12:10..12:10):  <= qs78/package.K#`<init>`().
         |[13:8..13:9): a <= qs78/package.K#a.
         |[13:12..13:19): classOf => scala/Predef.classOf().
         |[13:20..13:29): `package` => qs78/package.
         |[13:35..13:36): O => qs78/package.O.
         |[13:37..13:38): C => qs78/package.O.C#
         |[14:8..14:9): b <= qs78/package.K#b.
         |[14:12..14:16): List => scala/collection/immutable/Nil.
         |[14:17..14:26): `package` => qs78/package.
         |[14:32..14:33): O => qs78/package.O.
         |[14:34..14:35): C => qs78/package.O.C#
         |[15:8..15:9): c <= qs78/package.K#c.
         |[15:12..15:19): classOf => scala/Predef.classOf().
         |[15:20..15:24): List => scala/package.List#
         |[15:25..15:26): X => local15
         |[15:43..15:44): X <= local15
         |[15:48..15:57): `package` => qs78/package.
         |[15:63..15:64): O => qs78/package.O.
         |[15:65..15:66): C => qs78/package.O.C#
         |[16:8..16:9): d <= qs78/package.K#d.
         |[16:11..16:15): List => scala/package.List#
         |[16:16..16:17): X => local16
         |[16:34..16:35): X <= local16
         |[16:39..16:48): `package` => qs78/package.
         |[16:54..16:55): O => qs78/package.O.
         |[16:56..16:57): C => qs78/package.O.C#
         |[17:8..17:9): e <= qs78/package.K#e.
         |[17:11..17:26): PartialFunction => scala/PartialFunction#
         |[17:27..17:36): `package` => qs78/package.
         |[17:42..17:43): O => qs78/package.O.
         |[17:44..17:45): C => qs78/package.O.C#
         |[17:47..17:50): Int => scala/Int#
         |[17:54..17:104): (x: `package`.this.O.C) => x match { case _ => 1 } <= local17
         |[17:55..17:56): x <= local24
         |[17:58..17:67): `package` => qs78/package.
         |[17:73..17:74): O => qs78/package.O.
         |[17:75..17:76): C => qs78/package.O.C#
         |[17:81..17:82): x => local24
         |[18:8..18:9): f <= qs78/package.K#f.
         |[18:12..18:16): List => scala/collection/immutable/Nil.
         |[18:17..18:23): AnyRef => java/lang/Object#
         |[18:30..18:31): m <= local25
         |[18:33..18:42): `package` => qs78/package.
         |[18:48..18:49): O => qs78/package.O.
         |[18:50..18:51): C => qs78/package.O.C#
         |[19:8..19:9): g <= qs78/package.K#g.
         |[19:12..19:16): List => scala/collection/immutable/Nil.
         |[19:17..19:26): `package` => qs78/package.
         |[20:10..20:11): I <= qs78/package.K#I#
         |[20:12..20:12):  <= qs78/package.K#I#`<init>`().
         |[20:14..20:15): s <= local26
         |[20:17..20:26): `package` => qs78/package.
         |[20:32..20:33): O => qs78/package.O.
         |[20:34..20:36): Tr => qs78/package.O.Tr#
         |[21:10..21:11): J <= qs78/package.K#J#
         |[21:12..21:12):  <= qs78/package.K#J#`<init>`().
         |[21:35..21:36): O => qs78/package.O.
         |[21:37..21:38): P => qs78/package.O.P#
         |[21:38..21:38):  => qs78/package.O.P#`<init>`().
         |[22:8..22:9): h <= qs78/package.K#h.
         |[22:12..22:16): List => scala/collection/immutable/Nil.
         |[22:17..22:23): AnyRef => java/lang/Object#
         |[22:30..22:31): m <= local27
         |[22:32..22:33): A <= local29
         |[22:35..22:35):  <= local28
         |[22:35..22:44): `package` => qs78/package.
         |[22:50..22:51): O => qs78/package.O.
         |[22:52..22:54): TC => qs78/package.O.TC#
         |[22:57..22:60): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:15..0:19): qs78 <= qs78/package.
         |[1:9..1:10): O <= qs78/package.O.
         |[1:19..1:20): C <= qs78/package.O.C#
         |[1:20..1:20):  <= qs78/package.O.C#`<init>`().
         |[1:28..1:30): Tr <= qs78/package.O.Tr#
         |[1:38..1:39): P <= qs78/package.O.P#
         |[1:40..1:41): A <= qs78/package.O.P#[A]
         |[1:42..1:42):  <= qs78/package.O.P#`<init>`().
         |[1:43..1:44): a <= qs78/package.O.P#`<init>`().(a)
         |[1:43..1:44): a <= qs78/package.O.P#a.
         |[1:46..1:47): A => qs78/package.O.P#[A]
         |[1:56..1:58): TC <= qs78/package.O.TC#
         |[1:59..1:60): A <= qs78/package.O.TC#[A]
         |[2:6..2:7): a <= qs78/package.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:27): `package` => qs78/package.
         |[2:33..2:34): O => qs78/package.O.
         |[2:35..2:36): C => qs78/package.O.C#
         |[3:6..3:7): b <= qs78/package.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:24): `package` => qs78/package.
         |[3:30..3:31): O => qs78/package.O.
         |[3:32..3:33): C => qs78/package.O.C#
         |[4:6..4:7): c <= qs78/package.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:55): `package` => qs78/package.
         |[4:61..4:62): O => qs78/package.O.
         |[4:63..4:64): C => qs78/package.O.C#
         |[5:6..5:7): d <= qs78/package.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:46): `package` => qs78/package.
         |[5:52..5:53): O => qs78/package.O.
         |[5:54..5:55): C => qs78/package.O.C#
         |[6:6..6:7): e <= qs78/package.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:34): `package` => qs78/package.
         |[6:40..6:41): O => qs78/package.O.
         |[6:42..6:43): C => qs78/package.O.C#
         |[6:45..6:48): Int => scala/Int#
         |[6:52..6:102): (x: `package`.this.O.C) => x match { case _ => 1 } <= local2
         |[6:53..6:54): x <= local9
         |[6:79..6:80): x => local9
         |[7:6..7:7): f <= qs78/package.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:40): `package` => qs78/package.
         |[7:46..7:47): O => qs78/package.O.
         |[7:48..7:49): C => qs78/package.O.C#
         |[8:6..8:7): g <= qs78/package.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:24): `package` => qs78/package.
         |[9:8..9:9): I <= qs78/package.I#
         |[9:10..9:10):  <= qs78/package.I#`<init>`().
         |[9:12..9:13): s <= local11
         |[9:15..9:24): `package` => qs78/package.
         |[9:30..9:31): O => qs78/package.O.
         |[9:32..9:34): Tr => qs78/package.O.Tr#
         |[10:8..10:9): J <= qs78/package.J#
         |[10:10..10:10):  <= qs78/package.J#`<init>`().
         |[10:33..10:34): O => qs78/package.O.
         |[10:35..10:36): P => qs78/package.O.P#
         |[10:36..10:36):  => qs78/package.O.P#`<init>`().
         |[11:6..11:7): h <= qs78/package.h.
         |[11:10..11:14): List => scala/collection/immutable/Nil.
         |[11:15..11:21): AnyRef => java/lang/Object#
         |[11:28..11:29): m <= local12
         |[11:30..11:31): A <= local14
         |[11:33..11:33):  <= local13
         |[11:33..11:42): `package` => qs78/package.
         |[11:48..11:49): O => qs78/package.O.
         |[11:50..11:52): TC => qs78/package.O.TC#
         |[11:55..11:58): Int => scala/Int#
         |[12:8..12:9): K <= qs78/package.K#
         |[12:10..12:10):  <= qs78/package.K#`<init>`().
         |[13:8..13:9): a <= qs78/package.K#a.
         |[13:12..13:19): classOf => scala/Predef.classOf().
         |[13:20..13:29): `package` => qs78/package.
         |[13:35..13:36): O => qs78/package.O.
         |[13:37..13:38): C => qs78/package.O.C#
         |[14:8..14:9): b <= qs78/package.K#b.
         |[14:12..14:16): List => scala/collection/immutable/Nil.
         |[14:17..14:26): `package` => qs78/package.
         |[14:32..14:33): O => qs78/package.O.
         |[14:34..14:35): C => qs78/package.O.C#
         |[15:8..15:9): c <= qs78/package.K#c.
         |[15:12..15:19): classOf => scala/Predef.classOf().
         |[15:20..15:24): List => scala/package.List#
         |[15:25..15:26): X => local15
         |[15:43..15:44): X <= local15
         |[15:48..15:57): `package` => qs78/package.
         |[15:63..15:64): O => qs78/package.O.
         |[15:65..15:66): C => qs78/package.O.C#
         |[16:8..16:9): d <= qs78/package.K#d.
         |[16:11..16:15): List => scala/package.List#
         |[16:16..16:17): X => local16
         |[16:34..16:35): X <= local16
         |[16:39..16:48): `package` => qs78/package.
         |[16:54..16:55): O => qs78/package.O.
         |[16:56..16:57): C => qs78/package.O.C#
         |[17:8..17:9): e <= qs78/package.K#e.
         |[17:11..17:26): PartialFunction => scala/PartialFunction#
         |[17:27..17:36): `package` => qs78/package.
         |[17:42..17:43): O => qs78/package.O.
         |[17:44..17:45): C => qs78/package.O.C#
         |[17:47..17:50): Int => scala/Int#
         |[17:54..17:104): (x: `package`.this.O.C) => x match { case _ => 1 } <= local17
         |[17:55..17:56): x <= local24
         |[17:81..17:82): x => local24
         |[18:8..18:9): f <= qs78/package.K#f.
         |[18:12..18:16): List => scala/collection/immutable/Nil.
         |[18:17..18:23): AnyRef => java/lang/Object#
         |[18:30..18:31): m <= local25
         |[18:33..18:42): `package` => qs78/package.
         |[18:48..18:49): O => qs78/package.O.
         |[18:50..18:51): C => qs78/package.O.C#
         |[19:8..19:9): g <= qs78/package.K#g.
         |[19:12..19:16): List => scala/collection/immutable/Nil.
         |[19:17..19:26): `package` => qs78/package.
         |[20:10..20:11): I <= qs78/package.K#I#
         |[20:12..20:12):  <= qs78/package.K#I#`<init>`().
         |[20:14..20:15): s <= local26
         |[20:17..20:26): `package` => qs78/package.
         |[20:32..20:33): O => qs78/package.O.
         |[20:34..20:36): Tr => qs78/package.O.Tr#
         |[21:10..21:11): J <= qs78/package.K#J#
         |[21:12..21:12):  <= qs78/package.K#J#`<init>`().
         |[21:35..21:36): O => qs78/package.O.
         |[21:37..21:38): P => qs78/package.O.P#
         |[21:38..21:38):  => qs78/package.O.P#`<init>`().
         |[22:8..22:9): h <= qs78/package.K#h.
         |[22:12..22:16): List => scala/collection/immutable/Nil.
         |[22:17..22:23): AnyRef => java/lang/Object#
         |[22:30..22:31): m <= local27
         |[22:32..22:33): A <= local29
         |[22:35..22:35):  <= local28
         |[22:35..22:44): `package` => qs78/package.
         |[22:50..22:51): O => qs78/package.O.
         |[22:52..22:54): TC => qs78/package.O.TC#
         |[22:57..22:60): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "a type through super of a package object",
    """|trait qs58t { class G; final class F; object O { class C } }
       |package object qs58p extends qs58t {
       |  val a1 = classOf[super.G]
       |  val b1 = List[super.G]()
       |  val c1 = classOf[List[X] forSome { type X <: super.G }]
       |  val e1: PartialFunction[super.G, Int] = (x: super.G) => x match { case _ => 1 }
       |  val a2 = classOf[`package`.super[qs58t].G]
       |  val a3 = classOf[super.O.C]
       |  val a4 = classOf[super.F]
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:11): qs58t <= _empty_/qs58t#
         |[0:12..0:12):  <= _empty_/qs58t#`<init>`().
         |[0:20..0:21): G <= _empty_/qs58t#G#
         |[0:21..0:21):  <= _empty_/qs58t#G#`<init>`().
         |[0:35..0:36): F <= _empty_/qs58t#F#
         |[0:36..0:36):  <= _empty_/qs58t#F#`<init>`().
         |[0:45..0:46): O <= _empty_/qs58t#O.
         |[0:55..0:56): C <= _empty_/qs58t#O.C#
         |[0:56..0:56):  <= _empty_/qs58t#O.C#`<init>`().
         |[1:15..1:20): qs58p <= qs58p/package.
         |[1:29..1:34): qs58t => _empty_/qs58t#
         |[1:34..1:34):  => java/lang/Object#`<init>`().
         |[2:6..2:8): a1 <= qs58p/package.a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:25..2:26): G => _empty_/qs58t#G#
         |[3:6..3:8): b1 <= qs58p/package.b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:22..3:23): G => _empty_/qs58t#G#
         |[4:6..4:8): c1 <= qs58p/package.c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:53..4:54): G => _empty_/qs58t#G#
         |[5:6..5:8): e1 <= qs58p/package.e1.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:26):  => qs58p/package.
         |[5:31..5:31):  => _empty_/qs58t#
         |[5:32..5:33): G => _empty_/qs58t#G#
         |[5:35..5:38): Int => scala/Int#
         |[5:42..5:81): (x: super.G) => x match { case _ => 1 } <= local1
         |[5:43..5:44): x <= local8
         |[5:52..5:53): G => _empty_/qs58t#G#
         |[5:58..5:59): x => local8
         |[6:6..6:8): a2 <= qs58p/package.a2.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:42..6:43): G => _empty_/qs58t#G#
         |[7:6..7:8): a3 <= qs58p/package.a3.
         |[7:11..7:18): classOf => scala/Predef.classOf().
         |[7:25..7:26): O => _empty_/qs58t#O.
         |[7:27..7:28): C => _empty_/qs58t#O.C#
         |[8:6..8:8): a4 <= qs58p/package.a4.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:25..8:26): F => _empty_/qs58t#F#
         |""".stripMargin
    else
      """|[0:6..0:11): qs58t <= _empty_/qs58t#
         |[0:12..0:12):  <= _empty_/qs58t#`<init>`().
         |[0:20..0:21): G <= _empty_/qs58t#G#
         |[0:21..0:21):  <= _empty_/qs58t#G#`<init>`().
         |[0:35..0:36): F <= _empty_/qs58t#F#
         |[0:36..0:36):  <= _empty_/qs58t#F#`<init>`().
         |[0:45..0:46): O <= _empty_/qs58t#O.
         |[0:55..0:56): C <= _empty_/qs58t#O.C#
         |[0:56..0:56):  <= _empty_/qs58t#O.C#`<init>`().
         |[1:15..1:20): qs58p <= qs58p/package.
         |[1:29..1:34): qs58t => _empty_/qs58t#
         |[1:34..1:34):  => java/lang/Object#`<init>`().
         |[2:6..2:8): a1 <= qs58p/package.a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:25..2:26): G => _empty_/qs58t#G#
         |[3:6..3:8): b1 <= qs58p/package.b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:22..3:23): G => _empty_/qs58t#G#
         |[4:6..4:8): c1 <= qs58p/package.c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:53..4:54): G => _empty_/qs58t#G#
         |[5:6..5:8): e1 <= qs58p/package.e1.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:26):  => qs58p/package.
         |[5:31..5:31):  => _empty_/qs58t#
         |[5:32..5:33): G => _empty_/qs58t#G#
         |[5:35..5:38): Int => scala/Int#
         |[5:42..5:81): (x: super.G) => x match { case _ => 1 } <= local1
         |[5:43..5:44): x <= local8
         |[5:58..5:59): x => local8
         |[6:6..6:8): a2 <= qs58p/package.a2.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:42..6:43): G => _empty_/qs58t#G#
         |[7:6..7:8): a3 <= qs58p/package.a3.
         |[7:11..7:18): classOf => scala/Predef.classOf().
         |[7:25..7:26): O => _empty_/qs58t#O.
         |[7:27..7:28): C => _empty_/qs58t#O.C#
         |[8:6..8:8): a4 <= qs58p/package.a4.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:25..8:26): F => _empty_/qs58t#F#
         |""".stripMargin,
  )

  occurrences(
    "aliases in the scala package object",
    """|object qs6 {
       |  val a1 = classOf[scala.List[Int]]
       |  val b1 = List[scala.List[Int]]()
       |  val c1 = classOf[List[X] forSome { type X <: scala.List[Int] }]
       |  val d1: List[X] forSome { type X <: scala.List[Int] } = null
       |  val e1: PartialFunction[scala.List[Int], Int] = (x: scala.List[Int]) => x match { case _ => 1 }
       |  val f1 = List[AnyRef { def m: scala.List[Int] }]()
       |  val a2 = classOf[scala.Seq[Int]]
       |  val b2 = List[scala.Seq[Int]]()
       |  val c2 = classOf[List[X] forSome { type X <: scala.Seq[Int] }]
       |  val d2: List[X] forSome { type X <: scala.Seq[Int] } = null
       |  val e2: PartialFunction[scala.Seq[Int], Int] = (x: scala.Seq[Int]) => x match { case _ => 1 }
       |  val f2 = List[AnyRef { def m: scala.Seq[Int] }]()
       |  val g2 = List[scala.Nil.type]()
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Scala212 ->
          """|[0:7..0:10): qs6 <= _empty_/qs6.
             |[1:6..1:8): a1 <= _empty_/qs6.a1.
             |[1:11..1:18): classOf => scala/Predef.classOf().
             |[1:19..1:24): scala => scala/
             |[1:25..1:29): List => scala/package.List#
             |[1:30..1:33): Int => scala/Int#
             |[2:6..2:8): b1 <= _empty_/qs6.b1.
             |[2:11..2:15): List => scala/collection/immutable/Nil.
             |[2:16..2:21): scala => scala/
             |[2:22..2:26): List => scala/package.List#
             |[2:27..2:30): Int => scala/Int#
             |[3:6..3:8): c1 <= _empty_/qs6.c1.
             |[3:11..3:18): classOf => scala/Predef.classOf().
             |[3:19..3:23): List => scala/package.List#
             |[3:24..3:25): X => local0
             |[3:42..3:43): X <= local0
             |[3:47..3:52): scala => scala/
             |[3:53..3:57): List => scala/package.List#
             |[3:58..3:61): Int => scala/Int#
             |[4:6..4:8): d1 <= _empty_/qs6.d1.
             |[4:10..4:14): List => scala/package.List#
             |[4:15..4:16): X => local1
             |[4:33..4:34): X <= local1
             |[4:38..4:43): scala => scala/
             |[4:44..4:48): List => scala/package.List#
             |[4:49..4:52): Int => scala/Int#
             |[5:6..5:8): e1 <= _empty_/qs6.e1.
             |[5:10..5:25): PartialFunction => scala/PartialFunction#
             |[5:26..5:31): scala => scala/
             |[5:32..5:36): List => scala/package.List#
             |[5:37..5:40): Int => scala/Int#
             |[5:43..5:46): Int => scala/Int#
             |[5:50..5:97): (x: scala.List[Int]) => x match { case _ => 1 } <= local2
             |[5:51..5:52): x <= local9
             |[5:74..5:75): x => local9
             |[6:6..6:8): f1 <= _empty_/qs6.f1.
             |[6:11..6:15): List => scala/collection/immutable/Nil.
             |[6:16..6:22): AnyRef => java/lang/Object#
             |[6:29..6:30): m <= local10
             |[6:38..6:42): List => scala/collection/immutable/List#
             |[6:43..6:46): Int => scala/Int#
             |[7:6..7:8): a2 <= _empty_/qs6.a2.
             |[7:11..7:18): classOf => scala/Predef.classOf().
             |[7:19..7:24): scala => scala/
             |[7:25..7:28): Seq => scala/package.Seq#
             |[7:29..7:32): Int => scala/Int#
             |[8:6..8:8): b2 <= _empty_/qs6.b2.
             |[8:11..8:15): List => scala/collection/immutable/Nil.
             |[8:16..8:21): scala => scala/
             |[8:22..8:25): Seq => scala/package.Seq#
             |[8:26..8:29): Int => scala/Int#
             |[9:6..9:8): c2 <= _empty_/qs6.c2.
             |[9:11..9:18): classOf => scala/Predef.classOf().
             |[9:19..9:23): List => scala/package.List#
             |[9:24..9:25): X => local11
             |[9:42..9:43): X <= local11
             |[9:47..9:52): scala => scala/
             |[9:53..9:56): Seq => scala/package.Seq#
             |[9:57..9:60): Int => scala/Int#
             |[10:6..10:8): d2 <= _empty_/qs6.d2.
             |[10:10..10:14): List => scala/package.List#
             |[10:15..10:16): X => local12
             |[10:33..10:34): X <= local12
             |[10:38..10:43): scala => scala/
             |[10:44..10:47): Seq => scala/package.Seq#
             |[10:48..10:51): Int => scala/Int#
             |[11:6..11:8): e2 <= _empty_/qs6.e2.
             |[11:10..11:25): PartialFunction => scala/PartialFunction#
             |[11:26..11:31): scala => scala/
             |[11:32..11:35): Seq => scala/package.Seq#
             |[11:36..11:39): Int => scala/Int#
             |[11:42..11:45): Int => scala/Int#
             |[11:49..11:95): (x: scala.Seq[Int]) => x match { case _ => 1 } <= local13
             |[11:50..11:51): x <= local20
             |[11:72..11:73): x => local20
             |[12:6..12:8): f2 <= _empty_/qs6.f2.
             |[12:11..12:15): List => scala/collection/immutable/Nil.
             |[12:16..12:22): AnyRef => java/lang/Object#
             |[12:29..12:30): m <= local21
             |[12:38..12:41): Seq => scala/collection/Seq#
             |[12:42..12:45): Int => scala/Int#
             |[13:6..13:8): g2 <= _empty_/qs6.g2.
             |[13:11..13:15): List => scala/collection/immutable/Nil.
             |[13:22..13:25): Nil => scala/collection/immutable/Nil.
             |""".stripMargin,
        ScalaVersion.AtMost213(16) ->
          """|[0:7..0:10): qs6 <= _empty_/qs6.
             |[1:6..1:8): a1 <= _empty_/qs6.a1.
             |[1:11..1:18): classOf => scala/Predef.classOf().
             |[1:19..1:24): scala => scala/
             |[1:25..1:29): List => scala/package.List#
             |[1:30..1:33): Int => scala/Int#
             |[2:6..2:8): b1 <= _empty_/qs6.b1.
             |[2:11..2:15): List => scala/collection/immutable/Nil.
             |[2:16..2:21): scala => scala/
             |[2:22..2:26): List => scala/package.List#
             |[2:27..2:30): Int => scala/Int#
             |[3:6..3:8): c1 <= _empty_/qs6.c1.
             |[3:11..3:18): classOf => scala/Predef.classOf().
             |[3:19..3:23): List => scala/package.List#
             |[3:24..3:25): X => local0
             |[3:42..3:43): X <= local0
             |[3:47..3:52): scala => scala/
             |[3:53..3:57): List => scala/package.List#
             |[3:58..3:61): Int => scala/Int#
             |[4:6..4:8): d1 <= _empty_/qs6.d1.
             |[4:10..4:14): List => scala/package.List#
             |[4:15..4:16): X => local1
             |[4:33..4:34): X <= local1
             |[4:38..4:43): scala => scala/
             |[4:44..4:48): List => scala/package.List#
             |[4:49..4:52): Int => scala/Int#
             |[5:6..5:8): e1 <= _empty_/qs6.e1.
             |[5:10..5:25): PartialFunction => scala/PartialFunction#
             |[5:26..5:31): scala => scala/
             |[5:32..5:36): List => scala/package.List#
             |[5:37..5:40): Int => scala/Int#
             |[5:43..5:46): Int => scala/Int#
             |[5:50..5:97): (x: scala.List[Int]) => x match { case _ => 1 } <= local2
             |[5:51..5:52): x <= local9
             |[5:74..5:75): x => local9
             |[6:6..6:8): f1 <= _empty_/qs6.f1.
             |[6:11..6:15): List => scala/collection/immutable/Nil.
             |[6:16..6:22): AnyRef => java/lang/Object#
             |[6:29..6:30): m <= local10
             |[6:38..6:42): List => scala/collection/immutable/List#
             |[6:43..6:46): Int => scala/Int#
             |[7:6..7:8): a2 <= _empty_/qs6.a2.
             |[7:11..7:18): classOf => scala/Predef.classOf().
             |[7:19..7:24): scala => scala/
             |[7:25..7:28): Seq => scala/package.Seq#
             |[7:29..7:32): Int => scala/Int#
             |[8:6..8:8): b2 <= _empty_/qs6.b2.
             |[8:11..8:15): List => scala/collection/immutable/Nil.
             |[8:16..8:21): scala => scala/
             |[8:22..8:25): Seq => scala/package.Seq#
             |[8:26..8:29): Int => scala/Int#
             |[9:6..9:8): c2 <= _empty_/qs6.c2.
             |[9:11..9:18): classOf => scala/Predef.classOf().
             |[9:19..9:23): List => scala/package.List#
             |[9:24..9:25): X => local11
             |[9:42..9:43): X <= local11
             |[9:47..9:52): scala => scala/
             |[9:53..9:56): Seq => scala/package.Seq#
             |[9:57..9:60): Int => scala/Int#
             |[10:6..10:8): d2 <= _empty_/qs6.d2.
             |[10:10..10:14): List => scala/package.List#
             |[10:15..10:16): X => local12
             |[10:33..10:34): X <= local12
             |[10:38..10:43): scala => scala/
             |[10:44..10:47): Seq => scala/package.Seq#
             |[10:48..10:51): Int => scala/Int#
             |[11:6..11:8): e2 <= _empty_/qs6.e2.
             |[11:10..11:25): PartialFunction => scala/PartialFunction#
             |[11:26..11:31): scala => scala/
             |[11:32..11:35): Seq => scala/package.Seq#
             |[11:36..11:39): Int => scala/Int#
             |[11:42..11:45): Int => scala/Int#
             |[11:49..11:95): (x: scala.Seq[Int]) => x match { case _ => 1 } <= local13
             |[11:50..11:51): x <= local20
             |[11:72..11:73): x => local20
             |[12:6..12:8): f2 <= _empty_/qs6.f2.
             |[12:11..12:15): List => scala/collection/immutable/Nil.
             |[12:16..12:22): AnyRef => java/lang/Object#
             |[12:29..12:30): m <= local21
             |[12:38..12:41): Seq => scala/collection/immutable/Seq#
             |[12:42..12:45): Int => scala/Int#
             |[13:6..13:8): g2 <= _empty_/qs6.g2.
             |[13:11..13:15): List => scala/collection/immutable/Nil.
             |[13:16..13:21): scala => scala/
             |[13:22..13:25): Nil => scala/package.Nil.
             |""".stripMargin,
      ),
      """|[0:7..0:10): qs6 <= _empty_/qs6.
         |[1:6..1:8): a1 <= _empty_/qs6.a1.
         |[1:11..1:18): classOf => scala/Predef.classOf().
         |[1:19..1:24): scala => scala/
         |[1:25..1:29): List => scala/package.List#
         |[1:30..1:33): Int => scala/Int#
         |[2:6..2:8): b1 <= _empty_/qs6.b1.
         |[2:11..2:15): List => scala/collection/immutable/Nil.
         |[2:16..2:21): scala => scala/
         |[2:22..2:26): List => scala/package.List#
         |[2:27..2:30): Int => scala/Int#
         |[3:6..3:8): c1 <= _empty_/qs6.c1.
         |[3:11..3:18): classOf => scala/Predef.classOf().
         |[3:19..3:23): List => scala/package.List#
         |[3:24..3:25): X => local0
         |[3:42..3:43): X <= local0
         |[3:47..3:52): scala => scala/
         |[3:53..3:57): List => scala/package.List#
         |[3:58..3:61): Int => scala/Int#
         |[4:6..4:8): d1 <= _empty_/qs6.d1.
         |[4:10..4:14): List => scala/package.List#
         |[4:15..4:16): X => local1
         |[4:33..4:34): X <= local1
         |[4:38..4:43): scala => scala/
         |[4:44..4:48): List => scala/package.List#
         |[4:49..4:52): Int => scala/Int#
         |[5:6..5:8): e1 <= _empty_/qs6.e1.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:31): scala => scala/
         |[5:32..5:36): List => scala/package.List#
         |[5:37..5:40): Int => scala/Int#
         |[5:43..5:46): Int => scala/Int#
         |[5:50..5:97): (x: scala.List[Int]) => x match { case _ => 1 } <= local2
         |[5:51..5:52): x <= local9
         |[5:54..5:59): scala => scala/
         |[5:60..5:64): List => scala/package.List#
         |[5:65..5:68): Int => scala/Int#
         |[5:74..5:75): x => local9
         |[6:6..6:8): f1 <= _empty_/qs6.f1.
         |[6:11..6:15): List => scala/collection/immutable/Nil.
         |[6:16..6:22): AnyRef => java/lang/Object#
         |[6:29..6:30): m <= local10
         |[6:38..6:42): List => scala/collection/immutable/List#
         |[6:43..6:46): Int => scala/Int#
         |[7:6..7:8): a2 <= _empty_/qs6.a2.
         |[7:11..7:18): classOf => scala/Predef.classOf().
         |[7:19..7:24): scala => scala/
         |[7:25..7:28): Seq => scala/package.Seq#
         |[7:29..7:32): Int => scala/Int#
         |[8:6..8:8): b2 <= _empty_/qs6.b2.
         |[8:11..8:15): List => scala/collection/immutable/Nil.
         |[8:16..8:21): scala => scala/
         |[8:22..8:25): Seq => scala/package.Seq#
         |[8:26..8:29): Int => scala/Int#
         |[9:6..9:8): c2 <= _empty_/qs6.c2.
         |[9:11..9:18): classOf => scala/Predef.classOf().
         |[9:19..9:23): List => scala/package.List#
         |[9:24..9:25): X => local11
         |[9:42..9:43): X <= local11
         |[9:47..9:52): scala => scala/
         |[9:53..9:56): Seq => scala/package.Seq#
         |[9:57..9:60): Int => scala/Int#
         |[10:6..10:8): d2 <= _empty_/qs6.d2.
         |[10:10..10:14): List => scala/package.List#
         |[10:15..10:16): X => local12
         |[10:33..10:34): X <= local12
         |[10:38..10:43): scala => scala/
         |[10:44..10:47): Seq => scala/package.Seq#
         |[10:48..10:51): Int => scala/Int#
         |[11:6..11:8): e2 <= _empty_/qs6.e2.
         |[11:10..11:25): PartialFunction => scala/PartialFunction#
         |[11:26..11:31): scala => scala/
         |[11:32..11:35): Seq => scala/package.Seq#
         |[11:36..11:39): Int => scala/Int#
         |[11:42..11:45): Int => scala/Int#
         |[11:49..11:95): (x: scala.Seq[Int]) => x match { case _ => 1 } <= local13
         |[11:50..11:51): x <= local20
         |[11:53..11:58): scala => scala/
         |[11:59..11:62): Seq => scala/package.Seq#
         |[11:63..11:66): Int => scala/Int#
         |[11:72..11:73): x => local20
         |[12:6..12:8): f2 <= _empty_/qs6.f2.
         |[12:11..12:15): List => scala/collection/immutable/Nil.
         |[12:16..12:22): AnyRef => java/lang/Object#
         |[12:29..12:30): m <= local21
         |[12:38..12:41): Seq => scala/collection/immutable/Seq#
         |[12:42..12:45): Int => scala/Int#
         |[13:6..13:8): g2 <= _empty_/qs6.g2.
         |[13:11..13:15): List => scala/collection/immutable/Nil.
         |[13:16..13:21): scala => scala/
         |[13:22..13:25): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "a type through this of a class",
    """|class qs7 {
       |  class B
       |  val a1 = classOf[qs7.this.B]
       |  val b1 = List[qs7.this.B]()
       |  val c1 = classOf[List[X] forSome { type X <: qs7.this.B }]
       |  val d1: List[X] forSome { type X <: qs7.this.B } = null
       |  val e1: PartialFunction[qs7.this.B, Int] = (x: qs7.this.B) => x match { case _ => 1 }
       |  val f1 = List[AnyRef { def m: qs7.this.B }]()
       |  val g1 = List[qs7.this.type]()
       |  val a2 = classOf[this.B]
       |  val b2 = List[this.B]()
       |  val c2 = classOf[List[X] forSome { type X <: this.B }]
       |  val d2: List[X] forSome { type X <: this.B } = null
       |  val e2: PartialFunction[this.B, Int] = (x: this.B) => x match { case _ => 1 }
       |  val g2 = List[this.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:9): qs7 <= _empty_/qs7#
         |[0:10..0:10):  <= _empty_/qs7#`<init>`().
         |[1:8..1:9): B <= _empty_/qs7#B#
         |[1:9..1:9):  <= _empty_/qs7#B#`<init>`().
         |[2:6..2:8): a1 <= _empty_/qs7#a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:19..2:22): qs7 => _empty_/qs7#
         |[2:28..2:29): B => _empty_/qs7#B#
         |[3:6..3:8): b1 <= _empty_/qs7#b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:16..3:19): qs7 => _empty_/qs7#
         |[3:25..3:26): B => _empty_/qs7#B#
         |[4:6..4:8): c1 <= _empty_/qs7#c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:47..4:50): qs7 => _empty_/qs7#
         |[4:56..4:57): B => _empty_/qs7#B#
         |[5:6..5:8): d1 <= _empty_/qs7#d1.
         |[5:10..5:14): List => scala/package.List#
         |[5:15..5:16): X => local1
         |[5:33..5:34): X <= local1
         |[5:38..5:41): qs7 => _empty_/qs7#
         |[5:47..5:48): B => _empty_/qs7#B#
         |[6:6..6:8): e1 <= _empty_/qs7#e1.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:29): qs7 => _empty_/qs7#
         |[6:35..6:36): B => _empty_/qs7#B#
         |[6:38..6:41): Int => scala/Int#
         |[6:45..6:87): (x: qs7.this.B) => x match { case _ => 1 } <= local2
         |[6:46..6:47): x <= local9
         |[6:49..6:52): qs7 => _empty_/qs7#
         |[6:58..6:59): B => _empty_/qs7#B#
         |[6:64..6:65): x => local9
         |[7:6..7:8): f1 <= _empty_/qs7#f1.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:22): AnyRef => java/lang/Object#
         |[7:29..7:30): m <= local10
         |[7:32..7:35): qs7 => _empty_/qs7#
         |[7:41..7:42): B => _empty_/qs7#B#
         |[8:6..8:8): g1 <= _empty_/qs7#g1.
         |[8:11..8:15): List => scala/collection/immutable/Nil.
         |[8:16..8:19): qs7 => _empty_/qs7#
         |[9:6..9:8): a2 <= _empty_/qs7#a2.
         |[9:11..9:18): classOf => scala/Predef.classOf().
         |[9:24..9:25): B => _empty_/qs7#B#
         |[10:6..10:8): b2 <= _empty_/qs7#b2.
         |[10:11..10:15): List => scala/collection/immutable/Nil.
         |[10:21..10:22): B => _empty_/qs7#B#
         |[11:6..11:8): c2 <= _empty_/qs7#c2.
         |[11:11..11:18): classOf => scala/Predef.classOf().
         |[11:19..11:23): List => scala/package.List#
         |[11:24..11:25): X => local11
         |[11:42..11:43): X <= local11
         |[11:52..11:53): B => _empty_/qs7#B#
         |[12:6..12:8): d2 <= _empty_/qs7#d2.
         |[12:10..12:14): List => scala/package.List#
         |[12:15..12:16): X => local12
         |[12:33..12:34): X <= local12
         |[12:43..12:44): B => _empty_/qs7#B#
         |[13:6..13:8): e2 <= _empty_/qs7#e2.
         |[13:10..13:25): PartialFunction => scala/PartialFunction#
         |[13:31..13:32): B => _empty_/qs7#B#
         |[13:34..13:37): Int => scala/Int#
         |[13:41..13:79): (x: this.B) => x match { case _ => 1 } <= local13
         |[13:42..13:43): x <= local20
         |[13:50..13:51): B => _empty_/qs7#B#
         |[13:56..13:57): x => local20
         |[14:6..14:8): g2 <= _empty_/qs7#g2.
         |[14:11..14:15): List => scala/collection/immutable/Nil.
         |""".stripMargin
    else
      """|[0:6..0:9): qs7 <= _empty_/qs7#
         |[0:10..0:10):  <= _empty_/qs7#`<init>`().
         |[1:8..1:9): B <= _empty_/qs7#B#
         |[1:9..1:9):  <= _empty_/qs7#B#`<init>`().
         |[2:6..2:8): a1 <= _empty_/qs7#a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:19..2:22): qs7 => _empty_/qs7#
         |[2:28..2:29): B => _empty_/qs7#B#
         |[3:6..3:8): b1 <= _empty_/qs7#b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:16..3:19): qs7 => _empty_/qs7#
         |[3:25..3:26): B => _empty_/qs7#B#
         |[4:6..4:8): c1 <= _empty_/qs7#c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:47..4:50): qs7 => _empty_/qs7#
         |[4:56..4:57): B => _empty_/qs7#B#
         |[5:6..5:8): d1 <= _empty_/qs7#d1.
         |[5:10..5:14): List => scala/package.List#
         |[5:15..5:16): X => local1
         |[5:33..5:34): X <= local1
         |[5:38..5:41): qs7 => _empty_/qs7#
         |[5:47..5:48): B => _empty_/qs7#B#
         |[6:6..6:8): e1 <= _empty_/qs7#e1.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:29): qs7 => _empty_/qs7#
         |[6:35..6:36): B => _empty_/qs7#B#
         |[6:38..6:41): Int => scala/Int#
         |[6:45..6:87): (x: qs7.this.B) => x match { case _ => 1 } <= local2
         |[6:46..6:47): x <= local9
         |[6:64..6:65): x => local9
         |[7:6..7:8): f1 <= _empty_/qs7#f1.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:22): AnyRef => java/lang/Object#
         |[7:29..7:30): m <= local10
         |[7:32..7:35): qs7 => _empty_/qs7#
         |[7:41..7:42): B => _empty_/qs7#B#
         |[8:6..8:8): g1 <= _empty_/qs7#g1.
         |[8:11..8:15): List => scala/collection/immutable/Nil.
         |[8:16..8:19): qs7 => _empty_/qs7#
         |[9:6..9:8): a2 <= _empty_/qs7#a2.
         |[9:11..9:18): classOf => scala/Predef.classOf().
         |[9:24..9:25): B => _empty_/qs7#B#
         |[10:6..10:8): b2 <= _empty_/qs7#b2.
         |[10:11..10:15): List => scala/collection/immutable/Nil.
         |[10:21..10:22): B => _empty_/qs7#B#
         |[11:6..11:8): c2 <= _empty_/qs7#c2.
         |[11:11..11:18): classOf => scala/Predef.classOf().
         |[11:19..11:23): List => scala/package.List#
         |[11:24..11:25): X => local11
         |[11:42..11:43): X <= local11
         |[11:52..11:53): B => _empty_/qs7#B#
         |[12:6..12:8): d2 <= _empty_/qs7#d2.
         |[12:10..12:14): List => scala/package.List#
         |[12:15..12:16): X => local12
         |[12:33..12:34): X <= local12
         |[12:43..12:44): B => _empty_/qs7#B#
         |[13:6..13:8): e2 <= _empty_/qs7#e2.
         |[13:10..13:25): PartialFunction => scala/PartialFunction#
         |[13:31..13:32): B => _empty_/qs7#B#
         |[13:34..13:37): Int => scala/Int#
         |[13:41..13:79): (x: this.B) => x match { case _ => 1 } <= local13
         |[13:42..13:43): x <= local20
         |[13:56..13:57): x => local20
         |[14:6..14:8): g2 <= _empty_/qs7#g2.
         |[14:11..14:15): List => scala/collection/immutable/Nil.
         |""".stripMargin,
  )

  occurrences(
    "a type through this of an object",
    """|object qs8 {
       |  class B
       |  val a1 = classOf[qs8.this.B]
       |  val b1 = List[qs8.this.B]()
       |  val c1 = classOf[List[X] forSome { type X <: qs8.this.B }]
       |  val d1: List[X] forSome { type X <: qs8.this.B } = null
       |  val e1: PartialFunction[qs8.this.B, Int] = (x: qs8.this.B) => x match { case _ => 1 }
       |  val f1 = List[AnyRef { def m: qs8.this.B }]()
       |  val g1 = List[qs8.this.type]()
       |  val a2 = classOf[qs8.B]
       |  val b2 = List[qs8.B]()
       |  val c2 = classOf[List[X] forSome { type X <: qs8.B }]
       |  val d2: List[X] forSome { type X <: qs8.B } = null
       |  val e2: PartialFunction[qs8.B, Int] = (x: qs8.B) => x match { case _ => 1 }
       |  val f2 = List[AnyRef { def m: qs8.B }]()
       |  val g2 = List[qs8.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:10): qs8 <= _empty_/qs8.
         |[1:8..1:9): B <= _empty_/qs8.B#
         |[1:9..1:9):  <= _empty_/qs8.B#`<init>`().
         |[2:6..2:8): a1 <= _empty_/qs8.a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:19..2:22): qs8 => _empty_/qs8.
         |[2:28..2:29): B => _empty_/qs8.B#
         |[3:6..3:8): b1 <= _empty_/qs8.b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:16..3:19): qs8 => _empty_/qs8.
         |[3:25..3:26): B => _empty_/qs8.B#
         |[4:6..4:8): c1 <= _empty_/qs8.c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:47..4:50): qs8 => _empty_/qs8.
         |[4:56..4:57): B => _empty_/qs8.B#
         |[5:6..5:8): d1 <= _empty_/qs8.d1.
         |[5:10..5:14): List => scala/package.List#
         |[5:15..5:16): X => local1
         |[5:33..5:34): X <= local1
         |[5:38..5:41): qs8 => _empty_/qs8.
         |[5:47..5:48): B => _empty_/qs8.B#
         |[6:6..6:8): e1 <= _empty_/qs8.e1.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:29): qs8 => _empty_/qs8.
         |[6:35..6:36): B => _empty_/qs8.B#
         |[6:38..6:41): Int => scala/Int#
         |[6:45..6:87): (x: qs8.this.B) => x match { case _ => 1 } <= local2
         |[6:46..6:47): x <= local9
         |[6:49..6:52): qs8 => _empty_/qs8.
         |[6:58..6:59): B => _empty_/qs8.B#
         |[6:64..6:65): x => local9
         |[7:6..7:8): f1 <= _empty_/qs8.f1.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:22): AnyRef => java/lang/Object#
         |[7:29..7:30): m <= local10
         |[7:32..7:35): qs8 => _empty_/qs8.
         |[7:41..7:42): B => _empty_/qs8.B#
         |[8:6..8:8): g1 <= _empty_/qs8.g1.
         |[8:11..8:15): List => scala/collection/immutable/Nil.
         |[8:16..8:19): qs8 => _empty_/qs8.
         |[9:6..9:8): a2 <= _empty_/qs8.a2.
         |[9:11..9:18): classOf => scala/Predef.classOf().
         |[9:19..9:22): qs8 => _empty_/qs8.
         |[9:23..9:24): B => _empty_/qs8.B#
         |[10:6..10:8): b2 <= _empty_/qs8.b2.
         |[10:11..10:15): List => scala/collection/immutable/Nil.
         |[10:16..10:19): qs8 => _empty_/qs8.
         |[10:20..10:21): B => _empty_/qs8.B#
         |[11:6..11:8): c2 <= _empty_/qs8.c2.
         |[11:11..11:18): classOf => scala/Predef.classOf().
         |[11:19..11:23): List => scala/package.List#
         |[11:24..11:25): X => local11
         |[11:42..11:43): X <= local11
         |[11:47..11:50): qs8 => _empty_/qs8.
         |[11:51..11:52): B => _empty_/qs8.B#
         |[12:6..12:8): d2 <= _empty_/qs8.d2.
         |[12:10..12:14): List => scala/package.List#
         |[12:15..12:16): X => local12
         |[12:33..12:34): X <= local12
         |[12:38..12:41): qs8 => _empty_/qs8.
         |[12:42..12:43): B => _empty_/qs8.B#
         |[13:6..13:8): e2 <= _empty_/qs8.e2.
         |[13:10..13:25): PartialFunction => scala/PartialFunction#
         |[13:26..13:29): qs8 => _empty_/qs8.
         |[13:30..13:31): B => _empty_/qs8.B#
         |[13:33..13:36): Int => scala/Int#
         |[13:40..13:77): (x: qs8.B) => x match { case _ => 1 } <= local13
         |[13:41..13:42): x <= local20
         |[13:44..13:47): qs8 => _empty_/qs8.
         |[13:48..13:49): B => _empty_/qs8.B#
         |[13:54..13:55): x => local20
         |[14:6..14:8): f2 <= _empty_/qs8.f2.
         |[14:11..14:15): List => scala/collection/immutable/Nil.
         |[14:16..14:22): AnyRef => java/lang/Object#
         |[14:29..14:30): m <= local21
         |[14:32..14:35): qs8 => _empty_/qs8.
         |[14:36..14:37): B => _empty_/qs8.B#
         |[15:6..15:8): g2 <= _empty_/qs8.g2.
         |[15:11..15:15): List => scala/collection/immutable/Nil.
         |[15:16..15:19): qs8 => _empty_/qs8.
         |""".stripMargin
    else
      """|[0:7..0:10): qs8 <= _empty_/qs8.
         |[1:8..1:9): B <= _empty_/qs8.B#
         |[1:9..1:9):  <= _empty_/qs8.B#`<init>`().
         |[2:6..2:8): a1 <= _empty_/qs8.a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:19..2:22): qs8 => _empty_/qs8.
         |[2:28..2:29): B => _empty_/qs8.B#
         |[3:6..3:8): b1 <= _empty_/qs8.b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:16..3:19): qs8 => _empty_/qs8.
         |[3:25..3:26): B => _empty_/qs8.B#
         |[4:6..4:8): c1 <= _empty_/qs8.c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:47..4:50): qs8 => _empty_/qs8.
         |[4:56..4:57): B => _empty_/qs8.B#
         |[5:6..5:8): d1 <= _empty_/qs8.d1.
         |[5:10..5:14): List => scala/package.List#
         |[5:15..5:16): X => local1
         |[5:33..5:34): X <= local1
         |[5:38..5:41): qs8 => _empty_/qs8.
         |[5:47..5:48): B => _empty_/qs8.B#
         |[6:6..6:8): e1 <= _empty_/qs8.e1.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:29): qs8 => _empty_/qs8.
         |[6:35..6:36): B => _empty_/qs8.B#
         |[6:38..6:41): Int => scala/Int#
         |[6:45..6:87): (x: qs8.this.B) => x match { case _ => 1 } <= local2
         |[6:46..6:47): x <= local9
         |[6:64..6:65): x => local9
         |[7:6..7:8): f1 <= _empty_/qs8.f1.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:22): AnyRef => java/lang/Object#
         |[7:29..7:30): m <= local10
         |[7:32..7:35): qs8 => _empty_/qs8.
         |[7:41..7:42): B => _empty_/qs8.B#
         |[8:6..8:8): g1 <= _empty_/qs8.g1.
         |[8:11..8:15): List => scala/collection/immutable/Nil.
         |[8:16..8:19): qs8 => _empty_/qs8.
         |[9:6..9:8): a2 <= _empty_/qs8.a2.
         |[9:11..9:18): classOf => scala/Predef.classOf().
         |[9:19..9:22): qs8 => _empty_/qs8.
         |[9:23..9:24): B => _empty_/qs8.B#
         |[10:6..10:8): b2 <= _empty_/qs8.b2.
         |[10:11..10:15): List => scala/collection/immutable/Nil.
         |[10:16..10:19): qs8 => _empty_/qs8.
         |[10:20..10:21): B => _empty_/qs8.B#
         |[11:6..11:8): c2 <= _empty_/qs8.c2.
         |[11:11..11:18): classOf => scala/Predef.classOf().
         |[11:19..11:23): List => scala/package.List#
         |[11:24..11:25): X => local11
         |[11:42..11:43): X <= local11
         |[11:47..11:50): qs8 => _empty_/qs8.
         |[11:51..11:52): B => _empty_/qs8.B#
         |[12:6..12:8): d2 <= _empty_/qs8.d2.
         |[12:10..12:14): List => scala/package.List#
         |[12:15..12:16): X => local12
         |[12:33..12:34): X <= local12
         |[12:38..12:41): qs8 => _empty_/qs8.
         |[12:42..12:43): B => _empty_/qs8.B#
         |[13:6..13:8): e2 <= _empty_/qs8.e2.
         |[13:10..13:25): PartialFunction => scala/PartialFunction#
         |[13:26..13:29): qs8 => _empty_/qs8.
         |[13:30..13:31): B => _empty_/qs8.B#
         |[13:33..13:36): Int => scala/Int#
         |[13:40..13:77): (x: qs8.B) => x match { case _ => 1 } <= local13
         |[13:41..13:42): x <= local20
         |[13:54..13:55): x => local20
         |[14:6..14:8): f2 <= _empty_/qs8.f2.
         |[14:11..14:15): List => scala/collection/immutable/Nil.
         |[14:16..14:22): AnyRef => java/lang/Object#
         |[14:29..14:30): m <= local21
         |[14:32..14:35): qs8 => _empty_/qs8.
         |[14:36..14:37): B => _empty_/qs8.B#
         |[15:6..15:8): g2 <= _empty_/qs8.g2.
         |[15:11..15:15): List => scala/collection/immutable/Nil.
         |[15:16..15:19): qs8 => _empty_/qs8.
         |""".stripMargin,
  )

  occurrences(
    "a type through a self alias of a class",
    """|class qs10 { self =>
       |  class B
       |  val a = classOf[self.B]
       |  val b = List[self.B]()
       |  val c = classOf[List[X] forSome { type X <: self.B }]
       |  val d: List[X] forSome { type X <: self.B } = null
       |  val e: PartialFunction[self.B, Int] = (x: self.B) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: self.B }]()
       |  val g = List[self.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): qs10 <= _empty_/qs10#
         |[0:11..0:11):  <= _empty_/qs10#`<init>`().
         |[0:13..0:17): self <= local0
         |[1:8..1:9): B <= _empty_/qs10#B#
         |[1:9..1:9):  <= _empty_/qs10#B#`<init>`().
         |[2:6..2:7): a <= _empty_/qs10#a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:22): self => local0
         |[2:23..2:24): B => _empty_/qs10#B#
         |[3:6..3:7): b <= _empty_/qs10#b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:19): self => local0
         |[3:20..3:21): B => _empty_/qs10#B#
         |[4:6..4:7): c <= _empty_/qs10#c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local1
         |[4:41..4:42): X <= local1
         |[4:46..4:50): self => local0
         |[4:51..4:52): B => _empty_/qs10#B#
         |[5:6..5:7): d <= _empty_/qs10#d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local2
         |[5:32..5:33): X <= local2
         |[5:37..5:41): self => local0
         |[5:42..5:43): B => _empty_/qs10#B#
         |[6:6..6:7): e <= _empty_/qs10#e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): self => local0
         |[6:30..6:31): B => _empty_/qs10#B#
         |[6:33..6:36): Int => scala/Int#
         |[6:40..6:78): (x: self.B) => x match { case _ => 1 } <= local3
         |[6:41..6:42): x <= local10
         |[6:44..6:48): self => local0
         |[6:49..6:50): B => _empty_/qs10#B#
         |[6:55..6:56): x => local10
         |[7:6..7:7): f <= _empty_/qs10#f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local11
         |[7:31..7:35): self => local0
         |[7:36..7:37): B => _empty_/qs10#B#
         |[8:6..8:7): g <= _empty_/qs10#g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |""".stripMargin
    else
      """|[0:6..0:10): qs10 <= _empty_/qs10#
         |[0:11..0:11):  <= _empty_/qs10#`<init>`().
         |[0:13..0:17): self <= local0
         |[1:8..1:9): B <= _empty_/qs10#B#
         |[1:9..1:9):  <= _empty_/qs10#B#`<init>`().
         |[2:6..2:7): a <= _empty_/qs10#a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:22): self => local0
         |[2:23..2:24): B => _empty_/qs10#B#
         |[3:6..3:7): b <= _empty_/qs10#b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:19): self => local0
         |[3:20..3:21): B => _empty_/qs10#B#
         |[4:6..4:7): c <= _empty_/qs10#c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local1
         |[4:41..4:42): X <= local1
         |[4:46..4:50): self => local0
         |[4:51..4:52): B => _empty_/qs10#B#
         |[5:6..5:7): d <= _empty_/qs10#d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local2
         |[5:32..5:33): X <= local2
         |[5:37..5:41): self => local0
         |[5:42..5:43): B => _empty_/qs10#B#
         |[6:6..6:7): e <= _empty_/qs10#e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): self => local0
         |[6:30..6:31): B => _empty_/qs10#B#
         |[6:33..6:36): Int => scala/Int#
         |[6:40..6:78): (x: self.B) => x match { case _ => 1 } <= local3
         |[6:41..6:42): x <= local10
         |[6:55..6:56): x => local10
         |[7:6..7:7): f <= _empty_/qs10#f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local11
         |[7:31..7:35): self => local0
         |[7:36..7:37): B => _empty_/qs10#B#
         |[8:6..8:7): g <= _empty_/qs10#g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |""".stripMargin,
  )

  occurrences(
    "a type through a self alias of an object",
    """|object qs11 { o =>
       |  class B
       |  val a = classOf[o.B]
       |  val b = List[o.B]()
       |  val c = classOf[List[X] forSome { type X <: o.B }]
       |  val d: List[X] forSome { type X <: o.B } = null
       |  val e: PartialFunction[o.B, Int] = (x: o.B) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: o.B }]()
       |  val g = List[o.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): qs11 <= _empty_/qs11.
         |[0:14..0:15): o <= local0
         |[1:8..1:9): B <= _empty_/qs11.B#
         |[1:9..1:9):  <= _empty_/qs11.B#`<init>`().
         |[2:6..2:7): a <= _empty_/qs11.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:19): o => local0
         |[2:20..2:21): B => _empty_/qs11.B#
         |[3:6..3:7): b <= _empty_/qs11.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:16): o => local0
         |[3:17..3:18): B => _empty_/qs11.B#
         |[4:6..4:7): c <= _empty_/qs11.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local1
         |[4:41..4:42): X <= local1
         |[4:46..4:47): o => local0
         |[4:48..4:49): B => _empty_/qs11.B#
         |[5:6..5:7): d <= _empty_/qs11.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local2
         |[5:32..5:33): X <= local2
         |[5:37..5:38): o => local0
         |[5:39..5:40): B => _empty_/qs11.B#
         |[6:6..6:7): e <= _empty_/qs11.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:26): o => local0
         |[6:27..6:28): B => _empty_/qs11.B#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:72): (x: o.B) => x match { case _ => 1 } <= local3
         |[6:38..6:39): x <= local10
         |[6:41..6:42): o => local0
         |[6:43..6:44): B => _empty_/qs11.B#
         |[6:49..6:50): x => local10
         |[7:6..7:7): f <= _empty_/qs11.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local11
         |[7:31..7:32): o => local0
         |[7:33..7:34): B => _empty_/qs11.B#
         |[8:6..8:7): g <= _empty_/qs11.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |""".stripMargin
    else
      """|[0:7..0:11): qs11 <= _empty_/qs11.
         |[0:14..0:15): o <= local0
         |[1:8..1:9): B <= _empty_/qs11.B#
         |[1:9..1:9):  <= _empty_/qs11.B#`<init>`().
         |[2:6..2:7): a <= _empty_/qs11.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:19): o => local0
         |[2:20..2:21): B => _empty_/qs11.B#
         |[3:6..3:7): b <= _empty_/qs11.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:16): o => local0
         |[3:17..3:18): B => _empty_/qs11.B#
         |[4:6..4:7): c <= _empty_/qs11.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local1
         |[4:41..4:42): X <= local1
         |[4:46..4:47): o => local0
         |[4:48..4:49): B => _empty_/qs11.B#
         |[5:6..5:7): d <= _empty_/qs11.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local2
         |[5:32..5:33): X <= local2
         |[5:37..5:38): o => local0
         |[5:39..5:40): B => _empty_/qs11.B#
         |[6:6..6:7): e <= _empty_/qs11.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:26): o => local0
         |[6:27..6:28): B => _empty_/qs11.B#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:72): (x: o.B) => x match { case _ => 1 } <= local3
         |[6:38..6:39): x <= local10
         |[6:49..6:50): x => local10
         |[7:6..7:7): f <= _empty_/qs11.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local11
         |[7:31..7:32): o => local0
         |[7:33..7:34): B => _empty_/qs11.B#
         |[8:6..8:7): g <= _empty_/qs11.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed package",
    """|package qs12a { package qs12b { class C; object o } }
       |object qs12 {
       |  import qs12a.{qs12b => r}
       |  val a = classOf[r.C]
       |  val b = List[r.C]()
       |  val c = classOf[List[X] forSome { type X <: r.C }]
       |  val d: List[X] forSome { type X <: r.C } = null
       |  val e: PartialFunction[r.C, Int] = (x: r.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: r.C }]()
       |  val g = List[r.o.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs12a <= qs12a/
         |[0:24..0:29): qs12b <= qs12a/qs12b/
         |[0:38..0:39): C <= qs12a/qs12b/C#
         |[0:39..0:39):  <= qs12a/qs12b/C#`<init>`().
         |[0:48..0:49): o <= qs12a/qs12b/o.
         |[1:7..1:11): qs12 <= _empty_/qs12.
         |[2:9..2:14): qs12a => qs12a/
         |[2:16..2:21): qs12b => qs12a/qs12b/
         |[3:6..3:7): a <= _empty_/qs12.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:19): r => qs12a/qs12b/
         |[3:20..3:21): C => qs12a/qs12b/C#
         |[4:6..4:7): b <= _empty_/qs12.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:16): r => qs12a/qs12b/
         |[4:17..4:18): C => qs12a/qs12b/C#
         |[5:6..5:7): c <= _empty_/qs12.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:47): r => qs12a/qs12b/
         |[5:48..5:49): C => qs12a/qs12b/C#
         |[6:6..6:7): d <= _empty_/qs12.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:38): r => qs12a/qs12b/
         |[6:39..6:40): C => qs12a/qs12b/C#
         |[7:6..7:7): e <= _empty_/qs12.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:26): r => qs12a/qs12b/
         |[7:27..7:28): C => qs12a/qs12b/C#
         |[7:30..7:33): Int => scala/Int#
         |[7:37..7:72): (x: r.C) => x match { case _ => 1 } <= local2
         |[7:38..7:39): x <= local9
         |[7:41..7:42): r => qs12a/qs12b/
         |[7:43..7:44): C => qs12a/qs12b/C#
         |[7:49..7:50): x => local9
         |[8:6..8:7): f <= _empty_/qs12.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:32): r => qs12a/qs12b/
         |[8:33..8:34): C => qs12a/qs12b/C#
         |[9:6..9:7): g <= _empty_/qs12.g.
         |[9:10..9:14): List => scala/collection/immutable/Nil.
         |[9:15..9:16): r => qs12a/qs12b/
         |[9:17..9:18): o => qs12a/qs12b/o.
         |""".stripMargin
    else
      """|[0:8..0:13): qs12a <= qs12a/
         |[0:24..0:29): qs12b <= qs12a/qs12b/
         |[0:38..0:39): C <= qs12a/qs12b/C#
         |[0:39..0:39):  <= qs12a/qs12b/C#`<init>`().
         |[0:48..0:49): o <= qs12a/qs12b/o.
         |[1:7..1:11): qs12 <= _empty_/qs12.
         |[2:9..2:14): qs12a => qs12a/
         |[2:16..2:21): qs12b => qs12a/qs12b/
         |[3:6..3:7): a <= _empty_/qs12.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:19): r => qs12a/qs12b/
         |[3:20..3:21): C => qs12a/qs12b/C#
         |[4:6..4:7): b <= _empty_/qs12.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:16): r => qs12a/qs12b/
         |[4:17..4:18): C => qs12a/qs12b/C#
         |[5:6..5:7): c <= _empty_/qs12.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:47): r => qs12a/qs12b/
         |[5:48..5:49): C => qs12a/qs12b/C#
         |[6:6..6:7): d <= _empty_/qs12.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:38): r => qs12a/qs12b/
         |[6:39..6:40): C => qs12a/qs12b/C#
         |[7:6..7:7): e <= _empty_/qs12.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:26): r => qs12a/qs12b/
         |[7:27..7:28): C => qs12a/qs12b/C#
         |[7:30..7:33): Int => scala/Int#
         |[7:37..7:72): (x: r.C) => x match { case _ => 1 } <= local2
         |[7:38..7:39): x <= local9
         |[7:49..7:50): x => local9
         |[8:6..8:7): f <= _empty_/qs12.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:32): r => qs12a/qs12b/
         |[8:33..8:34): C => qs12a/qs12b/C#
         |[9:6..9:7): g <= _empty_/qs12.g.
         |[9:10..9:14): List => scala/collection/immutable/Nil.
         |[9:15..9:16): r => qs12a/qs12b/
         |[9:17..9:18): o => qs12a/qs12b/o.
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed object",
    """|package qs13a { object o { class C; object p } }
       |object qs13 {
       |  import qs13a.{o => r}
       |  val a = classOf[r.C]
       |  val b = List[r.C]()
       |  val c = classOf[List[X] forSome { type X <: r.C }]
       |  val d: List[X] forSome { type X <: r.C } = null
       |  val e: PartialFunction[r.C, Int] = (x: r.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: r.C }]()
       |  val g = List[r.p.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs13a <= qs13a/
         |[0:23..0:24): o <= qs13a/o.
         |[0:33..0:34): C <= qs13a/o.C#
         |[0:34..0:34):  <= qs13a/o.C#`<init>`().
         |[0:43..0:44): p <= qs13a/o.p.
         |[1:7..1:11): qs13 <= _empty_/qs13.
         |[2:9..2:14): qs13a => qs13a/
         |[2:16..2:17): o => qs13a/o.
         |[3:6..3:7): a <= _empty_/qs13.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:19): r => qs13a/o.
         |[3:20..3:21): C => qs13a/o.C#
         |[4:6..4:7): b <= _empty_/qs13.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:16): r => qs13a/o.
         |[4:17..4:18): C => qs13a/o.C#
         |[5:6..5:7): c <= _empty_/qs13.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:47): r => qs13a/o.
         |[5:48..5:49): C => qs13a/o.C#
         |[6:6..6:7): d <= _empty_/qs13.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:38): r => qs13a/o.
         |[6:39..6:40): C => qs13a/o.C#
         |[7:6..7:7): e <= _empty_/qs13.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:26): r => qs13a/o.
         |[7:27..7:28): C => qs13a/o.C#
         |[7:30..7:33): Int => scala/Int#
         |[7:37..7:72): (x: r.C) => x match { case _ => 1 } <= local2
         |[7:38..7:39): x <= local9
         |[7:41..7:42): r => qs13a/o.
         |[7:43..7:44): C => qs13a/o.C#
         |[7:49..7:50): x => local9
         |[8:6..8:7): f <= _empty_/qs13.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:32): r => qs13a/o.
         |[8:33..8:34): C => qs13a/o.C#
         |[9:6..9:7): g <= _empty_/qs13.g.
         |[9:10..9:14): List => scala/collection/immutable/Nil.
         |[9:15..9:16): r => qs13a/o.
         |[9:17..9:18): p => qs13a/o.p.
         |""".stripMargin
    else
      """|[0:8..0:13): qs13a <= qs13a/
         |[0:23..0:24): o <= qs13a/o.
         |[0:33..0:34): C <= qs13a/o.C#
         |[0:34..0:34):  <= qs13a/o.C#`<init>`().
         |[0:43..0:44): p <= qs13a/o.p.
         |[1:7..1:11): qs13 <= _empty_/qs13.
         |[2:9..2:14): qs13a => qs13a/
         |[2:16..2:17): o => qs13a/o.
         |[3:6..3:7): a <= _empty_/qs13.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:19): r => qs13a/o.
         |[3:20..3:21): C => qs13a/o.C#
         |[4:6..4:7): b <= _empty_/qs13.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:16): r => qs13a/o.
         |[4:17..4:18): C => qs13a/o.C#
         |[5:6..5:7): c <= _empty_/qs13.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:47): r => qs13a/o.
         |[5:48..5:49): C => qs13a/o.C#
         |[6:6..6:7): d <= _empty_/qs13.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:38): r => qs13a/o.
         |[6:39..6:40): C => qs13a/o.C#
         |[7:6..7:7): e <= _empty_/qs13.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:26): r => qs13a/o.
         |[7:27..7:28): C => qs13a/o.C#
         |[7:30..7:33): Int => scala/Int#
         |[7:37..7:72): (x: r.C) => x match { case _ => 1 } <= local2
         |[7:38..7:39): x <= local9
         |[7:49..7:50): x => local9
         |[8:6..8:7): f <= _empty_/qs13.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:32): r => qs13a/o.
         |[8:33..8:34): C => qs13a/o.C#
         |[9:6..9:7): g <= _empty_/qs13.g.
         |[9:10..9:14): List => scala/collection/immutable/Nil.
         |[9:15..9:16): r => qs13a/o.
         |[9:17..9:18): p => qs13a/o.p.
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed object of a package object",
    """|package object qs14p { object O { class C; object p } }
       |object qs14 {
       |  import qs14p.{O => r}
       |  val a = classOf[r.C]
       |  val b = List[r.C]()
       |  val c = classOf[List[X] forSome { type X <: r.C }]
       |  val d: List[X] forSome { type X <: r.C } = null
       |  val e: PartialFunction[r.C, Int] = (x: r.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: r.C }]()
       |  val g = List[r.p.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:15..0:20): qs14p <= qs14p/package.
         |[0:30..0:31): O <= qs14p/package.O.
         |[0:40..0:41): C <= qs14p/package.O.C#
         |[0:41..0:41):  <= qs14p/package.O.C#`<init>`().
         |[0:50..0:51): p <= qs14p/package.O.p.
         |[1:7..1:11): qs14 <= _empty_/qs14.
         |[2:9..2:14): qs14p => qs14p/
         |[2:16..2:17): O => qs14p/package.O.
         |[3:6..3:7): a <= _empty_/qs14.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:19): r => qs14p/package.O.
         |[3:20..3:21): C => qs14p/package.O.C#
         |[4:6..4:7): b <= _empty_/qs14.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:16): r => qs14p/package.O.
         |[4:17..4:18): C => qs14p/package.O.C#
         |[5:6..5:7): c <= _empty_/qs14.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:47): r => qs14p/package.O.
         |[5:48..5:49): C => qs14p/package.O.C#
         |[6:6..6:7): d <= _empty_/qs14.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:38): r => qs14p/package.O.
         |[6:39..6:40): C => qs14p/package.O.C#
         |[7:6..7:7): e <= _empty_/qs14.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:26): r => qs14p/package.O.
         |[7:27..7:28): C => qs14p/package.O.C#
         |[7:30..7:33): Int => scala/Int#
         |[7:37..7:72): (x: r.C) => x match { case _ => 1 } <= local2
         |[7:38..7:39): x <= local9
         |[7:41..7:42): r => qs14p/package.O.
         |[7:43..7:44): C => qs14p/package.O.C#
         |[7:49..7:50): x => local9
         |[8:6..8:7): f <= _empty_/qs14.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:32): r => qs14p/package.O.
         |[8:33..8:34): C => qs14p/package.O.C#
         |[9:6..9:7): g <= _empty_/qs14.g.
         |[9:10..9:14): List => scala/collection/immutable/Nil.
         |[9:15..9:16): r => qs14p/package.O.
         |[9:17..9:18): p => qs14p/package.O.p.
         |""".stripMargin
    else
      """|[0:15..0:20): qs14p <= qs14p/package.
         |[0:30..0:31): O <= qs14p/package.O.
         |[0:40..0:41): C <= qs14p/package.O.C#
         |[0:41..0:41):  <= qs14p/package.O.C#`<init>`().
         |[0:50..0:51): p <= qs14p/package.O.p.
         |[1:7..1:11): qs14 <= _empty_/qs14.
         |[2:9..2:14): qs14p => qs14p/
         |[2:16..2:17): O => qs14p/package.O.
         |[3:6..3:7): a <= _empty_/qs14.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:19): r => qs14p/package.O.
         |[3:20..3:21): C => qs14p/package.O.C#
         |[4:6..4:7): b <= _empty_/qs14.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:16): r => qs14p/package.O.
         |[4:17..4:18): C => qs14p/package.O.C#
         |[5:6..5:7): c <= _empty_/qs14.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:47): r => qs14p/package.O.
         |[5:48..5:49): C => qs14p/package.O.C#
         |[6:6..6:7): d <= _empty_/qs14.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:38): r => qs14p/package.O.
         |[6:39..6:40): C => qs14p/package.O.C#
         |[7:6..7:7): e <= _empty_/qs14.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:26): r => qs14p/package.O.
         |[7:27..7:28): C => qs14p/package.O.C#
         |[7:30..7:33): Int => scala/Int#
         |[7:37..7:72): (x: r.C) => x match { case _ => 1 } <= local2
         |[7:38..7:39): x <= local9
         |[7:49..7:50): x => local9
         |[8:6..8:7): f <= _empty_/qs14.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:32): r => qs14p/package.O.
         |[8:33..8:34): C => qs14p/package.O.C#
         |[9:6..9:7): g <= _empty_/qs14.g.
         |[9:10..9:14): List => scala/collection/immutable/Nil.
         |[9:15..9:16): r => qs14p/package.O.
         |[9:17..9:18): p => qs14p/package.O.p.
         |""".stripMargin,
  )

  occurrences(
    "a type through an object renamed to an operator",
    """|object qs15a { object o { class C } }
       |object qs15 {
       |  import qs15a.{o => <+>}
       |  val a = classOf[<+>.C]
       |  val b = List[<+>.C]()
       |  val c = classOf[List[X] forSome { type X <: <+>.C }]
       |  val d: List[X] forSome { type X <: <+>.C } = null
       |  val e: PartialFunction[<+>.C, Int] = (x: <+>.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: <+>.C }]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:12): qs15a <= _empty_/qs15a.
         |[0:22..0:23): o <= _empty_/qs15a.o.
         |[0:32..0:33): C <= _empty_/qs15a.o.C#
         |[0:33..0:33):  <= _empty_/qs15a.o.C#`<init>`().
         |[1:7..1:11): qs15 <= _empty_/qs15.
         |[2:9..2:14): qs15a => _empty_/qs15a.
         |[2:16..2:17): o => _empty_/qs15a.o.
         |[3:6..3:7): a <= _empty_/qs15.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:21): <+> => _empty_/qs15a.o.
         |[3:22..3:23): C => _empty_/qs15a.o.C#
         |[4:6..4:7): b <= _empty_/qs15.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:18): <+> => _empty_/qs15a.o.
         |[4:19..4:20): C => _empty_/qs15a.o.C#
         |[5:6..5:7): c <= _empty_/qs15.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:49): <+> => _empty_/qs15a.o.
         |[5:50..5:51): C => _empty_/qs15a.o.C#
         |[6:6..6:7): d <= _empty_/qs15.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:40): <+> => _empty_/qs15a.o.
         |[6:41..6:42): C => _empty_/qs15a.o.C#
         |[7:6..7:7): e <= _empty_/qs15.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:28): <+> => _empty_/qs15a.o.
         |[7:29..7:30): C => _empty_/qs15a.o.C#
         |[7:32..7:35): Int => scala/Int#
         |[7:39..7:76): (x: <+>.C) => x match { case _ => 1 } <= local2
         |[7:40..7:41): x <= local9
         |[7:43..7:46): <+> => _empty_/qs15a.o.
         |[7:47..7:48): C => _empty_/qs15a.o.C#
         |[7:53..7:54): x => local9
         |[8:6..8:7): f <= _empty_/qs15.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:34): <+> => _empty_/qs15a.o.
         |[8:35..8:36): C => _empty_/qs15a.o.C#
         |""".stripMargin
    else
      """|[0:7..0:12): qs15a <= _empty_/qs15a.
         |[0:22..0:23): o <= _empty_/qs15a.o.
         |[0:32..0:33): C <= _empty_/qs15a.o.C#
         |[0:33..0:33):  <= _empty_/qs15a.o.C#`<init>`().
         |[1:7..1:11): qs15 <= _empty_/qs15.
         |[2:9..2:14): qs15a => _empty_/qs15a.
         |[2:16..2:17): o => _empty_/qs15a.o.
         |[3:6..3:7): a <= _empty_/qs15.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:21): <+> => _empty_/qs15a.o.
         |[3:22..3:23): C => _empty_/qs15a.o.C#
         |[4:6..4:7): b <= _empty_/qs15.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:18): <+> => _empty_/qs15a.o.
         |[4:19..4:20): C => _empty_/qs15a.o.C#
         |[5:6..5:7): c <= _empty_/qs15.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:49): <+> => _empty_/qs15a.o.
         |[5:50..5:51): C => _empty_/qs15a.o.C#
         |[6:6..6:7): d <= _empty_/qs15.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:40): <+> => _empty_/qs15a.o.
         |[6:41..6:42): C => _empty_/qs15a.o.C#
         |[7:6..7:7): e <= _empty_/qs15.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:28): <+> => _empty_/qs15a.o.
         |[7:29..7:30): C => _empty_/qs15a.o.C#
         |[7:32..7:35): Int => scala/Int#
         |[7:39..7:76): (x: <+>.C) => x match { case _ => 1 } <= local2
         |[7:40..7:41): x <= local9
         |[7:53..7:54): x => local9
         |[8:6..8:7): f <= _empty_/qs15.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:34): <+> => _empty_/qs15a.o.
         |[8:35..8:36): C => _empty_/qs15a.o.C#
         |""".stripMargin,
  )

  occurrences(
    "a type through an operator object, renamed and not",
    """|object qs16a { object +++ { class C } }
       |object qs16 {
       |  import qs16a.{+++ => ops}
       |  val a1 = classOf[qs16a.+++.C]
       |  val b1 = List[qs16a.+++.C]()
       |  val c1 = classOf[List[X] forSome { type X <: qs16a.+++.C }]
       |  val d1: List[X] forSome { type X <: qs16a.+++.C } = null
       |  val e1: PartialFunction[qs16a.+++.C, Int] = (x: qs16a.+++.C) => x match { case _ => 1 }
       |  val f1 = List[AnyRef { def m: qs16a.+++.C }]()
       |  val a2 = classOf[ops.C]
       |  val b2 = List[ops.C]()
       |  val c2 = classOf[List[X] forSome { type X <: ops.C }]
       |  val d2: List[X] forSome { type X <: ops.C } = null
       |  val e2: PartialFunction[ops.C, Int] = (x: ops.C) => x match { case _ => 1 }
       |  val f2 = List[AnyRef { def m: ops.C }]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:12): qs16a <= _empty_/qs16a.
         |[0:22..0:25): +++ <= _empty_/qs16a.`+++`.
         |[0:34..0:35): C <= _empty_/qs16a.`+++`.C#
         |[0:35..0:35):  <= _empty_/qs16a.`+++`.C#`<init>`().
         |[1:7..1:11): qs16 <= _empty_/qs16.
         |[2:9..2:14): qs16a => _empty_/qs16a.
         |[2:16..2:19): +++ => _empty_/qs16a.`+++`.
         |[3:6..3:8): a1 <= _empty_/qs16.a1.
         |[3:11..3:18): classOf => scala/Predef.classOf().
         |[3:19..3:24): qs16a => _empty_/qs16a.
         |[3:25..3:28): +++ => _empty_/qs16a.`+++`.
         |[3:29..3:30): C => _empty_/qs16a.`+++`.C#
         |[4:6..4:8): b1 <= _empty_/qs16.b1.
         |[4:11..4:15): List => scala/collection/immutable/Nil.
         |[4:16..4:21): qs16a => _empty_/qs16a.
         |[4:22..4:25): +++ => _empty_/qs16a.`+++`.
         |[4:26..4:27): C => _empty_/qs16a.`+++`.C#
         |[5:6..5:8): c1 <= _empty_/qs16.c1.
         |[5:11..5:18): classOf => scala/Predef.classOf().
         |[5:19..5:23): List => scala/package.List#
         |[5:24..5:25): X => local0
         |[5:42..5:43): X <= local0
         |[5:47..5:52): qs16a => _empty_/qs16a.
         |[5:53..5:56): +++ => _empty_/qs16a.`+++`.
         |[5:57..5:58): C => _empty_/qs16a.`+++`.C#
         |[6:6..6:8): d1 <= _empty_/qs16.d1.
         |[6:10..6:14): List => scala/package.List#
         |[6:15..6:16): X => local1
         |[6:33..6:34): X <= local1
         |[6:38..6:43): qs16a => _empty_/qs16a.
         |[6:44..6:47): +++ => _empty_/qs16a.`+++`.
         |[6:48..6:49): C => _empty_/qs16a.`+++`.C#
         |[7:6..7:8): e1 <= _empty_/qs16.e1.
         |[7:10..7:25): PartialFunction => scala/PartialFunction#
         |[7:26..7:31): qs16a => _empty_/qs16a.
         |[7:32..7:35): +++ => _empty_/qs16a.`+++`.
         |[7:36..7:37): C => _empty_/qs16a.`+++`.C#
         |[7:39..7:42): Int => scala/Int#
         |[7:46..7:89): (x: qs16a.+++.C) => x match { case _ => 1 } <= local2
         |[7:47..7:48): x <= local9
         |[7:50..7:55): qs16a => _empty_/qs16a.
         |[7:56..7:59): +++ => _empty_/qs16a.`+++`.
         |[7:60..7:61): C => _empty_/qs16a.`+++`.C#
         |[7:66..7:67): x => local9
         |[8:6..8:8): f1 <= _empty_/qs16.f1.
         |[8:11..8:15): List => scala/collection/immutable/Nil.
         |[8:16..8:22): AnyRef => java/lang/Object#
         |[8:29..8:30): m <= local10
         |[8:32..8:37): qs16a => _empty_/qs16a.
         |[8:38..8:41): +++ => _empty_/qs16a.`+++`.
         |[8:42..8:43): C => _empty_/qs16a.`+++`.C#
         |[9:6..9:8): a2 <= _empty_/qs16.a2.
         |[9:11..9:18): classOf => scala/Predef.classOf().
         |[9:19..9:22): ops => _empty_/qs16a.`+++`.
         |[9:23..9:24): C => _empty_/qs16a.`+++`.C#
         |[10:6..10:8): b2 <= _empty_/qs16.b2.
         |[10:11..10:15): List => scala/collection/immutable/Nil.
         |[10:16..10:19): ops => _empty_/qs16a.`+++`.
         |[10:20..10:21): C => _empty_/qs16a.`+++`.C#
         |[11:6..11:8): c2 <= _empty_/qs16.c2.
         |[11:11..11:18): classOf => scala/Predef.classOf().
         |[11:19..11:23): List => scala/package.List#
         |[11:24..11:25): X => local11
         |[11:42..11:43): X <= local11
         |[11:47..11:50): ops => _empty_/qs16a.`+++`.
         |[11:51..11:52): C => _empty_/qs16a.`+++`.C#
         |[12:6..12:8): d2 <= _empty_/qs16.d2.
         |[12:10..12:14): List => scala/package.List#
         |[12:15..12:16): X => local12
         |[12:33..12:34): X <= local12
         |[12:38..12:41): ops => _empty_/qs16a.`+++`.
         |[12:42..12:43): C => _empty_/qs16a.`+++`.C#
         |[13:6..13:8): e2 <= _empty_/qs16.e2.
         |[13:10..13:25): PartialFunction => scala/PartialFunction#
         |[13:26..13:29): ops => _empty_/qs16a.`+++`.
         |[13:30..13:31): C => _empty_/qs16a.`+++`.C#
         |[13:33..13:36): Int => scala/Int#
         |[13:40..13:77): (x: ops.C) => x match { case _ => 1 } <= local13
         |[13:41..13:42): x <= local20
         |[13:44..13:47): ops => _empty_/qs16a.`+++`.
         |[13:48..13:49): C => _empty_/qs16a.`+++`.C#
         |[13:54..13:55): x => local20
         |[14:6..14:8): f2 <= _empty_/qs16.f2.
         |[14:11..14:15): List => scala/collection/immutable/Nil.
         |[14:16..14:22): AnyRef => java/lang/Object#
         |[14:29..14:30): m <= local21
         |[14:32..14:35): ops => _empty_/qs16a.`+++`.
         |[14:36..14:37): C => _empty_/qs16a.`+++`.C#
         |""".stripMargin
    else
      """|[0:7..0:12): qs16a <= _empty_/qs16a.
         |[0:22..0:25): +++ <= _empty_/qs16a.`+++`.
         |[0:34..0:35): C <= _empty_/qs16a.`+++`.C#
         |[0:35..0:35):  <= _empty_/qs16a.`+++`.C#`<init>`().
         |[1:7..1:11): qs16 <= _empty_/qs16.
         |[2:9..2:14): qs16a => _empty_/qs16a.
         |[2:16..2:19): +++ => _empty_/qs16a.`+++`.
         |[3:6..3:8): a1 <= _empty_/qs16.a1.
         |[3:11..3:18): classOf => scala/Predef.classOf().
         |[3:19..3:24): qs16a => _empty_/qs16a.
         |[3:25..3:28): +++ => _empty_/qs16a.`+++`.
         |[3:29..3:30): C => _empty_/qs16a.`+++`.C#
         |[4:6..4:8): b1 <= _empty_/qs16.b1.
         |[4:11..4:15): List => scala/collection/immutable/Nil.
         |[4:16..4:21): qs16a => _empty_/qs16a.
         |[4:22..4:25): +++ => _empty_/qs16a.`+++`.
         |[4:26..4:27): C => _empty_/qs16a.`+++`.C#
         |[5:6..5:8): c1 <= _empty_/qs16.c1.
         |[5:11..5:18): classOf => scala/Predef.classOf().
         |[5:19..5:23): List => scala/package.List#
         |[5:24..5:25): X => local0
         |[5:42..5:43): X <= local0
         |[5:47..5:52): qs16a => _empty_/qs16a.
         |[5:53..5:56): +++ => _empty_/qs16a.`+++`.
         |[5:57..5:58): C => _empty_/qs16a.`+++`.C#
         |[6:6..6:8): d1 <= _empty_/qs16.d1.
         |[6:10..6:14): List => scala/package.List#
         |[6:15..6:16): X => local1
         |[6:33..6:34): X <= local1
         |[6:38..6:43): qs16a => _empty_/qs16a.
         |[6:44..6:47): +++ => _empty_/qs16a.`+++`.
         |[6:48..6:49): C => _empty_/qs16a.`+++`.C#
         |[7:6..7:8): e1 <= _empty_/qs16.e1.
         |[7:10..7:25): PartialFunction => scala/PartialFunction#
         |[7:26..7:31): qs16a => _empty_/qs16a.
         |[7:32..7:35): +++ => _empty_/qs16a.`+++`.
         |[7:36..7:37): C => _empty_/qs16a.`+++`.C#
         |[7:39..7:42): Int => scala/Int#
         |[7:46..7:89): (x: qs16a.+++.C) => x match { case _ => 1 } <= local2
         |[7:47..7:48): x <= local9
         |[7:66..7:67): x => local9
         |[8:6..8:8): f1 <= _empty_/qs16.f1.
         |[8:11..8:15): List => scala/collection/immutable/Nil.
         |[8:16..8:22): AnyRef => java/lang/Object#
         |[8:29..8:30): m <= local10
         |[8:32..8:37): qs16a => _empty_/qs16a.
         |[8:38..8:41): +++ => _empty_/qs16a.`+++`.
         |[8:42..8:43): C => _empty_/qs16a.`+++`.C#
         |[9:6..9:8): a2 <= _empty_/qs16.a2.
         |[9:11..9:18): classOf => scala/Predef.classOf().
         |[9:19..9:22): ops => _empty_/qs16a.`+++`.
         |[9:23..9:24): C => _empty_/qs16a.`+++`.C#
         |[10:6..10:8): b2 <= _empty_/qs16.b2.
         |[10:11..10:15): List => scala/collection/immutable/Nil.
         |[10:16..10:19): ops => _empty_/qs16a.`+++`.
         |[10:20..10:21): C => _empty_/qs16a.`+++`.C#
         |[11:6..11:8): c2 <= _empty_/qs16.c2.
         |[11:11..11:18): classOf => scala/Predef.classOf().
         |[11:19..11:23): List => scala/package.List#
         |[11:24..11:25): X => local11
         |[11:42..11:43): X <= local11
         |[11:47..11:50): ops => _empty_/qs16a.`+++`.
         |[11:51..11:52): C => _empty_/qs16a.`+++`.C#
         |[12:6..12:8): d2 <= _empty_/qs16.d2.
         |[12:10..12:14): List => scala/package.List#
         |[12:15..12:16): X => local12
         |[12:33..12:34): X <= local12
         |[12:38..12:41): ops => _empty_/qs16a.`+++`.
         |[12:42..12:43): C => _empty_/qs16a.`+++`.C#
         |[13:6..13:8): e2 <= _empty_/qs16.e2.
         |[13:10..13:25): PartialFunction => scala/PartialFunction#
         |[13:26..13:29): ops => _empty_/qs16a.`+++`.
         |[13:30..13:31): C => _empty_/qs16a.`+++`.C#
         |[13:33..13:36): Int => scala/Int#
         |[13:40..13:77): (x: ops.C) => x match { case _ => 1 } <= local13
         |[13:41..13:42): x <= local20
         |[13:54..13:55): x => local20
         |[14:6..14:8): f2 <= _empty_/qs16.f2.
         |[14:11..14:15): List => scala/collection/immutable/Nil.
         |[14:16..14:22): AnyRef => java/lang/Object#
         |[14:29..14:30): m <= local21
         |[14:32..14:35): ops => _empty_/qs16a.`+++`.
         |[14:36..14:37): C => _empty_/qs16a.`+++`.C#
         |""".stripMargin,
  )

  occurrences(
    "a type through an object renamed to a parameter name of applyOrElse",
    """|package qs17a { object o { class C } }
       |object qs17 {
       |  import qs17a.{o => default}
       |  val a = classOf[default.C]
       |  val b = List[default.C]()
       |  val c = classOf[List[X] forSome { type X <: default.C }]
       |  val d: List[X] forSome { type X <: default.C } = null
       |  val e: PartialFunction[default.C, Int] = (x: default.C) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: default.C }]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs17a <= qs17a/
         |[0:23..0:24): o <= qs17a/o.
         |[0:33..0:34): C <= qs17a/o.C#
         |[0:34..0:34):  <= qs17a/o.C#`<init>`().
         |[1:7..1:11): qs17 <= _empty_/qs17.
         |[2:9..2:14): qs17a => qs17a/
         |[2:16..2:17): o => qs17a/o.
         |[3:6..3:7): a <= _empty_/qs17.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:25): default => qs17a/o.
         |[3:26..3:27): C => qs17a/o.C#
         |[4:6..4:7): b <= _empty_/qs17.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:22): default => qs17a/o.
         |[4:23..4:24): C => qs17a/o.C#
         |[5:6..5:7): c <= _empty_/qs17.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:53): default => qs17a/o.
         |[5:54..5:55): C => qs17a/o.C#
         |[6:6..6:7): d <= _empty_/qs17.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:44): default => qs17a/o.
         |[6:45..6:46): C => qs17a/o.C#
         |[7:6..7:7): e <= _empty_/qs17.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:32): default => qs17a/o.
         |[7:33..7:34): C => qs17a/o.C#
         |[7:36..7:39): Int => scala/Int#
         |[7:43..7:84): (x: default.C) => x match { case _ => 1 } <= local2
         |[7:44..7:45): x <= local9
         |[7:47..7:54): default => qs17a/o.
         |[7:55..7:56): C => qs17a/o.C#
         |[7:61..7:62): x => local9
         |[8:6..8:7): f <= _empty_/qs17.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:38): default => qs17a/o.
         |[8:39..8:40): C => qs17a/o.C#
         |""".stripMargin
    else
      """|[0:8..0:13): qs17a <= qs17a/
         |[0:23..0:24): o <= qs17a/o.
         |[0:33..0:34): C <= qs17a/o.C#
         |[0:34..0:34):  <= qs17a/o.C#`<init>`().
         |[1:7..1:11): qs17 <= _empty_/qs17.
         |[2:9..2:14): qs17a => qs17a/
         |[2:16..2:17): o => qs17a/o.
         |[3:6..3:7): a <= _empty_/qs17.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:25): default => qs17a/o.
         |[3:26..3:27): C => qs17a/o.C#
         |[4:6..4:7): b <= _empty_/qs17.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:22): default => qs17a/o.
         |[4:23..4:24): C => qs17a/o.C#
         |[5:6..5:7): c <= _empty_/qs17.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:53): default => qs17a/o.
         |[5:54..5:55): C => qs17a/o.C#
         |[6:6..6:7): d <= _empty_/qs17.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:44): default => qs17a/o.
         |[6:45..6:46): C => qs17a/o.C#
         |[7:6..7:7): e <= _empty_/qs17.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:32): default => qs17a/o.
         |[7:33..7:34): C => qs17a/o.C#
         |[7:36..7:39): Int => scala/Int#
         |[7:43..7:84): (x: default.C) => x match { case _ => 1 } <= local2
         |[7:44..7:45): x <= local9
         |[7:61..7:62): x => local9
         |[8:6..8:7): f <= _empty_/qs17.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:38): default => qs17a/o.
         |[8:39..8:40): C => qs17a/o.C#
         |""".stripMargin,
  )

  occurrences(
    "a type through renames of a package object and of another package to one name",
    """|package object qs37a { class E }
       |package qs37x { package object qs37b { class C } }
       |object qs37 {
       |  import qs37a.{`package` => q}
       |  val a1 = classOf[q.E]
       |  val b1 = List[q.E]()
       |  val c1 = classOf[List[X] forSome { type X <: q.E }]
       |  val e1: PartialFunction[q.E, Int] = (x: q.E) => x match { case _ => 1 }
       |  object i {
       |    import qs37x.{qs37b => q}
       |    val a2 = classOf[q.C]
       |    val b2 = List[q.C]()
       |    val c2 = classOf[List[X] forSome { type X <: q.C }]
       |    val e2: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:15..0:20): qs37a <= qs37a/package.
         |[0:29..0:30): E <= qs37a/package.E#
         |[0:30..0:30):  <= qs37a/package.E#`<init>`().
         |[1:8..1:13): qs37x <= qs37x/
         |[1:31..1:36): qs37b <= qs37x/qs37b/package.
         |[1:45..1:46): C <= qs37x/qs37b/package.C#
         |[1:46..1:46):  <= qs37x/qs37b/package.C#`<init>`().
         |[2:7..2:11): qs37 <= _empty_/qs37.
         |[3:9..3:14): qs37a => qs37a/
         |[3:16..3:25): `package` => qs37a/package.
         |[4:6..4:8): a1 <= _empty_/qs37.a1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:20): q => qs37a/package.
         |[4:21..4:22): E => qs37a/package.E#
         |[5:6..5:8): b1 <= _empty_/qs37.b1.
         |[5:11..5:15): List => scala/collection/immutable/Nil.
         |[5:16..5:17): q => qs37a/package.
         |[5:18..5:19): E => qs37a/package.E#
         |[6:6..6:8): c1 <= _empty_/qs37.c1.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:19..6:23): List => scala/package.List#
         |[6:24..6:25): X => local0
         |[6:42..6:43): X <= local0
         |[6:47..6:48): q => qs37a/package.
         |[6:49..6:50): E => qs37a/package.E#
         |[7:6..7:8): e1 <= _empty_/qs37.e1.
         |[7:10..7:25): PartialFunction => scala/PartialFunction#
         |[7:26..7:27): q => qs37a/package.
         |[7:28..7:29): E => qs37a/package.E#
         |[7:31..7:34): Int => scala/Int#
         |[7:38..7:73): (x: q.E) => x match { case _ => 1 } <= local1
         |[7:39..7:40): x <= local8
         |[7:42..7:43): q => qs37a/package.
         |[7:44..7:45): E => qs37a/package.E#
         |[7:50..7:51): x => local8
         |[8:9..8:10): i <= _empty_/qs37.i.
         |[9:11..9:16): qs37x => qs37x/
         |[9:18..9:23): qs37b => qs37x/qs37b/
         |[10:8..10:10): a2 <= _empty_/qs37.i.a2.
         |[10:13..10:20): classOf => scala/Predef.classOf().
         |[10:21..10:22): q => qs37x/qs37b/
         |[10:23..10:24): C => qs37x/qs37b/package.C#
         |[11:8..11:10): b2 <= _empty_/qs37.i.b2.
         |[11:13..11:17): List => scala/collection/immutable/Nil.
         |[11:18..11:19): q => qs37x/qs37b/
         |[11:20..11:21): C => qs37x/qs37b/package.C#
         |[12:8..12:10): c2 <= _empty_/qs37.i.c2.
         |[12:13..12:20): classOf => scala/Predef.classOf().
         |[12:21..12:25): List => scala/package.List#
         |[12:26..12:27): X => local9
         |[12:44..12:45): X <= local9
         |[12:49..12:50): q => qs37x/qs37b/
         |[12:51..12:52): C => qs37x/qs37b/package.C#
         |[13:8..13:10): e2 <= _empty_/qs37.i.e2.
         |[13:12..13:27): PartialFunction => scala/PartialFunction#
         |[13:28..13:29): q => qs37x/qs37b/
         |[13:30..13:31): C => qs37x/qs37b/package.C#
         |[13:33..13:36): Int => scala/Int#
         |[13:40..13:75): (x: q.C) => x match { case _ => 1 } <= local10
         |[13:41..13:42): x <= local17
         |[13:44..13:45): q => qs37x/qs37b/
         |[13:46..13:47): C => qs37x/qs37b/package.C#
         |[13:52..13:53): x => local17
         |""".stripMargin
    else
      """|[0:15..0:20): qs37a <= qs37a/package.
         |[0:29..0:30): E <= qs37a/package.E#
         |[0:30..0:30):  <= qs37a/package.E#`<init>`().
         |[1:8..1:13): qs37x <= qs37x/
         |[1:31..1:36): qs37b <= qs37x/qs37b/package.
         |[1:45..1:46): C <= qs37x/qs37b/package.C#
         |[1:46..1:46):  <= qs37x/qs37b/package.C#`<init>`().
         |[2:7..2:11): qs37 <= _empty_/qs37.
         |[3:9..3:14): qs37a => qs37a/
         |[3:16..3:25): `package` => qs37a/package.
         |[4:6..4:8): a1 <= _empty_/qs37.a1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:20): q => qs37a/package.
         |[4:21..4:22): E => qs37a/package.E#
         |[5:6..5:8): b1 <= _empty_/qs37.b1.
         |[5:11..5:15): List => scala/collection/immutable/Nil.
         |[5:16..5:17): q => qs37a/package.
         |[5:18..5:19): E => qs37a/package.E#
         |[6:6..6:8): c1 <= _empty_/qs37.c1.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:19..6:23): List => scala/package.List#
         |[6:24..6:25): X => local0
         |[6:42..6:43): X <= local0
         |[6:47..6:48): q => qs37a/package.
         |[6:49..6:50): E => qs37a/package.E#
         |[7:6..7:8): e1 <= _empty_/qs37.e1.
         |[7:10..7:25): PartialFunction => scala/PartialFunction#
         |[7:26..7:27): q => qs37a/package.
         |[7:28..7:29): E => qs37a/package.E#
         |[7:31..7:34): Int => scala/Int#
         |[7:38..7:73): (x: q.E) => x match { case _ => 1 } <= local1
         |[7:39..7:40): x <= local8
         |[7:50..7:51): x => local8
         |[8:9..8:10): i <= _empty_/qs37.i.
         |[9:11..9:16): qs37x => qs37x/
         |[9:18..9:23): qs37b => qs37x/qs37b/
         |[10:8..10:10): a2 <= _empty_/qs37.i.a2.
         |[10:13..10:20): classOf => scala/Predef.classOf().
         |[10:21..10:22): q => qs37x/qs37b/
         |[10:23..10:24): C => qs37x/qs37b/package.C#
         |[11:8..11:10): b2 <= _empty_/qs37.i.b2.
         |[11:13..11:17): List => scala/collection/immutable/Nil.
         |[11:18..11:19): q => qs37x/qs37b/
         |[11:20..11:21): C => qs37x/qs37b/package.C#
         |[12:8..12:10): c2 <= _empty_/qs37.i.c2.
         |[12:13..12:20): classOf => scala/Predef.classOf().
         |[12:21..12:25): List => scala/package.List#
         |[12:26..12:27): X => local9
         |[12:44..12:45): X <= local9
         |[12:49..12:50): q => qs37x/qs37b/
         |[12:51..12:52): C => qs37x/qs37b/package.C#
         |[13:8..13:10): e2 <= _empty_/qs37.i.e2.
         |[13:12..13:27): PartialFunction => scala/PartialFunction#
         |[13:28..13:29): q => qs37x/qs37b/
         |[13:30..13:31): C => qs37x/qs37b/package.C#
         |[13:33..13:36): Int => scala/Int#
         |[13:40..13:75): (x: q.C) => x match { case _ => 1 } <= local10
         |[13:41..13:42): x <= local17
         |[13:52..13:53): x => local17
         |""".stripMargin,
  )

  occurrences(
    "a type through renames of a package and its package object to one name",
    """|package qs38a { package object qs38b { class C } }
       |object qs38 {
       |  import qs38a.qs38b.{`package` => q}
       |  object i {
       |    import qs38a.{qs38b => q}
       |    val a = classOf[q.C]
       |    val b = List[q.C]()
       |    val c = classOf[List[X] forSome { type X <: q.C }]
       |    val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs38a <= qs38a/
         |[0:31..0:36): qs38b <= qs38a/qs38b/package.
         |[0:45..0:46): C <= qs38a/qs38b/package.C#
         |[0:46..0:46):  <= qs38a/qs38b/package.C#`<init>`().
         |[1:7..1:11): qs38 <= _empty_/qs38.
         |[2:9..2:14): qs38a => qs38a/
         |[2:15..2:20): qs38b => qs38a/qs38b/
         |[2:22..2:31): `package` => qs38a/qs38b/package.
         |[3:9..3:10): i <= _empty_/qs38.i.
         |[4:11..4:16): qs38a => qs38a/
         |[4:18..4:23): qs38b => qs38a/qs38b/
         |[5:8..5:9): a <= _empty_/qs38.i.a.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:21): q => qs38a/qs38b/package.
         |[5:22..5:23): C => qs38a/qs38b/package.C#
         |[6:8..6:9): b <= _empty_/qs38.i.b.
         |[6:12..6:16): List => scala/collection/immutable/Nil.
         |[6:17..6:18): q => qs38a/qs38b/package.
         |[6:19..6:20): C => qs38a/qs38b/package.C#
         |[7:8..7:9): c <= _empty_/qs38.i.c.
         |[7:12..7:19): classOf => scala/Predef.classOf().
         |[7:20..7:24): List => scala/package.List#
         |[7:25..7:26): X => local0
         |[7:43..7:44): X <= local0
         |[7:48..7:49): q => qs38a/qs38b/package.
         |[7:50..7:51): C => qs38a/qs38b/package.C#
         |[8:8..8:9): e <= _empty_/qs38.i.e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): q => qs38a/qs38b/
         |[8:29..8:30): C => qs38a/qs38b/package.C#
         |[8:32..8:35): Int => scala/Int#
         |[8:39..8:74): (x: q.C) => x match { case _ => 1 } <= local1
         |[8:40..8:41): x <= local8
         |[8:43..8:44): q => qs38a/qs38b/package.
         |[8:45..8:46): C => qs38a/qs38b/package.C#
         |[8:51..8:52): x => local8
         |""".stripMargin
    else
      """|[0:8..0:13): qs38a <= qs38a/
         |[0:31..0:36): qs38b <= qs38a/qs38b/package.
         |[0:45..0:46): C <= qs38a/qs38b/package.C#
         |[0:46..0:46):  <= qs38a/qs38b/package.C#`<init>`().
         |[1:7..1:11): qs38 <= _empty_/qs38.
         |[2:9..2:14): qs38a => qs38a/
         |[2:15..2:20): qs38b => qs38a/qs38b/
         |[2:22..2:31): `package` => qs38a/qs38b/package.
         |[3:9..3:10): i <= _empty_/qs38.i.
         |[4:11..4:16): qs38a => qs38a/
         |[4:18..4:23): qs38b => qs38a/qs38b/
         |[5:8..5:9): a <= _empty_/qs38.i.a.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:21): q => qs38a/qs38b/package.
         |[5:22..5:23): C => qs38a/qs38b/package.C#
         |[6:8..6:9): b <= _empty_/qs38.i.b.
         |[6:12..6:16): List => scala/collection/immutable/Nil.
         |[6:17..6:18): q => qs38a/qs38b/package.
         |[6:19..6:20): C => qs38a/qs38b/package.C#
         |[7:8..7:9): c <= _empty_/qs38.i.c.
         |[7:12..7:19): classOf => scala/Predef.classOf().
         |[7:20..7:24): List => scala/package.List#
         |[7:25..7:26): X => local0
         |[7:43..7:44): X <= local0
         |[7:48..7:49): q => qs38a/qs38b/package.
         |[7:50..7:51): C => qs38a/qs38b/package.C#
         |[8:8..8:9): e <= _empty_/qs38.i.e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): q => qs38a/qs38b/
         |[8:29..8:30): C => qs38a/qs38b/package.C#
         |[8:32..8:35): Int => scala/Int#
         |[8:39..8:74): (x: q.C) => x match { case _ => 1 } <= local1
         |[8:40..8:41): x <= local8
         |[8:51..8:52): x => local8
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed object in constructor defaults",
    """|package qs39a { object o { class C } }
       |object qs39 {
       |  import qs39a.{o => q}
       |  object K
       |  class K(
       |      a: Any = classOf[q.C],
       |      b: Any = List[q.C](),
       |      c: Any = classOf[List[X] forSome { type X <: q.C }],
       |      e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 },
       |  )
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs39a <= qs39a/
         |[0:23..0:24): o <= qs39a/o.
         |[0:33..0:34): C <= qs39a/o.C#
         |[0:34..0:34):  <= qs39a/o.C#`<init>`().
         |[1:7..1:11): qs39 <= _empty_/qs39.
         |[2:9..2:14): qs39a => qs39a/
         |[2:16..2:17): o => qs39a/o.
         |[3:9..3:10): K <= _empty_/qs39.K.
         |[4:8..4:9): K <= _empty_/qs39.K#
         |[4:9..4:9):  <= _empty_/qs39.K#`<init>`().
         |[5:6..5:7): a <= _empty_/qs39.K#`<init>`().(a)
         |[5:6..5:7): a <= _empty_/qs39.K#a.
         |[5:9..5:12): Any => scala/Any#
         |[5:15..5:22): classOf => scala/Predef.classOf().
         |[5:23..5:24): q => qs39a/o.
         |[5:25..5:26): C => qs39a/o.C#
         |[6:6..6:7): b <= _empty_/qs39.K#`<init>`().(b)
         |[6:6..6:7): b <= _empty_/qs39.K#b.
         |[6:9..6:12): Any => scala/Any#
         |[6:15..6:19): List => scala/collection/immutable/Nil.
         |[6:20..6:21): q => qs39a/o.
         |[6:22..6:23): C => qs39a/o.C#
         |[7:6..7:7): c <= _empty_/qs39.K#`<init>`().(c)
         |[7:6..7:7): c <= _empty_/qs39.K#c.
         |[7:9..7:12): Any => scala/Any#
         |[7:15..7:22): classOf => scala/Predef.classOf().
         |[7:23..7:27): List => scala/package.List#
         |[7:28..7:29): X => local8
         |[7:46..7:47): X <= local8
         |[7:51..7:52): q => qs39a/o.
         |[7:53..7:54): C => qs39a/o.C#
         |[8:6..8:7): e <= _empty_/qs39.K#`<init>`().(e)
         |[8:6..8:7): e <= _empty_/qs39.K#e.
         |[8:9..8:24): PartialFunction => scala/PartialFunction#
         |[8:25..8:26): q => qs39a/o.
         |[8:27..8:28): C => qs39a/o.C#
         |[8:30..8:33): Int => scala/Int#
         |[8:37..8:72): (x: q.C) => x match { case _ => 1 } <= local9
         |[8:38..8:39): x <= local7
         |[8:41..8:42): q => qs39a/o.
         |[8:43..8:44): C => qs39a/o.C#
         |[8:49..8:50): x => local7
         |""".stripMargin
    else
      """|[0:8..0:13): qs39a <= qs39a/
         |[0:23..0:24): o <= qs39a/o.
         |[0:33..0:34): C <= qs39a/o.C#
         |[0:34..0:34):  <= qs39a/o.C#`<init>`().
         |[1:7..1:11): qs39 <= _empty_/qs39.
         |[2:9..2:14): qs39a => qs39a/
         |[2:16..2:17): o => qs39a/o.
         |[3:9..3:10): K <= _empty_/qs39.K.
         |[4:8..4:9): K <= _empty_/qs39.K#
         |[4:9..4:9):  <= _empty_/qs39.K#`<init>`().
         |[5:6..5:7): a <= _empty_/qs39.K#`<init>`().(a)
         |[5:6..5:7): a <= _empty_/qs39.K#a.
         |[5:9..5:12): Any => scala/Any#
         |[5:15..5:22): classOf => scala/Predef.classOf().
         |[5:23..5:24): q => qs39a/o.
         |[5:25..5:26): C => qs39a/o.C#
         |[6:6..6:7): b <= _empty_/qs39.K#`<init>`().(b)
         |[6:6..6:7): b <= _empty_/qs39.K#b.
         |[6:9..6:12): Any => scala/Any#
         |[6:15..6:19): List => scala/collection/immutable/Nil.
         |[6:20..6:21): q => qs39a/o.
         |[6:22..6:23): C => qs39a/o.C#
         |[7:6..7:7): c <= _empty_/qs39.K#`<init>`().(c)
         |[7:6..7:7): c <= _empty_/qs39.K#c.
         |[7:9..7:12): Any => scala/Any#
         |[7:15..7:22): classOf => scala/Predef.classOf().
         |[7:23..7:27): List => scala/package.List#
         |[7:28..7:29): X => local8
         |[7:46..7:47): X <= local8
         |[7:51..7:52): q => qs39a/o.
         |[7:53..7:54): C => qs39a/o.C#
         |[8:6..8:7): e <= _empty_/qs39.K#`<init>`().(e)
         |[8:6..8:7): e <= _empty_/qs39.K#e.
         |[8:9..8:24): PartialFunction => scala/PartialFunction#
         |[8:25..8:26): q => qs39a/o.
         |[8:27..8:28): C => qs39a/o.C#
         |[8:30..8:33): Int => scala/Int#
         |[8:37..8:72): (x: q.C) => x match { case _ => 1 } <= local9
         |[8:38..8:39): x <= local7
         |[8:49..8:50): x => local7
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed package object in constructor defaults, with a companion first that renames its package",
    """|package qs43a { package object qs43b { class C } }
       |object qs43 {
       |  import qs43a.qs43b.{`package` => q}
       |  object K { import qs43a.{qs43b => q} }
       |  class K(
       |      a: Any = classOf[q.C],
       |      b: Any = List[q.C](),
       |      c: Any = classOf[List[X] forSome { type X <: q.C }],
       |      e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 },
       |  )
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs43a <= qs43a/
         |[0:31..0:36): qs43b <= qs43a/qs43b/package.
         |[0:45..0:46): C <= qs43a/qs43b/package.C#
         |[0:46..0:46):  <= qs43a/qs43b/package.C#`<init>`().
         |[1:7..1:11): qs43 <= _empty_/qs43.
         |[2:9..2:14): qs43a => qs43a/
         |[2:15..2:20): qs43b => qs43a/qs43b/
         |[2:22..2:31): `package` => qs43a/qs43b/package.
         |[3:9..3:10): K <= _empty_/qs43.K.
         |[3:20..3:25): qs43a => qs43a/
         |[3:27..3:32): qs43b => qs43a/qs43b/
         |[4:8..4:9): K <= _empty_/qs43.K#
         |[4:9..4:9):  <= _empty_/qs43.K#`<init>`().
         |[5:6..5:7): a <= _empty_/qs43.K#`<init>`().(a)
         |[5:6..5:7): a <= _empty_/qs43.K#a.
         |[5:9..5:12): Any => scala/Any#
         |[5:15..5:22): classOf => scala/Predef.classOf().
         |[5:23..5:24): q => qs43a/qs43b/package.
         |[5:25..5:26): C => qs43a/qs43b/package.C#
         |[6:6..6:7): b <= _empty_/qs43.K#`<init>`().(b)
         |[6:6..6:7): b <= _empty_/qs43.K#b.
         |[6:9..6:12): Any => scala/Any#
         |[6:15..6:19): List => scala/collection/immutable/Nil.
         |[6:20..6:21): q => qs43a/qs43b/package.
         |[6:22..6:23): C => qs43a/qs43b/package.C#
         |[7:6..7:7): c <= _empty_/qs43.K#`<init>`().(c)
         |[7:6..7:7): c <= _empty_/qs43.K#c.
         |[7:9..7:12): Any => scala/Any#
         |[7:15..7:22): classOf => scala/Predef.classOf().
         |[7:23..7:27): List => scala/package.List#
         |[7:28..7:29): X => local8
         |[7:46..7:47): X <= local8
         |[7:51..7:52): q => qs43a/qs43b/package.
         |[7:53..7:54): C => qs43a/qs43b/package.C#
         |[8:6..8:7): e <= _empty_/qs43.K#`<init>`().(e)
         |[8:6..8:7): e <= _empty_/qs43.K#e.
         |[8:9..8:24): PartialFunction => scala/PartialFunction#
         |[8:25..8:26): q => qs43a/qs43b/package.
         |[8:27..8:28): C => qs43a/qs43b/package.C#
         |[8:30..8:33): Int => scala/Int#
         |[8:37..8:72): (x: q.C) => x match { case _ => 1 } <= local9
         |[8:38..8:39): x <= local7
         |[8:41..8:42): q => qs43a/qs43b/package.
         |[8:43..8:44): C => qs43a/qs43b/package.C#
         |[8:49..8:50): x => local7
         |""".stripMargin
    else
      """|[0:8..0:13): qs43a <= qs43a/
         |[0:31..0:36): qs43b <= qs43a/qs43b/package.
         |[0:45..0:46): C <= qs43a/qs43b/package.C#
         |[0:46..0:46):  <= qs43a/qs43b/package.C#`<init>`().
         |[1:7..1:11): qs43 <= _empty_/qs43.
         |[2:9..2:14): qs43a => qs43a/
         |[2:15..2:20): qs43b => qs43a/qs43b/
         |[2:22..2:31): `package` => qs43a/qs43b/package.
         |[3:9..3:10): K <= _empty_/qs43.K.
         |[3:20..3:25): qs43a => qs43a/
         |[3:27..3:32): qs43b => qs43a/qs43b/
         |[4:8..4:9): K <= _empty_/qs43.K#
         |[4:9..4:9):  <= _empty_/qs43.K#`<init>`().
         |[5:6..5:7): a <= _empty_/qs43.K#`<init>`().(a)
         |[5:6..5:7): a <= _empty_/qs43.K#a.
         |[5:9..5:12): Any => scala/Any#
         |[5:15..5:22): classOf => scala/Predef.classOf().
         |[5:23..5:24): q => qs43a/qs43b/package.
         |[5:25..5:26): C => qs43a/qs43b/package.C#
         |[6:6..6:7): b <= _empty_/qs43.K#`<init>`().(b)
         |[6:6..6:7): b <= _empty_/qs43.K#b.
         |[6:9..6:12): Any => scala/Any#
         |[6:15..6:19): List => scala/collection/immutable/Nil.
         |[6:20..6:21): q => qs43a/qs43b/package.
         |[6:22..6:23): C => qs43a/qs43b/package.C#
         |[7:6..7:7): c <= _empty_/qs43.K#`<init>`().(c)
         |[7:6..7:7): c <= _empty_/qs43.K#c.
         |[7:9..7:12): Any => scala/Any#
         |[7:15..7:22): classOf => scala/Predef.classOf().
         |[7:23..7:27): List => scala/package.List#
         |[7:28..7:29): X => local8
         |[7:46..7:47): X <= local8
         |[7:51..7:52): q => qs43a/qs43b/package.
         |[7:53..7:54): C => qs43a/qs43b/package.C#
         |[8:6..8:7): e <= _empty_/qs43.K#`<init>`().(e)
         |[8:6..8:7): e <= _empty_/qs43.K#e.
         |[8:9..8:24): PartialFunction => scala/PartialFunction#
         |[8:25..8:26): q => qs43a/qs43b/package.
         |[8:27..8:28): C => qs43a/qs43b/package.C#
         |[8:30..8:33): Int => scala/Int#
         |[8:37..8:72): (x: q.C) => x match { case _ => 1 } <= local9
         |[8:38..8:39): x <= local7
         |[8:49..8:50): x => local7
         |""".stripMargin,
  )

  occurrences(
    "a type through renamed objects in constructor defaults, with a companion first and an import between them",
    """|package qs51a { object o { class C }; object x { class C } }
       |object qs51 {
       |  import qs51a.{o => q}
       |  object i {
       |    object K
       |    import qs51a.{x => q}
       |    class K(
       |        a: Any = classOf[q.C],
       |        b: Any = List[q.C](),
       |        c: Any = classOf[List[X] forSome { type X <: q.C }],
       |        e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 },
       |    )
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs51a <= qs51a/
         |[0:23..0:24): o <= qs51a/o.
         |[0:33..0:34): C <= qs51a/o.C#
         |[0:34..0:34):  <= qs51a/o.C#`<init>`().
         |[0:45..0:46): x <= qs51a/x.
         |[0:55..0:56): C <= qs51a/x.C#
         |[0:56..0:56):  <= qs51a/x.C#`<init>`().
         |[1:7..1:11): qs51 <= _empty_/qs51.
         |[2:9..2:14): qs51a => qs51a/
         |[2:16..2:17): o => qs51a/o.
         |[3:9..3:10): i <= _empty_/qs51.i.
         |[4:11..4:12): K <= _empty_/qs51.i.K.
         |[5:11..5:16): qs51a => qs51a/
         |[5:18..5:19): x => qs51a/x.
         |[6:10..6:11): K <= _empty_/qs51.i.K#
         |[6:11..6:11):  <= _empty_/qs51.i.K#`<init>`().
         |[7:8..7:9): a <= _empty_/qs51.i.K#`<init>`().(a)
         |[7:8..7:9): a <= _empty_/qs51.i.K#a.
         |[7:11..7:14): Any => scala/Any#
         |[7:17..7:24): classOf => scala/Predef.classOf().
         |[7:25..7:26): q => qs51a/x.
         |[7:27..7:28): C => qs51a/x.C#
         |[8:8..8:9): b <= _empty_/qs51.i.K#`<init>`().(b)
         |[8:8..8:9): b <= _empty_/qs51.i.K#b.
         |[8:11..8:14): Any => scala/Any#
         |[8:17..8:21): List => scala/collection/immutable/Nil.
         |[8:22..8:23): q => qs51a/x.
         |[8:24..8:25): C => qs51a/x.C#
         |[9:8..9:9): c <= _empty_/qs51.i.K#`<init>`().(c)
         |[9:8..9:9): c <= _empty_/qs51.i.K#c.
         |[9:11..9:14): Any => scala/Any#
         |[9:17..9:24): classOf => scala/Predef.classOf().
         |[9:25..9:29): List => scala/package.List#
         |[9:30..9:31): X => local8
         |[9:48..9:49): X <= local8
         |[9:53..9:54): q => qs51a/x.
         |[9:55..9:56): C => qs51a/x.C#
         |[10:8..10:9): e <= _empty_/qs51.i.K#`<init>`().(e)
         |[10:8..10:9): e <= _empty_/qs51.i.K#e.
         |[10:11..10:26): PartialFunction => scala/PartialFunction#
         |[10:27..10:28): q => qs51a/o.
         |[10:29..10:30): C => qs51a/o.C#
         |[10:32..10:35): Int => scala/Int#
         |[10:39..10:74): (x: q.C) => x match { case _ => 1 } <= local9
         |[10:40..10:41): x <= local7
         |[10:43..10:44): q => qs51a/o.
         |[10:45..10:46): C => qs51a/o.C#
         |[10:51..10:52): x => local7
         |""".stripMargin
    else
      """|[0:8..0:13): qs51a <= qs51a/
         |[0:23..0:24): o <= qs51a/o.
         |[0:33..0:34): C <= qs51a/o.C#
         |[0:34..0:34):  <= qs51a/o.C#`<init>`().
         |[0:45..0:46): x <= qs51a/x.
         |[0:55..0:56): C <= qs51a/x.C#
         |[0:56..0:56):  <= qs51a/x.C#`<init>`().
         |[1:7..1:11): qs51 <= _empty_/qs51.
         |[2:9..2:14): qs51a => qs51a/
         |[2:16..2:17): o => qs51a/o.
         |[3:9..3:10): i <= _empty_/qs51.i.
         |[4:11..4:12): K <= _empty_/qs51.i.K.
         |[5:11..5:16): qs51a => qs51a/
         |[5:18..5:19): x => qs51a/x.
         |[6:10..6:11): K <= _empty_/qs51.i.K#
         |[6:11..6:11):  <= _empty_/qs51.i.K#`<init>`().
         |[7:8..7:9): a <= _empty_/qs51.i.K#`<init>`().(a)
         |[7:8..7:9): a <= _empty_/qs51.i.K#a.
         |[7:11..7:14): Any => scala/Any#
         |[7:17..7:24): classOf => scala/Predef.classOf().
         |[7:25..7:26): q => qs51a/x.
         |[7:27..7:28): C => qs51a/x.C#
         |[8:8..8:9): b <= _empty_/qs51.i.K#`<init>`().(b)
         |[8:8..8:9): b <= _empty_/qs51.i.K#b.
         |[8:11..8:14): Any => scala/Any#
         |[8:17..8:21): List => scala/collection/immutable/Nil.
         |[8:22..8:23): q => qs51a/x.
         |[8:24..8:25): C => qs51a/x.C#
         |[9:8..9:9): c <= _empty_/qs51.i.K#`<init>`().(c)
         |[9:8..9:9): c <= _empty_/qs51.i.K#c.
         |[9:11..9:14): Any => scala/Any#
         |[9:17..9:24): classOf => scala/Predef.classOf().
         |[9:25..9:29): List => scala/package.List#
         |[9:30..9:31): X => local8
         |[9:48..9:49): X <= local8
         |[9:53..9:54): q => qs51a/x.
         |[9:55..9:56): C => qs51a/x.C#
         |[10:8..10:9): e <= _empty_/qs51.i.K#`<init>`().(e)
         |[10:8..10:9): e <= _empty_/qs51.i.K#e.
         |[10:11..10:26): PartialFunction => scala/PartialFunction#
         |[10:27..10:28): q => qs51a/o.
         |[10:29..10:30): C => qs51a/o.C#
         |[10:32..10:35): Int => scala/Int#
         |[10:39..10:74): (x: q.C) => x match { case _ => 1 } <= local9
         |[10:40..10:41): x <= local7
         |[10:51..10:52): x => local7
         |""".stripMargin,
  )

  occurrences(
    "a type through renamed objects in constructor defaults, with a companion first, an import between them and a use of the defaults",
    """|package qs52a { object o { class C }; object x { class C } }
       |object qs52 {
       |  import qs52a.{o => q}
       |  object i {
       |    object K
       |    import qs52a.{x => q}
       |    class K(
       |        a: Any = classOf[q.C],
       |        b: Any = List[q.C](),
       |        c: Any = classOf[List[X] forSome { type X <: q.C }],
       |    )
       |    val k = new K()
       |  }
       |}
       |""".stripMargin,
    """|[0:8..0:13): qs52a <= qs52a/
       |[0:23..0:24): o <= qs52a/o.
       |[0:33..0:34): C <= qs52a/o.C#
       |[0:34..0:34):  <= qs52a/o.C#`<init>`().
       |[0:45..0:46): x <= qs52a/x.
       |[0:55..0:56): C <= qs52a/x.C#
       |[0:56..0:56):  <= qs52a/x.C#`<init>`().
       |[1:7..1:11): qs52 <= _empty_/qs52.
       |[2:9..2:14): qs52a => qs52a/
       |[2:16..2:17): o => qs52a/o.
       |[3:9..3:10): i <= _empty_/qs52.i.
       |[4:11..4:12): K <= _empty_/qs52.i.K.
       |[5:11..5:16): qs52a => qs52a/
       |[5:18..5:19): x => qs52a/x.
       |[6:10..6:11): K <= _empty_/qs52.i.K#
       |[6:11..6:11):  <= _empty_/qs52.i.K#`<init>`().
       |[7:8..7:9): a <= _empty_/qs52.i.K#`<init>`().(a)
       |[7:8..7:9): a <= _empty_/qs52.i.K#a.
       |[7:11..7:14): Any => scala/Any#
       |[7:17..7:24): classOf => scala/Predef.classOf().
       |[7:25..7:26): q => qs52a/x.
       |[7:27..7:28): C => qs52a/x.C#
       |[8:8..8:9): b <= _empty_/qs52.i.K#`<init>`().(b)
       |[8:8..8:9): b <= _empty_/qs52.i.K#b.
       |[8:11..8:14): Any => scala/Any#
       |[8:17..8:21): List => scala/collection/immutable/Nil.
       |[8:22..8:23): q => qs52a/x.
       |[8:24..8:25): C => qs52a/x.C#
       |[9:8..9:9): c <= _empty_/qs52.i.K#`<init>`().(c)
       |[9:8..9:9): c <= _empty_/qs52.i.K#c.
       |[9:11..9:14): Any => scala/Any#
       |[9:17..9:24): classOf => scala/Predef.classOf().
       |[9:25..9:29): List => scala/package.List#
       |[9:30..9:31): X => local0
       |[9:48..9:49): X <= local0
       |[9:53..9:54): q => qs52a/x.
       |[9:55..9:56): C => qs52a/x.C#
       |[11:8..11:9): k <= _empty_/qs52.i.k.
       |[11:16..11:17): K => _empty_/qs52.i.K#
       |[11:17..11:17):  => _empty_/qs52.i.K#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "a type through renames of a package object and its package in constructor defaults, with a companion first and the import of the package between them",
    """|package qs71a { package object qs71b { class C } }
       |object qs71 {
       |  import qs71a.qs71b.{`package` => q}
       |  object i {
       |    object K
       |    import qs71a.{qs71b => q}
       |    class K(
       |        a: Any = classOf[q.C],
       |        b: Any = List[q.C](),
       |        c: Any = classOf[List[X] forSome { type X <: q.C }],
       |        d: List[X] forSome { type X <: q.C } = null,
       |        e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 },
       |    )
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs71a <= qs71a/
         |[0:31..0:36): qs71b <= qs71a/qs71b/package.
         |[0:45..0:46): C <= qs71a/qs71b/package.C#
         |[0:46..0:46):  <= qs71a/qs71b/package.C#`<init>`().
         |[1:7..1:11): qs71 <= _empty_/qs71.
         |[2:9..2:14): qs71a => qs71a/
         |[2:15..2:20): qs71b => qs71a/qs71b/
         |[2:22..2:31): `package` => qs71a/qs71b/package.
         |[3:9..3:10): i <= _empty_/qs71.i.
         |[4:11..4:12): K <= _empty_/qs71.i.K.
         |[5:11..5:16): qs71a => qs71a/
         |[5:18..5:23): qs71b => qs71a/qs71b/
         |[6:10..6:11): K <= _empty_/qs71.i.K#
         |[6:11..6:11):  <= _empty_/qs71.i.K#`<init>`().
         |[7:8..7:9): a <= _empty_/qs71.i.K#`<init>`().(a)
         |[7:8..7:9): a <= _empty_/qs71.i.K#a.
         |[7:11..7:14): Any => scala/Any#
         |[7:17..7:24): classOf => scala/Predef.classOf().
         |[7:25..7:26): q => qs71a/qs71b/package.
         |[7:27..7:28): C => qs71a/qs71b/package.C#
         |[8:8..8:9): b <= _empty_/qs71.i.K#`<init>`().(b)
         |[8:8..8:9): b <= _empty_/qs71.i.K#b.
         |[8:11..8:14): Any => scala/Any#
         |[8:17..8:21): List => scala/collection/immutable/Nil.
         |[8:22..8:23): q => qs71a/qs71b/package.
         |[8:24..8:25): C => qs71a/qs71b/package.C#
         |[9:8..9:9): c <= _empty_/qs71.i.K#`<init>`().(c)
         |[9:8..9:9): c <= _empty_/qs71.i.K#c.
         |[9:11..9:14): Any => scala/Any#
         |[9:17..9:24): classOf => scala/Predef.classOf().
         |[9:25..9:29): List => scala/package.List#
         |[9:30..9:31): X => local9
         |[9:48..9:49): X <= local9
         |[9:53..9:54): q => qs71a/qs71b/package.
         |[9:55..9:56): C => qs71a/qs71b/package.C#
         |[10:8..10:9): d <= _empty_/qs71.i.K#`<init>`().(d)
         |[10:8..10:9): d <= _empty_/qs71.i.K#d.
         |[10:11..10:15): List => scala/package.List#
         |[10:16..10:17): X => local0
         |[10:34..10:35): X <= local0
         |[10:39..10:40): q => qs71a/qs71b/package.
         |[10:41..10:42): C => qs71a/qs71b/package.C#
         |[11:8..11:9): e <= _empty_/qs71.i.K#`<init>`().(e)
         |[11:8..11:9): e <= _empty_/qs71.i.K#e.
         |[11:11..11:26): PartialFunction => scala/PartialFunction#
         |[11:27..11:28): q => qs71a/qs71b/package.
         |[11:29..11:30): C => qs71a/qs71b/package.C#
         |[11:32..11:35): Int => scala/Int#
         |[11:39..11:74): (x: q.C) => x match { case _ => 1 } <= local10
         |[11:40..11:41): x <= local8
         |[11:43..11:44): q => qs71a/qs71b/package.
         |[11:45..11:46): C => qs71a/qs71b/package.C#
         |[11:51..11:52): x => local8
         |""".stripMargin
    else
      """|[0:8..0:13): qs71a <= qs71a/
         |[0:31..0:36): qs71b <= qs71a/qs71b/package.
         |[0:45..0:46): C <= qs71a/qs71b/package.C#
         |[0:46..0:46):  <= qs71a/qs71b/package.C#`<init>`().
         |[1:7..1:11): qs71 <= _empty_/qs71.
         |[2:9..2:14): qs71a => qs71a/
         |[2:15..2:20): qs71b => qs71a/qs71b/
         |[2:22..2:31): `package` => qs71a/qs71b/package.
         |[3:9..3:10): i <= _empty_/qs71.i.
         |[4:11..4:12): K <= _empty_/qs71.i.K.
         |[5:11..5:16): qs71a => qs71a/
         |[5:18..5:23): qs71b => qs71a/qs71b/
         |[6:10..6:11): K <= _empty_/qs71.i.K#
         |[6:11..6:11):  <= _empty_/qs71.i.K#`<init>`().
         |[7:8..7:9): a <= _empty_/qs71.i.K#`<init>`().(a)
         |[7:8..7:9): a <= _empty_/qs71.i.K#a.
         |[7:11..7:14): Any => scala/Any#
         |[7:17..7:24): classOf => scala/Predef.classOf().
         |[7:25..7:26): q => qs71a/qs71b/package.
         |[7:27..7:28): C => qs71a/qs71b/package.C#
         |[8:8..8:9): b <= _empty_/qs71.i.K#`<init>`().(b)
         |[8:8..8:9): b <= _empty_/qs71.i.K#b.
         |[8:11..8:14): Any => scala/Any#
         |[8:17..8:21): List => scala/collection/immutable/Nil.
         |[8:22..8:23): q => qs71a/qs71b/package.
         |[8:24..8:25): C => qs71a/qs71b/package.C#
         |[9:8..9:9): c <= _empty_/qs71.i.K#`<init>`().(c)
         |[9:8..9:9): c <= _empty_/qs71.i.K#c.
         |[9:11..9:14): Any => scala/Any#
         |[9:17..9:24): classOf => scala/Predef.classOf().
         |[9:25..9:29): List => scala/package.List#
         |[9:30..9:31): X => local9
         |[9:48..9:49): X <= local9
         |[9:53..9:54): q => qs71a/qs71b/package.
         |[9:55..9:56): C => qs71a/qs71b/package.C#
         |[10:8..10:9): d <= _empty_/qs71.i.K#`<init>`().(d)
         |[10:8..10:9): d <= _empty_/qs71.i.K#d.
         |[10:11..10:15): List => scala/package.List#
         |[10:16..10:17): X => local0
         |[10:34..10:35): X <= local0
         |[10:39..10:40): q => qs71a/qs71b/package.
         |[10:41..10:42): C => qs71a/qs71b/package.C#
         |[11:8..11:9): e <= _empty_/qs71.i.K#`<init>`().(e)
         |[11:8..11:9): e <= _empty_/qs71.i.K#e.
         |[11:11..11:26): PartialFunction => scala/PartialFunction#
         |[11:27..11:28): q => qs71a/qs71b/package.
         |[11:29..11:30): C => qs71a/qs71b/package.C#
         |[11:32..11:35): Int => scala/Int#
         |[11:39..11:74): (x: q.C) => x match { case _ => 1 } <= local10
         |[11:40..11:41): x <= local8
         |[11:51..11:52): x => local8
         |""".stripMargin,
  )

  occurrences(
    "a type through renamed objects in constructor defaults, with a synthetic companion and an import after the class",
    """|package qs53a { object o { class C }; object y { class C } }
       |object qs53 {
       |  import qs53a.{o => q}
       |  object i {
       |    class K(
       |        a: Any = classOf[q.C],
       |        b: Any = List[q.C](),
       |        c: Any = classOf[List[X] forSome { type X <: q.C }],
       |    )
       |    import qs53a.{y => q}
       |  }
       |}
       |""".stripMargin,
    """|[0:8..0:13): qs53a <= qs53a/
       |[0:23..0:24): o <= qs53a/o.
       |[0:33..0:34): C <= qs53a/o.C#
       |[0:34..0:34):  <= qs53a/o.C#`<init>`().
       |[0:45..0:46): y <= qs53a/y.
       |[0:55..0:56): C <= qs53a/y.C#
       |[0:56..0:56):  <= qs53a/y.C#`<init>`().
       |[1:7..1:11): qs53 <= _empty_/qs53.
       |[2:9..2:14): qs53a => qs53a/
       |[2:16..2:17): o => qs53a/o.
       |[3:9..3:10): i <= _empty_/qs53.i.
       |[4:10..4:11): K <= _empty_/qs53.i.K#
       |[4:11..4:11):  <= _empty_/qs53.i.K#`<init>`().
       |[5:8..5:9): a <= _empty_/qs53.i.K#`<init>`().(a)
       |[5:8..5:9): a <= _empty_/qs53.i.K#a.
       |[5:11..5:14): Any => scala/Any#
       |[5:17..5:24): classOf => scala/Predef.classOf().
       |[5:25..5:26): q => qs53a/o.
       |[5:27..5:28): C => qs53a/o.C#
       |[6:8..6:9): b <= _empty_/qs53.i.K#`<init>`().(b)
       |[6:8..6:9): b <= _empty_/qs53.i.K#b.
       |[6:11..6:14): Any => scala/Any#
       |[6:17..6:21): List => scala/collection/immutable/Nil.
       |[6:22..6:23): q => qs53a/o.
       |[6:24..6:25): C => qs53a/o.C#
       |[7:8..7:9): c <= _empty_/qs53.i.K#`<init>`().(c)
       |[7:8..7:9): c <= _empty_/qs53.i.K#c.
       |[7:11..7:14): Any => scala/Any#
       |[7:17..7:24): classOf => scala/Predef.classOf().
       |[7:25..7:29): List => scala/package.List#
       |[7:30..7:31): X => local0
       |[7:48..7:49): X <= local0
       |[7:53..7:54): q => qs53a/o.
       |[7:55..7:56): C => qs53a/o.C#
       |[9:11..9:16): qs53a => qs53a/
       |[9:18..9:19): y => qs53a/y.
       |""".stripMargin,
  )

  occurrences(
    "a type through renamed objects in constructor defaults, with a companion after the class and an import between them",
    """|package qs54a { object o { class C }; object y { class C } }
       |object qs54 {
       |  import qs54a.{o => q}
       |  object i {
       |    class K(
       |        a: Any = classOf[q.C],
       |        b: Any = List[q.C](),
       |        c: Any = classOf[List[X] forSome { type X <: q.C }],
       |        e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 },
       |    )
       |    import qs54a.{y => q}
       |    object K
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs54a <= qs54a/
         |[0:23..0:24): o <= qs54a/o.
         |[0:33..0:34): C <= qs54a/o.C#
         |[0:34..0:34):  <= qs54a/o.C#`<init>`().
         |[0:45..0:46): y <= qs54a/y.
         |[0:55..0:56): C <= qs54a/y.C#
         |[0:56..0:56):  <= qs54a/y.C#`<init>`().
         |[1:7..1:11): qs54 <= _empty_/qs54.
         |[2:9..2:14): qs54a => qs54a/
         |[2:16..2:17): o => qs54a/o.
         |[3:9..3:10): i <= _empty_/qs54.i.
         |[4:10..4:11): K <= _empty_/qs54.i.K#
         |[4:11..4:11):  <= _empty_/qs54.i.K#`<init>`().
         |[5:8..5:9): a <= _empty_/qs54.i.K#`<init>`().(a)
         |[5:8..5:9): a <= _empty_/qs54.i.K#a.
         |[5:11..5:14): Any => scala/Any#
         |[5:17..5:24): classOf => scala/Predef.classOf().
         |[5:25..5:26): q => qs54a/o.
         |[5:27..5:28): C => qs54a/o.C#
         |[6:8..6:9): b <= _empty_/qs54.i.K#`<init>`().(b)
         |[6:8..6:9): b <= _empty_/qs54.i.K#b.
         |[6:11..6:14): Any => scala/Any#
         |[6:17..6:21): List => scala/collection/immutable/Nil.
         |[6:22..6:23): q => qs54a/o.
         |[6:24..6:25): C => qs54a/o.C#
         |[7:8..7:9): c <= _empty_/qs54.i.K#`<init>`().(c)
         |[7:8..7:9): c <= _empty_/qs54.i.K#c.
         |[7:11..7:14): Any => scala/Any#
         |[7:17..7:24): classOf => scala/Predef.classOf().
         |[7:25..7:29): List => scala/package.List#
         |[7:30..7:31): X => local0
         |[7:48..7:49): X <= local0
         |[7:53..7:54): q => qs54a/o.
         |[7:55..7:56): C => qs54a/o.C#
         |[8:8..8:9): e <= _empty_/qs54.i.K#`<init>`().(e)
         |[8:8..8:9): e <= _empty_/qs54.i.K#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): q => qs54a/o.
         |[8:29..8:30): C => qs54a/o.C#
         |[8:32..8:35): Int => scala/Int#
         |[8:39..8:74): (x: q.C) => x match { case _ => 1 } <= local9
         |[8:40..8:41): x <= local8
         |[8:43..8:44): q => qs54a/o.
         |[8:45..8:46): C => qs54a/o.C#
         |[8:51..8:52): x => local8
         |[10:11..10:16): qs54a => qs54a/
         |[10:18..10:19): y => qs54a/y.
         |[11:11..11:12): K <= _empty_/qs54.i.K.
         |""".stripMargin
    else
      """|[0:8..0:13): qs54a <= qs54a/
         |[0:23..0:24): o <= qs54a/o.
         |[0:33..0:34): C <= qs54a/o.C#
         |[0:34..0:34):  <= qs54a/o.C#`<init>`().
         |[0:45..0:46): y <= qs54a/y.
         |[0:55..0:56): C <= qs54a/y.C#
         |[0:56..0:56):  <= qs54a/y.C#`<init>`().
         |[1:7..1:11): qs54 <= _empty_/qs54.
         |[2:9..2:14): qs54a => qs54a/
         |[2:16..2:17): o => qs54a/o.
         |[3:9..3:10): i <= _empty_/qs54.i.
         |[4:10..4:11): K <= _empty_/qs54.i.K#
         |[4:11..4:11):  <= _empty_/qs54.i.K#`<init>`().
         |[5:8..5:9): a <= _empty_/qs54.i.K#`<init>`().(a)
         |[5:8..5:9): a <= _empty_/qs54.i.K#a.
         |[5:11..5:14): Any => scala/Any#
         |[5:17..5:24): classOf => scala/Predef.classOf().
         |[5:25..5:26): q => qs54a/o.
         |[5:27..5:28): C => qs54a/o.C#
         |[6:8..6:9): b <= _empty_/qs54.i.K#`<init>`().(b)
         |[6:8..6:9): b <= _empty_/qs54.i.K#b.
         |[6:11..6:14): Any => scala/Any#
         |[6:17..6:21): List => scala/collection/immutable/Nil.
         |[6:22..6:23): q => qs54a/o.
         |[6:24..6:25): C => qs54a/o.C#
         |[7:8..7:9): c <= _empty_/qs54.i.K#`<init>`().(c)
         |[7:8..7:9): c <= _empty_/qs54.i.K#c.
         |[7:11..7:14): Any => scala/Any#
         |[7:17..7:24): classOf => scala/Predef.classOf().
         |[7:25..7:29): List => scala/package.List#
         |[7:30..7:31): X => local0
         |[7:48..7:49): X <= local0
         |[7:53..7:54): q => qs54a/o.
         |[7:55..7:56): C => qs54a/o.C#
         |[8:8..8:9): e <= _empty_/qs54.i.K#`<init>`().(e)
         |[8:8..8:9): e <= _empty_/qs54.i.K#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): q => qs54a/o.
         |[8:29..8:30): C => qs54a/o.C#
         |[8:32..8:35): Int => scala/Int#
         |[8:39..8:74): (x: q.C) => x match { case _ => 1 } <= local9
         |[8:40..8:41): x <= local8
         |[8:51..8:52): x => local8
         |[10:11..10:16): qs54a => qs54a/
         |[10:18..10:19): y => qs54a/y.
         |[11:11..11:12): K <= _empty_/qs54.i.K.
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed package object in secondary constructor calls, with a rename of its package in the class body",
    """|package qs44a { package object qs44b { class C } }
       |object qs44 {
       |  import qs44a.qs44b.{`package` => q}
       |  class F(z: Any) {
       |    import qs44a.{qs44b => q}
       |    def this(i: Int) = this(classOf[q.C])
       |    def this(i: Long) = this(List[q.C]())
       |    def this(i: Short) = this(classOf[List[X] forSome { type X <: q.C }])
       |    def this(i: Byte) = this(((x: q.C) => x match { case _ => 1 }): PartialFunction[q.C, Int])
       |    val a = classOf[q.C]
       |    val b = List[q.C]()
       |    val c = classOf[List[X] forSome { type X <: q.C }]
       |    val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs44a <= qs44a/
         |[0:31..0:36): qs44b <= qs44a/qs44b/package.
         |[0:45..0:46): C <= qs44a/qs44b/package.C#
         |[0:46..0:46):  <= qs44a/qs44b/package.C#`<init>`().
         |[1:7..1:11): qs44 <= _empty_/qs44.
         |[2:9..2:14): qs44a => qs44a/
         |[2:15..2:20): qs44b => qs44a/qs44b/
         |[2:22..2:31): `package` => qs44a/qs44b/package.
         |[3:8..3:9): F <= _empty_/qs44.F#
         |[3:9..3:9):  <= _empty_/qs44.F#`<init>`().
         |[3:10..3:11): z <= _empty_/qs44.F#`<init>`().(z)
         |[3:10..3:11): z <= _empty_/qs44.F#z.
         |[3:13..3:16): Any => scala/Any#
         |[4:11..4:16): qs44a => qs44a/
         |[4:18..4:23): qs44b => qs44a/qs44b/
         |[5:8..5:12): this <= _empty_/qs44.F#`<init>`(+1).
         |[5:13..5:14): i <= _empty_/qs44.F#`<init>`(+1).(i)
         |[5:16..5:19): Int => scala/Int#
         |[5:27..5:27):  => _empty_/qs44.F#`<init>`().
         |[5:28..5:35): classOf => scala/Predef.classOf().
         |[5:36..5:37): q => qs44a/qs44b/package.
         |[5:38..5:39): C => qs44a/qs44b/package.C#
         |[6:8..6:12): this <= _empty_/qs44.F#`<init>`(+2).
         |[6:13..6:14): i <= _empty_/qs44.F#`<init>`(+2).(i)
         |[6:16..6:20): Long => scala/Long#
         |[6:28..6:28):  => _empty_/qs44.F#`<init>`().
         |[6:29..6:33): List => scala/collection/immutable/Nil.
         |[6:34..6:35): q => qs44a/qs44b/package.
         |[6:36..6:37): C => qs44a/qs44b/package.C#
         |[7:8..7:12): this <= _empty_/qs44.F#`<init>`(+3).
         |[7:13..7:14): i <= _empty_/qs44.F#`<init>`(+3).(i)
         |[7:16..7:21): Short => scala/Short#
         |[7:29..7:29):  => _empty_/qs44.F#`<init>`().
         |[7:30..7:37): classOf => scala/Predef.classOf().
         |[7:38..7:42): List => scala/package.List#
         |[7:43..7:44): X => local0
         |[7:61..7:62): X <= local0
         |[7:66..7:67): q => qs44a/qs44b/package.
         |[7:68..7:69): C => qs44a/qs44b/package.C#
         |[8:8..8:12): this <= _empty_/qs44.F#`<init>`(+4).
         |[8:13..8:14): i <= _empty_/qs44.F#`<init>`(+4).(i)
         |[8:16..8:20): Byte => scala/Byte#
         |[8:28..8:28):  => _empty_/qs44.F#`<init>`().
         |[8:30..8:65): (x: q.C) => x match { case _ => 1 } <= local1
         |[8:31..8:32): x <= local8
         |[8:34..8:35): q => qs44a/qs44b/package.
         |[8:36..8:37): C => qs44a/qs44b/package.C#
         |[8:42..8:43): x => local8
         |[8:68..8:83): PartialFunction => scala/PartialFunction#
         |[8:84..8:85): q => qs44a/qs44b/package.
         |[8:86..8:87): C => qs44a/qs44b/package.C#
         |[8:89..8:92): Int => scala/Int#
         |[9:8..9:9): a <= _empty_/qs44.F#a.
         |[9:12..9:19): classOf => scala/Predef.classOf().
         |[9:20..9:21): q => qs44a/qs44b/package.
         |[9:22..9:23): C => qs44a/qs44b/package.C#
         |[10:8..10:9): b <= _empty_/qs44.F#b.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[10:17..10:18): q => qs44a/qs44b/package.
         |[10:19..10:20): C => qs44a/qs44b/package.C#
         |[11:8..11:9): c <= _empty_/qs44.F#c.
         |[11:12..11:19): classOf => scala/Predef.classOf().
         |[11:20..11:24): List => scala/package.List#
         |[11:25..11:26): X => local9
         |[11:43..11:44): X <= local9
         |[11:48..11:49): q => qs44a/qs44b/package.
         |[11:50..11:51): C => qs44a/qs44b/package.C#
         |[12:8..12:9): e <= _empty_/qs44.F#e.
         |[12:11..12:26): PartialFunction => scala/PartialFunction#
         |[12:27..12:28): q => qs44a/qs44b/
         |[12:29..12:30): C => qs44a/qs44b/package.C#
         |[12:32..12:35): Int => scala/Int#
         |[12:39..12:74): (x: q.C) => x match { case _ => 1 } <= local10
         |[12:40..12:41): x <= local17
         |[12:43..12:44): q => qs44a/qs44b/package.
         |[12:45..12:46): C => qs44a/qs44b/package.C#
         |[12:51..12:52): x => local17
         |""".stripMargin
    else
      """|[0:8..0:13): qs44a <= qs44a/
         |[0:31..0:36): qs44b <= qs44a/qs44b/package.
         |[0:45..0:46): C <= qs44a/qs44b/package.C#
         |[0:46..0:46):  <= qs44a/qs44b/package.C#`<init>`().
         |[1:7..1:11): qs44 <= _empty_/qs44.
         |[2:9..2:14): qs44a => qs44a/
         |[2:15..2:20): qs44b => qs44a/qs44b/
         |[2:22..2:31): `package` => qs44a/qs44b/package.
         |[3:8..3:9): F <= _empty_/qs44.F#
         |[3:9..3:9):  <= _empty_/qs44.F#`<init>`().
         |[3:10..3:11): z <= _empty_/qs44.F#`<init>`().(z)
         |[3:10..3:11): z <= _empty_/qs44.F#z.
         |[3:13..3:16): Any => scala/Any#
         |[4:11..4:16): qs44a => qs44a/
         |[4:18..4:23): qs44b => qs44a/qs44b/
         |[5:8..5:12): this <= _empty_/qs44.F#`<init>`(+1).
         |[5:13..5:14): i <= _empty_/qs44.F#`<init>`(+1).(i)
         |[5:16..5:19): Int => scala/Int#
         |[5:27..5:27):  => _empty_/qs44.F#`<init>`().
         |[5:28..5:35): classOf => scala/Predef.classOf().
         |[5:36..5:37): q => qs44a/qs44b/package.
         |[5:38..5:39): C => qs44a/qs44b/package.C#
         |[6:8..6:12): this <= _empty_/qs44.F#`<init>`(+2).
         |[6:13..6:14): i <= _empty_/qs44.F#`<init>`(+2).(i)
         |[6:16..6:20): Long => scala/Long#
         |[6:28..6:28):  => _empty_/qs44.F#`<init>`().
         |[6:29..6:33): List => scala/collection/immutable/Nil.
         |[6:34..6:35): q => qs44a/qs44b/package.
         |[6:36..6:37): C => qs44a/qs44b/package.C#
         |[7:8..7:12): this <= _empty_/qs44.F#`<init>`(+3).
         |[7:13..7:14): i <= _empty_/qs44.F#`<init>`(+3).(i)
         |[7:16..7:21): Short => scala/Short#
         |[7:29..7:29):  => _empty_/qs44.F#`<init>`().
         |[7:30..7:37): classOf => scala/Predef.classOf().
         |[7:38..7:42): List => scala/package.List#
         |[7:43..7:44): X => local0
         |[7:61..7:62): X <= local0
         |[7:66..7:67): q => qs44a/qs44b/package.
         |[7:68..7:69): C => qs44a/qs44b/package.C#
         |[8:8..8:12): this <= _empty_/qs44.F#`<init>`(+4).
         |[8:13..8:14): i <= _empty_/qs44.F#`<init>`(+4).(i)
         |[8:16..8:20): Byte => scala/Byte#
         |[8:28..8:28):  => _empty_/qs44.F#`<init>`().
         |[8:30..8:65): (x: q.C) => x match { case _ => 1 } <= local1
         |[8:31..8:32): x <= local8
         |[8:42..8:43): x => local8
         |[8:68..8:83): PartialFunction => scala/PartialFunction#
         |[8:84..8:85): q => qs44a/qs44b/package.
         |[8:86..8:87): C => qs44a/qs44b/package.C#
         |[8:89..8:92): Int => scala/Int#
         |[9:8..9:9): a <= _empty_/qs44.F#a.
         |[9:12..9:19): classOf => scala/Predef.classOf().
         |[9:20..9:21): q => qs44a/qs44b/package.
         |[9:22..9:23): C => qs44a/qs44b/package.C#
         |[10:8..10:9): b <= _empty_/qs44.F#b.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[10:17..10:18): q => qs44a/qs44b/package.
         |[10:19..10:20): C => qs44a/qs44b/package.C#
         |[11:8..11:9): c <= _empty_/qs44.F#c.
         |[11:12..11:19): classOf => scala/Predef.classOf().
         |[11:20..11:24): List => scala/package.List#
         |[11:25..11:26): X => local9
         |[11:43..11:44): X <= local9
         |[11:48..11:49): q => qs44a/qs44b/package.
         |[11:50..11:51): C => qs44a/qs44b/package.C#
         |[12:8..12:9): e <= _empty_/qs44.F#e.
         |[12:11..12:26): PartialFunction => scala/PartialFunction#
         |[12:27..12:28): q => qs44a/qs44b/
         |[12:29..12:30): C => qs44a/qs44b/package.C#
         |[12:32..12:35): Int => scala/Int#
         |[12:39..12:74): (x: q.C) => x match { case _ => 1 } <= local10
         |[12:40..12:41): x <= local17
         |[12:51..12:52): x => local17
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed package in secondary constructor defaults, with a rename of its package object in the class body",
    """|package qs70a { package object qs70b { class C } }
       |object qs70 {
       |  import qs70a.{qs70b => q}
       |  class F(z: Any) {
       |    import qs70a.qs70b.{`package` => q}
       |    def this(
       |        a: Any = classOf[q.C],
       |        b: Any = List[q.C](),
       |        c: Any = classOf[List[X] forSome { type X <: q.C }],
       |        d: List[X] forSome { type X <: q.C } = null,
       |        e: PartialFunction[q.C, Int] = (y: q.C) => y match { case _ => 1 },
       |    ) = this(1)
       |    val a = classOf[q.C]
       |    val b = List[q.C]()
       |    val c = classOf[List[X] forSome { type X <: q.C }]
       |    val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs70a <= qs70a/
         |[0:31..0:36): qs70b <= qs70a/qs70b/package.
         |[0:45..0:46): C <= qs70a/qs70b/package.C#
         |[0:46..0:46):  <= qs70a/qs70b/package.C#`<init>`().
         |[1:7..1:11): qs70 <= _empty_/qs70.
         |[2:9..2:14): qs70a => qs70a/
         |[2:16..2:21): qs70b => qs70a/qs70b/
         |[3:8..3:9): F <= _empty_/qs70.F#
         |[3:9..3:9):  <= _empty_/qs70.F#`<init>`().
         |[3:10..3:11): z <= _empty_/qs70.F#`<init>`().(z)
         |[3:10..3:11): z <= _empty_/qs70.F#z.
         |[3:13..3:16): Any => scala/Any#
         |[4:11..4:16): qs70a => qs70a/
         |[4:17..4:22): qs70b => qs70a/qs70b/
         |[4:24..4:33): `package` => qs70a/qs70b/package.
         |[5:8..5:12): this <= _empty_/qs70.F#`<init>`(+1).
         |[6:8..6:9): a <= _empty_/qs70.F#`<init>`(+1).(a)
         |[6:11..6:14): Any => scala/Any#
         |[6:17..6:24): classOf => scala/Predef.classOf().
         |[6:25..6:26): q => qs70a/qs70b/package.
         |[6:27..6:28): C => qs70a/qs70b/package.C#
         |[7:8..7:9): b <= _empty_/qs70.F#`<init>`(+1).(b)
         |[7:11..7:14): Any => scala/Any#
         |[7:17..7:21): List => scala/collection/immutable/Nil.
         |[7:22..7:23): q => qs70a/qs70b/package.
         |[7:24..7:25): C => qs70a/qs70b/package.C#
         |[8:8..8:9): c <= _empty_/qs70.F#`<init>`(+1).(c)
         |[8:11..8:14): Any => scala/Any#
         |[8:17..8:24): classOf => scala/Predef.classOf().
         |[8:25..8:29): List => scala/package.List#
         |[8:30..8:31): X => local0
         |[8:48..8:49): X <= local0
         |[8:53..8:54): q => qs70a/qs70b/package.
         |[8:55..8:56): C => qs70a/qs70b/package.C#
         |[9:8..9:9): d <= _empty_/qs70.F#`<init>`(+1).(d)
         |[9:11..9:15): List => scala/package.List#
         |[9:16..9:17): X => local1
         |[9:34..9:35): X <= local1
         |[9:39..9:40): q => qs70a/qs70b/package.
         |[9:41..9:42): C => qs70a/qs70b/package.C#
         |[10:8..10:9): e <= _empty_/qs70.F#`<init>`(+1).(e)
         |[10:11..10:26): PartialFunction => scala/PartialFunction#
         |[10:27..10:28): q => qs70a/qs70b/
         |[10:29..10:30): C => qs70a/qs70b/package.C#
         |[10:32..10:35): Int => scala/Int#
         |[10:39..10:74): (y: q.C) => y match { case _ => 1 } <= local19
         |[10:40..10:41): y <= local9
         |[10:43..10:44): q => qs70a/qs70b/package.
         |[10:45..10:46): C => qs70a/qs70b/package.C#
         |[10:51..10:52): y => local9
         |[11:12..11:12):  => _empty_/qs70.F#`<init>`().
         |[12:8..12:9): a <= _empty_/qs70.F#a.
         |[12:12..12:19): classOf => scala/Predef.classOf().
         |[12:20..12:21): q => qs70a/qs70b/package.
         |[12:22..12:23): C => qs70a/qs70b/package.C#
         |[13:8..13:9): b <= _empty_/qs70.F#b.
         |[13:12..13:16): List => scala/collection/immutable/Nil.
         |[13:17..13:18): q => qs70a/qs70b/package.
         |[13:19..13:20): C => qs70a/qs70b/package.C#
         |[14:8..14:9): c <= _empty_/qs70.F#c.
         |[14:12..14:19): classOf => scala/Predef.classOf().
         |[14:20..14:24): List => scala/package.List#
         |[14:25..14:26): X => local10
         |[14:43..14:44): X <= local10
         |[14:48..14:49): q => qs70a/qs70b/package.
         |[14:50..14:51): C => qs70a/qs70b/package.C#
         |[15:8..15:9): e <= _empty_/qs70.F#e.
         |[15:11..15:26): PartialFunction => scala/PartialFunction#
         |[15:27..15:28): q => qs70a/qs70b/package.
         |[15:29..15:30): C => qs70a/qs70b/package.C#
         |[15:32..15:35): Int => scala/Int#
         |[15:39..15:74): (x: q.C) => x match { case _ => 1 } <= local11
         |[15:40..15:41): x <= local18
         |[15:43..15:44): q => qs70a/qs70b/package.
         |[15:45..15:46): C => qs70a/qs70b/package.C#
         |[15:51..15:52): x => local18
         |""".stripMargin
    else
      """|[0:8..0:13): qs70a <= qs70a/
         |[0:31..0:36): qs70b <= qs70a/qs70b/package.
         |[0:45..0:46): C <= qs70a/qs70b/package.C#
         |[0:46..0:46):  <= qs70a/qs70b/package.C#`<init>`().
         |[1:7..1:11): qs70 <= _empty_/qs70.
         |[2:9..2:14): qs70a => qs70a/
         |[2:16..2:21): qs70b => qs70a/qs70b/
         |[3:8..3:9): F <= _empty_/qs70.F#
         |[3:9..3:9):  <= _empty_/qs70.F#`<init>`().
         |[3:10..3:11): z <= _empty_/qs70.F#`<init>`().(z)
         |[3:10..3:11): z <= _empty_/qs70.F#z.
         |[3:13..3:16): Any => scala/Any#
         |[4:11..4:16): qs70a => qs70a/
         |[4:17..4:22): qs70b => qs70a/qs70b/
         |[4:24..4:33): `package` => qs70a/qs70b/package.
         |[5:8..5:12): this <= _empty_/qs70.F#`<init>`(+1).
         |[6:8..6:9): a <= _empty_/qs70.F#`<init>`(+1).(a)
         |[6:11..6:14): Any => scala/Any#
         |[6:17..6:24): classOf => scala/Predef.classOf().
         |[6:25..6:26): q => qs70a/qs70b/package.
         |[6:27..6:28): C => qs70a/qs70b/package.C#
         |[7:8..7:9): b <= _empty_/qs70.F#`<init>`(+1).(b)
         |[7:11..7:14): Any => scala/Any#
         |[7:17..7:21): List => scala/collection/immutable/Nil.
         |[7:22..7:23): q => qs70a/qs70b/package.
         |[7:24..7:25): C => qs70a/qs70b/package.C#
         |[8:8..8:9): c <= _empty_/qs70.F#`<init>`(+1).(c)
         |[8:11..8:14): Any => scala/Any#
         |[8:17..8:24): classOf => scala/Predef.classOf().
         |[8:25..8:29): List => scala/package.List#
         |[8:30..8:31): X => local0
         |[8:48..8:49): X <= local0
         |[8:53..8:54): q => qs70a/qs70b/package.
         |[8:55..8:56): C => qs70a/qs70b/package.C#
         |[9:8..9:9): d <= _empty_/qs70.F#`<init>`(+1).(d)
         |[9:11..9:15): List => scala/package.List#
         |[9:16..9:17): X => local1
         |[9:34..9:35): X <= local1
         |[9:39..9:40): q => qs70a/qs70b/package.
         |[9:41..9:42): C => qs70a/qs70b/package.C#
         |[10:8..10:9): e <= _empty_/qs70.F#`<init>`(+1).(e)
         |[10:11..10:26): PartialFunction => scala/PartialFunction#
         |[10:27..10:28): q => qs70a/qs70b/
         |[10:29..10:30): C => qs70a/qs70b/package.C#
         |[10:32..10:35): Int => scala/Int#
         |[10:39..10:74): (y: q.C) => y match { case _ => 1 } <= local19
         |[10:40..10:41): y <= local9
         |[10:51..10:52): y => local9
         |[11:12..11:12):  => _empty_/qs70.F#`<init>`().
         |[12:8..12:9): a <= _empty_/qs70.F#a.
         |[12:12..12:19): classOf => scala/Predef.classOf().
         |[12:20..12:21): q => qs70a/qs70b/package.
         |[12:22..12:23): C => qs70a/qs70b/package.C#
         |[13:8..13:9): b <= _empty_/qs70.F#b.
         |[13:12..13:16): List => scala/collection/immutable/Nil.
         |[13:17..13:18): q => qs70a/qs70b/package.
         |[13:19..13:20): C => qs70a/qs70b/package.C#
         |[14:8..14:9): c <= _empty_/qs70.F#c.
         |[14:12..14:19): classOf => scala/Predef.classOf().
         |[14:20..14:24): List => scala/package.List#
         |[14:25..14:26): X => local10
         |[14:43..14:44): X <= local10
         |[14:48..14:49): q => qs70a/qs70b/package.
         |[14:50..14:51): C => qs70a/qs70b/package.C#
         |[15:8..15:9): e <= _empty_/qs70.F#e.
         |[15:11..15:26): PartialFunction => scala/PartialFunction#
         |[15:27..15:28): q => qs70a/qs70b/package.
         |[15:29..15:30): C => qs70a/qs70b/package.C#
         |[15:32..15:35): Int => scala/Int#
         |[15:39..15:74): (x: q.C) => x match { case _ => 1 } <= local11
         |[15:40..15:41): x <= local18
         |[15:51..15:52): x => local18
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed object in secondary constructor defaults, with another rename in the class body",
    """|package qs45a { object o { class C }; object x { class C } }
       |object qs45 {
       |  import qs45a.{o => q}
       |  class F(i: Int) {
       |    import qs45a.{x => q}
       |    def this(
       |        a: Any = classOf[q.C],
       |        b: Any = List[q.C](),
       |        c: Any = classOf[List[X] forSome { type X <: q.C }],
       |        e: PartialFunction[q.C, Int] = (y: q.C) => y match { case _ => 1 },
       |    ) = this(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs45a <= qs45a/
         |[0:23..0:24): o <= qs45a/o.
         |[0:33..0:34): C <= qs45a/o.C#
         |[0:34..0:34):  <= qs45a/o.C#`<init>`().
         |[0:45..0:46): x <= qs45a/x.
         |[0:55..0:56): C <= qs45a/x.C#
         |[0:56..0:56):  <= qs45a/x.C#`<init>`().
         |[1:7..1:11): qs45 <= _empty_/qs45.
         |[2:9..2:14): qs45a => qs45a/
         |[2:16..2:17): o => qs45a/o.
         |[3:8..3:9): F <= _empty_/qs45.F#
         |[3:9..3:9):  <= _empty_/qs45.F#`<init>`().
         |[3:10..3:11): i <= _empty_/qs45.F#`<init>`().(i)
         |[3:10..3:11): i <= _empty_/qs45.F#i.
         |[3:13..3:16): Int => scala/Int#
         |[4:11..4:16): qs45a => qs45a/
         |[4:18..4:19): x => qs45a/x.
         |[5:8..5:12): this <= _empty_/qs45.F#`<init>`(+1).
         |[6:8..6:9): a <= _empty_/qs45.F#`<init>`(+1).(a)
         |[6:11..6:14): Any => scala/Any#
         |[6:17..6:24): classOf => scala/Predef.classOf().
         |[6:25..6:26): q => qs45a/o.
         |[6:27..6:28): C => qs45a/o.C#
         |[7:8..7:9): b <= _empty_/qs45.F#`<init>`(+1).(b)
         |[7:11..7:14): Any => scala/Any#
         |[7:17..7:21): List => scala/collection/immutable/Nil.
         |[7:22..7:23): q => qs45a/o.
         |[7:24..7:25): C => qs45a/o.C#
         |[8:8..8:9): c <= _empty_/qs45.F#`<init>`(+1).(c)
         |[8:11..8:14): Any => scala/Any#
         |[8:17..8:24): classOf => scala/Predef.classOf().
         |[8:25..8:29): List => scala/package.List#
         |[8:30..8:31): X => local0
         |[8:48..8:49): X <= local0
         |[8:53..8:54): q => qs45a/o.
         |[8:55..8:56): C => qs45a/o.C#
         |[9:8..9:9): e <= _empty_/qs45.F#`<init>`(+1).(e)
         |[9:11..9:26): PartialFunction => scala/PartialFunction#
         |[9:27..9:28): q => qs45a/o.
         |[9:29..9:30): C => qs45a/o.C#
         |[9:32..9:35): Int => scala/Int#
         |[9:39..9:74): (y: q.C) => y match { case _ => 1 } <= local9
         |[9:40..9:41): y <= local8
         |[9:43..9:44): q => qs45a/o.
         |[9:45..9:46): C => qs45a/o.C#
         |[9:51..9:52): y => local8
         |[10:12..10:12):  => _empty_/qs45.F#`<init>`().
         |""".stripMargin
    else
      """|[0:8..0:13): qs45a <= qs45a/
         |[0:23..0:24): o <= qs45a/o.
         |[0:33..0:34): C <= qs45a/o.C#
         |[0:34..0:34):  <= qs45a/o.C#`<init>`().
         |[0:45..0:46): x <= qs45a/x.
         |[0:55..0:56): C <= qs45a/x.C#
         |[0:56..0:56):  <= qs45a/x.C#`<init>`().
         |[1:7..1:11): qs45 <= _empty_/qs45.
         |[2:9..2:14): qs45a => qs45a/
         |[2:16..2:17): o => qs45a/o.
         |[3:8..3:9): F <= _empty_/qs45.F#
         |[3:9..3:9):  <= _empty_/qs45.F#`<init>`().
         |[3:10..3:11): i <= _empty_/qs45.F#`<init>`().(i)
         |[3:10..3:11): i <= _empty_/qs45.F#i.
         |[3:13..3:16): Int => scala/Int#
         |[4:11..4:16): qs45a => qs45a/
         |[4:18..4:19): x => qs45a/x.
         |[5:8..5:12): this <= _empty_/qs45.F#`<init>`(+1).
         |[6:8..6:9): a <= _empty_/qs45.F#`<init>`(+1).(a)
         |[6:11..6:14): Any => scala/Any#
         |[6:17..6:24): classOf => scala/Predef.classOf().
         |[6:25..6:26): q => qs45a/o.
         |[6:27..6:28): C => qs45a/o.C#
         |[7:8..7:9): b <= _empty_/qs45.F#`<init>`(+1).(b)
         |[7:11..7:14): Any => scala/Any#
         |[7:17..7:21): List => scala/collection/immutable/Nil.
         |[7:22..7:23): q => qs45a/o.
         |[7:24..7:25): C => qs45a/o.C#
         |[8:8..8:9): c <= _empty_/qs45.F#`<init>`(+1).(c)
         |[8:11..8:14): Any => scala/Any#
         |[8:17..8:24): classOf => scala/Predef.classOf().
         |[8:25..8:29): List => scala/package.List#
         |[8:30..8:31): X => local0
         |[8:48..8:49): X <= local0
         |[8:53..8:54): q => qs45a/o.
         |[8:55..8:56): C => qs45a/o.C#
         |[9:8..9:9): e <= _empty_/qs45.F#`<init>`(+1).(e)
         |[9:11..9:26): PartialFunction => scala/PartialFunction#
         |[9:27..9:28): q => qs45a/o.
         |[9:29..9:30): C => qs45a/o.C#
         |[9:32..9:35): Int => scala/Int#
         |[9:39..9:74): (y: q.C) => y match { case _ => 1 } <= local9
         |[9:40..9:41): y <= local8
         |[9:51..9:52): y => local8
         |[10:12..10:12):  => _empty_/qs45.F#`<init>`().
         |""".stripMargin,
  )

  occurrences(
    "a type through renamed objects in annotations of secondary constructors, with another rename in the class body",
    """|package qs55a { object o { class C }; object x { class C } }
       |class qs55n(a: Any) extends scala.annotation.StaticAnnotation
       |object qs55 {
       |  import qs55a.{o => q}
       |  class F(s: String) {
       |    import qs55a.{x => q}
       |    @qs55n(classOf[q.C]) def this() = this("")
       |    @qs55n(List[q.C]()) def this(i: Int) = this("")
       |    @qs55n(classOf[List[X] forSome { type X <: q.C }]) def this(i: Long) = this("")
       |    @qs55n(((x: q.C) => x match { case _ => 1 }): PartialFunction[q.C, Int])
       |    def this(i: Short) = this("")
       |    def this(@qs55n(classOf[q.C]) i: Char) = this("")
       |    def this(@qs55n(List[q.C]()) i: Float) = this("")
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs55a <= qs55a/
         |[0:23..0:24): o <= qs55a/o.
         |[0:33..0:34): C <= qs55a/o.C#
         |[0:34..0:34):  <= qs55a/o.C#`<init>`().
         |[0:45..0:46): x <= qs55a/x.
         |[0:55..0:56): C <= qs55a/x.C#
         |[0:56..0:56):  <= qs55a/x.C#`<init>`().
         |[1:6..1:11): qs55n <= _empty_/qs55n#
         |[1:11..1:11):  <= _empty_/qs55n#`<init>`().
         |[1:12..1:13): a <= _empty_/qs55n#`<init>`().(a)
         |[1:12..1:13): a <= _empty_/qs55n#a.
         |[1:15..1:18): Any => scala/Any#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[2:7..2:11): qs55 <= _empty_/qs55.
         |[3:9..3:14): qs55a => qs55a/
         |[3:16..3:17): o => qs55a/o.
         |[4:8..4:9): F <= _empty_/qs55.F#
         |[4:9..4:9):  <= _empty_/qs55.F#`<init>`().
         |[4:10..4:11): s <= _empty_/qs55.F#`<init>`().(s)
         |[4:10..4:11): s <= _empty_/qs55.F#s.
         |[4:13..4:19): String => scala/Predef.String#
         |[5:11..5:16): qs55a => qs55a/
         |[5:18..5:19): x => qs55a/x.
         |[6:5..6:10): qs55n => _empty_/qs55n#
         |[6:10..6:10):  => _empty_/qs55n#`<init>`().
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:19..6:20): q => qs55a/x.
         |[6:21..6:22): C => qs55a/x.C#
         |[6:42..6:42):  => _empty_/qs55.F#`<init>`().
         |[7:5..7:10): qs55n => _empty_/qs55n#
         |[7:10..7:10):  => _empty_/qs55n#`<init>`().
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:17): q => qs55a/x.
         |[7:18..7:19): C => qs55a/x.C#
         |[7:33..7:34): i <= _empty_/qs55.F#`<init>`(+2).(i)
         |[7:36..7:39): Int => scala/Int#
         |[7:47..7:47):  => _empty_/qs55.F#`<init>`().
         |[8:5..8:10): qs55n => _empty_/qs55n#
         |[8:10..8:10):  => _empty_/qs55n#`<init>`().
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:19..8:23): List => scala/package.List#
         |[8:24..8:25): X => local0
         |[8:42..8:43): X <= local0
         |[8:47..8:48): q => qs55a/x.
         |[8:49..8:50): C => qs55a/x.C#
         |[8:64..8:65): i <= _empty_/qs55.F#`<init>`(+3).(i)
         |[8:67..8:71): Long => scala/Long#
         |[8:79..8:79):  => _empty_/qs55.F#`<init>`().
         |[9:5..9:10): qs55n => _empty_/qs55n#
         |[9:10..9:10):  => _empty_/qs55n#`<init>`().
         |[9:12..9:47): (x: q.C) => x match { case _ => 1 } <= local1
         |[9:13..9:14): x <= local8
         |[9:16..9:17): q => qs55a/x.
         |[9:18..9:19): C => qs55a/x.C#
         |[9:24..9:25): x => local8
         |[9:50..9:65): PartialFunction => scala/PartialFunction#
         |[9:66..9:67): q => qs55a/x.
         |[9:68..9:69): C => qs55a/x.C#
         |[9:71..9:74): Int => scala/Int#
         |[10:13..10:14): i <= _empty_/qs55.F#`<init>`(+4).(i)
         |[10:16..10:21): Short => scala/Short#
         |[10:29..10:29):  => _empty_/qs55.F#`<init>`().
         |[11:8..11:12): this <= _empty_/qs55.F#`<init>`(+5).
         |[11:14..11:19): qs55n => _empty_/qs55n#
         |[11:19..11:19):  => _empty_/qs55n#`<init>`().
         |[11:20..11:27): classOf => scala/Predef.classOf().
         |[11:28..11:29): q => qs55a/x.
         |[11:30..11:31): C => qs55a/x.C#
         |[11:34..11:35): i <= _empty_/qs55.F#`<init>`(+5).(i)
         |[11:37..11:41): Char => scala/Char#
         |[11:49..11:49):  => _empty_/qs55.F#`<init>`().
         |[12:8..12:12): this <= _empty_/qs55.F#`<init>`(+6).
         |[12:14..12:19): qs55n => _empty_/qs55n#
         |[12:19..12:19):  => _empty_/qs55n#`<init>`().
         |[12:20..12:24): List => scala/collection/immutable/Nil.
         |[12:25..12:26): q => qs55a/x.
         |[12:27..12:28): C => qs55a/x.C#
         |[12:33..12:34): i <= _empty_/qs55.F#`<init>`(+6).(i)
         |[12:36..12:41): Float => scala/Float#
         |[12:49..12:49):  => _empty_/qs55.F#`<init>`().
         |""".stripMargin
    else
      """|[0:8..0:13): qs55a <= qs55a/
         |[0:23..0:24): o <= qs55a/o.
         |[0:33..0:34): C <= qs55a/o.C#
         |[0:34..0:34):  <= qs55a/o.C#`<init>`().
         |[0:45..0:46): x <= qs55a/x.
         |[0:55..0:56): C <= qs55a/x.C#
         |[0:56..0:56):  <= qs55a/x.C#`<init>`().
         |[1:6..1:11): qs55n <= _empty_/qs55n#
         |[1:11..1:11):  <= _empty_/qs55n#`<init>`().
         |[1:12..1:13): a <= _empty_/qs55n#`<init>`().(a)
         |[1:12..1:13): a <= _empty_/qs55n#a.
         |[1:15..1:18): Any => scala/Any#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[2:7..2:11): qs55 <= _empty_/qs55.
         |[3:9..3:14): qs55a => qs55a/
         |[3:16..3:17): o => qs55a/o.
         |[4:8..4:9): F <= _empty_/qs55.F#
         |[4:9..4:9):  <= _empty_/qs55.F#`<init>`().
         |[4:10..4:11): s <= _empty_/qs55.F#`<init>`().(s)
         |[4:10..4:11): s <= _empty_/qs55.F#s.
         |[4:13..4:19): String => scala/Predef.String#
         |[5:11..5:16): qs55a => qs55a/
         |[5:18..5:19): x => qs55a/x.
         |[6:5..6:10): qs55n => _empty_/qs55n#
         |[6:10..6:10):  => _empty_/qs55n#`<init>`().
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:19..6:20): q => qs55a/x.
         |[6:21..6:22): C => qs55a/x.C#
         |[6:42..6:42):  => _empty_/qs55.F#`<init>`().
         |[7:5..7:10): qs55n => _empty_/qs55n#
         |[7:10..7:10):  => _empty_/qs55n#`<init>`().
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:17): q => qs55a/x.
         |[7:18..7:19): C => qs55a/x.C#
         |[7:33..7:34): i <= _empty_/qs55.F#`<init>`(+2).(i)
         |[7:36..7:39): Int => scala/Int#
         |[7:47..7:47):  => _empty_/qs55.F#`<init>`().
         |[8:5..8:10): qs55n => _empty_/qs55n#
         |[8:10..8:10):  => _empty_/qs55n#`<init>`().
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:19..8:23): List => scala/package.List#
         |[8:24..8:25): X => local0
         |[8:42..8:43): X <= local0
         |[8:47..8:48): q => qs55a/x.
         |[8:49..8:50): C => qs55a/x.C#
         |[8:64..8:65): i <= _empty_/qs55.F#`<init>`(+3).(i)
         |[8:67..8:71): Long => scala/Long#
         |[8:79..8:79):  => _empty_/qs55.F#`<init>`().
         |[9:5..9:10): qs55n => _empty_/qs55n#
         |[9:10..9:10):  => _empty_/qs55n#`<init>`().
         |[9:12..9:47): (x: q.C) => x match { case _ => 1 } <= local1
         |[9:13..9:14): x <= local8
         |[9:24..9:25): x => local8
         |[9:50..9:65): PartialFunction => scala/PartialFunction#
         |[9:66..9:67): q => qs55a/x.
         |[9:68..9:69): C => qs55a/x.C#
         |[9:71..9:74): Int => scala/Int#
         |[10:13..10:14): i <= _empty_/qs55.F#`<init>`(+4).(i)
         |[10:16..10:21): Short => scala/Short#
         |[10:29..10:29):  => _empty_/qs55.F#`<init>`().
         |[11:8..11:12): this <= _empty_/qs55.F#`<init>`(+5).
         |[11:14..11:19): qs55n => _empty_/qs55n#
         |[11:19..11:19):  => _empty_/qs55n#`<init>`().
         |[11:20..11:27): classOf => scala/Predef.classOf().
         |[11:28..11:29): q => qs55a/x.
         |[11:30..11:31): C => qs55a/x.C#
         |[11:34..11:35): i <= _empty_/qs55.F#`<init>`(+5).(i)
         |[11:37..11:41): Char => scala/Char#
         |[11:49..11:49):  => _empty_/qs55.F#`<init>`().
         |[12:8..12:12): this <= _empty_/qs55.F#`<init>`(+6).
         |[12:14..12:19): qs55n => _empty_/qs55n#
         |[12:19..12:19):  => _empty_/qs55n#`<init>`().
         |[12:20..12:24): List => scala/collection/immutable/Nil.
         |[12:25..12:26): q => qs55a/x.
         |[12:27..12:28): C => qs55a/x.C#
         |[12:33..12:34): i <= _empty_/qs55.F#`<init>`(+6).(i)
         |[12:36..12:41): Float => scala/Float#
         |[12:49..12:49):  => _empty_/qs55.F#`<init>`().
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed package object, with an import of its package by that name",
    """|package qs46x { package object q { class C } }
       |object qs46 {
       |  import qs46x.q.{`package` => q}
       |  object i {
       |    import qs46x.q
       |    val a = classOf[q.C]
       |    val b = List[q.C]()
       |    val c = classOf[List[X] forSome { type X <: q.C }]
       |    val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs46x <= qs46x/
         |[0:31..0:32): q <= qs46x/q/package.
         |[0:41..0:42): C <= qs46x/q/package.C#
         |[0:42..0:42):  <= qs46x/q/package.C#`<init>`().
         |[1:7..1:11): qs46 <= _empty_/qs46.
         |[2:9..2:14): qs46x => qs46x/
         |[2:15..2:16): q => qs46x/q/
         |[2:18..2:27): `package` => qs46x/q/package.
         |[3:9..3:10): i <= _empty_/qs46.i.
         |[4:11..4:16): qs46x => qs46x/
         |[4:17..4:18): q => qs46x/q/
         |[5:8..5:9): a <= _empty_/qs46.i.a.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:21): q => qs46x/q/package.
         |[5:22..5:23): C => qs46x/q/package.C#
         |[6:8..6:9): b <= _empty_/qs46.i.b.
         |[6:12..6:16): List => scala/collection/immutable/Nil.
         |[6:17..6:18): q => qs46x/q/package.
         |[6:19..6:20): C => qs46x/q/package.C#
         |[7:8..7:9): c <= _empty_/qs46.i.c.
         |[7:12..7:19): classOf => scala/Predef.classOf().
         |[7:20..7:24): List => scala/package.List#
         |[7:25..7:26): X => local0
         |[7:43..7:44): X <= local0
         |[7:48..7:49): q => qs46x/q/package.
         |[7:50..7:51): C => qs46x/q/package.C#
         |[8:8..8:9): e <= _empty_/qs46.i.e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): q => qs46x/q/
         |[8:29..8:30): C => qs46x/q/package.C#
         |[8:32..8:35): Int => scala/Int#
         |[8:39..8:74): (x: q.C) => x match { case _ => 1 } <= local1
         |[8:40..8:41): x <= local8
         |[8:43..8:44): q => qs46x/q/package.
         |[8:45..8:46): C => qs46x/q/package.C#
         |[8:51..8:52): x => local8
         |""".stripMargin
    else
      """|[0:8..0:13): qs46x <= qs46x/
         |[0:31..0:32): q <= qs46x/q/package.
         |[0:41..0:42): C <= qs46x/q/package.C#
         |[0:42..0:42):  <= qs46x/q/package.C#`<init>`().
         |[1:7..1:11): qs46 <= _empty_/qs46.
         |[2:9..2:14): qs46x => qs46x/
         |[2:15..2:16): q => qs46x/q/
         |[2:18..2:27): `package` => qs46x/q/package.
         |[3:9..3:10): i <= _empty_/qs46.i.
         |[4:11..4:16): qs46x => qs46x/
         |[4:17..4:18): q => qs46x/q/
         |[5:8..5:9): a <= _empty_/qs46.i.a.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:21): q => qs46x/q/package.
         |[5:22..5:23): C => qs46x/q/package.C#
         |[6:8..6:9): b <= _empty_/qs46.i.b.
         |[6:12..6:16): List => scala/collection/immutable/Nil.
         |[6:17..6:18): q => qs46x/q/package.
         |[6:19..6:20): C => qs46x/q/package.C#
         |[7:8..7:9): c <= _empty_/qs46.i.c.
         |[7:12..7:19): classOf => scala/Predef.classOf().
         |[7:20..7:24): List => scala/package.List#
         |[7:25..7:26): X => local0
         |[7:43..7:44): X <= local0
         |[7:48..7:49): q => qs46x/q/package.
         |[7:50..7:51): C => qs46x/q/package.C#
         |[8:8..8:9): e <= _empty_/qs46.i.e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): q => qs46x/q/
         |[8:29..8:30): C => qs46x/q/package.C#
         |[8:32..8:35): Int => scala/Int#
         |[8:39..8:74): (x: q.C) => x match { case _ => 1 } <= local1
         |[8:40..8:41): x <= local8
         |[8:51..8:52): x => local8
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed object, with a renamed class of that name",
    """|package qs47a { object o { class C }; class T { class C } }
       |object qs47 {
       |  import qs47a.{o => q}
       |  object i {
       |    import qs47a.{T => q}
       |    val a = classOf[q.C]
       |    val b = List[q.C]()
       |    val c = classOf[List[X] forSome { type X <: q.C }]
       |    val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs47a <= qs47a/
         |[0:23..0:24): o <= qs47a/o.
         |[0:33..0:34): C <= qs47a/o.C#
         |[0:34..0:34):  <= qs47a/o.C#`<init>`().
         |[0:44..0:45): T <= qs47a/T#
         |[0:46..0:46):  <= qs47a/T#`<init>`().
         |[0:54..0:55): C <= qs47a/T#C#
         |[0:55..0:55):  <= qs47a/T#C#`<init>`().
         |[1:7..1:11): qs47 <= _empty_/qs47.
         |[2:9..2:14): qs47a => qs47a/
         |[2:16..2:17): o => qs47a/o.
         |[3:9..3:10): i <= _empty_/qs47.i.
         |[4:11..4:16): qs47a => qs47a/
         |[4:18..4:19): T => qs47a/T#
         |[5:8..5:9): a <= _empty_/qs47.i.a.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:21): q => qs47a/o.
         |[5:22..5:23): C => qs47a/o.C#
         |[6:8..6:9): b <= _empty_/qs47.i.b.
         |[6:12..6:16): List => scala/collection/immutable/Nil.
         |[6:17..6:18): q => qs47a/o.
         |[6:19..6:20): C => qs47a/o.C#
         |[7:8..7:9): c <= _empty_/qs47.i.c.
         |[7:12..7:19): classOf => scala/Predef.classOf().
         |[7:20..7:24): List => scala/package.List#
         |[7:25..7:26): X => local0
         |[7:43..7:44): X <= local0
         |[7:48..7:49): q => qs47a/o.
         |[7:50..7:51): C => qs47a/o.C#
         |[8:8..8:9): e <= _empty_/qs47.i.e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): q => qs47a/o.
         |[8:29..8:30): C => qs47a/o.C#
         |[8:32..8:35): Int => scala/Int#
         |[8:39..8:74): (x: q.C) => x match { case _ => 1 } <= local1
         |[8:40..8:41): x <= local8
         |[8:43..8:44): q => qs47a/o.
         |[8:45..8:46): C => qs47a/o.C#
         |[8:51..8:52): x => local8
         |""".stripMargin
    else
      """|[0:8..0:13): qs47a <= qs47a/
         |[0:23..0:24): o <= qs47a/o.
         |[0:33..0:34): C <= qs47a/o.C#
         |[0:34..0:34):  <= qs47a/o.C#`<init>`().
         |[0:44..0:45): T <= qs47a/T#
         |[0:46..0:46):  <= qs47a/T#`<init>`().
         |[0:54..0:55): C <= qs47a/T#C#
         |[0:55..0:55):  <= qs47a/T#C#`<init>`().
         |[1:7..1:11): qs47 <= _empty_/qs47.
         |[2:9..2:14): qs47a => qs47a/
         |[2:16..2:17): o => qs47a/o.
         |[3:9..3:10): i <= _empty_/qs47.i.
         |[4:11..4:16): qs47a => qs47a/
         |[4:18..4:19): T => qs47a/T#
         |[5:8..5:9): a <= _empty_/qs47.i.a.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:21): q => qs47a/o.
         |[5:22..5:23): C => qs47a/o.C#
         |[6:8..6:9): b <= _empty_/qs47.i.b.
         |[6:12..6:16): List => scala/collection/immutable/Nil.
         |[6:17..6:18): q => qs47a/o.
         |[6:19..6:20): C => qs47a/o.C#
         |[7:8..7:9): c <= _empty_/qs47.i.c.
         |[7:12..7:19): classOf => scala/Predef.classOf().
         |[7:20..7:24): List => scala/package.List#
         |[7:25..7:26): X => local0
         |[7:43..7:44): X <= local0
         |[7:48..7:49): q => qs47a/o.
         |[7:50..7:51): C => qs47a/o.C#
         |[8:8..8:9): e <= _empty_/qs47.i.e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): q => qs47a/o.
         |[8:29..8:30): C => qs47a/o.C#
         |[8:32..8:35): Int => scala/Int#
         |[8:39..8:74): (x: q.C) => x match { case _ => 1 } <= local1
         |[8:40..8:41): x <= local8
         |[8:51..8:52): x => local8
         |""".stripMargin,
  )

  occurrences(
    "renames outside the qualifiers of matched types",
    """|object qs79 {
       |  object o { class C; val x = 1 }
       |  import o.{C => D}
       |  import qs79.{o => q}
       |  val a: q.C = null
       |  val b: D = new D
       |  val c = classOf[D]
       |  val d = List[D]()
       |  val e = q.x
       |  val f: PartialFunction[q.C, Int] = null
       |}
       |""".stripMargin,
    """|[0:7..0:11): qs79 <= _empty_/qs79.
       |[1:9..1:10): o <= _empty_/qs79.o.
       |[1:19..1:20): C <= _empty_/qs79.o.C#
       |[1:20..1:20):  <= _empty_/qs79.o.C#`<init>`().
       |[1:26..1:27): x <= _empty_/qs79.o.x.
       |[2:9..2:10): o => _empty_/qs79.o.
       |[2:12..2:13): C => _empty_/qs79.o.C#
       |[3:9..3:13): qs79 => _empty_/qs79.
       |[3:15..3:16): o => _empty_/qs79.o.
       |[4:6..4:7): a <= _empty_/qs79.a.
       |[4:9..4:10): q => _empty_/qs79.o.
       |[4:11..4:12): C => _empty_/qs79.o.C#
       |[5:6..5:7): b <= _empty_/qs79.b.
       |[5:9..5:10): D => _empty_/qs79.o.C#
       |[5:17..5:18): D => _empty_/qs79.o.C#
       |[5:18..5:18):  => _empty_/qs79.o.C#`<init>`().
       |[6:6..6:7): c <= _empty_/qs79.c.
       |[6:10..6:17): classOf => scala/Predef.classOf().
       |[6:18..6:19): D => _empty_/qs79.o.C#
       |[7:6..7:7): d <= _empty_/qs79.d.
       |[7:10..7:14): List => scala/collection/immutable/Nil.
       |[7:15..7:16): D => _empty_/qs79.o.C#
       |[8:6..8:7): e <= _empty_/qs79.e.
       |[8:10..8:11): q => _empty_/qs79.o.
       |[8:12..8:13): x => _empty_/qs79.o.x.
       |[9:6..9:7): f <= _empty_/qs79.f.
       |[9:9..9:24): PartialFunction => scala/PartialFunction#
       |[9:25..9:26): q => _empty_/qs79.o.
       |[9:27..9:28): C => _empty_/qs79.o.C#
       |[9:30..9:33): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "a type through an explicit import and a wildcard import of one name in one statement list",
    """|package qs56a { object o { class C }; object x { object q { class C } } }
       |object qs56 {
       |  object i {
       |    import qs56a.{o => q}
       |    import qs56a.x._
       |    val a = classOf[q.C]
       |    val b = List[q.C]()
       |    val c = classOf[List[X] forSome { type X <: q.C }]
       |    val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |  }
       |  object j {
       |    import qs56a.x._
       |    import qs56a.{o => q}
       |    val a = classOf[q.C]
       |    val b = List[q.C]()
       |    val c = classOf[List[X] forSome { type X <: q.C }]
       |    val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs56a <= qs56a/
         |[0:23..0:24): o <= qs56a/o.
         |[0:33..0:34): C <= qs56a/o.C#
         |[0:34..0:34):  <= qs56a/o.C#`<init>`().
         |[0:45..0:46): x <= qs56a/x.
         |[0:56..0:57): q <= qs56a/x.q.
         |[0:66..0:67): C <= qs56a/x.q.C#
         |[0:67..0:67):  <= qs56a/x.q.C#`<init>`().
         |[1:7..1:11): qs56 <= _empty_/qs56.
         |[2:9..2:10): i <= _empty_/qs56.i.
         |[3:11..3:16): qs56a => qs56a/
         |[3:18..3:19): o => qs56a/o.
         |[4:11..4:16): qs56a => qs56a/
         |[4:17..4:18): x => qs56a/x.
         |[5:8..5:9): a <= _empty_/qs56.i.a.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:21): q => qs56a/o.
         |[5:22..5:23): C => qs56a/o.C#
         |[6:8..6:9): b <= _empty_/qs56.i.b.
         |[6:12..6:16): List => scala/collection/immutable/Nil.
         |[6:17..6:18): q => qs56a/o.
         |[6:19..6:20): C => qs56a/o.C#
         |[7:8..7:9): c <= _empty_/qs56.i.c.
         |[7:12..7:19): classOf => scala/Predef.classOf().
         |[7:20..7:24): List => scala/package.List#
         |[7:25..7:26): X => local0
         |[7:43..7:44): X <= local0
         |[7:48..7:49): q => qs56a/o.
         |[7:50..7:51): C => qs56a/o.C#
         |[8:8..8:9): e <= _empty_/qs56.i.e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): q => qs56a/o.
         |[8:29..8:30): C => qs56a/o.C#
         |[8:32..8:35): Int => scala/Int#
         |[8:39..8:74): (x: q.C) => x match { case _ => 1 } <= local1
         |[8:40..8:41): x <= local8
         |[8:43..8:44): q => qs56a/o.
         |[8:45..8:46): C => qs56a/o.C#
         |[8:51..8:52): x => local8
         |[10:9..10:10): j <= _empty_/qs56.j.
         |[11:11..11:16): qs56a => qs56a/
         |[11:17..11:18): x => qs56a/x.
         |[12:11..12:16): qs56a => qs56a/
         |[12:18..12:19): o => qs56a/o.
         |[13:8..13:9): a <= _empty_/qs56.j.a.
         |[13:12..13:19): classOf => scala/Predef.classOf().
         |[13:20..13:21): q => qs56a/o.
         |[13:22..13:23): C => qs56a/o.C#
         |[14:8..14:9): b <= _empty_/qs56.j.b.
         |[14:12..14:16): List => scala/collection/immutable/Nil.
         |[14:17..14:18): q => qs56a/o.
         |[14:19..14:20): C => qs56a/o.C#
         |[15:8..15:9): c <= _empty_/qs56.j.c.
         |[15:12..15:19): classOf => scala/Predef.classOf().
         |[15:20..15:24): List => scala/package.List#
         |[15:25..15:26): X => local9
         |[15:43..15:44): X <= local9
         |[15:48..15:49): q => qs56a/o.
         |[15:50..15:51): C => qs56a/o.C#
         |[16:8..16:9): e <= _empty_/qs56.j.e.
         |[16:11..16:26): PartialFunction => scala/PartialFunction#
         |[16:27..16:28): q => qs56a/o.
         |[16:29..16:30): C => qs56a/o.C#
         |[16:32..16:35): Int => scala/Int#
         |[16:39..16:74): (x: q.C) => x match { case _ => 1 } <= local10
         |[16:40..16:41): x <= local17
         |[16:43..16:44): q => qs56a/o.
         |[16:45..16:46): C => qs56a/o.C#
         |[16:51..16:52): x => local17
         |""".stripMargin
    else
      """|[0:8..0:13): qs56a <= qs56a/
         |[0:23..0:24): o <= qs56a/o.
         |[0:33..0:34): C <= qs56a/o.C#
         |[0:34..0:34):  <= qs56a/o.C#`<init>`().
         |[0:45..0:46): x <= qs56a/x.
         |[0:56..0:57): q <= qs56a/x.q.
         |[0:66..0:67): C <= qs56a/x.q.C#
         |[0:67..0:67):  <= qs56a/x.q.C#`<init>`().
         |[1:7..1:11): qs56 <= _empty_/qs56.
         |[2:9..2:10): i <= _empty_/qs56.i.
         |[3:11..3:16): qs56a => qs56a/
         |[3:18..3:19): o => qs56a/o.
         |[4:11..4:16): qs56a => qs56a/
         |[4:17..4:18): x => qs56a/x.
         |[5:8..5:9): a <= _empty_/qs56.i.a.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:21): q => qs56a/o.
         |[5:22..5:23): C => qs56a/o.C#
         |[6:8..6:9): b <= _empty_/qs56.i.b.
         |[6:12..6:16): List => scala/collection/immutable/Nil.
         |[6:17..6:18): q => qs56a/o.
         |[6:19..6:20): C => qs56a/o.C#
         |[7:8..7:9): c <= _empty_/qs56.i.c.
         |[7:12..7:19): classOf => scala/Predef.classOf().
         |[7:20..7:24): List => scala/package.List#
         |[7:25..7:26): X => local0
         |[7:43..7:44): X <= local0
         |[7:48..7:49): q => qs56a/o.
         |[7:50..7:51): C => qs56a/o.C#
         |[8:8..8:9): e <= _empty_/qs56.i.e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): q => qs56a/o.
         |[8:29..8:30): C => qs56a/o.C#
         |[8:32..8:35): Int => scala/Int#
         |[8:39..8:74): (x: q.C) => x match { case _ => 1 } <= local1
         |[8:40..8:41): x <= local8
         |[8:51..8:52): x => local8
         |[10:9..10:10): j <= _empty_/qs56.j.
         |[11:11..11:16): qs56a => qs56a/
         |[11:17..11:18): x => qs56a/x.
         |[12:11..12:16): qs56a => qs56a/
         |[12:18..12:19): o => qs56a/o.
         |[13:8..13:9): a <= _empty_/qs56.j.a.
         |[13:12..13:19): classOf => scala/Predef.classOf().
         |[13:20..13:21): q => qs56a/o.
         |[13:22..13:23): C => qs56a/o.C#
         |[14:8..14:9): b <= _empty_/qs56.j.b.
         |[14:12..14:16): List => scala/collection/immutable/Nil.
         |[14:17..14:18): q => qs56a/o.
         |[14:19..14:20): C => qs56a/o.C#
         |[15:8..15:9): c <= _empty_/qs56.j.c.
         |[15:12..15:19): classOf => scala/Predef.classOf().
         |[15:20..15:24): List => scala/package.List#
         |[15:25..15:26): X => local9
         |[15:43..15:44): X <= local9
         |[15:48..15:49): q => qs56a/o.
         |[15:50..15:51): C => qs56a/o.C#
         |[16:8..16:9): e <= _empty_/qs56.j.e.
         |[16:11..16:26): PartialFunction => scala/PartialFunction#
         |[16:27..16:28): q => qs56a/o.
         |[16:29..16:30): C => qs56a/o.C#
         |[16:32..16:35): Int => scala/Int#
         |[16:39..16:74): (x: q.C) => x match { case _ => 1 } <= local10
         |[16:40..16:41): x <= local17
         |[16:51..16:52): x => local17
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed object, with an inner import of an inaccessible object of that name",
    """|package qs59a {
       |  object o { class C; trait Tr; class P[A](a: A); trait TC[A] }
       |  object x {
       |    private object q { class C; trait Tr; class P[A](a: A); trait TC[A] }
       |    private object p { class C; trait Tr; class P[A](a: A); trait TC[A] }
       |  }
       |}
       |object qs59 {
       |  import qs59a.{o => q}
       |  object i {
       |    import qs59a.x.q
       |    val a = classOf[q.C]
       |    val b = List[q.C]()
       |    val c = classOf[List[X] forSome { type X <: q.C }]
       |    val d: List[X] forSome { type X <: q.C } = null
       |    val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |    val f = List[AnyRef { def m: q.C }]()
       |    val g = List[q.type]()
       |    class A { self: q.Tr => }
       |    class B extends q.P(1)
       |    val h = List[AnyRef { def m[A: q.TC]: Int }]()
       |  }
       |  object j {
       |    import qs59a.x.{p => q}
       |    val a = classOf[q.C]
       |    val b = List[q.C]()
       |    val c = classOf[List[X] forSome { type X <: q.C }]
       |    val d: List[X] forSome { type X <: q.C } = null
       |    val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |    val f = List[AnyRef { def m: q.C }]()
       |    val g = List[q.type]()
       |    class A { self: q.Tr => }
       |    class B extends q.P(1)
       |    val h = List[AnyRef { def m[A: q.TC]: Int }]()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs59a <= qs59a/
         |[1:9..1:10): o <= qs59a/o.
         |[1:19..1:20): C <= qs59a/o.C#
         |[1:20..1:20):  <= qs59a/o.C#`<init>`().
         |[1:28..1:30): Tr <= qs59a/o.Tr#
         |[1:38..1:39): P <= qs59a/o.P#
         |[1:40..1:41): A <= qs59a/o.P#[A]
         |[1:42..1:42):  <= qs59a/o.P#`<init>`().
         |[1:43..1:44): a <= qs59a/o.P#`<init>`().(a)
         |[1:43..1:44): a <= qs59a/o.P#a.
         |[1:46..1:47): A => qs59a/o.P#[A]
         |[1:56..1:58): TC <= qs59a/o.TC#
         |[1:59..1:60): A <= qs59a/o.TC#[A]
         |[2:9..2:10): x <= qs59a/x.
         |[3:19..3:20): q <= qs59a/x.q.
         |[3:29..3:30): C <= qs59a/x.q.C#
         |[3:30..3:30):  <= qs59a/x.q.C#`<init>`().
         |[3:38..3:40): Tr <= qs59a/x.q.Tr#
         |[3:48..3:49): P <= qs59a/x.q.P#
         |[3:50..3:51): A <= qs59a/x.q.P#[A]
         |[3:52..3:52):  <= qs59a/x.q.P#`<init>`().
         |[3:53..3:54): a <= qs59a/x.q.P#`<init>`().(a)
         |[3:53..3:54): a <= qs59a/x.q.P#a.
         |[3:56..3:57): A => qs59a/x.q.P#[A]
         |[3:66..3:68): TC <= qs59a/x.q.TC#
         |[3:69..3:70): A <= qs59a/x.q.TC#[A]
         |[4:19..4:20): p <= qs59a/x.p.
         |[4:29..4:30): C <= qs59a/x.p.C#
         |[4:30..4:30):  <= qs59a/x.p.C#`<init>`().
         |[4:38..4:40): Tr <= qs59a/x.p.Tr#
         |[4:48..4:49): P <= qs59a/x.p.P#
         |[4:50..4:51): A <= qs59a/x.p.P#[A]
         |[4:52..4:52):  <= qs59a/x.p.P#`<init>`().
         |[4:53..4:54): a <= qs59a/x.p.P#`<init>`().(a)
         |[4:53..4:54): a <= qs59a/x.p.P#a.
         |[4:56..4:57): A => qs59a/x.p.P#[A]
         |[4:66..4:68): TC <= qs59a/x.p.TC#
         |[4:69..4:70): A <= qs59a/x.p.TC#[A]
         |[7:7..7:11): qs59 <= _empty_/qs59.
         |[8:9..8:14): qs59a => qs59a/
         |[8:16..8:17): o => qs59a/o.
         |[9:9..9:10): i <= _empty_/qs59.i.
         |[10:11..10:16): qs59a => qs59a/
         |[10:17..10:18): x => qs59a/x.
         |[10:19..10:20): q => qs59a/x.q.
         |[11:8..11:9): a <= _empty_/qs59.i.a.
         |[11:12..11:19): classOf => scala/Predef.classOf().
         |[11:20..11:21): q => qs59a/o.
         |[11:22..11:23): C => qs59a/o.C#
         |[12:8..12:9): b <= _empty_/qs59.i.b.
         |[12:12..12:16): List => scala/collection/immutable/Nil.
         |[12:17..12:18): q => qs59a/o.
         |[12:19..12:20): C => qs59a/o.C#
         |[13:8..13:9): c <= _empty_/qs59.i.c.
         |[13:12..13:19): classOf => scala/Predef.classOf().
         |[13:20..13:24): List => scala/package.List#
         |[13:25..13:26): X => local0
         |[13:43..13:44): X <= local0
         |[13:48..13:49): q => qs59a/o.
         |[13:50..13:51): C => qs59a/o.C#
         |[14:8..14:9): d <= _empty_/qs59.i.d.
         |[14:11..14:15): List => scala/package.List#
         |[14:16..14:17): X => local1
         |[14:34..14:35): X <= local1
         |[14:39..14:40): q => qs59a/o.
         |[14:41..14:42): C => qs59a/o.C#
         |[15:8..15:9): e <= _empty_/qs59.i.e.
         |[15:11..15:26): PartialFunction => scala/PartialFunction#
         |[15:27..15:28): q => qs59a/o.
         |[15:29..15:30): C => qs59a/o.C#
         |[15:32..15:35): Int => scala/Int#
         |[15:39..15:74): (x: q.C) => x match { case _ => 1 } <= local2
         |[15:40..15:41): x <= local9
         |[15:43..15:44): q => qs59a/o.
         |[15:45..15:46): C => qs59a/o.C#
         |[15:51..15:52): x => local9
         |[16:8..16:9): f <= _empty_/qs59.i.f.
         |[16:12..16:16): List => scala/collection/immutable/Nil.
         |[16:17..16:23): AnyRef => java/lang/Object#
         |[16:30..16:31): m <= local10
         |[16:33..16:34): q => qs59a/o.
         |[16:35..16:36): C => qs59a/o.C#
         |[17:8..17:9): g <= _empty_/qs59.i.g.
         |[17:12..17:16): List => scala/collection/immutable/Nil.
         |[17:17..17:18): q => qs59a/o.
         |[18:10..18:11): A <= _empty_/qs59.i.A#
         |[18:12..18:12):  <= _empty_/qs59.i.A#`<init>`().
         |[18:14..18:18): self <= local11
         |[18:20..18:21): q => qs59a/o.
         |[18:22..18:24): Tr => qs59a/o.Tr#
         |[19:10..19:11): B <= _empty_/qs59.i.B#
         |[19:12..19:12):  <= _empty_/qs59.i.B#`<init>`().
         |[19:22..19:23): P => qs59a/o.P#
         |[19:23..19:23):  => qs59a/o.P#`<init>`().
         |[20:8..20:9): h <= _empty_/qs59.i.h.
         |[20:12..20:16): List => scala/collection/immutable/Nil.
         |[20:17..20:23): AnyRef => java/lang/Object#
         |[20:30..20:31): m <= local12
         |[20:32..20:33): A <= local14
         |[20:35..20:35):  <= local13
         |[20:35..20:36): q => qs59a/o.
         |[20:37..20:39): TC => qs59a/o.TC#
         |[20:42..20:45): Int => scala/Int#
         |[22:9..22:10): j <= _empty_/qs59.j.
         |[23:11..23:16): qs59a => qs59a/
         |[23:17..23:18): x => qs59a/x.
         |[23:20..23:21): p => qs59a/x.p.
         |[24:8..24:9): a <= _empty_/qs59.j.a.
         |[24:12..24:19): classOf => scala/Predef.classOf().
         |[24:20..24:21): q => qs59a/o.
         |[24:22..24:23): C => qs59a/o.C#
         |[25:8..25:9): b <= _empty_/qs59.j.b.
         |[25:12..25:16): List => scala/collection/immutable/Nil.
         |[25:17..25:18): q => qs59a/o.
         |[25:19..25:20): C => qs59a/o.C#
         |[26:8..26:9): c <= _empty_/qs59.j.c.
         |[26:12..26:19): classOf => scala/Predef.classOf().
         |[26:20..26:24): List => scala/package.List#
         |[26:25..26:26): X => local15
         |[26:43..26:44): X <= local15
         |[26:48..26:49): q => qs59a/o.
         |[26:50..26:51): C => qs59a/o.C#
         |[27:8..27:9): d <= _empty_/qs59.j.d.
         |[27:11..27:15): List => scala/package.List#
         |[27:16..27:17): X => local16
         |[27:34..27:35): X <= local16
         |[27:39..27:40): q => qs59a/o.
         |[27:41..27:42): C => qs59a/o.C#
         |[28:8..28:9): e <= _empty_/qs59.j.e.
         |[28:11..28:26): PartialFunction => scala/PartialFunction#
         |[28:27..28:28): q => qs59a/o.
         |[28:29..28:30): C => qs59a/o.C#
         |[28:32..28:35): Int => scala/Int#
         |[28:39..28:74): (x: q.C) => x match { case _ => 1 } <= local17
         |[28:40..28:41): x <= local24
         |[28:43..28:44): q => qs59a/o.
         |[28:45..28:46): C => qs59a/o.C#
         |[28:51..28:52): x => local24
         |[29:8..29:9): f <= _empty_/qs59.j.f.
         |[29:12..29:16): List => scala/collection/immutable/Nil.
         |[29:17..29:23): AnyRef => java/lang/Object#
         |[29:30..29:31): m <= local25
         |[29:33..29:34): q => qs59a/o.
         |[29:35..29:36): C => qs59a/o.C#
         |[30:8..30:9): g <= _empty_/qs59.j.g.
         |[30:12..30:16): List => scala/collection/immutable/Nil.
         |[30:17..30:18): q => qs59a/o.
         |[31:10..31:11): A <= _empty_/qs59.j.A#
         |[31:12..31:12):  <= _empty_/qs59.j.A#`<init>`().
         |[31:14..31:18): self <= local26
         |[31:20..31:21): q => qs59a/o.
         |[31:22..31:24): Tr => qs59a/o.Tr#
         |[32:10..32:11): B <= _empty_/qs59.j.B#
         |[32:12..32:12):  <= _empty_/qs59.j.B#`<init>`().
         |[32:22..32:23): P => qs59a/o.P#
         |[32:23..32:23):  => qs59a/o.P#`<init>`().
         |[33:8..33:9): h <= _empty_/qs59.j.h.
         |[33:12..33:16): List => scala/collection/immutable/Nil.
         |[33:17..33:23): AnyRef => java/lang/Object#
         |[33:30..33:31): m <= local27
         |[33:32..33:33): A <= local29
         |[33:35..33:35):  <= local28
         |[33:35..33:36): q => qs59a/o.
         |[33:37..33:39): TC => qs59a/o.TC#
         |[33:42..33:45): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:8..0:13): qs59a <= qs59a/
         |[1:9..1:10): o <= qs59a/o.
         |[1:19..1:20): C <= qs59a/o.C#
         |[1:20..1:20):  <= qs59a/o.C#`<init>`().
         |[1:28..1:30): Tr <= qs59a/o.Tr#
         |[1:38..1:39): P <= qs59a/o.P#
         |[1:40..1:41): A <= qs59a/o.P#[A]
         |[1:42..1:42):  <= qs59a/o.P#`<init>`().
         |[1:43..1:44): a <= qs59a/o.P#`<init>`().(a)
         |[1:43..1:44): a <= qs59a/o.P#a.
         |[1:46..1:47): A => qs59a/o.P#[A]
         |[1:56..1:58): TC <= qs59a/o.TC#
         |[1:59..1:60): A <= qs59a/o.TC#[A]
         |[2:9..2:10): x <= qs59a/x.
         |[3:19..3:20): q <= qs59a/x.q.
         |[3:29..3:30): C <= qs59a/x.q.C#
         |[3:30..3:30):  <= qs59a/x.q.C#`<init>`().
         |[3:38..3:40): Tr <= qs59a/x.q.Tr#
         |[3:48..3:49): P <= qs59a/x.q.P#
         |[3:50..3:51): A <= qs59a/x.q.P#[A]
         |[3:52..3:52):  <= qs59a/x.q.P#`<init>`().
         |[3:53..3:54): a <= qs59a/x.q.P#`<init>`().(a)
         |[3:53..3:54): a <= qs59a/x.q.P#a.
         |[3:56..3:57): A => qs59a/x.q.P#[A]
         |[3:66..3:68): TC <= qs59a/x.q.TC#
         |[3:69..3:70): A <= qs59a/x.q.TC#[A]
         |[4:19..4:20): p <= qs59a/x.p.
         |[4:29..4:30): C <= qs59a/x.p.C#
         |[4:30..4:30):  <= qs59a/x.p.C#`<init>`().
         |[4:38..4:40): Tr <= qs59a/x.p.Tr#
         |[4:48..4:49): P <= qs59a/x.p.P#
         |[4:50..4:51): A <= qs59a/x.p.P#[A]
         |[4:52..4:52):  <= qs59a/x.p.P#`<init>`().
         |[4:53..4:54): a <= qs59a/x.p.P#`<init>`().(a)
         |[4:53..4:54): a <= qs59a/x.p.P#a.
         |[4:56..4:57): A => qs59a/x.p.P#[A]
         |[4:66..4:68): TC <= qs59a/x.p.TC#
         |[4:69..4:70): A <= qs59a/x.p.TC#[A]
         |[7:7..7:11): qs59 <= _empty_/qs59.
         |[8:9..8:14): qs59a => qs59a/
         |[8:16..8:17): o => qs59a/o.
         |[9:9..9:10): i <= _empty_/qs59.i.
         |[10:11..10:16): qs59a => qs59a/
         |[10:17..10:18): x => qs59a/x.
         |[10:19..10:20): q => qs59a/x.q.
         |[11:8..11:9): a <= _empty_/qs59.i.a.
         |[11:12..11:19): classOf => scala/Predef.classOf().
         |[11:20..11:21): q => qs59a/o.
         |[11:22..11:23): C => qs59a/o.C#
         |[12:8..12:9): b <= _empty_/qs59.i.b.
         |[12:12..12:16): List => scala/collection/immutable/Nil.
         |[12:17..12:18): q => qs59a/o.
         |[12:19..12:20): C => qs59a/o.C#
         |[13:8..13:9): c <= _empty_/qs59.i.c.
         |[13:12..13:19): classOf => scala/Predef.classOf().
         |[13:20..13:24): List => scala/package.List#
         |[13:25..13:26): X => local0
         |[13:43..13:44): X <= local0
         |[13:48..13:49): q => qs59a/o.
         |[13:50..13:51): C => qs59a/o.C#
         |[14:8..14:9): d <= _empty_/qs59.i.d.
         |[14:11..14:15): List => scala/package.List#
         |[14:16..14:17): X => local1
         |[14:34..14:35): X <= local1
         |[14:39..14:40): q => qs59a/o.
         |[14:41..14:42): C => qs59a/o.C#
         |[15:8..15:9): e <= _empty_/qs59.i.e.
         |[15:11..15:26): PartialFunction => scala/PartialFunction#
         |[15:27..15:28): q => qs59a/o.
         |[15:29..15:30): C => qs59a/o.C#
         |[15:32..15:35): Int => scala/Int#
         |[15:39..15:74): (x: q.C) => x match { case _ => 1 } <= local2
         |[15:40..15:41): x <= local9
         |[15:51..15:52): x => local9
         |[16:8..16:9): f <= _empty_/qs59.i.f.
         |[16:12..16:16): List => scala/collection/immutable/Nil.
         |[16:17..16:23): AnyRef => java/lang/Object#
         |[16:30..16:31): m <= local10
         |[16:33..16:34): q => qs59a/o.
         |[16:35..16:36): C => qs59a/o.C#
         |[17:8..17:9): g <= _empty_/qs59.i.g.
         |[17:12..17:16): List => scala/collection/immutable/Nil.
         |[17:17..17:18): q => qs59a/o.
         |[18:10..18:11): A <= _empty_/qs59.i.A#
         |[18:12..18:12):  <= _empty_/qs59.i.A#`<init>`().
         |[18:14..18:18): self <= local11
         |[18:20..18:21): q => qs59a/o.
         |[18:22..18:24): Tr => qs59a/o.Tr#
         |[19:10..19:11): B <= _empty_/qs59.i.B#
         |[19:12..19:12):  <= _empty_/qs59.i.B#`<init>`().
         |[19:22..19:23): P => qs59a/o.P#
         |[19:23..19:23):  => qs59a/o.P#`<init>`().
         |[20:8..20:9): h <= _empty_/qs59.i.h.
         |[20:12..20:16): List => scala/collection/immutable/Nil.
         |[20:17..20:23): AnyRef => java/lang/Object#
         |[20:30..20:31): m <= local12
         |[20:32..20:33): A <= local14
         |[20:35..20:35):  <= local13
         |[20:35..20:36): q => qs59a/o.
         |[20:37..20:39): TC => qs59a/o.TC#
         |[20:42..20:45): Int => scala/Int#
         |[22:9..22:10): j <= _empty_/qs59.j.
         |[23:11..23:16): qs59a => qs59a/
         |[23:17..23:18): x => qs59a/x.
         |[23:20..23:21): p => qs59a/x.p.
         |[24:8..24:9): a <= _empty_/qs59.j.a.
         |[24:12..24:19): classOf => scala/Predef.classOf().
         |[24:20..24:21): q => qs59a/o.
         |[24:22..24:23): C => qs59a/o.C#
         |[25:8..25:9): b <= _empty_/qs59.j.b.
         |[25:12..25:16): List => scala/collection/immutable/Nil.
         |[25:17..25:18): q => qs59a/o.
         |[25:19..25:20): C => qs59a/o.C#
         |[26:8..26:9): c <= _empty_/qs59.j.c.
         |[26:12..26:19): classOf => scala/Predef.classOf().
         |[26:20..26:24): List => scala/package.List#
         |[26:25..26:26): X => local15
         |[26:43..26:44): X <= local15
         |[26:48..26:49): q => qs59a/o.
         |[26:50..26:51): C => qs59a/o.C#
         |[27:8..27:9): d <= _empty_/qs59.j.d.
         |[27:11..27:15): List => scala/package.List#
         |[27:16..27:17): X => local16
         |[27:34..27:35): X <= local16
         |[27:39..27:40): q => qs59a/o.
         |[27:41..27:42): C => qs59a/o.C#
         |[28:8..28:9): e <= _empty_/qs59.j.e.
         |[28:11..28:26): PartialFunction => scala/PartialFunction#
         |[28:27..28:28): q => qs59a/o.
         |[28:29..28:30): C => qs59a/o.C#
         |[28:32..28:35): Int => scala/Int#
         |[28:39..28:74): (x: q.C) => x match { case _ => 1 } <= local17
         |[28:40..28:41): x <= local24
         |[28:51..28:52): x => local24
         |[29:8..29:9): f <= _empty_/qs59.j.f.
         |[29:12..29:16): List => scala/collection/immutable/Nil.
         |[29:17..29:23): AnyRef => java/lang/Object#
         |[29:30..29:31): m <= local25
         |[29:33..29:34): q => qs59a/o.
         |[29:35..29:36): C => qs59a/o.C#
         |[30:8..30:9): g <= _empty_/qs59.j.g.
         |[30:12..30:16): List => scala/collection/immutable/Nil.
         |[30:17..30:18): q => qs59a/o.
         |[31:10..31:11): A <= _empty_/qs59.j.A#
         |[31:12..31:12):  <= _empty_/qs59.j.A#`<init>`().
         |[31:14..31:18): self <= local26
         |[31:20..31:21): q => qs59a/o.
         |[31:22..31:24): Tr => qs59a/o.Tr#
         |[32:10..32:11): B <= _empty_/qs59.j.B#
         |[32:12..32:12):  <= _empty_/qs59.j.B#`<init>`().
         |[32:22..32:23): P => qs59a/o.P#
         |[32:23..32:23):  => qs59a/o.P#`<init>`().
         |[33:8..33:9): h <= _empty_/qs59.j.h.
         |[33:12..33:16): List => scala/collection/immutable/Nil.
         |[33:17..33:23): AnyRef => java/lang/Object#
         |[33:30..33:31): m <= local27
         |[33:32..33:33): A <= local29
         |[33:35..33:35):  <= local28
         |[33:35..33:36): q => qs59a/o.
         |[33:37..33:39): TC => qs59a/o.TC#
         |[33:42..33:45): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "inferred parents through a renamed object, with an inner import of an inaccessible object with an alias of that parent",
    """|package qs63a {
       |  object o { class P[A](a: A) }
       |  object x {
       |    private object q { type P[A] = qs63a.o.P[A] }
       |    private object p { type P[A] = qs63a.o.P[A] }
       |  }
       |}
       |object qs63 {
       |  import qs63a.{o => q}
       |  object i {
       |    import qs63a.x.q
       |    class B extends q.P(1)
       |    class D extends q.P[Int](1)
       |  }
       |  object j {
       |    import qs63a.x.{p => q}
       |    class B extends q.P(1)
       |    class D extends q.P[Int](1)
       |  }
       |}
       |""".stripMargin,
    """|[0:8..0:13): qs63a <= qs63a/
       |[1:9..1:10): o <= qs63a/o.
       |[1:19..1:20): P <= qs63a/o.P#
       |[1:21..1:22): A <= qs63a/o.P#[A]
       |[1:23..1:23):  <= qs63a/o.P#`<init>`().
       |[1:24..1:25): a <= qs63a/o.P#`<init>`().(a)
       |[1:24..1:25): a <= qs63a/o.P#a.
       |[1:27..1:28): A => qs63a/o.P#[A]
       |[2:9..2:10): x <= qs63a/x.
       |[3:19..3:20): q <= qs63a/x.q.
       |[3:28..3:29): P <= qs63a/x.q.P#
       |[3:30..3:31): A <= qs63a/x.q.P#[A]
       |[3:35..3:40): qs63a => qs63a/
       |[3:41..3:42): o => qs63a/o.
       |[3:43..3:44): P => qs63a/o.P#
       |[3:45..3:46): A => qs63a/x.q.P#[A]
       |[4:19..4:20): p <= qs63a/x.p.
       |[4:28..4:29): P <= qs63a/x.p.P#
       |[4:30..4:31): A <= qs63a/x.p.P#[A]
       |[4:35..4:40): qs63a => qs63a/
       |[4:41..4:42): o => qs63a/o.
       |[4:43..4:44): P => qs63a/o.P#
       |[4:45..4:46): A => qs63a/x.p.P#[A]
       |[7:7..7:11): qs63 <= _empty_/qs63.
       |[8:9..8:14): qs63a => qs63a/
       |[8:16..8:17): o => qs63a/o.
       |[9:9..9:10): i <= _empty_/qs63.i.
       |[10:11..10:16): qs63a => qs63a/
       |[10:17..10:18): x => qs63a/x.
       |[10:19..10:20): q => qs63a/x.q.
       |[11:10..11:11): B <= _empty_/qs63.i.B#
       |[11:12..11:12):  <= _empty_/qs63.i.B#`<init>`().
       |[11:20..11:21): q => qs63a/x.q.
       |[11:22..11:23): P => qs63a/x.q.P#
       |[11:23..11:23):  => qs63a/o.P#`<init>`().
       |[12:10..12:11): D <= _empty_/qs63.i.D#
       |[12:12..12:12):  <= _empty_/qs63.i.D#`<init>`().
       |[12:20..12:21): q => qs63a/o.
       |[12:22..12:23): P => qs63a/o.P#
       |[12:24..12:27): Int => scala/Int#
       |[12:28..12:28):  => qs63a/o.P#`<init>`().
       |[14:9..14:10): j <= _empty_/qs63.j.
       |[15:11..15:16): qs63a => qs63a/
       |[15:17..15:18): x => qs63a/x.
       |[15:20..15:21): p => qs63a/x.p.
       |[16:10..16:11): B <= _empty_/qs63.j.B#
       |[16:12..16:12):  <= _empty_/qs63.j.B#`<init>`().
       |[16:20..16:21): q => qs63a/x.p.
       |[16:22..16:23): P => qs63a/x.p.P#
       |[16:23..16:23):  => qs63a/o.P#`<init>`().
       |[17:10..17:11): D <= _empty_/qs63.j.D#
       |[17:12..17:12):  <= _empty_/qs63.j.D#`<init>`().
       |[17:20..17:21): q => qs63a/o.
       |[17:22..17:23): P => qs63a/o.P#
       |[17:24..17:27): Int => scala/Int#
       |[17:28..17:28):  => qs63a/o.P#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "a type through a renamed object, with an inner import of a protected static Java class of that name, used in a subclass",
    """|package qs64a { object o { class C; trait Tr; class P[A](a: A); trait TC[A] } }
       |object qs64 {
       |  import qs64a.{o => q}
       |  object i {
       |    import qs64j.J.{Q => q}
       |    val z = classOf[q.C]
       |    class K extends qs64j.J {
       |      val a = classOf[q.C]
       |      val b = List[q.C]()
       |      val c = classOf[List[X] forSome { type X <: q.C }]
       |      val d: List[X] forSome { type X <: q.C } = null
       |      val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |      val f = List[AnyRef { def m: q.C }]()
       |      class A { self: q.Tr => }
       |      class B extends q.P(1)
       |      val h = List[AnyRef { def m[A: q.TC]: Int }]()
       |    }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs64a <= qs64a/
         |[0:23..0:24): o <= qs64a/o.
         |[0:33..0:34): C <= qs64a/o.C#
         |[0:34..0:34):  <= qs64a/o.C#`<init>`().
         |[0:42..0:44): Tr <= qs64a/o.Tr#
         |[0:52..0:53): P <= qs64a/o.P#
         |[0:54..0:55): A <= qs64a/o.P#[A]
         |[0:56..0:56):  <= qs64a/o.P#`<init>`().
         |[0:57..0:58): a <= qs64a/o.P#`<init>`().(a)
         |[0:57..0:58): a <= qs64a/o.P#a.
         |[0:60..0:61): A => qs64a/o.P#[A]
         |[0:70..0:72): TC <= qs64a/o.TC#
         |[0:73..0:74): A <= qs64a/o.TC#[A]
         |[1:7..1:11): qs64 <= _empty_/qs64.
         |[2:9..2:14): qs64a => qs64a/
         |[2:16..2:17): o => qs64a/o.
         |[3:9..3:10): i <= _empty_/qs64.i.
         |[4:11..4:16): qs64j => qs64j/
         |[4:17..4:18): J => qs64j/J#
         |[4:20..4:21): Q => qs64j/J#Q#
         |[5:8..5:9): z <= _empty_/qs64.i.z.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:21): q => qs64j/J#Q#
         |[5:22..5:23): C => qs64j/J#Q#C#
         |[6:10..6:11): K <= _empty_/qs64.i.K#
         |[6:12..6:12):  <= _empty_/qs64.i.K#`<init>`().
         |[6:20..6:25): qs64j => qs64j/
         |[6:26..6:27): J => qs64j/J#
         |[6:27..6:27):  => qs64j/J#`<init>`().
         |[7:10..7:11): a <= _empty_/qs64.i.K#a.
         |[7:14..7:21): classOf => scala/Predef.classOf().
         |[7:22..7:23): q => qs64j/J#Q#
         |[7:24..7:25): C => qs64j/J#Q#C#
         |[8:10..8:11): b <= _empty_/qs64.i.K#b.
         |[8:14..8:18): List => scala/collection/immutable/Nil.
         |[8:19..8:20): q => qs64j/J#Q#
         |[8:21..8:22): C => qs64j/J#Q#C#
         |[9:10..9:11): c <= _empty_/qs64.i.K#c.
         |[9:14..9:21): classOf => scala/Predef.classOf().
         |[9:22..9:26): List => scala/package.List#
         |[9:27..9:28): X => local0
         |[9:45..9:46): X <= local0
         |[9:50..9:51): q => qs64j/J#Q#
         |[9:52..9:53): C => qs64j/J#Q#C#
         |[10:10..10:11): d <= _empty_/qs64.i.K#d.
         |[10:13..10:17): List => scala/package.List#
         |[10:18..10:19): X => local1
         |[10:36..10:37): X <= local1
         |[10:41..10:42): q => qs64j/J#Q#
         |[10:43..10:44): C => qs64j/J#Q#C#
         |[11:10..11:11): e <= _empty_/qs64.i.K#e.
         |[11:13..11:28): PartialFunction => scala/PartialFunction#
         |[11:29..11:30): q => qs64j/J#Q#
         |[11:31..11:32): C => qs64j/J#Q#C#
         |[11:34..11:37): Int => scala/Int#
         |[11:41..11:76): (x: q.C) => x match { case _ => 1 } <= local2
         |[11:42..11:43): x <= local9
         |[11:45..11:46): q => qs64j/J#Q#
         |[11:47..11:48): C => qs64j/J#Q#C#
         |[11:53..11:54): x => local9
         |[12:10..12:11): f <= _empty_/qs64.i.K#f.
         |[12:14..12:18): List => scala/collection/immutable/Nil.
         |[12:19..12:25): AnyRef => java/lang/Object#
         |[12:32..12:33): m <= local10
         |[12:35..12:36): q => qs64j/J#Q#
         |[12:37..12:38): C => qs64j/J#Q#C#
         |[13:12..13:13): A <= _empty_/qs64.i.K#A#
         |[13:14..13:14):  <= _empty_/qs64.i.K#A#`<init>`().
         |[13:16..13:20): self <= local11
         |[13:22..13:23): q => qs64j/J#Q#
         |[13:24..13:26): Tr => qs64j/J#Q#Tr#
         |[14:12..14:13): B <= _empty_/qs64.i.K#B#
         |[14:14..14:14):  <= _empty_/qs64.i.K#B#`<init>`().
         |[14:22..14:23): q => qs64j/J#Q#
         |[14:24..14:25): P => qs64j/J#Q#P#
         |[14:25..14:25):  => qs64j/J#Q#P#`<init>`().
         |[15:10..15:11): h <= _empty_/qs64.i.K#h.
         |[15:14..15:18): List => scala/collection/immutable/Nil.
         |[15:19..15:25): AnyRef => java/lang/Object#
         |[15:32..15:33): m <= local12
         |[15:34..15:35): A <= local14
         |[15:37..15:37):  <= local13
         |[15:37..15:38): q => qs64j/J#Q#
         |[15:39..15:41): TC => qs64j/J#Q#TC#
         |[15:44..15:47): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:8..0:13): qs64a <= qs64a/
         |[0:23..0:24): o <= qs64a/o.
         |[0:33..0:34): C <= qs64a/o.C#
         |[0:34..0:34):  <= qs64a/o.C#`<init>`().
         |[0:42..0:44): Tr <= qs64a/o.Tr#
         |[0:52..0:53): P <= qs64a/o.P#
         |[0:54..0:55): A <= qs64a/o.P#[A]
         |[0:56..0:56):  <= qs64a/o.P#`<init>`().
         |[0:57..0:58): a <= qs64a/o.P#`<init>`().(a)
         |[0:57..0:58): a <= qs64a/o.P#a.
         |[0:60..0:61): A => qs64a/o.P#[A]
         |[0:70..0:72): TC <= qs64a/o.TC#
         |[0:73..0:74): A <= qs64a/o.TC#[A]
         |[1:7..1:11): qs64 <= _empty_/qs64.
         |[2:9..2:14): qs64a => qs64a/
         |[2:16..2:17): o => qs64a/o.
         |[3:9..3:10): i <= _empty_/qs64.i.
         |[4:11..4:16): qs64j => qs64j/
         |[4:17..4:18): J => qs64j/J#
         |[4:20..4:21): Q => qs64j/J#Q#
         |[5:8..5:9): z <= _empty_/qs64.i.z.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:21): q => qs64j/J#Q#
         |[5:22..5:23): C => qs64j/J#Q#C#
         |[6:10..6:11): K <= _empty_/qs64.i.K#
         |[6:12..6:12):  <= _empty_/qs64.i.K#`<init>`().
         |[6:20..6:25): qs64j => qs64j/
         |[6:26..6:27): J => qs64j/J#
         |[6:27..6:27):  => qs64j/J#`<init>`().
         |[7:10..7:11): a <= _empty_/qs64.i.K#a.
         |[7:14..7:21): classOf => scala/Predef.classOf().
         |[7:22..7:23): q => qs64j/J#Q#
         |[7:24..7:25): C => qs64j/J#Q#C#
         |[8:10..8:11): b <= _empty_/qs64.i.K#b.
         |[8:14..8:18): List => scala/collection/immutable/Nil.
         |[8:19..8:20): q => qs64j/J#Q#
         |[8:21..8:22): C => qs64j/J#Q#C#
         |[9:10..9:11): c <= _empty_/qs64.i.K#c.
         |[9:14..9:21): classOf => scala/Predef.classOf().
         |[9:22..9:26): List => scala/package.List#
         |[9:27..9:28): X => local0
         |[9:45..9:46): X <= local0
         |[9:50..9:51): q => qs64j/J#Q#
         |[9:52..9:53): C => qs64j/J#Q#C#
         |[10:10..10:11): d <= _empty_/qs64.i.K#d.
         |[10:13..10:17): List => scala/package.List#
         |[10:18..10:19): X => local1
         |[10:36..10:37): X <= local1
         |[10:41..10:42): q => qs64j/J#Q#
         |[10:43..10:44): C => qs64j/J#Q#C#
         |[11:10..11:11): e <= _empty_/qs64.i.K#e.
         |[11:13..11:28): PartialFunction => scala/PartialFunction#
         |[11:29..11:30): q => qs64j/J#Q#
         |[11:31..11:32): C => qs64j/J#Q#C#
         |[11:34..11:37): Int => scala/Int#
         |[11:41..11:76): (x: q.C) => x match { case _ => 1 } <= local2
         |[11:42..11:43): x <= local9
         |[11:53..11:54): x => local9
         |[12:10..12:11): f <= _empty_/qs64.i.K#f.
         |[12:14..12:18): List => scala/collection/immutable/Nil.
         |[12:19..12:25): AnyRef => java/lang/Object#
         |[12:32..12:33): m <= local10
         |[12:35..12:36): q => qs64j/J#Q#
         |[12:37..12:38): C => qs64j/J#Q#C#
         |[13:12..13:13): A <= _empty_/qs64.i.K#A#
         |[13:14..13:14):  <= _empty_/qs64.i.K#A#`<init>`().
         |[13:16..13:20): self <= local11
         |[13:22..13:23): q => qs64j/J#Q#
         |[13:24..13:26): Tr => qs64j/J#Q#Tr#
         |[14:12..14:13): B <= _empty_/qs64.i.K#B#
         |[14:14..14:14):  <= _empty_/qs64.i.K#B#`<init>`().
         |[14:22..14:23): q => qs64j/J#Q#
         |[14:24..14:25): P => qs64j/J#Q#P#
         |[14:25..14:25):  => qs64j/J#Q#P#`<init>`().
         |[15:10..15:11): h <= _empty_/qs64.i.K#h.
         |[15:14..15:18): List => scala/collection/immutable/Nil.
         |[15:19..15:25): AnyRef => java/lang/Object#
         |[15:32..15:33): m <= local12
         |[15:34..15:35): A <= local14
         |[15:37..15:37):  <= local13
         |[15:37..15:38): q => qs64j/J#Q#
         |[15:39..15:41): TC => qs64j/J#Q#TC#
         |[15:44..15:47): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "a type through a renamed object, with an inner import of a protected object of that name, used in a subclass",
    """|package qs65a { object o { class C; trait Tr; class P[A](a: A); trait TC[A] } }
       |object qs65 {
       |  import qs65a.{o => q}
       |  class R { protected object Q { class C; trait Tr; class P[A](a: A); trait TC[A] } }
       |  object k extends i.K
       |  object i {
       |    import qs65.k.{Q => q}
       |    val z = classOf[q.C]
       |    class K extends R {
       |      val a = classOf[q.C]
       |      val b = List[q.C]()
       |      val c = classOf[List[X] forSome { type X <: q.C }]
       |      val d: List[X] forSome { type X <: q.C } = null
       |      val e: PartialFunction[q.C, Int] = (x: q.C) => x match { case _ => 1 }
       |      val f = List[AnyRef { def m: q.C }]()
       |      val g = List[q.type]()
       |      class A { self: q.Tr => }
       |      class B extends q.P(1)
       |      val h = List[AnyRef { def m[A: q.TC]: Int }]()
       |    }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:8..0:13): qs65a <= qs65a/
         |[0:23..0:24): o <= qs65a/o.
         |[0:33..0:34): C <= qs65a/o.C#
         |[0:34..0:34):  <= qs65a/o.C#`<init>`().
         |[0:42..0:44): Tr <= qs65a/o.Tr#
         |[0:52..0:53): P <= qs65a/o.P#
         |[0:54..0:55): A <= qs65a/o.P#[A]
         |[0:56..0:56):  <= qs65a/o.P#`<init>`().
         |[0:57..0:58): a <= qs65a/o.P#`<init>`().(a)
         |[0:57..0:58): a <= qs65a/o.P#a.
         |[0:60..0:61): A => qs65a/o.P#[A]
         |[0:70..0:72): TC <= qs65a/o.TC#
         |[0:73..0:74): A <= qs65a/o.TC#[A]
         |[1:7..1:11): qs65 <= _empty_/qs65.
         |[2:9..2:14): qs65a => qs65a/
         |[2:16..2:17): o => qs65a/o.
         |[3:8..3:9): R <= _empty_/qs65.R#
         |[3:10..3:10):  <= _empty_/qs65.R#`<init>`().
         |[3:29..3:30): Q <= _empty_/qs65.R#Q.
         |[3:39..3:40): C <= _empty_/qs65.R#Q.C#
         |[3:40..3:40):  <= _empty_/qs65.R#Q.C#`<init>`().
         |[3:48..3:50): Tr <= _empty_/qs65.R#Q.Tr#
         |[3:58..3:59): P <= _empty_/qs65.R#Q.P#
         |[3:60..3:61): A <= _empty_/qs65.R#Q.P#[A]
         |[3:62..3:62):  <= _empty_/qs65.R#Q.P#`<init>`().
         |[3:63..3:64): a <= _empty_/qs65.R#Q.P#`<init>`().(a)
         |[3:63..3:64): a <= _empty_/qs65.R#Q.P#a.
         |[3:66..3:67): A => _empty_/qs65.R#Q.P#[A]
         |[3:76..3:78): TC <= _empty_/qs65.R#Q.TC#
         |[3:79..3:80): A <= _empty_/qs65.R#Q.TC#[A]
         |[4:9..4:10): k <= _empty_/qs65.k.
         |[4:19..4:20): i => _empty_/qs65.i.
         |[4:21..4:22): K => _empty_/qs65.i.K#
         |[4:22..4:22):  => _empty_/qs65.i.K#`<init>`().
         |[5:9..5:10): i <= _empty_/qs65.i.
         |[6:11..6:15): qs65 => _empty_/qs65.
         |[6:16..6:17): k => _empty_/qs65.k.
         |[6:19..6:20): Q => _empty_/qs65.R#Q.
         |[7:8..7:9): z <= _empty_/qs65.i.z.
         |[7:12..7:19): classOf => scala/Predef.classOf().
         |[7:20..7:21): q => qs65a/o.
         |[7:22..7:23): C => qs65a/o.C#
         |[8:10..8:11): K <= _empty_/qs65.i.K#
         |[8:12..8:12):  <= _empty_/qs65.i.K#`<init>`().
         |[8:20..8:21): R => _empty_/qs65.R#
         |[8:21..8:21):  => _empty_/qs65.R#`<init>`().
         |[9:10..9:11): a <= _empty_/qs65.i.K#a.
         |[9:14..9:21): classOf => scala/Predef.classOf().
         |[9:22..9:23): q => _empty_/qs65.R#Q.
         |[9:24..9:25): C => _empty_/qs65.R#Q.C#
         |[10:10..10:11): b <= _empty_/qs65.i.K#b.
         |[10:14..10:18): List => scala/collection/immutable/Nil.
         |[10:19..10:20): q => _empty_/qs65.R#Q.
         |[10:21..10:22): C => _empty_/qs65.R#Q.C#
         |[11:10..11:11): c <= _empty_/qs65.i.K#c.
         |[11:14..11:21): classOf => scala/Predef.classOf().
         |[11:22..11:26): List => scala/package.List#
         |[11:27..11:28): X => local0
         |[11:45..11:46): X <= local0
         |[11:50..11:51): q => _empty_/qs65.R#Q.
         |[11:52..11:53): C => _empty_/qs65.R#Q.C#
         |[12:10..12:11): d <= _empty_/qs65.i.K#d.
         |[12:13..12:17): List => scala/package.List#
         |[12:18..12:19): X => local1
         |[12:36..12:37): X <= local1
         |[12:41..12:42): q => _empty_/qs65.R#Q.
         |[12:43..12:44): C => _empty_/qs65.R#Q.C#
         |[13:10..13:11): e <= _empty_/qs65.i.K#e.
         |[13:13..13:28): PartialFunction => scala/PartialFunction#
         |[13:29..13:30): q => _empty_/qs65.R#Q.
         |[13:31..13:32): C => _empty_/qs65.R#Q.C#
         |[13:34..13:37): Int => scala/Int#
         |[13:41..13:76): (x: q.C) => x match { case _ => 1 } <= local2
         |[13:42..13:43): x <= local9
         |[13:45..13:46): q => _empty_/qs65.R#Q.
         |[13:47..13:48): C => _empty_/qs65.R#Q.C#
         |[13:53..13:54): x => local9
         |[14:10..14:11): f <= _empty_/qs65.i.K#f.
         |[14:14..14:18): List => scala/collection/immutable/Nil.
         |[14:19..14:25): AnyRef => java/lang/Object#
         |[14:32..14:33): m <= local10
         |[14:35..14:36): q => _empty_/qs65.R#Q.
         |[14:37..14:38): C => _empty_/qs65.R#Q.C#
         |[15:10..15:11): g <= _empty_/qs65.i.K#g.
         |[15:14..15:18): List => scala/collection/immutable/Nil.
         |[15:19..15:20): q => _empty_/qs65.R#Q.
         |[16:12..16:13): A <= _empty_/qs65.i.K#A#
         |[16:14..16:14):  <= _empty_/qs65.i.K#A#`<init>`().
         |[16:16..16:20): self <= local11
         |[16:22..16:23): q => _empty_/qs65.R#Q.
         |[16:24..16:26): Tr => _empty_/qs65.R#Q.Tr#
         |[17:12..17:13): B <= _empty_/qs65.i.K#B#
         |[17:14..17:14):  <= _empty_/qs65.i.K#B#`<init>`().
         |[17:22..17:23): q => _empty_/qs65.R#Q.
         |[17:24..17:25): P => _empty_/qs65.R#Q.P#
         |[17:25..17:25):  => _empty_/qs65.R#Q.P#`<init>`().
         |[18:10..18:11): h <= _empty_/qs65.i.K#h.
         |[18:14..18:18): List => scala/collection/immutable/Nil.
         |[18:19..18:25): AnyRef => java/lang/Object#
         |[18:32..18:33): m <= local12
         |[18:34..18:35): A <= local14
         |[18:37..18:37):  <= local13
         |[18:37..18:38): q => _empty_/qs65.R#Q.
         |[18:39..18:41): TC => _empty_/qs65.R#Q.TC#
         |[18:44..18:47): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:8..0:13): qs65a <= qs65a/
         |[0:23..0:24): o <= qs65a/o.
         |[0:33..0:34): C <= qs65a/o.C#
         |[0:34..0:34):  <= qs65a/o.C#`<init>`().
         |[0:42..0:44): Tr <= qs65a/o.Tr#
         |[0:52..0:53): P <= qs65a/o.P#
         |[0:54..0:55): A <= qs65a/o.P#[A]
         |[0:56..0:56):  <= qs65a/o.P#`<init>`().
         |[0:57..0:58): a <= qs65a/o.P#`<init>`().(a)
         |[0:57..0:58): a <= qs65a/o.P#a.
         |[0:60..0:61): A => qs65a/o.P#[A]
         |[0:70..0:72): TC <= qs65a/o.TC#
         |[0:73..0:74): A <= qs65a/o.TC#[A]
         |[1:7..1:11): qs65 <= _empty_/qs65.
         |[2:9..2:14): qs65a => qs65a/
         |[2:16..2:17): o => qs65a/o.
         |[3:8..3:9): R <= _empty_/qs65.R#
         |[3:10..3:10):  <= _empty_/qs65.R#`<init>`().
         |[3:29..3:30): Q <= _empty_/qs65.R#Q.
         |[3:39..3:40): C <= _empty_/qs65.R#Q.C#
         |[3:40..3:40):  <= _empty_/qs65.R#Q.C#`<init>`().
         |[3:48..3:50): Tr <= _empty_/qs65.R#Q.Tr#
         |[3:58..3:59): P <= _empty_/qs65.R#Q.P#
         |[3:60..3:61): A <= _empty_/qs65.R#Q.P#[A]
         |[3:62..3:62):  <= _empty_/qs65.R#Q.P#`<init>`().
         |[3:63..3:64): a <= _empty_/qs65.R#Q.P#`<init>`().(a)
         |[3:63..3:64): a <= _empty_/qs65.R#Q.P#a.
         |[3:66..3:67): A => _empty_/qs65.R#Q.P#[A]
         |[3:76..3:78): TC <= _empty_/qs65.R#Q.TC#
         |[3:79..3:80): A <= _empty_/qs65.R#Q.TC#[A]
         |[4:9..4:10): k <= _empty_/qs65.k.
         |[4:19..4:20): i => _empty_/qs65.i.
         |[4:21..4:22): K => _empty_/qs65.i.K#
         |[4:22..4:22):  => _empty_/qs65.i.K#`<init>`().
         |[5:9..5:10): i <= _empty_/qs65.i.
         |[6:11..6:15): qs65 => _empty_/qs65.
         |[6:16..6:17): k => _empty_/qs65.k.
         |[6:19..6:20): Q => _empty_/qs65.R#Q.
         |[7:8..7:9): z <= _empty_/qs65.i.z.
         |[7:12..7:19): classOf => scala/Predef.classOf().
         |[7:20..7:21): q => qs65a/o.
         |[7:22..7:23): C => qs65a/o.C#
         |[8:10..8:11): K <= _empty_/qs65.i.K#
         |[8:12..8:12):  <= _empty_/qs65.i.K#`<init>`().
         |[8:20..8:21): R => _empty_/qs65.R#
         |[8:21..8:21):  => _empty_/qs65.R#`<init>`().
         |[9:10..9:11): a <= _empty_/qs65.i.K#a.
         |[9:14..9:21): classOf => scala/Predef.classOf().
         |[9:22..9:23): q => _empty_/qs65.R#Q.
         |[9:24..9:25): C => _empty_/qs65.R#Q.C#
         |[10:10..10:11): b <= _empty_/qs65.i.K#b.
         |[10:14..10:18): List => scala/collection/immutable/Nil.
         |[10:19..10:20): q => _empty_/qs65.R#Q.
         |[10:21..10:22): C => _empty_/qs65.R#Q.C#
         |[11:10..11:11): c <= _empty_/qs65.i.K#c.
         |[11:14..11:21): classOf => scala/Predef.classOf().
         |[11:22..11:26): List => scala/package.List#
         |[11:27..11:28): X => local0
         |[11:45..11:46): X <= local0
         |[11:50..11:51): q => _empty_/qs65.R#Q.
         |[11:52..11:53): C => _empty_/qs65.R#Q.C#
         |[12:10..12:11): d <= _empty_/qs65.i.K#d.
         |[12:13..12:17): List => scala/package.List#
         |[12:18..12:19): X => local1
         |[12:36..12:37): X <= local1
         |[12:41..12:42): q => _empty_/qs65.R#Q.
         |[12:43..12:44): C => _empty_/qs65.R#Q.C#
         |[13:10..13:11): e <= _empty_/qs65.i.K#e.
         |[13:13..13:28): PartialFunction => scala/PartialFunction#
         |[13:29..13:30): q => _empty_/qs65.R#Q.
         |[13:31..13:32): C => _empty_/qs65.R#Q.C#
         |[13:34..13:37): Int => scala/Int#
         |[13:41..13:76): (x: q.C) => x match { case _ => 1 } <= local2
         |[13:42..13:43): x <= local9
         |[13:53..13:54): x => local9
         |[14:10..14:11): f <= _empty_/qs65.i.K#f.
         |[14:14..14:18): List => scala/collection/immutable/Nil.
         |[14:19..14:25): AnyRef => java/lang/Object#
         |[14:32..14:33): m <= local10
         |[14:35..14:36): q => _empty_/qs65.R#Q.
         |[14:37..14:38): C => _empty_/qs65.R#Q.C#
         |[15:10..15:11): g <= _empty_/qs65.i.K#g.
         |[15:14..15:18): List => scala/collection/immutable/Nil.
         |[15:19..15:20): q => _empty_/qs65.R#Q.
         |[16:12..16:13): A <= _empty_/qs65.i.K#A#
         |[16:14..16:14):  <= _empty_/qs65.i.K#A#`<init>`().
         |[16:16..16:20): self <= local11
         |[16:22..16:23): q => _empty_/qs65.R#Q.
         |[16:24..16:26): Tr => _empty_/qs65.R#Q.Tr#
         |[17:12..17:13): B <= _empty_/qs65.i.K#B#
         |[17:14..17:14):  <= _empty_/qs65.i.K#B#`<init>`().
         |[17:22..17:23): q => _empty_/qs65.R#Q.
         |[17:24..17:25): P => _empty_/qs65.R#Q.P#
         |[17:25..17:25):  => _empty_/qs65.R#Q.P#`<init>`().
         |[18:10..18:11): h <= _empty_/qs65.i.K#h.
         |[18:14..18:18): List => scala/collection/immutable/Nil.
         |[18:19..18:25): AnyRef => java/lang/Object#
         |[18:32..18:33): m <= local12
         |[18:34..18:35): A <= local14
         |[18:37..18:37):  <= local13
         |[18:37..18:38): q => _empty_/qs65.R#Q.
         |[18:39..18:41): TC => _empty_/qs65.R#Q.TC#
         |[18:44..18:47): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "a type through the name of an object and a class, inside the class",
    """|class qs18 {
       |  class B
       |  val a1 = classOf[qs18.B]
       |  val b1 = List[qs18.B]()
       |  val c1 = classOf[List[X] forSome { type X <: qs18.B }]
       |  val d1: List[X] forSome { type X <: qs18.B } = null
       |  val e1: PartialFunction[qs18.B, Int] = (x: qs18.B) => x match { case _ => 1 }
       |  val f1 = List[AnyRef { def m: qs18.B }]()
       |  val a2 = classOf[qs18.this.B]
       |  val b2 = List[qs18.this.B]()
       |  val c2 = classOf[List[X] forSome { type X <: qs18.this.B }]
       |  val d2: List[X] forSome { type X <: qs18.this.B } = null
       |  val e2: PartialFunction[qs18.this.B, Int] = (x: qs18.this.B) => x match { case _ => 1 }
       |  val f2 = List[AnyRef { def m: qs18.this.B }]()
       |}
       |object qs18 { class B }
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): qs18 <= _empty_/qs18#
         |[0:11..0:11):  <= _empty_/qs18#`<init>`().
         |[1:8..1:9): B <= _empty_/qs18#B#
         |[1:9..1:9):  <= _empty_/qs18#B#`<init>`().
         |[2:6..2:8): a1 <= _empty_/qs18#a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:19..2:23): qs18 => _empty_/qs18.
         |[2:24..2:25): B => _empty_/qs18.B#
         |[3:6..3:8): b1 <= _empty_/qs18#b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:16..3:20): qs18 => _empty_/qs18.
         |[3:21..3:22): B => _empty_/qs18.B#
         |[4:6..4:8): c1 <= _empty_/qs18#c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:47..4:51): qs18 => _empty_/qs18.
         |[4:52..4:53): B => _empty_/qs18.B#
         |[5:6..5:8): d1 <= _empty_/qs18#d1.
         |[5:10..5:14): List => scala/package.List#
         |[5:15..5:16): X => local1
         |[5:33..5:34): X <= local1
         |[5:38..5:42): qs18 => _empty_/qs18.
         |[5:43..5:44): B => _empty_/qs18.B#
         |[6:6..6:8): e1 <= _empty_/qs18#e1.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:30): qs18 => _empty_/qs18.
         |[6:31..6:32): B => _empty_/qs18.B#
         |[6:34..6:37): Int => scala/Int#
         |[6:41..6:79): (x: qs18.B) => x match { case _ => 1 } <= local2
         |[6:42..6:43): x <= local9
         |[6:45..6:49): qs18 => _empty_/qs18.
         |[6:50..6:51): B => _empty_/qs18.B#
         |[6:56..6:57): x => local9
         |[7:6..7:8): f1 <= _empty_/qs18#f1.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:22): AnyRef => java/lang/Object#
         |[7:29..7:30): m <= local10
         |[7:32..7:36): qs18 => _empty_/qs18.
         |[7:37..7:38): B => _empty_/qs18.B#
         |[8:6..8:8): a2 <= _empty_/qs18#a2.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:19..8:23): qs18 => _empty_/qs18#
         |[8:29..8:30): B => _empty_/qs18#B#
         |[9:6..9:8): b2 <= _empty_/qs18#b2.
         |[9:11..9:15): List => scala/collection/immutable/Nil.
         |[9:16..9:20): qs18 => _empty_/qs18#
         |[9:26..9:27): B => _empty_/qs18#B#
         |[10:6..10:8): c2 <= _empty_/qs18#c2.
         |[10:11..10:18): classOf => scala/Predef.classOf().
         |[10:19..10:23): List => scala/package.List#
         |[10:24..10:25): X => local11
         |[10:42..10:43): X <= local11
         |[10:47..10:51): qs18 => _empty_/qs18#
         |[10:57..10:58): B => _empty_/qs18#B#
         |[11:6..11:8): d2 <= _empty_/qs18#d2.
         |[11:10..11:14): List => scala/package.List#
         |[11:15..11:16): X => local12
         |[11:33..11:34): X <= local12
         |[11:38..11:42): qs18 => _empty_/qs18#
         |[11:48..11:49): B => _empty_/qs18#B#
         |[12:6..12:8): e2 <= _empty_/qs18#e2.
         |[12:10..12:25): PartialFunction => scala/PartialFunction#
         |[12:26..12:30): qs18 => _empty_/qs18#
         |[12:36..12:37): B => _empty_/qs18#B#
         |[12:39..12:42): Int => scala/Int#
         |[12:46..12:89): (x: qs18.this.B) => x match { case _ => 1 } <= local13
         |[12:47..12:48): x <= local20
         |[12:50..12:54): qs18 => _empty_/qs18#
         |[12:60..12:61): B => _empty_/qs18#B#
         |[12:66..12:67): x => local20
         |[13:6..13:8): f2 <= _empty_/qs18#f2.
         |[13:11..13:15): List => scala/collection/immutable/Nil.
         |[13:16..13:22): AnyRef => java/lang/Object#
         |[13:29..13:30): m <= local21
         |[13:32..13:36): qs18 => _empty_/qs18#
         |[13:42..13:43): B => _empty_/qs18#B#
         |[15:7..15:11): qs18 <= _empty_/qs18.
         |[15:20..15:21): B <= _empty_/qs18.B#
         |[15:21..15:21):  <= _empty_/qs18.B#`<init>`().
         |""".stripMargin
    else
      """|[0:6..0:10): qs18 <= _empty_/qs18#
         |[0:11..0:11):  <= _empty_/qs18#`<init>`().
         |[1:8..1:9): B <= _empty_/qs18#B#
         |[1:9..1:9):  <= _empty_/qs18#B#`<init>`().
         |[2:6..2:8): a1 <= _empty_/qs18#a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:19..2:23): qs18 => _empty_/qs18.
         |[2:24..2:25): B => _empty_/qs18.B#
         |[3:6..3:8): b1 <= _empty_/qs18#b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:16..3:20): qs18 => _empty_/qs18.
         |[3:21..3:22): B => _empty_/qs18.B#
         |[4:6..4:8): c1 <= _empty_/qs18#c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:47..4:51): qs18 => _empty_/qs18.
         |[4:52..4:53): B => _empty_/qs18.B#
         |[5:6..5:8): d1 <= _empty_/qs18#d1.
         |[5:10..5:14): List => scala/package.List#
         |[5:15..5:16): X => local1
         |[5:33..5:34): X <= local1
         |[5:38..5:42): qs18 => _empty_/qs18.
         |[5:43..5:44): B => _empty_/qs18.B#
         |[6:6..6:8): e1 <= _empty_/qs18#e1.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:30): qs18 => _empty_/qs18.
         |[6:31..6:32): B => _empty_/qs18.B#
         |[6:34..6:37): Int => scala/Int#
         |[6:41..6:79): (x: qs18.B) => x match { case _ => 1 } <= local2
         |[6:42..6:43): x <= local9
         |[6:56..6:57): x => local9
         |[7:6..7:8): f1 <= _empty_/qs18#f1.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:16..7:22): AnyRef => java/lang/Object#
         |[7:29..7:30): m <= local10
         |[7:32..7:36): qs18 => _empty_/qs18.
         |[7:37..7:38): B => _empty_/qs18.B#
         |[8:6..8:8): a2 <= _empty_/qs18#a2.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:19..8:23): qs18 => _empty_/qs18#
         |[8:29..8:30): B => _empty_/qs18#B#
         |[9:6..9:8): b2 <= _empty_/qs18#b2.
         |[9:11..9:15): List => scala/collection/immutable/Nil.
         |[9:16..9:20): qs18 => _empty_/qs18#
         |[9:26..9:27): B => _empty_/qs18#B#
         |[10:6..10:8): c2 <= _empty_/qs18#c2.
         |[10:11..10:18): classOf => scala/Predef.classOf().
         |[10:19..10:23): List => scala/package.List#
         |[10:24..10:25): X => local11
         |[10:42..10:43): X <= local11
         |[10:47..10:51): qs18 => _empty_/qs18#
         |[10:57..10:58): B => _empty_/qs18#B#
         |[11:6..11:8): d2 <= _empty_/qs18#d2.
         |[11:10..11:14): List => scala/package.List#
         |[11:15..11:16): X => local12
         |[11:33..11:34): X <= local12
         |[11:38..11:42): qs18 => _empty_/qs18#
         |[11:48..11:49): B => _empty_/qs18#B#
         |[12:6..12:8): e2 <= _empty_/qs18#e2.
         |[12:10..12:25): PartialFunction => scala/PartialFunction#
         |[12:26..12:30): qs18 => _empty_/qs18#
         |[12:36..12:37): B => _empty_/qs18#B#
         |[12:39..12:42): Int => scala/Int#
         |[12:46..12:89): (x: qs18.this.B) => x match { case _ => 1 } <= local13
         |[12:47..12:48): x <= local20
         |[12:66..12:67): x => local20
         |[13:6..13:8): f2 <= _empty_/qs18#f2.
         |[13:11..13:15): List => scala/collection/immutable/Nil.
         |[13:16..13:22): AnyRef => java/lang/Object#
         |[13:29..13:30): m <= local21
         |[13:32..13:36): qs18 => _empty_/qs18#
         |[13:42..13:43): B => _empty_/qs18#B#
         |[15:7..15:11): qs18 <= _empty_/qs18.
         |[15:20..15:21): B <= _empty_/qs18.B#
         |[15:21..15:21):  <= _empty_/qs18.B#`<init>`().
         |""".stripMargin,
  )

  occurrences(
    "a type through the name of an object and a class, outside them",
    """|class qs19a { class B }
       |object qs19a { class B }
       |object qs19 {
       |  val a = classOf[qs19a.B]
       |  val b = List[qs19a.B]()
       |  val c = classOf[List[X] forSome { type X <: qs19a.B }]
       |  val d: List[X] forSome { type X <: qs19a.B } = null
       |  val e: PartialFunction[qs19a.B, Int] = (x: qs19a.B) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: qs19a.B }]()
       |  val g = List[qs19a.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:11): qs19a <= _empty_/qs19a#
         |[0:12..0:12):  <= _empty_/qs19a#`<init>`().
         |[0:20..0:21): B <= _empty_/qs19a#B#
         |[0:21..0:21):  <= _empty_/qs19a#B#`<init>`().
         |[1:7..1:12): qs19a <= _empty_/qs19a.
         |[1:21..1:22): B <= _empty_/qs19a.B#
         |[1:22..1:22):  <= _empty_/qs19a.B#`<init>`().
         |[2:7..2:11): qs19 <= _empty_/qs19.
         |[3:6..3:7): a <= _empty_/qs19.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:23): qs19a => _empty_/qs19a.
         |[3:24..3:25): B => _empty_/qs19a.B#
         |[4:6..4:7): b <= _empty_/qs19.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:20): qs19a => _empty_/qs19a.
         |[4:21..4:22): B => _empty_/qs19a.B#
         |[5:6..5:7): c <= _empty_/qs19.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:51): qs19a => _empty_/qs19a.
         |[5:52..5:53): B => _empty_/qs19a.B#
         |[6:6..6:7): d <= _empty_/qs19.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:42): qs19a => _empty_/qs19a.
         |[6:43..6:44): B => _empty_/qs19a.B#
         |[7:6..7:7): e <= _empty_/qs19.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:30): qs19a => _empty_/qs19a.
         |[7:31..7:32): B => _empty_/qs19a.B#
         |[7:34..7:37): Int => scala/Int#
         |[7:41..7:80): (x: qs19a.B) => x match { case _ => 1 } <= local2
         |[7:42..7:43): x <= local9
         |[7:45..7:50): qs19a => _empty_/qs19a.
         |[7:51..7:52): B => _empty_/qs19a.B#
         |[7:57..7:58): x => local9
         |[8:6..8:7): f <= _empty_/qs19.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:36): qs19a => _empty_/qs19a.
         |[8:37..8:38): B => _empty_/qs19a.B#
         |[9:6..9:7): g <= _empty_/qs19.g.
         |[9:10..9:14): List => scala/collection/immutable/Nil.
         |[9:15..9:20): qs19a => _empty_/qs19a.
         |""".stripMargin
    else
      """|[0:6..0:11): qs19a <= _empty_/qs19a#
         |[0:12..0:12):  <= _empty_/qs19a#`<init>`().
         |[0:20..0:21): B <= _empty_/qs19a#B#
         |[0:21..0:21):  <= _empty_/qs19a#B#`<init>`().
         |[1:7..1:12): qs19a <= _empty_/qs19a.
         |[1:21..1:22): B <= _empty_/qs19a.B#
         |[1:22..1:22):  <= _empty_/qs19a.B#`<init>`().
         |[2:7..2:11): qs19 <= _empty_/qs19.
         |[3:6..3:7): a <= _empty_/qs19.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:23): qs19a => _empty_/qs19a.
         |[3:24..3:25): B => _empty_/qs19a.B#
         |[4:6..4:7): b <= _empty_/qs19.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:20): qs19a => _empty_/qs19a.
         |[4:21..4:22): B => _empty_/qs19a.B#
         |[5:6..5:7): c <= _empty_/qs19.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:51): qs19a => _empty_/qs19a.
         |[5:52..5:53): B => _empty_/qs19a.B#
         |[6:6..6:7): d <= _empty_/qs19.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:42): qs19a => _empty_/qs19a.
         |[6:43..6:44): B => _empty_/qs19a.B#
         |[7:6..7:7): e <= _empty_/qs19.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:30): qs19a => _empty_/qs19a.
         |[7:31..7:32): B => _empty_/qs19a.B#
         |[7:34..7:37): Int => scala/Int#
         |[7:41..7:80): (x: qs19a.B) => x match { case _ => 1 } <= local2
         |[7:42..7:43): x <= local9
         |[7:57..7:58): x => local9
         |[8:6..8:7): f <= _empty_/qs19.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:36): qs19a => _empty_/qs19a.
         |[8:37..8:38): B => _empty_/qs19a.B#
         |[9:6..9:7): g <= _empty_/qs19.g.
         |[9:10..9:14): List => scala/collection/immutable/Nil.
         |[9:15..9:20): qs19a => _empty_/qs19a.
         |""".stripMargin,
  )

  occurrences(
    "a type through nested objects",
    """|object qs20a { object o { object p { class T } } }
       |object qs20 {
       |  val a = classOf[qs20a.o.p.T]
       |  val b = List[qs20a.o.p.T]()
       |  val c = classOf[List[X] forSome { type X <: qs20a.o.p.T }]
       |  val d: List[X] forSome { type X <: qs20a.o.p.T } = null
       |  val e: PartialFunction[qs20a.o.p.T, Int] = (x: qs20a.o.p.T) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: qs20a.o.p.T }]()
       |  val g = List[qs20a.o.p.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:12): qs20a <= _empty_/qs20a.
         |[0:22..0:23): o <= _empty_/qs20a.o.
         |[0:33..0:34): p <= _empty_/qs20a.o.p.
         |[0:43..0:44): T <= _empty_/qs20a.o.p.T#
         |[0:44..0:44):  <= _empty_/qs20a.o.p.T#`<init>`().
         |[1:7..1:11): qs20 <= _empty_/qs20.
         |[2:6..2:7): a <= _empty_/qs20.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:23): qs20a => _empty_/qs20a.
         |[2:24..2:25): o => _empty_/qs20a.o.
         |[2:26..2:27): p => _empty_/qs20a.o.p.
         |[2:28..2:29): T => _empty_/qs20a.o.p.T#
         |[3:6..3:7): b <= _empty_/qs20.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:20): qs20a => _empty_/qs20a.
         |[3:21..3:22): o => _empty_/qs20a.o.
         |[3:23..3:24): p => _empty_/qs20a.o.p.
         |[3:25..3:26): T => _empty_/qs20a.o.p.T#
         |[4:6..4:7): c <= _empty_/qs20.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:51): qs20a => _empty_/qs20a.
         |[4:52..4:53): o => _empty_/qs20a.o.
         |[4:54..4:55): p => _empty_/qs20a.o.p.
         |[4:56..4:57): T => _empty_/qs20a.o.p.T#
         |[5:6..5:7): d <= _empty_/qs20.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:42): qs20a => _empty_/qs20a.
         |[5:43..5:44): o => _empty_/qs20a.o.
         |[5:45..5:46): p => _empty_/qs20a.o.p.
         |[5:47..5:48): T => _empty_/qs20a.o.p.T#
         |[6:6..6:7): e <= _empty_/qs20.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:30): qs20a => _empty_/qs20a.
         |[6:31..6:32): o => _empty_/qs20a.o.
         |[6:33..6:34): p => _empty_/qs20a.o.p.
         |[6:35..6:36): T => _empty_/qs20a.o.p.T#
         |[6:38..6:41): Int => scala/Int#
         |[6:45..6:88): (x: qs20a.o.p.T) => x match { case _ => 1 } <= local2
         |[6:46..6:47): x <= local9
         |[6:49..6:54): qs20a => _empty_/qs20a.
         |[6:55..6:56): o => _empty_/qs20a.o.
         |[6:57..6:58): p => _empty_/qs20a.o.p.
         |[6:59..6:60): T => _empty_/qs20a.o.p.T#
         |[6:65..6:66): x => local9
         |[7:6..7:7): f <= _empty_/qs20.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:36): qs20a => _empty_/qs20a.
         |[7:37..7:38): o => _empty_/qs20a.o.
         |[7:39..7:40): p => _empty_/qs20a.o.p.
         |[7:41..7:42): T => _empty_/qs20a.o.p.T#
         |[8:6..8:7): g <= _empty_/qs20.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:20): qs20a => _empty_/qs20a.
         |[8:21..8:22): o => _empty_/qs20a.o.
         |[8:23..8:24): p => _empty_/qs20a.o.p.
         |""".stripMargin
    else
      """|[0:7..0:12): qs20a <= _empty_/qs20a.
         |[0:22..0:23): o <= _empty_/qs20a.o.
         |[0:33..0:34): p <= _empty_/qs20a.o.p.
         |[0:43..0:44): T <= _empty_/qs20a.o.p.T#
         |[0:44..0:44):  <= _empty_/qs20a.o.p.T#`<init>`().
         |[1:7..1:11): qs20 <= _empty_/qs20.
         |[2:6..2:7): a <= _empty_/qs20.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:23): qs20a => _empty_/qs20a.
         |[2:24..2:25): o => _empty_/qs20a.o.
         |[2:26..2:27): p => _empty_/qs20a.o.p.
         |[2:28..2:29): T => _empty_/qs20a.o.p.T#
         |[3:6..3:7): b <= _empty_/qs20.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:20): qs20a => _empty_/qs20a.
         |[3:21..3:22): o => _empty_/qs20a.o.
         |[3:23..3:24): p => _empty_/qs20a.o.p.
         |[3:25..3:26): T => _empty_/qs20a.o.p.T#
         |[4:6..4:7): c <= _empty_/qs20.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:51): qs20a => _empty_/qs20a.
         |[4:52..4:53): o => _empty_/qs20a.o.
         |[4:54..4:55): p => _empty_/qs20a.o.p.
         |[4:56..4:57): T => _empty_/qs20a.o.p.T#
         |[5:6..5:7): d <= _empty_/qs20.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:42): qs20a => _empty_/qs20a.
         |[5:43..5:44): o => _empty_/qs20a.o.
         |[5:45..5:46): p => _empty_/qs20a.o.p.
         |[5:47..5:48): T => _empty_/qs20a.o.p.T#
         |[6:6..6:7): e <= _empty_/qs20.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:30): qs20a => _empty_/qs20a.
         |[6:31..6:32): o => _empty_/qs20a.o.
         |[6:33..6:34): p => _empty_/qs20a.o.p.
         |[6:35..6:36): T => _empty_/qs20a.o.p.T#
         |[6:38..6:41): Int => scala/Int#
         |[6:45..6:88): (x: qs20a.o.p.T) => x match { case _ => 1 } <= local2
         |[6:46..6:47): x <= local9
         |[6:65..6:66): x => local9
         |[7:6..7:7): f <= _empty_/qs20.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:36): qs20a => _empty_/qs20a.
         |[7:37..7:38): o => _empty_/qs20a.o.
         |[7:39..7:40): p => _empty_/qs20a.o.p.
         |[7:41..7:42): T => _empty_/qs20a.o.p.T#
         |[8:6..8:7): g <= _empty_/qs20.g.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:20): qs20a => _empty_/qs20a.
         |[8:21..8:22): o => _empty_/qs20a.o.
         |[8:23..8:24): p => _empty_/qs20a.o.p.
         |""".stripMargin,
  )

  occurrences(
    "a type through nested objects from an imported object",
    """|object qs21a { object o { object p { class T } } }
       |object qs21 {
       |  import qs21a.o
       |  val a = classOf[o.p.T]
       |  val b = List[o.p.T]()
       |  val c = classOf[List[X] forSome { type X <: o.p.T }]
       |  val d: List[X] forSome { type X <: o.p.T } = null
       |  val e: PartialFunction[o.p.T, Int] = (x: o.p.T) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: o.p.T }]()
       |  val g = List[o.p.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:12): qs21a <= _empty_/qs21a.
         |[0:22..0:23): o <= _empty_/qs21a.o.
         |[0:33..0:34): p <= _empty_/qs21a.o.p.
         |[0:43..0:44): T <= _empty_/qs21a.o.p.T#
         |[0:44..0:44):  <= _empty_/qs21a.o.p.T#`<init>`().
         |[1:7..1:11): qs21 <= _empty_/qs21.
         |[2:9..2:14): qs21a => _empty_/qs21a.
         |[2:15..2:16): o => _empty_/qs21a.o.
         |[3:6..3:7): a <= _empty_/qs21.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:19): o => _empty_/qs21a.o.
         |[3:20..3:21): p => _empty_/qs21a.o.p.
         |[3:22..3:23): T => _empty_/qs21a.o.p.T#
         |[4:6..4:7): b <= _empty_/qs21.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:16): o => _empty_/qs21a.o.
         |[4:17..4:18): p => _empty_/qs21a.o.p.
         |[4:19..4:20): T => _empty_/qs21a.o.p.T#
         |[5:6..5:7): c <= _empty_/qs21.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:47): o => _empty_/qs21a.o.
         |[5:48..5:49): p => _empty_/qs21a.o.p.
         |[5:50..5:51): T => _empty_/qs21a.o.p.T#
         |[6:6..6:7): d <= _empty_/qs21.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:38): o => _empty_/qs21a.o.
         |[6:39..6:40): p => _empty_/qs21a.o.p.
         |[6:41..6:42): T => _empty_/qs21a.o.p.T#
         |[7:6..7:7): e <= _empty_/qs21.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:26): o => _empty_/qs21a.o.
         |[7:27..7:28): p => _empty_/qs21a.o.p.
         |[7:29..7:30): T => _empty_/qs21a.o.p.T#
         |[7:32..7:35): Int => scala/Int#
         |[7:39..7:76): (x: o.p.T) => x match { case _ => 1 } <= local2
         |[7:40..7:41): x <= local9
         |[7:43..7:44): o => _empty_/qs21a.o.
         |[7:45..7:46): p => _empty_/qs21a.o.p.
         |[7:47..7:48): T => _empty_/qs21a.o.p.T#
         |[7:53..7:54): x => local9
         |[8:6..8:7): f <= _empty_/qs21.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:32): o => _empty_/qs21a.o.
         |[8:33..8:34): p => _empty_/qs21a.o.p.
         |[8:35..8:36): T => _empty_/qs21a.o.p.T#
         |[9:6..9:7): g <= _empty_/qs21.g.
         |[9:10..9:14): List => scala/collection/immutable/Nil.
         |[9:15..9:16): o => _empty_/qs21a.o.
         |[9:17..9:18): p => _empty_/qs21a.o.p.
         |""".stripMargin
    else
      """|[0:7..0:12): qs21a <= _empty_/qs21a.
         |[0:22..0:23): o <= _empty_/qs21a.o.
         |[0:33..0:34): p <= _empty_/qs21a.o.p.
         |[0:43..0:44): T <= _empty_/qs21a.o.p.T#
         |[0:44..0:44):  <= _empty_/qs21a.o.p.T#`<init>`().
         |[1:7..1:11): qs21 <= _empty_/qs21.
         |[2:9..2:14): qs21a => _empty_/qs21a.
         |[2:15..2:16): o => _empty_/qs21a.o.
         |[3:6..3:7): a <= _empty_/qs21.a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:19): o => _empty_/qs21a.o.
         |[3:20..3:21): p => _empty_/qs21a.o.p.
         |[3:22..3:23): T => _empty_/qs21a.o.p.T#
         |[4:6..4:7): b <= _empty_/qs21.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:16): o => _empty_/qs21a.o.
         |[4:17..4:18): p => _empty_/qs21a.o.p.
         |[4:19..4:20): T => _empty_/qs21a.o.p.T#
         |[5:6..5:7): c <= _empty_/qs21.c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:47): o => _empty_/qs21a.o.
         |[5:48..5:49): p => _empty_/qs21a.o.p.
         |[5:50..5:51): T => _empty_/qs21a.o.p.T#
         |[6:6..6:7): d <= _empty_/qs21.d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:38): o => _empty_/qs21a.o.
         |[6:39..6:40): p => _empty_/qs21a.o.p.
         |[6:41..6:42): T => _empty_/qs21a.o.p.T#
         |[7:6..7:7): e <= _empty_/qs21.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:26): o => _empty_/qs21a.o.
         |[7:27..7:28): p => _empty_/qs21a.o.p.
         |[7:29..7:30): T => _empty_/qs21a.o.p.T#
         |[7:32..7:35): Int => scala/Int#
         |[7:39..7:76): (x: o.p.T) => x match { case _ => 1 } <= local2
         |[7:40..7:41): x <= local9
         |[7:53..7:54): x => local9
         |[8:6..8:7): f <= _empty_/qs21.f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:32): o => _empty_/qs21a.o.
         |[8:33..8:34): p => _empty_/qs21a.o.p.
         |[8:35..8:36): T => _empty_/qs21a.o.p.T#
         |[9:6..9:7): g <= _empty_/qs21.g.
         |[9:10..9:14): List => scala/collection/immutable/Nil.
         |[9:15..9:16): o => _empty_/qs21a.o.
         |[9:17..9:18): p => _empty_/qs21a.o.p.
         |""".stripMargin,
  )

  occurrences(
    "an alias of a class of a nested object, through an object of that nested object's name",
    """|object qs72 {
       |  object w { object q { class U } }
       |  object q { type T = w.q.U }
       |  val a = classOf[q.T]
       |  val b = List[q.T]()
       |  val f = List[AnyRef { def m: q.T }]()
       |}
       |""".stripMargin,
    """|[0:7..0:11): qs72 <= _empty_/qs72.
       |[1:9..1:10): w <= _empty_/qs72.w.
       |[1:20..1:21): q <= _empty_/qs72.w.q.
       |[1:30..1:31): U <= _empty_/qs72.w.q.U#
       |[1:31..1:31):  <= _empty_/qs72.w.q.U#`<init>`().
       |[2:9..2:10): q <= _empty_/qs72.q.
       |[2:18..2:19): T <= _empty_/qs72.q.T#
       |[2:22..2:23): w => _empty_/qs72.w.
       |[2:24..2:25): q => _empty_/qs72.w.q.
       |[2:26..2:27): U => _empty_/qs72.w.q.U#
       |[3:6..3:7): a <= _empty_/qs72.a.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:19): q => _empty_/qs72.q.
       |[3:20..3:21): T => _empty_/qs72.q.T#
       |[4:6..4:7): b <= _empty_/qs72.b.
       |[4:10..4:14): List => scala/collection/immutable/Nil.
       |[4:15..4:16): q => _empty_/qs72.w.q.
       |[4:17..4:18): T => _empty_/qs72.w.q.U#
       |[5:6..5:7): f <= _empty_/qs72.f.
       |[5:10..5:14): List => scala/collection/immutable/Nil.
       |[5:15..5:21): AnyRef => java/lang/Object#
       |[5:28..5:29): m <= local0
       |[5:31..5:32): q => _empty_/qs72.w.q.
       |[5:33..5:34): T => _empty_/qs72.w.q.U#
       |""".stripMargin,
  )

  occurrences(
    "an alias of a class of the same object",
    """|object qs73 {
       |  object o { class U; type T = U }
       |  val a = classOf[o.T]
       |  val b = List[o.T]()
       |  val f = List[AnyRef { def m: o.T }]()
       |}
       |""".stripMargin,
    """|[0:7..0:11): qs73 <= _empty_/qs73.
       |[1:9..1:10): o <= _empty_/qs73.o.
       |[1:19..1:20): U <= _empty_/qs73.o.U#
       |[1:20..1:20):  <= _empty_/qs73.o.U#`<init>`().
       |[1:27..1:28): T <= _empty_/qs73.o.T#
       |[1:31..1:32): U => _empty_/qs73.o.U#
       |[2:6..2:7): a <= _empty_/qs73.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:19): o => _empty_/qs73.o.
       |[2:20..2:21): T => _empty_/qs73.o.T#
       |[3:6..3:7): b <= _empty_/qs73.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:16): o => _empty_/qs73.o.
       |[3:17..3:18): T => _empty_/qs73.o.U#
       |[4:6..4:7): f <= _empty_/qs73.f.
       |[4:10..4:14): List => scala/collection/immutable/Nil.
       |[4:15..4:21): AnyRef => java/lang/Object#
       |[4:28..4:29): m <= local0
       |[4:31..4:32): o => _empty_/qs73.o.
       |[4:33..4:34): T => _empty_/qs73.o.U#
       |""".stripMargin,
  )

  occurrences(
    "a static class of a Java class",
    """|object qs22 {
       |  val a = classOf[java.util.Map.Entry[Int, Int]]
       |  val b = List[java.util.Map.Entry[Int, Int]]()
       |  val c = classOf[List[X] forSome { type X <: java.util.Map.Entry[Int, Int] }]
       |  val d: List[X] forSome { type X <: java.util.Map.Entry[Int, Int] } = null
       |  val e: PartialFunction[java.util.Map.Entry[Int, Int], Int] = (x: java.util.Map.Entry[Int, Int]) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: java.util.Map.Entry[Int, Int] }]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): qs22 <= _empty_/qs22.
         |[1:6..1:7): a <= _empty_/qs22.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:22): java => java/
         |[1:23..1:27): util => java/util/
         |[1:28..1:31): Map => java/util/Map#
         |[1:32..1:37): Entry => java/util/Map#Entry#
         |[1:38..1:41): Int => scala/Int#
         |[1:43..1:46): Int => scala/Int#
         |[2:6..2:7): b <= _empty_/qs22.b.
         |[2:10..2:14): List => scala/collection/immutable/Nil.
         |[2:15..2:19): java => java/
         |[2:20..2:24): util => java/util/
         |[2:25..2:28): Map => java/util/Map#
         |[2:29..2:34): Entry => java/util/Map#Entry#
         |[2:35..2:38): Int => scala/Int#
         |[2:40..2:43): Int => scala/Int#
         |[3:6..3:7): c <= _empty_/qs22.c.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:22): List => scala/package.List#
         |[3:23..3:24): X => local0
         |[3:41..3:42): X <= local0
         |[3:46..3:50): java => java/
         |[3:51..3:55): util => java/util/
         |[3:56..3:59): Map => java/util/Map#
         |[3:60..3:65): Entry => java/util/Map#Entry#
         |[3:66..3:69): Int => scala/Int#
         |[3:71..3:74): Int => scala/Int#
         |[4:6..4:7): d <= _empty_/qs22.d.
         |[4:9..4:13): List => scala/package.List#
         |[4:14..4:15): X => local1
         |[4:32..4:33): X <= local1
         |[4:37..4:41): java => java/
         |[4:42..4:46): util => java/util/
         |[4:47..4:50): Map => java/util/Map#
         |[4:51..4:56): Entry => java/util/Map#Entry#
         |[4:57..4:60): Int => scala/Int#
         |[4:62..4:65): Int => scala/Int#
         |[5:6..5:7): e <= _empty_/qs22.e.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:29): java => java/
         |[5:30..5:34): util => java/util/
         |[5:35..5:38): Map => java/util/Map#
         |[5:39..5:44): Entry => java/util/Map#Entry#
         |[5:45..5:48): Int => scala/Int#
         |[5:50..5:53): Int => scala/Int#
         |[5:56..5:59): Int => scala/Int#
         |[5:63..5:124): (x: java.util.Map.Entry[Int, Int]) => x match { case _ => 1 } <= local2
         |[5:64..5:65): x <= local9
         |[5:67..5:71): java => java/
         |[5:72..5:76): util => java/util/
         |[5:77..5:80): Map => java/util/Map#
         |[5:81..5:86): Entry => java/util/Map#Entry#
         |[5:87..5:90): Int => scala/Int#
         |[5:92..5:95): Int => scala/Int#
         |[5:101..5:102): x => local9
         |[6:6..6:7): f <= _empty_/qs22.f.
         |[6:10..6:14): List => scala/collection/immutable/Nil.
         |[6:15..6:21): AnyRef => java/lang/Object#
         |[6:28..6:29): m <= local10
         |[6:31..6:35): java => java/
         |[6:36..6:40): util => java/util/
         |[6:41..6:44): Map => java/util/Map#
         |[6:45..6:50): Entry => java/util/Map#Entry#
         |[6:51..6:54): Int => scala/Int#
         |[6:56..6:59): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:7..0:11): qs22 <= _empty_/qs22.
         |[1:6..1:7): a <= _empty_/qs22.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:22): java => java/
         |[1:23..1:27): util => java/util/
         |[1:28..1:31): Map => java/util/Map#
         |[1:32..1:37): Entry => java/util/Map#Entry#
         |[1:38..1:41): Int => scala/Int#
         |[1:43..1:46): Int => scala/Int#
         |[2:6..2:7): b <= _empty_/qs22.b.
         |[2:10..2:14): List => scala/collection/immutable/Nil.
         |[2:15..2:19): java => java/
         |[2:20..2:24): util => java/util/
         |[2:25..2:28): Map => java/util/Map#
         |[2:29..2:34): Entry => java/util/Map#Entry#
         |[2:35..2:38): Int => scala/Int#
         |[2:40..2:43): Int => scala/Int#
         |[3:6..3:7): c <= _empty_/qs22.c.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:22): List => scala/package.List#
         |[3:23..3:24): X => local0
         |[3:41..3:42): X <= local0
         |[3:46..3:50): java => java/
         |[3:51..3:55): util => java/util/
         |[3:56..3:59): Map => java/util/Map#
         |[3:60..3:65): Entry => java/util/Map#Entry#
         |[3:66..3:69): Int => scala/Int#
         |[3:71..3:74): Int => scala/Int#
         |[4:6..4:7): d <= _empty_/qs22.d.
         |[4:9..4:13): List => scala/package.List#
         |[4:14..4:15): X => local1
         |[4:32..4:33): X <= local1
         |[4:37..4:41): java => java/
         |[4:42..4:46): util => java/util/
         |[4:47..4:50): Map => java/util/Map#
         |[4:51..4:56): Entry => java/util/Map#Entry#
         |[4:57..4:60): Int => scala/Int#
         |[4:62..4:65): Int => scala/Int#
         |[5:6..5:7): e <= _empty_/qs22.e.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:29): java => java/
         |[5:30..5:34): util => java/util/
         |[5:35..5:38): Map => java/util/Map#
         |[5:39..5:44): Entry => java/util/Map#Entry#
         |[5:45..5:48): Int => scala/Int#
         |[5:50..5:53): Int => scala/Int#
         |[5:56..5:59): Int => scala/Int#
         |[5:63..5:124): (x: java.util.Map.Entry[Int, Int]) => x match { case _ => 1 } <= local2
         |[5:64..5:65): x <= local9
         |[5:101..5:102): x => local9
         |[6:6..6:7): f <= _empty_/qs22.f.
         |[6:10..6:14): List => scala/collection/immutable/Nil.
         |[6:15..6:21): AnyRef => java/lang/Object#
         |[6:28..6:29): m <= local10
         |[6:31..6:35): java => java/
         |[6:36..6:40): util => java/util/
         |[6:41..6:44): Map => java/util/Map#
         |[6:45..6:50): Entry => java/util/Map#Entry#
         |[6:51..6:54): Int => scala/Int#
         |[6:56..6:59): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "a static class of an imported Java class",
    """|object qs23 {
       |  import java.util.Map
       |  val a = classOf[Map.Entry[Int, Int]]
       |  val b = List[Map.Entry[Int, Int]]()
       |  val c = classOf[List[X] forSome { type X <: Map.Entry[Int, Int] }]
       |  val d: List[X] forSome { type X <: Map.Entry[Int, Int] } = null
       |  val e: PartialFunction[Map.Entry[Int, Int], Int] = (x: Map.Entry[Int, Int]) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: Map.Entry[Int, Int] }]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): qs23 <= _empty_/qs23.
         |[1:9..1:13): java => java/
         |[1:14..1:18): util => java/util/
         |[1:19..1:22): Map => java/util/Map#
         |[2:6..2:7): a <= _empty_/qs23.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:21): Map => java/util/Map#
         |[2:22..2:27): Entry => java/util/Map#Entry#
         |[2:28..2:31): Int => scala/Int#
         |[2:33..2:36): Int => scala/Int#
         |[3:6..3:7): b <= _empty_/qs23.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:18): Map => java/util/Map#
         |[3:19..3:24): Entry => java/util/Map#Entry#
         |[3:25..3:28): Int => scala/Int#
         |[3:30..3:33): Int => scala/Int#
         |[4:6..4:7): c <= _empty_/qs23.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:49): Map => java/util/Map#
         |[4:50..4:55): Entry => java/util/Map#Entry#
         |[4:56..4:59): Int => scala/Int#
         |[4:61..4:64): Int => scala/Int#
         |[5:6..5:7): d <= _empty_/qs23.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:40): Map => java/util/Map#
         |[5:41..5:46): Entry => java/util/Map#Entry#
         |[5:47..5:50): Int => scala/Int#
         |[5:52..5:55): Int => scala/Int#
         |[6:6..6:7): e <= _empty_/qs23.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:28): Map => java/util/Map#
         |[6:29..6:34): Entry => java/util/Map#Entry#
         |[6:35..6:38): Int => scala/Int#
         |[6:40..6:43): Int => scala/Int#
         |[6:46..6:49): Int => scala/Int#
         |[6:53..6:104): (x: Map.Entry[Int, Int]) => x match { case _ => 1 } <= local2
         |[6:54..6:55): x <= local9
         |[6:57..6:60): Map => java/util/Map#
         |[6:61..6:66): Entry => java/util/Map#Entry#
         |[6:67..6:70): Int => scala/Int#
         |[6:72..6:75): Int => scala/Int#
         |[6:81..6:82): x => local9
         |[7:6..7:7): f <= _empty_/qs23.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:34): Map => java/util/Map#
         |[7:35..7:40): Entry => java/util/Map#Entry#
         |[7:41..7:44): Int => scala/Int#
         |[7:46..7:49): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:7..0:11): qs23 <= _empty_/qs23.
         |[1:9..1:13): java => java/
         |[1:14..1:18): util => java/util/
         |[1:19..1:22): Map => java/util/Map#
         |[2:6..2:7): a <= _empty_/qs23.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:21): Map => java/util/Map#
         |[2:22..2:27): Entry => java/util/Map#Entry#
         |[2:28..2:31): Int => scala/Int#
         |[2:33..2:36): Int => scala/Int#
         |[3:6..3:7): b <= _empty_/qs23.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:18): Map => java/util/Map#
         |[3:19..3:24): Entry => java/util/Map#Entry#
         |[3:25..3:28): Int => scala/Int#
         |[3:30..3:33): Int => scala/Int#
         |[4:6..4:7): c <= _empty_/qs23.c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:18..4:22): List => scala/package.List#
         |[4:23..4:24): X => local0
         |[4:41..4:42): X <= local0
         |[4:46..4:49): Map => java/util/Map#
         |[4:50..4:55): Entry => java/util/Map#Entry#
         |[4:56..4:59): Int => scala/Int#
         |[4:61..4:64): Int => scala/Int#
         |[5:6..5:7): d <= _empty_/qs23.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): X => local1
         |[5:32..5:33): X <= local1
         |[5:37..5:40): Map => java/util/Map#
         |[5:41..5:46): Entry => java/util/Map#Entry#
         |[5:47..5:50): Int => scala/Int#
         |[5:52..5:55): Int => scala/Int#
         |[6:6..6:7): e <= _empty_/qs23.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:28): Map => java/util/Map#
         |[6:29..6:34): Entry => java/util/Map#Entry#
         |[6:35..6:38): Int => scala/Int#
         |[6:40..6:43): Int => scala/Int#
         |[6:46..6:49): Int => scala/Int#
         |[6:53..6:104): (x: Map.Entry[Int, Int]) => x match { case _ => 1 } <= local2
         |[6:54..6:55): x <= local9
         |[6:81..6:82): x => local9
         |[7:6..7:7): f <= _empty_/qs23.f.
         |[7:10..7:14): List => scala/collection/immutable/Nil.
         |[7:15..7:21): AnyRef => java/lang/Object#
         |[7:28..7:29): m <= local10
         |[7:31..7:34): Map => java/util/Map#
         |[7:35..7:40): Entry => java/util/Map#Entry#
         |[7:41..7:44): Int => scala/Int#
         |[7:46..7:49): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "a type through a stable value",
    """|object qs24 {
       |  class X { class T }
       |  val v = new X
       |  object o { val u = new X }
       |  val a1 = classOf[v.T]
       |  val b1 = List[v.T]()
       |  val c1 = classOf[List[X] forSome { type X <: v.T }]
       |  val d1: List[X] forSome { type X <: v.T } = null
       |  val e1: PartialFunction[v.T, Int] = (x: v.T) => x match { case _ => 1 }
       |  val f1 = List[AnyRef { def m: v.T }]()
       |  val g1 = List[v.type]()
       |  val a2 = classOf[o.u.T]
       |  val b2 = List[o.u.T]()
       |  val c2 = classOf[List[X] forSome { type X <: o.u.T }]
       |  val d2: List[X] forSome { type X <: o.u.T } = null
       |  val e2: PartialFunction[o.u.T, Int] = (x: o.u.T) => x match { case _ => 1 }
       |  val f2 = List[AnyRef { def m: o.u.T }]()
       |  val g2 = List[o.u.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): qs24 <= _empty_/qs24.
         |[1:8..1:9): X <= _empty_/qs24.X#
         |[1:10..1:10):  <= _empty_/qs24.X#`<init>`().
         |[1:18..1:19): T <= _empty_/qs24.X#T#
         |[1:19..1:19):  <= _empty_/qs24.X#T#`<init>`().
         |[2:6..2:7): v <= _empty_/qs24.v.
         |[2:14..2:15): X => _empty_/qs24.X#
         |[2:15..2:15):  => _empty_/qs24.X#`<init>`().
         |[3:9..3:10): o <= _empty_/qs24.o.
         |[3:17..3:18): u <= _empty_/qs24.o.u.
         |[3:25..3:26): X => _empty_/qs24.X#
         |[3:26..3:26):  => _empty_/qs24.X#`<init>`().
         |[4:6..4:8): a1 <= _empty_/qs24.a1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:20): v => _empty_/qs24.v.
         |[4:21..4:22): T => _empty_/qs24.X#T#
         |[5:6..5:8): b1 <= _empty_/qs24.b1.
         |[5:11..5:15): List => scala/collection/immutable/Nil.
         |[5:16..5:17): v => _empty_/qs24.v.
         |[5:18..5:19): T => _empty_/qs24.X#T#
         |[6:6..6:8): c1 <= _empty_/qs24.c1.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:19..6:23): List => scala/package.List#
         |[6:24..6:25): X => local0
         |[6:42..6:43): X <= local0
         |[6:47..6:48): v => _empty_/qs24.v.
         |[6:49..6:50): T => _empty_/qs24.X#T#
         |[7:6..7:8): d1 <= _empty_/qs24.d1.
         |[7:10..7:14): List => scala/package.List#
         |[7:15..7:16): X => local1
         |[7:33..7:34): X <= local1
         |[7:38..7:39): v => _empty_/qs24.v.
         |[7:40..7:41): T => _empty_/qs24.X#T#
         |[8:6..8:8): e1 <= _empty_/qs24.e1.
         |[8:10..8:25): PartialFunction => scala/PartialFunction#
         |[8:26..8:27): v => _empty_/qs24.v.
         |[8:28..8:29): T => _empty_/qs24.X#T#
         |[8:31..8:34): Int => scala/Int#
         |[8:38..8:73): (x: v.T) => x match { case _ => 1 } <= local2
         |[8:39..8:40): x <= local9
         |[8:42..8:43): v => _empty_/qs24.v.
         |[8:44..8:45): T => _empty_/qs24.X#T#
         |[8:50..8:51): x => local9
         |[9:6..9:8): f1 <= _empty_/qs24.f1.
         |[9:11..9:15): List => scala/collection/immutable/Nil.
         |[9:16..9:22): AnyRef => java/lang/Object#
         |[9:29..9:30): m <= local10
         |[9:32..9:33): v => _empty_/qs24.v.
         |[9:34..9:35): T => _empty_/qs24.X#T#
         |[10:6..10:8): g1 <= _empty_/qs24.g1.
         |[10:11..10:15): List => scala/collection/immutable/Nil.
         |[10:16..10:17): v => _empty_/qs24.v.
         |[11:6..11:8): a2 <= _empty_/qs24.a2.
         |[11:11..11:18): classOf => scala/Predef.classOf().
         |[11:19..11:20): o => _empty_/qs24.o.
         |[11:21..11:22): u => _empty_/qs24.o.u.
         |[11:23..11:24): T => _empty_/qs24.X#T#
         |[12:6..12:8): b2 <= _empty_/qs24.b2.
         |[12:11..12:15): List => scala/collection/immutable/Nil.
         |[12:16..12:17): o => _empty_/qs24.o.
         |[12:18..12:19): u => _empty_/qs24.o.u.
         |[12:20..12:21): T => _empty_/qs24.X#T#
         |[13:6..13:8): c2 <= _empty_/qs24.c2.
         |[13:11..13:18): classOf => scala/Predef.classOf().
         |[13:19..13:23): List => scala/package.List#
         |[13:24..13:25): X => local11
         |[13:42..13:43): X <= local11
         |[13:47..13:48): o => _empty_/qs24.o.
         |[13:49..13:50): u => _empty_/qs24.o.u.
         |[13:51..13:52): T => _empty_/qs24.X#T#
         |[14:6..14:8): d2 <= _empty_/qs24.d2.
         |[14:10..14:14): List => scala/package.List#
         |[14:15..14:16): X => local12
         |[14:33..14:34): X <= local12
         |[14:38..14:39): o => _empty_/qs24.o.
         |[14:40..14:41): u => _empty_/qs24.o.u.
         |[14:42..14:43): T => _empty_/qs24.X#T#
         |[15:6..15:8): e2 <= _empty_/qs24.e2.
         |[15:10..15:25): PartialFunction => scala/PartialFunction#
         |[15:26..15:27): o => _empty_/qs24.o.
         |[15:28..15:29): u => _empty_/qs24.o.u.
         |[15:30..15:31): T => _empty_/qs24.X#T#
         |[15:33..15:36): Int => scala/Int#
         |[15:40..15:77): (x: o.u.T) => x match { case _ => 1 } <= local13
         |[15:41..15:42): x <= local20
         |[15:44..15:45): o => _empty_/qs24.o.
         |[15:46..15:47): u => _empty_/qs24.o.u.
         |[15:48..15:49): T => _empty_/qs24.X#T#
         |[15:54..15:55): x => local20
         |[16:6..16:8): f2 <= _empty_/qs24.f2.
         |[16:11..16:15): List => scala/collection/immutable/Nil.
         |[16:16..16:22): AnyRef => java/lang/Object#
         |[16:29..16:30): m <= local21
         |[16:32..16:33): o => _empty_/qs24.o.
         |[16:34..16:35): u => _empty_/qs24.o.u.
         |[16:36..16:37): T => _empty_/qs24.X#T#
         |[17:6..17:8): g2 <= _empty_/qs24.g2.
         |[17:11..17:15): List => scala/collection/immutable/Nil.
         |[17:16..17:17): o => _empty_/qs24.o.
         |[17:18..17:19): u => _empty_/qs24.o.u.
         |""".stripMargin
    else
      """|[0:7..0:11): qs24 <= _empty_/qs24.
         |[1:8..1:9): X <= _empty_/qs24.X#
         |[1:10..1:10):  <= _empty_/qs24.X#`<init>`().
         |[1:18..1:19): T <= _empty_/qs24.X#T#
         |[1:19..1:19):  <= _empty_/qs24.X#T#`<init>`().
         |[2:6..2:7): v <= _empty_/qs24.v.
         |[2:14..2:15): X => _empty_/qs24.X#
         |[2:15..2:15):  => _empty_/qs24.X#`<init>`().
         |[3:9..3:10): o <= _empty_/qs24.o.
         |[3:17..3:18): u <= _empty_/qs24.o.u.
         |[3:25..3:26): X => _empty_/qs24.X#
         |[3:26..3:26):  => _empty_/qs24.X#`<init>`().
         |[4:6..4:8): a1 <= _empty_/qs24.a1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:20): v => _empty_/qs24.v.
         |[4:21..4:22): T => _empty_/qs24.X#T#
         |[5:6..5:8): b1 <= _empty_/qs24.b1.
         |[5:11..5:15): List => scala/collection/immutable/Nil.
         |[5:16..5:17): v => _empty_/qs24.v.
         |[5:18..5:19): T => _empty_/qs24.X#T#
         |[6:6..6:8): c1 <= _empty_/qs24.c1.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:19..6:23): List => scala/package.List#
         |[6:24..6:25): X => local0
         |[6:42..6:43): X <= local0
         |[6:47..6:48): v => _empty_/qs24.v.
         |[6:49..6:50): T => _empty_/qs24.X#T#
         |[7:6..7:8): d1 <= _empty_/qs24.d1.
         |[7:10..7:14): List => scala/package.List#
         |[7:15..7:16): X => local1
         |[7:33..7:34): X <= local1
         |[7:38..7:39): v => _empty_/qs24.v.
         |[7:40..7:41): T => _empty_/qs24.X#T#
         |[8:6..8:8): e1 <= _empty_/qs24.e1.
         |[8:10..8:25): PartialFunction => scala/PartialFunction#
         |[8:26..8:27): v => _empty_/qs24.v.
         |[8:28..8:29): T => _empty_/qs24.X#T#
         |[8:31..8:34): Int => scala/Int#
         |[8:38..8:73): (x: v.T) => x match { case _ => 1 } <= local2
         |[8:39..8:40): x <= local9
         |[8:50..8:51): x => local9
         |[9:6..9:8): f1 <= _empty_/qs24.f1.
         |[9:11..9:15): List => scala/collection/immutable/Nil.
         |[9:16..9:22): AnyRef => java/lang/Object#
         |[9:29..9:30): m <= local10
         |[9:32..9:33): v => _empty_/qs24.v.
         |[9:34..9:35): T => _empty_/qs24.X#T#
         |[10:6..10:8): g1 <= _empty_/qs24.g1.
         |[10:11..10:15): List => scala/collection/immutable/Nil.
         |[10:16..10:17): v => _empty_/qs24.v.
         |[11:6..11:8): a2 <= _empty_/qs24.a2.
         |[11:11..11:18): classOf => scala/Predef.classOf().
         |[11:19..11:20): o => _empty_/qs24.o.
         |[11:21..11:22): u => _empty_/qs24.o.u.
         |[11:23..11:24): T => _empty_/qs24.X#T#
         |[12:6..12:8): b2 <= _empty_/qs24.b2.
         |[12:11..12:15): List => scala/collection/immutable/Nil.
         |[12:16..12:17): o => _empty_/qs24.o.
         |[12:18..12:19): u => _empty_/qs24.o.u.
         |[12:20..12:21): T => _empty_/qs24.X#T#
         |[13:6..13:8): c2 <= _empty_/qs24.c2.
         |[13:11..13:18): classOf => scala/Predef.classOf().
         |[13:19..13:23): List => scala/package.List#
         |[13:24..13:25): X => local11
         |[13:42..13:43): X <= local11
         |[13:47..13:48): o => _empty_/qs24.o.
         |[13:49..13:50): u => _empty_/qs24.o.u.
         |[13:51..13:52): T => _empty_/qs24.X#T#
         |[14:6..14:8): d2 <= _empty_/qs24.d2.
         |[14:10..14:14): List => scala/package.List#
         |[14:15..14:16): X => local12
         |[14:33..14:34): X <= local12
         |[14:38..14:39): o => _empty_/qs24.o.
         |[14:40..14:41): u => _empty_/qs24.o.u.
         |[14:42..14:43): T => _empty_/qs24.X#T#
         |[15:6..15:8): e2 <= _empty_/qs24.e2.
         |[15:10..15:25): PartialFunction => scala/PartialFunction#
         |[15:26..15:27): o => _empty_/qs24.o.
         |[15:28..15:29): u => _empty_/qs24.o.u.
         |[15:30..15:31): T => _empty_/qs24.X#T#
         |[15:33..15:36): Int => scala/Int#
         |[15:40..15:77): (x: o.u.T) => x match { case _ => 1 } <= local13
         |[15:41..15:42): x <= local20
         |[15:54..15:55): x => local20
         |[16:6..16:8): f2 <= _empty_/qs24.f2.
         |[16:11..16:15): List => scala/collection/immutable/Nil.
         |[16:16..16:22): AnyRef => java/lang/Object#
         |[16:29..16:30): m <= local21
         |[16:32..16:33): o => _empty_/qs24.o.
         |[16:34..16:35): u => _empty_/qs24.o.u.
         |[16:36..16:37): T => _empty_/qs24.X#T#
         |[17:6..17:8): g2 <= _empty_/qs24.g2.
         |[17:11..17:15): List => scala/collection/immutable/Nil.
         |[17:16..17:17): o => _empty_/qs24.o.
         |[17:18..17:19): u => _empty_/qs24.o.u.
         |""".stripMargin,
  )

  occurrences(
    "a type through this of an outer class in an inner class",
    """|class qs9 {
       |  class B
       |  class I {
       |    val a = classOf[qs9.this.B]
       |    val b = List[qs9.this.B]()
       |    val c = classOf[List[X] forSome { type X <: qs9.this.B }]
       |    val d: List[X] forSome { type X <: qs9.this.B } = null
       |    val e: PartialFunction[qs9.this.B, Int] = (x: qs9.this.B) => x match { case _ => 1 }
       |    val f = List[AnyRef { def m: qs9.this.B }]()
       |    val g = List[qs9.this.type]()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:9): qs9 <= _empty_/qs9#
         |[0:10..0:10):  <= _empty_/qs9#`<init>`().
         |[1:8..1:9): B <= _empty_/qs9#B#
         |[1:9..1:9):  <= _empty_/qs9#B#`<init>`().
         |[2:8..2:9): I <= _empty_/qs9#I#
         |[2:10..2:10):  <= _empty_/qs9#I#`<init>`().
         |[3:8..3:9): a <= _empty_/qs9#I#a.
         |[3:12..3:19): classOf => scala/Predef.classOf().
         |[3:20..3:23): qs9 => _empty_/qs9#
         |[3:29..3:30): B => _empty_/qs9#B#
         |[4:8..4:9): b <= _empty_/qs9#I#b.
         |[4:12..4:16): List => scala/collection/immutable/Nil.
         |[4:17..4:20): qs9 => _empty_/qs9#
         |[4:26..4:27): B => _empty_/qs9#B#
         |[5:8..5:9): c <= _empty_/qs9#I#c.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:24): List => scala/package.List#
         |[5:25..5:26): X => local0
         |[5:43..5:44): X <= local0
         |[5:48..5:51): qs9 => _empty_/qs9#
         |[5:57..5:58): B => _empty_/qs9#B#
         |[6:8..6:9): d <= _empty_/qs9#I#d.
         |[6:11..6:15): List => scala/package.List#
         |[6:16..6:17): X => local1
         |[6:34..6:35): X <= local1
         |[6:39..6:42): qs9 => _empty_/qs9#
         |[6:48..6:49): B => _empty_/qs9#B#
         |[7:8..7:9): e <= _empty_/qs9#I#e.
         |[7:11..7:26): PartialFunction => scala/PartialFunction#
         |[7:27..7:30): qs9 => _empty_/qs9#
         |[7:36..7:37): B => _empty_/qs9#B#
         |[7:39..7:42): Int => scala/Int#
         |[7:46..7:88): (x: qs9.this.B) => x match { case _ => 1 } <= local2
         |[7:47..7:48): x <= local9
         |[7:50..7:53): qs9 => _empty_/qs9#
         |[7:59..7:60): B => _empty_/qs9#B#
         |[7:65..7:66): x => local9
         |[8:8..8:9): f <= _empty_/qs9#I#f.
         |[8:12..8:16): List => scala/collection/immutable/Nil.
         |[8:17..8:23): AnyRef => java/lang/Object#
         |[8:30..8:31): m <= local10
         |[8:33..8:36): qs9 => _empty_/qs9#
         |[8:42..8:43): B => _empty_/qs9#B#
         |[9:8..9:9): g <= _empty_/qs9#I#g.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:20): qs9 => _empty_/qs9#
         |""".stripMargin
    else
      """|[0:6..0:9): qs9 <= _empty_/qs9#
         |[0:10..0:10):  <= _empty_/qs9#`<init>`().
         |[1:8..1:9): B <= _empty_/qs9#B#
         |[1:9..1:9):  <= _empty_/qs9#B#`<init>`().
         |[2:8..2:9): I <= _empty_/qs9#I#
         |[2:10..2:10):  <= _empty_/qs9#I#`<init>`().
         |[3:8..3:9): a <= _empty_/qs9#I#a.
         |[3:12..3:19): classOf => scala/Predef.classOf().
         |[3:20..3:23): qs9 => _empty_/qs9#
         |[3:29..3:30): B => _empty_/qs9#B#
         |[4:8..4:9): b <= _empty_/qs9#I#b.
         |[4:12..4:16): List => scala/collection/immutable/Nil.
         |[4:17..4:20): qs9 => _empty_/qs9#
         |[4:26..4:27): B => _empty_/qs9#B#
         |[5:8..5:9): c <= _empty_/qs9#I#c.
         |[5:12..5:19): classOf => scala/Predef.classOf().
         |[5:20..5:24): List => scala/package.List#
         |[5:25..5:26): X => local0
         |[5:43..5:44): X <= local0
         |[5:48..5:51): qs9 => _empty_/qs9#
         |[5:57..5:58): B => _empty_/qs9#B#
         |[6:8..6:9): d <= _empty_/qs9#I#d.
         |[6:11..6:15): List => scala/package.List#
         |[6:16..6:17): X => local1
         |[6:34..6:35): X <= local1
         |[6:39..6:42): qs9 => _empty_/qs9#
         |[6:48..6:49): B => _empty_/qs9#B#
         |[7:8..7:9): e <= _empty_/qs9#I#e.
         |[7:11..7:26): PartialFunction => scala/PartialFunction#
         |[7:27..7:30): qs9 => _empty_/qs9#
         |[7:36..7:37): B => _empty_/qs9#B#
         |[7:39..7:42): Int => scala/Int#
         |[7:46..7:88): (x: qs9.this.B) => x match { case _ => 1 } <= local2
         |[7:47..7:48): x <= local9
         |[7:65..7:66): x => local9
         |[8:8..8:9): f <= _empty_/qs9#I#f.
         |[8:12..8:16): List => scala/collection/immutable/Nil.
         |[8:17..8:23): AnyRef => java/lang/Object#
         |[8:30..8:31): m <= local10
         |[8:33..8:36): qs9 => _empty_/qs9#
         |[8:42..8:43): B => _empty_/qs9#B#
         |[9:8..9:9): g <= _empty_/qs9#I#g.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:20): qs9 => _empty_/qs9#
         |""".stripMargin,
  )

  occurrences(
    "a type through this of a class in an inner class of the same name",
    """|class qs68 {
       |  class C; trait Tr; class P[A](a: A); trait TC[A]
       |  class qs68 {
       |    class C; trait Tr; class P[A](a: A); trait TC[A]
       |    val a = classOf[qs68.this.C]
       |    val b = List[qs68.this.C]()
       |    val c = classOf[List[X] forSome { type X <: qs68.this.C }]
       |    val d: List[X] forSome { type X <: qs68.this.C } = null
       |    val e: PartialFunction[qs68.this.C, Int] = (x: qs68.this.C) => x match { case _ => 1 }
       |    val f = List[AnyRef { def m: qs68.this.C }]()
       |    val g = List[qs68.this.type]()
       |    class A { self: qs68.this.Tr => }
       |    class B extends qs68.this.P(1)
       |    val h = List[AnyRef { def m[A: qs68.this.TC]: Int }]()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): qs68 <= _empty_/qs68#
         |[0:11..0:11):  <= _empty_/qs68#`<init>`().
         |[1:8..1:9): C <= _empty_/qs68#C#
         |[1:9..1:9):  <= _empty_/qs68#C#`<init>`().
         |[1:17..1:19): Tr <= _empty_/qs68#Tr#
         |[1:27..1:28): P <= _empty_/qs68#P#
         |[1:29..1:30): A <= _empty_/qs68#P#[A]
         |[1:31..1:31):  <= _empty_/qs68#P#`<init>`().
         |[1:32..1:33): a <= _empty_/qs68#P#`<init>`().(a)
         |[1:32..1:33): a <= _empty_/qs68#P#a.
         |[1:35..1:36): A => _empty_/qs68#P#[A]
         |[1:45..1:47): TC <= _empty_/qs68#TC#
         |[1:48..1:49): A <= _empty_/qs68#TC#[A]
         |[2:8..2:12): qs68 <= _empty_/qs68#qs68#
         |[2:13..2:13):  <= _empty_/qs68#qs68#`<init>`().
         |[3:10..3:11): C <= _empty_/qs68#qs68#C#
         |[3:11..3:11):  <= _empty_/qs68#qs68#C#`<init>`().
         |[3:19..3:21): Tr <= _empty_/qs68#qs68#Tr#
         |[3:29..3:30): P <= _empty_/qs68#qs68#P#
         |[3:31..3:32): A <= _empty_/qs68#qs68#P#[A]
         |[3:33..3:33):  <= _empty_/qs68#qs68#P#`<init>`().
         |[3:34..3:35): a <= _empty_/qs68#qs68#P#`<init>`().(a)
         |[3:34..3:35): a <= _empty_/qs68#qs68#P#a.
         |[3:37..3:38): A => _empty_/qs68#qs68#P#[A]
         |[3:47..3:49): TC <= _empty_/qs68#qs68#TC#
         |[3:50..3:51): A <= _empty_/qs68#qs68#TC#[A]
         |[4:8..4:9): a <= _empty_/qs68#qs68#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:20..4:24): qs68 => _empty_/qs68#qs68#
         |[4:30..4:31): C => _empty_/qs68#qs68#C#
         |[5:8..5:9): b <= _empty_/qs68#qs68#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:17..5:21): qs68 => _empty_/qs68#qs68#
         |[5:27..5:28): C => _empty_/qs68#qs68#C#
         |[6:8..6:9): c <= _empty_/qs68#qs68#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local0
         |[6:43..6:44): X <= local0
         |[6:48..6:52): qs68 => _empty_/qs68#qs68#
         |[6:58..6:59): C => _empty_/qs68#qs68#C#
         |[7:8..7:9): d <= _empty_/qs68#qs68#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local1
         |[7:34..7:35): X <= local1
         |[7:39..7:43): qs68 => _empty_/qs68#qs68#
         |[7:49..7:50): C => _empty_/qs68#qs68#C#
         |[8:8..8:9): e <= _empty_/qs68#qs68#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:31): qs68 => _empty_/qs68#qs68#
         |[8:37..8:38): C => _empty_/qs68#qs68#C#
         |[8:40..8:43): Int => scala/Int#
         |[8:47..8:90): (x: qs68.this.C) => x match { case _ => 1 } <= local2
         |[8:48..8:49): x <= local9
         |[8:51..8:55): qs68 => _empty_/qs68#qs68#
         |[8:61..8:62): C => _empty_/qs68#qs68#C#
         |[8:67..8:68): x => local9
         |[9:8..9:9): f <= _empty_/qs68#qs68#f.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:23): AnyRef => java/lang/Object#
         |[9:30..9:31): m <= local10
         |[9:33..9:37): qs68 => _empty_/qs68#qs68#
         |[9:43..9:44): C => _empty_/qs68#qs68#C#
         |[10:8..10:9): g <= _empty_/qs68#qs68#g.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[10:17..10:21): qs68 => _empty_/qs68#qs68#
         |[11:10..11:11): A <= _empty_/qs68#qs68#A#
         |[11:12..11:12):  <= _empty_/qs68#qs68#A#`<init>`().
         |[11:14..11:18): self <= local11
         |[11:20..11:24): qs68 => _empty_/qs68#qs68#
         |[11:30..11:32): Tr => _empty_/qs68#qs68#Tr#
         |[12:10..12:11): B <= _empty_/qs68#qs68#B#
         |[12:12..12:12):  <= _empty_/qs68#qs68#B#`<init>`().
         |[12:30..12:31): P => _empty_/qs68#qs68#P#
         |[12:31..12:31):  => _empty_/qs68#qs68#P#`<init>`().
         |[13:8..13:9): h <= _empty_/qs68#qs68#h.
         |[13:12..13:16): List => scala/collection/immutable/Nil.
         |[13:17..13:23): AnyRef => java/lang/Object#
         |[13:30..13:31): m <= local12
         |[13:32..13:33): A <= local14
         |[13:35..13:35):  <= local13
         |[13:35..13:39): qs68 => _empty_/qs68#qs68#
         |[13:45..13:47): TC => _empty_/qs68#qs68#TC#
         |[13:50..13:53): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:6..0:10): qs68 <= _empty_/qs68#
         |[0:11..0:11):  <= _empty_/qs68#`<init>`().
         |[1:8..1:9): C <= _empty_/qs68#C#
         |[1:9..1:9):  <= _empty_/qs68#C#`<init>`().
         |[1:17..1:19): Tr <= _empty_/qs68#Tr#
         |[1:27..1:28): P <= _empty_/qs68#P#
         |[1:29..1:30): A <= _empty_/qs68#P#[A]
         |[1:31..1:31):  <= _empty_/qs68#P#`<init>`().
         |[1:32..1:33): a <= _empty_/qs68#P#`<init>`().(a)
         |[1:32..1:33): a <= _empty_/qs68#P#a.
         |[1:35..1:36): A => _empty_/qs68#P#[A]
         |[1:45..1:47): TC <= _empty_/qs68#TC#
         |[1:48..1:49): A <= _empty_/qs68#TC#[A]
         |[2:8..2:12): qs68 <= _empty_/qs68#qs68#
         |[2:13..2:13):  <= _empty_/qs68#qs68#`<init>`().
         |[3:10..3:11): C <= _empty_/qs68#qs68#C#
         |[3:11..3:11):  <= _empty_/qs68#qs68#C#`<init>`().
         |[3:19..3:21): Tr <= _empty_/qs68#qs68#Tr#
         |[3:29..3:30): P <= _empty_/qs68#qs68#P#
         |[3:31..3:32): A <= _empty_/qs68#qs68#P#[A]
         |[3:33..3:33):  <= _empty_/qs68#qs68#P#`<init>`().
         |[3:34..3:35): a <= _empty_/qs68#qs68#P#`<init>`().(a)
         |[3:34..3:35): a <= _empty_/qs68#qs68#P#a.
         |[3:37..3:38): A => _empty_/qs68#qs68#P#[A]
         |[3:47..3:49): TC <= _empty_/qs68#qs68#TC#
         |[3:50..3:51): A <= _empty_/qs68#qs68#TC#[A]
         |[4:8..4:9): a <= _empty_/qs68#qs68#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:20..4:24): qs68 => _empty_/qs68#qs68#
         |[4:30..4:31): C => _empty_/qs68#qs68#C#
         |[5:8..5:9): b <= _empty_/qs68#qs68#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:17..5:21): qs68 => _empty_/qs68#qs68#
         |[5:27..5:28): C => _empty_/qs68#qs68#C#
         |[6:8..6:9): c <= _empty_/qs68#qs68#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local0
         |[6:43..6:44): X <= local0
         |[6:48..6:52): qs68 => _empty_/qs68#qs68#
         |[6:58..6:59): C => _empty_/qs68#qs68#C#
         |[7:8..7:9): d <= _empty_/qs68#qs68#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local1
         |[7:34..7:35): X <= local1
         |[7:39..7:43): qs68 => _empty_/qs68#qs68#
         |[7:49..7:50): C => _empty_/qs68#qs68#C#
         |[8:8..8:9): e <= _empty_/qs68#qs68#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:31): qs68 => _empty_/qs68#qs68#
         |[8:37..8:38): C => _empty_/qs68#qs68#C#
         |[8:40..8:43): Int => scala/Int#
         |[8:47..8:90): (x: qs68.this.C) => x match { case _ => 1 } <= local2
         |[8:48..8:49): x <= local9
         |[8:67..8:68): x => local9
         |[9:8..9:9): f <= _empty_/qs68#qs68#f.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:23): AnyRef => java/lang/Object#
         |[9:30..9:31): m <= local10
         |[9:33..9:37): qs68 => _empty_/qs68#qs68#
         |[9:43..9:44): C => _empty_/qs68#qs68#C#
         |[10:8..10:9): g <= _empty_/qs68#qs68#g.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[10:17..10:21): qs68 => _empty_/qs68#qs68#
         |[11:10..11:11): A <= _empty_/qs68#qs68#A#
         |[11:12..11:12):  <= _empty_/qs68#qs68#A#`<init>`().
         |[11:14..11:18): self <= local11
         |[11:20..11:24): qs68 => _empty_/qs68#qs68#
         |[11:30..11:32): Tr => _empty_/qs68#qs68#Tr#
         |[12:10..12:11): B <= _empty_/qs68#qs68#B#
         |[12:12..12:12):  <= _empty_/qs68#qs68#B#`<init>`().
         |[12:30..12:31): P => _empty_/qs68#qs68#P#
         |[12:31..12:31):  => _empty_/qs68#qs68#P#`<init>`().
         |[13:8..13:9): h <= _empty_/qs68#qs68#h.
         |[13:12..13:16): List => scala/collection/immutable/Nil.
         |[13:17..13:23): AnyRef => java/lang/Object#
         |[13:30..13:31): m <= local12
         |[13:32..13:33): A <= local14
         |[13:35..13:35):  <= local13
         |[13:35..13:39): qs68 => _empty_/qs68#qs68#
         |[13:45..13:47): TC => _empty_/qs68#qs68#TC#
         |[13:50..13:53): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "an alias of a member of an outer class through this of an inner class of the same name",
    """|class qs75 {
       |  class U; trait TCU[B]; class P[B](b: B)
       |  trait A { type T = qs75.this.U; type TC[B] = qs75.this.TCU[B]; type PA[B] = qs75.this.P[B] }
       |  class qs75 extends A {
       |    val a = classOf[qs75.this.T]
       |    val b = List[qs75.this.T]()
       |    val c = classOf[List[X] forSome { type X <: qs75.this.T }]
       |    val d: List[X] forSome { type X <: qs75.this.T } = null
       |    val e: PartialFunction[qs75.this.T, Int] = (x: qs75.this.T) => x match { case _ => 1 }
       |    val f = List[AnyRef { def m: qs75.this.T }]()
       |    val g = List[qs75.this.type]()
       |    class I { s: qs75.this.T => }
       |    class J extends qs75.this.PA(1)
       |    val h = List[AnyRef { def m[B: qs75.this.TC]: Int }]()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): qs75 <= _empty_/qs75#
         |[0:11..0:11):  <= _empty_/qs75#`<init>`().
         |[1:8..1:9): U <= _empty_/qs75#U#
         |[1:9..1:9):  <= _empty_/qs75#U#`<init>`().
         |[1:17..1:20): TCU <= _empty_/qs75#TCU#
         |[1:21..1:22): B <= _empty_/qs75#TCU#[B]
         |[1:31..1:32): P <= _empty_/qs75#P#
         |[1:33..1:34): B <= _empty_/qs75#P#[B]
         |[1:35..1:35):  <= _empty_/qs75#P#`<init>`().
         |[1:36..1:37): b <= _empty_/qs75#P#`<init>`().(b)
         |[1:36..1:37): b <= _empty_/qs75#P#b.
         |[1:39..1:40): B => _empty_/qs75#P#[B]
         |[2:8..2:9): A <= _empty_/qs75#A#
         |[2:17..2:18): T <= _empty_/qs75#A#T#
         |[2:21..2:25): qs75 => _empty_/qs75#
         |[2:31..2:32): U => _empty_/qs75#U#
         |[2:39..2:41): TC <= _empty_/qs75#A#TC#
         |[2:42..2:43): B <= _empty_/qs75#A#TC#[B]
         |[2:47..2:51): qs75 => _empty_/qs75#
         |[2:57..2:60): TCU => _empty_/qs75#TCU#
         |[2:61..2:62): B => _empty_/qs75#A#TC#[B]
         |[2:70..2:72): PA <= _empty_/qs75#A#PA#
         |[2:73..2:74): B <= _empty_/qs75#A#PA#[B]
         |[2:78..2:82): qs75 => _empty_/qs75#
         |[2:88..2:89): P => _empty_/qs75#P#
         |[2:90..2:91): B => _empty_/qs75#A#PA#[B]
         |[3:8..3:12): qs75 <= _empty_/qs75#qs75#
         |[3:13..3:13):  <= _empty_/qs75#qs75#`<init>`().
         |[3:21..3:22): A => _empty_/qs75#A#
         |[3:22..3:22):  => java/lang/Object#`<init>`().
         |[4:8..4:9): a <= _empty_/qs75#qs75#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:20..4:24): qs75 => _empty_/qs75#qs75#
         |[4:30..4:31): T => _empty_/qs75#A#T#
         |[5:8..5:9): b <= _empty_/qs75#qs75#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:27..5:28): T => _empty_/qs75#U#
         |[6:8..6:9): c <= _empty_/qs75#qs75#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local0
         |[6:43..6:44): X <= local0
         |[6:48..6:52): qs75 => _empty_/qs75#qs75#
         |[6:58..6:59): T => _empty_/qs75#A#T#
         |[7:8..7:9): d <= _empty_/qs75#qs75#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local1
         |[7:34..7:35): X <= local1
         |[7:39..7:43): qs75 => _empty_/qs75#qs75#
         |[7:49..7:50): T => _empty_/qs75#A#T#
         |[8:8..8:9): e <= _empty_/qs75#qs75#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:31): qs75 => _empty_/qs75#qs75#
         |[8:37..8:38): T => _empty_/qs75#A#T#
         |[8:40..8:43): Int => scala/Int#
         |[8:47..8:90): (x: qs75.this.T) => x match { case _ => 1 } <= local2
         |[8:48..8:49): x <= local9
         |[8:51..8:55): qs75 => _empty_/qs75#qs75#
         |[8:61..8:62): T => _empty_/qs75#A#T#
         |[8:67..8:68): x => local9
         |[9:8..9:9): f <= _empty_/qs75#qs75#f.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:23): AnyRef => java/lang/Object#
         |[9:30..9:31): m <= local10
         |[9:43..9:44): T => _empty_/qs75#U#
         |[10:8..10:9): g <= _empty_/qs75#qs75#g.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[10:17..10:21): qs75 => _empty_/qs75#qs75#
         |[11:10..11:11): I <= _empty_/qs75#qs75#I#
         |[11:12..11:12):  <= _empty_/qs75#qs75#I#`<init>`().
         |[11:14..11:15): s <= local11
         |[11:17..11:21): qs75 => _empty_/qs75#qs75#
         |[11:27..11:28): T => _empty_/qs75#A#T#
         |[12:10..12:11): J <= _empty_/qs75#qs75#J#
         |[12:12..12:12):  <= _empty_/qs75#qs75#J#`<init>`().
         |[12:30..12:32): PA => _empty_/qs75#A#PA#
         |[12:32..12:32):  => _empty_/qs75#P#`<init>`().
         |[13:8..13:9): h <= _empty_/qs75#qs75#h.
         |[13:12..13:16): List => scala/collection/immutable/Nil.
         |[13:17..13:23): AnyRef => java/lang/Object#
         |[13:30..13:31): m <= local12
         |[13:32..13:33): B <= local14
         |[13:35..13:35):  <= local13
         |[13:45..13:47): TC => _empty_/qs75#TCU#
         |[13:50..13:53): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:6..0:10): qs75 <= _empty_/qs75#
         |[0:11..0:11):  <= _empty_/qs75#`<init>`().
         |[1:8..1:9): U <= _empty_/qs75#U#
         |[1:9..1:9):  <= _empty_/qs75#U#`<init>`().
         |[1:17..1:20): TCU <= _empty_/qs75#TCU#
         |[1:21..1:22): B <= _empty_/qs75#TCU#[B]
         |[1:31..1:32): P <= _empty_/qs75#P#
         |[1:33..1:34): B <= _empty_/qs75#P#[B]
         |[1:35..1:35):  <= _empty_/qs75#P#`<init>`().
         |[1:36..1:37): b <= _empty_/qs75#P#`<init>`().(b)
         |[1:36..1:37): b <= _empty_/qs75#P#b.
         |[1:39..1:40): B => _empty_/qs75#P#[B]
         |[2:8..2:9): A <= _empty_/qs75#A#
         |[2:17..2:18): T <= _empty_/qs75#A#T#
         |[2:21..2:25): qs75 => _empty_/qs75#
         |[2:31..2:32): U => _empty_/qs75#U#
         |[2:39..2:41): TC <= _empty_/qs75#A#TC#
         |[2:42..2:43): B <= _empty_/qs75#A#TC#[B]
         |[2:47..2:51): qs75 => _empty_/qs75#
         |[2:57..2:60): TCU => _empty_/qs75#TCU#
         |[2:61..2:62): B => _empty_/qs75#A#TC#[B]
         |[2:70..2:72): PA <= _empty_/qs75#A#PA#
         |[2:73..2:74): B <= _empty_/qs75#A#PA#[B]
         |[2:78..2:82): qs75 => _empty_/qs75#
         |[2:88..2:89): P => _empty_/qs75#P#
         |[2:90..2:91): B => _empty_/qs75#A#PA#[B]
         |[3:8..3:12): qs75 <= _empty_/qs75#qs75#
         |[3:13..3:13):  <= _empty_/qs75#qs75#`<init>`().
         |[3:21..3:22): A => _empty_/qs75#A#
         |[3:22..3:22):  => java/lang/Object#`<init>`().
         |[4:8..4:9): a <= _empty_/qs75#qs75#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:20..4:24): qs75 => _empty_/qs75#qs75#
         |[4:30..4:31): T => _empty_/qs75#A#T#
         |[5:8..5:9): b <= _empty_/qs75#qs75#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:27..5:28): T => _empty_/qs75#U#
         |[6:8..6:9): c <= _empty_/qs75#qs75#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local0
         |[6:43..6:44): X <= local0
         |[6:48..6:52): qs75 => _empty_/qs75#qs75#
         |[6:58..6:59): T => _empty_/qs75#A#T#
         |[7:8..7:9): d <= _empty_/qs75#qs75#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local1
         |[7:34..7:35): X <= local1
         |[7:39..7:43): qs75 => _empty_/qs75#qs75#
         |[7:49..7:50): T => _empty_/qs75#A#T#
         |[8:8..8:9): e <= _empty_/qs75#qs75#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:31): qs75 => _empty_/qs75#qs75#
         |[8:37..8:38): T => _empty_/qs75#A#T#
         |[8:40..8:43): Int => scala/Int#
         |[8:47..8:90): (x: qs75.this.T) => x match { case _ => 1 } <= local2
         |[8:48..8:49): x <= local9
         |[8:67..8:68): x => local9
         |[9:8..9:9): f <= _empty_/qs75#qs75#f.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:23): AnyRef => java/lang/Object#
         |[9:30..9:31): m <= local10
         |[9:43..9:44): T => _empty_/qs75#U#
         |[10:8..10:9): g <= _empty_/qs75#qs75#g.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[10:17..10:21): qs75 => _empty_/qs75#qs75#
         |[11:10..11:11): I <= _empty_/qs75#qs75#I#
         |[11:12..11:12):  <= _empty_/qs75#qs75#I#`<init>`().
         |[11:14..11:15): s <= local11
         |[11:17..11:21): qs75 => _empty_/qs75#qs75#
         |[11:27..11:28): T => _empty_/qs75#A#T#
         |[12:10..12:11): J <= _empty_/qs75#qs75#J#
         |[12:12..12:12):  <= _empty_/qs75#qs75#J#`<init>`().
         |[12:30..12:32): PA => _empty_/qs75#A#PA#
         |[12:32..12:32):  => _empty_/qs75#P#`<init>`().
         |[13:8..13:9): h <= _empty_/qs75#qs75#h.
         |[13:12..13:16): List => scala/collection/immutable/Nil.
         |[13:17..13:23): AnyRef => java/lang/Object#
         |[13:30..13:31): m <= local12
         |[13:32..13:33): B <= local14
         |[13:35..13:35):  <= local13
         |[13:45..13:47): TC => _empty_/qs75#TCU#
         |[13:50..13:53): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "an alias of a member of an object through this of a class of that name",
    """|object qs76 { class U; trait TCU[B]; class P[B](b: B) }
       |class qs76d {
       |  trait A { type T = qs76.U; type TC[B] = qs76.TCU[B]; type PA[B] = qs76.P[B] }
       |  class qs76 extends A {
       |    val a = classOf[qs76.this.T]
       |    val b = List[qs76.this.T]()
       |    val c = classOf[List[X] forSome { type X <: qs76.this.T }]
       |    val d: List[X] forSome { type X <: qs76.this.T } = null
       |    val e: PartialFunction[qs76.this.T, Int] = (x: qs76.this.T) => x match { case _ => 1 }
       |    val f = List[AnyRef { def m: qs76.this.T }]()
       |    val g = List[qs76.this.type]()
       |    class I { s: qs76.this.T => }
       |    class J extends qs76.this.PA(1)
       |    val h = List[AnyRef { def m[B: qs76.this.TC]: Int }]()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): qs76 <= _empty_/qs76.
         |[0:20..0:21): U <= _empty_/qs76.U#
         |[0:21..0:21):  <= _empty_/qs76.U#`<init>`().
         |[0:29..0:32): TCU <= _empty_/qs76.TCU#
         |[0:33..0:34): B <= _empty_/qs76.TCU#[B]
         |[0:43..0:44): P <= _empty_/qs76.P#
         |[0:45..0:46): B <= _empty_/qs76.P#[B]
         |[0:47..0:47):  <= _empty_/qs76.P#`<init>`().
         |[0:48..0:49): b <= _empty_/qs76.P#`<init>`().(b)
         |[0:48..0:49): b <= _empty_/qs76.P#b.
         |[0:51..0:52): B => _empty_/qs76.P#[B]
         |[1:6..1:11): qs76d <= _empty_/qs76d#
         |[1:12..1:12):  <= _empty_/qs76d#`<init>`().
         |[2:8..2:9): A <= _empty_/qs76d#A#
         |[2:17..2:18): T <= _empty_/qs76d#A#T#
         |[2:21..2:25): qs76 => _empty_/qs76.
         |[2:26..2:27): U => _empty_/qs76.U#
         |[2:34..2:36): TC <= _empty_/qs76d#A#TC#
         |[2:37..2:38): B <= _empty_/qs76d#A#TC#[B]
         |[2:42..2:46): qs76 => _empty_/qs76.
         |[2:47..2:50): TCU => _empty_/qs76.TCU#
         |[2:51..2:52): B => _empty_/qs76d#A#TC#[B]
         |[2:60..2:62): PA <= _empty_/qs76d#A#PA#
         |[2:63..2:64): B <= _empty_/qs76d#A#PA#[B]
         |[2:68..2:72): qs76 => _empty_/qs76.
         |[2:73..2:74): P => _empty_/qs76.P#
         |[2:75..2:76): B => _empty_/qs76d#A#PA#[B]
         |[3:8..3:12): qs76 <= _empty_/qs76d#qs76#
         |[3:13..3:13):  <= _empty_/qs76d#qs76#`<init>`().
         |[3:21..3:22): A => _empty_/qs76d#A#
         |[3:22..3:22):  => java/lang/Object#`<init>`().
         |[4:8..4:9): a <= _empty_/qs76d#qs76#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:20..4:24): qs76 => _empty_/qs76d#qs76#
         |[4:30..4:31): T => _empty_/qs76d#A#T#
         |[5:8..5:9): b <= _empty_/qs76d#qs76#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:27..5:28): T => _empty_/qs76.U#
         |[6:8..6:9): c <= _empty_/qs76d#qs76#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local0
         |[6:43..6:44): X <= local0
         |[6:48..6:52): qs76 => _empty_/qs76d#qs76#
         |[6:58..6:59): T => _empty_/qs76d#A#T#
         |[7:8..7:9): d <= _empty_/qs76d#qs76#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local1
         |[7:34..7:35): X <= local1
         |[7:39..7:43): qs76 => _empty_/qs76d#qs76#
         |[7:49..7:50): T => _empty_/qs76d#A#T#
         |[8:8..8:9): e <= _empty_/qs76d#qs76#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:31): qs76 => _empty_/qs76d#qs76#
         |[8:37..8:38): T => _empty_/qs76d#A#T#
         |[8:40..8:43): Int => scala/Int#
         |[8:47..8:90): (x: qs76.this.T) => x match { case _ => 1 } <= local2
         |[8:48..8:49): x <= local9
         |[8:51..8:55): qs76 => _empty_/qs76d#qs76#
         |[8:61..8:62): T => _empty_/qs76d#A#T#
         |[8:67..8:68): x => local9
         |[9:8..9:9): f <= _empty_/qs76d#qs76#f.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:23): AnyRef => java/lang/Object#
         |[9:30..9:31): m <= local10
         |[9:43..9:44): T => _empty_/qs76.U#
         |[10:8..10:9): g <= _empty_/qs76d#qs76#g.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[10:17..10:21): qs76 => _empty_/qs76d#qs76#
         |[11:10..11:11): I <= _empty_/qs76d#qs76#I#
         |[11:12..11:12):  <= _empty_/qs76d#qs76#I#`<init>`().
         |[11:14..11:15): s <= local11
         |[11:17..11:21): qs76 => _empty_/qs76d#qs76#
         |[11:27..11:28): T => _empty_/qs76d#A#T#
         |[12:10..12:11): J <= _empty_/qs76d#qs76#J#
         |[12:12..12:12):  <= _empty_/qs76d#qs76#J#`<init>`().
         |[12:30..12:32): PA => _empty_/qs76d#A#PA#
         |[12:32..12:32):  => _empty_/qs76.P#`<init>`().
         |[13:8..13:9): h <= _empty_/qs76d#qs76#h.
         |[13:12..13:16): List => scala/collection/immutable/Nil.
         |[13:17..13:23): AnyRef => java/lang/Object#
         |[13:30..13:31): m <= local12
         |[13:32..13:33): B <= local14
         |[13:35..13:35):  <= local13
         |[13:45..13:47): TC => _empty_/qs76.TCU#
         |[13:50..13:53): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:7..0:11): qs76 <= _empty_/qs76.
         |[0:20..0:21): U <= _empty_/qs76.U#
         |[0:21..0:21):  <= _empty_/qs76.U#`<init>`().
         |[0:29..0:32): TCU <= _empty_/qs76.TCU#
         |[0:33..0:34): B <= _empty_/qs76.TCU#[B]
         |[0:43..0:44): P <= _empty_/qs76.P#
         |[0:45..0:46): B <= _empty_/qs76.P#[B]
         |[0:47..0:47):  <= _empty_/qs76.P#`<init>`().
         |[0:48..0:49): b <= _empty_/qs76.P#`<init>`().(b)
         |[0:48..0:49): b <= _empty_/qs76.P#b.
         |[0:51..0:52): B => _empty_/qs76.P#[B]
         |[1:6..1:11): qs76d <= _empty_/qs76d#
         |[1:12..1:12):  <= _empty_/qs76d#`<init>`().
         |[2:8..2:9): A <= _empty_/qs76d#A#
         |[2:17..2:18): T <= _empty_/qs76d#A#T#
         |[2:21..2:25): qs76 => _empty_/qs76.
         |[2:26..2:27): U => _empty_/qs76.U#
         |[2:34..2:36): TC <= _empty_/qs76d#A#TC#
         |[2:37..2:38): B <= _empty_/qs76d#A#TC#[B]
         |[2:42..2:46): qs76 => _empty_/qs76.
         |[2:47..2:50): TCU => _empty_/qs76.TCU#
         |[2:51..2:52): B => _empty_/qs76d#A#TC#[B]
         |[2:60..2:62): PA <= _empty_/qs76d#A#PA#
         |[2:63..2:64): B <= _empty_/qs76d#A#PA#[B]
         |[2:68..2:72): qs76 => _empty_/qs76.
         |[2:73..2:74): P => _empty_/qs76.P#
         |[2:75..2:76): B => _empty_/qs76d#A#PA#[B]
         |[3:8..3:12): qs76 <= _empty_/qs76d#qs76#
         |[3:13..3:13):  <= _empty_/qs76d#qs76#`<init>`().
         |[3:21..3:22): A => _empty_/qs76d#A#
         |[3:22..3:22):  => java/lang/Object#`<init>`().
         |[4:8..4:9): a <= _empty_/qs76d#qs76#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:20..4:24): qs76 => _empty_/qs76d#qs76#
         |[4:30..4:31): T => _empty_/qs76d#A#T#
         |[5:8..5:9): b <= _empty_/qs76d#qs76#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:27..5:28): T => _empty_/qs76.U#
         |[6:8..6:9): c <= _empty_/qs76d#qs76#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local0
         |[6:43..6:44): X <= local0
         |[6:48..6:52): qs76 => _empty_/qs76d#qs76#
         |[6:58..6:59): T => _empty_/qs76d#A#T#
         |[7:8..7:9): d <= _empty_/qs76d#qs76#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local1
         |[7:34..7:35): X <= local1
         |[7:39..7:43): qs76 => _empty_/qs76d#qs76#
         |[7:49..7:50): T => _empty_/qs76d#A#T#
         |[8:8..8:9): e <= _empty_/qs76d#qs76#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:31): qs76 => _empty_/qs76d#qs76#
         |[8:37..8:38): T => _empty_/qs76d#A#T#
         |[8:40..8:43): Int => scala/Int#
         |[8:47..8:90): (x: qs76.this.T) => x match { case _ => 1 } <= local2
         |[8:48..8:49): x <= local9
         |[8:67..8:68): x => local9
         |[9:8..9:9): f <= _empty_/qs76d#qs76#f.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:23): AnyRef => java/lang/Object#
         |[9:30..9:31): m <= local10
         |[9:43..9:44): T => _empty_/qs76.U#
         |[10:8..10:9): g <= _empty_/qs76d#qs76#g.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[10:17..10:21): qs76 => _empty_/qs76d#qs76#
         |[11:10..11:11): I <= _empty_/qs76d#qs76#I#
         |[11:12..11:12):  <= _empty_/qs76d#qs76#I#`<init>`().
         |[11:14..11:15): s <= local11
         |[11:17..11:21): qs76 => _empty_/qs76d#qs76#
         |[11:27..11:28): T => _empty_/qs76d#A#T#
         |[12:10..12:11): J <= _empty_/qs76d#qs76#J#
         |[12:12..12:12):  <= _empty_/qs76d#qs76#J#`<init>`().
         |[12:30..12:32): PA => _empty_/qs76d#A#PA#
         |[12:32..12:32):  => _empty_/qs76.P#`<init>`().
         |[13:8..13:9): h <= _empty_/qs76d#qs76#h.
         |[13:12..13:16): List => scala/collection/immutable/Nil.
         |[13:17..13:23): AnyRef => java/lang/Object#
         |[13:30..13:31): m <= local12
         |[13:32..13:33): B <= local14
         |[13:35..13:35):  <= local13
         |[13:45..13:47): TC => _empty_/qs76.TCU#
         |[13:50..13:53): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "an alias of a member of an object named package through this of its class",
    """|class qs85 {
       |  object `package` { class B; trait TCB[C]; class P[C](c: C) }
       |  type T = `package`.B; type TC[C] = `package`.TCB[C]; type PA[C] = `package`.P[C]
       |  val a = classOf[qs85.this.T]
       |  val b = List[qs85.this.T]()
       |  val c = classOf[List[X] forSome { type X <: qs85.this.T }]
       |  val d: List[X] forSome { type X <: qs85.this.T } = null
       |  val e: PartialFunction[qs85.this.T, Int] = (x: qs85.this.T) => x match { case _ => 1 }
       |  val f = List[AnyRef { def m: qs85.this.T }]()
       |  class I { s: qs85.this.T => }
       |  class J extends qs85.this.PA(1)
       |  val h = List[AnyRef { def m[C: qs85.this.TC]: Int }]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): qs85 <= _empty_/qs85#
         |[0:11..0:11):  <= _empty_/qs85#`<init>`().
         |[1:9..1:18): `package` <= _empty_/qs85#package.
         |[1:27..1:28): B <= _empty_/qs85#package.B#
         |[1:28..1:28):  <= _empty_/qs85#package.B#`<init>`().
         |[1:36..1:39): TCB <= _empty_/qs85#package.TCB#
         |[1:40..1:41): C <= _empty_/qs85#package.TCB#[C]
         |[1:50..1:51): P <= _empty_/qs85#package.P#
         |[1:52..1:53): C <= _empty_/qs85#package.P#[C]
         |[1:54..1:54):  <= _empty_/qs85#package.P#`<init>`().
         |[1:55..1:56): c <= _empty_/qs85#package.P#`<init>`().(c)
         |[1:55..1:56): c <= _empty_/qs85#package.P#c.
         |[1:58..1:59): C => _empty_/qs85#package.P#[C]
         |[2:7..2:8): T <= _empty_/qs85#T#
         |[2:11..2:20): `package` => _empty_/qs85#package.
         |[2:21..2:22): B => _empty_/qs85#package.B#
         |[2:29..2:31): TC <= _empty_/qs85#TC#
         |[2:32..2:33): C <= _empty_/qs85#TC#[C]
         |[2:37..2:46): `package` => _empty_/qs85#package.
         |[2:47..2:50): TCB => _empty_/qs85#package.TCB#
         |[2:51..2:52): C => _empty_/qs85#TC#[C]
         |[2:60..2:62): PA <= _empty_/qs85#PA#
         |[2:63..2:64): C <= _empty_/qs85#PA#[C]
         |[2:68..2:77): `package` => _empty_/qs85#package.
         |[2:78..2:79): P => _empty_/qs85#package.P#
         |[2:80..2:81): C => _empty_/qs85#PA#[C]
         |[3:6..3:7): a <= _empty_/qs85#a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:22): qs85 => _empty_/qs85#
         |[3:28..3:29): T => _empty_/qs85#T#
         |[4:6..4:7): b <= _empty_/qs85#b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:19): qs85 => _empty_/qs85#
         |[4:25..4:26): T => _empty_/qs85#package.B#
         |[5:6..5:7): c <= _empty_/qs85#c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:50): qs85 => _empty_/qs85#
         |[5:56..5:57): T => _empty_/qs85#T#
         |[6:6..6:7): d <= _empty_/qs85#d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:41): qs85 => _empty_/qs85#
         |[6:47..6:48): T => _empty_/qs85#T#
         |[7:6..7:7): e <= _empty_/qs85#e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:29): qs85 => _empty_/qs85#
         |[7:35..7:36): T => _empty_/qs85#T#
         |[7:38..7:41): Int => scala/Int#
         |[7:45..7:88): (x: qs85.this.T) => x match { case _ => 1 } <= local2
         |[7:46..7:47): x <= local9
         |[7:49..7:53): qs85 => _empty_/qs85#
         |[7:59..7:60): T => _empty_/qs85#T#
         |[7:65..7:66): x => local9
         |[8:6..8:7): f <= _empty_/qs85#f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:35): qs85 => _empty_/qs85#
         |[8:41..8:42): T => _empty_/qs85#package.B#
         |[9:8..9:9): I <= _empty_/qs85#I#
         |[9:10..9:10):  <= _empty_/qs85#I#`<init>`().
         |[9:12..9:13): s <= local11
         |[9:15..9:19): qs85 => _empty_/qs85#
         |[9:25..9:26): T => _empty_/qs85#T#
         |[10:8..10:9): J <= _empty_/qs85#J#
         |[10:10..10:10):  <= _empty_/qs85#J#`<init>`().
         |[10:28..10:30): PA => _empty_/qs85#PA#
         |[10:30..10:30):  => _empty_/qs85#package.P#`<init>`().
         |[11:6..11:7): h <= _empty_/qs85#h.
         |[11:10..11:14): List => scala/collection/immutable/Nil.
         |[11:15..11:21): AnyRef => java/lang/Object#
         |[11:28..11:29): m <= local12
         |[11:30..11:31): C <= local14
         |[11:33..11:33):  <= local13
         |[11:33..11:37): qs85 => _empty_/qs85#
         |[11:43..11:45): TC => _empty_/qs85#package.TCB#
         |[11:48..11:51): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:6..0:10): qs85 <= _empty_/qs85#
         |[0:11..0:11):  <= _empty_/qs85#`<init>`().
         |[1:9..1:18): `package` <= _empty_/qs85#package.
         |[1:27..1:28): B <= _empty_/qs85#package.B#
         |[1:28..1:28):  <= _empty_/qs85#package.B#`<init>`().
         |[1:36..1:39): TCB <= _empty_/qs85#package.TCB#
         |[1:40..1:41): C <= _empty_/qs85#package.TCB#[C]
         |[1:50..1:51): P <= _empty_/qs85#package.P#
         |[1:52..1:53): C <= _empty_/qs85#package.P#[C]
         |[1:54..1:54):  <= _empty_/qs85#package.P#`<init>`().
         |[1:55..1:56): c <= _empty_/qs85#package.P#`<init>`().(c)
         |[1:55..1:56): c <= _empty_/qs85#package.P#c.
         |[1:58..1:59): C => _empty_/qs85#package.P#[C]
         |[2:7..2:8): T <= _empty_/qs85#T#
         |[2:11..2:20): `package` => _empty_/qs85#package.
         |[2:21..2:22): B => _empty_/qs85#package.B#
         |[2:29..2:31): TC <= _empty_/qs85#TC#
         |[2:32..2:33): C <= _empty_/qs85#TC#[C]
         |[2:37..2:46): `package` => _empty_/qs85#package.
         |[2:47..2:50): TCB => _empty_/qs85#package.TCB#
         |[2:51..2:52): C => _empty_/qs85#TC#[C]
         |[2:60..2:62): PA <= _empty_/qs85#PA#
         |[2:63..2:64): C <= _empty_/qs85#PA#[C]
         |[2:68..2:77): `package` => _empty_/qs85#package.
         |[2:78..2:79): P => _empty_/qs85#package.P#
         |[2:80..2:81): C => _empty_/qs85#PA#[C]
         |[3:6..3:7): a <= _empty_/qs85#a.
         |[3:10..3:17): classOf => scala/Predef.classOf().
         |[3:18..3:22): qs85 => _empty_/qs85#
         |[3:28..3:29): T => _empty_/qs85#T#
         |[4:6..4:7): b <= _empty_/qs85#b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[4:15..4:19): qs85 => _empty_/qs85#
         |[4:25..4:26): T => _empty_/qs85#package.B#
         |[5:6..5:7): c <= _empty_/qs85#c.
         |[5:10..5:17): classOf => scala/Predef.classOf().
         |[5:18..5:22): List => scala/package.List#
         |[5:23..5:24): X => local0
         |[5:41..5:42): X <= local0
         |[5:46..5:50): qs85 => _empty_/qs85#
         |[5:56..5:57): T => _empty_/qs85#T#
         |[6:6..6:7): d <= _empty_/qs85#d.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): X => local1
         |[6:32..6:33): X <= local1
         |[6:37..6:41): qs85 => _empty_/qs85#
         |[6:47..6:48): T => _empty_/qs85#T#
         |[7:6..7:7): e <= _empty_/qs85#e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:29): qs85 => _empty_/qs85#
         |[7:35..7:36): T => _empty_/qs85#T#
         |[7:38..7:41): Int => scala/Int#
         |[7:45..7:88): (x: qs85.this.T) => x match { case _ => 1 } <= local2
         |[7:46..7:47): x <= local9
         |[7:65..7:66): x => local9
         |[8:6..8:7): f <= _empty_/qs85#f.
         |[8:10..8:14): List => scala/collection/immutable/Nil.
         |[8:15..8:21): AnyRef => java/lang/Object#
         |[8:28..8:29): m <= local10
         |[8:31..8:35): qs85 => _empty_/qs85#
         |[8:41..8:42): T => _empty_/qs85#package.B#
         |[9:8..9:9): I <= _empty_/qs85#I#
         |[9:10..9:10):  <= _empty_/qs85#I#`<init>`().
         |[9:12..9:13): s <= local11
         |[9:15..9:19): qs85 => _empty_/qs85#
         |[9:25..9:26): T => _empty_/qs85#T#
         |[10:8..10:9): J <= _empty_/qs85#J#
         |[10:10..10:10):  <= _empty_/qs85#J#`<init>`().
         |[10:28..10:30): PA => _empty_/qs85#PA#
         |[10:30..10:30):  => _empty_/qs85#package.P#`<init>`().
         |[11:6..11:7): h <= _empty_/qs85#h.
         |[11:10..11:14): List => scala/collection/immutable/Nil.
         |[11:15..11:21): AnyRef => java/lang/Object#
         |[11:28..11:29): m <= local12
         |[11:30..11:31): C <= local14
         |[11:33..11:33):  <= local13
         |[11:33..11:37): qs85 => _empty_/qs85#
         |[11:43..11:45): TC => _empty_/qs85#package.TCB#
         |[11:48..11:51): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "an alias of a member of an object named package through this of an inner class of the same name",
    """|class qs86 {
       |  object `package` { class B; trait TCB[C]; class P[C](c: C) }
       |  class qs86 {
       |    type T = `package`.B; type TC[C] = `package`.TCB[C]; type PA[C] = `package`.P[C]
       |    val a = classOf[qs86.this.T]
       |    val b = List[qs86.this.T]()
       |    val c = classOf[List[X] forSome { type X <: qs86.this.T }]
       |    val d: List[X] forSome { type X <: qs86.this.T } = null
       |    val e: PartialFunction[qs86.this.T, Int] = (x: qs86.this.T) => x match { case _ => 1 }
       |    val f = List[AnyRef { def m: qs86.this.T }]()
       |    class I { s: qs86.this.T => }
       |    class J extends qs86.this.PA(1)
       |    val h = List[AnyRef { def m[C: qs86.this.TC]: Int }]()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): qs86 <= _empty_/qs86#
         |[0:11..0:11):  <= _empty_/qs86#`<init>`().
         |[1:9..1:18): `package` <= _empty_/qs86#package.
         |[1:27..1:28): B <= _empty_/qs86#package.B#
         |[1:28..1:28):  <= _empty_/qs86#package.B#`<init>`().
         |[1:36..1:39): TCB <= _empty_/qs86#package.TCB#
         |[1:40..1:41): C <= _empty_/qs86#package.TCB#[C]
         |[1:50..1:51): P <= _empty_/qs86#package.P#
         |[1:52..1:53): C <= _empty_/qs86#package.P#[C]
         |[1:54..1:54):  <= _empty_/qs86#package.P#`<init>`().
         |[1:55..1:56): c <= _empty_/qs86#package.P#`<init>`().(c)
         |[1:55..1:56): c <= _empty_/qs86#package.P#c.
         |[1:58..1:59): C => _empty_/qs86#package.P#[C]
         |[2:8..2:12): qs86 <= _empty_/qs86#qs86#
         |[2:13..2:13):  <= _empty_/qs86#qs86#`<init>`().
         |[3:9..3:10): T <= _empty_/qs86#qs86#T#
         |[3:13..3:22): `package` => _empty_/qs86#package.
         |[3:23..3:24): B => _empty_/qs86#package.B#
         |[3:31..3:33): TC <= _empty_/qs86#qs86#TC#
         |[3:34..3:35): C <= _empty_/qs86#qs86#TC#[C]
         |[3:39..3:48): `package` => _empty_/qs86#package.
         |[3:49..3:52): TCB => _empty_/qs86#package.TCB#
         |[3:53..3:54): C => _empty_/qs86#qs86#TC#[C]
         |[3:62..3:64): PA <= _empty_/qs86#qs86#PA#
         |[3:65..3:66): C <= _empty_/qs86#qs86#PA#[C]
         |[3:70..3:79): `package` => _empty_/qs86#package.
         |[3:80..3:81): P => _empty_/qs86#package.P#
         |[3:82..3:83): C => _empty_/qs86#qs86#PA#[C]
         |[4:8..4:9): a <= _empty_/qs86#qs86#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:20..4:24): qs86 => _empty_/qs86#qs86#
         |[4:30..4:31): T => _empty_/qs86#qs86#T#
         |[5:8..5:9): b <= _empty_/qs86#qs86#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:27..5:28): T => _empty_/qs86#package.B#
         |[6:8..6:9): c <= _empty_/qs86#qs86#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local0
         |[6:43..6:44): X <= local0
         |[6:48..6:52): qs86 => _empty_/qs86#qs86#
         |[6:58..6:59): T => _empty_/qs86#qs86#T#
         |[7:8..7:9): d <= _empty_/qs86#qs86#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local1
         |[7:34..7:35): X <= local1
         |[7:39..7:43): qs86 => _empty_/qs86#qs86#
         |[7:49..7:50): T => _empty_/qs86#qs86#T#
         |[8:8..8:9): e <= _empty_/qs86#qs86#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:31): qs86 => _empty_/qs86#qs86#
         |[8:37..8:38): T => _empty_/qs86#qs86#T#
         |[8:40..8:43): Int => scala/Int#
         |[8:47..8:90): (x: qs86.this.T) => x match { case _ => 1 } <= local2
         |[8:48..8:49): x <= local9
         |[8:51..8:55): qs86 => _empty_/qs86#qs86#
         |[8:61..8:62): T => _empty_/qs86#qs86#T#
         |[8:67..8:68): x => local9
         |[9:8..9:9): f <= _empty_/qs86#qs86#f.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:23): AnyRef => java/lang/Object#
         |[9:30..9:31): m <= local10
         |[9:43..9:44): T => _empty_/qs86#package.B#
         |[10:10..10:11): I <= _empty_/qs86#qs86#I#
         |[10:12..10:12):  <= _empty_/qs86#qs86#I#`<init>`().
         |[10:14..10:15): s <= local11
         |[10:17..10:21): qs86 => _empty_/qs86#qs86#
         |[10:27..10:28): T => _empty_/qs86#qs86#T#
         |[11:10..11:11): J <= _empty_/qs86#qs86#J#
         |[11:12..11:12):  <= _empty_/qs86#qs86#J#`<init>`().
         |[11:30..11:32): PA => _empty_/qs86#qs86#PA#
         |[11:32..11:32):  => _empty_/qs86#package.P#`<init>`().
         |[12:8..12:9): h <= _empty_/qs86#qs86#h.
         |[12:12..12:16): List => scala/collection/immutable/Nil.
         |[12:17..12:23): AnyRef => java/lang/Object#
         |[12:30..12:31): m <= local12
         |[12:32..12:33): C <= local14
         |[12:35..12:35):  <= local13
         |[12:45..12:47): TC => _empty_/qs86#package.TCB#
         |[12:50..12:53): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:6..0:10): qs86 <= _empty_/qs86#
         |[0:11..0:11):  <= _empty_/qs86#`<init>`().
         |[1:9..1:18): `package` <= _empty_/qs86#package.
         |[1:27..1:28): B <= _empty_/qs86#package.B#
         |[1:28..1:28):  <= _empty_/qs86#package.B#`<init>`().
         |[1:36..1:39): TCB <= _empty_/qs86#package.TCB#
         |[1:40..1:41): C <= _empty_/qs86#package.TCB#[C]
         |[1:50..1:51): P <= _empty_/qs86#package.P#
         |[1:52..1:53): C <= _empty_/qs86#package.P#[C]
         |[1:54..1:54):  <= _empty_/qs86#package.P#`<init>`().
         |[1:55..1:56): c <= _empty_/qs86#package.P#`<init>`().(c)
         |[1:55..1:56): c <= _empty_/qs86#package.P#c.
         |[1:58..1:59): C => _empty_/qs86#package.P#[C]
         |[2:8..2:12): qs86 <= _empty_/qs86#qs86#
         |[2:13..2:13):  <= _empty_/qs86#qs86#`<init>`().
         |[3:9..3:10): T <= _empty_/qs86#qs86#T#
         |[3:13..3:22): `package` => _empty_/qs86#package.
         |[3:23..3:24): B => _empty_/qs86#package.B#
         |[3:31..3:33): TC <= _empty_/qs86#qs86#TC#
         |[3:34..3:35): C <= _empty_/qs86#qs86#TC#[C]
         |[3:39..3:48): `package` => _empty_/qs86#package.
         |[3:49..3:52): TCB => _empty_/qs86#package.TCB#
         |[3:53..3:54): C => _empty_/qs86#qs86#TC#[C]
         |[3:62..3:64): PA <= _empty_/qs86#qs86#PA#
         |[3:65..3:66): C <= _empty_/qs86#qs86#PA#[C]
         |[3:70..3:79): `package` => _empty_/qs86#package.
         |[3:80..3:81): P => _empty_/qs86#package.P#
         |[3:82..3:83): C => _empty_/qs86#qs86#PA#[C]
         |[4:8..4:9): a <= _empty_/qs86#qs86#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:20..4:24): qs86 => _empty_/qs86#qs86#
         |[4:30..4:31): T => _empty_/qs86#qs86#T#
         |[5:8..5:9): b <= _empty_/qs86#qs86#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:27..5:28): T => _empty_/qs86#package.B#
         |[6:8..6:9): c <= _empty_/qs86#qs86#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local0
         |[6:43..6:44): X <= local0
         |[6:48..6:52): qs86 => _empty_/qs86#qs86#
         |[6:58..6:59): T => _empty_/qs86#qs86#T#
         |[7:8..7:9): d <= _empty_/qs86#qs86#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local1
         |[7:34..7:35): X <= local1
         |[7:39..7:43): qs86 => _empty_/qs86#qs86#
         |[7:49..7:50): T => _empty_/qs86#qs86#T#
         |[8:8..8:9): e <= _empty_/qs86#qs86#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:31): qs86 => _empty_/qs86#qs86#
         |[8:37..8:38): T => _empty_/qs86#qs86#T#
         |[8:40..8:43): Int => scala/Int#
         |[8:47..8:90): (x: qs86.this.T) => x match { case _ => 1 } <= local2
         |[8:48..8:49): x <= local9
         |[8:67..8:68): x => local9
         |[9:8..9:9): f <= _empty_/qs86#qs86#f.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:23): AnyRef => java/lang/Object#
         |[9:30..9:31): m <= local10
         |[9:43..9:44): T => _empty_/qs86#package.B#
         |[10:10..10:11): I <= _empty_/qs86#qs86#I#
         |[10:12..10:12):  <= _empty_/qs86#qs86#I#`<init>`().
         |[10:14..10:15): s <= local11
         |[10:17..10:21): qs86 => _empty_/qs86#qs86#
         |[10:27..10:28): T => _empty_/qs86#qs86#T#
         |[11:10..11:11): J <= _empty_/qs86#qs86#J#
         |[11:12..11:12):  <= _empty_/qs86#qs86#J#`<init>`().
         |[11:30..11:32): PA => _empty_/qs86#qs86#PA#
         |[11:32..11:32):  => _empty_/qs86#package.P#`<init>`().
         |[12:8..12:9): h <= _empty_/qs86#qs86#h.
         |[12:12..12:16): List => scala/collection/immutable/Nil.
         |[12:17..12:23): AnyRef => java/lang/Object#
         |[12:30..12:31): m <= local12
         |[12:32..12:33): C <= local14
         |[12:35..12:35):  <= local13
         |[12:45..12:47): TC => _empty_/qs86#package.TCB#
         |[12:50..12:53): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "a type through a parameter and a local value",
    """|object qs25 {
       |  class X { class T }
       |  def a(w: X) = classOf[w.T]
       |  def b(w: X) = List[w.T]()
       |  def c(w: X) = classOf[List[Y] forSome { type Y <: w.T }]
       |  def e(w: X): PartialFunction[w.T, Int] = (x: w.T) => x match { case _ => 1 }
       |  def g(w: X) = List[w.type]()
       |  def h = { val w = new X; classOf[w.T] }
       |  def i = { val w = new X; List[w.T]() }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): qs25 <= _empty_/qs25.
         |[1:8..1:9): X <= _empty_/qs25.X#
         |[1:10..1:10):  <= _empty_/qs25.X#`<init>`().
         |[1:18..1:19): T <= _empty_/qs25.X#T#
         |[1:19..1:19):  <= _empty_/qs25.X#T#`<init>`().
         |[2:6..2:7): a <= _empty_/qs25.a().
         |[2:8..2:9): w <= _empty_/qs25.a().(w)
         |[2:11..2:12): X => _empty_/qs25.X#
         |[2:16..2:23): classOf => scala/Predef.classOf().
         |[2:24..2:25): w => _empty_/qs25.a().(w)
         |[2:26..2:27): T => _empty_/qs25.X#T#
         |[3:6..3:7): b <= _empty_/qs25.b().
         |[3:8..3:9): w <= _empty_/qs25.b().(w)
         |[3:11..3:12): X => _empty_/qs25.X#
         |[3:16..3:20): List => scala/collection/immutable/Nil.
         |[3:21..3:22): w => _empty_/qs25.b().(w)
         |[3:23..3:24): T => _empty_/qs25.X#T#
         |[4:6..4:7): c <= _empty_/qs25.c().
         |[4:8..4:9): w <= _empty_/qs25.c().(w)
         |[4:11..4:12): X => _empty_/qs25.X#
         |[4:16..4:23): classOf => scala/Predef.classOf().
         |[4:24..4:28): List => scala/package.List#
         |[4:29..4:30): Y => local0
         |[4:47..4:48): Y <= local0
         |[4:52..4:53): w => _empty_/qs25.c().(w)
         |[4:54..4:55): T => _empty_/qs25.X#T#
         |[5:6..5:7): e <= _empty_/qs25.e().
         |[5:8..5:9): w <= _empty_/qs25.e().(w)
         |[5:11..5:12): X => _empty_/qs25.X#
         |[5:15..5:30): PartialFunction => scala/PartialFunction#
         |[5:31..5:32): w => _empty_/qs25.e().(w)
         |[5:33..5:34): T => _empty_/qs25.X#T#
         |[5:36..5:39): Int => scala/Int#
         |[5:43..5:78): (x: w.T) => x match { case _ => 1 } <= local1
         |[5:44..5:45): x <= local8
         |[5:47..5:48): w => _empty_/qs25.e().(w)
         |[5:49..5:50): T => _empty_/qs25.X#T#
         |[5:55..5:56): x => local8
         |[6:6..6:7): g <= _empty_/qs25.g().
         |[6:8..6:9): w <= _empty_/qs25.g().(w)
         |[6:11..6:12): X => _empty_/qs25.X#
         |[6:16..6:20): List => scala/collection/immutable/Nil.
         |[6:21..6:22): w => _empty_/qs25.g().(w)
         |[7:6..7:7): h <= _empty_/qs25.h().
         |[7:16..7:17): w <= local10
         |[7:24..7:25): X => _empty_/qs25.X#
         |[7:25..7:25):  => _empty_/qs25.X#`<init>`().
         |[7:27..7:34): classOf => scala/Predef.classOf().
         |[7:35..7:36): w => local10
         |[7:37..7:38): T => _empty_/qs25.X#T#
         |[8:6..8:7): i <= _empty_/qs25.i().
         |[8:16..8:17): w <= local11
         |[8:24..8:25): X => _empty_/qs25.X#
         |[8:25..8:25):  => _empty_/qs25.X#`<init>`().
         |[8:27..8:31): List => scala/collection/immutable/Nil.
         |[8:32..8:33): w => local11
         |[8:34..8:35): T => _empty_/qs25.X#T#
         |""".stripMargin
    else
      """|[0:7..0:11): qs25 <= _empty_/qs25.
         |[1:8..1:9): X <= _empty_/qs25.X#
         |[1:10..1:10):  <= _empty_/qs25.X#`<init>`().
         |[1:18..1:19): T <= _empty_/qs25.X#T#
         |[1:19..1:19):  <= _empty_/qs25.X#T#`<init>`().
         |[2:6..2:7): a <= _empty_/qs25.a().
         |[2:8..2:9): w <= _empty_/qs25.a().(w)
         |[2:11..2:12): X => _empty_/qs25.X#
         |[2:16..2:23): classOf => scala/Predef.classOf().
         |[2:24..2:25): w => _empty_/qs25.a().(w)
         |[2:26..2:27): T => _empty_/qs25.X#T#
         |[3:6..3:7): b <= _empty_/qs25.b().
         |[3:8..3:9): w <= _empty_/qs25.b().(w)
         |[3:11..3:12): X => _empty_/qs25.X#
         |[3:16..3:20): List => scala/collection/immutable/Nil.
         |[3:21..3:22): w => _empty_/qs25.b().(w)
         |[3:23..3:24): T => _empty_/qs25.X#T#
         |[4:6..4:7): c <= _empty_/qs25.c().
         |[4:8..4:9): w <= _empty_/qs25.c().(w)
         |[4:11..4:12): X => _empty_/qs25.X#
         |[4:16..4:23): classOf => scala/Predef.classOf().
         |[4:24..4:28): List => scala/package.List#
         |[4:29..4:30): Y => local0
         |[4:47..4:48): Y <= local0
         |[4:52..4:53): w => _empty_/qs25.c().(w)
         |[4:54..4:55): T => _empty_/qs25.X#T#
         |[5:6..5:7): e <= _empty_/qs25.e().
         |[5:8..5:9): w <= _empty_/qs25.e().(w)
         |[5:11..5:12): X => _empty_/qs25.X#
         |[5:15..5:30): PartialFunction => scala/PartialFunction#
         |[5:31..5:32): w => _empty_/qs25.e().(w)
         |[5:33..5:34): T => _empty_/qs25.X#T#
         |[5:36..5:39): Int => scala/Int#
         |[5:43..5:78): (x: w.T) => x match { case _ => 1 } <= local1
         |[5:44..5:45): x <= local8
         |[5:55..5:56): x => local8
         |[6:6..6:7): g <= _empty_/qs25.g().
         |[6:8..6:9): w <= _empty_/qs25.g().(w)
         |[6:11..6:12): X => _empty_/qs25.X#
         |[6:16..6:20): List => scala/collection/immutable/Nil.
         |[6:21..6:22): w => _empty_/qs25.g().(w)
         |[7:6..7:7): h <= _empty_/qs25.h().
         |[7:16..7:17): w <= local10
         |[7:24..7:25): X => _empty_/qs25.X#
         |[7:25..7:25):  => _empty_/qs25.X#`<init>`().
         |[7:27..7:34): classOf => scala/Predef.classOf().
         |[7:35..7:36): w => local10
         |[7:37..7:38): T => _empty_/qs25.X#T#
         |[8:6..8:7): i <= _empty_/qs25.i().
         |[8:16..8:17): w <= local11
         |[8:24..8:25): X => _empty_/qs25.X#
         |[8:25..8:25):  => _empty_/qs25.X#`<init>`().
         |[8:27..8:31): List => scala/collection/immutable/Nil.
         |[8:32..8:33): w => local11
         |[8:34..8:35): T => _empty_/qs25.X#T#
         |""".stripMargin,
  )

  occurrences(
    "a self alias of a class in a term and in types",
    """|class qs32 { self =>
       |  class B
       |  val t = self.toString
       |  val a = classOf[self.B]
       |  val b = List[self.B]()
       |  val z: self.B = null
       |}
       |""".stripMargin,
    """|[0:6..0:10): qs32 <= _empty_/qs32#
       |[0:11..0:11):  <= _empty_/qs32#`<init>`().
       |[0:13..0:17): self <= local0
       |[1:8..1:9): B <= _empty_/qs32#B#
       |[1:9..1:9):  <= _empty_/qs32#B#`<init>`().
       |[2:6..2:7): t <= _empty_/qs32#t.
       |[2:10..2:14): self => local0
       |[2:15..2:23): toString => java/lang/Object#toString().
       |[3:6..3:7): a <= _empty_/qs32#a.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:22): self => local0
       |[3:23..3:24): B => _empty_/qs32#B#
       |[4:6..4:7): b <= _empty_/qs32#b.
       |[4:10..4:14): List => scala/collection/immutable/Nil.
       |[4:15..4:19): self => local0
       |[4:20..4:21): B => _empty_/qs32#B#
       |[5:6..5:7): z <= _empty_/qs32#z.
       |[5:9..5:13): self => local0
       |[5:14..5:15): B => _empty_/qs32#B#
       |""".stripMargin,
  )

  occurrences(
    "a self alias of a class in an inner class",
    """|class qs33 { self =>
       |  class B
       |  class I {
       |    val a = classOf[self.B]
       |    val b = List[self.B]()
       |    val e: PartialFunction[self.B, Int] = (x: self.B) => x match { case _ => 1 }
       |    val g = List[self.type]()
       |    val z: self.B = null
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): qs33 <= _empty_/qs33#
         |[0:11..0:11):  <= _empty_/qs33#`<init>`().
         |[0:13..0:17): self <= local0
         |[1:8..1:9): B <= _empty_/qs33#B#
         |[1:9..1:9):  <= _empty_/qs33#B#`<init>`().
         |[2:8..2:9): I <= _empty_/qs33#I#
         |[2:10..2:10):  <= _empty_/qs33#I#`<init>`().
         |[3:8..3:9): a <= _empty_/qs33#I#a.
         |[3:12..3:19): classOf => scala/Predef.classOf().
         |[3:20..3:24): self => local0
         |[3:25..3:26): B => _empty_/qs33#B#
         |[4:8..4:9): b <= _empty_/qs33#I#b.
         |[4:12..4:16): List => scala/collection/immutable/Nil.
         |[4:17..4:21): self => local0
         |[4:22..4:23): B => _empty_/qs33#B#
         |[5:8..5:9): e <= _empty_/qs33#I#e.
         |[5:11..5:26): PartialFunction => scala/PartialFunction#
         |[5:27..5:31): self => local0
         |[5:32..5:33): B => _empty_/qs33#B#
         |[5:35..5:38): Int => scala/Int#
         |[5:42..5:80): (x: self.B) => x match { case _ => 1 } <= local1
         |[5:43..5:44): x <= local8
         |[5:46..5:50): self => local0
         |[5:51..5:52): B => _empty_/qs33#B#
         |[5:57..5:58): x => local8
         |[6:8..6:9): g <= _empty_/qs33#I#g.
         |[6:12..6:16): List => scala/collection/immutable/Nil.
         |[7:8..7:9): z <= _empty_/qs33#I#z.
         |[7:11..7:15): self => local0
         |[7:16..7:17): B => _empty_/qs33#B#
         |""".stripMargin
    else
      """|[0:6..0:10): qs33 <= _empty_/qs33#
         |[0:11..0:11):  <= _empty_/qs33#`<init>`().
         |[0:13..0:17): self <= local0
         |[1:8..1:9): B <= _empty_/qs33#B#
         |[1:9..1:9):  <= _empty_/qs33#B#`<init>`().
         |[2:8..2:9): I <= _empty_/qs33#I#
         |[2:10..2:10):  <= _empty_/qs33#I#`<init>`().
         |[3:8..3:9): a <= _empty_/qs33#I#a.
         |[3:12..3:19): classOf => scala/Predef.classOf().
         |[3:20..3:24): self => local0
         |[3:25..3:26): B => _empty_/qs33#B#
         |[4:8..4:9): b <= _empty_/qs33#I#b.
         |[4:12..4:16): List => scala/collection/immutable/Nil.
         |[4:17..4:21): self => local0
         |[4:22..4:23): B => _empty_/qs33#B#
         |[5:8..5:9): e <= _empty_/qs33#I#e.
         |[5:11..5:26): PartialFunction => scala/PartialFunction#
         |[5:27..5:31): self => local0
         |[5:32..5:33): B => _empty_/qs33#B#
         |[5:35..5:38): Int => scala/Int#
         |[5:42..5:80): (x: self.B) => x match { case _ => 1 } <= local1
         |[5:43..5:44): x <= local8
         |[5:57..5:58): x => local8
         |[6:8..6:9): g <= _empty_/qs33#I#g.
         |[6:12..6:16): List => scala/collection/immutable/Nil.
         |[7:8..7:9): z <= _empty_/qs33#I#z.
         |[7:11..7:15): self => local0
         |[7:16..7:17): B => _empty_/qs33#B#
         |""".stripMargin,
  )

  occurrences(
    "a type through a self alias of an inner class, with a self alias of that name in the outer class",
    """|class qs69 { self =>
       |  class U; trait TCU[B]; class P[B](b: B)
       |  trait A { type T = qs69.this.U; type TC[B] = qs69.this.TCU[B]; type PA[B] = qs69.this.P[B] }
       |  class K extends A { self =>
       |    val a = classOf[self.T]
       |    val b = List[self.T]()
       |    val c = classOf[List[X] forSome { type X <: self.T }]
       |    val d: List[X] forSome { type X <: self.T } = null
       |    val e: PartialFunction[self.T, Int] = (x: self.T) => x match { case _ => 1 }
       |    val f = List[AnyRef { def m: self.T }]()
       |    val g = List[self.type]()
       |    class I { s: self.T => }
       |    class J extends self.PA(1)
       |    val h = List[AnyRef { def m[B: self.TC]: Int }]()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): qs69 <= _empty_/qs69#
         |[0:11..0:11):  <= _empty_/qs69#`<init>`().
         |[0:13..0:17): self <= local0
         |[1:8..1:9): U <= _empty_/qs69#U#
         |[1:9..1:9):  <= _empty_/qs69#U#`<init>`().
         |[1:17..1:20): TCU <= _empty_/qs69#TCU#
         |[1:21..1:22): B <= _empty_/qs69#TCU#[B]
         |[1:31..1:32): P <= _empty_/qs69#P#
         |[1:33..1:34): B <= _empty_/qs69#P#[B]
         |[1:35..1:35):  <= _empty_/qs69#P#`<init>`().
         |[1:36..1:37): b <= _empty_/qs69#P#`<init>`().(b)
         |[1:36..1:37): b <= _empty_/qs69#P#b.
         |[1:39..1:40): B => _empty_/qs69#P#[B]
         |[2:8..2:9): A <= _empty_/qs69#A#
         |[2:17..2:18): T <= _empty_/qs69#A#T#
         |[2:21..2:25): qs69 => _empty_/qs69#
         |[2:31..2:32): U => _empty_/qs69#U#
         |[2:39..2:41): TC <= _empty_/qs69#A#TC#
         |[2:42..2:43): B <= _empty_/qs69#A#TC#[B]
         |[2:47..2:51): qs69 => _empty_/qs69#
         |[2:57..2:60): TCU => _empty_/qs69#TCU#
         |[2:61..2:62): B => _empty_/qs69#A#TC#[B]
         |[2:70..2:72): PA <= _empty_/qs69#A#PA#
         |[2:73..2:74): B <= _empty_/qs69#A#PA#[B]
         |[2:78..2:82): qs69 => _empty_/qs69#
         |[2:88..2:89): P => _empty_/qs69#P#
         |[2:90..2:91): B => _empty_/qs69#A#PA#[B]
         |[3:8..3:9): K <= _empty_/qs69#K#
         |[3:10..3:10):  <= _empty_/qs69#K#`<init>`().
         |[3:18..3:19): A => _empty_/qs69#A#
         |[3:19..3:19):  => java/lang/Object#`<init>`().
         |[3:22..3:26): self <= local1
         |[4:8..4:9): a <= _empty_/qs69#K#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:20..4:24): self => local1
         |[4:25..4:26): T => _empty_/qs69#A#T#
         |[5:8..5:9): b <= _empty_/qs69#K#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:17..5:21): self => local0
         |[5:22..5:23): T => _empty_/qs69#U#
         |[6:8..6:9): c <= _empty_/qs69#K#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local2
         |[6:43..6:44): X <= local2
         |[6:48..6:52): self => local1
         |[6:53..6:54): T => _empty_/qs69#A#T#
         |[7:8..7:9): d <= _empty_/qs69#K#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local3
         |[7:34..7:35): X <= local3
         |[7:39..7:43): self => local1
         |[7:44..7:45): T => _empty_/qs69#A#T#
         |[8:8..8:9): e <= _empty_/qs69#K#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:31): self => local1
         |[8:32..8:33): T => _empty_/qs69#A#T#
         |[8:35..8:38): Int => scala/Int#
         |[8:42..8:80): (x: self.T) => x match { case _ => 1 } <= local4
         |[8:43..8:44): x <= local11
         |[8:46..8:50): self => local1
         |[8:51..8:52): T => _empty_/qs69#A#T#
         |[8:57..8:58): x => local11
         |[9:8..9:9): f <= _empty_/qs69#K#f.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:23): AnyRef => java/lang/Object#
         |[9:30..9:31): m <= local12
         |[9:33..9:37): self => local0
         |[9:38..9:39): T => _empty_/qs69#U#
         |[10:8..10:9): g <= _empty_/qs69#K#g.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[11:10..11:11): I <= _empty_/qs69#K#I#
         |[11:12..11:12):  <= _empty_/qs69#K#I#`<init>`().
         |[11:14..11:15): s <= local13
         |[11:17..11:21): self => local1
         |[11:22..11:23): T => _empty_/qs69#A#T#
         |[12:10..12:11): J <= _empty_/qs69#K#J#
         |[12:12..12:12):  <= _empty_/qs69#K#J#`<init>`().
         |[12:20..12:24): self => local1
         |[12:25..12:27): PA => _empty_/qs69#A#PA#
         |[12:27..12:27):  => _empty_/qs69#P#`<init>`().
         |[13:8..13:9): h <= _empty_/qs69#K#h.
         |[13:12..13:16): List => scala/collection/immutable/Nil.
         |[13:17..13:23): AnyRef => java/lang/Object#
         |[13:30..13:31): m <= local14
         |[13:32..13:33): B <= local16
         |[13:35..13:35):  <= local15
         |[13:35..13:39): self => local0
         |[13:40..13:42): TC => _empty_/qs69#TCU#
         |[13:45..13:48): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:6..0:10): qs69 <= _empty_/qs69#
         |[0:11..0:11):  <= _empty_/qs69#`<init>`().
         |[0:13..0:17): self <= local0
         |[1:8..1:9): U <= _empty_/qs69#U#
         |[1:9..1:9):  <= _empty_/qs69#U#`<init>`().
         |[1:17..1:20): TCU <= _empty_/qs69#TCU#
         |[1:21..1:22): B <= _empty_/qs69#TCU#[B]
         |[1:31..1:32): P <= _empty_/qs69#P#
         |[1:33..1:34): B <= _empty_/qs69#P#[B]
         |[1:35..1:35):  <= _empty_/qs69#P#`<init>`().
         |[1:36..1:37): b <= _empty_/qs69#P#`<init>`().(b)
         |[1:36..1:37): b <= _empty_/qs69#P#b.
         |[1:39..1:40): B => _empty_/qs69#P#[B]
         |[2:8..2:9): A <= _empty_/qs69#A#
         |[2:17..2:18): T <= _empty_/qs69#A#T#
         |[2:21..2:25): qs69 => _empty_/qs69#
         |[2:31..2:32): U => _empty_/qs69#U#
         |[2:39..2:41): TC <= _empty_/qs69#A#TC#
         |[2:42..2:43): B <= _empty_/qs69#A#TC#[B]
         |[2:47..2:51): qs69 => _empty_/qs69#
         |[2:57..2:60): TCU => _empty_/qs69#TCU#
         |[2:61..2:62): B => _empty_/qs69#A#TC#[B]
         |[2:70..2:72): PA <= _empty_/qs69#A#PA#
         |[2:73..2:74): B <= _empty_/qs69#A#PA#[B]
         |[2:78..2:82): qs69 => _empty_/qs69#
         |[2:88..2:89): P => _empty_/qs69#P#
         |[2:90..2:91): B => _empty_/qs69#A#PA#[B]
         |[3:8..3:9): K <= _empty_/qs69#K#
         |[3:10..3:10):  <= _empty_/qs69#K#`<init>`().
         |[3:18..3:19): A => _empty_/qs69#A#
         |[3:19..3:19):  => java/lang/Object#`<init>`().
         |[3:22..3:26): self <= local1
         |[4:8..4:9): a <= _empty_/qs69#K#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:20..4:24): self => local1
         |[4:25..4:26): T => _empty_/qs69#A#T#
         |[5:8..5:9): b <= _empty_/qs69#K#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:17..5:21): self => local0
         |[5:22..5:23): T => _empty_/qs69#U#
         |[6:8..6:9): c <= _empty_/qs69#K#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local2
         |[6:43..6:44): X <= local2
         |[6:48..6:52): self => local1
         |[6:53..6:54): T => _empty_/qs69#A#T#
         |[7:8..7:9): d <= _empty_/qs69#K#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local3
         |[7:34..7:35): X <= local3
         |[7:39..7:43): self => local1
         |[7:44..7:45): T => _empty_/qs69#A#T#
         |[8:8..8:9): e <= _empty_/qs69#K#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:31): self => local1
         |[8:32..8:33): T => _empty_/qs69#A#T#
         |[8:35..8:38): Int => scala/Int#
         |[8:42..8:80): (x: self.T) => x match { case _ => 1 } <= local4
         |[8:43..8:44): x <= local11
         |[8:57..8:58): x => local11
         |[9:8..9:9): f <= _empty_/qs69#K#f.
         |[9:12..9:16): List => scala/collection/immutable/Nil.
         |[9:17..9:23): AnyRef => java/lang/Object#
         |[9:30..9:31): m <= local12
         |[9:33..9:37): self => local0
         |[9:38..9:39): T => _empty_/qs69#U#
         |[10:8..10:9): g <= _empty_/qs69#K#g.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[11:10..11:11): I <= _empty_/qs69#K#I#
         |[11:12..11:12):  <= _empty_/qs69#K#I#`<init>`().
         |[11:14..11:15): s <= local13
         |[11:17..11:21): self => local1
         |[11:22..11:23): T => _empty_/qs69#A#T#
         |[12:10..12:11): J <= _empty_/qs69#K#J#
         |[12:12..12:12):  <= _empty_/qs69#K#J#`<init>`().
         |[12:20..12:24): self => local1
         |[12:25..12:27): PA => _empty_/qs69#A#PA#
         |[12:27..12:27):  => _empty_/qs69#P#`<init>`().
         |[13:8..13:9): h <= _empty_/qs69#K#h.
         |[13:12..13:16): List => scala/collection/immutable/Nil.
         |[13:17..13:23): AnyRef => java/lang/Object#
         |[13:30..13:31): m <= local14
         |[13:32..13:33): B <= local16
         |[13:35..13:35):  <= local15
         |[13:35..13:39): self => local0
         |[13:40..13:42): TC => _empty_/qs69#TCU#
         |[13:45..13:48): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "a type through a self alias of a class with an inherited val of that name",
    """|class qs80 {
       |  class U; trait TCU[B]; class P[B](b: B)
       |  class W { class T; trait TC[B]; class PA[B](b: B) }
       |  trait A {
       |    type T = qs80.this.U; type TC[B] = qs80.this.TCU[B]; type PA[B] = qs80.this.P[B]
       |    type F[X] = Map[A.this.type, X]
       |    val self: W = new W
       |  }
       |  class K extends A { self =>
       |    val a = classOf[self.T]
       |    val b = List[self.T]()
       |    val c = classOf[List[X] forSome { type X <: self.T }]
       |    val f = List[AnyRef { def m: self.T }]()
       |    val g = List[self.type]()
       |    class I { s: self.T => }
       |    val h = List[AnyRef { def m[B: self.TC]: Int }]()
       |    val i = List[AnyRef { def m: F[self.type] }]()
       |    val j = List[F[self.type]]()
       |    val k: Any = List[F[self.type]]()
       |    val l: Any = List[self.type]()
       |    val m: Any = classOf[self.T]
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:10): qs80 <= _empty_/qs80#
       |[0:11..0:11):  <= _empty_/qs80#`<init>`().
       |[1:8..1:9): U <= _empty_/qs80#U#
       |[1:9..1:9):  <= _empty_/qs80#U#`<init>`().
       |[1:17..1:20): TCU <= _empty_/qs80#TCU#
       |[1:21..1:22): B <= _empty_/qs80#TCU#[B]
       |[1:31..1:32): P <= _empty_/qs80#P#
       |[1:33..1:34): B <= _empty_/qs80#P#[B]
       |[1:35..1:35):  <= _empty_/qs80#P#`<init>`().
       |[1:36..1:37): b <= _empty_/qs80#P#`<init>`().(b)
       |[1:36..1:37): b <= _empty_/qs80#P#b.
       |[1:39..1:40): B => _empty_/qs80#P#[B]
       |[2:8..2:9): W <= _empty_/qs80#W#
       |[2:10..2:10):  <= _empty_/qs80#W#`<init>`().
       |[2:18..2:19): T <= _empty_/qs80#W#T#
       |[2:19..2:19):  <= _empty_/qs80#W#T#`<init>`().
       |[2:27..2:29): TC <= _empty_/qs80#W#TC#
       |[2:30..2:31): B <= _empty_/qs80#W#TC#[B]
       |[2:40..2:42): PA <= _empty_/qs80#W#PA#
       |[2:43..2:44): B <= _empty_/qs80#W#PA#[B]
       |[2:45..2:45):  <= _empty_/qs80#W#PA#`<init>`().
       |[2:46..2:47): b <= _empty_/qs80#W#PA#`<init>`().(b)
       |[2:46..2:47): b <= _empty_/qs80#W#PA#b.
       |[2:49..2:50): B => _empty_/qs80#W#PA#[B]
       |[3:8..3:9): A <= _empty_/qs80#A#
       |[3:10..3:10):  <= _empty_/qs80#A#`<init>`().
       |[4:9..4:10): T <= _empty_/qs80#A#T#
       |[4:13..4:17): qs80 => _empty_/qs80#
       |[4:23..4:24): U => _empty_/qs80#U#
       |[4:31..4:33): TC <= _empty_/qs80#A#TC#
       |[4:34..4:35): B <= _empty_/qs80#A#TC#[B]
       |[4:39..4:43): qs80 => _empty_/qs80#
       |[4:49..4:52): TCU => _empty_/qs80#TCU#
       |[4:53..4:54): B => _empty_/qs80#A#TC#[B]
       |[4:62..4:64): PA <= _empty_/qs80#A#PA#
       |[4:65..4:66): B <= _empty_/qs80#A#PA#[B]
       |[4:70..4:74): qs80 => _empty_/qs80#
       |[4:80..4:81): P => _empty_/qs80#P#
       |[4:82..4:83): B => _empty_/qs80#A#PA#[B]
       |[5:9..5:10): F <= _empty_/qs80#A#F#
       |[5:11..5:12): X <= _empty_/qs80#A#F#[X]
       |[5:16..5:19): Map => scala/Predef.Map#
       |[5:20..5:21): A => _empty_/qs80#A#
       |[5:33..5:34): X => _empty_/qs80#A#F#[X]
       |[6:8..6:12): self <= _empty_/qs80#A#self.
       |[6:14..6:15): W => _empty_/qs80#W#
       |[6:22..6:23): W => _empty_/qs80#W#
       |[6:23..6:23):  => _empty_/qs80#W#`<init>`().
       |[8:8..8:9): K <= _empty_/qs80#K#
       |[8:10..8:10):  <= _empty_/qs80#K#`<init>`().
       |[8:18..8:19): A => _empty_/qs80#A#
       |[8:19..8:19):  => java/lang/Object#`<init>`().
       |[8:22..8:26): self <= local0
       |[9:8..9:9): a <= _empty_/qs80#K#a.
       |[9:12..9:19): classOf => scala/Predef.classOf().
       |[9:20..9:24): self => _empty_/qs80#A#self.
       |[9:25..9:26): T => _empty_/qs80#W#T#
       |[10:8..10:9): b <= _empty_/qs80#K#b.
       |[10:12..10:16): List => scala/collection/immutable/Nil.
       |[10:17..10:21): self => _empty_/qs80#A#self.
       |[10:22..10:23): T => _empty_/qs80#W#T#
       |[11:8..11:9): c <= _empty_/qs80#K#c.
       |[11:12..11:19): classOf => scala/Predef.classOf().
       |[11:20..11:24): List => scala/package.List#
       |[11:25..11:26): X => local1
       |[11:43..11:44): X <= local1
       |[11:48..11:52): self => _empty_/qs80#A#self.
       |[11:53..11:54): T => _empty_/qs80#W#T#
       |[12:8..12:9): f <= _empty_/qs80#K#f.
       |[12:12..12:16): List => scala/collection/immutable/Nil.
       |[12:17..12:23): AnyRef => java/lang/Object#
       |[12:30..12:31): m <= local2
       |[12:33..12:37): self => _empty_/qs80#A#self.
       |[12:38..12:39): T => _empty_/qs80#W#T#
       |[13:8..13:9): g <= _empty_/qs80#K#g.
       |[13:12..13:16): List => scala/collection/immutable/Nil.
       |[13:17..13:21): self => _empty_/qs80#A#self.
       |[14:10..14:11): I <= _empty_/qs80#K#I#
       |[14:12..14:12):  <= _empty_/qs80#K#I#`<init>`().
       |[14:14..14:15): s <= local3
       |[14:17..14:21): self => _empty_/qs80#A#self.
       |[14:22..14:23): T => _empty_/qs80#W#T#
       |[15:8..15:9): h <= _empty_/qs80#K#h.
       |[15:12..15:16): List => scala/collection/immutable/Nil.
       |[15:17..15:23): AnyRef => java/lang/Object#
       |[15:30..15:31): m <= local4
       |[15:32..15:33): B <= local6
       |[15:35..15:35):  <= local5
       |[15:35..15:39): self => _empty_/qs80#A#self.
       |[15:40..15:42): TC => _empty_/qs80#W#TC#
       |[15:45..15:48): Int => scala/Int#
       |[16:8..16:9): i <= _empty_/qs80#K#i.
       |[16:12..16:16): List => scala/collection/immutable/Nil.
       |[16:17..16:23): AnyRef => java/lang/Object#
       |[16:30..16:31): m <= local7
       |[16:33..16:34): F => scala/collection/immutable/Map#
       |[17:8..17:9): j <= _empty_/qs80#K#j.
       |[17:12..17:16): List => scala/collection/immutable/Nil.
       |[17:17..17:18): F => scala/collection/immutable/Map#
       |[18:8..18:9): k <= _empty_/qs80#K#k.
       |[18:11..18:14): Any => scala/Any#
       |[18:17..18:21): List => scala/collection/immutable/Nil.
       |[18:22..18:23): F => scala/collection/immutable/Map#
       |[19:8..19:9): l <= _empty_/qs80#K#l.
       |[19:11..19:14): Any => scala/Any#
       |[19:17..19:21): List => scala/collection/immutable/Nil.
       |[20:8..20:9): m <= _empty_/qs80#K#m.
       |[20:11..20:14): Any => scala/Any#
       |[20:17..20:24): classOf => scala/Predef.classOf().
       |[20:25..20:29): self => local0
       |[20:30..20:31): T => _empty_/qs80#A#T#
       |""".stripMargin,
  )

  occurrences(
    "a type through a self alias of a class with an inherited object of that name",
    """|class qs81 {
       |  class U; trait TCU[B]; class P[B](b: B)
       |  class W { class T; trait TC[B]; class PA[B](b: B) }
       |  trait A {
       |    type T = qs81.this.U; type TC[B] = qs81.this.TCU[B]; type PA[B] = qs81.this.P[B]
       |    type F[X] = Map[A.this.type, X]
       |    object self { class T; trait TC[B]; class PA[B](b: B) }
       |  }
       |  class K extends A { self =>
       |    val a = classOf[self.T]
       |    val b = List[self.T]()
       |    val c = classOf[List[X] forSome { type X <: self.T }]
       |    val f = List[AnyRef { def m: self.T }]()
       |    val g = List[self.type]()
       |    class I { s: self.T => }
       |    val h = List[AnyRef { def m[B: self.TC]: Int }]()
       |    val i = List[AnyRef { def m: F[self.type] }]()
       |    val j = List[F[self.type]]()
       |    val k: Any = List[F[self.type]]()
       |    val l: Any = List[self.type]()
       |    val m: Any = classOf[self.T]
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:10): qs81 <= _empty_/qs81#
       |[0:11..0:11):  <= _empty_/qs81#`<init>`().
       |[1:8..1:9): U <= _empty_/qs81#U#
       |[1:9..1:9):  <= _empty_/qs81#U#`<init>`().
       |[1:17..1:20): TCU <= _empty_/qs81#TCU#
       |[1:21..1:22): B <= _empty_/qs81#TCU#[B]
       |[1:31..1:32): P <= _empty_/qs81#P#
       |[1:33..1:34): B <= _empty_/qs81#P#[B]
       |[1:35..1:35):  <= _empty_/qs81#P#`<init>`().
       |[1:36..1:37): b <= _empty_/qs81#P#`<init>`().(b)
       |[1:36..1:37): b <= _empty_/qs81#P#b.
       |[1:39..1:40): B => _empty_/qs81#P#[B]
       |[2:8..2:9): W <= _empty_/qs81#W#
       |[2:10..2:10):  <= _empty_/qs81#W#`<init>`().
       |[2:18..2:19): T <= _empty_/qs81#W#T#
       |[2:19..2:19):  <= _empty_/qs81#W#T#`<init>`().
       |[2:27..2:29): TC <= _empty_/qs81#W#TC#
       |[2:30..2:31): B <= _empty_/qs81#W#TC#[B]
       |[2:40..2:42): PA <= _empty_/qs81#W#PA#
       |[2:43..2:44): B <= _empty_/qs81#W#PA#[B]
       |[2:45..2:45):  <= _empty_/qs81#W#PA#`<init>`().
       |[2:46..2:47): b <= _empty_/qs81#W#PA#`<init>`().(b)
       |[2:46..2:47): b <= _empty_/qs81#W#PA#b.
       |[2:49..2:50): B => _empty_/qs81#W#PA#[B]
       |[3:8..3:9): A <= _empty_/qs81#A#
       |[3:10..3:10):  <= _empty_/qs81#A#`<init>`().
       |[4:9..4:10): T <= _empty_/qs81#A#T#
       |[4:13..4:17): qs81 => _empty_/qs81#
       |[4:23..4:24): U => _empty_/qs81#U#
       |[4:31..4:33): TC <= _empty_/qs81#A#TC#
       |[4:34..4:35): B <= _empty_/qs81#A#TC#[B]
       |[4:39..4:43): qs81 => _empty_/qs81#
       |[4:49..4:52): TCU => _empty_/qs81#TCU#
       |[4:53..4:54): B => _empty_/qs81#A#TC#[B]
       |[4:62..4:64): PA <= _empty_/qs81#A#PA#
       |[4:65..4:66): B <= _empty_/qs81#A#PA#[B]
       |[4:70..4:74): qs81 => _empty_/qs81#
       |[4:80..4:81): P => _empty_/qs81#P#
       |[4:82..4:83): B => _empty_/qs81#A#PA#[B]
       |[5:9..5:10): F <= _empty_/qs81#A#F#
       |[5:11..5:12): X <= _empty_/qs81#A#F#[X]
       |[5:16..5:19): Map => scala/Predef.Map#
       |[5:20..5:21): A => _empty_/qs81#A#
       |[5:33..5:34): X => _empty_/qs81#A#F#[X]
       |[6:11..6:15): self <= _empty_/qs81#A#self.
       |[6:24..6:25): T <= _empty_/qs81#A#self.T#
       |[6:25..6:25):  <= _empty_/qs81#A#self.T#`<init>`().
       |[6:33..6:35): TC <= _empty_/qs81#A#self.TC#
       |[6:36..6:37): B <= _empty_/qs81#A#self.TC#[B]
       |[6:46..6:48): PA <= _empty_/qs81#A#self.PA#
       |[6:49..6:50): B <= _empty_/qs81#A#self.PA#[B]
       |[6:51..6:51):  <= _empty_/qs81#A#self.PA#`<init>`().
       |[6:52..6:53): b <= _empty_/qs81#A#self.PA#`<init>`().(b)
       |[6:52..6:53): b <= _empty_/qs81#A#self.PA#b.
       |[6:55..6:56): B => _empty_/qs81#A#self.PA#[B]
       |[8:8..8:9): K <= _empty_/qs81#K#
       |[8:10..8:10):  <= _empty_/qs81#K#`<init>`().
       |[8:18..8:19): A => _empty_/qs81#A#
       |[8:19..8:19):  => java/lang/Object#`<init>`().
       |[8:22..8:26): self <= local0
       |[9:8..9:9): a <= _empty_/qs81#K#a.
       |[9:12..9:19): classOf => scala/Predef.classOf().
       |[9:20..9:24): self => _empty_/qs81#A#self.
       |[9:25..9:26): T => _empty_/qs81#A#self.T#
       |[10:8..10:9): b <= _empty_/qs81#K#b.
       |[10:12..10:16): List => scala/collection/immutable/Nil.
       |[10:17..10:21): self => _empty_/qs81#A#self.
       |[10:22..10:23): T => _empty_/qs81#A#self.T#
       |[11:8..11:9): c <= _empty_/qs81#K#c.
       |[11:12..11:19): classOf => scala/Predef.classOf().
       |[11:20..11:24): List => scala/package.List#
       |[11:25..11:26): X => local1
       |[11:43..11:44): X <= local1
       |[11:48..11:52): self => _empty_/qs81#A#self.
       |[11:53..11:54): T => _empty_/qs81#A#self.T#
       |[12:8..12:9): f <= _empty_/qs81#K#f.
       |[12:12..12:16): List => scala/collection/immutable/Nil.
       |[12:17..12:23): AnyRef => java/lang/Object#
       |[12:30..12:31): m <= local2
       |[12:33..12:37): self => _empty_/qs81#A#self.
       |[12:38..12:39): T => _empty_/qs81#A#self.T#
       |[13:8..13:9): g <= _empty_/qs81#K#g.
       |[13:12..13:16): List => scala/collection/immutable/Nil.
       |[13:17..13:21): self => _empty_/qs81#A#self.
       |[14:10..14:11): I <= _empty_/qs81#K#I#
       |[14:12..14:12):  <= _empty_/qs81#K#I#`<init>`().
       |[14:14..14:15): s <= local3
       |[14:17..14:21): self => _empty_/qs81#A#self.
       |[14:22..14:23): T => _empty_/qs81#A#self.T#
       |[15:8..15:9): h <= _empty_/qs81#K#h.
       |[15:12..15:16): List => scala/collection/immutable/Nil.
       |[15:17..15:23): AnyRef => java/lang/Object#
       |[15:30..15:31): m <= local4
       |[15:32..15:33): B <= local6
       |[15:35..15:35):  <= local5
       |[15:35..15:39): self => _empty_/qs81#A#self.
       |[15:40..15:42): TC => _empty_/qs81#A#self.TC#
       |[15:45..15:48): Int => scala/Int#
       |[16:8..16:9): i <= _empty_/qs81#K#i.
       |[16:12..16:16): List => scala/collection/immutable/Nil.
       |[16:17..16:23): AnyRef => java/lang/Object#
       |[16:30..16:31): m <= local7
       |[16:33..16:34): F => scala/collection/immutable/Map#
       |[17:8..17:9): j <= _empty_/qs81#K#j.
       |[17:12..17:16): List => scala/collection/immutable/Nil.
       |[17:17..17:18): F => scala/collection/immutable/Map#
       |[18:8..18:9): k <= _empty_/qs81#K#k.
       |[18:11..18:14): Any => scala/Any#
       |[18:17..18:21): List => scala/collection/immutable/Nil.
       |[18:22..18:23): F => scala/collection/immutable/Map#
       |[19:8..19:9): l <= _empty_/qs81#K#l.
       |[19:11..19:14): Any => scala/Any#
       |[19:17..19:21): List => scala/collection/immutable/Nil.
       |[20:8..20:9): m <= _empty_/qs81#K#m.
       |[20:11..20:14): Any => scala/Any#
       |[20:17..20:24): classOf => scala/Predef.classOf().
       |[20:25..20:29): self => local0
       |[20:30..20:31): T => _empty_/qs81#A#T#
       |""".stripMargin,
  )

  occurrences(
    "a type through a self alias of a class with an inherited method of that name",
    """|class qs82 {
       |  class U; trait TCU[B]; class P[B](b: B)
       |  class W { class T; trait TC[B]; class PA[B](b: B) }
       |  trait A {
       |    type T = qs82.this.U; type TC[B] = qs82.this.TCU[B]; type PA[B] = qs82.this.P[B]
       |    type F[X] = Map[A.this.type, X]
       |    def self: W = new W
       |  }
       |  class K extends A { self =>
       |    val k: Any = List[F[self.type]]()
       |    val l: Any = List[self.type]()
       |    val m: Any = classOf[self.T]
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:10): qs82 <= _empty_/qs82#
       |[0:11..0:11):  <= _empty_/qs82#`<init>`().
       |[1:8..1:9): U <= _empty_/qs82#U#
       |[1:9..1:9):  <= _empty_/qs82#U#`<init>`().
       |[1:17..1:20): TCU <= _empty_/qs82#TCU#
       |[1:21..1:22): B <= _empty_/qs82#TCU#[B]
       |[1:31..1:32): P <= _empty_/qs82#P#
       |[1:33..1:34): B <= _empty_/qs82#P#[B]
       |[1:35..1:35):  <= _empty_/qs82#P#`<init>`().
       |[1:36..1:37): b <= _empty_/qs82#P#`<init>`().(b)
       |[1:36..1:37): b <= _empty_/qs82#P#b.
       |[1:39..1:40): B => _empty_/qs82#P#[B]
       |[2:8..2:9): W <= _empty_/qs82#W#
       |[2:10..2:10):  <= _empty_/qs82#W#`<init>`().
       |[2:18..2:19): T <= _empty_/qs82#W#T#
       |[2:19..2:19):  <= _empty_/qs82#W#T#`<init>`().
       |[2:27..2:29): TC <= _empty_/qs82#W#TC#
       |[2:30..2:31): B <= _empty_/qs82#W#TC#[B]
       |[2:40..2:42): PA <= _empty_/qs82#W#PA#
       |[2:43..2:44): B <= _empty_/qs82#W#PA#[B]
       |[2:45..2:45):  <= _empty_/qs82#W#PA#`<init>`().
       |[2:46..2:47): b <= _empty_/qs82#W#PA#`<init>`().(b)
       |[2:46..2:47): b <= _empty_/qs82#W#PA#b.
       |[2:49..2:50): B => _empty_/qs82#W#PA#[B]
       |[3:8..3:9): A <= _empty_/qs82#A#
       |[3:10..3:10):  <= _empty_/qs82#A#`<init>`().
       |[4:9..4:10): T <= _empty_/qs82#A#T#
       |[4:13..4:17): qs82 => _empty_/qs82#
       |[4:23..4:24): U => _empty_/qs82#U#
       |[4:31..4:33): TC <= _empty_/qs82#A#TC#
       |[4:34..4:35): B <= _empty_/qs82#A#TC#[B]
       |[4:39..4:43): qs82 => _empty_/qs82#
       |[4:49..4:52): TCU => _empty_/qs82#TCU#
       |[4:53..4:54): B => _empty_/qs82#A#TC#[B]
       |[4:62..4:64): PA <= _empty_/qs82#A#PA#
       |[4:65..4:66): B <= _empty_/qs82#A#PA#[B]
       |[4:70..4:74): qs82 => _empty_/qs82#
       |[4:80..4:81): P => _empty_/qs82#P#
       |[4:82..4:83): B => _empty_/qs82#A#PA#[B]
       |[5:9..5:10): F <= _empty_/qs82#A#F#
       |[5:11..5:12): X <= _empty_/qs82#A#F#[X]
       |[5:16..5:19): Map => scala/Predef.Map#
       |[5:20..5:21): A => _empty_/qs82#A#
       |[5:33..5:34): X => _empty_/qs82#A#F#[X]
       |[6:8..6:12): self <= _empty_/qs82#A#self().
       |[6:14..6:15): W => _empty_/qs82#W#
       |[6:22..6:23): W => _empty_/qs82#W#
       |[6:23..6:23):  => _empty_/qs82#W#`<init>`().
       |[8:8..8:9): K <= _empty_/qs82#K#
       |[8:10..8:10):  <= _empty_/qs82#K#`<init>`().
       |[8:18..8:19): A => _empty_/qs82#A#
       |[8:19..8:19):  => java/lang/Object#`<init>`().
       |[8:22..8:26): self <= local0
       |[9:8..9:9): k <= _empty_/qs82#K#k.
       |[9:11..9:14): Any => scala/Any#
       |[9:17..9:21): List => scala/collection/immutable/Nil.
       |[9:22..9:23): F => scala/collection/immutable/Map#
       |[10:8..10:9): l <= _empty_/qs82#K#l.
       |[10:11..10:14): Any => scala/Any#
       |[10:17..10:21): List => scala/collection/immutable/Nil.
       |[11:8..11:9): m <= _empty_/qs82#K#m.
       |[11:11..11:14): Any => scala/Any#
       |[11:17..11:24): classOf => scala/Predef.classOf().
       |[11:25..11:29): self => local0
       |[11:30..11:31): T => _empty_/qs82#A#T#
       |""".stripMargin,
  )

  occurrences(
    "a type through a self alias of a class with an inherited type of that name",
    """|class qs83 {
       |  class U; trait TCU[B]; class P[B](b: B)
       |  class W { class T; trait TC[B]; class PA[B](b: B) }
       |  trait A {
       |    type T = qs83.this.U; type TC[B] = qs83.this.TCU[B]; type PA[B] = qs83.this.P[B]
       |    type F[X] = Map[A.this.type, X]
       |    type self = W
       |  }
       |  class K extends A { self =>
       |    val a = classOf[self.T]
       |    val b = List[self.T]()
       |    val c = classOf[List[X] forSome { type X <: self.T }]
       |    val d: List[X] forSome { type X <: self.T } = null
       |    val e: PartialFunction[self.T, Int] = (x: self.T) => x match { case _ => 1 }
       |    val f = List[AnyRef { def m: self.T }]()
       |    val g = List[self.type]()
       |    class I { s: self.T => }
       |    class J extends self.PA(1)
       |    val h = List[AnyRef { def m[B: self.TC]: Int }]()
       |    val i = List[AnyRef { def m: F[self.type] }]()
       |    val j = List[F[self.type]]()
       |    val k: Any = List[F[self.type]]()
       |    val l: Any = List[self.type]()
       |    val m: Any = classOf[self.T]
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): qs83 <= _empty_/qs83#
         |[0:11..0:11):  <= _empty_/qs83#`<init>`().
         |[1:8..1:9): U <= _empty_/qs83#U#
         |[1:9..1:9):  <= _empty_/qs83#U#`<init>`().
         |[1:17..1:20): TCU <= _empty_/qs83#TCU#
         |[1:21..1:22): B <= _empty_/qs83#TCU#[B]
         |[1:31..1:32): P <= _empty_/qs83#P#
         |[1:33..1:34): B <= _empty_/qs83#P#[B]
         |[1:35..1:35):  <= _empty_/qs83#P#`<init>`().
         |[1:36..1:37): b <= _empty_/qs83#P#`<init>`().(b)
         |[1:36..1:37): b <= _empty_/qs83#P#b.
         |[1:39..1:40): B => _empty_/qs83#P#[B]
         |[2:8..2:9): W <= _empty_/qs83#W#
         |[2:10..2:10):  <= _empty_/qs83#W#`<init>`().
         |[2:18..2:19): T <= _empty_/qs83#W#T#
         |[2:19..2:19):  <= _empty_/qs83#W#T#`<init>`().
         |[2:27..2:29): TC <= _empty_/qs83#W#TC#
         |[2:30..2:31): B <= _empty_/qs83#W#TC#[B]
         |[2:40..2:42): PA <= _empty_/qs83#W#PA#
         |[2:43..2:44): B <= _empty_/qs83#W#PA#[B]
         |[2:45..2:45):  <= _empty_/qs83#W#PA#`<init>`().
         |[2:46..2:47): b <= _empty_/qs83#W#PA#`<init>`().(b)
         |[2:46..2:47): b <= _empty_/qs83#W#PA#b.
         |[2:49..2:50): B => _empty_/qs83#W#PA#[B]
         |[3:8..3:9): A <= _empty_/qs83#A#
         |[4:9..4:10): T <= _empty_/qs83#A#T#
         |[4:13..4:17): qs83 => _empty_/qs83#
         |[4:23..4:24): U => _empty_/qs83#U#
         |[4:31..4:33): TC <= _empty_/qs83#A#TC#
         |[4:34..4:35): B <= _empty_/qs83#A#TC#[B]
         |[4:39..4:43): qs83 => _empty_/qs83#
         |[4:49..4:52): TCU => _empty_/qs83#TCU#
         |[4:53..4:54): B => _empty_/qs83#A#TC#[B]
         |[4:62..4:64): PA <= _empty_/qs83#A#PA#
         |[4:65..4:66): B <= _empty_/qs83#A#PA#[B]
         |[4:70..4:74): qs83 => _empty_/qs83#
         |[4:80..4:81): P => _empty_/qs83#P#
         |[4:82..4:83): B => _empty_/qs83#A#PA#[B]
         |[5:9..5:10): F <= _empty_/qs83#A#F#
         |[5:11..5:12): X <= _empty_/qs83#A#F#[X]
         |[5:16..5:19): Map => scala/Predef.Map#
         |[5:20..5:21): A => _empty_/qs83#A#
         |[5:33..5:34): X => _empty_/qs83#A#F#[X]
         |[6:9..6:13): self <= _empty_/qs83#A#self#
         |[6:16..6:17): W => _empty_/qs83#W#
         |[8:8..8:9): K <= _empty_/qs83#K#
         |[8:10..8:10):  <= _empty_/qs83#K#`<init>`().
         |[8:18..8:19): A => _empty_/qs83#A#
         |[8:19..8:19):  => java/lang/Object#`<init>`().
         |[8:22..8:26): self <= local0
         |[9:8..9:9): a <= _empty_/qs83#K#a.
         |[9:12..9:19): classOf => scala/Predef.classOf().
         |[9:20..9:24): self => local0
         |[9:25..9:26): T => _empty_/qs83#A#T#
         |[10:8..10:9): b <= _empty_/qs83#K#b.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[10:22..10:23): T => _empty_/qs83#U#
         |[11:8..11:9): c <= _empty_/qs83#K#c.
         |[11:12..11:19): classOf => scala/Predef.classOf().
         |[11:20..11:24): List => scala/package.List#
         |[11:25..11:26): X => local1
         |[11:43..11:44): X <= local1
         |[11:48..11:52): self => local0
         |[11:53..11:54): T => _empty_/qs83#A#T#
         |[12:8..12:9): d <= _empty_/qs83#K#d.
         |[12:11..12:15): List => scala/package.List#
         |[12:16..12:17): X => local2
         |[12:34..12:35): X <= local2
         |[12:39..12:43): self => local0
         |[12:44..12:45): T => _empty_/qs83#A#T#
         |[13:8..13:9): e <= _empty_/qs83#K#e.
         |[13:11..13:26): PartialFunction => scala/PartialFunction#
         |[13:27..13:31): self => local0
         |[13:32..13:33): T => _empty_/qs83#A#T#
         |[13:35..13:38): Int => scala/Int#
         |[13:42..13:80): (x: self.T) => x match { case _ => 1 } <= local3
         |[13:43..13:44): x <= local10
         |[13:46..13:50): self => local0
         |[13:51..13:52): T => _empty_/qs83#A#T#
         |[13:57..13:58): x => local10
         |[14:8..14:9): f <= _empty_/qs83#K#f.
         |[14:12..14:16): List => scala/collection/immutable/Nil.
         |[14:17..14:23): AnyRef => java/lang/Object#
         |[14:30..14:31): m <= local11
         |[14:38..14:39): T => _empty_/qs83#U#
         |[15:8..15:9): g <= _empty_/qs83#K#g.
         |[15:12..15:16): List => scala/collection/immutable/Nil.
         |[16:10..16:11): I <= _empty_/qs83#K#I#
         |[16:12..16:12):  <= _empty_/qs83#K#I#`<init>`().
         |[16:14..16:15): s <= local12
         |[16:17..16:21): self => local0
         |[16:22..16:23): T => _empty_/qs83#A#T#
         |[17:10..17:11): J <= _empty_/qs83#K#J#
         |[17:12..17:12):  <= _empty_/qs83#K#J#`<init>`().
         |[17:20..17:24): self => local0
         |[17:25..17:27): PA => _empty_/qs83#A#PA#
         |[17:27..17:27):  => _empty_/qs83#P#`<init>`().
         |[18:8..18:9): h <= _empty_/qs83#K#h.
         |[18:12..18:16): List => scala/collection/immutable/Nil.
         |[18:17..18:23): AnyRef => java/lang/Object#
         |[18:30..18:31): m <= local13
         |[18:32..18:33): B <= local15
         |[18:35..18:35):  <= local14
         |[18:40..18:42): TC => _empty_/qs83#TCU#
         |[18:45..18:48): Int => scala/Int#
         |[19:8..19:9): i <= _empty_/qs83#K#i.
         |[19:12..19:16): List => scala/collection/immutable/Nil.
         |[19:17..19:23): AnyRef => java/lang/Object#
         |[19:30..19:31): m <= local16
         |[19:33..19:34): F => scala/collection/immutable/Map#
         |[20:8..20:9): j <= _empty_/qs83#K#j.
         |[20:12..20:16): List => scala/collection/immutable/Nil.
         |[20:17..20:18): F => scala/collection/immutable/Map#
         |[21:8..21:9): k <= _empty_/qs83#K#k.
         |[21:11..21:14): Any => scala/Any#
         |[21:17..21:21): List => scala/collection/immutable/Nil.
         |[21:22..21:23): F => scala/collection/immutable/Map#
         |[22:8..22:9): l <= _empty_/qs83#K#l.
         |[22:11..22:14): Any => scala/Any#
         |[22:17..22:21): List => scala/collection/immutable/Nil.
         |[23:8..23:9): m <= _empty_/qs83#K#m.
         |[23:11..23:14): Any => scala/Any#
         |[23:17..23:24): classOf => scala/Predef.classOf().
         |[23:25..23:29): self => local0
         |[23:30..23:31): T => _empty_/qs83#A#T#
         |""".stripMargin
    else
      """|[0:6..0:10): qs83 <= _empty_/qs83#
         |[0:11..0:11):  <= _empty_/qs83#`<init>`().
         |[1:8..1:9): U <= _empty_/qs83#U#
         |[1:9..1:9):  <= _empty_/qs83#U#`<init>`().
         |[1:17..1:20): TCU <= _empty_/qs83#TCU#
         |[1:21..1:22): B <= _empty_/qs83#TCU#[B]
         |[1:31..1:32): P <= _empty_/qs83#P#
         |[1:33..1:34): B <= _empty_/qs83#P#[B]
         |[1:35..1:35):  <= _empty_/qs83#P#`<init>`().
         |[1:36..1:37): b <= _empty_/qs83#P#`<init>`().(b)
         |[1:36..1:37): b <= _empty_/qs83#P#b.
         |[1:39..1:40): B => _empty_/qs83#P#[B]
         |[2:8..2:9): W <= _empty_/qs83#W#
         |[2:10..2:10):  <= _empty_/qs83#W#`<init>`().
         |[2:18..2:19): T <= _empty_/qs83#W#T#
         |[2:19..2:19):  <= _empty_/qs83#W#T#`<init>`().
         |[2:27..2:29): TC <= _empty_/qs83#W#TC#
         |[2:30..2:31): B <= _empty_/qs83#W#TC#[B]
         |[2:40..2:42): PA <= _empty_/qs83#W#PA#
         |[2:43..2:44): B <= _empty_/qs83#W#PA#[B]
         |[2:45..2:45):  <= _empty_/qs83#W#PA#`<init>`().
         |[2:46..2:47): b <= _empty_/qs83#W#PA#`<init>`().(b)
         |[2:46..2:47): b <= _empty_/qs83#W#PA#b.
         |[2:49..2:50): B => _empty_/qs83#W#PA#[B]
         |[3:8..3:9): A <= _empty_/qs83#A#
         |[4:9..4:10): T <= _empty_/qs83#A#T#
         |[4:13..4:17): qs83 => _empty_/qs83#
         |[4:23..4:24): U => _empty_/qs83#U#
         |[4:31..4:33): TC <= _empty_/qs83#A#TC#
         |[4:34..4:35): B <= _empty_/qs83#A#TC#[B]
         |[4:39..4:43): qs83 => _empty_/qs83#
         |[4:49..4:52): TCU => _empty_/qs83#TCU#
         |[4:53..4:54): B => _empty_/qs83#A#TC#[B]
         |[4:62..4:64): PA <= _empty_/qs83#A#PA#
         |[4:65..4:66): B <= _empty_/qs83#A#PA#[B]
         |[4:70..4:74): qs83 => _empty_/qs83#
         |[4:80..4:81): P => _empty_/qs83#P#
         |[4:82..4:83): B => _empty_/qs83#A#PA#[B]
         |[5:9..5:10): F <= _empty_/qs83#A#F#
         |[5:11..5:12): X <= _empty_/qs83#A#F#[X]
         |[5:16..5:19): Map => scala/Predef.Map#
         |[5:20..5:21): A => _empty_/qs83#A#
         |[5:33..5:34): X => _empty_/qs83#A#F#[X]
         |[6:9..6:13): self <= _empty_/qs83#A#self#
         |[6:16..6:17): W => _empty_/qs83#W#
         |[8:8..8:9): K <= _empty_/qs83#K#
         |[8:10..8:10):  <= _empty_/qs83#K#`<init>`().
         |[8:18..8:19): A => _empty_/qs83#A#
         |[8:19..8:19):  => java/lang/Object#`<init>`().
         |[8:22..8:26): self <= local0
         |[9:8..9:9): a <= _empty_/qs83#K#a.
         |[9:12..9:19): classOf => scala/Predef.classOf().
         |[9:20..9:24): self => local0
         |[9:25..9:26): T => _empty_/qs83#A#T#
         |[10:8..10:9): b <= _empty_/qs83#K#b.
         |[10:12..10:16): List => scala/collection/immutable/Nil.
         |[10:22..10:23): T => _empty_/qs83#U#
         |[11:8..11:9): c <= _empty_/qs83#K#c.
         |[11:12..11:19): classOf => scala/Predef.classOf().
         |[11:20..11:24): List => scala/package.List#
         |[11:25..11:26): X => local1
         |[11:43..11:44): X <= local1
         |[11:48..11:52): self => local0
         |[11:53..11:54): T => _empty_/qs83#A#T#
         |[12:8..12:9): d <= _empty_/qs83#K#d.
         |[12:11..12:15): List => scala/package.List#
         |[12:16..12:17): X => local2
         |[12:34..12:35): X <= local2
         |[12:39..12:43): self => local0
         |[12:44..12:45): T => _empty_/qs83#A#T#
         |[13:8..13:9): e <= _empty_/qs83#K#e.
         |[13:11..13:26): PartialFunction => scala/PartialFunction#
         |[13:27..13:31): self => local0
         |[13:32..13:33): T => _empty_/qs83#A#T#
         |[13:35..13:38): Int => scala/Int#
         |[13:42..13:80): (x: self.T) => x match { case _ => 1 } <= local3
         |[13:43..13:44): x <= local10
         |[13:57..13:58): x => local10
         |[14:8..14:9): f <= _empty_/qs83#K#f.
         |[14:12..14:16): List => scala/collection/immutable/Nil.
         |[14:17..14:23): AnyRef => java/lang/Object#
         |[14:30..14:31): m <= local11
         |[14:38..14:39): T => _empty_/qs83#U#
         |[15:8..15:9): g <= _empty_/qs83#K#g.
         |[15:12..15:16): List => scala/collection/immutable/Nil.
         |[16:10..16:11): I <= _empty_/qs83#K#I#
         |[16:12..16:12):  <= _empty_/qs83#K#I#`<init>`().
         |[16:14..16:15): s <= local12
         |[16:17..16:21): self => local0
         |[16:22..16:23): T => _empty_/qs83#A#T#
         |[17:10..17:11): J <= _empty_/qs83#K#J#
         |[17:12..17:12):  <= _empty_/qs83#K#J#`<init>`().
         |[17:20..17:24): self => local0
         |[17:25..17:27): PA => _empty_/qs83#A#PA#
         |[17:27..17:27):  => _empty_/qs83#P#`<init>`().
         |[18:8..18:9): h <= _empty_/qs83#K#h.
         |[18:12..18:16): List => scala/collection/immutable/Nil.
         |[18:17..18:23): AnyRef => java/lang/Object#
         |[18:30..18:31): m <= local13
         |[18:32..18:33): B <= local15
         |[18:35..18:35):  <= local14
         |[18:40..18:42): TC => _empty_/qs83#TCU#
         |[18:45..18:48): Int => scala/Int#
         |[19:8..19:9): i <= _empty_/qs83#K#i.
         |[19:12..19:16): List => scala/collection/immutable/Nil.
         |[19:17..19:23): AnyRef => java/lang/Object#
         |[19:30..19:31): m <= local16
         |[19:33..19:34): F => scala/collection/immutable/Map#
         |[20:8..20:9): j <= _empty_/qs83#K#j.
         |[20:12..20:16): List => scala/collection/immutable/Nil.
         |[20:17..20:18): F => scala/collection/immutable/Map#
         |[21:8..21:9): k <= _empty_/qs83#K#k.
         |[21:11..21:14): Any => scala/Any#
         |[21:17..21:21): List => scala/collection/immutable/Nil.
         |[21:22..21:23): F => scala/collection/immutable/Map#
         |[22:8..22:9): l <= _empty_/qs83#K#l.
         |[22:11..22:14): Any => scala/Any#
         |[22:17..22:21): List => scala/collection/immutable/Nil.
         |[23:8..23:9): m <= _empty_/qs83#K#m.
         |[23:11..23:14): Any => scala/Any#
         |[23:17..23:24): classOf => scala/Predef.classOf().
         |[23:25..23:29): self => local0
         |[23:30..23:31): T => _empty_/qs83#A#T#
         |""".stripMargin,
  )

  occurrences(
    "a type through a self alias of a class with an object of that name",
    """|class qs84 {
       |  class U; trait TCU[B]; class P[B](b: B)
       |  class W { class T; trait TC[B]; class PA[B](b: B) }
       |  trait A {
       |    type T = qs84.this.U; type TC[B] = qs84.this.TCU[B]; type PA[B] = qs84.this.P[B]
       |    type F[X] = Map[A.this.type, X]
       |    
       |  }
       |  class K extends A { self =>
       |    object self { class T; trait TC[B]; class PA[B](b: B) }
       |    val a = classOf[self.T]
       |    val b = List[self.T]()
       |    val c = classOf[List[X] forSome { type X <: self.T }]
       |    val f = List[AnyRef { def m: self.T }]()
       |    val g = List[self.type]()
       |    class I { s: self.T => }
       |    val h = List[AnyRef { def m[B: self.TC]: Int }]()
       |    val i = List[AnyRef { def m: F[self.type] }]()
       |    val j = List[F[self.type]]()
       |    val k: Any = List[F[self.type]]()
       |    val l: Any = List[self.type]()
       |    val m: Any = classOf[self.T]
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:10): qs84 <= _empty_/qs84#
       |[0:11..0:11):  <= _empty_/qs84#`<init>`().
       |[1:8..1:9): U <= _empty_/qs84#U#
       |[1:9..1:9):  <= _empty_/qs84#U#`<init>`().
       |[1:17..1:20): TCU <= _empty_/qs84#TCU#
       |[1:21..1:22): B <= _empty_/qs84#TCU#[B]
       |[1:31..1:32): P <= _empty_/qs84#P#
       |[1:33..1:34): B <= _empty_/qs84#P#[B]
       |[1:35..1:35):  <= _empty_/qs84#P#`<init>`().
       |[1:36..1:37): b <= _empty_/qs84#P#`<init>`().(b)
       |[1:36..1:37): b <= _empty_/qs84#P#b.
       |[1:39..1:40): B => _empty_/qs84#P#[B]
       |[2:8..2:9): W <= _empty_/qs84#W#
       |[2:10..2:10):  <= _empty_/qs84#W#`<init>`().
       |[2:18..2:19): T <= _empty_/qs84#W#T#
       |[2:19..2:19):  <= _empty_/qs84#W#T#`<init>`().
       |[2:27..2:29): TC <= _empty_/qs84#W#TC#
       |[2:30..2:31): B <= _empty_/qs84#W#TC#[B]
       |[2:40..2:42): PA <= _empty_/qs84#W#PA#
       |[2:43..2:44): B <= _empty_/qs84#W#PA#[B]
       |[2:45..2:45):  <= _empty_/qs84#W#PA#`<init>`().
       |[2:46..2:47): b <= _empty_/qs84#W#PA#`<init>`().(b)
       |[2:46..2:47): b <= _empty_/qs84#W#PA#b.
       |[2:49..2:50): B => _empty_/qs84#W#PA#[B]
       |[3:8..3:9): A <= _empty_/qs84#A#
       |[4:9..4:10): T <= _empty_/qs84#A#T#
       |[4:13..4:17): qs84 => _empty_/qs84#
       |[4:23..4:24): U => _empty_/qs84#U#
       |[4:31..4:33): TC <= _empty_/qs84#A#TC#
       |[4:34..4:35): B <= _empty_/qs84#A#TC#[B]
       |[4:39..4:43): qs84 => _empty_/qs84#
       |[4:49..4:52): TCU => _empty_/qs84#TCU#
       |[4:53..4:54): B => _empty_/qs84#A#TC#[B]
       |[4:62..4:64): PA <= _empty_/qs84#A#PA#
       |[4:65..4:66): B <= _empty_/qs84#A#PA#[B]
       |[4:70..4:74): qs84 => _empty_/qs84#
       |[4:80..4:81): P => _empty_/qs84#P#
       |[4:82..4:83): B => _empty_/qs84#A#PA#[B]
       |[5:9..5:10): F <= _empty_/qs84#A#F#
       |[5:11..5:12): X <= _empty_/qs84#A#F#[X]
       |[5:16..5:19): Map => scala/Predef.Map#
       |[5:20..5:21): A => _empty_/qs84#A#
       |[5:33..5:34): X => _empty_/qs84#A#F#[X]
       |[8:8..8:9): K <= _empty_/qs84#K#
       |[8:10..8:10):  <= _empty_/qs84#K#`<init>`().
       |[8:18..8:19): A => _empty_/qs84#A#
       |[8:19..8:19):  => java/lang/Object#`<init>`().
       |[8:22..8:26): self <= local0
       |[9:11..9:15): self <= _empty_/qs84#K#self.
       |[9:24..9:25): T <= _empty_/qs84#K#self.T#
       |[9:25..9:25):  <= _empty_/qs84#K#self.T#`<init>`().
       |[9:33..9:35): TC <= _empty_/qs84#K#self.TC#
       |[9:36..9:37): B <= _empty_/qs84#K#self.TC#[B]
       |[9:46..9:48): PA <= _empty_/qs84#K#self.PA#
       |[9:49..9:50): B <= _empty_/qs84#K#self.PA#[B]
       |[9:51..9:51):  <= _empty_/qs84#K#self.PA#`<init>`().
       |[9:52..9:53): b <= _empty_/qs84#K#self.PA#`<init>`().(b)
       |[9:52..9:53): b <= _empty_/qs84#K#self.PA#b.
       |[9:55..9:56): B => _empty_/qs84#K#self.PA#[B]
       |[10:8..10:9): a <= _empty_/qs84#K#a.
       |[10:12..10:19): classOf => scala/Predef.classOf().
       |[10:20..10:24): self => _empty_/qs84#K#self.
       |[10:25..10:26): T => _empty_/qs84#K#self.T#
       |[11:8..11:9): b <= _empty_/qs84#K#b.
       |[11:12..11:16): List => scala/collection/immutable/Nil.
       |[11:17..11:21): self => _empty_/qs84#K#self.
       |[11:22..11:23): T => _empty_/qs84#K#self.T#
       |[12:8..12:9): c <= _empty_/qs84#K#c.
       |[12:12..12:19): classOf => scala/Predef.classOf().
       |[12:20..12:24): List => scala/package.List#
       |[12:25..12:26): X => local1
       |[12:43..12:44): X <= local1
       |[12:48..12:52): self => _empty_/qs84#K#self.
       |[12:53..12:54): T => _empty_/qs84#K#self.T#
       |[13:8..13:9): f <= _empty_/qs84#K#f.
       |[13:12..13:16): List => scala/collection/immutable/Nil.
       |[13:17..13:23): AnyRef => java/lang/Object#
       |[13:30..13:31): m <= local2
       |[13:33..13:37): self => _empty_/qs84#K#self.
       |[13:38..13:39): T => _empty_/qs84#K#self.T#
       |[14:8..14:9): g <= _empty_/qs84#K#g.
       |[14:12..14:16): List => scala/collection/immutable/Nil.
       |[14:17..14:21): self => _empty_/qs84#K#self.
       |[15:10..15:11): I <= _empty_/qs84#K#I#
       |[15:12..15:12):  <= _empty_/qs84#K#I#`<init>`().
       |[15:14..15:15): s <= local3
       |[15:17..15:21): self => _empty_/qs84#K#self.
       |[15:22..15:23): T => _empty_/qs84#K#self.T#
       |[16:8..16:9): h <= _empty_/qs84#K#h.
       |[16:12..16:16): List => scala/collection/immutable/Nil.
       |[16:17..16:23): AnyRef => java/lang/Object#
       |[16:30..16:31): m <= local4
       |[16:32..16:33): B <= local6
       |[16:35..16:35):  <= local5
       |[16:35..16:39): self => _empty_/qs84#K#self.
       |[16:40..16:42): TC => _empty_/qs84#K#self.TC#
       |[16:45..16:48): Int => scala/Int#
       |[17:8..17:9): i <= _empty_/qs84#K#i.
       |[17:12..17:16): List => scala/collection/immutable/Nil.
       |[17:17..17:23): AnyRef => java/lang/Object#
       |[17:30..17:31): m <= local7
       |[17:33..17:34): F => scala/collection/immutable/Map#
       |[18:8..18:9): j <= _empty_/qs84#K#j.
       |[18:12..18:16): List => scala/collection/immutable/Nil.
       |[18:17..18:18): F => scala/collection/immutable/Map#
       |[19:8..19:9): k <= _empty_/qs84#K#k.
       |[19:11..19:14): Any => scala/Any#
       |[19:17..19:21): List => scala/collection/immutable/Nil.
       |[19:22..19:23): F => scala/collection/immutable/Map#
       |[20:8..20:9): l <= _empty_/qs84#K#l.
       |[20:11..20:14): Any => scala/Any#
       |[20:17..20:21): List => scala/collection/immutable/Nil.
       |[21:8..21:9): m <= _empty_/qs84#K#m.
       |[21:11..21:14): Any => scala/Any#
       |[21:17..21:24): classOf => scala/Predef.classOf().
       |[21:25..21:29): self => local0
       |[21:30..21:31): T => _empty_/qs84#A#T#
       |""".stripMargin,
  )

  occurrences(
    "a type through super",
    """|class qs26a { class T }
       |class qs26 extends qs26a {
       |  val a = classOf[super.T]
       |  val b = List[qs26.super.T]()
       |  val c = classOf[qs26.super[qs26a].T]
       |  val e: PartialFunction[super.T, Int] = (x: super.T) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:11): qs26a <= _empty_/qs26a#
         |[0:12..0:12):  <= _empty_/qs26a#`<init>`().
         |[0:20..0:21): T <= _empty_/qs26a#T#
         |[0:21..0:21):  <= _empty_/qs26a#T#`<init>`().
         |[1:6..1:10): qs26 <= _empty_/qs26#
         |[1:11..1:11):  <= _empty_/qs26#`<init>`().
         |[1:19..1:24): qs26a => _empty_/qs26a#
         |[1:24..1:24):  => _empty_/qs26a#`<init>`().
         |[2:6..2:7): a <= _empty_/qs26#a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:24..2:25): T => _empty_/qs26a#T#
         |[3:6..3:7): b <= _empty_/qs26#b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:26..3:27): T => _empty_/qs26a#T#
         |[4:6..4:7): c <= _empty_/qs26#c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:36..4:37): T => _empty_/qs26a#T#
         |[5:6..5:7): e <= _empty_/qs26#e.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:25):  => _empty_/qs26#
         |[5:30..5:30):  => _empty_/qs26a#
         |[5:31..5:32): T => _empty_/qs26a#T#
         |[5:34..5:37): Int => scala/Int#
         |[5:41..5:80): (x: super.T) => x match { case _ => 1 } <= local0
         |[5:42..5:43): x <= local7
         |[5:51..5:52): T => _empty_/qs26a#T#
         |[5:57..5:58): x => local7
         |""".stripMargin
    else
      """|[0:6..0:11): qs26a <= _empty_/qs26a#
         |[0:12..0:12):  <= _empty_/qs26a#`<init>`().
         |[0:20..0:21): T <= _empty_/qs26a#T#
         |[0:21..0:21):  <= _empty_/qs26a#T#`<init>`().
         |[1:6..1:10): qs26 <= _empty_/qs26#
         |[1:11..1:11):  <= _empty_/qs26#`<init>`().
         |[1:19..1:24): qs26a => _empty_/qs26a#
         |[1:24..1:24):  => _empty_/qs26a#`<init>`().
         |[2:6..2:7): a <= _empty_/qs26#a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:24..2:25): T => _empty_/qs26a#T#
         |[3:6..3:7): b <= _empty_/qs26#b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:26..3:27): T => _empty_/qs26a#T#
         |[4:6..4:7): c <= _empty_/qs26#c.
         |[4:10..4:17): classOf => scala/Predef.classOf().
         |[4:36..4:37): T => _empty_/qs26a#T#
         |[5:6..5:7): e <= _empty_/qs26#e.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:25):  => _empty_/qs26#
         |[5:30..5:30):  => _empty_/qs26a#
         |[5:31..5:32): T => _empty_/qs26a#T#
         |[5:34..5:37): Int => scala/Int#
         |[5:41..5:80): (x: super.T) => x match { case _ => 1 } <= local0
         |[5:42..5:43): x <= local7
         |[5:57..5:58): x => local7
         |""".stripMargin,
  )

  occurrences(
    "a type through a stable value of super",
    """|class qs35x { class T }
       |class qs35a { val x = new qs35x }
       |class qs35 extends qs35a {
       |  val a1 = classOf[super.x.T]
       |  val b1 = List[super.x.T]()
       |  val c1 = classOf[List[X] forSome { type X <: super.x.T }]
       |  val e1: PartialFunction[super.x.T, Int] = (y: super.x.T) => y match { case _ => 1 }
       |  val g1 = List[super.x.type]()
       |  val a2 = classOf[qs35.super[qs35a].x.T]
       |  val b2 = List[qs35.super[qs35a].x.T]()
       |  val c2 = classOf[List[X] forSome { type X <: qs35.super[qs35a].x.T }]
       |  val e2: PartialFunction[qs35.super[qs35a].x.T, Int] = (y: qs35.super[qs35a].x.T) => y match { case _ => 1 }
       |  val g2 = List[qs35.super[qs35a].x.type]()
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:11): qs35x <= _empty_/qs35x#
         |[0:12..0:12):  <= _empty_/qs35x#`<init>`().
         |[0:20..0:21): T <= _empty_/qs35x#T#
         |[0:21..0:21):  <= _empty_/qs35x#T#`<init>`().
         |[1:6..1:11): qs35a <= _empty_/qs35a#
         |[1:12..1:12):  <= _empty_/qs35a#`<init>`().
         |[1:18..1:19): x <= _empty_/qs35a#x.
         |[1:26..1:31): qs35x => _empty_/qs35x#
         |[1:31..1:31):  => _empty_/qs35x#`<init>`().
         |[2:6..2:10): qs35 <= _empty_/qs35#
         |[2:11..2:11):  <= _empty_/qs35#`<init>`().
         |[2:19..2:24): qs35a => _empty_/qs35a#
         |[2:24..2:24):  => _empty_/qs35a#`<init>`().
         |[3:6..3:8): a1 <= _empty_/qs35#a1.
         |[3:11..3:18): classOf => scala/Predef.classOf().
         |[3:25..3:26): x => _empty_/qs35a#x.
         |[3:27..3:28): T => _empty_/qs35x#T#
         |[4:6..4:8): b1 <= _empty_/qs35#b1.
         |[4:11..4:15): List => scala/collection/immutable/Nil.
         |[4:22..4:23): x => _empty_/qs35a#x.
         |[4:24..4:25): T => _empty_/qs35x#T#
         |[5:6..5:8): c1 <= _empty_/qs35#c1.
         |[5:11..5:18): classOf => scala/Predef.classOf().
         |[5:19..5:23): List => scala/package.List#
         |[5:24..5:25): X => local0
         |[5:42..5:43): X <= local0
         |[5:53..5:54): x => _empty_/qs35a#x.
         |[5:55..5:56): T => _empty_/qs35x#T#
         |[6:6..6:8): e1 <= _empty_/qs35#e1.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:26):  => _empty_/qs35#
         |[6:31..6:31):  => _empty_/qs35a#
         |[6:32..6:33): x => _empty_/qs35a#x.
         |[6:34..6:35): T => _empty_/qs35x#T#
         |[6:37..6:40): Int => scala/Int#
         |[6:44..6:85): (y: super.x.T) => y match { case _ => 1 } <= local1
         |[6:45..6:46): y <= local8
         |[6:54..6:55): x => _empty_/qs35a#x.
         |[6:56..6:57): T => _empty_/qs35x#T#
         |[6:62..6:63): y => local8
         |[7:6..7:8): g1 <= _empty_/qs35#g1.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:22..7:23): x => _empty_/qs35a#x.
         |[8:6..8:8): a2 <= _empty_/qs35#a2.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:37..8:38): x => _empty_/qs35a#x.
         |[8:39..8:40): T => _empty_/qs35x#T#
         |[9:6..9:8): b2 <= _empty_/qs35#b2.
         |[9:11..9:15): List => scala/collection/immutable/Nil.
         |[9:34..9:35): x => _empty_/qs35a#x.
         |[9:36..9:37): T => _empty_/qs35x#T#
         |[10:6..10:8): c2 <= _empty_/qs35#c2.
         |[10:11..10:18): classOf => scala/Predef.classOf().
         |[10:19..10:23): List => scala/package.List#
         |[10:24..10:25): X => local9
         |[10:42..10:43): X <= local9
         |[10:65..10:66): x => _empty_/qs35a#x.
         |[10:67..10:68): T => _empty_/qs35x#T#
         |[11:6..11:8): e2 <= _empty_/qs35#e2.
         |[11:10..11:25): PartialFunction => scala/PartialFunction#
         |[11:26..11:30): qs35 => _empty_/qs35#
         |[11:37..11:42): qs35a => _empty_/qs35a#
         |[11:44..11:45): x => _empty_/qs35a#x.
         |[11:46..11:47): T => _empty_/qs35x#T#
         |[11:49..11:52): Int => scala/Int#
         |[11:56..11:109): (y: qs35.super[qs35a].x.T) => y match { case _ => 1 } <= local10
         |[11:57..11:58): y <= local17
         |[11:78..11:79): x => _empty_/qs35a#x.
         |[11:80..11:81): T => _empty_/qs35x#T#
         |[11:86..11:87): y => local17
         |[12:6..12:8): g2 <= _empty_/qs35#g2.
         |[12:11..12:15): List => scala/collection/immutable/Nil.
         |[12:34..12:35): x => _empty_/qs35a#x.
         |""".stripMargin
    else
      """|[0:6..0:11): qs35x <= _empty_/qs35x#
         |[0:12..0:12):  <= _empty_/qs35x#`<init>`().
         |[0:20..0:21): T <= _empty_/qs35x#T#
         |[0:21..0:21):  <= _empty_/qs35x#T#`<init>`().
         |[1:6..1:11): qs35a <= _empty_/qs35a#
         |[1:12..1:12):  <= _empty_/qs35a#`<init>`().
         |[1:18..1:19): x <= _empty_/qs35a#x.
         |[1:26..1:31): qs35x => _empty_/qs35x#
         |[1:31..1:31):  => _empty_/qs35x#`<init>`().
         |[2:6..2:10): qs35 <= _empty_/qs35#
         |[2:11..2:11):  <= _empty_/qs35#`<init>`().
         |[2:19..2:24): qs35a => _empty_/qs35a#
         |[2:24..2:24):  => _empty_/qs35a#`<init>`().
         |[3:6..3:8): a1 <= _empty_/qs35#a1.
         |[3:11..3:18): classOf => scala/Predef.classOf().
         |[3:25..3:26): x => _empty_/qs35a#x.
         |[3:27..3:28): T => _empty_/qs35x#T#
         |[4:6..4:8): b1 <= _empty_/qs35#b1.
         |[4:11..4:15): List => scala/collection/immutable/Nil.
         |[4:22..4:23): x => _empty_/qs35a#x.
         |[4:24..4:25): T => _empty_/qs35x#T#
         |[5:6..5:8): c1 <= _empty_/qs35#c1.
         |[5:11..5:18): classOf => scala/Predef.classOf().
         |[5:19..5:23): List => scala/package.List#
         |[5:24..5:25): X => local0
         |[5:42..5:43): X <= local0
         |[5:53..5:54): x => _empty_/qs35a#x.
         |[5:55..5:56): T => _empty_/qs35x#T#
         |[6:6..6:8): e1 <= _empty_/qs35#e1.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:26):  => _empty_/qs35#
         |[6:31..6:31):  => _empty_/qs35a#
         |[6:32..6:33): x => _empty_/qs35a#x.
         |[6:34..6:35): T => _empty_/qs35x#T#
         |[6:37..6:40): Int => scala/Int#
         |[6:44..6:85): (y: super.x.T) => y match { case _ => 1 } <= local1
         |[6:45..6:46): y <= local8
         |[6:62..6:63): y => local8
         |[7:6..7:8): g1 <= _empty_/qs35#g1.
         |[7:11..7:15): List => scala/collection/immutable/Nil.
         |[7:22..7:23): x => _empty_/qs35a#x.
         |[8:6..8:8): a2 <= _empty_/qs35#a2.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:37..8:38): x => _empty_/qs35a#x.
         |[8:39..8:40): T => _empty_/qs35x#T#
         |[9:6..9:8): b2 <= _empty_/qs35#b2.
         |[9:11..9:15): List => scala/collection/immutable/Nil.
         |[9:34..9:35): x => _empty_/qs35a#x.
         |[9:36..9:37): T => _empty_/qs35x#T#
         |[10:6..10:8): c2 <= _empty_/qs35#c2.
         |[10:11..10:18): classOf => scala/Predef.classOf().
         |[10:19..10:23): List => scala/package.List#
         |[10:24..10:25): X => local9
         |[10:42..10:43): X <= local9
         |[10:65..10:66): x => _empty_/qs35a#x.
         |[10:67..10:68): T => _empty_/qs35x#T#
         |[11:6..11:8): e2 <= _empty_/qs35#e2.
         |[11:10..11:25): PartialFunction => scala/PartialFunction#
         |[11:26..11:30): qs35 => _empty_/qs35#
         |[11:37..11:42): qs35a => _empty_/qs35a#
         |[11:44..11:45): x => _empty_/qs35a#x.
         |[11:46..11:47): T => _empty_/qs35x#T#
         |[11:49..11:52): Int => scala/Int#
         |[11:56..11:109): (y: qs35.super[qs35a].x.T) => y match { case _ => 1 } <= local10
         |[11:57..11:58): y <= local17
         |[11:86..11:87): y => local17
         |[12:6..12:8): g2 <= _empty_/qs35#g2.
         |[12:11..12:15): List => scala/collection/immutable/Nil.
         |[12:34..12:35): x => _empty_/qs35a#x.
         |""".stripMargin,
  )

  occurrences(
    "a type through super of a class with a trait",
    """|class qs36a
       |trait qs36t { class T }
       |class qs36 extends qs36a with qs36t {
       |  val a1 = classOf[super.T]
       |  val b1 = List[super.T]()
       |  val c1 = classOf[List[X] forSome { type X <: super.T }]
       |  val e1: PartialFunction[super.T, Int] = (x: super.T) => x match { case _ => 1 }
       |  val a2 = classOf[qs36.super.T]
       |  val a3 = classOf[qs36.super[qs36t].T]
       |  val e3: PartialFunction[qs36.super[qs36t].T, Int] = (x: qs36.super[qs36t].T) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:11): qs36a <= _empty_/qs36a#
         |[0:11..0:11):  <= _empty_/qs36a#`<init>`().
         |[1:6..1:11): qs36t <= _empty_/qs36t#
         |[1:12..1:12):  <= _empty_/qs36t#`<init>`().
         |[1:20..1:21): T <= _empty_/qs36t#T#
         |[1:21..1:21):  <= _empty_/qs36t#T#`<init>`().
         |[2:6..2:10): qs36 <= _empty_/qs36#
         |[2:11..2:11):  <= _empty_/qs36#`<init>`().
         |[2:19..2:24): qs36a => _empty_/qs36a#
         |[2:24..2:24):  => _empty_/qs36a#`<init>`().
         |[2:30..2:35): qs36t => _empty_/qs36t#
         |[3:6..3:8): a1 <= _empty_/qs36#a1.
         |[3:11..3:18): classOf => scala/Predef.classOf().
         |[3:25..3:26): T => _empty_/qs36t#T#
         |[4:6..4:8): b1 <= _empty_/qs36#b1.
         |[4:11..4:15): List => scala/collection/immutable/Nil.
         |[4:22..4:23): T => _empty_/qs36t#T#
         |[5:6..5:8): c1 <= _empty_/qs36#c1.
         |[5:11..5:18): classOf => scala/Predef.classOf().
         |[5:19..5:23): List => scala/package.List#
         |[5:24..5:25): X => local0
         |[5:42..5:43): X <= local0
         |[5:53..5:54): T => _empty_/qs36t#T#
         |[6:6..6:8): e1 <= _empty_/qs36#e1.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:26):  => _empty_/qs36#
         |[6:31..6:31):  => _empty_/qs36t#
         |[6:32..6:33): T => _empty_/qs36t#T#
         |[6:35..6:38): Int => scala/Int#
         |[6:42..6:81): (x: super.T) => x match { case _ => 1 } <= local1
         |[6:43..6:44): x <= local8
         |[6:52..6:53): T => _empty_/qs36t#T#
         |[6:58..6:59): x => local8
         |[7:6..7:8): a2 <= _empty_/qs36#a2.
         |[7:11..7:18): classOf => scala/Predef.classOf().
         |[7:30..7:31): T => _empty_/qs36t#T#
         |[8:6..8:8): a3 <= _empty_/qs36#a3.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:37..8:38): T => _empty_/qs36t#T#
         |[9:6..9:8): e3 <= _empty_/qs36#e3.
         |[9:10..9:25): PartialFunction => scala/PartialFunction#
         |[9:26..9:30): qs36 => _empty_/qs36#
         |[9:37..9:42): qs36t => _empty_/qs36t#
         |[9:44..9:45): T => _empty_/qs36t#T#
         |[9:47..9:50): Int => scala/Int#
         |[9:54..9:105): (x: qs36.super[qs36t].T) => x match { case _ => 1 } <= local9
         |[9:55..9:56): x <= local16
         |[9:76..9:77): T => _empty_/qs36t#T#
         |[9:82..9:83): x => local16
         |""".stripMargin
    else
      """|[0:6..0:11): qs36a <= _empty_/qs36a#
         |[0:11..0:11):  <= _empty_/qs36a#`<init>`().
         |[1:6..1:11): qs36t <= _empty_/qs36t#
         |[1:12..1:12):  <= _empty_/qs36t#`<init>`().
         |[1:20..1:21): T <= _empty_/qs36t#T#
         |[1:21..1:21):  <= _empty_/qs36t#T#`<init>`().
         |[2:6..2:10): qs36 <= _empty_/qs36#
         |[2:11..2:11):  <= _empty_/qs36#`<init>`().
         |[2:19..2:24): qs36a => _empty_/qs36a#
         |[2:24..2:24):  => _empty_/qs36a#`<init>`().
         |[2:30..2:35): qs36t => _empty_/qs36t#
         |[3:6..3:8): a1 <= _empty_/qs36#a1.
         |[3:11..3:18): classOf => scala/Predef.classOf().
         |[3:25..3:26): T => _empty_/qs36t#T#
         |[4:6..4:8): b1 <= _empty_/qs36#b1.
         |[4:11..4:15): List => scala/collection/immutable/Nil.
         |[4:22..4:23): T => _empty_/qs36t#T#
         |[5:6..5:8): c1 <= _empty_/qs36#c1.
         |[5:11..5:18): classOf => scala/Predef.classOf().
         |[5:19..5:23): List => scala/package.List#
         |[5:24..5:25): X => local0
         |[5:42..5:43): X <= local0
         |[5:53..5:54): T => _empty_/qs36t#T#
         |[6:6..6:8): e1 <= _empty_/qs36#e1.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:26):  => _empty_/qs36#
         |[6:31..6:31):  => _empty_/qs36t#
         |[6:32..6:33): T => _empty_/qs36t#T#
         |[6:35..6:38): Int => scala/Int#
         |[6:42..6:81): (x: super.T) => x match { case _ => 1 } <= local1
         |[6:43..6:44): x <= local8
         |[6:58..6:59): x => local8
         |[7:6..7:8): a2 <= _empty_/qs36#a2.
         |[7:11..7:18): classOf => scala/Predef.classOf().
         |[7:30..7:31): T => _empty_/qs36t#T#
         |[8:6..8:8): a3 <= _empty_/qs36#a3.
         |[8:11..8:18): classOf => scala/Predef.classOf().
         |[8:37..8:38): T => _empty_/qs36t#T#
         |[9:6..9:8): e3 <= _empty_/qs36#e3.
         |[9:10..9:25): PartialFunction => scala/PartialFunction#
         |[9:26..9:30): qs36 => _empty_/qs36#
         |[9:37..9:42): qs36t => _empty_/qs36t#
         |[9:44..9:45): T => _empty_/qs36t#T#
         |[9:47..9:50): Int => scala/Int#
         |[9:54..9:105): (x: qs36.super[qs36t].T) => x match { case _ => 1 } <= local9
         |[9:55..9:56): x <= local16
         |[9:82..9:83): x => local16
         |""".stripMargin,
  )

  occurrences(
    "a final class through super",
    """|class qs49a { final class T }
       |class qs49 extends qs49a {
       |  val a1 = classOf[super.T]
       |  val b1 = List[super.T]()
       |  val c1 = classOf[List[X] forSome { type X <: super.T }]
       |  val e1: PartialFunction[super.T, Int] = (x: super.T) => x match { case _ => 1 }
       |  val a2 = classOf[qs49.super.T]
       |  val a3 = classOf[qs49.super[qs49a].T]
       |  val e3: PartialFunction[qs49.super[qs49a].T, Int] = (x: qs49.super[qs49a].T) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:11): qs49a <= _empty_/qs49a#
         |[0:12..0:12):  <= _empty_/qs49a#`<init>`().
         |[0:26..0:27): T <= _empty_/qs49a#T#
         |[0:27..0:27):  <= _empty_/qs49a#T#`<init>`().
         |[1:6..1:10): qs49 <= _empty_/qs49#
         |[1:11..1:11):  <= _empty_/qs49#`<init>`().
         |[1:19..1:24): qs49a => _empty_/qs49a#
         |[1:24..1:24):  => _empty_/qs49a#`<init>`().
         |[2:6..2:8): a1 <= _empty_/qs49#a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:25..2:26): T => _empty_/qs49a#T#
         |[3:6..3:8): b1 <= _empty_/qs49#b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:22..3:23): T => _empty_/qs49a#T#
         |[4:6..4:8): c1 <= _empty_/qs49#c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:53..4:54): T => _empty_/qs49a#T#
         |[5:6..5:8): e1 <= _empty_/qs49#e1.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:26):  => _empty_/qs49#
         |[5:31..5:31):  => _empty_/qs49a#
         |[5:32..5:33): T => _empty_/qs49a#T#
         |[5:35..5:38): Int => scala/Int#
         |[5:42..5:81): (x: super.T) => x match { case _ => 1 } <= local1
         |[5:43..5:44): x <= local8
         |[5:52..5:53): T => _empty_/qs49a#T#
         |[5:58..5:59): x => local8
         |[6:6..6:8): a2 <= _empty_/qs49#a2.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:30..6:31): T => _empty_/qs49a#T#
         |[7:6..7:8): a3 <= _empty_/qs49#a3.
         |[7:11..7:18): classOf => scala/Predef.classOf().
         |[7:37..7:38): T => _empty_/qs49a#T#
         |[8:6..8:8): e3 <= _empty_/qs49#e3.
         |[8:10..8:25): PartialFunction => scala/PartialFunction#
         |[8:26..8:30): qs49 => _empty_/qs49#
         |[8:37..8:42): qs49a => _empty_/qs49a#
         |[8:44..8:45): T => _empty_/qs49a#T#
         |[8:47..8:50): Int => scala/Int#
         |[8:54..8:105): (x: qs49.super[qs49a].T) => x match { case _ => 1 } <= local9
         |[8:55..8:56): x <= local16
         |[8:76..8:77): T => _empty_/qs49a#T#
         |[8:82..8:83): x => local16
         |""".stripMargin
    else
      """|[0:6..0:11): qs49a <= _empty_/qs49a#
         |[0:12..0:12):  <= _empty_/qs49a#`<init>`().
         |[0:26..0:27): T <= _empty_/qs49a#T#
         |[0:27..0:27):  <= _empty_/qs49a#T#`<init>`().
         |[1:6..1:10): qs49 <= _empty_/qs49#
         |[1:11..1:11):  <= _empty_/qs49#`<init>`().
         |[1:19..1:24): qs49a => _empty_/qs49a#
         |[1:24..1:24):  => _empty_/qs49a#`<init>`().
         |[2:6..2:8): a1 <= _empty_/qs49#a1.
         |[2:11..2:18): classOf => scala/Predef.classOf().
         |[2:25..2:26): T => _empty_/qs49a#T#
         |[3:6..3:8): b1 <= _empty_/qs49#b1.
         |[3:11..3:15): List => scala/collection/immutable/Nil.
         |[3:22..3:23): T => _empty_/qs49a#T#
         |[4:6..4:8): c1 <= _empty_/qs49#c1.
         |[4:11..4:18): classOf => scala/Predef.classOf().
         |[4:19..4:23): List => scala/package.List#
         |[4:24..4:25): X => local0
         |[4:42..4:43): X <= local0
         |[4:53..4:54): T => _empty_/qs49a#T#
         |[5:6..5:8): e1 <= _empty_/qs49#e1.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:26):  => _empty_/qs49#
         |[5:31..5:31):  => _empty_/qs49a#
         |[5:32..5:33): T => _empty_/qs49a#T#
         |[5:35..5:38): Int => scala/Int#
         |[5:42..5:81): (x: super.T) => x match { case _ => 1 } <= local1
         |[5:43..5:44): x <= local8
         |[5:58..5:59): x => local8
         |[6:6..6:8): a2 <= _empty_/qs49#a2.
         |[6:11..6:18): classOf => scala/Predef.classOf().
         |[6:30..6:31): T => _empty_/qs49a#T#
         |[7:6..7:8): a3 <= _empty_/qs49#a3.
         |[7:11..7:18): classOf => scala/Predef.classOf().
         |[7:37..7:38): T => _empty_/qs49a#T#
         |[8:6..8:8): e3 <= _empty_/qs49#e3.
         |[8:10..8:25): PartialFunction => scala/PartialFunction#
         |[8:26..8:30): qs49 => _empty_/qs49#
         |[8:37..8:42): qs49a => _empty_/qs49a#
         |[8:44..8:45): T => _empty_/qs49a#T#
         |[8:47..8:50): Int => scala/Int#
         |[8:54..8:105): (x: qs49.super[qs49a].T) => x match { case _ => 1 } <= local9
         |[8:55..8:56): x <= local16
         |[8:82..8:83): x => local16
         |""".stripMargin,
  )

  occurrences(
    "an alias of a member of an outer class through super, this and a self alias",
    """|class qs62o {
       |  class U; trait TCU[B]; class P[B](b: B)
       |  trait A { type T = qs62o.this.U; type TC[B] = qs62o.this.TCU[B]; type PA[B] = qs62o.this.P[B] }
       |  class K extends A { s =>
       |    val a = classOf[super.T]
       |    val b = List[super.T]()
       |    val c = classOf[List[X] forSome { type X <: super.T }]
       |    val d: List[X] forSome { type X <: super.T } = null
       |    val e: PartialFunction[super.T, Int] = (x: super.T) => x match { case _ => 1 }
       |    val f1 = List[AnyRef { def m: K.super.T }]()
       |    val f2 = List[AnyRef { def m: K.super[A].T }]()
       |    val f3 = List[AnyRef { def m: K.this.T }]()
       |    val f4 = List[AnyRef { def m: s.T }]()
       |    val g1 = classOf[this.T]
       |    val g2 = List[this.T]()
       |    val h = List[AnyRef { def m[B: K.super.TC]: Int }]()
       |    class I { self: K.super.T => }
       |    class J extends super.PA(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:11): qs62o <= _empty_/qs62o#
         |[0:12..0:12):  <= _empty_/qs62o#`<init>`().
         |[1:8..1:9): U <= _empty_/qs62o#U#
         |[1:9..1:9):  <= _empty_/qs62o#U#`<init>`().
         |[1:17..1:20): TCU <= _empty_/qs62o#TCU#
         |[1:21..1:22): B <= _empty_/qs62o#TCU#[B]
         |[1:31..1:32): P <= _empty_/qs62o#P#
         |[1:33..1:34): B <= _empty_/qs62o#P#[B]
         |[1:35..1:35):  <= _empty_/qs62o#P#`<init>`().
         |[1:36..1:37): b <= _empty_/qs62o#P#`<init>`().(b)
         |[1:36..1:37): b <= _empty_/qs62o#P#b.
         |[1:39..1:40): B => _empty_/qs62o#P#[B]
         |[2:8..2:9): A <= _empty_/qs62o#A#
         |[2:17..2:18): T <= _empty_/qs62o#A#T#
         |[2:21..2:26): qs62o => _empty_/qs62o#
         |[2:32..2:33): U => _empty_/qs62o#U#
         |[2:40..2:42): TC <= _empty_/qs62o#A#TC#
         |[2:43..2:44): B <= _empty_/qs62o#A#TC#[B]
         |[2:48..2:53): qs62o => _empty_/qs62o#
         |[2:59..2:62): TCU => _empty_/qs62o#TCU#
         |[2:63..2:64): B => _empty_/qs62o#A#TC#[B]
         |[2:72..2:74): PA <= _empty_/qs62o#A#PA#
         |[2:75..2:76): B <= _empty_/qs62o#A#PA#[B]
         |[2:80..2:85): qs62o => _empty_/qs62o#
         |[2:91..2:92): P => _empty_/qs62o#P#
         |[2:93..2:94): B => _empty_/qs62o#A#PA#[B]
         |[3:8..3:9): K <= _empty_/qs62o#K#
         |[3:10..3:10):  <= _empty_/qs62o#K#`<init>`().
         |[3:18..3:19): A => _empty_/qs62o#A#
         |[3:19..3:19):  => java/lang/Object#`<init>`().
         |[3:22..3:23): s <= local0
         |[4:8..4:9): a <= _empty_/qs62o#K#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:26..4:27): T => _empty_/qs62o#A#T#
         |[5:8..5:9): b <= _empty_/qs62o#K#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:23..5:24): T => _empty_/qs62o#U#
         |[6:8..6:9): c <= _empty_/qs62o#K#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local1
         |[6:43..6:44): X <= local1
         |[6:54..6:55): T => _empty_/qs62o#A#T#
         |[7:8..7:9): d <= _empty_/qs62o#K#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local2
         |[7:34..7:35): X <= local2
         |[7:39..7:39):  => _empty_/qs62o#K#
         |[7:44..7:44):  => _empty_/qs62o#A#
         |[7:45..7:46): T => _empty_/qs62o#A#T#
         |[8:8..8:9): e <= _empty_/qs62o#K#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:27):  => _empty_/qs62o#K#
         |[8:32..8:32):  => _empty_/qs62o#A#
         |[8:33..8:34): T => _empty_/qs62o#A#T#
         |[8:36..8:39): Int => scala/Int#
         |[8:43..8:82): (x: super.T) => x match { case _ => 1 } <= local3
         |[8:44..8:45): x <= local10
         |[8:53..8:54): T => _empty_/qs62o#A#T#
         |[8:59..8:60): x => local10
         |[9:8..9:10): f1 <= _empty_/qs62o#K#f1.
         |[9:13..9:17): List => scala/collection/immutable/Nil.
         |[9:18..9:24): AnyRef => java/lang/Object#
         |[9:31..9:32): m <= local11
         |[9:42..9:43): T => _empty_/qs62o#U#
         |[10:8..10:10): f2 <= _empty_/qs62o#K#f2.
         |[10:13..10:17): List => scala/collection/immutable/Nil.
         |[10:18..10:24): AnyRef => java/lang/Object#
         |[10:31..10:32): m <= local12
         |[10:45..10:46): T => _empty_/qs62o#U#
         |[11:8..11:10): f3 <= _empty_/qs62o#K#f3.
         |[11:13..11:17): List => scala/collection/immutable/Nil.
         |[11:18..11:24): AnyRef => java/lang/Object#
         |[11:31..11:32): m <= local13
         |[11:41..11:42): T => _empty_/qs62o#U#
         |[12:8..12:10): f4 <= _empty_/qs62o#K#f4.
         |[12:13..12:17): List => scala/collection/immutable/Nil.
         |[12:18..12:24): AnyRef => java/lang/Object#
         |[12:31..12:32): m <= local14
         |[12:36..12:37): T => _empty_/qs62o#U#
         |[13:8..13:10): g1 <= _empty_/qs62o#K#g1.
         |[13:13..13:20): classOf => scala/Predef.classOf().
         |[13:26..13:27): T => _empty_/qs62o#A#T#
         |[14:8..14:10): g2 <= _empty_/qs62o#K#g2.
         |[14:13..14:17): List => scala/collection/immutable/Nil.
         |[14:23..14:24): T => _empty_/qs62o#U#
         |[15:8..15:9): h <= _empty_/qs62o#K#h.
         |[15:12..15:16): List => scala/collection/immutable/Nil.
         |[15:17..15:23): AnyRef => java/lang/Object#
         |[15:30..15:31): m <= local15
         |[15:32..15:33): B <= local17
         |[15:35..15:35):  <= local16
         |[15:43..15:45): TC => _empty_/qs62o#TCU#
         |[15:48..15:51): Int => scala/Int#
         |[16:10..16:11): I <= _empty_/qs62o#K#I#
         |[16:12..16:12):  <= _empty_/qs62o#K#I#`<init>`().
         |[16:14..16:18): self <= local18
         |[16:20..16:21): K => _empty_/qs62o#K#
         |[16:27..16:27):  => _empty_/qs62o#A#
         |[16:28..16:29): T => _empty_/qs62o#A#T#
         |[17:10..17:11): J <= _empty_/qs62o#K#J#
         |[17:12..17:12):  <= _empty_/qs62o#K#J#`<init>`().
         |[17:26..17:28): PA => _empty_/qs62o#A#PA#
         |[17:28..17:28):  => _empty_/qs62o#P#`<init>`().
         |""".stripMargin
    else
      """|[0:6..0:11): qs62o <= _empty_/qs62o#
         |[0:12..0:12):  <= _empty_/qs62o#`<init>`().
         |[1:8..1:9): U <= _empty_/qs62o#U#
         |[1:9..1:9):  <= _empty_/qs62o#U#`<init>`().
         |[1:17..1:20): TCU <= _empty_/qs62o#TCU#
         |[1:21..1:22): B <= _empty_/qs62o#TCU#[B]
         |[1:31..1:32): P <= _empty_/qs62o#P#
         |[1:33..1:34): B <= _empty_/qs62o#P#[B]
         |[1:35..1:35):  <= _empty_/qs62o#P#`<init>`().
         |[1:36..1:37): b <= _empty_/qs62o#P#`<init>`().(b)
         |[1:36..1:37): b <= _empty_/qs62o#P#b.
         |[1:39..1:40): B => _empty_/qs62o#P#[B]
         |[2:8..2:9): A <= _empty_/qs62o#A#
         |[2:17..2:18): T <= _empty_/qs62o#A#T#
         |[2:21..2:26): qs62o => _empty_/qs62o#
         |[2:32..2:33): U => _empty_/qs62o#U#
         |[2:40..2:42): TC <= _empty_/qs62o#A#TC#
         |[2:43..2:44): B <= _empty_/qs62o#A#TC#[B]
         |[2:48..2:53): qs62o => _empty_/qs62o#
         |[2:59..2:62): TCU => _empty_/qs62o#TCU#
         |[2:63..2:64): B => _empty_/qs62o#A#TC#[B]
         |[2:72..2:74): PA <= _empty_/qs62o#A#PA#
         |[2:75..2:76): B <= _empty_/qs62o#A#PA#[B]
         |[2:80..2:85): qs62o => _empty_/qs62o#
         |[2:91..2:92): P => _empty_/qs62o#P#
         |[2:93..2:94): B => _empty_/qs62o#A#PA#[B]
         |[3:8..3:9): K <= _empty_/qs62o#K#
         |[3:10..3:10):  <= _empty_/qs62o#K#`<init>`().
         |[3:18..3:19): A => _empty_/qs62o#A#
         |[3:19..3:19):  => java/lang/Object#`<init>`().
         |[3:22..3:23): s <= local0
         |[4:8..4:9): a <= _empty_/qs62o#K#a.
         |[4:12..4:19): classOf => scala/Predef.classOf().
         |[4:26..4:27): T => _empty_/qs62o#A#T#
         |[5:8..5:9): b <= _empty_/qs62o#K#b.
         |[5:12..5:16): List => scala/collection/immutable/Nil.
         |[5:23..5:24): T => _empty_/qs62o#U#
         |[6:8..6:9): c <= _empty_/qs62o#K#c.
         |[6:12..6:19): classOf => scala/Predef.classOf().
         |[6:20..6:24): List => scala/package.List#
         |[6:25..6:26): X => local1
         |[6:43..6:44): X <= local1
         |[6:54..6:55): T => _empty_/qs62o#A#T#
         |[7:8..7:9): d <= _empty_/qs62o#K#d.
         |[7:11..7:15): List => scala/package.List#
         |[7:16..7:17): X => local2
         |[7:34..7:35): X <= local2
         |[7:39..7:39):  => _empty_/qs62o#K#
         |[7:44..7:44):  => _empty_/qs62o#A#
         |[7:45..7:46): T => _empty_/qs62o#A#T#
         |[8:8..8:9): e <= _empty_/qs62o#K#e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:27):  => _empty_/qs62o#K#
         |[8:32..8:32):  => _empty_/qs62o#A#
         |[8:33..8:34): T => _empty_/qs62o#A#T#
         |[8:36..8:39): Int => scala/Int#
         |[8:43..8:82): (x: super.T) => x match { case _ => 1 } <= local3
         |[8:44..8:45): x <= local10
         |[8:59..8:60): x => local10
         |[9:8..9:10): f1 <= _empty_/qs62o#K#f1.
         |[9:13..9:17): List => scala/collection/immutable/Nil.
         |[9:18..9:24): AnyRef => java/lang/Object#
         |[9:31..9:32): m <= local11
         |[9:42..9:43): T => _empty_/qs62o#U#
         |[10:8..10:10): f2 <= _empty_/qs62o#K#f2.
         |[10:13..10:17): List => scala/collection/immutable/Nil.
         |[10:18..10:24): AnyRef => java/lang/Object#
         |[10:31..10:32): m <= local12
         |[10:45..10:46): T => _empty_/qs62o#U#
         |[11:8..11:10): f3 <= _empty_/qs62o#K#f3.
         |[11:13..11:17): List => scala/collection/immutable/Nil.
         |[11:18..11:24): AnyRef => java/lang/Object#
         |[11:31..11:32): m <= local13
         |[11:41..11:42): T => _empty_/qs62o#U#
         |[12:8..12:10): f4 <= _empty_/qs62o#K#f4.
         |[12:13..12:17): List => scala/collection/immutable/Nil.
         |[12:18..12:24): AnyRef => java/lang/Object#
         |[12:31..12:32): m <= local14
         |[12:36..12:37): T => _empty_/qs62o#U#
         |[13:8..13:10): g1 <= _empty_/qs62o#K#g1.
         |[13:13..13:20): classOf => scala/Predef.classOf().
         |[13:26..13:27): T => _empty_/qs62o#A#T#
         |[14:8..14:10): g2 <= _empty_/qs62o#K#g2.
         |[14:13..14:17): List => scala/collection/immutable/Nil.
         |[14:23..14:24): T => _empty_/qs62o#U#
         |[15:8..15:9): h <= _empty_/qs62o#K#h.
         |[15:12..15:16): List => scala/collection/immutable/Nil.
         |[15:17..15:23): AnyRef => java/lang/Object#
         |[15:30..15:31): m <= local15
         |[15:32..15:33): B <= local17
         |[15:35..15:35):  <= local16
         |[15:43..15:45): TC => _empty_/qs62o#TCU#
         |[15:48..15:51): Int => scala/Int#
         |[16:10..16:11): I <= _empty_/qs62o#K#I#
         |[16:12..16:12):  <= _empty_/qs62o#K#I#`<init>`().
         |[16:14..16:18): self <= local18
         |[16:20..16:21): K => _empty_/qs62o#K#
         |[16:27..16:27):  => _empty_/qs62o#A#
         |[16:28..16:29): T => _empty_/qs62o#A#T#
         |[17:10..17:11): J <= _empty_/qs62o#K#J#
         |[17:12..17:12):  <= _empty_/qs62o#K#J#`<init>`().
         |[17:26..17:28): PA => _empty_/qs62o#A#PA#
         |[17:28..17:28):  => _empty_/qs62o#P#`<init>`().
         |""".stripMargin,
  )

  occurrences(
    "a type through this in constructor defaults and parent arguments",
    """|class qs66p { class T }
       |class qs66q(z: Any*) { class T }
       |class qs66 extends qs66p {
       |  class K(
       |      a: Any = classOf[this.T],
       |      b: Any = List[this.T](),
       |      c: Any = classOf[List[X] forSome { type X <: this.T }],
       |      d: Any = null: List[X] forSome { type X <: this.T },
       |      e: Any = ((x: this.T) => x match { case _ => 1 }): PartialFunction[this.T, Int],
       |      g: Any = List[this.type](),
       |  ) extends qs66q
       |  class J extends qs66q {
       |    def this(
       |        i: Int,
       |        a: Any = classOf[this.T],
       |        b: Any = List[this.T](),
       |        c: Any = classOf[List[X] forSome { type X <: this.T }],
       |        d: Any = null: List[X] forSome { type X <: this.T },
       |        e: Any = ((x: this.T) => x match { case _ => 1 }): PartialFunction[this.T, Int],
       |        g: Any = List[this.type](),
       |    ) = this()
       |  }
       |  class L
       |      extends qs66q(
       |        classOf[this.T],
       |        List[this.T](),
       |        classOf[List[X] forSome { type X <: this.T }],
       |        null: List[X] forSome { type X <: this.T },
       |        ((x: this.T) => x match { case _ => 1 }): PartialFunction[this.T, Int],
       |        List[this.type](),
       |      )
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:11): qs66p <= _empty_/qs66p#
         |[0:12..0:12):  <= _empty_/qs66p#`<init>`().
         |[0:20..0:21): T <= _empty_/qs66p#T#
         |[0:21..0:21):  <= _empty_/qs66p#T#`<init>`().
         |[1:6..1:11): qs66q <= _empty_/qs66q#
         |[1:11..1:11):  <= _empty_/qs66q#`<init>`().
         |[1:12..1:13): z <= _empty_/qs66q#`<init>`().(z)
         |[1:12..1:13): z <= _empty_/qs66q#z.
         |[1:15..1:18): Any => scala/Any#
         |[1:29..1:30): T <= _empty_/qs66q#T#
         |[1:30..1:30):  <= _empty_/qs66q#T#`<init>`().
         |[2:6..2:10): qs66 <= _empty_/qs66#
         |[2:11..2:11):  <= _empty_/qs66#`<init>`().
         |[2:19..2:24): qs66p => _empty_/qs66p#
         |[2:24..2:24):  => _empty_/qs66p#`<init>`().
         |[3:8..3:9): K <= _empty_/qs66#K#
         |[3:9..3:9):  <= _empty_/qs66#K#`<init>`().
         |[4:6..4:7): a <= _empty_/qs66#K#`<init>`().(a)
         |[4:6..4:7): a <= _empty_/qs66#K#a.
         |[4:9..4:12): Any => scala/Any#
         |[4:15..4:22): classOf => scala/Predef.classOf().
         |[4:28..4:29): T => _empty_/qs66p#T#
         |[5:6..5:7): b <= _empty_/qs66#K#`<init>`().(b)
         |[5:6..5:7): b <= _empty_/qs66#K#b.
         |[5:9..5:12): Any => scala/Any#
         |[5:15..5:19): List => scala/collection/immutable/Nil.
         |[5:25..5:26): T => _empty_/qs66p#T#
         |[6:6..6:7): c <= _empty_/qs66#K#`<init>`().(c)
         |[6:6..6:7): c <= _empty_/qs66#K#c.
         |[6:9..6:12): Any => scala/Any#
         |[6:15..6:22): classOf => scala/Predef.classOf().
         |[6:23..6:27): List => scala/package.List#
         |[6:28..6:29): X => local0
         |[6:46..6:47): X <= local0
         |[6:56..6:57): T => _empty_/qs66p#T#
         |[7:6..7:7): d <= _empty_/qs66#K#`<init>`().(d)
         |[7:6..7:7): d <= _empty_/qs66#K#d.
         |[7:9..7:12): Any => scala/Any#
         |[7:21..7:25): List => scala/package.List#
         |[7:26..7:27): X => local1
         |[7:44..7:45): X <= local1
         |[7:54..7:55): T => _empty_/qs66p#T#
         |[8:6..8:7): e <= _empty_/qs66#K#`<init>`().(e)
         |[8:6..8:7): e <= _empty_/qs66#K#e.
         |[8:9..8:12): Any => scala/Any#
         |[8:16..8:54): (x: this.T) => x match { case _ => 1 } <= local10
         |[8:17..8:18): x <= local9
         |[8:25..8:26): T => _empty_/qs66p#T#
         |[8:31..8:32): x => local9
         |[8:57..8:72): PartialFunction => scala/PartialFunction#
         |[8:78..8:79): T => _empty_/qs66p#T#
         |[8:81..8:84): Int => scala/Int#
         |[9:6..9:7): g <= _empty_/qs66#K#`<init>`().(g)
         |[9:6..9:7): g <= _empty_/qs66#K#g.
         |[9:9..9:12): Any => scala/Any#
         |[9:15..9:19): List => scala/collection/immutable/Nil.
         |[10:12..10:17): qs66q => _empty_/qs66q#
         |[10:17..10:17):  => _empty_/qs66q#`<init>`().
         |[11:8..11:9): J <= _empty_/qs66#J#
         |[11:10..11:10):  <= _empty_/qs66#J#`<init>`().
         |[11:18..11:23): qs66q => _empty_/qs66q#
         |[11:23..11:23):  => _empty_/qs66q#`<init>`().
         |[12:8..12:12): this <= _empty_/qs66#J#`<init>`(+1).
         |[13:8..13:9): i <= _empty_/qs66#J#`<init>`(+1).(i)
         |[13:11..13:14): Int => scala/Int#
         |[14:8..14:9): a <= _empty_/qs66#J#`<init>`(+1).(a)
         |[14:11..14:14): Any => scala/Any#
         |[14:17..14:24): classOf => scala/Predef.classOf().
         |[14:30..14:31): T => _empty_/qs66p#T#
         |[15:8..15:9): b <= _empty_/qs66#J#`<init>`(+1).(b)
         |[15:11..15:14): Any => scala/Any#
         |[15:17..15:21): List => scala/collection/immutable/Nil.
         |[15:27..15:28): T => _empty_/qs66p#T#
         |[16:8..16:9): c <= _empty_/qs66#J#`<init>`(+1).(c)
         |[16:11..16:14): Any => scala/Any#
         |[16:17..16:24): classOf => scala/Predef.classOf().
         |[16:25..16:29): List => scala/package.List#
         |[16:30..16:31): X => local18
         |[16:48..16:49): X <= local18
         |[16:58..16:59): T => _empty_/qs66p#T#
         |[17:8..17:9): d <= _empty_/qs66#J#`<init>`(+1).(d)
         |[17:11..17:14): Any => scala/Any#
         |[17:23..17:27): List => scala/package.List#
         |[17:28..17:29): X => local19
         |[17:46..17:47): X <= local19
         |[17:56..17:57): T => _empty_/qs66p#T#
         |[18:8..18:9): e <= _empty_/qs66#J#`<init>`(+1).(e)
         |[18:11..18:14): Any => scala/Any#
         |[18:18..18:56): (x: this.T) => x match { case _ => 1 } <= local28
         |[18:19..18:20): x <= local27
         |[18:27..18:28): T => _empty_/qs66p#T#
         |[18:33..18:34): x => local27
         |[18:59..18:74): PartialFunction => scala/PartialFunction#
         |[18:80..18:81): T => _empty_/qs66p#T#
         |[18:83..18:86): Int => scala/Int#
         |[19:8..19:9): g <= _empty_/qs66#J#`<init>`(+1).(g)
         |[19:11..19:14): Any => scala/Any#
         |[19:17..19:21): List => scala/collection/immutable/Nil.
         |[20:12..20:12):  => _empty_/qs66#J#`<init>`().
         |[22:8..22:9): L <= _empty_/qs66#L#
         |[23:6..23:6):  <= _empty_/qs66#L#`<init>`().
         |[23:14..23:19): qs66q => _empty_/qs66q#
         |[23:19..23:19):  => _empty_/qs66q#`<init>`().
         |[24:8..24:15): classOf => scala/Predef.classOf().
         |[24:21..24:22): T => _empty_/qs66p#T#
         |[25:8..25:12): List => scala/collection/immutable/Nil.
         |[25:18..25:19): T => _empty_/qs66p#T#
         |[26:8..26:15): classOf => scala/Predef.classOf().
         |[26:16..26:20): List => scala/package.List#
         |[26:21..26:22): X => local36
         |[26:39..26:40): X <= local36
         |[26:49..26:50): T => _empty_/qs66p#T#
         |[27:14..27:18): List => scala/package.List#
         |[27:19..27:20): X => local37
         |[27:37..27:38): X <= local37
         |[27:47..27:48): T => _empty_/qs66p#T#
         |[28:9..28:47): (x: this.T) => x match { case _ => 1 } <= local38
         |[28:10..28:11): x <= local45
         |[28:18..28:19): T => _empty_/qs66p#T#
         |[28:24..28:25): x => local45
         |[28:50..28:65): PartialFunction => scala/PartialFunction#
         |[28:71..28:72): T => _empty_/qs66p#T#
         |[28:74..28:77): Int => scala/Int#
         |[29:8..29:12): List => scala/collection/immutable/Nil.
         |""".stripMargin
    else
      """|[0:6..0:11): qs66p <= _empty_/qs66p#
         |[0:12..0:12):  <= _empty_/qs66p#`<init>`().
         |[0:20..0:21): T <= _empty_/qs66p#T#
         |[0:21..0:21):  <= _empty_/qs66p#T#`<init>`().
         |[1:6..1:11): qs66q <= _empty_/qs66q#
         |[1:11..1:11):  <= _empty_/qs66q#`<init>`().
         |[1:12..1:13): z <= _empty_/qs66q#`<init>`().(z)
         |[1:12..1:13): z <= _empty_/qs66q#z.
         |[1:15..1:18): Any => scala/Any#
         |[1:29..1:30): T <= _empty_/qs66q#T#
         |[1:30..1:30):  <= _empty_/qs66q#T#`<init>`().
         |[2:6..2:10): qs66 <= _empty_/qs66#
         |[2:11..2:11):  <= _empty_/qs66#`<init>`().
         |[2:19..2:24): qs66p => _empty_/qs66p#
         |[2:24..2:24):  => _empty_/qs66p#`<init>`().
         |[3:8..3:9): K <= _empty_/qs66#K#
         |[3:9..3:9):  <= _empty_/qs66#K#`<init>`().
         |[4:6..4:7): a <= _empty_/qs66#K#`<init>`().(a)
         |[4:6..4:7): a <= _empty_/qs66#K#a.
         |[4:9..4:12): Any => scala/Any#
         |[4:15..4:22): classOf => scala/Predef.classOf().
         |[4:28..4:29): T => _empty_/qs66p#T#
         |[5:6..5:7): b <= _empty_/qs66#K#`<init>`().(b)
         |[5:6..5:7): b <= _empty_/qs66#K#b.
         |[5:9..5:12): Any => scala/Any#
         |[5:15..5:19): List => scala/collection/immutable/Nil.
         |[5:25..5:26): T => _empty_/qs66p#T#
         |[6:6..6:7): c <= _empty_/qs66#K#`<init>`().(c)
         |[6:6..6:7): c <= _empty_/qs66#K#c.
         |[6:9..6:12): Any => scala/Any#
         |[6:15..6:22): classOf => scala/Predef.classOf().
         |[6:23..6:27): List => scala/package.List#
         |[6:28..6:29): X => local0
         |[6:46..6:47): X <= local0
         |[6:56..6:57): T => _empty_/qs66p#T#
         |[7:6..7:7): d <= _empty_/qs66#K#`<init>`().(d)
         |[7:6..7:7): d <= _empty_/qs66#K#d.
         |[7:9..7:12): Any => scala/Any#
         |[7:21..7:25): List => scala/package.List#
         |[7:26..7:27): X => local1
         |[7:44..7:45): X <= local1
         |[7:54..7:55): T => _empty_/qs66p#T#
         |[8:6..8:7): e <= _empty_/qs66#K#`<init>`().(e)
         |[8:6..8:7): e <= _empty_/qs66#K#e.
         |[8:9..8:12): Any => scala/Any#
         |[8:16..8:54): (x: this.T) => x match { case _ => 1 } <= local10
         |[8:17..8:18): x <= local9
         |[8:31..8:32): x => local9
         |[8:57..8:72): PartialFunction => scala/PartialFunction#
         |[8:78..8:79): T => _empty_/qs66p#T#
         |[8:81..8:84): Int => scala/Int#
         |[9:6..9:7): g <= _empty_/qs66#K#`<init>`().(g)
         |[9:6..9:7): g <= _empty_/qs66#K#g.
         |[9:9..9:12): Any => scala/Any#
         |[9:15..9:19): List => scala/collection/immutable/Nil.
         |[10:12..10:17): qs66q => _empty_/qs66q#
         |[10:17..10:17):  => _empty_/qs66q#`<init>`().
         |[11:8..11:9): J <= _empty_/qs66#J#
         |[11:10..11:10):  <= _empty_/qs66#J#`<init>`().
         |[11:18..11:23): qs66q => _empty_/qs66q#
         |[11:23..11:23):  => _empty_/qs66q#`<init>`().
         |[12:8..12:12): this <= _empty_/qs66#J#`<init>`(+1).
         |[13:8..13:9): i <= _empty_/qs66#J#`<init>`(+1).(i)
         |[13:11..13:14): Int => scala/Int#
         |[14:8..14:9): a <= _empty_/qs66#J#`<init>`(+1).(a)
         |[14:11..14:14): Any => scala/Any#
         |[14:17..14:24): classOf => scala/Predef.classOf().
         |[14:30..14:31): T => _empty_/qs66p#T#
         |[15:8..15:9): b <= _empty_/qs66#J#`<init>`(+1).(b)
         |[15:11..15:14): Any => scala/Any#
         |[15:17..15:21): List => scala/collection/immutable/Nil.
         |[15:27..15:28): T => _empty_/qs66p#T#
         |[16:8..16:9): c <= _empty_/qs66#J#`<init>`(+1).(c)
         |[16:11..16:14): Any => scala/Any#
         |[16:17..16:24): classOf => scala/Predef.classOf().
         |[16:25..16:29): List => scala/package.List#
         |[16:30..16:31): X => local18
         |[16:48..16:49): X <= local18
         |[16:58..16:59): T => _empty_/qs66p#T#
         |[17:8..17:9): d <= _empty_/qs66#J#`<init>`(+1).(d)
         |[17:11..17:14): Any => scala/Any#
         |[17:23..17:27): List => scala/package.List#
         |[17:28..17:29): X => local19
         |[17:46..17:47): X <= local19
         |[17:56..17:57): T => _empty_/qs66p#T#
         |[18:8..18:9): e <= _empty_/qs66#J#`<init>`(+1).(e)
         |[18:11..18:14): Any => scala/Any#
         |[18:18..18:56): (x: this.T) => x match { case _ => 1 } <= local28
         |[18:19..18:20): x <= local27
         |[18:33..18:34): x => local27
         |[18:59..18:74): PartialFunction => scala/PartialFunction#
         |[18:80..18:81): T => _empty_/qs66p#T#
         |[18:83..18:86): Int => scala/Int#
         |[19:8..19:9): g <= _empty_/qs66#J#`<init>`(+1).(g)
         |[19:11..19:14): Any => scala/Any#
         |[19:17..19:21): List => scala/collection/immutable/Nil.
         |[20:12..20:12):  => _empty_/qs66#J#`<init>`().
         |[22:8..22:9): L <= _empty_/qs66#L#
         |[23:6..23:6):  <= _empty_/qs66#L#`<init>`().
         |[23:14..23:19): qs66q => _empty_/qs66q#
         |[23:19..23:19):  => _empty_/qs66q#`<init>`().
         |[24:8..24:15): classOf => scala/Predef.classOf().
         |[24:21..24:22): T => _empty_/qs66p#T#
         |[25:8..25:12): List => scala/collection/immutable/Nil.
         |[25:18..25:19): T => _empty_/qs66p#T#
         |[26:8..26:15): classOf => scala/Predef.classOf().
         |[26:16..26:20): List => scala/package.List#
         |[26:21..26:22): X => local36
         |[26:39..26:40): X <= local36
         |[26:49..26:50): T => _empty_/qs66p#T#
         |[27:14..27:18): List => scala/package.List#
         |[27:19..27:20): X => local37
         |[27:37..27:38): X <= local37
         |[27:47..27:48): T => _empty_/qs66p#T#
         |[28:9..28:47): (x: this.T) => x match { case _ => 1 } <= local38
         |[28:10..28:11): x <= local45
         |[28:24..28:25): x => local45
         |[28:50..28:65): PartialFunction => scala/PartialFunction#
         |[28:71..28:72): T => _empty_/qs66p#T#
         |[28:74..28:77): Int => scala/Int#
         |[29:8..29:12): List => scala/collection/immutable/Nil.
         |""".stripMargin,
  )

  occurrences(
    "a type through this of an outer class of the same name in constructor defaults, early definitions, parents and a self type",
    """|class qs77 {
       |  class T; class P(z: Any*); trait Tr; trait TC[A]
       |  class qs77(
       |      a: Any = classOf[qs77.this.T],
       |      b: Any = List[qs77.this.T](),
       |      c: Any = classOf[List[X] forSome { type X <: qs77.this.T }],
       |      d: Any = null: List[X] forSome { type X <: qs77.this.T },
       |      e: Any = ((x: qs77.this.T) => x match { case _ => 1 }): PartialFunction[qs77.this.T, Int],
       |      f: Any = List[AnyRef { def m: qs77.this.T }](),
       |      g: Any = List[qs77.this.type](),
       |      h: Any = List[AnyRef { def m[A: qs77.this.TC]: Int }](),
       |  ) extends {
       |    val a1 = classOf[qs77.this.T]
       |    val b1 = List[qs77.this.T]()
       |    val c1 = classOf[List[X] forSome { type X <: qs77.this.T }]
       |    val d1: List[X] forSome { type X <: qs77.this.T } = null
       |    val e1: PartialFunction[qs77.this.T, Int] = (x: qs77.this.T) => x match { case _ => 1 }
       |    val f1 = List[AnyRef { def m: qs77.this.T }]()
       |    val g1 = List[qs77.this.type]()
       |    val h1 = List[AnyRef { def m[A: qs77.this.TC]: Int }]()
       |  } with qs77.this.P(
       |    classOf[qs77.this.T],
       |    List[qs77.this.T](),
       |    classOf[List[X] forSome { type X <: qs77.this.T }],
       |    null: List[X] forSome { type X <: qs77.this.T },
       |    ((x: qs77.this.T) => x match { case _ => 1 }): PartialFunction[qs77.this.T, Int],
       |    List[AnyRef { def m: qs77.this.T }](),
       |    List[qs77.this.type](),
       |    List[AnyRef { def m[A: qs77.this.TC]: Int }](),
       |  ) { self: qs77.this.Tr =>
       |    class T; trait TC[A]
       |    def this(
       |        i: Int,
       |        a: Any = classOf[qs77.this.T],
       |        b: Any = List[qs77.this.T](),
       |        c: Any = classOf[List[X] forSome { type X <: qs77.this.T }],
       |        d: Any = null: List[X] forSome { type X <: qs77.this.T },
       |        e: Any = ((x: qs77.this.T) => x match { case _ => 1 }): PartialFunction[qs77.this.T, Int],
       |        f: Any = List[AnyRef { def m: qs77.this.T }](),
       |        g: Any = List[qs77.this.type](),
       |        h: Any = List[AnyRef { def m[A: qs77.this.TC]: Int }](),
       |    ) = this(
       |      classOf[qs77.this.T],
       |      List[qs77.this.T](),
       |      classOf[List[X] forSome { type X <: qs77.this.T }],
       |      null: List[X] forSome { type X <: qs77.this.T },
       |      ((x: qs77.this.T) => x match { case _ => 1 }): PartialFunction[qs77.this.T, Int],
       |      List[AnyRef { def m: qs77.this.T }](),
       |      List[qs77.this.type](),
       |      List[AnyRef { def m[A: qs77.this.TC]: Int }](),
       |    )
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): qs77 <= _empty_/qs77#
         |[0:11..0:11):  <= _empty_/qs77#`<init>`().
         |[1:8..1:9): T <= _empty_/qs77#T#
         |[1:9..1:9):  <= _empty_/qs77#T#`<init>`().
         |[1:17..1:18): P <= _empty_/qs77#P#
         |[1:18..1:18):  <= _empty_/qs77#P#`<init>`().
         |[1:19..1:20): z <= _empty_/qs77#P#`<init>`().(z)
         |[1:19..1:20): z <= _empty_/qs77#P#z.
         |[1:22..1:25): Any => scala/Any#
         |[1:35..1:37): Tr <= _empty_/qs77#Tr#
         |[1:45..1:47): TC <= _empty_/qs77#TC#
         |[1:48..1:49): A <= _empty_/qs77#TC#[A]
         |[2:8..2:12): qs77 <= _empty_/qs77#qs77#
         |[2:12..2:12):  <= _empty_/qs77#qs77#`<init>`().
         |[3:6..3:7): a <= _empty_/qs77#qs77#`<init>`().(a)
         |[3:6..3:7): a <= _empty_/qs77#qs77#a.
         |[3:9..3:12): Any => scala/Any#
         |[3:15..3:22): classOf => scala/Predef.classOf().
         |[3:33..3:34): T => _empty_/qs77#T#
         |[4:6..4:7): b <= _empty_/qs77#qs77#`<init>`().(b)
         |[4:6..4:7): b <= _empty_/qs77#qs77#b.
         |[4:9..4:12): Any => scala/Any#
         |[4:15..4:19): List => scala/collection/immutable/Nil.
         |[4:30..4:31): T => _empty_/qs77#T#
         |[5:6..5:7): c <= _empty_/qs77#qs77#`<init>`().(c)
         |[5:6..5:7): c <= _empty_/qs77#qs77#c.
         |[5:9..5:12): Any => scala/Any#
         |[5:15..5:22): classOf => scala/Predef.classOf().
         |[5:23..5:27): List => scala/package.List#
         |[5:28..5:29): X => local7
         |[5:46..5:47): X <= local7
         |[5:61..5:62): T => _empty_/qs77#T#
         |[6:6..6:7): d <= _empty_/qs77#qs77#`<init>`().(d)
         |[6:6..6:7): d <= _empty_/qs77#qs77#d.
         |[6:9..6:12): Any => scala/Any#
         |[6:21..6:25): List => scala/package.List#
         |[6:26..6:27): X => local8
         |[6:44..6:45): X <= local8
         |[6:49..6:53): qs77 => _empty_/qs77#
         |[6:59..6:60): T => _empty_/qs77#T#
         |[7:6..7:7): e <= _empty_/qs77#qs77#`<init>`().(e)
         |[7:6..7:7): e <= _empty_/qs77#qs77#e.
         |[7:9..7:12): Any => scala/Any#
         |[7:16..7:59): (x: qs77.this.T) => x match { case _ => 1 } <= local75
         |[7:17..7:18): x <= local16
         |[7:30..7:31): T => _empty_/qs77#T#
         |[7:36..7:37): x => local16
         |[7:62..7:77): PartialFunction => scala/PartialFunction#
         |[7:78..7:82): qs77 => _empty_/qs77#
         |[7:88..7:89): T => _empty_/qs77#T#
         |[7:91..7:94): Int => scala/Int#
         |[8:6..8:7): f <= _empty_/qs77#qs77#`<init>`().(f)
         |[8:6..8:7): f <= _empty_/qs77#qs77#f.
         |[8:9..8:12): Any => scala/Any#
         |[8:15..8:19): List => scala/collection/immutable/Nil.
         |[8:20..8:26): AnyRef => java/lang/Object#
         |[8:33..8:34): m <= local17
         |[8:46..8:47): T => _empty_/qs77#T#
         |[9:6..9:7): g <= _empty_/qs77#qs77#`<init>`().(g)
         |[9:6..9:7): g <= _empty_/qs77#qs77#g.
         |[9:9..9:12): Any => scala/Any#
         |[9:15..9:19): List => scala/collection/immutable/Nil.
         |[10:6..10:7): h <= _empty_/qs77#qs77#`<init>`().(h)
         |[10:6..10:7): h <= _empty_/qs77#qs77#h.
         |[10:9..10:12): Any => scala/Any#
         |[10:15..10:19): List => scala/collection/immutable/Nil.
         |[10:20..10:26): AnyRef => java/lang/Object#
         |[10:33..10:34): m <= local18
         |[10:35..10:36): A <= local20
         |[10:38..10:38):  <= local19
         |[10:48..10:50): TC => _empty_/qs77#TC#
         |[10:53..10:56): Int => scala/Int#
         |[12:8..12:10): a1 <= _empty_/qs77#qs77#a1.
         |[12:13..12:20): classOf => scala/Predef.classOf().
         |[12:31..12:32): T => _empty_/qs77#T#
         |[13:8..13:10): b1 <= _empty_/qs77#qs77#b1.
         |[13:13..13:17): List => scala/collection/immutable/Nil.
         |[13:28..13:29): T => _empty_/qs77#T#
         |[14:8..14:10): c1 <= _empty_/qs77#qs77#c1.
         |[14:13..14:20): classOf => scala/Predef.classOf().
         |[14:21..14:25): List => scala/package.List#
         |[14:26..14:27): X => local1
         |[14:44..14:45): X <= local1
         |[14:59..14:60): T => _empty_/qs77#T#
         |[15:8..15:10): d1 <= _empty_/qs77#qs77#d1.
         |[15:12..15:16): List => scala/package.List#
         |[15:17..15:18): X => local2
         |[15:35..15:36): X <= local2
         |[15:40..15:44): qs77 => _empty_/qs77#
         |[15:50..15:51): T => _empty_/qs77#T#
         |[16:8..16:10): e1 <= _empty_/qs77#qs77#e1.
         |[16:12..16:27): PartialFunction => scala/PartialFunction#
         |[16:28..16:32): qs77 => _empty_/qs77#
         |[16:38..16:39): T => _empty_/qs77#T#
         |[16:41..16:44): Int => scala/Int#
         |[16:48..16:91): (x: qs77.this.T) => x match { case _ => 1 } <= local21
         |[16:49..16:50): x <= local28
         |[16:62..16:63): T => _empty_/qs77#T#
         |[16:68..16:69): x => local28
         |[17:8..17:10): f1 <= _empty_/qs77#qs77#f1.
         |[17:13..17:17): List => scala/collection/immutable/Nil.
         |[17:18..17:24): AnyRef => java/lang/Object#
         |[17:31..17:32): m <= local29
         |[17:44..17:45): T => _empty_/qs77#T#
         |[18:8..18:10): g1 <= _empty_/qs77#qs77#g1.
         |[18:13..18:17): List => scala/collection/immutable/Nil.
         |[19:8..19:10): h1 <= _empty_/qs77#qs77#h1.
         |[19:13..19:17): List => scala/collection/immutable/Nil.
         |[19:18..19:24): AnyRef => java/lang/Object#
         |[19:31..19:32): m <= local30
         |[19:33..19:34): A <= local32
         |[19:36..19:36):  <= local31
         |[19:46..19:48): TC => _empty_/qs77#TC#
         |[19:51..19:54): Int => scala/Int#
         |[20:9..20:13): qs77 => _empty_/qs77#
         |[20:19..20:20): P => _empty_/qs77#P#
         |[20:20..20:20):  => _empty_/qs77#P#`<init>`().
         |[21:4..21:11): classOf => scala/Predef.classOf().
         |[21:22..21:23): T => _empty_/qs77#T#
         |[22:4..22:8): List => scala/collection/immutable/Nil.
         |[22:19..22:20): T => _empty_/qs77#T#
         |[23:4..23:11): classOf => scala/Predef.classOf().
         |[23:12..23:16): List => scala/package.List#
         |[23:17..23:18): X => local33
         |[23:35..23:36): X <= local33
         |[23:50..23:51): T => _empty_/qs77#T#
         |[24:10..24:14): List => scala/package.List#
         |[24:15..24:16): X => local34
         |[24:33..24:34): X <= local34
         |[24:38..24:42): qs77 => _empty_/qs77#
         |[24:48..24:49): T => _empty_/qs77#T#
         |[25:5..25:48): (x: qs77.this.T) => x match { case _ => 1 } <= local35
         |[25:6..25:7): x <= local42
         |[25:19..25:20): T => _empty_/qs77#T#
         |[25:25..25:26): x => local42
         |[25:51..25:66): PartialFunction => scala/PartialFunction#
         |[25:67..25:71): qs77 => _empty_/qs77#
         |[25:77..25:78): T => _empty_/qs77#T#
         |[25:80..25:83): Int => scala/Int#
         |[26:4..26:8): List => scala/collection/immutable/Nil.
         |[26:9..26:15): AnyRef => java/lang/Object#
         |[26:22..26:23): m <= local43
         |[26:35..26:36): T => _empty_/qs77#T#
         |[27:4..27:8): List => scala/collection/immutable/Nil.
         |[28:4..28:8): List => scala/collection/immutable/Nil.
         |[28:9..28:15): AnyRef => java/lang/Object#
         |[28:22..28:23): m <= local44
         |[28:24..28:25): A <= local46
         |[28:27..28:27):  <= local45
         |[28:37..28:39): TC => _empty_/qs77#TC#
         |[28:42..28:45): Int => scala/Int#
         |[29:6..29:10): self <= local0
         |[29:12..29:16): qs77 => _empty_/qs77#
         |[29:22..29:24): Tr => _empty_/qs77#Tr#
         |[30:10..30:11): T <= _empty_/qs77#qs77#T#
         |[30:11..30:11):  <= _empty_/qs77#qs77#T#`<init>`().
         |[30:19..30:21): TC <= _empty_/qs77#qs77#TC#
         |[30:22..30:23): A <= _empty_/qs77#qs77#TC#[A]
         |[31:8..31:12): this <= _empty_/qs77#qs77#`<init>`(+1).
         |[32:8..32:9): i <= _empty_/qs77#qs77#`<init>`(+1).(i)
         |[32:11..32:14): Int => scala/Int#
         |[33:8..33:9): a <= _empty_/qs77#qs77#`<init>`(+1).(a)
         |[33:11..33:14): Any => scala/Any#
         |[33:17..33:24): classOf => scala/Predef.classOf().
         |[33:35..33:36): T => _empty_/qs77#T#
         |[34:8..34:9): b <= _empty_/qs77#qs77#`<init>`(+1).(b)
         |[34:11..34:14): Any => scala/Any#
         |[34:17..34:21): List => scala/collection/immutable/Nil.
         |[34:32..34:33): T => _empty_/qs77#T#
         |[35:8..35:9): c <= _empty_/qs77#qs77#`<init>`(+1).(c)
         |[35:11..35:14): Any => scala/Any#
         |[35:17..35:24): classOf => scala/Predef.classOf().
         |[35:25..35:29): List => scala/package.List#
         |[35:30..35:31): X => local47
         |[35:48..35:49): X <= local47
         |[35:63..35:64): T => _empty_/qs77#T#
         |[36:8..36:9): d <= _empty_/qs77#qs77#`<init>`(+1).(d)
         |[36:11..36:14): Any => scala/Any#
         |[36:23..36:27): List => scala/package.List#
         |[36:28..36:29): X => local48
         |[36:46..36:47): X <= local48
         |[36:51..36:55): qs77 => _empty_/qs77#
         |[36:61..36:62): T => _empty_/qs77#T#
         |[37:8..37:9): e <= _empty_/qs77#qs77#`<init>`(+1).(e)
         |[37:11..37:14): Any => scala/Any#
         |[37:18..37:61): (x: qs77.this.T) => x match { case _ => 1 } <= local83
         |[37:19..37:20): x <= local56
         |[37:32..37:33): T => _empty_/qs77#T#
         |[37:38..37:39): x => local56
         |[37:64..37:79): PartialFunction => scala/PartialFunction#
         |[37:80..37:84): qs77 => _empty_/qs77#
         |[37:90..37:91): T => _empty_/qs77#T#
         |[37:93..37:96): Int => scala/Int#
         |[38:8..38:9): f <= _empty_/qs77#qs77#`<init>`(+1).(f)
         |[38:11..38:14): Any => scala/Any#
         |[38:17..38:21): List => scala/collection/immutable/Nil.
         |[38:22..38:28): AnyRef => java/lang/Object#
         |[38:35..38:36): m <= local57
         |[38:48..38:49): T => _empty_/qs77#T#
         |[39:8..39:9): g <= _empty_/qs77#qs77#`<init>`(+1).(g)
         |[39:11..39:14): Any => scala/Any#
         |[39:17..39:21): List => scala/collection/immutable/Nil.
         |[40:8..40:9): h <= _empty_/qs77#qs77#`<init>`(+1).(h)
         |[40:11..40:14): Any => scala/Any#
         |[40:17..40:21): List => scala/collection/immutable/Nil.
         |[40:22..40:28): AnyRef => java/lang/Object#
         |[40:35..40:36): m <= local58
         |[40:37..40:38): A <= local60
         |[40:40..40:40):  <= local59
         |[40:50..40:52): TC => _empty_/qs77#TC#
         |[40:55..40:58): Int => scala/Int#
         |[41:12..41:12):  => _empty_/qs77#qs77#`<init>`().
         |[42:6..42:13): classOf => scala/Predef.classOf().
         |[42:24..42:25): T => _empty_/qs77#T#
         |[43:6..43:10): List => scala/collection/immutable/Nil.
         |[43:21..43:22): T => _empty_/qs77#T#
         |[44:6..44:13): classOf => scala/Predef.classOf().
         |[44:14..44:18): List => scala/package.List#
         |[44:19..44:20): X => local61
         |[44:37..44:38): X <= local61
         |[44:52..44:53): T => _empty_/qs77#T#
         |[45:12..45:16): List => scala/package.List#
         |[45:17..45:18): X => local62
         |[45:35..45:36): X <= local62
         |[45:40..45:44): qs77 => _empty_/qs77#
         |[45:50..45:51): T => _empty_/qs77#T#
         |[46:7..46:50): (x: qs77.this.T) => x match { case _ => 1 } <= local63
         |[46:8..46:9): x <= local70
         |[46:21..46:22): T => _empty_/qs77#T#
         |[46:27..46:28): x => local70
         |[46:53..46:68): PartialFunction => scala/PartialFunction#
         |[46:69..46:73): qs77 => _empty_/qs77#
         |[46:79..46:80): T => _empty_/qs77#T#
         |[46:82..46:85): Int => scala/Int#
         |[47:6..47:10): List => scala/collection/immutable/Nil.
         |[47:11..47:17): AnyRef => java/lang/Object#
         |[47:24..47:25): m <= local71
         |[47:37..47:38): T => _empty_/qs77#T#
         |[48:6..48:10): List => scala/collection/immutable/Nil.
         |[49:6..49:10): List => scala/collection/immutable/Nil.
         |[49:11..49:17): AnyRef => java/lang/Object#
         |[49:24..49:25): m <= local72
         |[49:26..49:27): A <= local74
         |[49:29..49:29):  <= local73
         |[49:39..49:41): TC => _empty_/qs77#TC#
         |[49:44..49:47): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:6..0:10): qs77 <= _empty_/qs77#
         |[0:11..0:11):  <= _empty_/qs77#`<init>`().
         |[1:8..1:9): T <= _empty_/qs77#T#
         |[1:9..1:9):  <= _empty_/qs77#T#`<init>`().
         |[1:17..1:18): P <= _empty_/qs77#P#
         |[1:18..1:18):  <= _empty_/qs77#P#`<init>`().
         |[1:19..1:20): z <= _empty_/qs77#P#`<init>`().(z)
         |[1:19..1:20): z <= _empty_/qs77#P#z.
         |[1:22..1:25): Any => scala/Any#
         |[1:35..1:37): Tr <= _empty_/qs77#Tr#
         |[1:45..1:47): TC <= _empty_/qs77#TC#
         |[1:48..1:49): A <= _empty_/qs77#TC#[A]
         |[2:8..2:12): qs77 <= _empty_/qs77#qs77#
         |[2:12..2:12):  <= _empty_/qs77#qs77#`<init>`().
         |[3:6..3:7): a <= _empty_/qs77#qs77#`<init>`().(a)
         |[3:6..3:7): a <= _empty_/qs77#qs77#a.
         |[3:9..3:12): Any => scala/Any#
         |[3:15..3:22): classOf => scala/Predef.classOf().
         |[3:33..3:34): T => _empty_/qs77#T#
         |[4:6..4:7): b <= _empty_/qs77#qs77#`<init>`().(b)
         |[4:6..4:7): b <= _empty_/qs77#qs77#b.
         |[4:9..4:12): Any => scala/Any#
         |[4:15..4:19): List => scala/collection/immutable/Nil.
         |[4:30..4:31): T => _empty_/qs77#T#
         |[5:6..5:7): c <= _empty_/qs77#qs77#`<init>`().(c)
         |[5:6..5:7): c <= _empty_/qs77#qs77#c.
         |[5:9..5:12): Any => scala/Any#
         |[5:15..5:22): classOf => scala/Predef.classOf().
         |[5:23..5:27): List => scala/package.List#
         |[5:28..5:29): X => local7
         |[5:46..5:47): X <= local7
         |[5:61..5:62): T => _empty_/qs77#T#
         |[6:6..6:7): d <= _empty_/qs77#qs77#`<init>`().(d)
         |[6:6..6:7): d <= _empty_/qs77#qs77#d.
         |[6:9..6:12): Any => scala/Any#
         |[6:21..6:25): List => scala/package.List#
         |[6:26..6:27): X => local8
         |[6:44..6:45): X <= local8
         |[6:49..6:53): qs77 => _empty_/qs77#
         |[6:59..6:60): T => _empty_/qs77#T#
         |[7:6..7:7): e <= _empty_/qs77#qs77#`<init>`().(e)
         |[7:6..7:7): e <= _empty_/qs77#qs77#e.
         |[7:9..7:12): Any => scala/Any#
         |[7:16..7:59): (x: qs77.this.T) => x match { case _ => 1 } <= local75
         |[7:17..7:18): x <= local16
         |[7:36..7:37): x => local16
         |[7:62..7:77): PartialFunction => scala/PartialFunction#
         |[7:78..7:82): qs77 => _empty_/qs77#
         |[7:88..7:89): T => _empty_/qs77#T#
         |[7:91..7:94): Int => scala/Int#
         |[8:6..8:7): f <= _empty_/qs77#qs77#`<init>`().(f)
         |[8:6..8:7): f <= _empty_/qs77#qs77#f.
         |[8:9..8:12): Any => scala/Any#
         |[8:15..8:19): List => scala/collection/immutable/Nil.
         |[8:20..8:26): AnyRef => java/lang/Object#
         |[8:33..8:34): m <= local17
         |[8:46..8:47): T => _empty_/qs77#T#
         |[9:6..9:7): g <= _empty_/qs77#qs77#`<init>`().(g)
         |[9:6..9:7): g <= _empty_/qs77#qs77#g.
         |[9:9..9:12): Any => scala/Any#
         |[9:15..9:19): List => scala/collection/immutable/Nil.
         |[10:6..10:7): h <= _empty_/qs77#qs77#`<init>`().(h)
         |[10:6..10:7): h <= _empty_/qs77#qs77#h.
         |[10:9..10:12): Any => scala/Any#
         |[10:15..10:19): List => scala/collection/immutable/Nil.
         |[10:20..10:26): AnyRef => java/lang/Object#
         |[10:33..10:34): m <= local18
         |[10:35..10:36): A <= local20
         |[10:38..10:38):  <= local19
         |[10:48..10:50): TC => _empty_/qs77#TC#
         |[10:53..10:56): Int => scala/Int#
         |[12:8..12:10): a1 <= _empty_/qs77#qs77#a1.
         |[12:13..12:20): classOf => scala/Predef.classOf().
         |[12:31..12:32): T => _empty_/qs77#T#
         |[13:8..13:10): b1 <= _empty_/qs77#qs77#b1.
         |[13:13..13:17): List => scala/collection/immutable/Nil.
         |[13:28..13:29): T => _empty_/qs77#T#
         |[14:8..14:10): c1 <= _empty_/qs77#qs77#c1.
         |[14:13..14:20): classOf => scala/Predef.classOf().
         |[14:21..14:25): List => scala/package.List#
         |[14:26..14:27): X => local1
         |[14:44..14:45): X <= local1
         |[14:59..14:60): T => _empty_/qs77#T#
         |[15:8..15:10): d1 <= _empty_/qs77#qs77#d1.
         |[15:12..15:16): List => scala/package.List#
         |[15:17..15:18): X => local2
         |[15:35..15:36): X <= local2
         |[15:40..15:44): qs77 => _empty_/qs77#
         |[15:50..15:51): T => _empty_/qs77#T#
         |[16:8..16:10): e1 <= _empty_/qs77#qs77#e1.
         |[16:12..16:27): PartialFunction => scala/PartialFunction#
         |[16:28..16:32): qs77 => _empty_/qs77#
         |[16:38..16:39): T => _empty_/qs77#T#
         |[16:41..16:44): Int => scala/Int#
         |[16:48..16:91): (x: qs77.this.T) => x match { case _ => 1 } <= local21
         |[16:49..16:50): x <= local28
         |[16:68..16:69): x => local28
         |[17:8..17:10): f1 <= _empty_/qs77#qs77#f1.
         |[17:13..17:17): List => scala/collection/immutable/Nil.
         |[17:18..17:24): AnyRef => java/lang/Object#
         |[17:31..17:32): m <= local29
         |[17:44..17:45): T => _empty_/qs77#T#
         |[18:8..18:10): g1 <= _empty_/qs77#qs77#g1.
         |[18:13..18:17): List => scala/collection/immutable/Nil.
         |[19:8..19:10): h1 <= _empty_/qs77#qs77#h1.
         |[19:13..19:17): List => scala/collection/immutable/Nil.
         |[19:18..19:24): AnyRef => java/lang/Object#
         |[19:31..19:32): m <= local30
         |[19:33..19:34): A <= local32
         |[19:36..19:36):  <= local31
         |[19:46..19:48): TC => _empty_/qs77#TC#
         |[19:51..19:54): Int => scala/Int#
         |[20:9..20:13): qs77 => _empty_/qs77#
         |[20:19..20:20): P => _empty_/qs77#P#
         |[20:20..20:20):  => _empty_/qs77#P#`<init>`().
         |[21:4..21:11): classOf => scala/Predef.classOf().
         |[21:22..21:23): T => _empty_/qs77#T#
         |[22:4..22:8): List => scala/collection/immutable/Nil.
         |[22:19..22:20): T => _empty_/qs77#T#
         |[23:4..23:11): classOf => scala/Predef.classOf().
         |[23:12..23:16): List => scala/package.List#
         |[23:17..23:18): X => local33
         |[23:35..23:36): X <= local33
         |[23:50..23:51): T => _empty_/qs77#T#
         |[24:10..24:14): List => scala/package.List#
         |[24:15..24:16): X => local34
         |[24:33..24:34): X <= local34
         |[24:38..24:42): qs77 => _empty_/qs77#
         |[24:48..24:49): T => _empty_/qs77#T#
         |[25:5..25:48): (x: qs77.this.T) => x match { case _ => 1 } <= local35
         |[25:6..25:7): x <= local42
         |[25:25..25:26): x => local42
         |[25:51..25:66): PartialFunction => scala/PartialFunction#
         |[25:67..25:71): qs77 => _empty_/qs77#
         |[25:77..25:78): T => _empty_/qs77#T#
         |[25:80..25:83): Int => scala/Int#
         |[26:4..26:8): List => scala/collection/immutable/Nil.
         |[26:9..26:15): AnyRef => java/lang/Object#
         |[26:22..26:23): m <= local43
         |[26:35..26:36): T => _empty_/qs77#T#
         |[27:4..27:8): List => scala/collection/immutable/Nil.
         |[28:4..28:8): List => scala/collection/immutable/Nil.
         |[28:9..28:15): AnyRef => java/lang/Object#
         |[28:22..28:23): m <= local44
         |[28:24..28:25): A <= local46
         |[28:27..28:27):  <= local45
         |[28:37..28:39): TC => _empty_/qs77#TC#
         |[28:42..28:45): Int => scala/Int#
         |[29:6..29:10): self <= local0
         |[29:12..29:16): qs77 => _empty_/qs77#
         |[29:22..29:24): Tr => _empty_/qs77#Tr#
         |[30:10..30:11): T <= _empty_/qs77#qs77#T#
         |[30:11..30:11):  <= _empty_/qs77#qs77#T#`<init>`().
         |[30:19..30:21): TC <= _empty_/qs77#qs77#TC#
         |[30:22..30:23): A <= _empty_/qs77#qs77#TC#[A]
         |[31:8..31:12): this <= _empty_/qs77#qs77#`<init>`(+1).
         |[32:8..32:9): i <= _empty_/qs77#qs77#`<init>`(+1).(i)
         |[32:11..32:14): Int => scala/Int#
         |[33:8..33:9): a <= _empty_/qs77#qs77#`<init>`(+1).(a)
         |[33:11..33:14): Any => scala/Any#
         |[33:17..33:24): classOf => scala/Predef.classOf().
         |[33:35..33:36): T => _empty_/qs77#T#
         |[34:8..34:9): b <= _empty_/qs77#qs77#`<init>`(+1).(b)
         |[34:11..34:14): Any => scala/Any#
         |[34:17..34:21): List => scala/collection/immutable/Nil.
         |[34:32..34:33): T => _empty_/qs77#T#
         |[35:8..35:9): c <= _empty_/qs77#qs77#`<init>`(+1).(c)
         |[35:11..35:14): Any => scala/Any#
         |[35:17..35:24): classOf => scala/Predef.classOf().
         |[35:25..35:29): List => scala/package.List#
         |[35:30..35:31): X => local47
         |[35:48..35:49): X <= local47
         |[35:63..35:64): T => _empty_/qs77#T#
         |[36:8..36:9): d <= _empty_/qs77#qs77#`<init>`(+1).(d)
         |[36:11..36:14): Any => scala/Any#
         |[36:23..36:27): List => scala/package.List#
         |[36:28..36:29): X => local48
         |[36:46..36:47): X <= local48
         |[36:51..36:55): qs77 => _empty_/qs77#
         |[36:61..36:62): T => _empty_/qs77#T#
         |[37:8..37:9): e <= _empty_/qs77#qs77#`<init>`(+1).(e)
         |[37:11..37:14): Any => scala/Any#
         |[37:18..37:61): (x: qs77.this.T) => x match { case _ => 1 } <= local83
         |[37:19..37:20): x <= local56
         |[37:38..37:39): x => local56
         |[37:64..37:79): PartialFunction => scala/PartialFunction#
         |[37:80..37:84): qs77 => _empty_/qs77#
         |[37:90..37:91): T => _empty_/qs77#T#
         |[37:93..37:96): Int => scala/Int#
         |[38:8..38:9): f <= _empty_/qs77#qs77#`<init>`(+1).(f)
         |[38:11..38:14): Any => scala/Any#
         |[38:17..38:21): List => scala/collection/immutable/Nil.
         |[38:22..38:28): AnyRef => java/lang/Object#
         |[38:35..38:36): m <= local57
         |[38:48..38:49): T => _empty_/qs77#T#
         |[39:8..39:9): g <= _empty_/qs77#qs77#`<init>`(+1).(g)
         |[39:11..39:14): Any => scala/Any#
         |[39:17..39:21): List => scala/collection/immutable/Nil.
         |[40:8..40:9): h <= _empty_/qs77#qs77#`<init>`(+1).(h)
         |[40:11..40:14): Any => scala/Any#
         |[40:17..40:21): List => scala/collection/immutable/Nil.
         |[40:22..40:28): AnyRef => java/lang/Object#
         |[40:35..40:36): m <= local58
         |[40:37..40:38): A <= local60
         |[40:40..40:40):  <= local59
         |[40:50..40:52): TC => _empty_/qs77#TC#
         |[40:55..40:58): Int => scala/Int#
         |[41:12..41:12):  => _empty_/qs77#qs77#`<init>`().
         |[42:6..42:13): classOf => scala/Predef.classOf().
         |[42:24..42:25): T => _empty_/qs77#T#
         |[43:6..43:10): List => scala/collection/immutable/Nil.
         |[43:21..43:22): T => _empty_/qs77#T#
         |[44:6..44:13): classOf => scala/Predef.classOf().
         |[44:14..44:18): List => scala/package.List#
         |[44:19..44:20): X => local61
         |[44:37..44:38): X <= local61
         |[44:52..44:53): T => _empty_/qs77#T#
         |[45:12..45:16): List => scala/package.List#
         |[45:17..45:18): X => local62
         |[45:35..45:36): X <= local62
         |[45:40..45:44): qs77 => _empty_/qs77#
         |[45:50..45:51): T => _empty_/qs77#T#
         |[46:7..46:50): (x: qs77.this.T) => x match { case _ => 1 } <= local63
         |[46:8..46:9): x <= local70
         |[46:27..46:28): x => local70
         |[46:53..46:68): PartialFunction => scala/PartialFunction#
         |[46:69..46:73): qs77 => _empty_/qs77#
         |[46:79..46:80): T => _empty_/qs77#T#
         |[46:82..46:85): Int => scala/Int#
         |[47:6..47:10): List => scala/collection/immutable/Nil.
         |[47:11..47:17): AnyRef => java/lang/Object#
         |[47:24..47:25): m <= local71
         |[47:37..47:38): T => _empty_/qs77#T#
         |[48:6..48:10): List => scala/collection/immutable/Nil.
         |[49:6..49:10): List => scala/collection/immutable/Nil.
         |[49:11..49:17): AnyRef => java/lang/Object#
         |[49:24..49:25): m <= local72
         |[49:26..49:27): A <= local74
         |[49:29..49:29):  <= local73
         |[49:39..49:41): TC => _empty_/qs77#TC#
         |[49:44..49:47): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "a type through super in constructor defaults and parent arguments",
    """|class qs67p { class T }
       |class qs67q(z: Any*) { class T }
       |class qs67 extends qs67p {
       |  class K(
       |      a: Any = classOf[super.T],
       |      b: Any = List[super.T](),
       |      c: Any = classOf[List[X] forSome { type X <: super.T }],
       |      d: Any = null: List[X] forSome { type X <: super.T },
       |      e: Any = ((x: super.T) => x match { case _ => 1 }): PartialFunction[super.T, Int],
       |  ) extends qs67q
       |  class J extends qs67q {
       |    def this(
       |        i: Int,
       |        a: Any = classOf[super.T],
       |        b: Any = List[super.T](),
       |        c: Any = classOf[List[X] forSome { type X <: super.T }],
       |        d: Any = null: List[X] forSome { type X <: super.T },
       |        e: Any = ((x: super.T) => x match { case _ => 1 }): PartialFunction[super.T, Int],
       |    ) = this()
       |  }
       |  class L
       |      extends qs67q(
       |        classOf[super.T],
       |        List[super.T](),
       |        classOf[List[X] forSome { type X <: super.T }],
       |        null: List[X] forSome { type X <: super.T },
       |        ((x: super.T) => x match { case _ => 1 }): PartialFunction[super.T, Int],
       |      )
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:11): qs67p <= _empty_/qs67p#
         |[0:12..0:12):  <= _empty_/qs67p#`<init>`().
         |[0:20..0:21): T <= _empty_/qs67p#T#
         |[0:21..0:21):  <= _empty_/qs67p#T#`<init>`().
         |[1:6..1:11): qs67q <= _empty_/qs67q#
         |[1:11..1:11):  <= _empty_/qs67q#`<init>`().
         |[1:12..1:13): z <= _empty_/qs67q#`<init>`().(z)
         |[1:12..1:13): z <= _empty_/qs67q#z.
         |[1:15..1:18): Any => scala/Any#
         |[1:29..1:30): T <= _empty_/qs67q#T#
         |[1:30..1:30):  <= _empty_/qs67q#T#`<init>`().
         |[2:6..2:10): qs67 <= _empty_/qs67#
         |[2:11..2:11):  <= _empty_/qs67#`<init>`().
         |[2:19..2:24): qs67p => _empty_/qs67p#
         |[2:24..2:24):  => _empty_/qs67p#`<init>`().
         |[3:8..3:9): K <= _empty_/qs67#K#
         |[3:9..3:9):  <= _empty_/qs67#K#`<init>`().
         |[4:6..4:7): a <= _empty_/qs67#K#`<init>`().(a)
         |[4:6..4:7): a <= _empty_/qs67#K#a.
         |[4:9..4:12): Any => scala/Any#
         |[4:15..4:22): classOf => scala/Predef.classOf().
         |[4:29..4:30): T => _empty_/qs67p#T#
         |[5:6..5:7): b <= _empty_/qs67#K#`<init>`().(b)
         |[5:6..5:7): b <= _empty_/qs67#K#b.
         |[5:9..5:12): Any => scala/Any#
         |[5:15..5:19): List => scala/collection/immutable/Nil.
         |[5:26..5:27): T => _empty_/qs67p#T#
         |[6:6..6:7): c <= _empty_/qs67#K#`<init>`().(c)
         |[6:6..6:7): c <= _empty_/qs67#K#c.
         |[6:9..6:12): Any => scala/Any#
         |[6:15..6:22): classOf => scala/Predef.classOf().
         |[6:23..6:27): List => scala/package.List#
         |[6:28..6:29): X => local0
         |[6:46..6:47): X <= local0
         |[6:57..6:58): T => _empty_/qs67p#T#
         |[7:6..7:7): d <= _empty_/qs67#K#`<init>`().(d)
         |[7:6..7:7): d <= _empty_/qs67#K#d.
         |[7:9..7:12): Any => scala/Any#
         |[7:21..7:25): List => scala/package.List#
         |[7:26..7:27): X => local1
         |[7:44..7:45): X <= local1
         |[7:49..7:49):  => _empty_/qs67#
         |[7:54..7:54):  => _empty_/qs67p#
         |[7:55..7:56): T => _empty_/qs67p#T#
         |[8:6..8:7): e <= _empty_/qs67#K#`<init>`().(e)
         |[8:6..8:7): e <= _empty_/qs67#K#e.
         |[8:9..8:12): Any => scala/Any#
         |[8:16..8:55): (x: super.T) => x match { case _ => 1 } <= local10
         |[8:17..8:18): x <= local9
         |[8:26..8:27): T => _empty_/qs67p#T#
         |[8:32..8:33): x => local9
         |[8:58..8:73): PartialFunction => scala/PartialFunction#
         |[8:74..8:74):  => _empty_/qs67#
         |[8:79..8:79):  => _empty_/qs67p#
         |[8:80..8:81): T => _empty_/qs67p#T#
         |[8:83..8:86): Int => scala/Int#
         |[9:12..9:17): qs67q => _empty_/qs67q#
         |[9:17..9:17):  => _empty_/qs67q#`<init>`().
         |[10:8..10:9): J <= _empty_/qs67#J#
         |[10:10..10:10):  <= _empty_/qs67#J#`<init>`().
         |[10:18..10:23): qs67q => _empty_/qs67q#
         |[10:23..10:23):  => _empty_/qs67q#`<init>`().
         |[11:8..11:12): this <= _empty_/qs67#J#`<init>`(+1).
         |[12:8..12:9): i <= _empty_/qs67#J#`<init>`(+1).(i)
         |[12:11..12:14): Int => scala/Int#
         |[13:8..13:9): a <= _empty_/qs67#J#`<init>`(+1).(a)
         |[13:11..13:14): Any => scala/Any#
         |[13:17..13:24): classOf => scala/Predef.classOf().
         |[13:31..13:32): T => _empty_/qs67p#T#
         |[14:8..14:9): b <= _empty_/qs67#J#`<init>`(+1).(b)
         |[14:11..14:14): Any => scala/Any#
         |[14:17..14:21): List => scala/collection/immutable/Nil.
         |[14:28..14:29): T => _empty_/qs67p#T#
         |[15:8..15:9): c <= _empty_/qs67#J#`<init>`(+1).(c)
         |[15:11..15:14): Any => scala/Any#
         |[15:17..15:24): classOf => scala/Predef.classOf().
         |[15:25..15:29): List => scala/package.List#
         |[15:30..15:31): X => local18
         |[15:48..15:49): X <= local18
         |[15:59..15:60): T => _empty_/qs67p#T#
         |[16:8..16:9): d <= _empty_/qs67#J#`<init>`(+1).(d)
         |[16:11..16:14): Any => scala/Any#
         |[16:23..16:27): List => scala/package.List#
         |[16:28..16:29): X => local19
         |[16:46..16:47): X <= local19
         |[16:51..16:51):  => _empty_/qs67#
         |[16:56..16:56):  => _empty_/qs67p#
         |[16:57..16:58): T => _empty_/qs67p#T#
         |[17:8..17:9): e <= _empty_/qs67#J#`<init>`(+1).(e)
         |[17:11..17:14): Any => scala/Any#
         |[17:18..17:57): (x: super.T) => x match { case _ => 1 } <= local28
         |[17:19..17:20): x <= local27
         |[17:28..17:29): T => _empty_/qs67p#T#
         |[17:34..17:35): x => local27
         |[17:60..17:75): PartialFunction => scala/PartialFunction#
         |[17:76..17:76):  => _empty_/qs67#
         |[17:81..17:81):  => _empty_/qs67p#
         |[17:82..17:83): T => _empty_/qs67p#T#
         |[17:85..17:88): Int => scala/Int#
         |[18:12..18:12):  => _empty_/qs67#J#`<init>`().
         |[20:8..20:9): L <= _empty_/qs67#L#
         |[21:6..21:6):  <= _empty_/qs67#L#`<init>`().
         |[21:14..21:19): qs67q => _empty_/qs67q#
         |[21:19..21:19):  => _empty_/qs67q#`<init>`().
         |[22:8..22:15): classOf => scala/Predef.classOf().
         |[22:22..22:23): T => _empty_/qs67p#T#
         |[23:8..23:12): List => scala/collection/immutable/Nil.
         |[23:19..23:20): T => _empty_/qs67p#T#
         |[24:8..24:15): classOf => scala/Predef.classOf().
         |[24:16..24:20): List => scala/package.List#
         |[24:21..24:22): X => local36
         |[24:39..24:40): X <= local36
         |[24:50..24:51): T => _empty_/qs67p#T#
         |[25:14..25:18): List => scala/package.List#
         |[25:19..25:20): X => local37
         |[25:37..25:38): X <= local37
         |[25:42..25:42):  => _empty_/qs67#
         |[25:47..25:47):  => _empty_/qs67p#
         |[25:48..25:49): T => _empty_/qs67p#T#
         |[26:9..26:48): (x: super.T) => x match { case _ => 1 } <= local38
         |[26:10..26:11): x <= local45
         |[26:19..26:20): T => _empty_/qs67p#T#
         |[26:25..26:26): x => local45
         |[26:51..26:66): PartialFunction => scala/PartialFunction#
         |[26:67..26:67):  => _empty_/qs67#
         |[26:72..26:72):  => _empty_/qs67p#
         |[26:73..26:74): T => _empty_/qs67p#T#
         |[26:76..26:79): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:6..0:11): qs67p <= _empty_/qs67p#
         |[0:12..0:12):  <= _empty_/qs67p#`<init>`().
         |[0:20..0:21): T <= _empty_/qs67p#T#
         |[0:21..0:21):  <= _empty_/qs67p#T#`<init>`().
         |[1:6..1:11): qs67q <= _empty_/qs67q#
         |[1:11..1:11):  <= _empty_/qs67q#`<init>`().
         |[1:12..1:13): z <= _empty_/qs67q#`<init>`().(z)
         |[1:12..1:13): z <= _empty_/qs67q#z.
         |[1:15..1:18): Any => scala/Any#
         |[1:29..1:30): T <= _empty_/qs67q#T#
         |[1:30..1:30):  <= _empty_/qs67q#T#`<init>`().
         |[2:6..2:10): qs67 <= _empty_/qs67#
         |[2:11..2:11):  <= _empty_/qs67#`<init>`().
         |[2:19..2:24): qs67p => _empty_/qs67p#
         |[2:24..2:24):  => _empty_/qs67p#`<init>`().
         |[3:8..3:9): K <= _empty_/qs67#K#
         |[3:9..3:9):  <= _empty_/qs67#K#`<init>`().
         |[4:6..4:7): a <= _empty_/qs67#K#`<init>`().(a)
         |[4:6..4:7): a <= _empty_/qs67#K#a.
         |[4:9..4:12): Any => scala/Any#
         |[4:15..4:22): classOf => scala/Predef.classOf().
         |[4:29..4:30): T => _empty_/qs67p#T#
         |[5:6..5:7): b <= _empty_/qs67#K#`<init>`().(b)
         |[5:6..5:7): b <= _empty_/qs67#K#b.
         |[5:9..5:12): Any => scala/Any#
         |[5:15..5:19): List => scala/collection/immutable/Nil.
         |[5:26..5:27): T => _empty_/qs67p#T#
         |[6:6..6:7): c <= _empty_/qs67#K#`<init>`().(c)
         |[6:6..6:7): c <= _empty_/qs67#K#c.
         |[6:9..6:12): Any => scala/Any#
         |[6:15..6:22): classOf => scala/Predef.classOf().
         |[6:23..6:27): List => scala/package.List#
         |[6:28..6:29): X => local0
         |[6:46..6:47): X <= local0
         |[6:57..6:58): T => _empty_/qs67p#T#
         |[7:6..7:7): d <= _empty_/qs67#K#`<init>`().(d)
         |[7:6..7:7): d <= _empty_/qs67#K#d.
         |[7:9..7:12): Any => scala/Any#
         |[7:21..7:25): List => scala/package.List#
         |[7:26..7:27): X => local1
         |[7:44..7:45): X <= local1
         |[7:49..7:49):  => _empty_/qs67#
         |[7:54..7:54):  => _empty_/qs67p#
         |[7:55..7:56): T => _empty_/qs67p#T#
         |[8:6..8:7): e <= _empty_/qs67#K#`<init>`().(e)
         |[8:6..8:7): e <= _empty_/qs67#K#e.
         |[8:9..8:12): Any => scala/Any#
         |[8:16..8:55): (x: super.T) => x match { case _ => 1 } <= local10
         |[8:17..8:18): x <= local9
         |[8:32..8:33): x => local9
         |[8:58..8:73): PartialFunction => scala/PartialFunction#
         |[8:74..8:74):  => _empty_/qs67#
         |[8:79..8:79):  => _empty_/qs67p#
         |[8:80..8:81): T => _empty_/qs67p#T#
         |[8:83..8:86): Int => scala/Int#
         |[9:12..9:17): qs67q => _empty_/qs67q#
         |[9:17..9:17):  => _empty_/qs67q#`<init>`().
         |[10:8..10:9): J <= _empty_/qs67#J#
         |[10:10..10:10):  <= _empty_/qs67#J#`<init>`().
         |[10:18..10:23): qs67q => _empty_/qs67q#
         |[10:23..10:23):  => _empty_/qs67q#`<init>`().
         |[11:8..11:12): this <= _empty_/qs67#J#`<init>`(+1).
         |[12:8..12:9): i <= _empty_/qs67#J#`<init>`(+1).(i)
         |[12:11..12:14): Int => scala/Int#
         |[13:8..13:9): a <= _empty_/qs67#J#`<init>`(+1).(a)
         |[13:11..13:14): Any => scala/Any#
         |[13:17..13:24): classOf => scala/Predef.classOf().
         |[13:31..13:32): T => _empty_/qs67p#T#
         |[14:8..14:9): b <= _empty_/qs67#J#`<init>`(+1).(b)
         |[14:11..14:14): Any => scala/Any#
         |[14:17..14:21): List => scala/collection/immutable/Nil.
         |[14:28..14:29): T => _empty_/qs67p#T#
         |[15:8..15:9): c <= _empty_/qs67#J#`<init>`(+1).(c)
         |[15:11..15:14): Any => scala/Any#
         |[15:17..15:24): classOf => scala/Predef.classOf().
         |[15:25..15:29): List => scala/package.List#
         |[15:30..15:31): X => local18
         |[15:48..15:49): X <= local18
         |[15:59..15:60): T => _empty_/qs67p#T#
         |[16:8..16:9): d <= _empty_/qs67#J#`<init>`(+1).(d)
         |[16:11..16:14): Any => scala/Any#
         |[16:23..16:27): List => scala/package.List#
         |[16:28..16:29): X => local19
         |[16:46..16:47): X <= local19
         |[16:51..16:51):  => _empty_/qs67#
         |[16:56..16:56):  => _empty_/qs67p#
         |[16:57..16:58): T => _empty_/qs67p#T#
         |[17:8..17:9): e <= _empty_/qs67#J#`<init>`(+1).(e)
         |[17:11..17:14): Any => scala/Any#
         |[17:18..17:57): (x: super.T) => x match { case _ => 1 } <= local28
         |[17:19..17:20): x <= local27
         |[17:34..17:35): x => local27
         |[17:60..17:75): PartialFunction => scala/PartialFunction#
         |[17:76..17:76):  => _empty_/qs67#
         |[17:81..17:81):  => _empty_/qs67p#
         |[17:82..17:83): T => _empty_/qs67p#T#
         |[17:85..17:88): Int => scala/Int#
         |[18:12..18:12):  => _empty_/qs67#J#`<init>`().
         |[20:8..20:9): L <= _empty_/qs67#L#
         |[21:6..21:6):  <= _empty_/qs67#L#`<init>`().
         |[21:14..21:19): qs67q => _empty_/qs67q#
         |[21:19..21:19):  => _empty_/qs67q#`<init>`().
         |[22:8..22:15): classOf => scala/Predef.classOf().
         |[22:22..22:23): T => _empty_/qs67p#T#
         |[23:8..23:12): List => scala/collection/immutable/Nil.
         |[23:19..23:20): T => _empty_/qs67p#T#
         |[24:8..24:15): classOf => scala/Predef.classOf().
         |[24:16..24:20): List => scala/package.List#
         |[24:21..24:22): X => local36
         |[24:39..24:40): X <= local36
         |[24:50..24:51): T => _empty_/qs67p#T#
         |[25:14..25:18): List => scala/package.List#
         |[25:19..25:20): X => local37
         |[25:37..25:38): X <= local37
         |[25:42..25:42):  => _empty_/qs67#
         |[25:47..25:47):  => _empty_/qs67p#
         |[25:48..25:49): T => _empty_/qs67p#T#
         |[26:9..26:48): (x: super.T) => x match { case _ => 1 } <= local38
         |[26:10..26:11): x <= local45
         |[26:25..26:26): x => local45
         |[26:51..26:66): PartialFunction => scala/PartialFunction#
         |[26:67..26:67):  => _empty_/qs67#
         |[26:72..26:72):  => _empty_/qs67p#
         |[26:73..26:74): T => _empty_/qs67p#T#
         |[26:76..26:79): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "empty lists through qualifiers",
    """|class qs27k(l: Any)
       |object qs27 {
       |  import scala.{collection => q}
       |  import scala.collection.immutable
       |  val a = scala.List()
       |  val b = immutable.List()
       |  val c = q.immutable.List()
       |  val d = _root_.scala.List()
       |  val e = scala.collection.immutable.List[Int]()
       |  val f: PartialFunction[Int, Any] = x => x match { case _ => q.immutable.List() }
       |  val g = new qs27k(q.immutable.List())
       |  val h = new qs27k(scala.List())
       |}
       |""".stripMargin,
    """|[0:6..0:11): qs27k <= _empty_/qs27k#
       |[0:11..0:11):  <= _empty_/qs27k#`<init>`().
       |[0:12..0:13): l <= _empty_/qs27k#`<init>`().(l)
       |[0:12..0:13): l <= _empty_/qs27k#l.
       |[0:15..0:18): Any => scala/Any#
       |[1:7..1:11): qs27 <= _empty_/qs27.
       |[2:9..2:14): scala => scala/
       |[2:16..2:26): collection => scala/collection/
       |[3:9..3:14): scala => scala/
       |[3:15..3:25): collection => scala/collection/
       |[3:26..3:35): immutable => scala/collection/immutable/
       |[4:6..4:7): a <= _empty_/qs27.a.
       |[4:16..4:20): List => scala/collection/immutable/Nil.
       |[5:6..5:7): b <= _empty_/qs27.b.
       |[5:20..5:24): List => scala/collection/immutable/Nil.
       |[6:6..6:7): c <= _empty_/qs27.c.
       |[6:22..6:26): List => scala/collection/immutable/Nil.
       |[7:6..7:7): d <= _empty_/qs27.d.
       |[7:23..7:27): List => scala/collection/immutable/Nil.
       |[8:6..8:7): e <= _empty_/qs27.e.
       |[8:37..8:41): List => scala/collection/immutable/Nil.
       |[8:42..8:45): Int => scala/Int#
       |[9:6..9:7): f <= _empty_/qs27.f.
       |[9:9..9:24): PartialFunction => scala/PartialFunction#
       |[9:25..9:28): Int => scala/Int#
       |[9:30..9:33): Any => scala/Any#
       |[9:37..9:38): x <= local7
       |[9:37..9:82): x => x match { case _ => q.immutable.List() } <= local0
       |[9:42..9:43): x => local7
       |[9:74..9:78): List => scala/collection/immutable/Nil.
       |[10:6..10:7): g <= _empty_/qs27.g.
       |[10:14..10:19): qs27k => _empty_/qs27k#
       |[10:19..10:19):  => _empty_/qs27k#`<init>`().
       |[10:32..10:36): List => scala/collection/immutable/Nil.
       |[11:6..11:7): h <= _empty_/qs27.h.
       |[11:14..11:19): qs27k => _empty_/qs27k#
       |[11:19..11:19):  => _empty_/qs27k#`<init>`().
       |[11:26..11:30): List => scala/collection/immutable/Nil.
       |""".stripMargin,
  )

  occurrences(
    "an empty list through a qualifier named like a member",
    """|object qs28 {
       |  import scala.{collection => default}
       |  class E(l: Any) { val scala = 1; def this() = this(scala.List()) }
       |  val a: PartialFunction[Int, Any] = x => x match { case _ => default.immutable.List() }
       |}
       |""".stripMargin,
    """|[0:7..0:11): qs28 <= _empty_/qs28.
       |[1:9..1:14): scala => scala/
       |[1:16..1:26): collection => scala/collection/
       |[2:8..2:9): E <= _empty_/qs28.E#
       |[2:9..2:9):  <= _empty_/qs28.E#`<init>`().
       |[2:10..2:11): l <= _empty_/qs28.E#`<init>`().(l)
       |[2:10..2:11): l <= _empty_/qs28.E#l.
       |[2:13..2:16): Any => scala/Any#
       |[2:24..2:29): scala <= _empty_/qs28.E#scala.
       |[2:39..2:43): this <= _empty_/qs28.E#`<init>`(+1).
       |[2:52..2:52):  => _empty_/qs28.E#`<init>`().
       |[2:59..2:63): List => scala/collection/immutable/Nil.
       |[3:6..3:7): a <= _empty_/qs28.a.
       |[3:9..3:24): PartialFunction => scala/PartialFunction#
       |[3:25..3:28): Int => scala/Int#
       |[3:30..3:33): Any => scala/Any#
       |[3:37..3:38): x <= local7
       |[3:37..3:88): x => x match { case _ => default.immutable.List() } <= local0
       |[3:42..3:43): x => local7
       |[3:80..3:84): List => scala/collection/immutable/Nil.
       |""".stripMargin,
  )

  occurrences(
    "empty lists through an alias of List and without a qualifier",
    """|class qs40k(l: Any)
       |object qs40 {
       |  object o { val List = scala.List }
       |  val a = o.List()
       |  val b = List()
       |  val f: PartialFunction[Int, Any] = x => x match { case _ => o.List() }
       |  val g = new qs40k(o.List())
       |  val h = new qs40k(List())
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:6..0:11): qs40k <= _empty_/qs40k#
         |[0:11..0:11):  <= _empty_/qs40k#`<init>`().
         |[0:12..0:13): l <= _empty_/qs40k#`<init>`().(l)
         |[0:12..0:13): l <= _empty_/qs40k#l.
         |[0:15..0:18): Any => scala/Any#
         |[1:7..1:11): qs40 <= _empty_/qs40.
         |[2:9..2:10): o <= _empty_/qs40.o.
         |[2:17..2:21): List <= _empty_/qs40.o.List.
         |[2:30..2:34): List => scala/collection/immutable/List.
         |[3:6..3:7): a <= _empty_/qs40.a.
         |[3:12..3:16): List => scala/collection/immutable/Nil.
         |[4:6..4:7): b <= _empty_/qs40.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[5:6..5:7): f <= _empty_/qs40.f.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Any => scala/Any#
         |[5:37..5:38): x <= local7
         |[5:37..5:72): x => x match { case _ => o.List() } <= local0
         |[5:42..5:43): x => local7
         |[5:64..5:68): List => scala/collection/immutable/Nil.
         |[6:6..6:7): g <= _empty_/qs40.g.
         |[6:14..6:19): qs40k => _empty_/qs40k#
         |[6:19..6:19):  => _empty_/qs40k#`<init>`().
         |[6:22..6:26): List => scala/collection/immutable/Nil.
         |[7:6..7:7): h <= _empty_/qs40.h.
         |[7:14..7:19): qs40k => _empty_/qs40k#
         |[7:19..7:19):  => _empty_/qs40k#`<init>`().
         |[7:20..7:24): List => scala/collection/immutable/Nil.
         |""".stripMargin
    else
      """|[0:6..0:11): qs40k <= _empty_/qs40k#
         |[0:11..0:11):  <= _empty_/qs40k#`<init>`().
         |[0:12..0:13): l <= _empty_/qs40k#`<init>`().(l)
         |[0:12..0:13): l <= _empty_/qs40k#l.
         |[0:15..0:18): Any => scala/Any#
         |[1:7..1:11): qs40 <= _empty_/qs40.
         |[2:9..2:10): o <= _empty_/qs40.o.
         |[2:17..2:21): List <= _empty_/qs40.o.List.
         |[2:24..2:29): scala => scala/
         |[2:30..2:34): List => scala/package.List.
         |[3:6..3:7): a <= _empty_/qs40.a.
         |[3:10..3:11): o => _empty_/qs40.o.
         |[3:12..3:16): List => _empty_/qs40.o.List.
         |[4:6..4:7): b <= _empty_/qs40.b.
         |[4:10..4:14): List => scala/collection/immutable/Nil.
         |[5:6..5:7): f <= _empty_/qs40.f.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Any => scala/Any#
         |[5:37..5:38): x <= local7
         |[5:37..5:72): x => x match { case _ => o.List() } <= local0
         |[5:42..5:43): x => local7
         |[5:62..5:63): o => _empty_/qs40.o.
         |[5:64..5:68): List => _empty_/qs40.o.List.
         |[6:6..6:7): g <= _empty_/qs40.g.
         |[6:14..6:19): qs40k => _empty_/qs40k#
         |[6:19..6:19):  => _empty_/qs40k#`<init>`().
         |[6:20..6:21): o => _empty_/qs40.o.
         |[6:22..6:26): List => _empty_/qs40.o.List.
         |[7:6..7:7): h <= _empty_/qs40.h.
         |[7:14..7:19): qs40k => _empty_/qs40k#
         |[7:19..7:19):  => _empty_/qs40k#`<init>`().
         |[7:20..7:24): List => scala/collection/immutable/Nil.
         |""".stripMargin,
  )

  occurrences(
    "an empty list through an object named scala",
    """|class qs41k(l: Any)
       |object qs41 {
       |  object scala { val List = _root_.scala.List }
       |  val a = scala.List()
       |  val f: PartialFunction[Int, Any] = x => x match { case _ => scala.List() }
       |  val g = new qs41k(scala.List())
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:6..0:11): qs41k <= _empty_/qs41k#
         |[0:11..0:11):  <= _empty_/qs41k#`<init>`().
         |[0:12..0:13): l <= _empty_/qs41k#`<init>`().(l)
         |[0:12..0:13): l <= _empty_/qs41k#l.
         |[0:15..0:18): Any => scala/Any#
         |[1:7..1:11): qs41 <= _empty_/qs41.
         |[2:9..2:14): scala <= _empty_/qs41.scala.
         |[2:21..2:25): List <= _empty_/qs41.scala.List.
         |[2:41..2:45): List => scala/collection/immutable/List.
         |[3:6..3:7): a <= _empty_/qs41.a.
         |[3:16..3:20): List => scala/collection/immutable/Nil.
         |[4:6..4:7): f <= _empty_/qs41.f.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:28): Int => scala/Int#
         |[4:30..4:33): Any => scala/Any#
         |[4:37..4:38): x <= local7
         |[4:37..4:76): x => x match { case _ => scala.List() } <= local0
         |[4:42..4:43): x => local7
         |[4:68..4:72): List => scala/collection/immutable/Nil.
         |[5:6..5:7): g <= _empty_/qs41.g.
         |[5:14..5:19): qs41k => _empty_/qs41k#
         |[5:19..5:19):  => _empty_/qs41k#`<init>`().
         |[5:26..5:30): List => scala/collection/immutable/Nil.
         |""".stripMargin
    else
      """|[0:6..0:11): qs41k <= _empty_/qs41k#
         |[0:11..0:11):  <= _empty_/qs41k#`<init>`().
         |[0:12..0:13): l <= _empty_/qs41k#`<init>`().(l)
         |[0:12..0:13): l <= _empty_/qs41k#l.
         |[0:15..0:18): Any => scala/Any#
         |[1:7..1:11): qs41 <= _empty_/qs41.
         |[2:9..2:14): scala <= _empty_/qs41.scala.
         |[2:21..2:25): List <= _empty_/qs41.scala.List.
         |[2:35..2:40): scala => scala/
         |[2:41..2:45): List => scala/package.List.
         |[3:6..3:7): a <= _empty_/qs41.a.
         |[3:10..3:15): scala => _empty_/qs41.scala.
         |[3:16..3:20): List => _empty_/qs41.scala.List.
         |[4:6..4:7): f <= _empty_/qs41.f.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:28): Int => scala/Int#
         |[4:30..4:33): Any => scala/Any#
         |[4:37..4:38): x <= local7
         |[4:37..4:76): x => x match { case _ => scala.List() } <= local0
         |[4:42..4:43): x => local7
         |[4:62..4:67): scala => _empty_/qs41.scala.
         |[4:68..4:72): List => _empty_/qs41.scala.List.
         |[5:6..5:7): g <= _empty_/qs41.g.
         |[5:14..5:19): qs41k => _empty_/qs41k#
         |[5:19..5:19):  => _empty_/qs41k#`<init>`().
         |[5:20..5:25): scala => _empty_/qs41.scala.
         |[5:26..5:30): List => _empty_/qs41.scala.List.
         |""".stripMargin,
  )

  occurrences(
    "empty lists with a type argument through an alias of List and the scala package",
    """|class qs50k(l: Any)
       |object qs50 {
       |  object o { val List = scala.List }
       |  val a = o.List[Int]()
       |  val b = scala.List[Int]()
       |  val f: PartialFunction[Int, Any] = x => x match { case _ => o.List[Int]() }
       |  val g = new qs50k(o.List[Int]())
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:6..0:11): qs50k <= _empty_/qs50k#
         |[0:11..0:11):  <= _empty_/qs50k#`<init>`().
         |[0:12..0:13): l <= _empty_/qs50k#`<init>`().(l)
         |[0:12..0:13): l <= _empty_/qs50k#l.
         |[0:15..0:18): Any => scala/Any#
         |[1:7..1:11): qs50 <= _empty_/qs50.
         |[2:9..2:10): o <= _empty_/qs50.o.
         |[2:17..2:21): List <= _empty_/qs50.o.List.
         |[2:30..2:34): List => scala/collection/immutable/List.
         |[3:6..3:7): a <= _empty_/qs50.a.
         |[3:12..3:16): List => scala/collection/immutable/Nil.
         |[3:17..3:20): Int => scala/Int#
         |[4:6..4:7): b <= _empty_/qs50.b.
         |[4:16..4:20): List => scala/collection/immutable/Nil.
         |[4:21..4:24): Int => scala/Int#
         |[5:6..5:7): f <= _empty_/qs50.f.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Any => scala/Any#
         |[5:37..5:38): x <= local7
         |[5:37..5:77): x => x match { case _ => o.List[Int]() } <= local0
         |[5:42..5:43): x => local7
         |[5:64..5:68): List => scala/collection/immutable/Nil.
         |[5:69..5:72): Int => scala/Int#
         |[6:6..6:7): g <= _empty_/qs50.g.
         |[6:14..6:19): qs50k => _empty_/qs50k#
         |[6:19..6:19):  => _empty_/qs50k#`<init>`().
         |[6:22..6:26): List => scala/collection/immutable/Nil.
         |[6:27..6:30): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:6..0:11): qs50k <= _empty_/qs50k#
         |[0:11..0:11):  <= _empty_/qs50k#`<init>`().
         |[0:12..0:13): l <= _empty_/qs50k#`<init>`().(l)
         |[0:12..0:13): l <= _empty_/qs50k#l.
         |[0:15..0:18): Any => scala/Any#
         |[1:7..1:11): qs50 <= _empty_/qs50.
         |[2:9..2:10): o <= _empty_/qs50.o.
         |[2:17..2:21): List <= _empty_/qs50.o.List.
         |[2:24..2:29): scala => scala/
         |[2:30..2:34): List => scala/package.List.
         |[3:6..3:7): a <= _empty_/qs50.a.
         |[3:10..3:11): o => _empty_/qs50.o.
         |[3:12..3:16): List => _empty_/qs50.o.List.
         |[3:17..3:20): Int => scala/Int#
         |[4:6..4:7): b <= _empty_/qs50.b.
         |[4:16..4:20): List => scala/collection/immutable/Nil.
         |[4:21..4:24): Int => scala/Int#
         |[5:6..5:7): f <= _empty_/qs50.f.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Any => scala/Any#
         |[5:37..5:38): x <= local7
         |[5:37..5:77): x => x match { case _ => o.List[Int]() } <= local0
         |[5:42..5:43): x => local7
         |[5:62..5:63): o => _empty_/qs50.o.
         |[5:64..5:68): List => _empty_/qs50.o.List.
         |[5:69..5:72): Int => scala/Int#
         |[6:6..6:7): g <= _empty_/qs50.g.
         |[6:14..6:19): qs50k => _empty_/qs50k#
         |[6:19..6:19):  => _empty_/qs50k#`<init>`().
         |[6:20..6:21): o => _empty_/qs50.o.
         |[6:22..6:26): List => _empty_/qs50.o.List.
         |[6:27..6:30): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "empty lists through an explicit apply",
    """|class qs57k(l: Any)
       |object qs57 {
       |  import scala.collection.immutable
       |  object o { val List = scala.List }
       |  val a1 = scala.List.apply()
       |  val a2 = immutable.List.apply()
       |  val a3 = o.List.apply()
       |  val f1: PartialFunction[Int, Any] = x => x match { case _ => scala.List.apply() }
       |  val f2: PartialFunction[Int, Any] = x => x match { case _ => immutable.List.apply() }
       |  val f3: PartialFunction[Int, Any] = x => x match { case _ => o.List.apply() }
       |  val g1 = new qs57k(scala.List.apply())
       |  val g2 = new qs57k(immutable.List.apply())
       |  val g3 = new qs57k(o.List.apply())
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:6..0:11): qs57k <= _empty_/qs57k#
         |[0:11..0:11):  <= _empty_/qs57k#`<init>`().
         |[0:12..0:13): l <= _empty_/qs57k#`<init>`().(l)
         |[0:12..0:13): l <= _empty_/qs57k#l.
         |[0:15..0:18): Any => scala/Any#
         |[1:7..1:11): qs57 <= _empty_/qs57.
         |[2:9..2:14): scala => scala/
         |[2:15..2:25): collection => scala/collection/
         |[2:26..2:35): immutable => scala/collection/immutable/
         |[3:9..3:10): o <= _empty_/qs57.o.
         |[3:17..3:21): List <= _empty_/qs57.o.List.
         |[3:30..3:34): List => scala/collection/immutable/List.
         |[4:6..4:8): a1 <= _empty_/qs57.a1.
         |[4:22..4:27): apply => scala/collection/immutable/Nil.
         |[5:6..5:8): a2 <= _empty_/qs57.a2.
         |[5:26..5:31): apply => scala/collection/immutable/Nil.
         |[6:6..6:8): a3 <= _empty_/qs57.a3.
         |[6:18..6:23): apply => scala/collection/immutable/Nil.
         |[7:6..7:8): f1 <= _empty_/qs57.f1.
         |[7:10..7:25): PartialFunction => scala/PartialFunction#
         |[7:26..7:29): Int => scala/Int#
         |[7:31..7:34): Any => scala/Any#
         |[7:38..7:39): x <= local7
         |[7:38..7:83): x => x match { case _ => scala.List.apply() } <= local0
         |[7:43..7:44): x => local7
         |[7:74..7:79): apply => scala/collection/immutable/Nil.
         |[8:6..8:8): f2 <= _empty_/qs57.f2.
         |[8:10..8:25): PartialFunction => scala/PartialFunction#
         |[8:26..8:29): Int => scala/Int#
         |[8:31..8:34): Any => scala/Any#
         |[8:38..8:39): x <= local15
         |[8:38..8:87): x => x match { case _ => immutable.List.apply() } <= local8
         |[8:43..8:44): x => local15
         |[8:78..8:83): apply => scala/collection/immutable/Nil.
         |[9:6..9:8): f3 <= _empty_/qs57.f3.
         |[9:10..9:25): PartialFunction => scala/PartialFunction#
         |[9:26..9:29): Int => scala/Int#
         |[9:31..9:34): Any => scala/Any#
         |[9:38..9:39): x <= local23
         |[9:38..9:79): x => x match { case _ => o.List.apply() } <= local16
         |[9:43..9:44): x => local23
         |[9:70..9:75): apply => scala/collection/immutable/Nil.
         |[10:6..10:8): g1 <= _empty_/qs57.g1.
         |[10:15..10:20): qs57k => _empty_/qs57k#
         |[10:20..10:20):  => _empty_/qs57k#`<init>`().
         |[10:32..10:37): apply => scala/collection/immutable/Nil.
         |[11:6..11:8): g2 <= _empty_/qs57.g2.
         |[11:15..11:20): qs57k => _empty_/qs57k#
         |[11:20..11:20):  => _empty_/qs57k#`<init>`().
         |[11:36..11:41): apply => scala/collection/immutable/Nil.
         |[12:6..12:8): g3 <= _empty_/qs57.g3.
         |[12:15..12:20): qs57k => _empty_/qs57k#
         |[12:20..12:20):  => _empty_/qs57k#`<init>`().
         |[12:28..12:33): apply => scala/collection/immutable/Nil.
         |""".stripMargin
    else
      """|[0:6..0:11): qs57k <= _empty_/qs57k#
         |[0:11..0:11):  <= _empty_/qs57k#`<init>`().
         |[0:12..0:13): l <= _empty_/qs57k#`<init>`().(l)
         |[0:12..0:13): l <= _empty_/qs57k#l.
         |[0:15..0:18): Any => scala/Any#
         |[1:7..1:11): qs57 <= _empty_/qs57.
         |[2:9..2:14): scala => scala/
         |[2:15..2:25): collection => scala/collection/
         |[2:26..2:35): immutable => scala/collection/immutable/
         |[3:9..3:10): o <= _empty_/qs57.o.
         |[3:17..3:21): List <= _empty_/qs57.o.List.
         |[3:24..3:29): scala => scala/
         |[3:30..3:34): List => scala/package.List.
         |[4:6..4:8): a1 <= _empty_/qs57.a1.
         |[4:22..4:27): apply => scala/collection/immutable/Nil.
         |[5:6..5:8): a2 <= _empty_/qs57.a2.
         |[5:26..5:31): apply => scala/collection/immutable/Nil.
         |[6:6..6:8): a3 <= _empty_/qs57.a3.
         |[6:11..6:12): o => _empty_/qs57.o.
         |[6:13..6:17): List => _empty_/qs57.o.List.
         |[6:18..6:23): apply => scala/collection/IterableFactory#apply().
         |[7:6..7:8): f1 <= _empty_/qs57.f1.
         |[7:10..7:25): PartialFunction => scala/PartialFunction#
         |[7:26..7:29): Int => scala/Int#
         |[7:31..7:34): Any => scala/Any#
         |[7:38..7:39): x <= local7
         |[7:38..7:83): x => x match { case _ => scala.List.apply() } <= local0
         |[7:43..7:44): x => local7
         |[7:74..7:79): apply => scala/collection/immutable/Nil.
         |[8:6..8:8): f2 <= _empty_/qs57.f2.
         |[8:10..8:25): PartialFunction => scala/PartialFunction#
         |[8:26..8:29): Int => scala/Int#
         |[8:31..8:34): Any => scala/Any#
         |[8:38..8:39): x <= local15
         |[8:38..8:87): x => x match { case _ => immutable.List.apply() } <= local8
         |[8:43..8:44): x => local15
         |[8:78..8:83): apply => scala/collection/immutable/Nil.
         |[9:6..9:8): f3 <= _empty_/qs57.f3.
         |[9:10..9:25): PartialFunction => scala/PartialFunction#
         |[9:26..9:29): Int => scala/Int#
         |[9:31..9:34): Any => scala/Any#
         |[9:38..9:39): x <= local23
         |[9:38..9:79): x => x match { case _ => o.List.apply() } <= local16
         |[9:43..9:44): x => local23
         |[9:63..9:64): o => _empty_/qs57.o.
         |[9:65..9:69): List => _empty_/qs57.o.List.
         |[9:70..9:75): apply => scala/collection/IterableFactory#apply().
         |[10:6..10:8): g1 <= _empty_/qs57.g1.
         |[10:15..10:20): qs57k => _empty_/qs57k#
         |[10:20..10:20):  => _empty_/qs57k#`<init>`().
         |[10:32..10:37): apply => scala/collection/immutable/Nil.
         |[11:6..11:8): g2 <= _empty_/qs57.g2.
         |[11:15..11:20): qs57k => _empty_/qs57k#
         |[11:20..11:20):  => _empty_/qs57k#`<init>`().
         |[11:36..11:41): apply => scala/collection/immutable/Nil.
         |[12:6..12:8): g3 <= _empty_/qs57.g3.
         |[12:15..12:20): qs57k => _empty_/qs57k#
         |[12:20..12:20):  => _empty_/qs57k#`<init>`().
         |[12:21..12:22): o => _empty_/qs57.o.
         |[12:23..12:27): List => _empty_/qs57.o.List.
         |[12:28..12:33): apply => scala/collection/IterableFactory#apply().
         |""".stripMargin,
  )

  occurrences(
    "empty lists through an explicit apply without a qualifier and with a type argument",
    """|class qs60k(l: Any)
       |object qs60 {
       |  val a1 = List.apply()
       |  val a2 = List.apply[Int]()
       |  val a3 = scala.List.apply[Int]()
       |  val f1: PartialFunction[Int, Any] = x => x match { case _ => List.apply() }
       |  val f2: PartialFunction[Int, Any] = x => x match { case _ => List.apply[Int]() }
       |  val f3: PartialFunction[Int, Any] = x => x match { case _ => scala.List.apply[Int]() }
       |  val g1 = new qs60k(List.apply())
       |  val g2 = new qs60k(List.apply[Int]())
       |  val g3 = new qs60k(scala.List.apply[Int]())
       |}
       |""".stripMargin,
    """|[0:6..0:11): qs60k <= _empty_/qs60k#
       |[0:11..0:11):  <= _empty_/qs60k#`<init>`().
       |[0:12..0:13): l <= _empty_/qs60k#`<init>`().(l)
       |[0:12..0:13): l <= _empty_/qs60k#l.
       |[0:15..0:18): Any => scala/Any#
       |[1:7..1:11): qs60 <= _empty_/qs60.
       |[2:6..2:8): a1 <= _empty_/qs60.a1.
       |[2:16..2:21): apply => scala/collection/immutable/Nil.
       |[3:6..3:8): a2 <= _empty_/qs60.a2.
       |[3:16..3:21): apply => scala/collection/immutable/Nil.
       |[3:22..3:25): Int => scala/Int#
       |[4:6..4:8): a3 <= _empty_/qs60.a3.
       |[4:22..4:27): apply => scala/collection/immutable/Nil.
       |[4:28..4:31): Int => scala/Int#
       |[5:6..5:8): f1 <= _empty_/qs60.f1.
       |[5:10..5:25): PartialFunction => scala/PartialFunction#
       |[5:26..5:29): Int => scala/Int#
       |[5:31..5:34): Any => scala/Any#
       |[5:38..5:39): x <= local7
       |[5:38..5:77): x => x match { case _ => List.apply() } <= local0
       |[5:43..5:44): x => local7
       |[5:68..5:73): apply => scala/collection/immutable/Nil.
       |[6:6..6:8): f2 <= _empty_/qs60.f2.
       |[6:10..6:25): PartialFunction => scala/PartialFunction#
       |[6:26..6:29): Int => scala/Int#
       |[6:31..6:34): Any => scala/Any#
       |[6:38..6:39): x <= local15
       |[6:38..6:82): x => x match { case _ => List.apply[Int]() } <= local8
       |[6:43..6:44): x => local15
       |[6:68..6:73): apply => scala/collection/immutable/Nil.
       |[6:74..6:77): Int => scala/Int#
       |[7:6..7:8): f3 <= _empty_/qs60.f3.
       |[7:10..7:25): PartialFunction => scala/PartialFunction#
       |[7:26..7:29): Int => scala/Int#
       |[7:31..7:34): Any => scala/Any#
       |[7:38..7:39): x <= local23
       |[7:38..7:88): x => x match { case _ => scala.List.apply[Int]() } <= local16
       |[7:43..7:44): x => local23
       |[7:74..7:79): apply => scala/collection/immutable/Nil.
       |[7:80..7:83): Int => scala/Int#
       |[8:6..8:8): g1 <= _empty_/qs60.g1.
       |[8:15..8:20): qs60k => _empty_/qs60k#
       |[8:20..8:20):  => _empty_/qs60k#`<init>`().
       |[8:26..8:31): apply => scala/collection/immutable/Nil.
       |[9:6..9:8): g2 <= _empty_/qs60.g2.
       |[9:15..9:20): qs60k => _empty_/qs60k#
       |[9:20..9:20):  => _empty_/qs60k#`<init>`().
       |[9:26..9:31): apply => scala/collection/immutable/Nil.
       |[9:32..9:35): Int => scala/Int#
       |[10:6..10:8): g3 <= _empty_/qs60.g3.
       |[10:15..10:20): qs60k => _empty_/qs60k#
       |[10:20..10:20):  => _empty_/qs60k#`<init>`().
       |[10:32..10:37): apply => scala/collection/immutable/Nil.
       |[10:38..10:41): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "empty lists through an explicit apply without a qualifier, after an import of the immutable List",
    """|class qs74k(l: Any)
       |object qs74 {
       |  import scala.collection.immutable.List
       |  val a1 = List.apply()
       |  val a2 = List.apply[Int]()
       |  val b = List()
       |  val f1: PartialFunction[Int, Any] = x => x match { case _ => List.apply() }
       |  val g1 = new qs74k(List.apply())
       |}
       |""".stripMargin,
    """|[0:6..0:11): qs74k <= _empty_/qs74k#
       |[0:11..0:11):  <= _empty_/qs74k#`<init>`().
       |[0:12..0:13): l <= _empty_/qs74k#`<init>`().(l)
       |[0:12..0:13): l <= _empty_/qs74k#l.
       |[0:15..0:18): Any => scala/Any#
       |[1:7..1:11): qs74 <= _empty_/qs74.
       |[2:9..2:14): scala => scala/
       |[2:15..2:25): collection => scala/collection/
       |[2:26..2:35): immutable => scala/collection/immutable/
       |[2:36..2:40): List => scala/collection/immutable/List#
       |[2:36..2:40): List => scala/collection/immutable/List.
       |[3:6..3:8): a1 <= _empty_/qs74.a1.
       |[3:16..3:21): apply => scala/collection/immutable/Nil.
       |[4:6..4:8): a2 <= _empty_/qs74.a2.
       |[4:16..4:21): apply => scala/collection/immutable/Nil.
       |[4:22..4:25): Int => scala/Int#
       |[5:6..5:7): b <= _empty_/qs74.b.
       |[5:10..5:14): List => scala/collection/immutable/Nil.
       |[6:6..6:8): f1 <= _empty_/qs74.f1.
       |[6:10..6:25): PartialFunction => scala/PartialFunction#
       |[6:26..6:29): Int => scala/Int#
       |[6:31..6:34): Any => scala/Any#
       |[6:38..6:39): x <= local7
       |[6:38..6:77): x => x match { case _ => List.apply() } <= local0
       |[6:43..6:44): x => local7
       |[6:68..6:73): apply => scala/collection/immutable/Nil.
       |[7:6..7:8): g1 <= _empty_/qs74.g1.
       |[7:15..7:20): qs74k => _empty_/qs74k#
       |[7:20..7:20):  => _empty_/qs74k#`<init>`().
       |[7:26..7:31): apply => scala/collection/immutable/Nil.
       |""".stripMargin,
  )

  occurrences(
    "lists through the scala package",
    """|object qs29 {
       |  val a = scala.List(1)
       |  val b = scala.Seq(1)
       |  val c = scala.Nil
       |  val d = _root_.scala.List(1)
       |  import scala.{List => L}
       |  val e = L(1)
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): qs29 <= _empty_/qs29.
         |[1:6..1:7): a <= _empty_/qs29.a.
         |[1:16..1:20): List => scala/collection/immutable/List.
         |[2:6..2:7): b <= _empty_/qs29.b.
         |[2:16..2:19): Seq => scala/collection/Seq.
         |[3:6..3:7): c <= _empty_/qs29.c.
         |[3:16..3:19): Nil => scala/collection/immutable/Nil.
         |[4:6..4:7): d <= _empty_/qs29.d.
         |[4:23..4:27): List => scala/collection/immutable/List.
         |[5:9..5:14): scala => scala/
         |[5:16..5:20): List => scala/package.List#
         |[5:16..5:20): List => scala/package.List.
         |[6:6..6:7): e <= _empty_/qs29.e.
         |""".stripMargin
    else
      """|[0:7..0:11): qs29 <= _empty_/qs29.
         |[1:6..1:7): a <= _empty_/qs29.a.
         |[1:10..1:15): scala => scala/
         |[1:16..1:20): List => scala/package.List.
         |[2:6..2:7): b <= _empty_/qs29.b.
         |[2:10..2:15): scala => scala/
         |[2:16..2:19): Seq => scala/package.Seq.
         |[3:6..3:7): c <= _empty_/qs29.c.
         |[3:10..3:15): scala => scala/
         |[3:16..3:19): Nil => scala/package.Nil.
         |[4:6..4:7): d <= _empty_/qs29.d.
         |[4:17..4:22): scala => scala/
         |[4:23..4:27): List => scala/package.List.
         |[5:9..5:14): scala => scala/
         |[5:16..5:20): List => scala/package.List#
         |[5:16..5:20): List => scala/package.List.
         |[6:6..6:7): e <= _empty_/qs29.e.
         |[6:10..6:11): L => scala/package.List.
         |""".stripMargin,
  )

  occurrences(
    "qualifiers in a self type, an inferred parent and a context bound in a refinement",
    """|package qs30a { object o { trait Tr; class P[A](a: A); trait TC[A] } }
       |object qs30 {
       |  import qs30a.{o => r}
       |  class A { self: qs30a.o.Tr => }
       |  class B { this: r.Tr => }
       |  class C extends qs30a.o.P(1)
       |  class D extends r.P(1)
       |  val e = List[AnyRef { def m[A: qs30a.o.TC]: Int }]()
       |  val f = List[AnyRef { def m[A: r.TC]: Int }]()
       |}
       |""".stripMargin,
    """|[0:8..0:13): qs30a <= qs30a/
       |[0:23..0:24): o <= qs30a/o.
       |[0:33..0:35): Tr <= qs30a/o.Tr#
       |[0:43..0:44): P <= qs30a/o.P#
       |[0:45..0:46): A <= qs30a/o.P#[A]
       |[0:47..0:47):  <= qs30a/o.P#`<init>`().
       |[0:48..0:49): a <= qs30a/o.P#`<init>`().(a)
       |[0:48..0:49): a <= qs30a/o.P#a.
       |[0:51..0:52): A => qs30a/o.P#[A]
       |[0:61..0:63): TC <= qs30a/o.TC#
       |[0:64..0:65): A <= qs30a/o.TC#[A]
       |[1:7..1:11): qs30 <= _empty_/qs30.
       |[2:9..2:14): qs30a => qs30a/
       |[2:16..2:17): o => qs30a/o.
       |[3:8..3:9): A <= _empty_/qs30.A#
       |[3:10..3:10):  <= _empty_/qs30.A#`<init>`().
       |[3:12..3:16): self <= local0
       |[3:18..3:23): qs30a => qs30a/
       |[3:24..3:25): o => qs30a/o.
       |[3:26..3:28): Tr => qs30a/o.Tr#
       |[4:8..4:9): B <= _empty_/qs30.B#
       |[4:10..4:10):  <= _empty_/qs30.B#`<init>`().
       |[4:18..4:19): r => qs30a/o.
       |[4:20..4:22): Tr => qs30a/o.Tr#
       |[5:8..5:9): C <= _empty_/qs30.C#
       |[5:10..5:10):  <= _empty_/qs30.C#`<init>`().
       |[5:18..5:23): qs30a => qs30a/
       |[5:24..5:25): o => qs30a/o.
       |[5:26..5:27): P => qs30a/o.P#
       |[5:27..5:27):  => qs30a/o.P#`<init>`().
       |[6:8..6:9): D <= _empty_/qs30.D#
       |[6:10..6:10):  <= _empty_/qs30.D#`<init>`().
       |[6:18..6:19): r => qs30a/o.
       |[6:20..6:21): P => qs30a/o.P#
       |[6:21..6:21):  => qs30a/o.P#`<init>`().
       |[7:6..7:7): e <= _empty_/qs30.e.
       |[7:10..7:14): List => scala/collection/immutable/Nil.
       |[7:15..7:21): AnyRef => java/lang/Object#
       |[7:28..7:29): m <= local1
       |[7:30..7:31): A <= local3
       |[7:33..7:33):  <= local2
       |[7:33..7:38): qs30a => qs30a/
       |[7:39..7:40): o => qs30a/o.
       |[7:41..7:43): TC => qs30a/o.TC#
       |[7:46..7:49): Int => scala/Int#
       |[8:6..8:7): f <= _empty_/qs30.f.
       |[8:10..8:14): List => scala/collection/immutable/Nil.
       |[8:15..8:21): AnyRef => java/lang/Object#
       |[8:28..8:29): m <= local4
       |[8:30..8:31): A <= local6
       |[8:33..8:33):  <= local5
       |[8:33..8:34): r => qs30a/o.
       |[8:35..8:37): TC => qs30a/o.TC#
       |[8:40..8:43): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "qualifiers through this in a self type, an inferred parent and a context bound",
    """|class qs31 { s =>
       |  trait Tr; class P[A](a: A); trait TC[A]
       |  class A { self: qs31.this.Tr => }
       |  class B { this: s.Tr => }
       |  class C extends qs31.this.P(1)
       |  class D extends s.P(1)
       |  val e = List[AnyRef { def m[A: qs31.this.TC]: Int }]()
       |  val f = List[AnyRef { def m[A: s.TC]: Int }]()
       |}
       |""".stripMargin,
    """|[0:6..0:10): qs31 <= _empty_/qs31#
       |[0:11..0:11):  <= _empty_/qs31#`<init>`().
       |[0:13..0:14): s <= local0
       |[1:8..1:10): Tr <= _empty_/qs31#Tr#
       |[1:18..1:19): P <= _empty_/qs31#P#
       |[1:20..1:21): A <= _empty_/qs31#P#[A]
       |[1:22..1:22):  <= _empty_/qs31#P#`<init>`().
       |[1:23..1:24): a <= _empty_/qs31#P#`<init>`().(a)
       |[1:23..1:24): a <= _empty_/qs31#P#a.
       |[1:26..1:27): A => _empty_/qs31#P#[A]
       |[1:36..1:38): TC <= _empty_/qs31#TC#
       |[1:39..1:40): A <= _empty_/qs31#TC#[A]
       |[2:8..2:9): A <= _empty_/qs31#A#
       |[2:10..2:10):  <= _empty_/qs31#A#`<init>`().
       |[2:12..2:16): self <= local1
       |[2:18..2:22): qs31 => _empty_/qs31#
       |[2:28..2:30): Tr => _empty_/qs31#Tr#
       |[3:8..3:9): B <= _empty_/qs31#B#
       |[3:10..3:10):  <= _empty_/qs31#B#`<init>`().
       |[3:18..3:19): s => local0
       |[3:20..3:22): Tr => _empty_/qs31#Tr#
       |[4:8..4:9): C <= _empty_/qs31#C#
       |[4:10..4:10):  <= _empty_/qs31#C#`<init>`().
       |[4:28..4:29): P => _empty_/qs31#P#
       |[4:29..4:29):  => _empty_/qs31#P#`<init>`().
       |[5:8..5:9): D <= _empty_/qs31#D#
       |[5:10..5:10):  <= _empty_/qs31#D#`<init>`().
       |[5:18..5:19): s => local0
       |[5:20..5:21): P => _empty_/qs31#P#
       |[5:21..5:21):  => _empty_/qs31#P#`<init>`().
       |[6:6..6:7): e <= _empty_/qs31#e.
       |[6:10..6:14): List => scala/collection/immutable/Nil.
       |[6:15..6:21): AnyRef => java/lang/Object#
       |[6:28..6:29): m <= local2
       |[6:30..6:31): A <= local4
       |[6:33..6:33):  <= local3
       |[6:33..6:37): qs31 => _empty_/qs31#
       |[6:43..6:45): TC => _empty_/qs31#TC#
       |[6:48..6:51): Int => scala/Int#
       |[7:6..7:7): f <= _empty_/qs31#f.
       |[7:10..7:14): List => scala/collection/immutable/Nil.
       |[7:15..7:21): AnyRef => java/lang/Object#
       |[7:28..7:29): m <= local5
       |[7:30..7:31): A <= local7
       |[7:33..7:33):  <= local6
       |[7:33..7:34): s => local0
       |[7:35..7:37): TC => _empty_/qs31#TC#
       |[7:40..7:43): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "inferred parents through a type alias",
    """|package qs42a { object o { class P[A](a: A) }; object x { type A[B] = o.P[B] } }
       |object qs42 {
       |  import qs42a.{x => r}
       |  class C extends qs42a.x.A(1)
       |  class D extends r.A(1)
       |  class E extends qs42a.x.A[Int](1)
       |}
       |class qs42 {
       |  class P[A](a: A)
       |  type A[B] = P[B]
       |  class C extends qs42.this.A(1)
       |}
       |""".stripMargin,
    """|[0:8..0:13): qs42a <= qs42a/
       |[0:23..0:24): o <= qs42a/o.
       |[0:33..0:34): P <= qs42a/o.P#
       |[0:35..0:36): A <= qs42a/o.P#[A]
       |[0:37..0:37):  <= qs42a/o.P#`<init>`().
       |[0:38..0:39): a <= qs42a/o.P#`<init>`().(a)
       |[0:38..0:39): a <= qs42a/o.P#a.
       |[0:41..0:42): A => qs42a/o.P#[A]
       |[0:54..0:55): x <= qs42a/x.
       |[0:63..0:64): A <= qs42a/x.A#
       |[0:65..0:66): B <= qs42a/x.A#[B]
       |[0:70..0:71): o => qs42a/o.
       |[0:72..0:73): P => qs42a/o.P#
       |[0:74..0:75): B => qs42a/x.A#[B]
       |[1:7..1:11): qs42 <= _empty_/qs42.
       |[2:9..2:14): qs42a => qs42a/
       |[2:16..2:17): x => qs42a/x.
       |[3:8..3:9): C <= _empty_/qs42.C#
       |[3:10..3:10):  <= _empty_/qs42.C#`<init>`().
       |[3:18..3:23): qs42a => qs42a/
       |[3:24..3:25): x => qs42a/x.
       |[3:26..3:27): A => qs42a/x.A#
       |[3:27..3:27):  => qs42a/o.P#`<init>`().
       |[4:8..4:9): D <= _empty_/qs42.D#
       |[4:10..4:10):  <= _empty_/qs42.D#`<init>`().
       |[4:18..4:19): r => qs42a/x.
       |[4:20..4:21): A => qs42a/x.A#
       |[4:21..4:21):  => qs42a/o.P#`<init>`().
       |[5:8..5:9): E <= _empty_/qs42.E#
       |[5:10..5:10):  <= _empty_/qs42.E#`<init>`().
       |[5:18..5:23): qs42a => qs42a/
       |[5:24..5:25): x => qs42a/x.
       |[5:26..5:27): A => qs42a/x.A#
       |[5:28..5:31): Int => scala/Int#
       |[5:32..5:32):  => qs42a/o.P#`<init>`().
       |[7:6..7:10): qs42 <= _empty_/qs42#
       |[7:11..7:11):  <= _empty_/qs42#`<init>`().
       |[8:8..8:9): P <= _empty_/qs42#P#
       |[8:10..8:11): A <= _empty_/qs42#P#[A]
       |[8:12..8:12):  <= _empty_/qs42#P#`<init>`().
       |[8:13..8:14): a <= _empty_/qs42#P#`<init>`().(a)
       |[8:13..8:14): a <= _empty_/qs42#P#a.
       |[8:16..8:17): A => _empty_/qs42#P#[A]
       |[9:7..9:8): A <= _empty_/qs42#A#
       |[9:9..9:10): B <= _empty_/qs42#A#[B]
       |[9:14..9:15): P => _empty_/qs42#P#
       |[9:16..9:17): B => _empty_/qs42#A#[B]
       |[10:8..10:9): C <= _empty_/qs42#C#
       |[10:10..10:10):  <= _empty_/qs42#C#`<init>`().
       |[10:28..10:29): A => _empty_/qs42#A#
       |[10:29..10:29):  => _empty_/qs42#P#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "inferred parents through super",
    """|class qs61a { class P[A](a: A) }
       |class qs61 extends qs61a {
       |  class K extends super.P(1)
       |  class L extends qs61.super.P(1)
       |  class M extends qs61.super[qs61a].P(1)
       |  class N extends super.P[Int](1)
       |  class I { class J extends qs61.super.P(1) }
       |  class H { class J extends qs61.super[qs61a].P(1) }
       |}
       |""".stripMargin,
    """|[0:6..0:11): qs61a <= _empty_/qs61a#
       |[0:12..0:12):  <= _empty_/qs61a#`<init>`().
       |[0:20..0:21): P <= _empty_/qs61a#P#
       |[0:22..0:23): A <= _empty_/qs61a#P#[A]
       |[0:24..0:24):  <= _empty_/qs61a#P#`<init>`().
       |[0:25..0:26): a <= _empty_/qs61a#P#`<init>`().(a)
       |[0:25..0:26): a <= _empty_/qs61a#P#a.
       |[0:28..0:29): A => _empty_/qs61a#P#[A]
       |[1:6..1:10): qs61 <= _empty_/qs61#
       |[1:11..1:11):  <= _empty_/qs61#`<init>`().
       |[1:19..1:24): qs61a => _empty_/qs61a#
       |[1:24..1:24):  => _empty_/qs61a#`<init>`().
       |[2:8..2:9): K <= _empty_/qs61#K#
       |[2:10..2:10):  <= _empty_/qs61#K#`<init>`().
       |[2:24..2:25): P => _empty_/qs61a#P#
       |[2:25..2:25):  => _empty_/qs61a#P#`<init>`().
       |[3:8..3:9): L <= _empty_/qs61#L#
       |[3:10..3:10):  <= _empty_/qs61#L#`<init>`().
       |[3:29..3:30): P => _empty_/qs61a#P#
       |[3:30..3:30):  => _empty_/qs61a#P#`<init>`().
       |[4:8..4:9): M <= _empty_/qs61#M#
       |[4:10..4:10):  <= _empty_/qs61#M#`<init>`().
       |[4:36..4:37): P => _empty_/qs61a#P#
       |[4:37..4:37):  => _empty_/qs61a#P#`<init>`().
       |[5:8..5:9): N <= _empty_/qs61#N#
       |[5:10..5:10):  <= _empty_/qs61#N#`<init>`().
       |[5:18..5:18):  => _empty_/qs61#
       |[5:23..5:23):  => _empty_/qs61a#
       |[5:24..5:25): P => _empty_/qs61a#P#
       |[5:26..5:29): Int => scala/Int#
       |[5:30..5:30):  => _empty_/qs61a#P#`<init>`().
       |[6:8..6:9): I <= _empty_/qs61#I#
       |[6:10..6:10):  <= _empty_/qs61#I#`<init>`().
       |[6:18..6:19): J <= _empty_/qs61#I#J#
       |[6:20..6:20):  <= _empty_/qs61#I#J#`<init>`().
       |[6:39..6:40): P => _empty_/qs61a#P#
       |[6:40..6:40):  => _empty_/qs61a#P#`<init>`().
       |[7:8..7:9): H <= _empty_/qs61#H#
       |[7:10..7:10):  <= _empty_/qs61#H#`<init>`().
       |[7:18..7:19): J <= _empty_/qs61#H#J#
       |[7:20..7:20):  <= _empty_/qs61#H#J#`<init>`().
       |[7:46..7:47): P => _empty_/qs61a#P#
       |[7:47..7:47):  => _empty_/qs61a#P#`<init>`().
       |""".stripMargin,
  )

}

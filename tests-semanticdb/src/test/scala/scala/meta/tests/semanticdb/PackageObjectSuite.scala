package scala.meta.tests.semanticdb

class PackageObjectSuite extends SemanticdbSuite {

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  occurrences(
    "a type alias of a package object in classOf and type arguments",
    """|package object po1p { type T = Int }
       |object po1 {
       |  val a = classOf[po1p.T]
       |  val b = List[po1p.T]()
       |  val z: po1p.T = 1
       |}
       |""".stripMargin,
    """|[0:15..0:19): po1p <= po1p/package.
       |[0:27..0:28): T <= po1p/package.T#
       |[0:31..0:34): Int => scala/Int#
       |[1:7..1:10): po1 <= _empty_/po1.
       |[2:6..2:7): a <= _empty_/po1.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:22): po1p => po1p/package.
       |[2:23..2:24): T => po1p/package.T#
       |[3:6..3:7): b <= _empty_/po1.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:19): po1p => po1p/package.
       |[3:20..3:21): T => po1p/package.T#
       |[4:6..4:7): z <= _empty_/po1.z.
       |[4:9..4:13): po1p => po1p/
       |[4:14..4:15): T => po1p/package.T#
       |""".stripMargin,
  )

  occurrences(
    "a type alias with a type parameter of a package object in classOf and type arguments",
    """|package object po2p { type L[A] = List[A] }
       |object po2 {
       |  val a = classOf[po2p.L[Int]]
       |  val b = List[po2p.L[Int]]()
       |  val z: po2p.L[Int] = Nil
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
          """|[0:15..0:19): po2p <= po2p/package.
             |[0:27..0:28): L <= po2p/package.L#
             |[0:29..0:30): A <= po2p/package.L#[A]
             |[0:34..0:38): List => scala/package.List#
             |[0:39..0:40): A => po2p/package.L#[A]
             |[1:7..1:10): po2 <= _empty_/po2.
             |[2:6..2:7): a <= _empty_/po2.a.
             |[2:10..2:17): classOf => scala/Predef.classOf().
             |[2:18..2:22): po2p => po2p/package.
             |[2:23..2:24): L => po2p/package.L#
             |[2:25..2:28): Int => scala/Int#
             |[3:6..3:7): b <= _empty_/po2.b.
             |[3:10..3:14): List => scala/collection/immutable/Nil.
             |[3:15..3:19): po2p => po2p/package.
             |[3:20..3:21): L => po2p/package.L#
             |[3:22..3:25): Int => scala/Int#
             |[4:6..4:7): z <= _empty_/po2.z.
             |[4:9..4:13): po2p => po2p/
             |[4:14..4:15): L => po2p/package.L#
             |[4:16..4:19): Int => scala/Int#
             |[4:23..4:26): Nil => scala/collection/immutable/Nil.
             |""".stripMargin
      },
      """|[0:15..0:19): po2p <= po2p/package.
         |[0:27..0:28): L <= po2p/package.L#
         |[0:29..0:30): A <= po2p/package.L#[A]
         |[0:34..0:38): List => scala/package.List#
         |[0:39..0:40): A => po2p/package.L#[A]
         |[1:7..1:10): po2 <= _empty_/po2.
         |[2:6..2:7): a <= _empty_/po2.a.
         |[2:10..2:17): classOf => scala/Predef.classOf().
         |[2:18..2:22): po2p => po2p/package.
         |[2:23..2:24): L => po2p/package.L#
         |[2:25..2:28): Int => scala/Int#
         |[3:6..3:7): b <= _empty_/po2.b.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:19): po2p => po2p/package.
         |[3:20..3:21): L => po2p/package.L#
         |[3:22..3:25): Int => scala/Int#
         |[4:6..4:7): z <= _empty_/po2.z.
         |[4:9..4:13): po2p => po2p/
         |[4:14..4:15): L => po2p/package.L#
         |[4:16..4:19): Int => scala/Int#
         |[4:23..4:26): Nil => scala/package.Nil.
         |""".stripMargin,
    ),
  )

  occurrences(
    "a class of a package object in classOf and type arguments",
    """|package object po3p { class C }
       |object po3 {
       |  val a = classOf[po3p.C]
       |  val b = List[po3p.C]()
       |  val z: po3p.C = null
       |}
       |""".stripMargin,
    """|[0:15..0:19): po3p <= po3p/package.
       |[0:28..0:29): C <= po3p/package.C#
       |[0:29..0:29):  <= po3p/package.C#`<init>`().
       |[1:7..1:10): po3 <= _empty_/po3.
       |[2:6..2:7): a <= _empty_/po3.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:22): po3p => po3p/package.
       |[2:23..2:24): C => po3p/package.C#
       |[3:6..3:7): b <= _empty_/po3.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:19): po3p => po3p/package.
       |[3:20..3:21): C => po3p/package.C#
       |[4:6..4:7): z <= _empty_/po3.z.
       |[4:9..4:13): po3p => po3p/
       |[4:14..4:15): C => po3p/package.C#
       |""".stripMargin,
  )

  occurrences(
    "a value of a package object as a singleton type in classOf and type arguments",
    """|package object po4p { val v = "" }
       |object po4 {
       |  val a = classOf[Option[po4p.v.type]]
       |  val b = List[po4p.v.type]()
       |  val z: po4p.v.type = po4p.v
       |}
       |""".stripMargin,
    """|[0:15..0:19): po4p <= po4p/package.
       |[0:26..0:27): v <= po4p/package.v.
       |[1:7..1:10): po4 <= _empty_/po4.
       |[2:6..2:7): a <= _empty_/po4.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:29): po4p => po4p/package.
       |[2:30..2:31): v => po4p/package.v.
       |[3:6..3:7): b <= _empty_/po4.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:19): po4p => po4p/package.
       |[3:20..3:21): v => po4p/package.v.
       |[4:6..4:7): z <= _empty_/po4.z.
       |[4:9..4:13): po4p => po4p/
       |[4:14..4:15): v => po4p/package.v.
       |[4:23..4:27): po4p => po4p/
       |[4:28..4:29): v => po4p/package.v.
       |""".stripMargin,
  )

  occurrences(
    "an object of a package object as a singleton type in classOf and type arguments",
    """|package object po5p { object O }
       |object po5 {
       |  val a = classOf[Option[po5p.O.type]]
       |  val b = List[po5p.O.type]()
       |  val z: po5p.O.type = po5p.O
       |}
       |""".stripMargin,
    """|[0:15..0:19): po5p <= po5p/package.
       |[0:29..0:30): O <= po5p/package.O.
       |[1:7..1:10): po5 <= _empty_/po5.
       |[2:6..2:7): a <= _empty_/po5.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:29): po5p => po5p/package.
       |[2:30..2:31): O => po5p/package.O.
       |[3:6..3:7): b <= _empty_/po5.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:19): po5p => po5p/package.
       |[3:20..3:21): O => po5p/package.O.
       |[4:6..4:7): z <= _empty_/po5.z.
       |[4:9..4:13): po5p => po5p/
       |[4:14..4:15): O => po5p/package.O.
       |[4:23..4:27): po5p => po5p/
       |[4:28..4:29): O => po5p/package.O.
       |""".stripMargin,
  )

  occurrences(
    "members of the package object of a nested package in classOf and type arguments",
    """|package po6a { package object po6b { type T = Int; class C } }
       |object po6 {
       |  val a = classOf[po6a.po6b.T]
       |  val b = List[po6a.po6b.T]()
       |  val c = classOf[po6a.po6b.C]
       |  val d = List[po6a.po6b.C]()
       |  val z: po6a.po6b.T = 1
       |  val y: po6a.po6b.C = null
       |}
       |""".stripMargin,
    """|[0:8..0:12): po6a <= po6a/
       |[0:30..0:34): po6b <= po6a/po6b/package.
       |[0:42..0:43): T <= po6a/po6b/package.T#
       |[0:46..0:49): Int => scala/Int#
       |[0:57..0:58): C <= po6a/po6b/package.C#
       |[0:58..0:58):  <= po6a/po6b/package.C#`<init>`().
       |[1:7..1:10): po6 <= _empty_/po6.
       |[2:6..2:7): a <= _empty_/po6.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:22): po6a => po6a/po6b/
       |[2:23..2:27): po6b => po6a/po6b/package.
       |[2:28..2:29): T => po6a/po6b/package.T#
       |[3:6..3:7): b <= _empty_/po6.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:19): po6a => po6a/po6b/
       |[3:20..3:24): po6b => po6a/po6b/package.
       |[3:25..3:26): T => po6a/po6b/package.T#
       |[4:6..4:7): c <= _empty_/po6.c.
       |[4:10..4:17): classOf => scala/Predef.classOf().
       |[4:18..4:22): po6a => po6a/po6b/
       |[4:23..4:27): po6b => po6a/po6b/package.
       |[4:28..4:29): C => po6a/po6b/package.C#
       |[5:6..5:7): d <= _empty_/po6.d.
       |[5:10..5:14): List => scala/collection/immutable/Nil.
       |[5:15..5:19): po6a => po6a/po6b/
       |[5:20..5:24): po6b => po6a/po6b/package.
       |[5:25..5:26): C => po6a/po6b/package.C#
       |[6:6..6:7): z <= _empty_/po6.z.
       |[6:9..6:13): po6a => po6a/
       |[6:14..6:18): po6b => po6a/po6b/
       |[6:19..6:20): T => po6a/po6b/package.T#
       |[7:6..7:7): y <= _empty_/po6.y.
       |[7:9..7:13): po6a => po6a/
       |[7:14..7:18): po6b => po6a/po6b/
       |[7:19..7:20): C => po6a/po6b/package.C#
       |""".stripMargin,
  )

  occurrences(
    "members of a package object through a renamed import of its package in classOf and type arguments",
    """|package po7a { package object po7b { type T = Int; class C } }
       |object po7 {
       |  import po7a.{po7b => q}
       |  val a = classOf[q.T]
       |  val b = List[q.T]()
       |  val c = classOf[q.C]
       |  val d = List[q.C]()
       |  val z: q.T = 1
       |  val y: q.C = null
       |}
       |""".stripMargin,
    """|[0:8..0:12): po7a <= po7a/
       |[0:30..0:34): po7b <= po7a/po7b/package.
       |[0:42..0:43): T <= po7a/po7b/package.T#
       |[0:46..0:49): Int => scala/Int#
       |[0:57..0:58): C <= po7a/po7b/package.C#
       |[0:58..0:58):  <= po7a/po7b/package.C#`<init>`().
       |[1:7..1:10): po7 <= _empty_/po7.
       |[2:9..2:13): po7a => po7a/
       |[2:15..2:19): po7b => po7a/po7b/
       |[3:6..3:7): a <= _empty_/po7.a.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:19): q => po7a/po7b/package.
       |[3:20..3:21): T => po7a/po7b/package.T#
       |[4:6..4:7): b <= _empty_/po7.b.
       |[4:10..4:14): List => scala/collection/immutable/Nil.
       |[4:15..4:16): q => po7a/po7b/package.
       |[4:17..4:18): T => po7a/po7b/package.T#
       |[5:6..5:7): c <= _empty_/po7.c.
       |[5:10..5:17): classOf => scala/Predef.classOf().
       |[5:18..5:19): q => po7a/po7b/package.
       |[5:20..5:21): C => po7a/po7b/package.C#
       |[6:6..6:7): d <= _empty_/po7.d.
       |[6:10..6:14): List => scala/collection/immutable/Nil.
       |[6:15..6:16): q => po7a/po7b/package.
       |[6:17..6:18): C => po7a/po7b/package.C#
       |[7:6..7:7): z <= _empty_/po7.z.
       |[7:9..7:10): q => po7a/po7b/
       |[7:11..7:12): T => po7a/po7b/package.T#
       |[8:6..8:7): y <= _empty_/po7.y.
       |[8:9..8:10): q => po7a/po7b/
       |[8:11..8:12): C => po7a/po7b/package.C#
       |""".stripMargin,
  )

  occurrences(
    "members of a package object through _root_ in classOf and type arguments",
    """|package object po8p { type T = Int; class C }
       |object po8 {
       |  val a = classOf[_root_.po8p.T]
       |  val b = List[_root_.po8p.T]()
       |  val c = classOf[_root_.po8p.C]
       |  val d = List[_root_.po8p.C]()
       |  val z: _root_.po8p.T = 1
       |  val y: _root_.po8p.C = null
       |}
       |""".stripMargin,
    """|[0:15..0:19): po8p <= po8p/package.
       |[0:27..0:28): T <= po8p/package.T#
       |[0:31..0:34): Int => scala/Int#
       |[0:42..0:43): C <= po8p/package.C#
       |[0:43..0:43):  <= po8p/package.C#`<init>`().
       |[1:7..1:10): po8 <= _empty_/po8.
       |[2:6..2:7): a <= _empty_/po8.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): _root_ => po8p/
       |[2:25..2:29): po8p => po8p/package.
       |[2:30..2:31): T => po8p/package.T#
       |[3:6..3:7): b <= _empty_/po8.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:21): _root_ => po8p/
       |[3:22..3:26): po8p => po8p/package.
       |[3:27..3:28): T => po8p/package.T#
       |[4:6..4:7): c <= _empty_/po8.c.
       |[4:10..4:17): classOf => scala/Predef.classOf().
       |[4:18..4:24): _root_ => po8p/
       |[4:25..4:29): po8p => po8p/package.
       |[4:30..4:31): C => po8p/package.C#
       |[5:6..5:7): d <= _empty_/po8.d.
       |[5:10..5:14): List => scala/collection/immutable/Nil.
       |[5:15..5:21): _root_ => po8p/
       |[5:22..5:26): po8p => po8p/package.
       |[5:27..5:28): C => po8p/package.C#
       |[6:6..6:7): z <= _empty_/po8.z.
       |[6:16..6:20): po8p => po8p/
       |[6:21..6:22): T => po8p/package.T#
       |[7:6..7:7): y <= _empty_/po8.y.
       |[7:16..7:20): po8p => po8p/
       |[7:21..7:22): C => po8p/package.C#
       |""".stripMargin,
  )

  occurrences(
    "members of a package object through an explicit package in classOf and type arguments",
    """|package object po9p { type T = Int; class C }
       |object po9 {
       |  val a = classOf[po9p.`package`.T]
       |  val b = List[po9p.`package`.T]()
       |  val c = classOf[po9p.`package`.C]
       |  val d = List[po9p.`package`.C]()
       |  val z: po9p.`package`.T = 1
       |  val y: po9p.`package`.C = null
       |}
       |""".stripMargin,
    """|[0:15..0:19): po9p <= po9p/package.
       |[0:27..0:28): T <= po9p/package.T#
       |[0:31..0:34): Int => scala/Int#
       |[0:42..0:43): C <= po9p/package.C#
       |[0:43..0:43):  <= po9p/package.C#`<init>`().
       |[1:7..1:10): po9 <= _empty_/po9.
       |[2:6..2:7): a <= _empty_/po9.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:22): po9p => po9p/
       |[2:23..2:32): `package` => po9p/package.
       |[2:33..2:34): T => po9p/package.T#
       |[3:6..3:7): b <= _empty_/po9.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:19): po9p => po9p/
       |[3:20..3:29): `package` => po9p/package.
       |[3:30..3:31): T => po9p/package.T#
       |[4:6..4:7): c <= _empty_/po9.c.
       |[4:10..4:17): classOf => scala/Predef.classOf().
       |[4:18..4:22): po9p => po9p/
       |[4:23..4:32): `package` => po9p/package.
       |[4:33..4:34): C => po9p/package.C#
       |[5:6..5:7): d <= _empty_/po9.d.
       |[5:10..5:14): List => scala/collection/immutable/Nil.
       |[5:15..5:19): po9p => po9p/
       |[5:20..5:29): `package` => po9p/package.
       |[5:30..5:31): C => po9p/package.C#
       |[6:6..6:7): z <= _empty_/po9.z.
       |[6:9..6:13): po9p => po9p/
       |[6:14..6:23): `package` => po9p/package.
       |[6:24..6:25): T => po9p/package.T#
       |[7:6..7:7): y <= _empty_/po9.y.
       |[7:9..7:13): po9p => po9p/
       |[7:14..7:23): `package` => po9p/package.
       |[7:24..7:25): C => po9p/package.C#
       |""".stripMargin,
  )

  occurrences(
    "members of a package object in a refinement in classOf and type arguments",
    """|package object po10p { type T = Int; class C; val v = "" }
       |object po10 {
       |  val a = classOf[Option[AnyRef { def m: po10p.T; val w: po10p.C; type U <: po10p.T }]]
       |  val b = List[AnyRef { def m: po10p.T; def n(x: po10p.v.type): po10p.C }]()
       |  val z: AnyRef { def m: po10p.T; val w: po10p.C; type U <: po10p.T } = null
       |}
       |""".stripMargin,
    """|[0:15..0:20): po10p <= po10p/package.
       |[0:28..0:29): T <= po10p/package.T#
       |[0:32..0:35): Int => scala/Int#
       |[0:43..0:44): C <= po10p/package.C#
       |[0:44..0:44):  <= po10p/package.C#`<init>`().
       |[0:50..0:51): v <= po10p/package.v.
       |[1:7..1:11): po10 <= _empty_/po10.
       |[2:6..2:7): a <= _empty_/po10.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:31): AnyRef => scala/AnyRef#
       |[2:38..2:39): m <= local0
       |[2:41..2:46): po10p => po10p/package.
       |[2:47..2:48): T => po10p/package.T#
       |[2:54..2:55): w <= local1
       |[2:57..2:62): po10p => po10p/package.
       |[2:63..2:64): C => po10p/package.C#
       |[2:71..2:72): U <= local2
       |[2:76..2:81): po10p => po10p/package.
       |[2:82..2:83): T => po10p/package.T#
       |[3:6..3:7): b <= _empty_/po10.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:21): AnyRef => java/lang/Object#
       |[3:28..3:29): m <= local3
       |[3:31..3:36): po10p => scala/
       |[3:37..3:38): T => scala/Int#
       |[3:44..3:45): n <= local4
       |[3:46..3:47): x <= local5
       |[3:49..3:54): po10p => po10p/package.
       |[3:55..3:56): v => po10p/package.v.
       |[3:64..3:69): po10p => po10p/package.
       |[3:70..3:71): C => po10p/package.C#
       |[4:6..4:7): z <= _empty_/po10.z.
       |[4:9..4:15): AnyRef => scala/AnyRef#
       |[4:22..4:23): m <= local6
       |[4:25..4:30): po10p => po10p/
       |[4:31..4:32): T => po10p/package.T#
       |[4:38..4:39): w <= local7
       |[4:41..4:46): po10p => po10p/
       |[4:47..4:48): C => po10p/package.C#
       |[4:55..4:56): U <= local8
       |[4:60..4:65): po10p => po10p/
       |[4:66..4:67): T => po10p/package.T#
       |""".stripMargin,
  )

  occurrences(
    "members of a package object in a forSome in classOf, type arguments and a written type",
    """|package object po11p { type T = Int; class C }
       |object po11 {
       |  val a = classOf[List[X] forSome { type X <: po11p.T }]
       |  val b = classOf[Option[v.type] forSome { val v: po11p.C }]
       |  val c = List[Option[v.type] forSome { val v: po11p.C }]()
       |  val z: Option[v.type] forSome { val v: po11p.C } = None
       |}
       |""".stripMargin,
    """|[0:15..0:20): po11p <= po11p/package.
       |[0:28..0:29): T <= po11p/package.T#
       |[0:32..0:35): Int => scala/Int#
       |[0:43..0:44): C <= po11p/package.C#
       |[0:44..0:44):  <= po11p/package.C#`<init>`().
       |[1:7..1:11): po11 <= _empty_/po11.
       |[2:6..2:7): a <= _empty_/po11.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:22): List => scala/package.List#
       |[2:23..2:24): X => local0
       |[2:41..2:42): X <= local0
       |[2:46..2:51): po11p => po11p/package.
       |[2:52..2:53): T => po11p/package.T#
       |[3:6..3:7): b <= _empty_/po11.b.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:24): Option => scala/Option#
       |[3:25..3:26): v => local1
       |[3:47..3:48): v <= local1
       |[3:50..3:55): po11p => po11p/package.
       |[3:56..3:57): C => po11p/package.C#
       |[4:6..4:7): c <= _empty_/po11.c.
       |[4:10..4:14): List => scala/collection/immutable/Nil.
       |[4:15..4:21): Option => scala/Option#
       |[4:22..4:23): v => local3
       |[4:44..4:45): v <= local3
       |[4:47..4:52): po11p => po11p/package.
       |[4:53..4:54): C => po11p/package.C#
       |[5:6..5:7): z <= _empty_/po11.z.
       |[5:9..5:15): Option => scala/Option#
       |[5:16..5:17): v => local5
       |[5:38..5:39): v <= local5
       |[5:41..5:46): po11p => po11p/package.
       |[5:47..5:48): C => po11p/package.C#
       |[5:53..5:57): None => scala/None.
       |""".stripMargin,
  )

  occurrences(
    "an annotation of a package object in classOf and type arguments",
    """|package object po12p { class ann extends scala.annotation.StaticAnnotation }
       |object po12 {
       |  val a = classOf[List[Int @po12p.ann]]
       |  val b = List[Int @po12p.ann]()
       |  val z: Int @po12p.ann = 1
       |}
       |""".stripMargin,
    """|[0:15..0:20): po12p <= po12p/package.
       |[0:29..0:32): ann <= po12p/package.ann#
       |[0:33..0:33):  <= po12p/package.ann#`<init>`().
       |[0:41..0:46): scala => scala/
       |[0:47..0:57): annotation => scala/annotation/
       |[0:58..0:74): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[0:74..0:74):  => scala/annotation/Annotation#`<init>`().
       |[1:7..1:11): po12 <= _empty_/po12.
       |[2:6..2:7): a <= _empty_/po12.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:22): List => scala/package.List#
       |[2:23..2:26): Int => scala/Int#
       |[2:28..2:33): po12p => po12p/package.
       |[2:34..2:37): ann => po12p/package.ann#
       |[3:6..3:7): b <= _empty_/po12.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:18): Int => scala/Int#
       |[3:20..3:25): po12p => po12p/package.
       |[3:26..3:29): ann => po12p/package.ann#
       |[4:6..4:7): z <= _empty_/po12.z.
       |[4:9..4:12): Int => scala/Int#
       |[4:14..4:19): po12p => po12p/
       |[4:20..4:23): ann => po12p/package.ann#
       |[4:23..4:23):  => po12p/package.ann#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "a self type from a package object",
    """|package object po13p { trait Tr; type A = Tr }
       |package po13a { package object po13b { trait Tr } }
       |class po13 { this: po13p.Tr => }
       |class po13x { this: po13p.A => }
       |class po13y { this: po13a.po13b.Tr => }
       |""".stripMargin,
    """|[0:15..0:20): po13p <= po13p/package.
       |[0:29..0:31): Tr <= po13p/package.Tr#
       |[0:38..0:39): A <= po13p/package.A#
       |[0:42..0:44): Tr => po13p/package.Tr#
       |[1:8..1:13): po13a <= po13a/
       |[1:31..1:36): po13b <= po13a/po13b/package.
       |[1:45..1:47): Tr <= po13a/po13b/package.Tr#
       |[2:6..2:10): po13 <= _empty_/po13#
       |[2:11..2:11):  <= _empty_/po13#`<init>`().
       |[2:19..2:24): po13p => po13p/
       |[2:25..2:27): Tr => po13p/package.Tr#
       |[3:6..3:11): po13x <= _empty_/po13x#
       |[3:12..3:12):  <= _empty_/po13x#`<init>`().
       |[3:20..3:25): po13p => po13p/
       |[3:26..3:27): A => po13p/package.A#
       |[4:6..4:11): po13y <= _empty_/po13y#
       |[4:12..4:12):  <= _empty_/po13y#`<init>`().
       |[4:20..4:25): po13a => po13a/
       |[4:26..4:31): po13b => po13a/po13b/
       |[4:32..4:34): Tr => po13a/po13b/package.Tr#
       |""".stripMargin,
  )

  occurrences(
    "a parent from a package object with an inferred type argument",
    """|package object po14p { class P[A](a: A); type Q[A] = P[A] }
       |class po14 extends po14p.P(1)
       |class po14x extends po14p.Q(1)
       |""".stripMargin,
    """|[0:15..0:20): po14p <= po14p/package.
       |[0:29..0:30): P <= po14p/package.P#
       |[0:31..0:32): A <= po14p/package.P#[A]
       |[0:33..0:33):  <= po14p/package.P#`<init>`().
       |[0:34..0:35): a <= po14p/package.P#`<init>`().(a)
       |[0:34..0:35): a <= po14p/package.P#a.
       |[0:37..0:38): A => po14p/package.P#[A]
       |[0:46..0:47): Q <= po14p/package.Q#
       |[0:48..0:49): A <= po14p/package.Q#[A]
       |[0:53..0:54): P => po14p/package.P#
       |[0:55..0:56): A => po14p/package.Q#[A]
       |[1:6..1:10): po14 <= _empty_/po14#
       |[1:11..1:11):  <= _empty_/po14#`<init>`().
       |[1:19..1:24): po14p => po14p/
       |[1:25..1:26): P => po14p/package.P#
       |[1:26..1:26):  => po14p/package.P#`<init>`().
       |[2:6..2:11): po14x <= _empty_/po14x#
       |[2:12..2:12):  <= _empty_/po14x#`<init>`().
       |[2:20..2:25): po14p => po14p/
       |[2:26..2:27): Q => po14p/package.Q#
       |[2:27..2:27):  => po14p/package.P#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "a context bound from a package object in a refinement in classOf and type arguments",
    """|package object po15p { trait TC[A] }
       |object po15 {
       |  val a = classOf[Option[AnyRef { def m[A: po15p.TC](a: A): Int }]]
       |  val b = List[AnyRef { def m[A: po15p.TC](a: A): Int }]()
       |  val z: AnyRef { def m[A: po15p.TC](a: A): Int } = null
       |}
       |""".stripMargin,
    """|[0:15..0:20): po15p <= po15p/package.
       |[0:29..0:31): TC <= po15p/package.TC#
       |[0:32..0:33): A <= po15p/package.TC#[A]
       |[1:7..1:11): po15 <= _empty_/po15.
       |[2:6..2:7): a <= _empty_/po15.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:24): Option => scala/Option#
       |[2:25..2:31): AnyRef => scala/AnyRef#
       |[2:38..2:39): m <= local0
       |[2:40..2:41): A <= local3
       |[2:43..2:43):  <= local2
       |[2:43..2:48): po15p => po15p/package.
       |[2:49..2:51): TC => po15p/package.TC#
       |[2:53..2:54): a <= local1
       |[2:56..2:57): A => local3
       |[2:60..2:63): Int => scala/Int#
       |[3:6..3:7): b <= _empty_/po15.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[3:15..3:21): AnyRef => java/lang/Object#
       |[3:28..3:29): m <= local4
       |[3:30..3:31): A <= local7
       |[3:33..3:33):  <= local6
       |[3:33..3:38): po15p => po15p/package.
       |[3:39..3:41): TC => po15p/package.TC#
       |[3:43..3:44): a <= local5
       |[3:46..3:47): A => local7
       |[3:50..3:53): Int => scala/Int#
       |[4:6..4:7): z <= _empty_/po15.z.
       |[4:9..4:15): AnyRef => scala/AnyRef#
       |[4:22..4:23): m <= local8
       |[4:24..4:25): A <= local11
       |[4:27..4:27):  <= local10
       |[4:27..4:32): po15p => po15p/
       |[4:33..4:35): TC => po15p/package.TC#
       |[4:37..4:38): a <= local9
       |[4:40..4:41): A => local11
       |[4:44..4:47): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "a package object imported by its own name, renamed and not, in classOf and type arguments",
    """|package object po17p { type T = Int; class C }
       |object po17 {
       |  import po17p.{`package` => q}
       |  val a = classOf[q.C]
       |  val b = List[q.T]()
       |  val z: q.T = 1
       |}
       |object po17x {
       |  import po17p.`package`
       |  val a = classOf[`package`.C]
       |  val b = List[`package`.T]()
       |  val z: `package`.T = 1
       |}
       |""".stripMargin,
    """|[0:15..0:20): po17p <= po17p/package.
       |[0:28..0:29): T <= po17p/package.T#
       |[0:32..0:35): Int => scala/Int#
       |[0:43..0:44): C <= po17p/package.C#
       |[0:44..0:44):  <= po17p/package.C#`<init>`().
       |[1:7..1:11): po17 <= _empty_/po17.
       |[2:9..2:14): po17p => po17p/
       |[2:16..2:25): `package` => po17p/package.
       |[3:6..3:7): a <= _empty_/po17.a.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:19): q => po17p/package.
       |[3:20..3:21): C => po17p/package.C#
       |[4:6..4:7): b <= _empty_/po17.b.
       |[4:10..4:14): List => scala/collection/immutable/Nil.
       |[4:15..4:16): q => po17p/package.
       |[4:17..4:18): T => po17p/package.T#
       |[5:6..5:7): z <= _empty_/po17.z.
       |[5:9..5:10): q => po17p/package.
       |[5:11..5:12): T => po17p/package.T#
       |[7:7..7:12): po17x <= _empty_/po17x.
       |[8:9..8:14): po17p => po17p/
       |[8:15..8:24): `package` => po17p/package.
       |[9:6..9:7): a <= _empty_/po17x.a.
       |[9:10..9:17): classOf => scala/Predef.classOf().
       |[9:18..9:27): `package` => po17p/package.
       |[9:28..9:29): C => po17p/package.C#
       |[10:6..10:7): b <= _empty_/po17x.b.
       |[10:10..10:14): List => scala/collection/immutable/Nil.
       |[10:15..10:24): `package` => po17p/package.
       |[10:25..10:26): T => po17p/package.T#
       |[11:6..11:7): z <= _empty_/po17x.z.
       |[11:9..11:18): `package` => po17p/package.
       |[11:19..11:20): T => po17p/package.T#
       |""".stripMargin,
  )

  occurrences(
    "members that a package object inherits from a class in classOf and type arguments",
    """|package po18a {
       |  class Base { type T = Int; class C }
       |  package object po18p extends Base
       |}
       |object po18 {
       |  val a = classOf[po18a.po18p.C]
       |  val b = List[po18a.po18p.T]()
       |  val z: po18a.po18p.T = 1
       |}
       |""".stripMargin,
    """|[0:8..0:13): po18a <= po18a/
       |[1:8..1:12): Base <= po18a/Base#
       |[1:13..1:13):  <= po18a/Base#`<init>`().
       |[1:20..1:21): T <= po18a/Base#T#
       |[1:24..1:27): Int => scala/Int#
       |[1:35..1:36): C <= po18a/Base#C#
       |[1:36..1:36):  <= po18a/Base#C#`<init>`().
       |[2:17..2:22): po18p <= po18a/po18p/package.
       |[2:31..2:35): Base => po18a/Base#
       |[2:35..2:35):  => po18a/Base#`<init>`().
       |[4:7..4:11): po18 <= _empty_/po18.
       |[5:6..5:7): a <= _empty_/po18.a.
       |[5:10..5:17): classOf => scala/Predef.classOf().
       |[5:18..5:23): po18a => po18a/po18p/
       |[5:24..5:29): po18p => po18a/po18p/package.
       |[5:30..5:31): C => po18a/Base#C#
       |[6:6..6:7): b <= _empty_/po18.b.
       |[6:10..6:14): List => scala/collection/immutable/Nil.
       |[6:15..6:20): po18a => po18a/po18p/
       |[6:21..6:26): po18p => po18a/po18p/package.
       |[6:27..6:28): T => po18a/Base#T#
       |[7:6..7:7): z <= _empty_/po18.z.
       |[7:9..7:14): po18a => po18a/
       |[7:15..7:20): po18p => po18a/po18p/
       |[7:21..7:22): T => po18a/Base#T#
       |""".stripMargin,
  )

  occurrences(
    "members of the scala package object in classOf and type arguments",
    """|object po19 {
       |  val a = classOf[scala.Seq[Int]]
       |  val b = List[scala.List[Int]]()
       |  val c = List[scala.Nil.type]()
       |  val z: scala.Seq[Int] = Nil
       |  val y: scala.Nil.type = Nil
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): po19 <= _empty_/po19.
         |[1:6..1:7): a <= _empty_/po19.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:23): scala => scala/package.
         |[1:24..1:27): Seq => scala/package.Seq#
         |[1:28..1:31): Int => scala/Int#
         |[2:6..2:7): b <= _empty_/po19.b.
         |[2:10..2:14): List => scala/collection/immutable/Nil.
         |[2:15..2:20): scala => scala/package.
         |[2:21..2:25): List => scala/package.List#
         |[2:26..2:29): Int => scala/Int#
         |[3:6..3:7): c <= _empty_/po19.c.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:20): scala => scala/collection/immutable/
         |[3:21..3:24): Nil => scala/collection/immutable/Nil.
         |[4:6..4:7): z <= _empty_/po19.z.
         |[4:9..4:14): scala => scala/
         |[4:15..4:18): Seq => scala/package.Seq#
         |[4:19..4:22): Int => scala/Int#
         |[4:26..4:29): Nil => scala/collection/immutable/Nil.
         |[5:6..5:7): y <= _empty_/po19.y.
         |[5:15..5:18): Nil => scala/collection/immutable/Nil.
         |[5:26..5:29): Nil => scala/collection/immutable/Nil.
         |""".stripMargin
    else
      """|[0:7..0:11): po19 <= _empty_/po19.
         |[1:6..1:7): a <= _empty_/po19.a.
         |[1:10..1:17): classOf => scala/Predef.classOf().
         |[1:18..1:23): scala => scala/package.
         |[1:24..1:27): Seq => scala/package.Seq#
         |[1:28..1:31): Int => scala/Int#
         |[2:6..2:7): b <= _empty_/po19.b.
         |[2:10..2:14): List => scala/collection/immutable/Nil.
         |[2:15..2:20): scala => scala/package.
         |[2:21..2:25): List => scala/package.List#
         |[2:26..2:29): Int => scala/Int#
         |[3:6..3:7): c <= _empty_/po19.c.
         |[3:10..3:14): List => scala/collection/immutable/Nil.
         |[3:15..3:20): scala => scala/package.
         |[3:21..3:24): Nil => scala/package.Nil.
         |[4:6..4:7): z <= _empty_/po19.z.
         |[4:9..4:14): scala => scala/
         |[4:15..4:18): Seq => scala/package.Seq#
         |[4:19..4:22): Int => scala/Int#
         |[4:26..4:29): Nil => scala/package.Nil.
         |[5:6..5:7): y <= _empty_/po19.y.
         |[5:9..5:14): scala => scala/
         |[5:15..5:18): Nil => scala/package.Nil.
         |[5:26..5:29): Nil => scala/package.Nil.
         |""".stripMargin,
  )

  occurrences(
    "a class of a package object in classOf inside the package object and inside the package",
    """|package object po20p {
       |  class C
       |  val a = classOf[po20p.C]
       |  val z: po20p.C = null
       |}
       |package po20p {
       |  object o {
       |    val a = classOf[po20p.C]
       |    val b = List[po20p.C]()
       |    val z: po20p.C = null
       |  }
       |}
       |""".stripMargin,
    """|[0:15..0:20): po20p <= po20p/package.
       |[1:8..1:9): C <= po20p/package.C#
       |[1:9..1:9):  <= po20p/package.C#`<init>`().
       |[2:6..2:7): a <= po20p/package.a.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:23): po20p => po20p/package.
       |[2:24..2:25): C => po20p/package.C#
       |[3:6..3:7): z <= po20p/package.z.
       |[3:9..3:14): po20p => po20p/
       |[3:15..3:16): C => po20p/package.C#
       |[5:8..5:13): po20p <= po20p/
       |[6:9..6:10): o <= po20p/o.
       |[7:8..7:9): a <= po20p/o.a.
       |[7:12..7:19): classOf => scala/Predef.classOf().
       |[7:20..7:25): po20p => po20p/package.
       |[7:26..7:27): C => po20p/package.C#
       |[8:8..8:9): b <= po20p/o.b.
       |[8:12..8:16): List => scala/collection/immutable/Nil.
       |[8:17..8:22): po20p => po20p/package.
       |[8:23..8:24): C => po20p/package.C#
       |[9:8..9:9): z <= po20p/o.z.
       |[9:11..9:16): po20p => po20p/
       |[9:17..9:18): C => po20p/package.C#
       |""".stripMargin,
  )

  occurrences(
    "a renamed import in a constructor call of a class with a member of that name",
    """|package po21a { object o { class C }; package object po21b { class D } }
       |object po21 {
       |  import po21a.{o => q, po21b => r}
       |  class B(c: Class[_])
       |  class E extends B(classOf[q.C]) { def q = 1 }
       |  class F extends B(classOf[r.D]) { def r = 1 }
       |  class G extends B(classOf[q.C])
       |}
       |""".stripMargin,
    """|[0:8..0:13): po21a <= po21a/
       |[0:23..0:24): o <= po21a/o.
       |[0:33..0:34): C <= po21a/o.C#
       |[0:34..0:34):  <= po21a/o.C#`<init>`().
       |[0:53..0:58): po21b <= po21a/po21b/package.
       |[0:67..0:68): D <= po21a/po21b/package.D#
       |[0:68..0:68):  <= po21a/po21b/package.D#`<init>`().
       |[1:7..1:11): po21 <= _empty_/po21.
       |[2:9..2:14): po21a => po21a/
       |[2:16..2:17): o => po21a/o.
       |[2:24..2:29): po21b => po21a/po21b/
       |[3:8..3:9): B <= _empty_/po21.B#
       |[3:9..3:9):  <= _empty_/po21.B#`<init>`().
       |[3:10..3:11): c <= _empty_/po21.B#`<init>`().(c)
       |[3:10..3:11): c <= _empty_/po21.B#c.
       |[3:13..3:18): Class => scala/Predef.Class#
       |[3:19..3:20): _ <= local0
       |[4:8..4:9): E <= _empty_/po21.E#
       |[4:10..4:10):  <= _empty_/po21.E#`<init>`().
       |[4:18..4:19): B => _empty_/po21.B#
       |[4:19..4:19):  => _empty_/po21.B#`<init>`().
       |[4:20..4:27): classOf => scala/Predef.classOf().
       |[4:28..4:29): q => po21a/o.
       |[4:30..4:31): C => po21a/o.C#
       |[4:40..4:41): q <= _empty_/po21.E#q().
       |[5:8..5:9): F <= _empty_/po21.F#
       |[5:10..5:10):  <= _empty_/po21.F#`<init>`().
       |[5:18..5:19): B => _empty_/po21.B#
       |[5:19..5:19):  => _empty_/po21.B#`<init>`().
       |[5:20..5:27): classOf => scala/Predef.classOf().
       |[5:28..5:29): r => po21a/po21b/package.
       |[5:30..5:31): D => po21a/po21b/package.D#
       |[5:40..5:41): r <= _empty_/po21.F#r().
       |[6:8..6:9): G <= _empty_/po21.G#
       |[6:10..6:10):  <= _empty_/po21.G#`<init>`().
       |[6:18..6:19): B => _empty_/po21.B#
       |[6:19..6:19):  => _empty_/po21.B#`<init>`().
       |[6:20..6:27): classOf => scala/Predef.classOf().
       |[6:28..6:29): q => po21a/o.
       |[6:30..6:31): C => po21a/o.C#
       |""".stripMargin,
  )

  occurrences(
    "a renamed import in a partial function lambda, with the name of a parameter of applyOrElse",
    """|package po22a { object o { class C } }
       |object po22 {
       |  import po22a.{o => default}
       |  val a: PartialFunction[Int, Any] = x => x match { case _ => classOf[default.C] }
       |  val b: Int => Any = x => x match { case _ => classOf[default.C] }
       |}
       |""".stripMargin,
    """|[0:8..0:13): po22a <= po22a/
       |[0:23..0:24): o <= po22a/o.
       |[0:33..0:34): C <= po22a/o.C#
       |[0:34..0:34):  <= po22a/o.C#`<init>`().
       |[1:7..1:11): po22 <= _empty_/po22.
       |[2:9..2:14): po22a => po22a/
       |[2:16..2:17): o => po22a/o.
       |[3:6..3:7): a <= _empty_/po22.a.
       |[3:9..3:24): PartialFunction => scala/PartialFunction#
       |[3:25..3:28): Int => scala/Int#
       |[3:30..3:33): Any => scala/Any#
       |[3:42..3:43): x => local3
       |[3:44..3:44):  <= local0
       |[3:62..3:69): classOf => scala/Predef.classOf().
       |[3:70..3:77): default => po22a/o.
       |[3:78..3:79): C => po22a/o.C#
       |[4:6..4:7): b <= _empty_/po22.b.
       |[4:9..4:12): Int => scala/Int#
       |[4:16..4:19): Any => scala/Any#
       |[4:22..4:23): x <= local8
       |[4:27..4:28): x => local8
       |[4:47..4:54): classOf => scala/Predef.classOf().
       |[4:55..4:62): default => po22a/o.
       |[4:63..4:64): C => po22a/o.C#
       |""".stripMargin,
  )

  occurrences(
    "a renamed import in a secondary constructor call, and an import of that name in the class body",
    """|package po23a { object o { class C }; object x { class C } }
       |object po23 {
       |  import po23a.{o => q}
       |  class F(c: Class[_]) {
       |    import po23a.{x => q}
       |    def this() = this(classOf[q.C])
       |    val a = classOf[q.C]
       |  }
       |}
       |""".stripMargin,
    """|[0:8..0:13): po23a <= po23a/
       |[0:23..0:24): o <= po23a/o.
       |[0:33..0:34): C <= po23a/o.C#
       |[0:34..0:34):  <= po23a/o.C#`<init>`().
       |[0:45..0:46): x <= po23a/x.
       |[0:55..0:56): C <= po23a/x.C#
       |[0:56..0:56):  <= po23a/x.C#`<init>`().
       |[1:7..1:11): po23 <= _empty_/po23.
       |[2:9..2:14): po23a => po23a/
       |[2:16..2:17): o => po23a/o.
       |[3:8..3:9): F <= _empty_/po23.F#
       |[3:9..3:9):  <= _empty_/po23.F#`<init>`().
       |[3:10..3:11): c <= _empty_/po23.F#`<init>`().(c)
       |[3:10..3:11): c <= _empty_/po23.F#c.
       |[3:13..3:18): Class => scala/Predef.Class#
       |[3:19..3:20): _ <= local0
       |[4:11..4:16): po23a => po23a/
       |[4:18..4:19): x => po23a/x.
       |[5:8..5:12): this <= _empty_/po23.F#`<init>`(+1).
       |[5:21..5:21):  => _empty_/po23.F#`<init>`().
       |[5:22..5:29): classOf => scala/Predef.classOf().
       |[5:30..5:31): q => po23a/o.
       |[5:32..5:33): C => po23a/o.C#
       |[6:8..6:9): a <= _empty_/po23.F#a.
       |[6:12..6:19): classOf => scala/Predef.classOf().
       |[6:20..6:21): q => po23a/x.
       |[6:22..6:23): C => po23a/x.C#
       |""".stripMargin,
  )

  occurrences(
    "a renamed package object in a constructor call and a partial function lambda, with a member of that name",
    """|package object po24p { class C }
       |object po24 {
       |  import po24p.{`package` => q}
       |  class B(c: Class[_])
       |  class E extends B(classOf[q.C]) { def q = 1 }
       |  val a: PartialFunction[Int, Any] = default => default match { case _ => classOf[q.C] }
       |}
       |""".stripMargin,
    """|[0:15..0:20): po24p <= po24p/package.
       |[0:29..0:30): C <= po24p/package.C#
       |[0:30..0:30):  <= po24p/package.C#`<init>`().
       |[1:7..1:11): po24 <= _empty_/po24.
       |[2:9..2:14): po24p => po24p/
       |[2:16..2:25): `package` => po24p/package.
       |[3:8..3:9): B <= _empty_/po24.B#
       |[3:9..3:9):  <= _empty_/po24.B#`<init>`().
       |[3:10..3:11): c <= _empty_/po24.B#`<init>`().(c)
       |[3:10..3:11): c <= _empty_/po24.B#c.
       |[3:13..3:18): Class => scala/Predef.Class#
       |[3:19..3:20): _ <= local0
       |[4:8..4:9): E <= _empty_/po24.E#
       |[4:10..4:10):  <= _empty_/po24.E#`<init>`().
       |[4:18..4:19): B => _empty_/po24.B#
       |[4:19..4:19):  => _empty_/po24.B#`<init>`().
       |[4:20..4:27): classOf => scala/Predef.classOf().
       |[4:28..4:29): q => po24p/package.
       |[4:30..4:31): C => po24p/package.C#
       |[4:40..4:41): q <= _empty_/po24.E#q().
       |[5:6..5:7): a <= _empty_/po24.a.
       |[5:9..5:24): PartialFunction => scala/PartialFunction#
       |[5:25..5:28): Int => scala/Int#
       |[5:30..5:33): Any => scala/Any#
       |[5:48..5:55): default => local4
       |[5:56..5:56):  <= local1
       |[5:74..5:81): classOf => scala/Predef.classOf().
       |[5:82..5:83): q => po24p/package.
       |[5:84..5:85): C => po24p/package.C#
       |""".stripMargin,
  )

  occurrences(
    "a renamed package of a package object in a constructor call and a partial function lambda",
    """|package po25a { package object po25b { class C } }
       |object po25 {
       |  import po25a.{po25b => q}
       |  import po25a.{po25b => default}
       |  class B(c: Class[_])
       |  class E extends B(classOf[q.C]) { def q = 1 }
       |  val a: PartialFunction[Int, Any] = x => x match { case _ => classOf[default.C] }
       |}
       |""".stripMargin,
    """|[0:8..0:13): po25a <= po25a/
       |[0:31..0:36): po25b <= po25a/po25b/package.
       |[0:45..0:46): C <= po25a/po25b/package.C#
       |[0:46..0:46):  <= po25a/po25b/package.C#`<init>`().
       |[1:7..1:11): po25 <= _empty_/po25.
       |[2:9..2:14): po25a => po25a/
       |[2:16..2:21): po25b => po25a/po25b/
       |[3:9..3:14): po25a => po25a/
       |[3:16..3:21): po25b => po25a/po25b/
       |[4:8..4:9): B <= _empty_/po25.B#
       |[4:9..4:9):  <= _empty_/po25.B#`<init>`().
       |[4:10..4:11): c <= _empty_/po25.B#`<init>`().(c)
       |[4:10..4:11): c <= _empty_/po25.B#c.
       |[4:13..4:18): Class => scala/Predef.Class#
       |[4:19..4:20): _ <= local0
       |[5:8..5:9): E <= _empty_/po25.E#
       |[5:10..5:10):  <= _empty_/po25.E#`<init>`().
       |[5:18..5:19): B => _empty_/po25.B#
       |[5:19..5:19):  => _empty_/po25.B#`<init>`().
       |[5:20..5:27): classOf => scala/Predef.classOf().
       |[5:28..5:29): q => po25a/po25b/package.
       |[5:30..5:31): C => po25a/po25b/package.C#
       |[5:40..5:41): q <= _empty_/po25.E#q().
       |[6:6..6:7): a <= _empty_/po25.a.
       |[6:9..6:24): PartialFunction => scala/PartialFunction#
       |[6:25..6:28): Int => scala/Int#
       |[6:30..6:33): Any => scala/Any#
       |[6:42..6:43): x => local4
       |[6:44..6:44):  <= local1
       |[6:62..6:69): classOf => scala/Predef.classOf().
       |[6:70..6:77): default => po25a/po25b/package.
       |[6:78..6:79): C => po25a/po25b/package.C#
       |""".stripMargin,
  )

  occurrences(
    "a package object and its package renamed to one name by imports in nested scopes",
    """|package po26a { package object po26b { class C } }
       |object po26 {
       |  import po26a.po26b.{`package` => q}
       |  object i {
       |    import po26a.{po26b => q}
       |    val a = classOf[q.C]
       |  }
       |}
       |""".stripMargin,
    """|[0:8..0:13): po26a <= po26a/
       |[0:31..0:36): po26b <= po26a/po26b/package.
       |[0:45..0:46): C <= po26a/po26b/package.C#
       |[0:46..0:46):  <= po26a/po26b/package.C#`<init>`().
       |[1:7..1:11): po26 <= _empty_/po26.
       |[2:9..2:14): po26a => po26a/
       |[2:15..2:20): po26b => po26a/po26b/
       |[2:22..2:31): `package` => po26a/po26b/package.
       |[3:9..3:10): i <= _empty_/po26.i.
       |[4:11..4:16): po26a => po26a/
       |[4:18..4:23): po26b => po26a/po26b/
       |[5:8..5:9): a <= _empty_/po26.i.a.
       |[5:12..5:19): classOf => scala/Predef.classOf().
       |[5:20..5:21): q => po26a/po26b/package.
       |[5:22..5:23): C => po26a/po26b/package.C#
       |""".stripMargin,
  )

  occurrences(
    "a renamed import in a default argument of a constructor, with a companion member of that name",
    """|package po27a { object o { class C } }
       |object po27 {
       |  import po27a.{o => q}
       |  class G(c: Class[_] = classOf[q.C])
       |  object G { val q = 1 }
       |}
       |""".stripMargin,
    """|[0:8..0:13): po27a <= po27a/
       |[0:23..0:24): o <= po27a/o.
       |[0:33..0:34): C <= po27a/o.C#
       |[0:34..0:34):  <= po27a/o.C#`<init>`().
       |[1:7..1:11): po27 <= _empty_/po27.
       |[2:9..2:14): po27a => po27a/
       |[2:16..2:17): o => po27a/o.
       |[3:8..3:9): G <= _empty_/po27.G#
       |[3:9..3:9):  <= _empty_/po27.G#`<init>`().
       |[3:10..3:11): c <= _empty_/po27.G#`<init>`().(c)
       |[3:10..3:11): c <= _empty_/po27.G#c.
       |[3:13..3:18): Class => scala/Predef.Class#
       |[3:19..3:20): _ <= local0
       |[3:24..3:31): classOf => scala/Predef.classOf().
       |[3:32..3:33): q => po27a/o.
       |[3:34..3:35): C => po27a/o.C#
       |[4:9..4:10): G <= _empty_/po27.G.
       |[4:17..4:18): q <= _empty_/po27.G.q.
       |""".stripMargin,
  )

  occurrences(
    "a self alias in a partial function lambda and in a nested class",
    """|class po28 { self =>
       |  class C
       |  val a: PartialFunction[Int, Any] = x => x match { case _ => classOf[self.C] }
       |  class D { val b = classOf[self.C] }
       |}
       |""".stripMargin,
    """|[0:6..0:10): po28 <= _empty_/po28#
       |[0:11..0:11):  <= _empty_/po28#`<init>`().
       |[0:13..0:17): self <= local0
       |[1:8..1:9): C <= _empty_/po28#C#
       |[1:9..1:9):  <= _empty_/po28#C#`<init>`().
       |[2:6..2:7): a <= _empty_/po28#a.
       |[2:9..2:24): PartialFunction => scala/PartialFunction#
       |[2:25..2:28): Int => scala/Int#
       |[2:30..2:33): Any => scala/Any#
       |[2:42..2:43): x => local4
       |[2:44..2:44):  <= local1
       |[2:62..2:69): classOf => scala/Predef.classOf().
       |[2:70..2:74): self => local0
       |[2:75..2:76): C => _empty_/po28#C#
       |[3:8..3:9): D <= _empty_/po28#D#
       |[3:10..3:10):  <= _empty_/po28#D#`<init>`().
       |[3:16..3:17): b <= _empty_/po28#D#b.
       |[3:20..3:27): classOf => scala/Predef.classOf().
       |[3:28..3:32): self => local0
       |[3:33..3:34): C => _empty_/po28#C#
       |""".stripMargin,
  )

  occurrences(
    "a package object renamed to the name of its package",
    """|package po29a { package object po29b { class C } }
       |object po29 {
       |  import po29a.po29b.{`package` => po29b}
       |  val a = classOf[po29b.C]
       |  val b = List[po29b.C]()
       |}
       |""".stripMargin,
    """|[0:8..0:13): po29a <= po29a/
       |[0:31..0:36): po29b <= po29a/po29b/package.
       |[0:45..0:46): C <= po29a/po29b/package.C#
       |[0:46..0:46):  <= po29a/po29b/package.C#`<init>`().
       |[1:7..1:11): po29 <= _empty_/po29.
       |[2:9..2:14): po29a => po29a/
       |[2:15..2:20): po29b => po29a/po29b/
       |[2:22..2:31): `package` => po29a/po29b/package.
       |[3:6..3:7): a <= _empty_/po29.a.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:23): po29b => po29a/po29b/package.
       |[3:24..3:25): C => po29a/po29b/package.C#
       |[4:6..4:7): b <= _empty_/po29.b.
       |[4:10..4:14): List => scala/collection/immutable/Nil.
       |[4:15..4:20): po29b => po29a/po29b/package.
       |[4:21..4:22): C => po29a/po29b/package.C#
       |""".stripMargin,
  )

}

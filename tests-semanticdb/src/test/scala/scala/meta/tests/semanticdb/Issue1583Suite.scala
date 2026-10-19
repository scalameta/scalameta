package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1583
class Issue1583Suite extends SemanticdbSuite {

  // an underscore type argument in a type pattern (#1583)
  private val typePattern =
    """|object O {
       |  def m(a: Any) = a match { case _: List[_] => }
       |}
       |""".stripMargin

  symbols(
    typePattern,
    """|_empty_/O. => final object O extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.m(). => method m(a: Any): Unit
       |  a => _empty_/O.m().(a)
       |  Any => scala/Any#
       |  Unit => scala/Unit#
       |_empty_/O.m().(a) => param a: Any
       |  Any => scala/Any#
       |local0 => abstract type _
       |""".stripMargin,
  )

  occurrences(
    typePattern,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): m <= _empty_/O.m().
       |[1:8..1:9): a <= _empty_/O.m().(a)
       |[1:11..1:14): Any => scala/Any#
       |[1:18..1:19): a => _empty_/O.m().(a)
       |[1:36..1:40): List => scala/package.List#
       |""".stripMargin,
  )

  // underscore type arguments in a bound type pattern (#1583)
  private val boundTypePattern =
    """|object O {
       |  def m(a: Any) = a match { case x: Map[_, _] => x }
       |}
       |""".stripMargin

  symbols(
    boundTypePattern,
    """|_empty_/O. => final object O extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.m(). => method m(a: Any): Map[_, Any] forSome { type _ }
       |  a => _empty_/O.m().(a)
       |  Any => scala/Any#
       |  Map => scala/collection/immutable/Map#
       |  _ => local0
       |_empty_/O.m().(a) => param a: Any
       |  Any => scala/Any#
       |local0 => abstract type _
       |local1 => val local x: Map[_, _]
       |  Map => scala/Predef.Map#
       |  _ => local0
       |  _ => local2
       |local2 => abstract type _
       |""".stripMargin,
  )

  occurrences(
    boundTypePattern,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): m <= _empty_/O.m().
       |[1:8..1:9): a <= _empty_/O.m().(a)
       |[1:11..1:14): Any => scala/Any#
       |[1:18..1:19): a => _empty_/O.m().(a)
       |[1:33..1:34): x <= local1
       |[1:36..1:39): Map => scala/Predef.Map#
       |[1:49..1:50): x => local1
       |""".stripMargin,
  )

  // underscore type arguments in a declared type (#1583)
  private val existential =
    """|object O {
       |  def l: List[_] = ???
       |  def b: Map[_ <: AnyRef, _ >: Null] = ???
       |}
       |""".stripMargin

  symbols(
    existential,
    """|_empty_/O. => final object O extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.b(). => method b: Map[_, _] forSome { type _ <: AnyRef; type _ >: Null }
       |  Map => scala/Predef.Map#
       |  _ => local1
       |  _ => local2
       |  AnyRef => scala/AnyRef#
       |  Null => scala/Null#
       |_empty_/O.l(). => method l: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local0
       |""".stripMargin,
  )

  occurrences(
    existential,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): l <= _empty_/O.l().
       |[1:9..1:13): List => scala/package.List#
       |[1:19..1:22): ??? => scala/Predef.`???`().
       |[2:6..2:7): b <= _empty_/O.b().
       |[2:9..2:12): Map => scala/Predef.Map#
       |[2:39..2:42): ??? => scala/Predef.`???`().
       |""".stripMargin,
  )

  // an underscore lambda parameter (#1584)
  private val lambdaParam =
    """|object O {
       |  val f: Int => Int = _ => 1
       |}
       |""".stripMargin

  symbols(
    lambdaParam,
    """|_empty_/O. => final object O extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.f. => val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    lambdaParam,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): f <= _empty_/O.f.
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |""".stripMargin,
  )

  // several and typed underscore lambda parameters (#1584)
  private val lambdaParams =
    """|object O {
       |  val f: (Int, Int) => Int = (_, _) => 1
       |  val g = (_: Int) => 1
       |}
       |""".stripMargin

  symbols(
    lambdaParams,
    """|_empty_/O. => final object O extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.f. => val method f: Function2[Int, Int, Int]
       |  Function2 => scala/Function2#
       |  Int => scala/Int#
       |_empty_/O.g. => val method g: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    lambdaParams,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): f <= _empty_/O.f.
       |[1:10..1:13): Int => scala/Int#
       |[1:15..1:18): Int => scala/Int#
       |[1:23..1:26): Int => scala/Int#
       |[2:6..2:7): g <= _empty_/O.g.
       |[2:14..2:17): Int => scala/Int#
       |""".stripMargin,
  )

  // nested underscore lambda parameters (#1584)
  private val nestedLambdas =
    """|object O {
       |  val f: Int => Int => Int = _ => _ => 1
       |}
       |""".stripMargin

  symbols(
    nestedLambdas,
    """|_empty_/O. => final object O extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.f. => val method f: Function1[Int, Function1[Int, Int]]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    nestedLambdas,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): f <= _empty_/O.f.
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:23..1:26): Int => scala/Int#
       |""".stripMargin,
  )

  // an underscore type parameter of a class (#1585)
  private val classTparam =
    """|class C[_]
       |""".stripMargin

  occurrences(
    classTparam,
    """|[0:6..0:7): C <= _empty_/C#
       |[0:10..0:10):  <= _empty_/C#`<init>`().
       |""".stripMargin,
  )

  // a bounded underscore type parameter (#1585)
  private val boundedTparam =
    """|class C[_ <: AnyRef]
       |""".stripMargin

  occurrences(
    boundedTparam,
    """|[0:6..0:7): C <= _empty_/C#
       |[0:13..0:19): AnyRef => scala/AnyRef#
       |[0:20..0:20):  <= _empty_/C#`<init>`().
       |""".stripMargin,
  )

  // an underscore type parameter of a type parameter (#1585)
  private val higherKindedTparam =
    """|object O {
       |  def m[T[_]] = 1
       |}
       |""".stripMargin

  occurrences(
    higherKindedTparam,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): m <= _empty_/O.m().
       |[1:8..1:9): T <= _empty_/O.m().[T]
       |""".stripMargin,
  )

  // an underscore type parameter of a method with a default argument (#1585)
  private val defaultTparam =
    """|object O {
       |  def m[T[_]](a: Int = 1) = 1
       |}
       |""".stripMargin

  occurrences(
    defaultTparam,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): m <= _empty_/O.m().
       |[1:8..1:9): T <= _empty_/O.m().[T]
       |[1:14..1:15): a <= _empty_/O.m().(a)
       |[1:17..1:20): Int => scala/Int#
       |""".stripMargin,
  )

  // an underscore type parameter of a case class (#1585)
  private val caseClassTparam =
    """|case class K[_](a: Int)
       |""".stripMargin

  occurrences(
    caseClassTparam,
    """|[0:11..0:12): K <= _empty_/K#
       |[0:15..0:15):  <= _empty_/K#`<init>`().
       |[0:16..0:17): a <= _empty_/K#`<init>`().(a)
       |[0:16..0:17): a <= _empty_/K#a.
       |[0:19..0:22): Int => scala/Int#
       |""".stripMargin,
  )

  // an underscore type parameter of a method (#1585)
  private val methodTparam =
    """|object O {
       |  def n[_] = 1
       |}
       |""".stripMargin

  occurrences(
    methodTparam,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): n <= _empty_/O.n().
       |""".stripMargin,
  )

  // an underscore type parameter of a type alias (#1585)
  private val aliasTparam =
    """|object O {
       |  type F[_] = Int
       |}
       |""".stripMargin

  occurrences(
    aliasTparam,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:7..1:8): F <= _empty_/O.F#
       |[1:14..1:17): Int => scala/Int#
       |""".stripMargin,
  )

  // an underscore placeholder (#1657)
  private val placeholder =
    """|object O {
       |  Option(1).map(_ + 1)
       |}
       |""".stripMargin

  symbols(
    placeholder,
    """|_empty_/O. => final object O extends AnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    placeholder,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:2..1:8): Option => scala/Option.
       |[1:12..1:15): map => scala/Option#map().
       |[1:18..1:19): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  // several and typed underscore placeholders (#1657)
  private val placeholders =
    """|object O {
       |  def r(f: (Int, Int) => Int) = f(1, 2)
       |  r(_ + _)
       |  Option(1).map((_: Int) + 1)
       |}
       |""".stripMargin

  symbols(
    placeholders,
    """|_empty_/O. => final object O extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.r(). => method r(f: Function2[Int, Int, Int]): Int
       |  f => _empty_/O.r().(f)
       |  Function2 => scala/Function2#
       |  Int => scala/Int#
       |_empty_/O.r().(f) => param f: Function2[Int, Int, Int]
       |  Function2 => scala/Function2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    placeholders,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): r <= _empty_/O.r().
       |[1:8..1:9): f <= _empty_/O.r().(f)
       |[1:12..1:15): Int => scala/Int#
       |[1:17..1:20): Int => scala/Int#
       |[1:25..1:28): Int => scala/Int#
       |[1:32..1:33): f => _empty_/O.r().(f)
       |[2:2..2:3): r => _empty_/O.r().
       |[2:6..2:7): + => scala/Int#`+`(+4).
       |[3:2..3:8): Option => scala/Option.
       |[3:12..3:15): map => scala/Option#map().
       |[3:20..3:23): Int => scala/Int#
       |[3:25..3:26): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  // an underscore placeholder as a qualifier and in a for body (#1657)
  private val qualifierPlaceholder =
    """|object O {
       |  Option(1).map(_.toString)
       |  for (x <- Option(1)) Option(2).map(_ + x)
       |}
       |""".stripMargin

  symbols(
    qualifierPlaceholder,
    """|_empty_/O. => final object O extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    qualifierPlaceholder,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:2..1:8): Option => scala/Option.
       |[1:12..1:15): map => scala/Option#map().
       |[1:18..1:26): toString => scala/Any#toString().
       |[2:7..2:8): x <= local0
       |[2:12..2:18): Option => scala/Option.
       |[2:23..2:29): Option => scala/Option.
       |[2:33..2:36): map => scala/Option#map().
       |[2:39..2:40): + => scala/Int#`+`(+4).
       |[2:41..2:42): x => local0
       |""".stripMargin,
  )

  // an underscore placeholder in an application with named arguments (#1657)
  private val namedArgPlaceholder =
    """|object O {
       |  def g(a: Int => Int, b: Int = 1) = 1
       |  val i = 2
       |  g(_ + 1)
       |  g(b = i, a = _ + 1)
       |}
       |""".stripMargin

  symbols(
    namedArgPlaceholder,
    """|_empty_/O. => final object O extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.g$default$2(). => synthetic method g$default$2: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/O.g(). => method g(a: Function1[Int, Int], b: Int): Int
       |  a => _empty_/O.g().(a)
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  b => _empty_/O.g().(b)
       |_empty_/O.g().(a) => param a: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/O.g().(b) => default param b: Int
       |  Int => scala/Int#
       |_empty_/O.i. => val method i: Int
       |  Int => scala/Int#
       |local0 => val local x$1: Int
       |  Int => scala/Int#
       |local1 => val local x$2: Function1[Int, Int] @uncheckedBounds
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  uncheckedBounds => scala/reflect/internal/annotations/uncheckedBounds#
       |""".stripMargin,
  )

  occurrences(
    namedArgPlaceholder,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): g <= _empty_/O.g().
       |[1:8..1:9): a <= _empty_/O.g().(a)
       |[1:11..1:14): Int => scala/Int#
       |[1:18..1:21): Int => scala/Int#
       |[1:23..1:24): b <= _empty_/O.g().(b)
       |[1:26..1:29): Int => scala/Int#
       |[2:6..2:7): i <= _empty_/O.i.
       |[3:2..3:3): g => _empty_/O.g().
       |[3:6..3:7): + => scala/Int#`+`(+4).
       |[4:2..4:3): g => _empty_/O.g().
       |[4:4..4:5): b => _empty_/O.g().(b)
       |[4:8..4:9): i => _empty_/O.i.
       |[4:11..4:12): a => _empty_/O.g().(a)
       |[4:17..4:18): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  // an underscore placeholder in SAM position (#1657)
  private val samPlaceholder =
    """|object O {
       |  trait S { def f(a: Int): Int }
       |  def w(s: S) = s.f(0)
       |  w(_ + 1)
       |}
       |""".stripMargin

  symbols(
    samPlaceholder,
    """|_empty_/O. => final object O extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/O.S#f().(a)
       |  Int => scala/Int#
       |_empty_/O.S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/O.w(). => method w(s: S): Int
       |  s => _empty_/O.w().(s)
       |  S => _empty_/O.S#
       |  Int => scala/Int#
       |_empty_/O.w().(s) => param s: S
       |  S => _empty_/O.S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/O.S#
       |local2 => synthetic method f(a: Int): Int <: _empty_/O.S#f().
       |  a => _empty_/O.S#f().(a)
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    samPlaceholder,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:8..1:9): S <= _empty_/O.S#
       |[1:16..1:17): f <= _empty_/O.S#f().
       |[1:18..1:19): a <= _empty_/O.S#f().(a)
       |[1:21..1:24): Int => scala/Int#
       |[1:27..1:30): Int => scala/Int#
       |[2:6..2:7): w <= _empty_/O.w().
       |[2:8..2:9): s <= _empty_/O.w().(s)
       |[2:11..2:12): S => _empty_/O.S#
       |[2:16..2:17): s => _empty_/O.w().(s)
       |[2:18..2:19): f => _empty_/O.S#f().
       |[3:2..3:3): w => _empty_/O.w().
       |[3:4..3:9): _ + 1 <= local0
       |[3:4..3:9): _ + 1 <= local2
       |[3:6..3:7): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  // underscore patterns
  private val patterns =
    """|object O {
       |  def m(a: Any) = a match {
       |    case _ =>
       |  }
       |  def n(a: Any) = a match {
       |    case _: Int =>
       |    case (_, _) =>
       |    case Array(_*) =>
       |    case Array(xs @ _*) =>
       |  }
       |  val p: PartialFunction[Any, Int] = { case _ => 1 }
       |}
       |""".stripMargin

  occurrences(
    patterns,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): m <= _empty_/O.m().
       |[1:8..1:9): a <= _empty_/O.m().(a)
       |[1:11..1:14): Any => scala/Any#
       |[1:18..1:19): a => _empty_/O.m().(a)
       |[4:6..4:7): n <= _empty_/O.n().
       |[4:8..4:9): a <= _empty_/O.n().(a)
       |[4:11..4:14): Any => scala/Any#
       |[4:18..4:19): a => _empty_/O.n().(a)
       |[5:12..5:15): Int => scala/Int#
       |[7:9..7:14): Array => scala/Array.
       |[8:9..8:14): Array => scala/Array.
       |[8:15..8:17): xs <= local2
       |[10:6..10:7): p <= _empty_/O.p.
       |[10:9..10:24): PartialFunction => scala/PartialFunction#
       |[10:25..10:28): Any => scala/Any#
       |[10:30..10:33): Int => scala/Int#
       |[10:37..10:37):  <= local3
       |""".stripMargin,
  )

  // underscores in an import
  private val imports =
    """|object O {
       |  import scala.util.{Try => _, _}
       |  val s = Success(1)
       |}
       |""".stripMargin

  occurrences(
    imports,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:9..1:14): scala => scala/
       |[1:15..1:19): util => scala/util/
       |[1:21..1:24): Try => scala/util/Try#
       |[1:21..1:24): Try => scala/util/Try.
       |[2:6..2:7): s <= _empty_/O.s.
       |[2:10..2:17): Success => scala/util/Success.
       |""".stripMargin,
  )

  // underscores in value definitions
  private val vals =
    """|object O {
       |  val _ = 1
       |  val (a, _) = (1, 2)
       |  var v: Int = _
       |}
       |""".stripMargin

  symbols(
    vals,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O._. => val method _: Int
         |  Int => scala/Int#
         |_empty_/O.`v_=`(). => var method v_=(x$1: Int): Unit
         |  x$1 => _empty_/O.`v_=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/O.`v_=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/O.a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/O.v(). => var method v: Int
         |  Int => scala/Int#
         |local0 => val local a: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.`v_=`(). => var method v_=(x$1: Int): Unit
         |  x$1 => _empty_/O.`v_=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/O.`v_=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/O.a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/O.v(). => var method v: Int
         |  Int => scala/Int#
         |_empty_/O.x$1. => synthetic val method x$1: Unit
         |  Unit => scala/Unit#
         |local0 => val local a: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    vals,
    """|[0:7..0:8): O <= _empty_/O.
       |[2:7..2:8): a <= local0
       |[3:6..3:7): v <= _empty_/O.v().
       |[3:9..3:12): Int => scala/Int#
       |""".stripMargin,
  )

  // a typed underscore value definition
  private val typedVal =
    """|object O {
       |  val _: Int = 1
       |}
       |""".stripMargin

  symbols(
    typedVal,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O._. => val method _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.x$1. => synthetic val method x$1: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  occurrences(
    typedVal,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:9..1:12): Int => scala/Int#
       |""".stripMargin,
  )

  // an underscore variable definition
  private val underscoreVar =
    """|object O {
       |  var _ = 1
       |}
       |""".stripMargin

  symbols(
    underscoreVar,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O._(). => var method _: Int
         |  Int => scala/Int#
         |_empty_/O.`__=`(). => var method __=(x$1: Int): Unit
         |  x$1 => _empty_/O.`__=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/O.`__=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.x$1. => synthetic val method x$1: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  occurrences(
    underscoreVar,
    """|[0:7..0:8): O <= _empty_/O.
       |""".stripMargin,
  )

  // a local underscore value definition
  private val localVal =
    """|object O {
       |  def m = {
       |    val _ = 1
       |    2
       |  }
       |}
       |""".stripMargin

  symbols(
    localVal,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => val local _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => synthetic val local x$1: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  occurrences(
    localVal,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): m <= _empty_/O.m().
       |""".stripMargin,
  )

  // an underscore vararg and an eta-expansion
  private val varargEta =
    """|object O {
       |  def f(a: Int*) = a
       |  f(Array(1): _*)
       |  def m(a: Int) = a
       |  val e = m _
       |}
       |""".stripMargin

  occurrences(
    varargEta,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): f <= _empty_/O.f().
       |[1:8..1:9): a <= _empty_/O.f().(a)
       |[1:11..1:14): Int => scala/Int#
       |[1:19..1:20): a => _empty_/O.f().(a)
       |[2:2..2:3): f => _empty_/O.f().
       |[2:4..2:9): Array => scala/Array.
       |[3:6..3:7): m <= _empty_/O.m().
       |[3:8..3:9): a <= _empty_/O.m().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): a => _empty_/O.m().(a)
       |[4:6..4:7): e <= _empty_/O.e.
       |[4:10..4:11): m => _empty_/O.m().
       |""".stripMargin,
  )

  // an underscore self parameter
  private val self =
    """|trait B
       |class C extends B { _: B =>
       |}
       |""".stripMargin

  occurrences(
    self,
    """|[0:6..0:7): B <= _empty_/B#
       |[1:6..1:7): C <= _empty_/C#
       |[1:8..1:8):  <= _empty_/C#`<init>`().
       |[1:16..1:17): B => _empty_/B#
       |[1:17..1:17):  => java/lang/Object#`<init>`().
       |[1:23..1:24): B => _empty_/B#
       |""".stripMargin,
  )

  // an underscore generator pattern
  private val generator =
    """|object O {
       |  for (_ <- Option(1)) ()
       |}
       |""".stripMargin

  symbols(
    generator,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => param _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin,
  )

  occurrences(
    generator,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:12..1:18): Option => scala/Option.
       |""".stripMargin,
  )

  // underscore generator patterns
  private val generators =
    """|object O {
       |  for (_ <- Option(1); _ <- Option(2)) ()
       |}
       |""".stripMargin

  symbols(
    generators,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => param _: Int
         |  Int => scala/Int#
         |local1 => param _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin,
  )

  occurrences(
    generators,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:12..1:18): Option => scala/Option.
       |[1:28..1:34): Option => scala/Option.
       |""".stripMargin,
  )

  // a typed underscore generator pattern
  private val typedGenerator =
    """|object O {
       |  for (_: Int <- Option(1)) ()
       |}
       |""".stripMargin

  symbols(
    typedGenerator,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local1 => param _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin,
  )

  occurrences(
    typedGenerator,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:10..1:13): Int => scala/Int#
       |[1:17..1:23): Option => scala/Option.
       |""".stripMargin,
  )

  // an underscore value definition in a for
  private val forValue =
    """|object O {
       |  for { x <- Option(1); _ = x } ()
       |}
       |""".stripMargin

  symbols(
    forValue,
    """|_empty_/O. => final object O extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local2 => val local x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    forValue,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:8..1:9): x <= local0
       |[1:13..1:19): Option => scala/Option.
       |[1:28..1:29): x => local0
       |""".stripMargin,
  )

  // an underscore generator pattern with yield
  private val generatorYield =
    """|object O {
       |  for (_ <- Option(1)) yield 1
       |}
       |""".stripMargin

  symbols(
    generatorYield,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => param _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin,
  )

  occurrences(
    generatorYield,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:12..1:18): Option => scala/Option.
       |""".stripMargin,
  )

  // an underscore generator pattern in a yield
  private val yieldGenerator =
    """|object O {
       |  for (x <- Option(1)) yield for (_ <- Option(2)) yield x
       |}
       |""".stripMargin

  symbols(
    yieldGenerator,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => param x: Int
         |  Int => scala/Int#
         |local1 => param _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => param x: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    yieldGenerator,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:7..1:8): x <= local0
       |[1:12..1:18): Option => scala/Option.
       |[1:39..1:45): Option => scala/Option.
       |[1:56..1:57): x => local0
       |""".stripMargin,
  )

  // an underscore that starts an infix value pattern
  private val infixVal =
    """|object O {
       |  val _ :: x = List(1, 2)
       |}
       |""".stripMargin

  occurrences(
    infixVal,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:8..1:10): :: => scala/package.`::`.
         |[1:11..1:12): x <= local0
         |[1:15..1:19): List => scala/collection/immutable/List.
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:8..1:10): :: => scala/package.`::`.
         |[1:11..1:12): x <= local0
         |[1:15..1:19): List => scala/package.List.
         |""".stripMargin,
  )

  // an underscore that starts a local infix value pattern
  private val localInfixVal =
    """|object O {
       |  def m = {
       |    val _ :: x = List(1, 2)
       |    x
       |  }
       |}
       |""".stripMargin

  occurrences(
    localInfixVal,
    if (ScalaVersion.is213 && !ScalaVersion.atLeast213_17)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): m <= _empty_/O.m().
         |[2:10..2:12): :: => scala/package.`::`.
         |[2:13..2:14): x <= local0
         |[2:17..2:21): List => scala/package.List.
         |[3:4..3:5): x => local1
         |""".stripMargin
    else if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): m <= _empty_/O.m().
         |[2:10..2:12): :: => scala/package.`::`.
         |[2:13..2:14): x <= local0
         |[2:17..2:21): List => scala/collection/immutable/List.
         |[3:4..3:5): x => local1
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): m <= _empty_/O.m().
         |[2:10..2:12): :: => scala/package.`::`.
         |[2:13..2:14): x <= local0
         |[2:17..2:21): List => scala/package.List.
         |[3:4..3:5): x => local0
         |""".stripMargin,
  )

  // an underscore that starts an infix generator pattern
  private val infixGenerator =
    """|object O {
       |  for (_ :: x <- List(List(1))) yield x
       |}
       |""".stripMargin

  symbols(
    infixGenerator,
    """|_empty_/O. => final object O extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local2 => val local x: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    infixGenerator,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:9..1:11): :: => scala/package.`::`.
         |[1:12..1:13): x <= local2
         |[1:17..1:21): List => scala/collection/immutable/List.
         |[1:22..1:26): List => scala/collection/immutable/List.
         |[1:38..1:39): x => local2
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:9..1:11): :: => scala/package.`::`.
         |[1:12..1:13): x <= local2
         |[1:17..1:21): List => scala/package.List.
         |[1:22..1:26): List => scala/package.List.
         |[1:38..1:39): x => local2
         |""".stripMargin,
  )

  // an underscore generator pattern with a guard
  private val guardedGenerator =
    """|object O {
       |  for (_ <- Option(1) if true) yield 1
       |}
       |""".stripMargin

  symbols(
    guardedGenerator,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => param _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin,
  )

  occurrences(
    guardedGenerator,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:12..1:18): Option => scala/Option.
       |""".stripMargin,
  )

  synthetics(
    guardedGenerator,
    if (ScalaVersion.is212)
      """|[1:2..1:38): for (_ <- Option(1) if true) yield 1 => orig(Option(1)).withFilter({(_) => orig(true)}).map[Int]({(_) => orig(1)})
         |  withFilter => scala/Option#withFilter().
         |  _ => local0
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |[1:12..1:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:38): for (_ <- Option(1) if true) yield 1 => orig(Option(1)).withFilter({(local0) => orig(true)}).map[Int]({(local1) => orig(1)})
         |  withFilter => scala/Option#withFilter().
         |  local0 => local0
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |  local1 => local1
         |[1:12..1:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  // underscores that start infix value patterns
  private val infixVals =
    """|object O {
       |  val _ :: x :: y = List(1, 2)
       |  val _ :: _ = List(1)
       |  var _ :: v = List(1)
       |}
       |""".stripMargin

  symbols(
    infixVals,
    """|_empty_/O. => final object O extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.`v_=`(). => var method v_=(x$1: List[Int]): Unit
       |  x$1 => _empty_/O.`v_=`().(x$1)
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/O.`v_=`().(x$1) => synthetic param x$1: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/O.v(). => var method v: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/O.x$1. => synthetic val method x$1: Tuple2[Int, List[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
       |_empty_/O.x$2. => synthetic val method x$2: Unit
       |  Unit => scala/Unit#
       |_empty_/O.x. => val method x: Int
       |  Int => scala/Int#
       |_empty_/O.y. => val method y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local2 => val local v: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    infixVals,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:8..1:10): :: => scala/package.`::`.
         |[1:11..1:12): x <= _empty_/O.x.
         |[1:13..1:15): :: => scala/package.`::`.
         |[1:16..1:17): y <= _empty_/O.y.
         |[1:20..1:24): List => scala/collection/immutable/List.
         |[2:8..2:10): :: => scala/package.`::`.
         |[2:15..2:19): List => scala/collection/immutable/List.
         |[3:8..3:10): :: => scala/package.`::`.
         |[3:11..3:12): v <= local2
         |[3:15..3:19): List => scala/collection/immutable/List.
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:8..1:10): :: => scala/package.`::`.
         |[1:11..1:12): x <= _empty_/O.x.
         |[1:13..1:15): :: => scala/package.`::`.
         |[1:16..1:17): y <= _empty_/O.y.
         |[1:20..1:24): List => scala/package.List.
         |[2:8..2:10): :: => scala/package.`::`.
         |[2:15..2:19): List => scala/package.List.
         |[3:8..3:10): :: => scala/package.`::`.
         |[3:11..3:12): v <= local2
         |[3:15..3:19): List => scala/package.List.
         |""".stripMargin,
  )

  // a typed underscore that starts an infix value pattern
  private val typedInfixVal =
    """|object O {
       |  val (_: Int) :: x = List(1)
       |}
       |""".stripMargin

  occurrences(
    typedInfixVal,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:10..1:13): Int => scala/Int#
         |[1:15..1:17): :: => scala/package.`::`.
         |[1:18..1:19): x <= local0
         |[1:22..1:26): List => scala/collection/immutable/List.
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:10..1:13): Int => scala/Int#
         |[1:15..1:17): :: => scala/package.`::`.
         |[1:18..1:19): x <= local0
         |[1:22..1:26): List => scala/package.List.
         |""".stripMargin,
  )

  // a parenthesized typed underscore value pattern
  private val typedValPattern =
    """|object O {
       |  val (_: Int) = 1
       |}
       |""".stripMargin

  symbols(
    typedValPattern,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O._. => val method _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.x$1. => synthetic val method x$1: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  occurrences(
    typedValPattern,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:10..1:13): Int => scala/Int#
       |""".stripMargin,
  )

  // an underscore that starts an infix case pattern
  private val infixCase =
    """|object O {
       |  List(1) match { case _ :: x => x }
       |}
       |""".stripMargin

  occurrences(
    infixCase,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:2..1:6): List => scala/collection/immutable/List.
         |[1:25..1:27): :: => scala/package.`::`.
         |[1:28..1:29): x <= local0
         |[1:33..1:34): x => local0
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:2..1:6): List => scala/package.List.
         |[1:25..1:27): :: => scala/package.`::`.
         |[1:28..1:29): x <= local0
         |[1:33..1:34): x => local0
         |""".stripMargin,
  )

  // an underscore that starts an infix value pattern in a for
  private val infixForValue =
    """|object O {
       |  for (x <- List(1); _ :: y = List(x)) yield y
       |}
       |""".stripMargin

  symbols(
    infixForValue,
    """|_empty_/O. => final object O extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local2 => val local x$1: ::[Int]
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |local3 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local4 => synthetic val local x$2: Tuple2[::[Int], List[Int]]
       |  Tuple2 => scala/Tuple2#
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
       |""".stripMargin,
  )

  occurrences(
    infixForValue,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Full("2.13.15") ->
          """|[0:7..0:8): O <= _empty_/O.
             |[1:7..1:8): x <= local0
             |[1:12..1:16): List => scala/package.List.
             |[1:23..1:25): :: => local2
             |[1:26..1:27): y <= local3
             |[1:30..1:34): List => scala/package.List.
             |[1:35..1:36): x => local0
             |[1:45..1:46): y => local1
             |""".stripMargin,
        ScalaVersion.Scala212 ->
          """|[0:7..0:8): O <= _empty_/O.
             |[1:7..1:8): x <= local0
             |[1:12..1:16): List => scala/collection/immutable/List.
             |[1:23..1:25): :: => local2
             |[1:26..1:27): y <= local3
             |[1:30..1:34): List => scala/collection/immutable/List.
             |[1:35..1:36): x => local0
             |[1:45..1:46): y => local1
             |""".stripMargin,
      ),
      """|[0:7..0:8): O <= _empty_/O.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:23..1:25): :: => local2
         |[1:26..1:27): y <= local3
         |[1:30..1:34): List => scala/package.List.
         |[1:35..1:36): x => local0
         |[1:45..1:46): y => local3
         |""".stripMargin,
    ),
  )

  // an underscore generator pattern with guards
  private val guardedGenerators =
    """|object O {
       |  for (_ <- Option(1) if true if false) yield 1
       |  for (_ <- Option(2) if true) ()
       |}
       |""".stripMargin

  symbols(
    guardedGenerators,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => param _: Int
         |  Int => scala/Int#
         |local1 => param _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin,
  )

  occurrences(
    guardedGenerators,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:12..1:18): Option => scala/Option.
       |[2:12..2:18): Option => scala/Option.
       |""".stripMargin,
  )

  synthetics(
    guardedGenerators,
    if (ScalaVersion.is212)
      """|[1:2..1:47): for (_ <- Option(1) if true if false) yield 1 => orig(Option(1)).withFilter({(_) => orig(true)}).withFilter({(_) => orig(false)}).map[Int]({(_) => orig(1)})
         |  withFilter => scala/Option#withFilter().
         |  _ => local0
         |  withFilter => scala/Option#WithFilter#withFilter().
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |[1:12..1:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[2:2..2:33): for (_ <- Option(2) if true) () => orig(Option(2)).withFilter({(_) => orig(true)}).foreach[Unit]({(_) => orig(())})
         |  withFilter => scala/Option#withFilter().
         |  _ => local1
         |  foreach => scala/Option#WithFilter#foreach().
         |  Unit => scala/Unit#
         |[2:12..2:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:47): for (_ <- Option(1) if true if false) yield 1 => orig(Option(1)).withFilter({(local0) => orig(true)}).withFilter({(local1) => orig(false)}).map[Int]({(local2) => orig(1)})
         |  withFilter => scala/Option#withFilter().
         |  local0 => local0
         |  withFilter => scala/Option#WithFilter#withFilter().
         |  local1 => local1
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |  local2 => local2
         |[1:12..1:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[2:2..2:33): for (_ <- Option(2) if true) () => orig(Option(2)).withFilter({(local3) => orig(true)}).foreach[Unit]({(local4) => orig(())})
         |  withFilter => scala/Option#withFilter().
         |  local3 => local3
         |  foreach => scala/Option#WithFilter#foreach().
         |  Unit => scala/Unit#
         |  local4 => local4
         |[2:12..2:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  // an underscore generator pattern and a value definition
  private val generatorValue =
    """|object O {
       |  for (_ <- Option(1); y = 2) yield y
       |}
       |""".stripMargin

  symbols(
    generatorValue,
    """|_empty_/O. => final object O extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local2 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    generatorValue,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Full("2.13.15") ->
          """|[0:7..0:8): O <= _empty_/O.
             |[1:12..1:18): Option => scala/Option.
             |[1:23..1:24): y <= local2
             |[1:36..1:37): y => local1
             |""".stripMargin,
        ScalaVersion.Scala212 ->
          """|[0:7..0:8): O <= _empty_/O.
             |[1:12..1:18): Option => scala/Option.
             |[1:23..1:24): y <= local2
             |[1:36..1:37): y => local1
             |""".stripMargin,
      ),
      """|[0:7..0:8): O <= _empty_/O.
         |[1:12..1:18): Option => scala/Option.
         |[1:23..1:24): y <= local2
         |[1:36..1:37): y => local2
         |""".stripMargin,
    ),
  )

  // an underscore generator pattern after a value definition
  private val valueGenerator =
    """|object O {
       |  for (x <- Option(1); y = x; _ <- Option(y)) yield y
       |}
       |""".stripMargin

  symbols(
    valueGenerator,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local2 => param _: Int
         |  Int => scala/Int#
         |local3 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local3 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    valueGenerator,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:7..1:8): x <= local0
       |[1:12..1:18): Option => scala/Option.
       |[1:23..1:24): y <= local3
       |[1:27..1:28): x => local0
       |[1:35..1:41): Option => scala/Option.
       |[1:42..1:43): y => local3
       |[1:52..1:53): y => local3
       |""".stripMargin,
  )

  // an underscore generator pattern after an underscore value definition
  private val underscoreValueGenerator =
    """|object O {
       |  for (x <- Option(1); _ = x; _ <- Option(x)) yield x
       |}
       |""".stripMargin

  symbols(
    underscoreValueGenerator,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local2 => param _: Int
         |  Int => scala/Int#
         |local3 => val local x$1: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local3 => val local x$1: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    underscoreValueGenerator,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:7..1:8): x <= local0
       |[1:12..1:18): Option => scala/Option.
       |[1:27..1:28): x => local0
       |[1:35..1:41): Option => scala/Option.
       |[1:42..1:43): x => local0
       |[1:52..1:53): x => local0
       |""".stripMargin,
  )

  // a guarded underscore generator pattern after a value definition
  private val guardedValueGenerator =
    """|object O {
       |  for (x <- Option(1); y = x; _ <- Option(y) if y > 0) yield y
       |}
       |""".stripMargin

  symbols(
    guardedValueGenerator,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local2 => param _: Int
         |  Int => scala/Int#
         |local3 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local4 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    guardedValueGenerator,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Full("2.13.15") ->
          """|[0:7..0:8): O <= _empty_/O.
             |[1:7..1:8): x <= local0
             |[1:12..1:18): Option => scala/Option.
             |[1:23..1:24): y <= local4
             |[1:27..1:28): x => local0
             |[1:35..1:41): Option => scala/Option.
             |[1:42..1:43): y => local4
             |[1:48..1:49): y => local4
             |[1:50..1:51): > => scala/Int#`>`(+3).
             |[1:61..1:62): y => local3
             |""".stripMargin,
        ScalaVersion.Scala212 ->
          """|[0:7..0:8): O <= _empty_/O.
             |[1:7..1:8): x <= local0
             |[1:12..1:18): Option => scala/Option.
             |[1:23..1:24): y <= local3
             |[1:27..1:28): x => local0
             |[1:35..1:41): Option => scala/Option.
             |[1:42..1:43): y => local3
             |[1:48..1:49): y => local3
             |[1:50..1:51): > => scala/Int#`>`(+3).
             |[1:61..1:62): y => local3
             |""".stripMargin,
      ),
      """|[0:7..0:8): O <= _empty_/O.
         |[1:7..1:8): x <= local0
         |[1:12..1:18): Option => scala/Option.
         |[1:23..1:24): y <= local4
         |[1:27..1:28): x => local0
         |[1:35..1:41): Option => scala/Option.
         |[1:42..1:43): y => local4
         |[1:48..1:49): y => local4
         |[1:50..1:51): > => scala/Int#`>`(+3).
         |[1:61..1:62): y => local4
         |""".stripMargin,
    ),
  )

  // a typed underscore generator pattern after a value definition
  private val typedValueGenerator =
    """|object O {
       |  for (x <- Option(1); y = x; _: Int <- Option(y)) yield y
       |}
       |""".stripMargin

  symbols(
    typedValueGenerator,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local3 => param _: Int
         |  Int => scala/Int#
         |local4 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local4 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    typedValueGenerator,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:7..1:8): x <= local0
       |[1:12..1:18): Option => scala/Option.
       |[1:23..1:24): y <= local4
       |[1:27..1:28): x => local0
       |[1:33..1:36): Int => scala/Int#
       |[1:40..1:46): Option => scala/Option.
       |[1:47..1:48): y => local4
       |[1:57..1:58): y => local4
       |""".stripMargin,
  )

  // an underscore generator pattern after a value definition in a for loop
  private val valueGeneratorForeach =
    """|object O {
       |  for (x <- Option(1); y = x; _ <- Option(y)) println(y)
       |}
       |""".stripMargin

  symbols(
    valueGeneratorForeach,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local2 => param _: Int
         |  Int => scala/Int#
         |local3 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local3 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    valueGeneratorForeach,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:7..1:8): x <= local0
       |[1:12..1:18): Option => scala/Option.
       |[1:23..1:24): y <= local3
       |[1:27..1:28): x => local0
       |[1:35..1:41): Option => scala/Option.
       |[1:42..1:43): y => local3
       |[1:46..1:53): println => scala/Predef.println(+1).
       |[1:54..1:55): y => local3
       |""".stripMargin,
  )

  // underscore generator patterns around a value definition
  private val generatorValueGenerator =
    """|object O {
       |  for (_ <- Option(1); y = 2; _ <- Option(y)) yield y
       |}
       |""".stripMargin

  symbols(
    generatorValueGenerator,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local2 => param _: Int
         |  Int => scala/Int#
         |local3 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local3 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    generatorValueGenerator,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:12..1:18): Option => scala/Option.
       |[1:23..1:24): y <= local3
       |[1:35..1:41): Option => scala/Option.
       |[1:42..1:43): y => local3
       |[1:52..1:53): y => local3
       |""".stripMargin,
  )

  // underscore generator patterns after value definitions
  private val valuesGenerators =
    """|object O {
       |  for (x <- Option(1); y = x; _ <- Option(y); z = y; _ <- Option(z)) yield z
       |}
       |""".stripMargin

  symbols(
    valuesGenerators,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local4 => param _: Int
         |  Int => scala/Int#
         |local5 => val local y: Int
         |  Int => scala/Int#
         |local6 => val local z: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local5 => val local y: Int
         |  Int => scala/Int#
         |local6 => val local z: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    valuesGenerators,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:7..1:8): x <= local0
       |[1:12..1:18): Option => scala/Option.
       |[1:23..1:24): y <= local5
       |[1:27..1:28): x => local0
       |[1:35..1:41): Option => scala/Option.
       |[1:42..1:43): y => local5
       |[1:46..1:47): z <= local6
       |[1:50..1:51): y => local5
       |[1:58..1:64): Option => scala/Option.
       |[1:65..1:66): z => local6
       |[1:75..1:76): z => local6
       |""".stripMargin,
  )

  // an underscore generator pattern after a guarded value definition
  private val guardedValueThenGenerator =
    """|object O {
       |  for (x <- Option(1); y = x if y > 0; _ <- Option(y)) yield y
       |}
       |""".stripMargin

  symbols(
    guardedValueThenGenerator,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local3 => param _: Int
         |  Int => scala/Int#
         |local4 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local4 => val local y: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    guardedValueThenGenerator,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:7..1:8): x <= local0
       |[1:12..1:18): Option => scala/Option.
       |[1:23..1:24): y <= local4
       |[1:27..1:28): x => local0
       |[1:32..1:33): y => local4
       |[1:34..1:35): > => scala/Int#`>`(+3).
       |[1:44..1:50): Option => scala/Option.
       |[1:51..1:52): y => local4
       |[1:61..1:62): y => local4
       |""".stripMargin,
  )

  // an underscore placeholder in a call of withFilter
  private val withFilterPlaceholder =
    """|object O {
       |  Option(1).withFilter(_ > 0)
       |}
       |""".stripMargin

  symbols(
    withFilterPlaceholder,
    """|_empty_/O. => final object O extends AnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    withFilterPlaceholder,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:2..1:8): Option => scala/Option.
       |[1:12..1:22): withFilter => scala/Option#withFilter().
       |[1:25..1:26): > => scala/Int#`>`(+3).
       |""".stripMargin,
  )

  // an underscore placeholder before a right-associative operator
  private val rightAssocPlaceholder =
    """|object O {
       |  List(1).map(_ :: Nil)
       |}
       |""".stripMargin

  symbols(
    rightAssocPlaceholder,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => synthetic val local x$2: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => synthetic final val local rassoc$1: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    rightAssocPlaceholder,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:2..1:6): List => scala/collection/immutable/List.
         |[1:10..1:13): map => scala/collection/immutable/List#map().
         |[1:16..1:18): :: => scala/collection/immutable/List#`::`().
         |[1:19..1:22): Nil => scala/collection/immutable/Nil.
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:2..1:6): List => scala/package.List.
         |[1:10..1:13): map => scala/collection/immutable/List#map().
         |[1:16..1:18): :: => scala/collection/immutable/List#`::`().
         |[1:19..1:22): Nil => scala/package.Nil.
         |""".stripMargin,
  )

  // underscore type arguments in declared types
  private val declaredTypes =
    """|object O {
       |  val v: List[_] = Nil
       |  var w: List[_] = Nil
       |  lazy val z: List[_] = Nil
       |  def c(a: Any) = a.asInstanceOf[List[_]]
       |  def o: Option[List[_]] = None
       |}
       |""".stripMargin

  symbols(
    declaredTypes,
    """|_empty_/O. => final object O extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.`w_=`(). => var method w_=(x$1: List[_] forSome { type _ }): Unit
       |  x$1 => _empty_/O.`w_=`().(x$1)
       |  List => scala/package.List#
       |  _ => local1
       |  Unit => scala/Unit#
       |_empty_/O.`w_=`().(x$1) => synthetic param x$1: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local1
       |_empty_/O.c(). => method c(a: Any): List[Any]
       |  a => _empty_/O.c().(a)
       |  Any => scala/Any#
       |  List => scala/collection/immutable/List#
       |_empty_/O.c().(a) => param a: Any
       |  Any => scala/Any#
       |_empty_/O.o(). => method o: Option[List[_] forSome { type _ }]
       |  Option => scala/Option#
       |  List => scala/package.List#
       |  _ => local3
       |_empty_/O.v. => val method v: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local0
       |_empty_/O.w(). => var method w: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local1
       |_empty_/O.z. => lazy val method z: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local2
       |""".stripMargin,
  )

  occurrences(
    declaredTypes,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): v <= _empty_/O.v.
         |[1:9..1:13): List => scala/package.List#
         |[1:19..1:22): Nil => scala/collection/immutable/Nil.
         |[2:6..2:7): w <= _empty_/O.w().
         |[2:9..2:13): List => scala/package.List#
         |[2:19..2:22): Nil => scala/collection/immutable/Nil.
         |[3:11..3:12): z <= _empty_/O.z.
         |[3:14..3:18): List => scala/package.List#
         |[3:24..3:27): Nil => scala/collection/immutable/Nil.
         |[4:6..4:7): c <= _empty_/O.c().
         |[4:8..4:9): a <= _empty_/O.c().(a)
         |[4:11..4:14): Any => scala/Any#
         |[4:18..4:19): a => _empty_/O.c().(a)
         |[4:20..4:32): asInstanceOf => scala/Any#asInstanceOf().
         |[4:33..4:37): List => scala/package.List#
         |[5:6..5:7): o <= _empty_/O.o().
         |[5:9..5:15): Option => scala/Option#
         |[5:16..5:20): List => scala/package.List#
         |[5:27..5:31): None => scala/None.
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): v <= _empty_/O.v.
         |[1:9..1:13): List => scala/package.List#
         |[1:19..1:22): Nil => scala/package.Nil.
         |[2:6..2:7): w <= _empty_/O.w().
         |[2:9..2:13): List => scala/package.List#
         |[2:19..2:22): Nil => scala/package.Nil.
         |[3:11..3:12): z <= _empty_/O.z.
         |[3:14..3:18): List => scala/package.List#
         |[3:24..3:27): Nil => scala/package.Nil.
         |[4:6..4:7): c <= _empty_/O.c().
         |[4:8..4:9): a <= _empty_/O.c().(a)
         |[4:11..4:14): Any => scala/Any#
         |[4:18..4:19): a => _empty_/O.c().(a)
         |[4:20..4:32): asInstanceOf => scala/Any#asInstanceOf().
         |[4:33..4:37): List => scala/package.List#
         |[5:6..5:7): o <= _empty_/O.o().
         |[5:9..5:15): Option => scala/Option#
         |[5:16..5:20): List => scala/package.List#
         |[5:27..5:31): None => scala/None.
         |""".stripMargin,
  )

  // an underscore type argument in an abstract value
  private val abstractVal =
    """|trait T {
       |  val a: List[_]
       |}
       |""".stripMargin

  symbols(
    abstractVal,
    """|_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#a. => abstract val method a: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local0
       |""".stripMargin,
  )

  occurrences(
    abstractVal,
    """|[0:6..0:7): T <= _empty_/T#
       |[1:6..1:7): a <= _empty_/T#a.
       |[1:9..1:13): List => scala/package.List#
       |""".stripMargin,
  )

  // an underscore type argument in a case class field
  private val caseClassField =
    """|case class K(a: List[_])
       |""".stripMargin

  symbols(
    caseClassField,
    if (ScalaVersion.is212)
      """|_empty_/K# => case class K extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |_empty_/K#`<init>`(). => primary ctor <init>(val a: List[_] forSome { type _ })
         |  a => _empty_/K#`<init>`().(a)
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K#`<init>`().(a) => val param a: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K#a. => val method a: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/K#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/K#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/K#copy$default$1(). => synthetic method copy$default$1: List[Any] @uncheckedVariance
         |  List => scala/collection/immutable/List#
         |  Any => scala/Any#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/K#copy(). => synthetic method copy(a: List[_] forSome { type _ }): K
         |  a => _empty_/K#copy().(a)
         |  List => scala/package.List#
         |  _ => local0
         |  K => _empty_/K#
         |_empty_/K#copy().(a) => default param a: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/K#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/K#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/K#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/K#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/K#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/K#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/K#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/K#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/K#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/K#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/K. => synthetic final object K extends AbstractFunction1[List[_] forSome { type _ }, K] with Serializable { +4 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  List => scala/package.List#
         |  _ => local0
         |  K => _empty_/K#
         |  Serializable => scala/Serializable#
         |_empty_/K.apply(). => synthetic method apply(a: List[_] forSome { type _ }): K <: scala/Function1#apply().
         |  a => _empty_/K.apply().(a)
         |  List => scala/package.List#
         |  _ => local0
         |  K => _empty_/K#
         |_empty_/K.apply().(a) => param a: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K.readResolve(). => private synthetic method readResolve(): Object
         |  Object => java/lang/Object#
         |_empty_/K.toString(). => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/K.unapply(). => synthetic method unapply(x$0: K): Option[List[_] forSome { type _ }]
         |  x$0 => _empty_/K.unapply().(x$0)
         |  K => _empty_/K#
         |  Option => scala/Option#
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K.unapply().(x$0) => synthetic param x$0: K
         |  K => _empty_/K#
         |local1 => synthetic val local K$1: K
         |  K => _empty_/K#
         |""".stripMargin
    else
      """|_empty_/K# => case class K extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |_empty_/K#`<init>`(). => primary ctor <init>(val a: List[_] forSome { type _ })
         |  a => _empty_/K#`<init>`().(a)
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K#`<init>`().(a) => val param a: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K#a. => val method a: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/K#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/K#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/K#copy$default$1(). => synthetic method copy$default$1: List[Any] @uncheckedVariance
         |  List => scala/collection/immutable/List#
         |  Any => scala/Any#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/K#copy(). => synthetic method copy(a: List[_] forSome { type _ }): K
         |  a => _empty_/K#copy().(a)
         |  List => scala/package.List#
         |  _ => local0
         |  K => _empty_/K#
         |_empty_/K#copy().(a) => default param a: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/K#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/K#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/K#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/K#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/K#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/K#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/K#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/K#productElementName(). => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => _empty_/K#productElementName().(x$1)
         |  Int => scala/Int#
         |  String => java/lang/String#
         |_empty_/K#productElementName().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/K#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/K#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/K#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/K. => synthetic final object K extends AbstractFunction1[List[_] forSome { type _ }, K] with Serializable { +4 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  List => scala/package.List#
         |  _ => local0
         |  K => _empty_/K#
         |  Serializable => java/io/Serializable#
         |_empty_/K.apply(). => synthetic method apply(a: List[_] forSome { type _ }): K <: scala/Function1#apply().
         |  a => _empty_/K.apply().(a)
         |  List => scala/package.List#
         |  _ => local0
         |  K => _empty_/K#
         |_empty_/K.apply().(a) => param a: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K.toString(). => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/K.unapply(). => synthetic method unapply(x$0: K): Option[List[_] forSome { type _ }]
         |  x$0 => _empty_/K.unapply().(x$0)
         |  K => _empty_/K#
         |  Option => scala/Option#
         |  List => scala/package.List#
         |  _ => local0
         |_empty_/K.unapply().(x$0) => synthetic param x$0: K
         |  K => _empty_/K#
         |_empty_/K.writeReplace(). => private synthetic method writeReplace(): Object
         |  Object => java/lang/Object#
         |local1 => synthetic val local K$1: K
         |  K => _empty_/K#
         |""".stripMargin,
  )

  occurrences(
    caseClassField,
    """|[0:11..0:12): K <= _empty_/K#
       |[0:12..0:12):  <= _empty_/K#`<init>`().
       |[0:13..0:14): a <= _empty_/K#`<init>`().(a)
       |[0:13..0:14): a <= _empty_/K#a.
       |[0:16..0:20): List => scala/package.List#
       |""".stripMargin,
  )

  // an underscore type argument in a local lazy value
  private val localLazyVal =
    """|object O {
       |  def m = {
       |    lazy val l: List[_] = Nil
       |    l
       |  }
       |}
       |""".stripMargin

  symbols(
    localLazyVal,
    """|_empty_/O. => final object O extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.m(). => method m: List[Any]
       |  List => scala/collection/immutable/List#
       |  Any => scala/Any#
       |local0 => lazy val local l: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local1
       |""".stripMargin,
  )

  occurrences(
    localLazyVal,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): m <= _empty_/O.m().
         |[2:13..2:14): l <= local0
         |[2:16..2:20): List => scala/package.List#
         |[2:26..2:29): Nil => scala/collection/immutable/Nil.
         |[3:4..3:5): l => local0
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): m <= _empty_/O.m().
         |[2:13..2:14): l <= local0
         |[2:16..2:20): List => scala/package.List#
         |[2:26..2:29): Nil => scala/package.Nil.
         |[3:4..3:5): l => local0
         |""".stripMargin,
  )

  // an underscore type argument in a self type
  private val selfType =
    """|trait T { this: Map[_, Int] =>
       |}
       |""".stripMargin

  symbols(
    selfType,
    """|_empty_/T# => trait T extends AnyRef { self: Map[_, Int] forSome { type _ } =>  }
       |  AnyRef => scala/AnyRef#
       |  Map => scala/Predef.Map#
       |  _ => local0
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    selfType,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:16..0:19): Map => scala/Predef.Map#
       |[0:23..0:26): Int => scala/Int#
       |""".stripMargin,
  )

  // underscore placeholders in super calls
  private val superCalls =
    """|class A(f: Int => Int)
       |class B extends A(_ + 1)
       |class C extends A(_ => 1)
       |object O {
       |  new A(_ + 1) {}
       |}
       |""".stripMargin

  symbols(
    superCalls,
    """|_empty_/A# => class A extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/A#`<init>`(). => primary ctor <init>(f: Function1[Int, Int])
       |  f => _empty_/A#`<init>`().(f)
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/A#`<init>`().(f) => param f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/A#f. => private[this] val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/B# => class B extends A { +1 decls }
       |  A => _empty_/A#
       |_empty_/B#`<init>`(). => primary ctor <init>()
       |_empty_/C# => class C extends A { +1 decls }
       |  A => _empty_/A#
       |_empty_/C#`<init>`(). => primary ctor <init>()
       |_empty_/O. => final object O extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => final class $anon extends A
       |  A => _empty_/A#
       |""".stripMargin,
  )

  occurrences(
    superCalls,
    """|[0:6..0:7): A <= _empty_/A#
       |[0:7..0:7):  <= _empty_/A#`<init>`().
       |[0:8..0:9): f <= _empty_/A#`<init>`().(f)
       |[0:8..0:9): f <= _empty_/A#f.
       |[0:11..0:14): Int => scala/Int#
       |[0:18..0:21): Int => scala/Int#
       |[1:6..1:7): B <= _empty_/B#
       |[1:8..1:8):  <= _empty_/B#`<init>`().
       |[1:16..1:17): A => _empty_/A#
       |[1:17..1:17):  => _empty_/A#`<init>`().
       |[1:20..1:21): + => scala/Int#`+`(+4).
       |[2:6..2:7): C <= _empty_/C#
       |[2:8..2:8):  <= _empty_/C#`<init>`().
       |[2:16..2:17): A => _empty_/A#
       |[2:17..2:17):  => _empty_/A#`<init>`().
       |[3:7..3:8): O <= _empty_/O.
       |[4:6..4:6):  <= local0
       |[4:6..4:7): A => _empty_/A#
       |[4:7..4:7):  => _empty_/A#`<init>`().
       |[4:10..4:11): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  // placeholders in the super calls of an object, of a typed parent and by name
  private val superCallArgs =
    """|class A(f: Int => Int)(g: Int => Int)
       |object P extends A(_ + 1)(_ + 2)
       |class Q[T](f: T => T)
       |class R extends Q[Int](_ + 1)
       |class S(f: Int => Int, g: Int = 1)
       |class T extends S(g = 2, f = _ + 1)
       |""".stripMargin

  symbols(
    superCallArgs,
    if (ScalaVersion.is212)
      """|_empty_/A# => class A extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/A#`<init>`(). => primary ctor <init>(f: Function1[Int, Int])(g: Function1[Int, Int])
         |  f => _empty_/A#`<init>`().(f)
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  g => _empty_/A#`<init>`().(g)
         |_empty_/A#`<init>`().(f) => param f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/A#`<init>`().(g) => param g: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/A#f. => private[this] val method f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/A#g. => private[this] val method g: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/P. => final object P extends A
         |  A => _empty_/A#
         |_empty_/Q# => class Q[T] extends AnyRef { +2 decls }
         |  T => _empty_/Q#[T]
         |  AnyRef => scala/AnyRef#
         |_empty_/Q#[T] => typeparam T
         |_empty_/Q#`<init>`(). => primary ctor <init>(f: Function1[T, T])
         |  f => _empty_/Q#`<init>`().(f)
         |  Function1 => scala/Function1#
         |  T => _empty_/Q#[T]
         |_empty_/Q#`<init>`().(f) => param f: Function1[T, T]
         |  Function1 => scala/Function1#
         |  T => _empty_/Q#[T]
         |_empty_/Q#f. => private[this] val method f: Function1[T, T]
         |  Function1 => scala/Function1#
         |  T => _empty_/Q#[T]
         |_empty_/R# => class R extends Q[Int] { +1 decls }
         |  Q => _empty_/Q#
         |  Int => scala/Int#
         |_empty_/R#`<init>`(). => primary ctor <init>()
         |_empty_/S# => class S extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#`<init>`(). => primary ctor <init>(f: Function1[Int, Int], g: Int)
         |  f => _empty_/S#`<init>`().(f)
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  g => _empty_/S#`<init>`().(g)
         |_empty_/S#`<init>`().(f) => param f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/S#`<init>`().(g) => default param g: Int
         |  Int => scala/Int#
         |_empty_/S#f. => private[this] val method f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/S#g. => private[this] val method g: Int
         |  Int => scala/Int#
         |_empty_/S. => synthetic final object S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S.`<init>$default$2`(). => synthetic method <init>$default$2: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/T# => class T extends S { +1 decls }
         |  S => _empty_/S#
         |_empty_/T#`<init>`(). => primary ctor <init>()
         |local0 => val local x$1: Int
         |  Int => scala/Int#
         |local1 => val local x$2: Function1[Int, Int] @uncheckedBounds
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  uncheckedBounds => scala/reflect/internal/annotations/uncheckedBounds#
         |""".stripMargin
    else
      """|_empty_/A# => class A extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/A#`<init>`(). => primary ctor <init>(f: Function1[Int, Int])(g: Function1[Int, Int])
         |  f => _empty_/A#`<init>`().(f)
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  g => _empty_/A#`<init>`().(g)
         |_empty_/A#`<init>`().(f) => param f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/A#`<init>`().(g) => param g: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/A#f. => private[this] val method f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/A#g. => private[this] val method g: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/P. => final object P extends A
         |  A => _empty_/A#
         |_empty_/Q# => class Q[T] extends AnyRef { +2 decls }
         |  T => _empty_/Q#[T]
         |  AnyRef => scala/AnyRef#
         |_empty_/Q#[T] => typeparam T
         |_empty_/Q#`<init>`(). => primary ctor <init>(f: Function1[T, T])
         |  f => _empty_/Q#`<init>`().(f)
         |  Function1 => scala/Function1#
         |  T => _empty_/Q#[T]
         |_empty_/Q#`<init>`().(f) => param f: Function1[T, T]
         |  Function1 => scala/Function1#
         |  T => _empty_/Q#[T]
         |_empty_/Q#f. => private[this] val method f: Function1[T, T]
         |  Function1 => scala/Function1#
         |  T => _empty_/Q#[T]
         |_empty_/R# => class R extends Q[Int] { +1 decls }
         |  Q => _empty_/Q#
         |  Int => scala/Int#
         |_empty_/R#`<init>`(). => primary ctor <init>()
         |_empty_/S# => class S extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#`<init>`(). => primary ctor <init>(f: Function1[Int, Int], g: Int)
         |  f => _empty_/S#`<init>`().(f)
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  g => _empty_/S#`<init>`().(g)
         |_empty_/S#`<init>`().(f) => param f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/S#`<init>`().(g) => default param g: Int
         |  Int => scala/Int#
         |_empty_/S#f. => private[this] val method f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/S#g. => private[this] val method g: Int
         |  Int => scala/Int#
         |_empty_/S. => synthetic final object S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S.`<init>$default$2`(). => synthetic method <init>$default$2: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/T# => class T extends S { +1 decls }
         |  S => _empty_/S#
         |_empty_/T#`<init>`(). => primary ctor <init>()
         |local0 => val local x$1: 2
         |local1 => val local x$2: Function1[Int, Int] @uncheckedBounds
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  uncheckedBounds => scala/reflect/internal/annotations/uncheckedBounds#
         |""".stripMargin,
  )

  occurrences(
    superCallArgs,
    """|[0:6..0:7): A <= _empty_/A#
       |[0:7..0:7):  <= _empty_/A#`<init>`().
       |[0:8..0:9): f <= _empty_/A#`<init>`().(f)
       |[0:8..0:9): f <= _empty_/A#f.
       |[0:11..0:14): Int => scala/Int#
       |[0:18..0:21): Int => scala/Int#
       |[0:23..0:24): g <= _empty_/A#`<init>`().(g)
       |[0:23..0:24): g <= _empty_/A#g.
       |[0:26..0:29): Int => scala/Int#
       |[0:33..0:36): Int => scala/Int#
       |[1:7..1:8): P <= _empty_/P.
       |[1:17..1:18): A => _empty_/A#
       |[1:18..1:18):  => _empty_/A#`<init>`().
       |[1:21..1:22): + => scala/Int#`+`(+4).
       |[1:28..1:29): + => scala/Int#`+`(+4).
       |[2:6..2:7): Q <= _empty_/Q#
       |[2:8..2:9): T <= _empty_/Q#[T]
       |[2:10..2:10):  <= _empty_/Q#`<init>`().
       |[2:11..2:12): f <= _empty_/Q#`<init>`().(f)
       |[2:11..2:12): f <= _empty_/Q#f.
       |[2:14..2:15): T => _empty_/Q#[T]
       |[2:19..2:20): T => _empty_/Q#[T]
       |[3:6..3:7): R <= _empty_/R#
       |[3:8..3:8):  <= _empty_/R#`<init>`().
       |[3:16..3:17): Q => _empty_/Q#
       |[3:18..3:21): Int => scala/Int#
       |[3:22..3:22):  => _empty_/Q#`<init>`().
       |[3:25..3:26): + => scala/Int#`+`(+4).
       |[4:6..4:7): S <= _empty_/S#
       |[4:7..4:7):  <= _empty_/S#`<init>`().
       |[4:8..4:9): f <= _empty_/S#`<init>`().(f)
       |[4:8..4:9): f <= _empty_/S#f.
       |[4:11..4:14): Int => scala/Int#
       |[4:18..4:21): Int => scala/Int#
       |[4:23..4:24): g <= _empty_/S#`<init>`().(g)
       |[4:23..4:24): g <= _empty_/S#g.
       |[4:26..4:29): Int => scala/Int#
       |[5:6..5:7): T <= _empty_/T#
       |[5:8..5:8):  <= _empty_/T#`<init>`().
       |[5:16..5:17): S => _empty_/S#
       |[5:17..5:17):  => _empty_/S#`<init>`().
       |[5:18..5:19): g => _empty_/S#`<init>`().(g)
       |[5:25..5:26): f => _empty_/S#`<init>`().(f)
       |[5:31..5:32): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  // placeholders in the super calls of local classes
  private val localSuperCalls =
    """|class A(f: Int => Int)
       |object O {
       |  def m = {
       |    class B extends A(_ + 1)
       |    new A(_ => 1) {}
       |  }
       |}
       |""".stripMargin

  symbols(
    localSuperCalls,
    """|_empty_/A# => class A extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/A#`<init>`(). => primary ctor <init>(f: Function1[Int, Int])
       |  f => _empty_/A#`<init>`().(f)
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/A#`<init>`().(f) => param f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/A#f. => private[this] val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/O. => final object O extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.m(). => method m: A
       |  A => _empty_/A#
       |local0 => class B extends A { +1 decls }
       |  A => _empty_/A#
       |local1 => primary ctor <init>()
       |local2 => final class $anon extends A
       |  A => _empty_/A#
       |""".stripMargin,
  )

  occurrences(
    localSuperCalls,
    """|[0:6..0:7): A <= _empty_/A#
       |[0:7..0:7):  <= _empty_/A#`<init>`().
       |[0:8..0:9): f <= _empty_/A#`<init>`().(f)
       |[0:8..0:9): f <= _empty_/A#f.
       |[0:11..0:14): Int => scala/Int#
       |[0:18..0:21): Int => scala/Int#
       |[1:7..1:8): O <= _empty_/O.
       |[2:6..2:7): m <= _empty_/O.m().
       |[3:10..3:11): B <= local0
       |[3:12..3:12):  <= local1
       |[3:20..3:21): A => _empty_/A#
       |[3:21..3:21):  => _empty_/A#`<init>`().
       |[3:24..3:25): + => scala/Int#`+`(+4).
       |[4:8..4:8):  <= local2
       |[4:8..4:9): A => _empty_/A#
       |[4:9..4:9):  => _empty_/A#`<init>`().
       |""".stripMargin,
  )

  // a placeholder in a call of the primary constructor from another one
  private val selfCtorCall =
    """|class A(f: Int => Int) {
       |  def this() = this(_ + 1)
       |}
       |""".stripMargin

  symbols(
    selfCtorCall,
    """|_empty_/A# => class A extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/A#`<init>`(). => primary ctor <init>(f: Function1[Int, Int])
       |  f => _empty_/A#`<init>`().(f)
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/A#`<init>`().(f) => param f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/A#`<init>`(+1). => ctor <init>()
       |_empty_/A#f. => private[this] val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    selfCtorCall,
    """|[0:6..0:7): A <= _empty_/A#
       |[0:7..0:7):  <= _empty_/A#`<init>`().
       |[0:8..0:9): f <= _empty_/A#`<init>`().(f)
       |[0:8..0:9): f <= _empty_/A#f.
       |[0:11..0:14): Int => scala/Int#
       |[0:18..0:21): Int => scala/Int#
       |[1:6..1:10): this <= _empty_/A#`<init>`(+1).
       |[1:19..1:19):  => _empty_/A#`<init>`().
       |[1:22..1:23): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  // a placeholder in an annotation argument
  private val annotationArg =
    """|class ann(f: Int => Int) extends scala.annotation.StaticAnnotation
       |object O {
       |  @ann(_ + 1) def m = 1
       |}
       |""".stripMargin

  symbols(
    annotationArg,
    """|_empty_/O. => final object O extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.m(). => @ann method m: Int
       |  ann => _empty_/ann#
       |  Int => scala/Int#
       |_empty_/ann# => class ann extends Annotation with StaticAnnotation { +2 decls }
       |  Annotation => scala/annotation/Annotation#
       |  StaticAnnotation => scala/annotation/StaticAnnotation#
       |_empty_/ann#`<init>`(). => primary ctor <init>(f: Function1[Int, Int])
       |  f => _empty_/ann#`<init>`().(f)
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/ann#`<init>`().(f) => param f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/ann#f. => private[this] val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    annotationArg,
    """|[0:6..0:9): ann <= _empty_/ann#
       |[0:9..0:9):  <= _empty_/ann#`<init>`().
       |[0:10..0:11): f <= _empty_/ann#`<init>`().(f)
       |[0:10..0:11): f <= _empty_/ann#f.
       |[0:13..0:16): Int => scala/Int#
       |[0:20..0:23): Int => scala/Int#
       |[0:33..0:38): scala => scala/
       |[0:39..0:49): annotation => scala/annotation/
       |[0:50..0:66): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[0:66..0:66):  => scala/annotation/Annotation#`<init>`().
       |[1:7..1:8): O <= _empty_/O.
       |[2:3..2:6): ann => _empty_/ann#
       |[2:6..2:6):  => _empty_/ann#`<init>`().
       |[2:9..2:10): + => scala/Int#`+`(+4).
       |[2:18..2:19): m <= _empty_/O.m().
       |""".stripMargin,
  )

  // placeholders in default arguments
  private val defaultArgs =
    """|class C(f: Int => Int = _ + 1)
       |object O {
       |  def m(f: Int => Int = _ + 1) = f
       |}
       |""".stripMargin

  symbols(
    defaultArgs,
    """|_empty_/C# => class C extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/C#`<init>`(). => primary ctor <init>(f: Function1[Int, Int])
       |  f => _empty_/C#`<init>`().(f)
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/C#`<init>`().(f) => default param f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/C#f. => private[this] val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/C. => synthetic final object C extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/C.`<init>$default$1`(). => synthetic method <init>$default$1: Function1[Int, Int] @uncheckedVariance
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/O. => final object O extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.m$default$1(). => synthetic method m$default$1: Function1[Int, Int] @uncheckedVariance
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/O.m(). => method m(f: Function1[Int, Int]): Function1[Int, Int]
       |  f => _empty_/O.m().(f)
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/O.m().(f) => default param f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    defaultArgs,
    """|[0:6..0:7): C <= _empty_/C#
       |[0:7..0:7):  <= _empty_/C#`<init>`().
       |[0:8..0:9): f <= _empty_/C#`<init>`().(f)
       |[0:8..0:9): f <= _empty_/C#f.
       |[0:11..0:14): Int => scala/Int#
       |[0:18..0:21): Int => scala/Int#
       |[0:26..0:27): + => scala/Int#`+`(+4).
       |[1:7..1:8): O <= _empty_/O.
       |[2:6..2:7): m <= _empty_/O.m().
       |[2:8..2:9): f <= _empty_/O.m().(f)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:21): Int => scala/Int#
       |[2:26..2:27): + => scala/Int#`+`(+4).
       |[2:33..2:34): f => _empty_/O.m().(f)
       |""".stripMargin,
  )

  // a placeholder in a default argument of a case class
  private val caseClassDefaultArg =
    """|case class K(f: Int => Int = _ + 1)
       |""".stripMargin

  occurrences(
    caseClassDefaultArg,
    """|[0:11..0:12): K <= _empty_/K#
       |[0:12..0:12):  <= _empty_/K#`<init>`().
       |[0:13..0:14): f <= _empty_/K#`<init>`().(f)
       |[0:13..0:14): f <= _empty_/K#f.
       |[0:16..0:19): Int => scala/Int#
       |[0:23..0:26): Int => scala/Int#
       |[0:31..0:32): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  // guards after a typed generator and a nested one, and before a generator
  private val guardVariants =
    """|object O {
       |  for (_: Int <- Option(1) if true) yield 1
       |  for (x <- Option(1); _ <- Option(x) if x > 0) yield x
       |  for (_ <- Option(1) if true; y <- Option(2)) yield y
       |}
       |""".stripMargin

  symbols(
    guardVariants,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local1 => param _: Int
         |  Int => scala/Int#
         |local2 => param x: Int
         |  Int => scala/Int#
         |local3 => param _: Int
         |  Int => scala/Int#
         |local4 => param _: Int
         |  Int => scala/Int#
         |local5 => param y: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local3 => param x: Int
         |  Int => scala/Int#
         |local8 => param y: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    guardVariants,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Full("2.13.15") ->
          """|[0:7..0:8): O <= _empty_/O.
             |[1:10..1:13): Int => scala/Int#
             |[1:17..1:23): Option => scala/Option.
             |[2:7..2:8): x <= local3
             |[2:12..2:18): Option => scala/Option.
             |[2:28..2:34): Option => scala/Option.
             |[2:35..2:36): x => local3
             |[2:41..2:42): x => local3
             |[2:43..2:44): > => scala/Int#`>`(+3).
             |[2:54..2:55): x => local5
             |[3:12..3:18): Option => scala/Option.
             |[3:31..3:32): y <= local8
             |[3:36..3:42): Option => scala/Option.
             |[3:53..3:54): y => local8
             |""".stripMargin,
        ScalaVersion.Scala212 ->
          """|[0:7..0:8): O <= _empty_/O.
             |[1:10..1:13): Int => scala/Int#
             |[1:17..1:23): Option => scala/Option.
             |[2:7..2:8): x <= local2
             |[2:12..2:18): Option => scala/Option.
             |[2:28..2:34): Option => scala/Option.
             |[2:35..2:36): x => local2
             |[2:41..2:42): x => local2
             |[2:43..2:44): > => scala/Int#`>`(+3).
             |[2:54..2:55): x => local2
             |[3:12..3:18): Option => scala/Option.
             |[3:31..3:32): y <= local5
             |[3:36..3:42): Option => scala/Option.
             |[3:53..3:54): y => local5
             |""".stripMargin,
      ),
      """|[0:7..0:8): O <= _empty_/O.
         |[1:10..1:13): Int => scala/Int#
         |[1:17..1:23): Option => scala/Option.
         |[2:7..2:8): x <= local3
         |[2:12..2:18): Option => scala/Option.
         |[2:28..2:34): Option => scala/Option.
         |[2:35..2:36): x => local3
         |[2:41..2:42): x => local3
         |[2:43..2:44): > => scala/Int#`>`(+3).
         |[2:54..2:55): x => local3
         |[3:12..3:18): Option => scala/Option.
         |[3:31..3:32): y <= local8
         |[3:36..3:42): Option => scala/Option.
         |[3:53..3:54): y => local8
         |""".stripMargin,
    ),
  )

  synthetics(
    guardVariants,
    if (ScalaVersion.is212)
      """|[1:2..1:43): for (_: Int <- Option(1) if true) yield 1 => orig(Option(1)).withFilter({(local0) => orig()}).withFilter({(_) => orig(true)}).map[Int]({(_) => orig(1)})
         |  withFilter => scala/Option#withFilter().
         |  local0 => local0
         |  withFilter => scala/Option#WithFilter#withFilter().
         |  _ => local1
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |[1:17..1:23): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[2:2..2:55): for (x <- Option(1); _ <- Option(x) if x > 0) yield x => orig(Option(1)).flatMap[Int]({(x) => orig(Option(x)).withFilter({(_) => orig(x > 0)}).map[Int]({(_) => orig(x)})})
         |  flatMap => scala/Option#flatMap().
         |  Int => scala/Int#
         |  x => local2
         |  withFilter => scala/Option#withFilter().
         |  _ => local3
         |  map => scala/Option#WithFilter#map().
         |[2:12..2:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[2:28..2:34): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[3:2..3:54): for (_ <- Option(1) if true; y <- Option(2)) yield y => orig(Option(1)).withFilter({(_) => orig(true)}).flatMap[Int]({(_) => orig(Option(2)).map[Int]({(y) => orig(y)})})
         |  withFilter => scala/Option#withFilter().
         |  _ => local4
         |  flatMap => scala/Option#WithFilter#flatMap().
         |  Int => scala/Int#
         |  map => scala/Option#map().
         |  y => local5
         |[3:12..3:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[3:36..3:42): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:43): for (_: Int <- Option(1) if true) yield 1 => orig(Option(1)).withFilter({(local0) => orig()}).withFilter({(local1) => orig(true)}).map[Int]({(local2) => orig(1)})
         |  withFilter => scala/Option#withFilter().
         |  local0 => local0
         |  withFilter => scala/Option#WithFilter#withFilter().
         |  local1 => local1
         |  map => scala/Option#WithFilter#map().
         |  Int => scala/Int#
         |  local2 => local2
         |[1:17..1:23): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[2:2..2:55): for (x <- Option(1); _ <- Option(x) if x > 0) yield x => orig(Option(1)).flatMap[Int]({(x) => orig(Option(x)).withFilter({(local4) => orig(x > 0)}).map[Int]({(local5) => orig(x)})})
         |  flatMap => scala/Option#flatMap().
         |  Int => scala/Int#
         |  x => local3
         |  withFilter => scala/Option#withFilter().
         |  local4 => local4
         |  map => scala/Option#WithFilter#map().
         |  local5 => local5
         |[2:12..2:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[2:28..2:34): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[3:2..3:54): for (_ <- Option(1) if true; y <- Option(2)) yield y => orig(Option(1)).withFilter({(local6) => orig(true)}).flatMap[Int]({(local7) => orig(Option(2)).map[Int]({(y) => orig(y)})})
         |  withFilter => scala/Option#withFilter().
         |  local6 => local6
         |  flatMap => scala/Option#WithFilter#flatMap().
         |  Int => scala/Int#
         |  local7 => local7
         |  map => scala/Option#map().
         |  y => local8
         |[3:12..3:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |[3:36..3:42): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  // a placeholder in the yield of a guarded generator
  private val guardedYieldPlaceholder =
    """|object O {
       |  for (_ <- Option(1) if true) yield (_: Int) + 1
       |}
       |""".stripMargin

  symbols(
    guardedYieldPlaceholder,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => param _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin,
  )

  occurrences(
    guardedYieldPlaceholder,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:12..1:18): Option => scala/Option.
       |[1:41..1:44): Int => scala/Int#
       |[1:46..1:47): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  synthetics(
    guardedYieldPlaceholder,
    if (ScalaVersion.is212)
      """|[1:2..1:49): for (_ <- Option(1) if true) yield (_: Int) + 1 => orig(Option(1)).withFilter({(_) => orig(true)}).map[Function1[Int, Int]]({(_) => orig((_: Int) + 1)})
         |  withFilter => scala/Option#withFilter().
         |  _ => local0
         |  map => scala/Option#WithFilter#map().
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |[1:12..1:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:49): for (_ <- Option(1) if true) yield (_: Int) + 1 => orig(Option(1)).withFilter({(local0) => orig(true)}).map[Function1[Int, Int]]({(local1) => orig((_: Int) + 1)})
         |  withFilter => scala/Option#withFilter().
         |  local0 => local0
         |  map => scala/Option#WithFilter#map().
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  local1 => local1
         |[1:12..1:18): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  // typed underscore patterns of a var, a local value and a for value
  private val typedVarAndLocals =
    """|object O {
       |  var (_: Int) = 1
       |  def m = {
       |    val (_: Int) = 1
       |    2
       |  }
       |  for (x <- Option(1); (_: Int) = x) yield x
       |}
       |""".stripMargin

  symbols(
    typedVarAndLocals,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O._(). => var method _: Int
         |  Int => scala/Int#
         |_empty_/O.`__=`(). => var method __=(x$1: Int): Unit
         |  x$1 => _empty_/O.`__=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/O.`__=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/O.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => val local _: Int
         |  Int => scala/Int#
         |local1 => val local x: Int
         |  Int => scala/Int#
         |local3 => val local x$1: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.m(). => method m: Int
         |  Int => scala/Int#
         |_empty_/O.x$1. => synthetic val method x$1: Unit
         |  Unit => scala/Unit#
         |local0 => synthetic val local x$2: Unit
         |  Unit => scala/Unit#
         |local1 => val local x: Int
         |  Int => scala/Int#
         |local3 => val local x$3: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    typedVarAndLocals,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Full("2.13.15") ->
          """|[0:7..0:8): O <= _empty_/O.
             |[1:10..1:13): Int => scala/Int#
             |[2:6..2:7): m <= _empty_/O.m().
             |[3:12..3:15): Int => scala/Int#
             |[6:7..6:8): x <= local1
             |[6:12..6:18): Option => scala/Option.
             |[6:27..6:30): Int => scala/Int#
             |[6:34..6:35): x => local1
             |[6:43..6:44): x => local2
             |""".stripMargin,
        ScalaVersion.Scala212 ->
          """|[0:7..0:8): O <= _empty_/O.
             |[1:10..1:13): Int => scala/Int#
             |[2:6..2:7): m <= _empty_/O.m().
             |[3:12..3:15): Int => scala/Int#
             |[6:7..6:8): x <= local1
             |[6:12..6:18): Option => scala/Option.
             |[6:27..6:30): Int => scala/Int#
             |[6:34..6:35): x => local1
             |[6:43..6:44): x => local2
             |""".stripMargin,
      ),
      """|[0:7..0:8): O <= _empty_/O.
         |[1:10..1:13): Int => scala/Int#
         |[2:6..2:7): m <= _empty_/O.m().
         |[3:12..3:15): Int => scala/Int#
         |[6:7..6:8): x <= local1
         |[6:12..6:18): Option => scala/Option.
         |[6:27..6:30): Int => scala/Int#
         |[6:34..6:35): x => local1
         |[6:43..6:44): x => local1
         |""".stripMargin,
    ),
  )

  // an underscore value pattern in parentheses
  private val parenValPattern =
    """|object O {
       |  val (_) = 1
       |}
       |""".stripMargin

  symbols(
    parenValPattern,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O._. => val method _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.x$1. => synthetic val method x$1: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  occurrences(
    parenValPattern,
    """|[0:7..0:8): O <= _empty_/O.
       |""".stripMargin,
  )

  // scalac 2.12 and 2.13.15 resolve the yield's `x` in `parenTypedPatterns` to another local
  private val parenTypedYieldRef = ScalaVersion.getExpected(
    List(ScalaVersion.Full("2.13.15") -> "local4", ScalaVersion.Scala212 -> "local4"),
    "local3",
  )

  // underscore patterns in parentheses with a type outside them
  private val parenTypedPatterns =
    """|object O {
       |  def m = {
       |    val ((_)): Int = 1
       |    2
       |  }
       |  for ((_): Int <- Option(1)) ()
       |  for (x <- Option(1); (_): Int = x) yield x
       |}
       |""".stripMargin

  symbols(
    parenTypedPatterns,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => val local _: Int
         |  Int => scala/Int#
         |local2 => param _: Int
         |  Int => scala/Int#
         |local3 => val local x: Int
         |  Int => scala/Int#
         |local5 => val local x$1: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => synthetic val local x$1: Unit
         |  Unit => scala/Unit#
         |local3 => val local x: Int
         |  Int => scala/Int#
         |local5 => val local x$3: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    parenTypedPatterns,
    s"""|[0:7..0:8): O <= _empty_/O.
        |[1:6..1:7): m <= _empty_/O.m().
        |[2:15..2:18): Int => scala/Int#
        |[5:12..5:15): Int => scala/Int#
        |[5:19..5:25): Option => scala/Option.
        |[6:7..6:8): x <= local3
        |[6:12..6:18): Option => scala/Option.
        |[6:28..6:31): Int => scala/Int#
        |[6:34..6:35): x => local3
        |[6:43..6:44): x => $parenTypedYieldRef
        |""".stripMargin,
  )

  // scalac 2.12 and 2.13.15 resolve the yield's `h` in `parenNames` to another local
  private val parenNamesYieldRef = ScalaVersion.getExpected(
    List(ScalaVersion.Full("2.13.15") -> "local6", ScalaVersion.Scala212 -> "local6"),
    "local7",
  )

  // names in parentheses, in patterns and in terms
  private val parenNames =
    """|object O {
       |  val (a) = 1
       |  val (b): Int = 2
       |  var (c) = 3
       |  def m(x: Any) = x match {
       |    case (d) => d
       |    case e @ (f: Int) => f
       |  }
       |  val ((i), j) = (1, 2)
       |  for ((g) <- Option(1); (h) = g) yield (h)
       |  def n = ((O): Any, (O.a), (a) + (b))
       |}
       |""".stripMargin

  occurrences(
    parenNames,
    s"""|[0:7..0:8): O <= _empty_/O.
        |[1:7..1:8): a <= _empty_/O.a.
        |[2:7..2:8): b <= _empty_/O.b.
        |[2:11..2:14): Int => scala/Int#
        |[3:7..3:8): c <= _empty_/O.c().
        |[4:6..4:7): m <= _empty_/O.m().
        |[4:8..4:9): x <= _empty_/O.m().(x)
        |[4:11..4:14): Any => scala/Any#
        |[4:18..4:19): x => _empty_/O.m().(x)
        |[5:10..5:11): d <= local0
        |[5:16..5:17): d => local0
        |[6:9..6:10): e <= local1
        |[6:14..6:15): f <= local2
        |[6:17..6:20): Int => scala/Int#
        |[6:25..6:26): f => local2
        |[8:8..8:9): i <= _empty_/O.i.
        |[8:12..8:13): j <= _empty_/O.j.
        |[9:8..9:9): g <= local5
        |[9:14..9:20): Option => scala/Option.
        |[9:26..9:27): h <= local7
        |[9:31..9:32): g => local5
        |[9:41..9:42): h => $parenNamesYieldRef
        |[10:6..10:7): n <= _empty_/O.n().
        |[10:12..10:13): O => _empty_/O.
        |[10:16..10:19): Any => scala/Any#
        |[10:22..10:23): O => _empty_/O.
        |[10:24..10:25): a => _empty_/O.a.
        |[10:29..10:30): a => _empty_/O.a.
        |[10:32..10:33): + => scala/Int#`+`(+4).
        |[10:35..10:36): b => _empty_/O.b.
        |""".stripMargin,
  )

  // underscore type arguments of classOf
  private val classOfs =
    """|object O {
       |  val c = classOf[List[_]]
       |  val d = classOf[Map[_, Int]]
       |}
       |""".stripMargin

  symbols(
    classOfs,
    """|_empty_/O. => final object O extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.c. => val method c: Class[List[_] forSome { type _ }]
       |  Class => java/lang/Class#
       |  List => scala/package.List#
       |  _ => local0
       |_empty_/O.d. => val method d: Class[Map[_, Int] forSome { type _ }]
       |  Class => java/lang/Class#
       |  Map => scala/Predef.Map#
       |  _ => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    classOfs,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): c <= _empty_/O.c.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[1:18..1:22): List => scala/package.List#
       |[2:6..2:7): d <= _empty_/O.d.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:21): Map => scala/Predef.Map#
       |[2:25..2:28): Int => scala/Int#
       |""".stripMargin,
  )

  // underscores with bounds, in pairs and nested in classOf
  private val classOfWildcards =
    """|object O {
       |  val e = classOf[Map[_, _]]
       |  val f = classOf[List[_ <: AnyRef]]
       |  val g = classOf[Option[List[_]]]
       |}
       |""".stripMargin

  symbols(
    classOfWildcards,
    """|_empty_/O. => final object O extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.e. => val method e: Class[Map[_, _] forSome { type _; type _ }]
       |  Class => java/lang/Class#
       |  Map => scala/Predef.Map#
       |  _ => local0
       |  _ => local1
       |_empty_/O.f. => val method f: Class[List[_] forSome { type _ <: AnyRef }]
       |  Class => java/lang/Class#
       |  List => scala/package.List#
       |  _ => local2
       |  AnyRef => scala/AnyRef#
       |_empty_/O.g. => val method g: Class[Option[List[_] forSome { type _ }]]
       |  Class => java/lang/Class#
       |  Option => scala/Option#
       |  List => scala/package.List#
       |  _ => local3
       |""".stripMargin,
  )

  occurrences(
    classOfWildcards,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): e <= _empty_/O.e.
       |[1:10..1:17): classOf => scala/Predef.classOf().
       |[1:18..1:21): Map => scala/Predef.Map#
       |[2:6..2:7): f <= _empty_/O.f.
       |[2:10..2:17): classOf => scala/Predef.classOf().
       |[2:18..2:22): List => scala/package.List#
       |[3:6..3:7): g <= _empty_/O.g.
       |[3:10..3:17): classOf => scala/Predef.classOf().
       |[3:18..3:24): Option => scala/Option#
       |[3:25..3:29): List => scala/package.List#
       |""".stripMargin,
  )

  // an underscore type argument of an empty List
  private val emptyList =
    """|object O {
       |  val x = List[List[_]]()
       |}
       |""".stripMargin

  symbols(
    emptyList,
    """|_empty_/O. => final object O extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.x. => val method x: List[List[_] forSome { type _ }]
       |  List => scala/collection/immutable/List#
       |  _ => local0
       |""".stripMargin,
  )

  occurrences(
    emptyList,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:6..1:7): x <= _empty_/O.x.
       |[1:10..1:14): List => scala/collection/immutable/Nil.
       |""".stripMargin,
  )

  // typed underscore value patterns with parentheses or a comment before the colon
  private val parenTypedValues =
    """|object O {
       |  val ((_): Int) = 3
       |  def m = {
       |    val ((_): Int) = 3
       |    4
       |  }
       |}
       |object P {
       |  var ((_): Int) = 3
       |}
       |object Q {
       |  val (_ /* c */: Int) = 1
       |  def m = {
       |    val (_ /* c */: Int) = 3
       |    4
       |  }
       |}
       |""".stripMargin

  symbols(
    parenTypedValues,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O._. => val method _: Int
         |  Int => scala/Int#
         |_empty_/O.m(). => method m: Int
         |  Int => scala/Int#
         |_empty_/P. => final object P extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/P._(). => var method _: Int
         |  Int => scala/Int#
         |_empty_/P.`__=`(). => var method __=(x$1: Int): Unit
         |  x$1 => _empty_/P.`__=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/P.`__=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/Q. => final object Q extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/Q._. => val method _: Int
         |  Int => scala/Int#
         |_empty_/Q.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => val local _: Int
         |  Int => scala/Int#
         |local1 => val local _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.m(). => method m: Int
         |  Int => scala/Int#
         |_empty_/O.x$1. => synthetic val method x$1: Unit
         |  Unit => scala/Unit#
         |_empty_/P. => final object P extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/P.x$3. => synthetic val method x$3: Unit
         |  Unit => scala/Unit#
         |_empty_/Q. => final object Q extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/Q.m(). => method m: Int
         |  Int => scala/Int#
         |_empty_/Q.x$4. => synthetic val method x$4: Unit
         |  Unit => scala/Unit#
         |local0 => synthetic val local x$2: Unit
         |  Unit => scala/Unit#
         |local1 => synthetic val local x$5: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  occurrences(
    parenTypedValues,
    """|[0:7..0:8): O <= _empty_/O.
       |[1:12..1:15): Int => scala/Int#
       |[2:6..2:7): m <= _empty_/O.m().
       |[3:14..3:17): Int => scala/Int#
       |[7:7..7:8): P <= _empty_/P.
       |[8:12..8:15): Int => scala/Int#
       |[10:7..10:8): Q <= _empty_/Q.
       |[11:18..11:21): Int => scala/Int#
       |[12:6..12:7): m <= _empty_/Q.m().
       |[13:20..13:23): Int => scala/Int#
       |""".stripMargin,
  )

  // type arguments of empty collections
  private val emptyCollections =
    """|object O {
       |  val a = List[Int]()
       |  val b = List[List[_]](Nil)
       |  val c = Seq[List[_]]()
       |  val d = Vector[List[_]]()
       |  val e = Array[List[_]]()
       |}
       |""".stripMargin

  symbols(
    emptyCollections,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.a. => val method a: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/O.b. => val method b: List[List[_] forSome { type _ }]
         |  List => scala/collection/immutable/List#
         |  _ => local0
         |_empty_/O.c. => val method c: Seq[List[_] forSome { type _ }]
         |  Seq => scala/collection/Seq#
         |  List => scala/collection/immutable/List#
         |  _ => local1
         |_empty_/O.d. => val method d: Vector[List[_] forSome { type _ }]
         |  Vector => scala/collection/immutable/Vector#
         |  List => scala/collection/immutable/List#
         |  _ => local2
         |_empty_/O.e. => val method e: Array[List[_] forSome { type _ }]
         |  Array => scala/Array#
         |  List => scala/collection/immutable/List#
         |  _ => local3
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.a. => val method a: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/O.b. => val method b: List[List[_] forSome { type _ }]
         |  List => scala/collection/immutable/List#
         |  _ => local0
         |_empty_/O.c. => val method c: Seq[List[_] forSome { type _ }]
         |  Seq => scala/collection/immutable/Seq#
         |  List => scala/collection/immutable/List#
         |  _ => local1
         |_empty_/O.d. => val method d: Vector[List[_] forSome { type _ }]
         |  Vector => scala/collection/immutable/Vector#
         |  List => scala/collection/immutable/List#
         |  _ => local2
         |_empty_/O.e. => val method e: Array[List[_] forSome { type _ }]
         |  Array => scala/Array#
         |  List => scala/collection/immutable/List#
         |  _ => local3
         |""".stripMargin,
  )

  occurrences(
    emptyCollections,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): a <= _empty_/O.a.
         |[1:10..1:14): List => scala/collection/immutable/Nil.
         |[2:6..2:7): b <= _empty_/O.b.
         |[2:10..2:14): List => scala/collection/immutable/List.
         |[2:15..2:19): List => scala/package.List#
         |[2:24..2:27): Nil => scala/collection/immutable/Nil.
         |[3:6..3:7): c <= _empty_/O.c.
         |[3:10..3:13): Seq => scala/collection/Seq.
         |[3:14..3:18): List => scala/package.List#
         |[4:6..4:7): d <= _empty_/O.d.
         |[4:10..4:16): Vector => scala/package.Vector.
         |[4:17..4:21): List => scala/package.List#
         |[5:6..5:7): e <= _empty_/O.e.
         |[5:10..5:15): Array => scala/Array.
         |[5:16..5:20): List => scala/package.List#
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): a <= _empty_/O.a.
         |[1:10..1:14): List => scala/collection/immutable/Nil.
         |[2:6..2:7): b <= _empty_/O.b.
         |[2:10..2:14): List => scala/package.List.
         |[2:15..2:19): List => scala/package.List#
         |[2:24..2:27): Nil => scala/package.Nil.
         |[3:6..3:7): c <= _empty_/O.c.
         |[3:10..3:13): Seq => scala/package.Seq.
         |[3:14..3:18): List => scala/package.List#
         |[4:6..4:7): d <= _empty_/O.d.
         |[4:10..4:16): Vector => scala/package.Vector.
         |[4:17..4:21): List => scala/package.List#
         |[5:6..5:7): e <= _empty_/O.e.
         |[5:10..5:15): Array => scala/Array.
         |[5:16..5:20): List => scala/package.List#
         |""".stripMargin,
  )

  // underscore type arguments in parameter types
  private val paramTypes =
    """|class C(a: List[_])
       |class D(val a: List[_])
       |object O {
       |  def m(a: List[_]) = a
       |  val f = (x: List[_]) => x
       |  def n: List[_] => Int = _.size
       |}
       |""".stripMargin

  symbols(
    paramTypes,
    """|_empty_/C# => class C extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/C#`<init>`(). => primary ctor <init>(a: List[_] forSome { type _ })
       |  a => _empty_/C#`<init>`().(a)
       |  List => scala/package.List#
       |  _ => local0
       |_empty_/C#`<init>`().(a) => param a: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local0
       |_empty_/C#a. => private[this] val method a: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local0
       |_empty_/D# => class D extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/D#`<init>`(). => primary ctor <init>(val a: List[_] forSome { type _ })
       |  a => _empty_/D#`<init>`().(a)
       |  List => scala/package.List#
       |  _ => local1
       |_empty_/D#`<init>`().(a) => val param a: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local1
       |_empty_/D#a. => val method a: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local1
       |_empty_/O. => final object O extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.f. => val method f: Function1[List[_] forSome { type _ }, List[Any]]
       |  Function1 => scala/Function1#
       |  List => scala/collection/immutable/List#
       |  _ => local4
       |  Any => scala/Any#
       |_empty_/O.m(). => method m(a: List[_] forSome { type _ }): List[Any]
       |  a => _empty_/O.m().(a)
       |  List => scala/package.List#
       |  _ => local2
       |  List => scala/collection/immutable/List#
       |  Any => scala/Any#
       |_empty_/O.m().(a) => param a: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local2
       |_empty_/O.n(). => method n: Function1[List[_] forSome { type _ }, Int]
       |  Function1 => scala/Function1#
       |  List => scala/package.List#
       |  _ => local5
       |  Int => scala/Int#
       |local3 => param x: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local4
       |""".stripMargin,
  )

  occurrences(
    paramTypes,
    if (ScalaVersion.is212)
      """|[0:6..0:7): C <= _empty_/C#
         |[0:7..0:7):  <= _empty_/C#`<init>`().
         |[0:8..0:9): a <= _empty_/C#`<init>`().(a)
         |[0:8..0:9): a <= _empty_/C#a.
         |[0:11..0:15): List => scala/package.List#
         |[1:6..1:7): D <= _empty_/D#
         |[1:7..1:7):  <= _empty_/D#`<init>`().
         |[1:12..1:13): a <= _empty_/D#`<init>`().(a)
         |[1:12..1:13): a <= _empty_/D#a.
         |[1:15..1:19): List => scala/package.List#
         |[2:7..2:8): O <= _empty_/O.
         |[3:6..3:7): m <= _empty_/O.m().
         |[3:8..3:9): a <= _empty_/O.m().(a)
         |[3:11..3:15): List => scala/package.List#
         |[3:22..3:23): a => _empty_/O.m().(a)
         |[4:6..4:7): f <= _empty_/O.f.
         |[4:11..4:12): x <= local3
         |[4:14..4:18): List => scala/package.List#
         |[4:26..4:27): x => local3
         |[5:6..5:7): n <= _empty_/O.n().
         |[5:9..5:13): List => scala/package.List#
         |[5:20..5:23): Int => scala/Int#
         |[5:28..5:32): size => scala/collection/SeqLike#size().
         |""".stripMargin
    else
      """|[0:6..0:7): C <= _empty_/C#
         |[0:7..0:7):  <= _empty_/C#`<init>`().
         |[0:8..0:9): a <= _empty_/C#`<init>`().(a)
         |[0:8..0:9): a <= _empty_/C#a.
         |[0:11..0:15): List => scala/package.List#
         |[1:6..1:7): D <= _empty_/D#
         |[1:7..1:7):  <= _empty_/D#`<init>`().
         |[1:12..1:13): a <= _empty_/D#`<init>`().(a)
         |[1:12..1:13): a <= _empty_/D#a.
         |[1:15..1:19): List => scala/package.List#
         |[2:7..2:8): O <= _empty_/O.
         |[3:6..3:7): m <= _empty_/O.m().
         |[3:8..3:9): a <= _empty_/O.m().(a)
         |[3:11..3:15): List => scala/package.List#
         |[3:22..3:23): a => _empty_/O.m().(a)
         |[4:6..4:7): f <= _empty_/O.f.
         |[4:11..4:12): x <= local3
         |[4:14..4:18): List => scala/package.List#
         |[4:26..4:27): x => local3
         |[5:6..5:7): n <= _empty_/O.n().
         |[5:9..5:13): List => scala/package.List#
         |[5:20..5:23): Int => scala/Int#
         |[5:28..5:32): size => scala/collection/SeqOps#size().
         |""".stripMargin,
  )

  // underscore type arguments in type aliases and bounds
  private val typeDefs =
    """|trait T {
       |  type A = List[_]
       |  type B <: List[_]
       |  def m[U <: List[_]] = 1
       |}
       |""".stripMargin

  symbols(
    typeDefs,
    """|_empty_/T# => trait T extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#A# => type A = List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local0
       |_empty_/T#B# => abstract type B <: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local1
       |_empty_/T#`<init>`(). => primary ctor <init>()
       |_empty_/T#m(). => method m[U <: List[_] forSome { type _ }]: Int
       |  U => _empty_/T#m().[U]
       |  List => scala/package.List#
       |  _ => local2
       |  Int => scala/Int#
       |_empty_/T#m().[U] => typeparam U <: List[_] forSome { type _ }
       |  List => scala/package.List#
       |  _ => local2
       |""".stripMargin,
  )

  occurrences(
    typeDefs,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:8..0:8):  <= _empty_/T#`<init>`().
       |[1:7..1:8): A <= _empty_/T#A#
       |[1:11..1:15): List => scala/package.List#
       |[2:7..2:8): B <= _empty_/T#B#
       |[2:12..2:16): List => scala/package.List#
       |[3:6..3:7): m <= _empty_/T#m().
       |[3:8..3:9): U <= _empty_/T#m().[U]
       |[3:13..3:17): List => scala/package.List#
       |""".stripMargin,
  )

  // underscore type arguments in terms and parents
  private val termTypeArgs =
    """|class C extends java.util.ArrayList[List[_]]
       |object O {
       |  val a = (Nil: List[_])
       |  val b = Option.empty[List[_]]
       |  val c = new java.util.ArrayList[List[_]]()
       |  def m(x: Any) = x.isInstanceOf[List[_]]
       |  def n(x: Any) = x match { case _: List[_] if true => 1 }
       |}
       |""".stripMargin

  symbols(
    termTypeArgs,
    """|_empty_/C# => class C extends ArrayList[List[_] forSome { type _ }] { +1 decls }
       |  ArrayList => java/util/ArrayList#
       |  List => scala/package.List#
       |  _ => local0
       |_empty_/C#`<init>`(). => primary ctor <init>()
       |_empty_/O. => final object O extends AnyRef { +5 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.a. => val method a: List[Any]
       |  List => scala/collection/immutable/List#
       |  Any => scala/Any#
       |_empty_/O.b. => val method b: Option[List[_] forSome { type _ }]
       |  Option => scala/Option#
       |  List => scala/collection/immutable/List#
       |  _ => local1
       |_empty_/O.c. => val method c: ArrayList[List[_] forSome { type _ }]
       |  ArrayList => java/util/ArrayList#
       |  List => scala/collection/immutable/List#
       |  _ => local2
       |_empty_/O.m(). => method m(x: Any): Boolean
       |  x => _empty_/O.m().(x)
       |  Any => scala/Any#
       |  Boolean => scala/Boolean#
       |_empty_/O.m().(x) => param x: Any
       |  Any => scala/Any#
       |_empty_/O.n(). => method n(x: Any): Int
       |  x => _empty_/O.n().(x)
       |  Any => scala/Any#
       |  Int => scala/Int#
       |_empty_/O.n().(x) => param x: Any
       |  Any => scala/Any#
       |local3 => abstract type _
       |""".stripMargin,
  )

  occurrences(
    termTypeArgs,
    if (ScalaVersion.is212)
      """|[0:6..0:7): C <= _empty_/C#
         |[0:8..0:8):  <= _empty_/C#`<init>`().
         |[0:16..0:20): java => java/
         |[0:21..0:25): util => java/util/
         |[0:26..0:35): ArrayList => java/util/ArrayList#
         |[0:36..0:40): List => scala/package.List#
         |[0:44..0:44):  => java/util/ArrayList#`<init>`(+1).
         |[1:7..1:8): O <= _empty_/O.
         |[2:6..2:7): a <= _empty_/O.a.
         |[2:11..2:14): Nil => scala/collection/immutable/Nil.
         |[2:16..2:20): List => scala/package.List#
         |[3:6..3:7): b <= _empty_/O.b.
         |[3:10..3:16): Option => scala/Option.
         |[3:17..3:22): empty => scala/Option.empty().
         |[3:23..3:27): List => scala/package.List#
         |[4:6..4:7): c <= _empty_/O.c.
         |[4:14..4:18): java => java/
         |[4:19..4:23): util => java/util/
         |[4:24..4:33): ArrayList => java/util/ArrayList#
         |[4:34..4:38): List => scala/package.List#
         |[4:42..4:42):  => java/util/ArrayList#`<init>`(+1).
         |[5:6..5:7): m <= _empty_/O.m().
         |[5:8..5:9): x <= _empty_/O.m().(x)
         |[5:11..5:14): Any => scala/Any#
         |[5:18..5:19): x => _empty_/O.m().(x)
         |[5:20..5:32): isInstanceOf => scala/Any#isInstanceOf().
         |[5:33..5:37): List => scala/package.List#
         |[6:6..6:7): n <= _empty_/O.n().
         |[6:8..6:9): x <= _empty_/O.n().(x)
         |[6:11..6:14): Any => scala/Any#
         |[6:18..6:19): x => _empty_/O.n().(x)
         |[6:36..6:40): List => scala/package.List#
         |""".stripMargin
    else
      """|[0:6..0:7): C <= _empty_/C#
         |[0:8..0:8):  <= _empty_/C#`<init>`().
         |[0:16..0:20): java => java/
         |[0:21..0:25): util => java/util/
         |[0:26..0:35): ArrayList => java/util/ArrayList#
         |[0:36..0:40): List => scala/package.List#
         |[0:44..0:44):  => java/util/ArrayList#`<init>`(+1).
         |[1:7..1:8): O <= _empty_/O.
         |[2:6..2:7): a <= _empty_/O.a.
         |[2:11..2:14): Nil => scala/package.Nil.
         |[2:16..2:20): List => scala/package.List#
         |[3:6..3:7): b <= _empty_/O.b.
         |[3:10..3:16): Option => scala/Option.
         |[3:17..3:22): empty => scala/Option.empty().
         |[3:23..3:27): List => scala/package.List#
         |[4:6..4:7): c <= _empty_/O.c.
         |[4:14..4:18): java => java/
         |[4:19..4:23): util => java/util/
         |[4:24..4:33): ArrayList => java/util/ArrayList#
         |[4:34..4:38): List => scala/package.List#
         |[4:42..4:42):  => java/util/ArrayList#`<init>`(+1).
         |[5:6..5:7): m <= _empty_/O.m().
         |[5:8..5:9): x <= _empty_/O.m().(x)
         |[5:11..5:14): Any => scala/Any#
         |[5:18..5:19): x => _empty_/O.m().(x)
         |[5:20..5:32): isInstanceOf => scala/Any#isInstanceOf().
         |[5:33..5:37): List => scala/package.List#
         |[6:6..6:7): n <= _empty_/O.n().
         |[6:8..6:9): x <= _empty_/O.n().(x)
         |[6:11..6:14): Any => scala/Any#
         |[6:18..6:19): x => _empty_/O.n().(x)
         |[6:36..6:40): List => scala/package.List#
         |""".stripMargin,
  )

  // underscore placeholders in operators, selections, calls and tuples
  private val placeholderForms =
    """|object O {
       |  List(1).foldLeft(0)(_ + _)
       |  List(1).map(_.toString.length)
       |  def f(a: Int) = a
       |  List(1).map(f(_))
       |  List(1).map((_, 1))
       |  List(1).map { _ + 1 }
       |  def g(a: Int, h: Int => Int) = 1
       |  g(1, _ + 1)
       |}
       |""".stripMargin

  symbols(
    placeholderForms,
    """|_empty_/O. => final object O extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/O.f(). => method f(a: Int): Int
       |  a => _empty_/O.f().(a)
       |  Int => scala/Int#
       |_empty_/O.f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/O.g(). => method g(a: Int, h: Function1[Int, Int]): Int
       |  a => _empty_/O.g().(a)
       |  Int => scala/Int#
       |  h => _empty_/O.g().(h)
       |  Function1 => scala/Function1#
       |_empty_/O.g().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/O.g().(h) => param h: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    placeholderForms,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:2..1:6): List => scala/collection/immutable/List.
         |[1:10..1:18): foldLeft => scala/collection/LinearSeqOptimized#foldLeft().
         |[1:24..1:25): + => scala/Int#`+`(+4).
         |[2:2..2:6): List => scala/collection/immutable/List.
         |[2:10..2:13): map => scala/collection/immutable/List#map().
         |[2:16..2:24): toString => scala/Any#toString().
         |[2:25..2:31): length => java/lang/String#length().
         |[3:6..3:7): f <= _empty_/O.f().
         |[3:8..3:9): a <= _empty_/O.f().(a)
         |[3:11..3:14): Int => scala/Int#
         |[3:18..3:19): a => _empty_/O.f().(a)
         |[4:2..4:6): List => scala/collection/immutable/List.
         |[4:10..4:13): map => scala/collection/immutable/List#map().
         |[4:14..4:15): f => _empty_/O.f().
         |[5:2..5:6): List => scala/collection/immutable/List.
         |[5:10..5:13): map => scala/collection/immutable/List#map().
         |[6:2..6:6): List => scala/collection/immutable/List.
         |[6:10..6:13): map => scala/collection/immutable/List#map().
         |[6:18..6:19): + => scala/Int#`+`(+4).
         |[7:6..7:7): g <= _empty_/O.g().
         |[7:8..7:9): a <= _empty_/O.g().(a)
         |[7:11..7:14): Int => scala/Int#
         |[7:16..7:17): h <= _empty_/O.g().(h)
         |[7:19..7:22): Int => scala/Int#
         |[7:26..7:29): Int => scala/Int#
         |[8:2..8:3): g => _empty_/O.g().
         |[8:9..8:10): + => scala/Int#`+`(+4).
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:2..1:6): List => scala/package.List.
         |[1:10..1:18): foldLeft => scala/collection/LinearSeqOps#foldLeft().
         |[1:24..1:25): + => scala/Int#`+`(+4).
         |[2:2..2:6): List => scala/package.List.
         |[2:10..2:13): map => scala/collection/immutable/List#map().
         |[2:16..2:24): toString => scala/Any#toString().
         |[2:25..2:31): length => java/lang/String#length().
         |[3:6..3:7): f <= _empty_/O.f().
         |[3:8..3:9): a <= _empty_/O.f().(a)
         |[3:11..3:14): Int => scala/Int#
         |[3:18..3:19): a => _empty_/O.f().(a)
         |[4:2..4:6): List => scala/package.List.
         |[4:10..4:13): map => scala/collection/immutable/List#map().
         |[4:14..4:15): f => _empty_/O.f().
         |[5:2..5:6): List => scala/package.List.
         |[5:10..5:13): map => scala/collection/immutable/List#map().
         |[6:2..6:6): List => scala/package.List.
         |[6:10..6:13): map => scala/collection/immutable/List#map().
         |[6:18..6:19): + => scala/Int#`+`(+4).
         |[7:6..7:7): g <= _empty_/O.g().
         |[7:8..7:9): a <= _empty_/O.g().(a)
         |[7:11..7:14): Int => scala/Int#
         |[7:16..7:17): h <= _empty_/O.g().(h)
         |[7:19..7:22): Int => scala/Int#
         |[7:26..7:29): Int => scala/Int#
         |[8:2..8:3): g => _empty_/O.g().
         |[8:9..8:10): + => scala/Int#`+`(+4).
         |""".stripMargin,
  )

  // underscore placeholders in super calls of classes with parameters
  private val paramSuperCalls =
    """|class A(f: Int => Int)
       |class B(f: Int => Int) extends A(_ + 1)
       |class C extends A({ _ + 1 })
       |""".stripMargin

  symbols(
    paramSuperCalls,
    """|_empty_/A# => class A extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/A#`<init>`(). => primary ctor <init>(f: Function1[Int, Int])
       |  f => _empty_/A#`<init>`().(f)
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/A#`<init>`().(f) => param f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/A#f. => private[this] val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/B# => class B extends A { +2 decls }
       |  A => _empty_/A#
       |_empty_/B#`<init>`(). => primary ctor <init>(f: Function1[Int, Int])
       |  f => _empty_/B#`<init>`().(f)
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/B#`<init>`().(f) => param f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/B#f. => private[this] val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/C# => class C extends A { +1 decls }
       |  A => _empty_/A#
       |_empty_/C#`<init>`(). => primary ctor <init>()
       |""".stripMargin,
  )

  occurrences(
    paramSuperCalls,
    """|[0:6..0:7): A <= _empty_/A#
       |[0:7..0:7):  <= _empty_/A#`<init>`().
       |[0:8..0:9): f <= _empty_/A#`<init>`().(f)
       |[0:8..0:9): f <= _empty_/A#f.
       |[0:11..0:14): Int => scala/Int#
       |[0:18..0:21): Int => scala/Int#
       |[1:6..1:7): B <= _empty_/B#
       |[1:7..1:7):  <= _empty_/B#`<init>`().
       |[1:8..1:9): f <= _empty_/B#`<init>`().(f)
       |[1:8..1:9): f <= _empty_/B#f.
       |[1:11..1:14): Int => scala/Int#
       |[1:18..1:21): Int => scala/Int#
       |[1:31..1:32): A => _empty_/A#
       |[1:32..1:32):  => _empty_/A#`<init>`().
       |[1:35..1:36): + => scala/Int#`+`(+4).
       |[2:6..2:7): C <= _empty_/C#
       |[2:8..2:8):  <= _empty_/C#`<init>`().
       |[2:16..2:17): A => _empty_/A#
       |[2:17..2:17):  => _empty_/A#`<init>`().
       |[2:22..2:23): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  // underscores in multiple, infix, bound and typed patterns
  private val innerPatterns =
    """|object O {
       |  val a, _ = 1
       |  val (_) :: Nil = List(1)
       |  val (b: List[_], c) = (Nil, 1)
       |  def m = {
       |    var _ = 1
       |    2
       |  }
       |  def n(x: Any) = x match { case y @ (_: List[_]) => y }
       |  for ((_, d) <- List((1, 2))) yield d
       |  for (_ @ _ <- List(1)) yield 1
       |  for (e @ _ <- List(1)) yield e
       |  for (f: List[_] <- List(Nil)) yield f
       |}
       |""".stripMargin

  symbols(
    innerPatterns,
    if (ScalaVersion.is212)
      """|_empty_/O. => final object O extends AnyRef { +8 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O._. => val method _: Int
         |  Int => scala/Int#
         |_empty_/O.a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/O.b. => val method b: Nil.type
         |  Nil => scala/collection/immutable/Nil.
         |_empty_/O.c. => val method c: Int
         |  Int => scala/Int#
         |_empty_/O.m(). => method m: Int
         |  Int => scala/Int#
         |_empty_/O.n(). => method n(x: Any): List[Any]
         |  x => _empty_/O.n().(x)
         |  Any => scala/Any#
         |  List => scala/collection/immutable/List#
         |_empty_/O.n().(x) => param x: Any
         |  Any => scala/Any#
         |_empty_/O.x$1. => synthetic val method x$1: Unit
         |  Unit => scala/Unit#
         |_empty_/O.x$2. => synthetic val method x$2: Tuple2[Nil.type, Int]
         |  Tuple2 => scala/Tuple2#
         |  Nil => scala/collection/immutable/Nil.
         |  Int => scala/Int#
         |local0 => val local b: Nil.type
         |  Nil => scala/collection/immutable/Nil.
         |local1 => abstract type _
         |local2 => val local c: Int
         |  Int => scala/Int#
         |local3 => var local _: Int
         |  Int => scala/Int#
         |local4 => val local y: List[_]
         |  List => scala/package.List#
         |  _ => local5
         |local5 => abstract type _
         |local8 => val local d: Int
         |  Int => scala/Int#
         |local9 => param _: Int
         |  Int => scala/Int#
         |local10 => param e: Int
         |  Int => scala/Int#
         |local12 => param f: List[_]
         |  List => scala/package.List#
         |  _ => local13
         |local13 => abstract type _
         |""".stripMargin
    else
      """|_empty_/O. => final object O extends AnyRef { +8 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/O.a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/O.b. => val method b: Nil.type
         |  Nil => scala/collection/immutable/Nil.
         |_empty_/O.c. => val method c: Int
         |  Int => scala/Int#
         |_empty_/O.m(). => method m: Int
         |  Int => scala/Int#
         |_empty_/O.n(). => method n(x: Any): List[Any]
         |  x => _empty_/O.n().(x)
         |  Any => scala/Any#
         |  List => scala/collection/immutable/List#
         |_empty_/O.n().(x) => param x: Any
         |  Any => scala/Any#
         |_empty_/O.x$1. => synthetic val method x$1: Unit
         |  Unit => scala/Unit#
         |_empty_/O.x$2. => synthetic val method x$2: Unit
         |  Unit => scala/Unit#
         |_empty_/O.x$3. => synthetic val method x$3: Tuple2[Nil.type, Int]
         |  Tuple2 => scala/Tuple2#
         |  Nil => scala/collection/immutable/Nil.
         |  Int => scala/Int#
         |local0 => val local b: Nil.type
         |  Nil => scala/collection/immutable/Nil.
         |local1 => abstract type _
         |local2 => val local c: Int
         |  Int => scala/Int#
         |local3 => synthetic val local x$4: Unit
         |  Unit => scala/Unit#
         |local4 => val local y: List[_]
         |  List => scala/package.List#
         |  _ => local5
         |local5 => abstract type _
         |local8 => val local d: Int
         |  Int => scala/Int#
         |local10 => param e: Int
         |  Int => scala/Int#
         |local12 => param f: List[_]
         |  List => scala/package.List#
         |  _ => local13
         |local13 => abstract type _
         |""".stripMargin,
  )

  occurrences(
    innerPatterns,
    if (ScalaVersion.is212)
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): a <= _empty_/O.a.
         |[2:10..2:12): :: => scala/package.`::`.
         |[2:13..2:16): Nil => scala/collection/immutable/Nil.
         |[2:19..2:23): List => scala/collection/immutable/List.
         |[3:7..3:8): b <= _empty_/O.b.
         |[3:10..3:14): List => scala/package.List#
         |[3:19..3:20): c <= _empty_/O.c.
         |[3:25..3:28): Nil => scala/collection/immutable/Nil.
         |[4:6..4:7): m <= _empty_/O.m().
         |[8:6..8:7): n <= _empty_/O.n().
         |[8:8..8:9): x <= _empty_/O.n().(x)
         |[8:11..8:14): Any => scala/Any#
         |[8:18..8:19): x => _empty_/O.n().(x)
         |[8:33..8:34): y <= local4
         |[8:41..8:45): List => scala/package.List#
         |[8:53..8:54): y => local4
         |[9:11..9:12): d <= local8
         |[9:17..9:21): List => scala/collection/immutable/List.
         |[9:37..9:38): d => local8
         |[10:16..10:20): List => scala/collection/immutable/List.
         |[11:7..11:8): e <= local10
         |[11:16..11:20): List => scala/collection/immutable/List.
         |[11:31..11:32): e => local10
         |[12:7..12:8): f <= local12
         |[12:10..12:14): List => scala/package.List#
         |[12:21..12:25): List => scala/collection/immutable/List.
         |[12:26..12:29): Nil => scala/collection/immutable/Nil.
         |[12:38..12:39): f => local12
         |""".stripMargin
    else
      """|[0:7..0:8): O <= _empty_/O.
         |[1:6..1:7): a <= _empty_/O.a.
         |[2:10..2:12): :: => scala/package.`::`.
         |[2:13..2:16): Nil => scala/package.Nil.
         |[2:19..2:23): List => scala/package.List.
         |[3:7..3:8): b <= _empty_/O.b.
         |[3:10..3:14): List => scala/package.List#
         |[3:19..3:20): c <= _empty_/O.c.
         |[3:25..3:28): Nil => scala/package.Nil.
         |[4:6..4:7): m <= _empty_/O.m().
         |[8:6..8:7): n <= _empty_/O.n().
         |[8:8..8:9): x <= _empty_/O.n().(x)
         |[8:11..8:14): Any => scala/Any#
         |[8:18..8:19): x => _empty_/O.n().(x)
         |[8:33..8:34): y <= local4
         |[8:41..8:45): List => scala/package.List#
         |[8:53..8:54): y => local4
         |[9:11..9:12): d <= local8
         |[9:17..9:21): List => scala/package.List.
         |[9:37..9:38): d => local8
         |[10:16..10:20): List => scala/package.List.
         |[11:7..11:8): e <= local10
         |[11:16..11:20): List => scala/package.List.
         |[11:31..11:32): e => local10
         |[12:7..12:8): f <= local12
         |[12:10..12:14): List => scala/package.List#
         |[12:21..12:25): List => scala/package.List.
         |[12:26..12:29): Nil => scala/package.Nil.
         |[12:38..12:39): f => local12
         |""".stripMargin,
  )

}

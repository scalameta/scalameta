package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}

import scala.collection.mutable

class PartialFunctionSuite extends SemanticdbSuite {

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  // for each tree, the first occurrence at its exact range, which scalafix reads as `tree.symbol`
  private def treeSymbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = test(s"$name: tree symbols") {
    import scala.meta._
    val occurrences = computeDatabaseFromSnippet(code).occurrences
    val obtained = mutable.LinkedHashSet.empty[String]
    dialects.Scala213(code).parse[Source].get.traverse { case t =>
      val p = t.pos
      val (bl, bc, el, ec) = (p.startLine, p.startColumn, p.endLine, p.endColumn)
      occurrences.find(_.range.contains(s.Range(bl, bc, el, ec))).foreach { o =>
        val role = if (o.role.isDefinition) "<=" else "=>"
        obtained += s"[$bl:$bc..$el:$ec): ${p.text} $role ${o.symbol}"
      }
    }
    assertNoDiff(obtained.mkString("\n"), expected)
  }

  symbols(
    "binders named defaultCase$",
    """|class t14 {
       |  def pf: PartialFunction[Int, Int] = { case 1 => 1; case defaultCase$ => defaultCase$ }
       |  def c = List(1).collect { case defaultCase$ if defaultCase$ > 0 => defaultCase$ }
       |  def m(x: Int) = x match { case defaultCase$ => defaultCase$ }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t14# => class t14 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t14#`<init>`(). => primary ctor <init>()
         |_empty_/t14#c(). => method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t14#m(). => method m(x: Int): Int
         |  x => _empty_/t14#m().(x)
         |  Int => scala/Int#
         |_empty_/t14#m().(x) => param x: Int
         |  Int => scala/Int#
         |_empty_/t14#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local8 => synthetic param x1: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x1 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x1: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local17 => synthetic param x1: Int
         |  Int => scala/Int#
         |local18 => val local defaultCase$: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t14# => class t14 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t14#`<init>`(). => primary ctor <init>()
         |_empty_/t14#c(). => method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t14#m(). => method m(x: Int): Int
         |  x => _empty_/t14#m().(x)
         |  Int => scala/Int#
         |_empty_/t14#m().(x) => param x: Int
         |  Int => scala/Int#
         |_empty_/t14#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local8 => synthetic param x1: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x1 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x1: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local17 => synthetic param x1: Int
         |  Int => scala/Int#
         |local18 => val local defaultCase$: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "binders named defaultCase$",
    """|class t14 {
       |  def pf: PartialFunction[Int, Int] = { case 1 => 1; case defaultCase$ => defaultCase$ }
       |  def c = List(1).collect { case defaultCase$ if defaultCase$ > 0 => defaultCase$ }
       |  def m(x: Int) = x match { case defaultCase$ => defaultCase$ }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:6..0:9): t14 <= _empty_/t14#
         |[0:10..0:10):  <= _empty_/t14#`<init>`().
         |[1:6..1:8): pf <= _empty_/t14#pf().
         |[1:10..1:25): PartialFunction => scala/PartialFunction#
         |[1:26..1:29): Int => scala/Int#
         |[1:31..1:34): Int => scala/Int#
         |[1:38..1:38):  <= local0
         |[1:58..1:70): defaultCase$ <= local7
         |[1:74..1:86): defaultCase$ => local7
         |[2:6..2:7): c <= _empty_/t14#c().
         |[2:10..2:14): List => scala/collection/immutable/List.
         |[2:18..2:25): collect => scala/collection/immutable/List#collect().
         |[2:26..2:26):  <= local9
         |[2:33..2:45): defaultCase$ <= local16
         |[2:49..2:61): defaultCase$ => local16
         |[2:62..2:63): > => scala/Int#`>`(+3).
         |[2:69..2:81): defaultCase$ => local16
         |[3:6..3:7): m <= _empty_/t14#m().
         |[3:8..3:9): x <= _empty_/t14#m().(x)
         |[3:11..3:14): Int => scala/Int#
         |[3:18..3:19): x => _empty_/t14#m().(x)
         |[3:33..3:45): defaultCase$ <= local18
         |[3:49..3:61): defaultCase$ => local18
         |""".stripMargin
    else
      """|[0:6..0:9): t14 <= _empty_/t14#
         |[0:10..0:10):  <= _empty_/t14#`<init>`().
         |[1:6..1:8): pf <= _empty_/t14#pf().
         |[1:10..1:25): PartialFunction => scala/PartialFunction#
         |[1:26..1:29): Int => scala/Int#
         |[1:31..1:34): Int => scala/Int#
         |[1:38..1:38):  <= local0
         |[1:58..1:70): defaultCase$ <= local7
         |[1:74..1:86): defaultCase$ => local7
         |[2:6..2:7): c <= _empty_/t14#c().
         |[2:10..2:14): List => scala/package.List.
         |[2:18..2:25): collect => scala/collection/immutable/List#collect().
         |[2:26..2:26):  <= local9
         |[2:33..2:45): defaultCase$ <= local16
         |[2:49..2:61): defaultCase$ => local16
         |[2:62..2:63): > => scala/Int#`>`(+3).
         |[2:69..2:81): defaultCase$ => local16
         |[3:6..3:7): m <= _empty_/t14#m().
         |[3:8..3:9): x <= _empty_/t14#m().(x)
         |[3:11..3:14): Int => scala/Int#
         |[3:18..3:19): x => _empty_/t14#m().(x)
         |[3:33..3:45): defaultCase$ <= local18
         |[3:49..3:61): defaultCase$ => local18
         |""".stripMargin,
  )

  symbols(
    "partial functions with binders",
    """|class t11 {
       |  def pf: PartialFunction[Int, Int] = { case i if i > 0 => i }
       |  def c = List(1).collect { case i: Int => i }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t11# => class t11 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t11#`<init>`(). => primary ctor <init>()
         |_empty_/t11#c(). => method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t11#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => val local i: Int
         |  Int => scala/Int#
         |local8 => synthetic param x1: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x1 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x1: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => val local i: Int
         |  Int => scala/Int#
         |local17 => synthetic param x1: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t11# => class t11 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t11#`<init>`(). => primary ctor <init>()
         |_empty_/t11#c(). => method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t11#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => val local i: Int
         |  Int => scala/Int#
         |local8 => synthetic param x1: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x1 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x1: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => val local i: Int
         |  Int => scala/Int#
         |local17 => synthetic param x1: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "function literal as a partial function",
    """|class t12 {
       |  def pf: PartialFunction[Int, Int] = x => x + 1
       |}
       |""".stripMargin,
    """|_empty_/t12# => class t12 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t12#`<init>`(). => primary ctor <init>()
       |_empty_/t12#pf(). => method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local6
       |  Int => scala/Int#
       |  B1 => local5
       |  x => local3
       |  default => local4
       |  Function1 => scala/Function1#
       |local2 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local7
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic param x: A1
       |  A1 => local6
       |local4 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local6
       |  B1 => local5
       |local5 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local6 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local7 => synthetic param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "function literal as a partial function",
    """|class t12 {
       |  def pf: PartialFunction[Int, Int] = x => x + 1
       |}
       |""".stripMargin,
    """|[0:6..0:9): t12 <= _empty_/t12#
       |[0:10..0:10):  <= _empty_/t12#`<init>`().
       |[1:6..1:8): pf <= _empty_/t12#pf().
       |[1:10..1:25): PartialFunction => scala/PartialFunction#
       |[1:26..1:29): Int => scala/Int#
       |[1:31..1:34): Int => scala/Int#
       |[1:43..1:44): x => local3
       |[1:45..1:45):  <= local0
       |[1:45..1:46): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "function literal as a partial function in a block",
    """|object t38 {
       |  def m = { val f: PartialFunction[Int, Int] = x => x; f }
       |}
       |""".stripMargin,
    """|[0:7..0:10): t38 <= _empty_/t38.
       |[1:6..1:7): m <= _empty_/t38.m().
       |[1:16..1:17): f <= local8
       |[1:19..1:34): PartialFunction => scala/PartialFunction#
       |[1:35..1:38): Int => scala/Int#
       |[1:40..1:43): Int => scala/Int#
       |[1:52..1:52):  <= local0
       |[1:52..1:53): x => local3
       |[1:55..1:56): f => local8
       |""".stripMargin,
  )

  symbols(
    "matches and partial functions",
    """|class t10 {
       |  def m(x: Any) = x match { case i: Int => i; case _ => 0 }
       |  def t = try 1 catch { case e: Exception => 2 } finally ()
       |  def pf: PartialFunction[Int, Int] = { case 1 => 1 }
       |  def eta = List(1).map(m)
       |  def bn(x: => Int) = x
       |  def u = { var v = 1; v += 1; bn(v) }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t10# => class t10 extends AnyRef { +7 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t10#`<init>`(). => primary ctor <init>()
         |_empty_/t10#bn(). => method bn(x: => Int): Int
         |  x => _empty_/t10#bn().(x)
         |  Int => scala/Int#
         |_empty_/t10#bn().(x) => param x: => Int
         |  Int => scala/Int#
         |_empty_/t10#eta(). => method eta: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t10#m(). => method m(x: Any): Int
         |  x => _empty_/t10#m().(x)
         |  Any => scala/Any#
         |  Int => scala/Int#
         |_empty_/t10#m().(x) => param x: Any
         |  Any => scala/Any#
         |_empty_/t10#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t10#t(). => method t: Int
         |  Int => scala/Int#
         |_empty_/t10#u(). => method u: Int
         |  Int => scala/Int#
         |local0 => val local i: Int
         |  Int => scala/Int#
         |local1 => val local e: Exception
         |  Exception => scala/package.Exception#
         |local2 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local3 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local8
         |  Int => scala/Int#
         |  B1 => local7
         |  x1 => local5
         |  default => local6
         |  Function1 => scala/Function1#
         |local4 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local9
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local5 => synthetic param x1: A1
         |  A1 => local8
         |local6 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local8
         |  B1 => local7
         |local7 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local8 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local9 => synthetic param x1: Int
         |  Int => scala/Int#
         |local10 => var local v: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t10# => class t10 extends AnyRef { +7 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t10#`<init>`(). => primary ctor <init>()
         |_empty_/t10#bn(). => method bn(x: => Int): Int
         |  x => _empty_/t10#bn().(x)
         |  Int => scala/Int#
         |_empty_/t10#bn().(x) => param x: => Int
         |  Int => scala/Int#
         |_empty_/t10#eta(). => method eta: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t10#m(). => method m(x: Any): Int
         |  x => _empty_/t10#m().(x)
         |  Any => scala/Any#
         |  Int => scala/Int#
         |_empty_/t10#m().(x) => param x: Any
         |  Any => scala/Any#
         |_empty_/t10#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t10#t(). => method t: Int
         |  Int => scala/Int#
         |_empty_/t10#u(). => method u: Int
         |  Int => scala/Int#
         |local0 => val local i: Int
         |  Int => scala/Int#
         |local1 => val local e: Exception
         |  Exception => scala/package.Exception#
         |local2 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local3 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local8
         |  Int => scala/Int#
         |  B1 => local7
         |  x1 => local5
         |  default => local6
         |  Function1 => scala/Function1#
         |local4 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local9
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local5 => synthetic param x1: A1
         |  A1 => local8
         |local6 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local8
         |  B1 => local7
         |local7 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local8 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local9 => synthetic param x1: Int
         |  Int => scala/Int#
         |local10 => var local v: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "function literal and partial function literal",
    """|object t178 {
       |  def f: Int => Int = x => x + 1
       |  def pf: PartialFunction[Int, Int] = x => x + 1
       |  def cpf: PartialFunction[Int, Int] = { case x => x + 1 }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t178 <= _empty_/t178.
       |[1:6..1:7): f <= _empty_/t178.f().
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:22..1:23): x <= local0
       |[1:27..1:28): x => local0
       |[1:29..1:30): + => scala/Int#`+`(+4).
       |[2:6..2:8): pf <= _empty_/t178.pf().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:43..2:44): x => local4
       |[2:45..2:45):  <= local1
       |[2:45..2:46): + => scala/Int#`+`(+4).
       |[3:6..3:9): cpf <= _empty_/t178.cpf().
       |[3:11..3:26): PartialFunction => scala/PartialFunction#
       |[3:27..3:30): Int => scala/Int#
       |[3:32..3:35): Int => scala/Int#
       |[3:39..3:39):  <= local9
       |[3:46..3:47): x <= local16
       |[3:51..3:52): x => local16
       |[3:53..3:54): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) treeSymbols(
    "function literal and partial function literal",
    """|object t178 {
       |  def f: Int => Int = x => x + 1
       |  def pf: PartialFunction[Int, Int] = x => x + 1
       |  def cpf: PartialFunction[Int, Int] = { case x => x + 1 }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t178 <= _empty_/t178.
       |[1:6..1:7): f <= _empty_/t178.f().
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:22..1:23): x <= local0
       |[1:27..1:28): x => local0
       |[1:29..1:30): + => scala/Int#`+`(+4).
       |[2:6..2:8): pf <= _empty_/t178.pf().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:43..2:44): x => local4
       |[2:45..2:46): + => scala/Int#`+`(+4).
       |[3:6..3:9): cpf <= _empty_/t178.cpf().
       |[3:11..3:26): PartialFunction => scala/PartialFunction#
       |[3:27..3:30): Int => scala/Int#
       |[3:32..3:35): Int => scala/Int#
       |[3:46..3:47): x <= local16
       |[3:51..3:52): x => local16
       |[3:53..3:54): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function literal with a typed parameter",
    """|object t179 {
       |  def f: Int => Int = (x: Int) => x + 1
       |  def pf: PartialFunction[Int, Int] = (x: Int) => x + 1
       |}
       |""".stripMargin,
    """|[0:7..0:11): t179 <= _empty_/t179.
       |[1:6..1:7): f <= _empty_/t179.f().
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:23..1:24): x <= local0
       |[1:26..1:29): Int => scala/Int#
       |[1:34..1:35): x => local0
       |[1:36..1:37): + => scala/Int#`+`(+4).
       |[2:6..2:8): pf <= _empty_/t179.pf().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:50..2:51): x => local4
       |[2:52..2:52):  <= local1
       |[2:52..2:53): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "placeholder as a partial function",
    """|object t180 {
       |  def f: Int => Int = _ + 1
       |  def pf: PartialFunction[Int, Int] = _ + 1
       |}
       |""".stripMargin,
    """|[0:7..0:11): t180 <= _empty_/t180.
       |[1:6..1:7): f <= _empty_/t180.f().
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:22..1:23): _ <= local0
       |[1:24..1:25): + => scala/Int#`+`(+4).
       |[2:6..2:8): pf <= _empty_/t180.pf().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:38..2:39): _ <= local2
       |[2:40..2:40):  <= local1
       |[2:40..2:41): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function literal in a field",
    """|class t182 {
       |  val f: Int => String = x => x.toString
       |  val pf: PartialFunction[Int, String] = x => x.toString
       |}
       |""".stripMargin,
    """|[0:6..0:10): t182 <= _empty_/t182#
       |[0:11..0:11):  <= _empty_/t182#`<init>`().
       |[1:6..1:7): f <= _empty_/t182#f.
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:22): String => scala/Predef.String#
       |[1:25..1:26): x <= local0
       |[1:30..1:31): x => local0
       |[1:32..1:40): toString => scala/Any#toString().
       |[2:6..2:8): pf <= _empty_/t182#pf.
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:37): String => scala/Predef.String#
       |[2:46..2:47): x => local4
       |[2:48..2:48):  <= local1
       |[2:48..2:56): toString => scala/Any#toString().
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function literal as an argument",
    """|object t183 {
       |  def l = List(1).map(x => x + 1)
       |  def m = List(1).collect(x => x + 1)
       |  def n = List(1).collect { x => x }
       |}
       |""".stripMargin,
    """|_empty_/t183. => final object t183 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t183.l(). => method l: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t183.m(). => method m: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t183.n(). => method n: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local2 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local7
       |  Int => scala/Int#
       |  B1 => local6
       |  x => local4
       |  default => local5
       |  Function1 => scala/Function1#
       |local3 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local8
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic param x: A1
       |  A1 => local7
       |local5 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local7
       |  B1 => local6
       |local6 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local7 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local8 => synthetic param x: Int
       |  Int => scala/Int#
       |local9 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  A1 => local9
       |  Serializable => java/io/Serializable#
       |local11 => final method applyOrElse[A1 <: Int, B1 >: A1](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local9
       |  Int => scala/Int#
       |  B1 => local15
       |  x => local13
       |  default => local14
       |  Function1 => scala/Function1#
       |local12 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local16
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local13 => synthetic param x: A1
       |  A1 => local9
       |local14 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local9
       |  B1 => local15
       |local15 => typeparam B1 >: A1
       |  A1 => local9
       |local16 => synthetic param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function literal as an argument",
    """|object t183 {
       |  def l = List(1).map(x => x + 1)
       |  def m = List(1).collect(x => x + 1)
       |  def n = List(1).collect { x => x }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t183 <= _empty_/t183.
       |[1:6..1:7): l <= _empty_/t183.l().
       |[1:10..1:14): List => scala/package.List.
       |[1:18..1:21): map => scala/collection/immutable/List#map().
       |[1:22..1:23): x <= local0
       |[1:27..1:28): x => local0
       |[1:29..1:30): + => scala/Int#`+`(+4).
       |[2:6..2:7): m <= _empty_/t183.m().
       |[2:10..2:14): List => scala/package.List.
       |[2:18..2:25): collect => scala/collection/immutable/List#collect().
       |[2:31..2:32): x => local4
       |[2:33..2:33):  <= local1
       |[2:33..2:34): + => scala/Int#`+`(+4).
       |[3:6..3:7): n <= _empty_/t183.n().
       |[3:10..3:14): List => scala/package.List.
       |[3:18..3:25): collect => scala/collection/immutable/List#collect().
       |[3:33..3:33):  <= local10
       |[3:33..3:34): x => local13
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function literal in a lambda",
    """|object t184 {
       |  def m = List(1).map(y => List(2).collect(x => x + y))
       |}
       |""".stripMargin,
    """|[0:7..0:11): t184 <= _empty_/t184.
       |[1:6..1:7): m <= _empty_/t184.m().
       |[1:10..1:14): List => scala/package.List.
       |[1:18..1:21): map => scala/collection/immutable/List#map().
       |[1:22..1:23): y <= local0
       |[1:27..1:31): List => scala/package.List.
       |[1:35..1:42): collect => scala/collection/immutable/List#collect().
       |[1:48..1:49): x => local4
       |[1:50..1:50):  <= local1
       |[1:50..1:51): + => scala/Int#`+`(+4).
       |[1:52..1:53): y => local0
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function literal that shadows",
    """|object t185 {
       |  def m(x: String): PartialFunction[Int, Int] = x => List(x).map(x => x + 1).head
       |}
       |""".stripMargin,
    """|[0:7..0:11): t185 <= _empty_/t185.
       |[1:6..1:7): m <= _empty_/t185.m().
       |[1:8..1:9): x <= _empty_/t185.m().(x)
       |[1:11..1:17): String => scala/Predef.String#
       |[1:20..1:35): PartialFunction => scala/PartialFunction#
       |[1:36..1:39): Int => scala/Int#
       |[1:41..1:44): Int => scala/Int#
       |[1:53..1:57): List => scala/package.List.
       |[1:58..1:59): x => local3
       |[1:61..1:64): map => scala/collection/immutable/List#map().
       |[1:65..1:66): x <= local7
       |[1:70..1:71): x => local7
       |[1:72..1:73): + => scala/Int#`+`(+4).
       |[1:77..1:77):  <= local0
       |[1:77..1:81): head => scala/collection/IterableOps#head().
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function literals with a cast and in a chain",
    """|object t187 {
       |  def pf: PartialFunction[Int, Int] = x => x.asInstanceOf[Int] match { case 1 => x }
       |  def m = List(1).collect(x => x).collect(x => x + 1)
       |}
       |""".stripMargin,
    """|_empty_/t187. => final object t187 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t187.m(). => method m: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t187.pf(). => method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local6
       |  Int => scala/Int#
       |  B1 => local5
       |  x => local3
       |  default => local4
       |  Function1 => scala/Function1#
       |local2 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local7
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic param x: A1
       |  A1 => local6
       |local4 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local6
       |  B1 => local5
       |local5 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local6 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local7 => synthetic param x: Int
       |  Int => scala/Int#
       |local8 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  A1 => local8
       |  Serializable => java/io/Serializable#
       |local10 => final method applyOrElse[A1 <: Int, B1 >: A1](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local8
       |  Int => scala/Int#
       |  B1 => local14
       |  x => local12
       |  default => local13
       |  Function1 => scala/Function1#
       |local11 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local15
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local12 => synthetic param x: A1
       |  A1 => local8
       |local13 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local8
       |  B1 => local14
       |local14 => typeparam B1 >: A1
       |  A1 => local8
       |local15 => synthetic param x: Int
       |  Int => scala/Int#
       |local16 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[A1, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  A1 => local8
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local17 => final method applyOrElse[A1 <: A1, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local22
       |  A1 => local8
       |  B1 => local21
       |  Int => scala/Int#
       |  x => local19
       |  default => local20
       |  Function1 => scala/Function1#
       |local18 => final method isDefinedAt(x: A1): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local23
       |  A1 => local8
       |  Boolean => scala/Boolean#
       |local19 => synthetic param x: A1
       |  A1 => local22
       |local20 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local22
       |  B1 => local21
       |local21 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local22 => typeparam A1 <: A1
       |  A1 => local8
       |local23 => synthetic param x: A1
       |  A1 => local8
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function literals with a cast and in a chain",
    """|object t187 {
       |  def pf: PartialFunction[Int, Int] = x => x.asInstanceOf[Int] match { case 1 => x }
       |  def m = List(1).collect(x => x).collect(x => x + 1)
       |}
       |""".stripMargin,
    """|[0:7..0:11): t187 <= _empty_/t187.
       |[1:6..1:8): pf <= _empty_/t187.pf().
       |[1:10..1:25): PartialFunction => scala/PartialFunction#
       |[1:26..1:29): Int => scala/Int#
       |[1:31..1:34): Int => scala/Int#
       |[1:43..1:44): x => local3
       |[1:45..1:57): asInstanceOf => scala/Any#asInstanceOf().
       |[1:58..1:61): Int => scala/Int#
       |[1:63..1:63):  <= local0
       |[1:81..1:82): x => local3
       |[2:6..2:7): m <= _empty_/t187.m().
       |[2:10..2:14): List => scala/package.List.
       |[2:18..2:25): collect => scala/collection/immutable/List#collect().
       |[2:31..2:31):  <= local9
       |[2:31..2:32): x => local12
       |[2:34..2:41): collect => scala/collection/immutable/List#collect().
       |[2:47..2:48): x => local19
       |[2:49..2:49):  <= local16
       |[2:49..2:50): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function literals with typed parameters",
    """|object t188 {
       |  def f: List[Int] => String = (xs: List[Int]) => xs.mkString
       |  def pf: PartialFunction[List[Int], String] = (xs: List[Int]) => xs.mkString
       |  def g: String => Int = (s : String) => s.length
       |  def pg: PartialFunction[String, Int] = (s : String) => s.length
       |}
       |""".stripMargin,
    """|[0:7..0:11): t188 <= _empty_/t188.
       |[1:6..1:7): f <= _empty_/t188.f().
       |[1:9..1:13): List => scala/package.List#
       |[1:14..1:17): Int => scala/Int#
       |[1:22..1:28): String => scala/Predef.String#
       |[1:32..1:34): xs <= local0
       |[1:36..1:40): List => scala/package.List#
       |[1:41..1:44): Int => scala/Int#
       |[1:50..1:52): xs => local0
       |[1:53..1:61): mkString => scala/collection/IterableOnceOps#mkString(+2).
       |[2:6..2:8): pf <= _empty_/t188.pf().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:30): List => scala/package.List#
       |[2:31..2:34): Int => scala/Int#
       |[2:37..2:43): String => scala/Predef.String#
       |[2:66..2:68): xs => local4
       |[2:69..2:69):  <= local1
       |[2:69..2:77): mkString => scala/collection/IterableOnceOps#mkString(+2).
       |[3:6..3:7): g <= _empty_/t188.g().
       |[3:9..3:15): String => scala/Predef.String#
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): s <= local9
       |[3:30..3:36): String => scala/Predef.String#
       |[3:41..3:42): s => local9
       |[3:43..3:49): length => java/lang/String#length().
       |[4:6..4:8): pg <= _empty_/t188.pg().
       |[4:10..4:25): PartialFunction => scala/PartialFunction#
       |[4:26..4:32): String => scala/Predef.String#
       |[4:34..4:37): Int => scala/Int#
       |[4:57..4:58): s => local13
       |[4:59..4:59):  <= local10
       |[4:59..4:65): length => java/lang/String#length().
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function literals with parentheses, a block or a nested lambda",
    """|object t189 {
       |  def pa: PartialFunction[Int, Int] = (x) => x + 1
       |  def pb: PartialFunction[Int, Int] = x => (x + 1)
       |  def pc: PartialFunction[Int, Int] = x => { x + 1 }
       |  def pd: PartialFunction[Int, PartialFunction[Int, Int]] = x => _ + x
       |  def pe: PartialFunction[Int, PartialFunction[Int, Int]] = x => y => x + y
       |}
       |""".stripMargin,
    """|[0:7..0:11): t189 <= _empty_/t189.
       |[1:6..1:8): pa <= _empty_/t189.pa().
       |[1:10..1:25): PartialFunction => scala/PartialFunction#
       |[1:26..1:29): Int => scala/Int#
       |[1:31..1:34): Int => scala/Int#
       |[1:45..1:46): x => local3
       |[1:47..1:47):  <= local0
       |[1:47..1:48): + => scala/Int#`+`(+4).
       |[2:6..2:8): pb <= _empty_/t189.pb().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:44..2:45): x => local11
       |[2:46..2:46):  <= local8
       |[2:46..2:47): + => scala/Int#`+`(+4).
       |[3:6..3:8): pc <= _empty_/t189.pc().
       |[3:10..3:25): PartialFunction => scala/PartialFunction#
       |[3:26..3:29): Int => scala/Int#
       |[3:31..3:34): Int => scala/Int#
       |[3:45..3:46): x => local19
       |[3:47..3:47):  <= local16
       |[3:47..3:48): + => scala/Int#`+`(+4).
       |[4:6..4:8): pd <= _empty_/t189.pd().
       |[4:10..4:25): PartialFunction => scala/PartialFunction#
       |[4:26..4:29): Int => scala/Int#
       |[4:31..4:46): PartialFunction => scala/PartialFunction#
       |[4:47..4:50): Int => scala/Int#
       |[4:52..4:55): Int => scala/Int#
       |[4:65..4:66): _ <= local25
       |[4:67..4:67):  <= local31
       |[4:67..4:68): + => scala/Int#`+`(+4).
       |[4:69..4:70): x => local27
       |[5:6..5:8): pe <= _empty_/t189.pe().
       |[5:10..5:25): PartialFunction => scala/PartialFunction#
       |[5:26..5:29): Int => scala/Int#
       |[5:31..5:46): PartialFunction => scala/PartialFunction#
       |[5:47..5:50): Int => scala/Int#
       |[5:52..5:55): Int => scala/Int#
       |[5:67..5:67):  <= local40
       |[5:70..5:71): x => local43
       |[5:72..5:72):  <= local47
       |[5:72..5:73): + => scala/Int#`+`(+4).
       |[5:74..5:75): y => local50
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "placeholders as partial functions",
    """|object t190 {
       |  def a: Int => String = _.toString
       |  def pa: PartialFunction[Int, String] = _.toString
       |  def b: Int => Int = (_: Int) + 1
       |  def pb: PartialFunction[Int, Int] = (_: Int) + 1
       |  def c = List(1).map(_ + 1)
       |  def pc = List(1).collect(_ + 1)
       |  def d: Int => List[Int] = List(_)
       |  def pd: PartialFunction[Int, List[Int]] = List(_)
       |  def e: Int => Int => Int = _ => _ + 1
       |  def pe: PartialFunction[Int, PartialFunction[Int, Int]] = _ => _ + 1
       |}
       |""".stripMargin,
    """|[0:7..0:11): t190 <= _empty_/t190.
       |[1:6..1:7): a <= _empty_/t190.a().
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:22): String => scala/Predef.String#
       |[1:25..1:26): _ <= local0
       |[1:27..1:35): toString => scala/Any#toString().
       |[2:6..2:8): pa <= _empty_/t190.pa().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:37): String => scala/Predef.String#
       |[2:41..2:42): _ <= local2
       |[2:43..2:43):  <= local1
       |[2:43..2:51): toString => scala/Any#toString().
       |[3:6..3:7): b <= _empty_/t190.b().
       |[3:9..3:12): Int => scala/Int#
       |[3:16..3:19): Int => scala/Int#
       |[3:23..3:24): _ <= local9
       |[3:26..3:29): Int => scala/Int#
       |[3:31..3:32): + => scala/Int#`+`(+4).
       |[4:6..4:8): pb <= _empty_/t190.pb().
       |[4:10..4:25): PartialFunction => scala/PartialFunction#
       |[4:26..4:29): Int => scala/Int#
       |[4:31..4:34): Int => scala/Int#
       |[4:39..4:40): _ <= local13
       |[4:42..4:45): Int => scala/Int#
       |[4:47..4:47):  <= local10
       |[4:47..4:48): + => scala/Int#`+`(+4).
       |[5:6..5:7): c <= _empty_/t190.c().
       |[5:10..5:14): List => scala/package.List.
       |[5:18..5:21): map => scala/collection/immutable/List#map().
       |[5:22..5:23): _ <= local18
       |[5:24..5:25): + => scala/Int#`+`(+4).
       |[6:6..6:8): pc <= _empty_/t190.pc().
       |[6:11..6:15): List => scala/package.List.
       |[6:19..6:26): collect => scala/collection/immutable/List#collect().
       |[6:27..6:28): _ <= local20
       |[6:29..6:29):  <= local19
       |[6:29..6:30): + => scala/Int#`+`(+4).
       |[7:6..7:7): d <= _empty_/t190.d().
       |[7:9..7:12): Int => scala/Int#
       |[7:16..7:20): List => scala/package.List#
       |[7:21..7:24): Int => scala/Int#
       |[7:28..7:32): List => scala/package.List.
       |[7:33..7:34): _ <= local27
       |[8:6..8:8): pd <= _empty_/t190.pd().
       |[8:10..8:25): PartialFunction => scala/PartialFunction#
       |[8:26..8:29): Int => scala/Int#
       |[8:31..8:35): List => scala/package.List#
       |[8:36..8:39): Int => scala/Int#
       |[8:44..8:48): List => scala/package.List.
       |[8:48..8:48):  <= local28
       |[8:49..8:50): _ <= local31
       |[9:6..9:7): e <= _empty_/t190.e().
       |[9:9..9:12): Int => scala/Int#
       |[9:16..9:19): Int => scala/Int#
       |[9:23..9:26): Int => scala/Int#
       |[9:29..9:30): _ <= local36
       |[9:34..9:35): _ <= local37
       |[9:36..9:37): + => scala/Int#`+`(+4).
       |[10:6..10:8): pe <= _empty_/t190.pe().
       |[10:10..10:25): PartialFunction => scala/PartialFunction#
       |[10:26..10:29): Int => scala/Int#
       |[10:31..10:46): PartialFunction => scala/PartialFunction#
       |[10:47..10:50): Int => scala/Int#
       |[10:52..10:55): Int => scala/Int#
       |[10:60..10:61): _ <= local41
       |[10:65..10:66): _ <= local39
       |[10:67..10:67):  <= local45
       |[10:67..10:68): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "implicit parameters of lambdas",
    """|object t197 {
       |  val f: Int => Int = { implicit x: Int => x + 1 }
       |  val pf: PartialFunction[Int, Int] = { implicit x: Int => x + 1 }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t197 <= _empty_/t197.
       |[1:6..1:7): f <= _empty_/t197.f.
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:33..1:34): x <= local0
       |[1:36..1:39): Int => scala/Int#
       |[1:43..1:44): x => local0
       |[1:45..1:46): + => scala/Int#`+`(+4).
       |[2:6..2:8): pf <= _empty_/t197.pf.
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:59..2:60): x => local4
       |[2:61..2:61):  <= local1
       |[2:61..2:62): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "untyped implicit parameters of lambdas",
    """|object t198 {
       |  val f: Int => Int = { implicit x => x + 1 }
       |  val pf: PartialFunction[Int, Int] = { implicit x => x + 1 }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t198 <= _empty_/t198.
       |[1:6..1:7): f <= _empty_/t198.f.
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:33..1:34): x <= local0
       |[1:38..1:39): x => local0
       |[1:40..1:41): + => scala/Int#`+`(+4).
       |[2:6..2:8): pf <= _empty_/t198.pf.
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:54..2:55): x => local4
       |[2:56..2:56):  <= local1
       |[2:56..2:57): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "eta-expansions of a partial function",
    """|object t202 {
       |  def foo(x: Int) = x
       |  val pf: PartialFunction[Int, Int] = foo _
       |}
       |""".stripMargin,
    """|_empty_/t202. => final object t202 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t202.foo(). => method foo(x: Int): Int
       |  x => _empty_/t202.foo().(x)
       |  Int => scala/Int#
       |_empty_/t202.foo().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/t202.pf. => val method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local6
       |  Int => scala/Int#
       |  B1 => local5
       |  x => local3
       |  default => local4
       |  Function1 => scala/Function1#
       |local2 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local7
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic param x: A1
       |  A1 => local6
       |local4 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local6
       |  B1 => local5
       |local5 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local6 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local7 => synthetic param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "eta-expansions of a partial function",
    """|object t202 {
       |  def foo(x: Int) = x
       |  val pf: PartialFunction[Int, Int] = foo _
       |}
       |""".stripMargin,
    """|[0:7..0:11): t202 <= _empty_/t202.
       |[1:6..1:9): foo <= _empty_/t202.foo().
       |[1:10..1:11): x <= _empty_/t202.foo().(x)
       |[1:13..1:16): Int => scala/Int#
       |[1:20..1:21): x => _empty_/t202.foo().(x)
       |[2:6..2:8): pf <= _empty_/t202.pf.
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:38..2:38):  <= local0
       |[2:38..2:41): foo => _empty_/t202.foo().
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function lambdas in braces with one statement, a definition or none",
    """|object t205 {
       |  val pa: PartialFunction[Int, Int] = { x => x + 1 }
       |  val pb: PartialFunction[Int, Unit] = { x => def y = x }
       |  val pc: PartialFunction[Int, Unit] = { x => }
       |  val pd = List(1).collect { x =>
       |    val y = x
       |    y
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t205 <= _empty_/t205.
       |[1:6..1:8): pa <= _empty_/t205.pa.
       |[1:10..1:25): PartialFunction => scala/PartialFunction#
       |[1:26..1:29): Int => scala/Int#
       |[1:31..1:34): Int => scala/Int#
       |[1:45..1:46): x => local3
       |[1:47..1:47):  <= local0
       |[1:47..1:48): + => scala/Int#`+`(+4).
       |[2:6..2:8): pb <= _empty_/t205.pb.
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:35): Unit => scala/Unit#
       |[2:43..2:43):  <= local8
       |[2:50..2:51): y <= local15
       |[2:54..2:55): x => local11
       |[3:6..3:8): pc <= _empty_/t205.pc.
       |[3:10..3:25): PartialFunction => scala/PartialFunction#
       |[3:26..3:29): Int => scala/Int#
       |[3:31..3:35): Unit => scala/Unit#
       |[3:43..3:43):  <= local17
       |[4:6..4:8): pd <= _empty_/t205.pd.
       |[4:11..4:15): List => scala/package.List.
       |[4:19..4:26): collect => scala/collection/immutable/List#collect().
       |[4:31..4:31):  <= local26
       |[5:8..5:9): y <= local32
       |[5:12..5:13): x => local29
       |[6:4..6:5): y => local32
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "a for comprehension over a map of a partial function",
    """|object t208 {
       |  class C { def map(f: PartialFunction[Int, Int]): List[Int] = ??? }
       |  val c = new C
       |  val a = for (x <- c) yield x + 1
       |  val b: Int => List[Int] = _ :: (for (x <- c) yield x + 1)
       |  val d: Int => List[Int] = y => y :: (for (x <- c) yield x + 1)
       |}
       |""".stripMargin,
    """|[0:7..0:11): t208 <= _empty_/t208.
       |[1:8..1:9): C <= _empty_/t208.C#
       |[1:10..1:10):  <= _empty_/t208.C#`<init>`().
       |[1:16..1:19): map <= _empty_/t208.C#map().
       |[1:20..1:21): f <= _empty_/t208.C#map().(f)
       |[1:23..1:38): PartialFunction => scala/PartialFunction#
       |[1:39..1:42): Int => scala/Int#
       |[1:44..1:47): Int => scala/Int#
       |[1:51..1:55): List => scala/package.List#
       |[1:56..1:59): Int => scala/Int#
       |[1:63..1:66): ??? => scala/Predef.`???`().
       |[2:6..2:7): c <= _empty_/t208.c.
       |[2:14..2:15): C => _empty_/t208.C#
       |[2:15..2:15):  => _empty_/t208.C#`<init>`().
       |[3:6..3:7): a <= _empty_/t208.a.
       |[3:20..3:21): c => _empty_/t208.c.
       |[3:29..3:30): x => local3
       |[3:31..3:31):  <= local0
       |[3:31..3:32): + => scala/Int#`+`(+4).
       |[4:6..4:7): b <= _empty_/t208.b.
       |[4:9..4:12): Int => scala/Int#
       |[4:16..4:20): List => scala/package.List#
       |[4:21..4:24): Int => scala/Int#
       |[4:28..4:29): _ <= local8
       |[4:30..4:32): :: => scala/collection/immutable/List#`::`().
       |[4:44..4:45): c => _empty_/t208.c.
       |[4:53..4:54): x => local12
       |[4:55..4:55):  <= local9
       |[4:55..4:56): + => scala/Int#`+`(+4).
       |[5:6..5:7): d <= _empty_/t208.d.
       |[5:9..5:12): Int => scala/Int#
       |[5:16..5:20): List => scala/package.List#
       |[5:21..5:24): Int => scala/Int#
       |[5:28..5:29): y <= local17
       |[5:33..5:34): y => local17
       |[5:35..5:37): :: => scala/collection/immutable/List#`::`().
       |[5:49..5:50): c => _empty_/t208.c.
       |[5:58..5:59): x => local21
       |[5:60..5:60):  <= local18
       |[5:60..5:61): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "a for comprehension with values over a map of a partial function",
    """|object t215 {
       |  class C {
       |    def map[B](f: PartialFunction[Int, B]): List[B] = ???
       |    def flatMap[B](f: PartialFunction[Int, List[B]]): List[B] = ???
       |    def foreach(f: PartialFunction[Int, Unit]): Unit = ???
       |    def withFilter(f: PartialFunction[Int, Boolean]): C = ???
       |  }
       |  val c = new C
       |  val a = for (x <- c; y = x + 1; z = y + 1) yield z
       |  val b = for (x <- c if x > 0; y = x + 1) yield y
       |  val f = for (x <- c; y = x + 1 if y > 0) yield y
       |  val g = for (x <- c; z <- List(x); y = z) yield y
       |  for (x <- c; y = x + 1) println(y)
       |}
       |""".stripMargin,
    """|[0:7..0:11): t215 <= _empty_/t215.
       |[1:8..1:9): C <= _empty_/t215.C#
       |[1:10..1:10):  <= _empty_/t215.C#`<init>`().
       |[2:8..2:11): map <= _empty_/t215.C#map().
       |[2:12..2:13): B <= _empty_/t215.C#map().[B]
       |[2:15..2:16): f <= _empty_/t215.C#map().(f)
       |[2:18..2:33): PartialFunction => scala/PartialFunction#
       |[2:34..2:37): Int => scala/Int#
       |[2:39..2:40): B => _empty_/t215.C#map().[B]
       |[2:44..2:48): List => scala/package.List#
       |[2:49..2:50): B => _empty_/t215.C#map().[B]
       |[2:54..2:57): ??? => scala/Predef.`???`().
       |[3:8..3:15): flatMap <= _empty_/t215.C#flatMap().
       |[3:16..3:17): B <= _empty_/t215.C#flatMap().[B]
       |[3:19..3:20): f <= _empty_/t215.C#flatMap().(f)
       |[3:22..3:37): PartialFunction => scala/PartialFunction#
       |[3:38..3:41): Int => scala/Int#
       |[3:43..3:47): List => scala/package.List#
       |[3:48..3:49): B => _empty_/t215.C#flatMap().[B]
       |[3:54..3:58): List => scala/package.List#
       |[3:59..3:60): B => _empty_/t215.C#flatMap().[B]
       |[3:64..3:67): ??? => scala/Predef.`???`().
       |[4:8..4:15): foreach <= _empty_/t215.C#foreach().
       |[4:16..4:17): f <= _empty_/t215.C#foreach().(f)
       |[4:19..4:34): PartialFunction => scala/PartialFunction#
       |[4:35..4:38): Int => scala/Int#
       |[4:40..4:44): Unit => scala/Unit#
       |[4:48..4:52): Unit => scala/Unit#
       |[4:55..4:58): ??? => scala/Predef.`???`().
       |[5:8..5:18): withFilter <= _empty_/t215.C#withFilter().
       |[5:19..5:20): f <= _empty_/t215.C#withFilter().(f)
       |[5:22..5:37): PartialFunction => scala/PartialFunction#
       |[5:38..5:41): Int => scala/Int#
       |[5:43..5:50): Boolean => scala/Boolean#
       |[5:54..5:55): C => _empty_/t215.C#
       |[5:58..5:61): ??? => scala/Predef.`???`().
       |[7:6..7:7): c <= _empty_/t215.c.
       |[7:14..7:15): C => _empty_/t215.C#
       |[7:15..7:15):  => _empty_/t215.C#`<init>`().
       |[8:6..8:7): a <= _empty_/t215.a.
       |[8:20..8:21): c => _empty_/t215.c.
       |[8:23..8:23):  <= local2
       |[8:23..8:24): y <= local8
       |[8:27..8:28): x => local5
       |[8:29..8:30): + => scala/Int#`+`(+4).
       |[8:34..8:35): z <= local9
       |[8:38..8:39): y => local8
       |[8:40..8:41): + => scala/Int#`+`(+4).
       |[8:51..8:52): z => local9
       |[9:6..9:7): b <= _empty_/t215.b.
       |[9:20..9:21): c => _empty_/t215.c.
       |[9:25..9:26): x => local17
       |[9:27..9:27):  <= local14
       |[9:27..9:28): > => scala/Int#`>`(+3).
       |[9:32..9:32):  <= local22
       |[9:32..9:33): y <= local28
       |[9:36..9:37): x => local25
       |[9:38..9:39): + => scala/Int#`+`(+4).
       |[9:49..9:50): y => local28
       |[10:6..10:7): f <= _empty_/t215.f.
       |[10:20..10:21): c => _empty_/t215.c.
       |[10:23..10:23):  <= local34
       |[10:23..10:24): y <= local40
       |[10:27..10:28): x => local37
       |[10:29..10:30): + => scala/Int#`+`(+4).
       |[10:36..10:37): y => local40
       |[10:38..10:39): > => scala/Int#`>`(+3).
       |[10:49..10:50): y => local40
       |[11:6..11:7): g <= _empty_/t215.g.
       |[11:20..11:21): c => _empty_/t215.c.
       |[11:23..11:24): z <= local50
       |[11:25..11:25):  <= local44
       |[11:28..11:32): List => scala/package.List.
       |[11:33..11:34): x => local47
       |[11:37..11:38): y <= local52
       |[11:41..11:42): z => local50
       |[11:50..11:51): y => local52
       |[12:12..12:13): c => _empty_/t215.c.
       |[12:15..12:15):  <= local56
       |[12:15..12:16): y <= local62
       |[12:19..12:20): x => local59
       |[12:21..12:22): + => scala/Int#`+`(+4).
       |[12:26..12:33): println => scala/Predef.println(+1).
       |[12:34..12:35): y => local62
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function lambdas in the enumerators of a for",
    """|object t224 {
       |  val a = for (x <- List(1).collect(_ + 1)) yield x
       |  val b = for (x <- List(1); y = List(x).collect(z => z + 1)) yield y
       |  val c = for (x <- List(1) if List(x).collect((z: Int) => z).nonEmpty) yield x
       |  val d = for (x <- List(1)) yield List(x).collect(z => z + x)
       |}
       |""".stripMargin,
    """|_empty_/t224. => final object t224 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t224.a. => val method a: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t224.b. => val method b: List[List[Int]]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t224.c. => val method c: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t224.d. => val method d: List[List[Int]]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local2 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local7
       |  Int => scala/Int#
       |  B1 => local6
       |  x$1 => local4
       |  default => local5
       |  Function1 => scala/Function1#
       |local3 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$1 => local8
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic param x$1: A1
       |  A1 => local7
       |local5 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local7
       |  B1 => local6
       |local6 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local7 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local8 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local9 => val local x: Int
       |  Int => scala/Int#
       |local10 => synthetic param x$2: Tuple2[Int, List[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
       |local11 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local12 => final method applyOrElse[A1 <: Int, B1 >: Int](z: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local17
       |  Int => scala/Int#
       |  B1 => local16
       |  z => local14
       |  default => local15
       |  Function1 => scala/Function1#
       |local13 => final method isDefinedAt(z: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  z => local18
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local14 => synthetic param z: A1
       |  A1 => local17
       |local15 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local17
       |  B1 => local16
       |local16 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local17 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local18 => synthetic param z: Int
       |  Int => scala/Int#
       |local19 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local20 => param x: Int
       |  Int => scala/Int#
       |local21 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local22 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  A1 => local21
       |  Serializable => java/io/Serializable#
       |local23 => final method applyOrElse[A1 <: Int, B1 >: A1](z: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local21
       |  Int => scala/Int#
       |  B1 => local27
       |  z => local25
       |  default => local26
       |  Function1 => scala/Function1#
       |local24 => final method isDefinedAt(z: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  z => local28
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local25 => synthetic param z: A1
       |  A1 => local21
       |local26 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local21
       |  B1 => local27
       |local27 => typeparam B1 >: A1
       |  A1 => local21
       |local28 => synthetic param z: Int
       |  Int => scala/Int#
       |local29 => param x: Int
       |  Int => scala/Int#
       |local30 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local31 => final method applyOrElse[A1 <: Int, B1 >: Int](z: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local36
       |  Int => scala/Int#
       |  B1 => local35
       |  z => local33
       |  default => local34
       |  Function1 => scala/Function1#
       |local32 => final method isDefinedAt(z: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  z => local37
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local33 => synthetic param z: A1
       |  A1 => local36
       |local34 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local36
       |  B1 => local35
       |local35 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local36 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local37 => synthetic param z: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function lambdas in the enumerators of a for",
    """|object t224 {
       |  val a = for (x <- List(1).collect(_ + 1)) yield x
       |  val b = for (x <- List(1); y = List(x).collect(z => z + 1)) yield y
       |  val c = for (x <- List(1) if List(x).collect((z: Int) => z).nonEmpty) yield x
       |  val d = for (x <- List(1)) yield List(x).collect(z => z + x)
       |}
       |""".stripMargin,
    """|[0:7..0:11): t224 <= _empty_/t224.
       |[1:6..1:7): a <= _empty_/t224.a.
       |[1:15..1:16): x <= local0
       |[1:20..1:24): List => scala/package.List.
       |[1:28..1:35): collect => scala/collection/immutable/List#collect().
       |[1:36..1:37): _ <= local2
       |[1:38..1:38):  <= local1
       |[1:38..1:39): + => scala/Int#`+`(+4).
       |[1:50..1:51): x => local0
       |[2:6..2:7): b <= _empty_/t224.b.
       |[2:15..2:16): x <= local9
       |[2:20..2:24): List => scala/package.List.
       |[2:29..2:30): y <= local19
       |[2:33..2:37): List => scala/package.List.
       |[2:38..2:39): x => local9
       |[2:41..2:48): collect => scala/collection/immutable/List#collect().
       |[2:54..2:55): z => local14
       |[2:56..2:56):  <= local11
       |[2:56..2:57): + => scala/Int#`+`(+4).
       |[2:68..2:69): y => local19
       |[3:6..3:7): c <= _empty_/t224.c.
       |[3:15..3:16): x <= local20
       |[3:20..3:24): List => scala/package.List.
       |[3:31..3:35): List => scala/package.List.
       |[3:36..3:37): x => local20
       |[3:39..3:46): collect => scala/collection/immutable/List#collect().
       |[3:59..3:59):  <= local22
       |[3:59..3:60): z => local25
       |[3:62..3:70): nonEmpty => scala/collection/IterableOnceOps#nonEmpty().
       |[3:78..3:79): x => local20
       |[4:6..4:7): d <= _empty_/t224.d.
       |[4:15..4:16): x <= local29
       |[4:20..4:24): List => scala/package.List.
       |[4:35..4:39): List => scala/package.List.
       |[4:40..4:41): x => local29
       |[4:43..4:50): collect => scala/collection/immutable/List#collect().
       |[4:56..4:57): z => local33
       |[4:58..4:58):  <= local30
       |[4:58..4:59): + => scala/Int#`+`(+4).
       |[4:60..4:61): x => local29
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "typed parameters of partial function lambdas with names of other symbols",
    """|object t231 {
       |  class Int
       |  object o { class Int extends t231.Int }
       |  type List[A] = scala.collection.Seq[A]
       |  val a: PartialFunction[o.Int, scala.Int] = (x: Int) => 1
       |  val b: PartialFunction[scala.List[scala.Int], scala.Int] = (x: List[scala.Int]) => x.hashCode
       |  val c: o.Int => scala.Int = (x: Int) => 1
       |  val d: scala.List[scala.Int] => scala.Int = (x: List[scala.Int]) => x.hashCode
       |}
       |""".stripMargin,
    """|[0:7..0:11): t231 <= _empty_/t231.
       |[1:8..1:11): Int <= _empty_/t231.Int#
       |[1:11..1:11):  <= _empty_/t231.Int#`<init>`().
       |[2:9..2:10): o <= _empty_/t231.o.
       |[2:19..2:22): Int <= _empty_/t231.o.Int#
       |[2:23..2:23):  <= _empty_/t231.o.Int#`<init>`().
       |[2:31..2:35): t231 => _empty_/t231.
       |[2:36..2:39): Int => _empty_/t231.Int#
       |[2:39..2:39):  => _empty_/t231.Int#`<init>`().
       |[3:7..3:11): List <= _empty_/t231.List#
       |[3:12..3:13): A <= _empty_/t231.List#[A]
       |[3:17..3:22): scala => scala/
       |[3:23..3:33): collection => scala/collection/
       |[3:34..3:37): Seq => scala/collection/Seq#
       |[3:38..3:39): A => _empty_/t231.List#[A]
       |[4:6..4:7): a <= _empty_/t231.a.
       |[4:9..4:24): PartialFunction => scala/PartialFunction#
       |[4:25..4:26): o => _empty_/t231.o.
       |[4:27..4:30): Int => _empty_/t231.o.Int#
       |[4:32..4:37): scala => scala/
       |[4:38..4:41): Int => scala/Int#
       |[4:57..4:57):  <= local0
       |[5:6..5:7): b <= _empty_/t231.b.
       |[5:9..5:24): PartialFunction => scala/PartialFunction#
       |[5:25..5:30): scala => scala/
       |[5:31..5:35): List => scala/package.List#
       |[5:36..5:41): scala => scala/
       |[5:42..5:45): Int => scala/Int#
       |[5:48..5:53): scala => scala/
       |[5:54..5:57): Int => scala/Int#
       |[5:85..5:86): x => local11
       |[5:87..5:87):  <= local8
       |[5:87..5:95): hashCode => scala/collection/Seq#hashCode().
       |[6:6..6:7): c <= _empty_/t231.c.
       |[6:9..6:10): o => _empty_/t231.o.
       |[6:11..6:14): Int => _empty_/t231.o.Int#
       |[6:18..6:23): scala => scala/
       |[6:24..6:27): Int => scala/Int#
       |[6:31..6:32): x <= local16
       |[6:34..6:37): Int => _empty_/t231.Int#
       |[7:6..7:7): d <= _empty_/t231.d.
       |[7:9..7:14): scala => scala/
       |[7:15..7:19): List => scala/package.List#
       |[7:20..7:25): scala => scala/
       |[7:26..7:29): Int => scala/Int#
       |[7:34..7:39): scala => scala/
       |[7:40..7:43): Int => scala/Int#
       |[7:47..7:48): x <= local17
       |[7:50..7:54): List => _empty_/t231.List#
       |[7:55..7:60): scala => scala/
       |[7:61..7:64): Int => scala/Int#
       |[7:70..7:71): x => local17
       |[7:72..7:80): hashCode => scala/collection/Seq#hashCode().
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "typed parameters of partial function lambdas through renamed imports",
    """|object t233 {
       |  object p { type I = Int; class B }
       |  import p.{B => C, I => J}
       |  val a: PartialFunction[p.B, Int] = (x: C) => 1
       |  val b: PartialFunction[Int, Int] = (x: J) => x
       |  val c: PartialFunction[p.B, Int] = (x: C) => x match { case _ => 1 }
       |  val d: PartialFunction[Int, Int] = (x: J) => x match { case y => y }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t233 <= _empty_/t233.
       |[1:9..1:10): p <= _empty_/t233.p.
       |[1:18..1:19): I <= _empty_/t233.p.I#
       |[1:22..1:25): Int => scala/Int#
       |[1:33..1:34): B <= _empty_/t233.p.B#
       |[1:34..1:34):  <= _empty_/t233.p.B#`<init>`().
       |[2:9..2:10): p => _empty_/t233.p.
       |[2:12..2:13): B => _empty_/t233.p.B#
       |[2:20..2:21): I => _empty_/t233.p.I#
       |[3:6..3:7): a <= _empty_/t233.a.
       |[3:9..3:24): PartialFunction => scala/PartialFunction#
       |[3:25..3:26): p => _empty_/t233.p.
       |[3:27..3:28): B => _empty_/t233.p.B#
       |[3:30..3:33): Int => scala/Int#
       |[3:47..3:47):  <= local0
       |[4:6..4:7): b <= _empty_/t233.b.
       |[4:9..4:24): PartialFunction => scala/PartialFunction#
       |[4:25..4:28): Int => scala/Int#
       |[4:30..4:33): Int => scala/Int#
       |[4:47..4:47):  <= local8
       |[4:47..4:48): x => local11
       |[5:6..5:7): c <= _empty_/t233.c.
       |[5:9..5:24): PartialFunction => scala/PartialFunction#
       |[5:25..5:26): p => _empty_/t233.p.
       |[5:27..5:28): B => _empty_/t233.p.B#
       |[5:30..5:33): Int => scala/Int#
       |[5:47..5:48): x => local19
       |[5:49..5:49):  <= local16
       |[6:6..6:7): d <= _empty_/t233.d.
       |[6:9..6:24): PartialFunction => scala/PartialFunction#
       |[6:25..6:28): Int => scala/Int#
       |[6:30..6:33): Int => scala/Int#
       |[6:47..6:48): x => local27
       |[6:49..6:49):  <= local24
       |[6:62..6:63): y <= local31
       |[6:67..6:68): y => local31
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "eta-expansions of a method as an argument and a placeholder",
    """|object t228 {
       |  def foo(x: Int) = x
       |  val a = List(1).collect(foo _)
       |  val b: PartialFunction[Int, Int] = foo(_)
       |}
       |""".stripMargin,
    """|_empty_/t228. => final object t228 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t228.a. => val method a: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t228.b. => val method b: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t228.foo(). => method foo(x: Int): Int
       |  x => _empty_/t228.foo().(x)
       |  Int => scala/Int#
       |_empty_/t228.foo().(x) => param x: Int
       |  Int => scala/Int#
       |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local6
       |  Int => scala/Int#
       |  B1 => local5
       |  x => local3
       |  default => local4
       |  Function1 => scala/Function1#
       |local2 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local7
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic param x: A1
       |  A1 => local6
       |local4 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local6
       |  B1 => local5
       |local5 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local6 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local7 => synthetic param x: Int
       |  Int => scala/Int#
       |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local9 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local14
       |  Int => scala/Int#
       |  B1 => local13
       |  x$1 => local11
       |  default => local12
       |  Function1 => scala/Function1#
       |local10 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$1 => local15
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local11 => synthetic param x$1: A1
       |  A1 => local14
       |local12 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local14
       |  B1 => local13
       |local13 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local14 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local15 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "eta-expansions of a method as an argument and a placeholder",
    """|object t228 {
       |  def foo(x: Int) = x
       |  val a = List(1).collect(foo _)
       |  val b: PartialFunction[Int, Int] = foo(_)
       |}
       |""".stripMargin,
    """|[0:7..0:11): t228 <= _empty_/t228.
       |[1:6..1:9): foo <= _empty_/t228.foo().
       |[1:10..1:11): x <= _empty_/t228.foo().(x)
       |[1:13..1:16): Int => scala/Int#
       |[1:20..1:21): x => _empty_/t228.foo().(x)
       |[2:6..2:7): a <= _empty_/t228.a.
       |[2:10..2:14): List => scala/package.List.
       |[2:18..2:25): collect => scala/collection/immutable/List#collect().
       |[2:26..2:26):  <= local0
       |[2:26..2:29): foo => _empty_/t228.foo().
       |[3:6..3:7): b <= _empty_/t228.b.
       |[3:9..3:24): PartialFunction => scala/PartialFunction#
       |[3:25..3:28): Int => scala/Int#
       |[3:30..3:33): Int => scala/Int#
       |[3:37..3:40): foo => _empty_/t228.foo().
       |[3:40..3:40):  <= local8
       |[3:41..3:42): _ <= local11
       |""".stripMargin,
  )

}

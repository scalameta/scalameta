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

  private def synthetics(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: synthetics", code, expected, "Synthetics")

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
       |local7 => param x: Int
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
       |[1:38..1:39): x <= local7
       |[1:38..1:48): x => x + 1 <= local0
       |[1:43..1:44): x => local7
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
       |[1:47..1:48): x <= local7
       |[1:47..1:53): x => x <= local0
       |[1:52..1:53): x => local7
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

  if (ScalaVersion.is213) symbols(
    "function literal and partial function literal",
    """|object t178 {
       |  def f: Int => Int = x => x + 1
       |  def pf: PartialFunction[Int, Int] = x => x + 1
       |  def cpf: PartialFunction[Int, Int] = { case x => x + 1 }
       |}
       |""".stripMargin,
    """|_empty_/t178. => final object t178 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t178.cpf(). => method cpf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t178.f(). => method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t178.pf(). => method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
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
       |local8 => param x: Int
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
       |local16 => val local x: Int
       |  Int => scala/Int#
       |local17 => synthetic param x1: Int
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
       |[2:38..2:39): x <= local8
       |[2:38..2:48): x => x + 1 <= local1
       |[2:43..2:44): x => local8
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
       |[2:38..2:48): x => x + 1 <= local1
       |[2:38..2:39): x <= local8
       |[2:43..2:44): x => local8
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

  if (ScalaVersion.is213) symbols(
    "partial function literal with a typed parameter",
    """|object t179 {
       |  def f: Int => Int = (x: Int) => x + 1
       |  def pf: PartialFunction[Int, Int] = (x: Int) => x + 1
       |}
       |""".stripMargin,
    """|_empty_/t179. => final object t179 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t179.f(). => method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t179.pf(). => method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
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
       |local8 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function literal with a typed parameter",
    """|object t179 {
       |  def f: Int => Int = (x: Int) => x + 1
       |  def pf: PartialFunction[Int, Int] = (x: Int) => x + 1
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
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
         |[2:38..2:55): (x: Int) => x + 1 <= local1
         |[2:39..2:40): x <= local8
         |[2:42..2:45): Int => scala/Int#
         |[2:50..2:51): x => local8
         |[2:52..2:53): + => scala/Int#`+`(+4).
         |""".stripMargin
    else
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
         |[2:38..2:55): (x: Int) => x + 1 <= local1
         |[2:39..2:40): x <= local8
         |[2:50..2:51): x => local8
         |[2:52..2:53): + => scala/Int#`+`(+4).
         |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "placeholder as a partial function",
    """|object t180 {
       |  def f: Int => Int = _ + 1
       |  def pf: PartialFunction[Int, Int] = _ + 1
       |}
       |""".stripMargin,
    """|_empty_/t180. => final object t180 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t180.f(). => method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t180.pf(). => method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local2 => final method applyOrElse[A1 <: Int, B1 >: Int](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local7
       |  Int => scala/Int#
       |  B1 => local6
       |  x$2 => local4
       |  default => local5
       |  Function1 => scala/Function1#
       |local3 => final method isDefinedAt(x$2: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$2 => local8
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic param x$2: A1
       |  A1 => local7
       |local5 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local7
       |  B1 => local6
       |local6 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local7 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local8 => synthetic param x$2: Int
       |  Int => scala/Int#
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
       |[2:38..2:39): _ <= local8
       |[2:38..2:43): _ + 1 <= local1
       |[2:40..2:41): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "wildcard parameter of a partial function literal",
    """|object t181 {
       |  def f: Int => Int = _ => 1
       |  def pf: PartialFunction[Int, Int] = _ => 1
       |}
       |""".stripMargin,
    """|_empty_/t181. => final object t181 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t181.f(). => method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t181.pf(). => method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => param x$1: Int
       |  Int => scala/Int#
       |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local2 => final method applyOrElse[A1 <: Int, B1 >: Int](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local7
       |  Int => scala/Int#
       |  B1 => local6
       |  x$2 => local4
       |  default => local5
       |  Function1 => scala/Function1#
       |local3 => final method isDefinedAt(x$2: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$2 => local8
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic param x$2: A1
       |  A1 => local7
       |local5 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local7
       |  B1 => local6
       |local6 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local7 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local8 => param x$2: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "wildcard parameter of a partial function literal",
    """|object t181 {
       |  def f: Int => Int = _ => 1
       |  def pf: PartialFunction[Int, Int] = _ => 1
       |}
       |""".stripMargin,
    """|[0:7..0:11): t181 <= _empty_/t181.
       |[1:6..1:7): f <= _empty_/t181.f().
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:22..1:23): _ <= local0
       |[2:6..2:8): pf <= _empty_/t181.pf().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:38..2:39): _ <= local8
       |[2:38..2:44): _ => 1 <= local1
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function literal in a field",
    """|class t182 {
       |  val f: Int => String = x => x.toString
       |  val pf: PartialFunction[Int, String] = x => x.toString
       |}
       |""".stripMargin,
    """|_empty_/t182# => class t182 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t182#`<init>`(). => primary ctor <init>()
       |_empty_/t182#f. => val method f: Function1[Int, String]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |_empty_/t182#pf. => val method pf: PartialFunction[Int, String]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, String] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |  Serializable => java/io/Serializable#
       |local2 => final method applyOrElse[A1 <: Int, B1 >: String](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local7
       |  Int => scala/Int#
       |  B1 => local6
       |  String => scala/Predef.String#
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
       |local6 => typeparam B1 >: String
       |  String => scala/Predef.String#
       |local7 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local8 => param x: Int
       |  Int => scala/Int#
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
       |[2:41..2:42): x <= local8
       |[2:41..2:56): x => x.toString <= local1
       |[2:46..2:47): x => local8
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
       |local8 => param x: Int
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
       |local16 => param x: Int
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
       |[2:26..2:27): x <= local8
       |[2:26..2:36): x => x + 1 <= local1
       |[2:31..2:32): x => local8
       |[2:33..2:34): + => scala/Int#`+`(+4).
       |[3:6..3:7): n <= _empty_/t183.n().
       |[3:10..3:14): List => scala/package.List.
       |[3:18..3:25): collect => scala/collection/immutable/List#collect().
       |[3:28..3:29): x <= local16
       |[3:28..3:34): x => x <= local10
       |[3:33..3:34): x => local16
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function literal in a lambda",
    """|object t184 {
       |  def m = List(1).map(y => List(2).collect(x => x + y))
       |}
       |""".stripMargin,
    """|_empty_/t184. => final object t184 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t184.m(). => method m: List[List[Int]]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => param y: Int
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
       |local8 => param x: Int
       |  Int => scala/Int#
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
       |[1:43..1:44): x <= local8
       |[1:43..1:53): x => x + y <= local1
       |[1:48..1:49): x => local8
       |[1:50..1:51): + => scala/Int#`+`(+4).
       |[1:52..1:53): y => local0
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function literal that shadows",
    """|object t185 {
       |  def m(x: String): PartialFunction[Int, Int] = x => List(x).map(x => x + 1).head
       |}
       |""".stripMargin,
    """|_empty_/t185. => final object t185 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t185.m(). => method m(x: String): PartialFunction[Int, Int]
       |  x => _empty_/t185.m().(x)
       |  String => scala/Predef.String#
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t185.m().(x) => param x: String
       |  String => scala/Predef.String#
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
       |local7 => param x: Int
       |  Int => scala/Int#
       |local8 => param x: A1
       |  A1 => local6
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
       |[1:48..1:49): x <= local7
       |[1:48..1:81): x => List(x).map(x => x + 1).head <= local0
       |[1:53..1:57): List => scala/package.List.
       |[1:58..1:59): x => local7
       |[1:61..1:64): map => scala/collection/immutable/List#map().
       |[1:65..1:66): x <= local8
       |[1:70..1:71): x => local8
       |[1:72..1:73): + => scala/Int#`+`(+4).
       |[1:77..1:81): head => scala/collection/IterableOps#head().
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function literal with a match",
    """|object t186 {
       |  def f: Int => Int = x => x match { case 1 => x }
       |  def pf: PartialFunction[Int, Int] = x => x match { case 1 => x }
       |}
       |""".stripMargin,
    """|_empty_/t186. => final object t186 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t186.f(). => method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t186.pf(). => method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
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
       |local8 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function literal with a match",
    """|object t186 {
       |  def f: Int => Int = x => x match { case 1 => x }
       |  def pf: PartialFunction[Int, Int] = x => x match { case 1 => x }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t186 <= _empty_/t186.
       |[1:6..1:7): f <= _empty_/t186.f().
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:22..1:23): x <= local0
       |[1:27..1:28): x => local0
       |[1:47..1:48): x => local0
       |[2:6..2:8): pf <= _empty_/t186.pf().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:38..2:39): x <= local8
       |[2:38..2:66): x => x match { case 1 => x } <= local1
       |[2:43..2:44): x => local8
       |[2:63..2:64): x => local8
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
       |local7 => param x: Int
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
       |local15 => param x: Int
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
       |local23 => param x: A1
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
       |[1:38..1:39): x <= local7
       |[1:38..1:84): x => x.asInstanceOf[Int] match { case 1 => x } <= local0
       |[1:43..1:44): x => local7
       |[1:45..1:57): asInstanceOf => scala/Any#asInstanceOf().
       |[1:58..1:61): Int => scala/Int#
       |[1:81..1:82): x => local7
       |[2:6..2:7): m <= _empty_/t187.m().
       |[2:10..2:14): List => scala/package.List.
       |[2:18..2:25): collect => scala/collection/immutable/List#collect().
       |[2:26..2:27): x <= local15
       |[2:26..2:32): x => x <= local9
       |[2:31..2:32): x => local15
       |[2:34..2:41): collect => scala/collection/immutable/List#collect().
       |[2:42..2:43): x <= local23
       |[2:42..2:52): x => x + 1 <= local16
       |[2:47..2:48): x => local23
       |[2:49..2:50): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function literals with typed parameters",
    """|object t188 {
       |  def f: List[Int] => String = (xs: List[Int]) => xs.mkString
       |  def pf: PartialFunction[List[Int], String] = (xs: List[Int]) => xs.mkString
       |  def g: String => Int = (s : String) => s.length
       |  def pg: PartialFunction[String, Int] = (s : String) => s.length
       |}
       |""".stripMargin,
    """|_empty_/t188. => final object t188 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t188.f(). => method f: Function1[List[Int], String]
       |  Function1 => scala/Function1#
       |  List => scala/package.List#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |_empty_/t188.g(). => method g: Function1[String, Int]
       |  Function1 => scala/Function1#
       |  String => scala/Predef.String#
       |  Int => scala/Int#
       |_empty_/t188.pf(). => method pf: PartialFunction[List[Int], String]
       |  PartialFunction => scala/PartialFunction#
       |  List => scala/package.List#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |_empty_/t188.pg(). => method pg: PartialFunction[String, Int]
       |  PartialFunction => scala/PartialFunction#
       |  String => scala/Predef.String#
       |  Int => scala/Int#
       |local0 => param xs: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], String] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  List => scala/package.List#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |  Serializable => java/io/Serializable#
       |local2 => final method applyOrElse[A1 <: List[Int], B1 >: String](xs: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local7
       |  List => scala/package.List#
       |  Int => scala/Int#
       |  B1 => local6
       |  String => scala/Predef.String#
       |  xs => local4
       |  default => local5
       |  Function1 => scala/Function1#
       |local3 => final method isDefinedAt(xs: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
       |  xs => local8
       |  List => scala/package.List#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic param xs: A1
       |  A1 => local7
       |local5 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local7
       |  B1 => local6
       |local6 => typeparam B1 >: String
       |  String => scala/Predef.String#
       |local7 => typeparam A1 <: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local8 => param xs: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local9 => param s: String
       |  String => scala/Predef.String#
       |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[String, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  String => scala/Predef.String#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local11 => final method applyOrElse[A1 <: String, B1 >: Int](s: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local16
       |  String => scala/Predef.String#
       |  B1 => local15
       |  Int => scala/Int#
       |  s => local13
       |  default => local14
       |  Function1 => scala/Function1#
       |local12 => final method isDefinedAt(s: String): Boolean <: scala/PartialFunction#isDefinedAt().
       |  s => local17
       |  String => scala/Predef.String#
       |  Boolean => scala/Boolean#
       |local13 => synthetic param s: A1
       |  A1 => local16
       |local14 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local16
       |  B1 => local15
       |local15 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local16 => typeparam A1 <: String
       |  String => scala/Predef.String#
       |local17 => param s: String
       |  String => scala/Predef.String#
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
    if (ScalaVersion.atLeast213_17)
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
         |[2:47..2:77): (xs: List[Int]) => xs.mkString <= local1
         |[2:48..2:50): xs <= local8
         |[2:52..2:56): List => scala/package.List#
         |[2:57..2:60): Int => scala/Int#
         |[2:66..2:68): xs => local8
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
         |[4:41..4:65): (s : String) => s.length <= local10
         |[4:42..4:43): s <= local17
         |[4:46..4:52): String => scala/Predef.String#
         |[4:57..4:58): s => local17
         |[4:59..4:65): length => java/lang/String#length().
         |""".stripMargin
    else
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
         |[2:47..2:77): (xs: List[Int]) => xs.mkString <= local1
         |[2:48..2:50): xs <= local8
         |[2:66..2:68): xs => local8
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
         |[4:41..4:65): (s : String) => s.length <= local10
         |[4:42..4:43): s <= local17
         |[4:57..4:58): s => local17
         |[4:59..4:65): length => java/lang/String#length().
         |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function literals with parentheses, a block or a nested lambda",
    """|object t189 {
       |  def pa: PartialFunction[Int, Int] = (x) => x + 1
       |  def pb: PartialFunction[Int, Int] = x => (x + 1)
       |  def pc: PartialFunction[Int, Int] = x => { x + 1 }
       |  def pd: PartialFunction[Int, PartialFunction[Int, Int]] = x => _ + x
       |  def pe: PartialFunction[Int, PartialFunction[Int, Int]] = x => y => x + y
       |}
       |""".stripMargin,
    """|_empty_/t189. => final object t189 extends AnyRef { +5 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t189.pa(). => method pa: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t189.pb(). => method pb: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t189.pc(). => method pc: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t189.pd(). => method pd: PartialFunction[Int, PartialFunction[Int, Int]]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t189.pe(). => method pe: PartialFunction[Int, PartialFunction[Int, Int]]
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
       |local7 => param x: Int
       |  Int => scala/Int#
       |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local9 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local14
       |  Int => scala/Int#
       |  B1 => local13
       |  x => local11
       |  default => local12
       |  Function1 => scala/Function1#
       |local10 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local15
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local11 => synthetic param x: A1
       |  A1 => local14
       |local12 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local14
       |  B1 => local13
       |local13 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local14 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local15 => param x: Int
       |  Int => scala/Int#
       |local16 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local17 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local22
       |  Int => scala/Int#
       |  B1 => local21
       |  x => local19
       |  default => local20
       |  Function1 => scala/Function1#
       |local18 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local23
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local19 => synthetic param x: A1
       |  A1 => local22
       |local20 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local22
       |  B1 => local21
       |local21 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local22 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local23 => param x: Int
       |  Int => scala/Int#
       |local24 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, PartialFunction[Int, Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  PartialFunction => scala/PartialFunction#
       |  Serializable => java/io/Serializable#
       |local25 => final method applyOrElse[A1 <: Int, B1 >: PartialFunction[Int, Int]](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local30
       |  Int => scala/Int#
       |  B1 => local29
       |  PartialFunction => scala/PartialFunction#
       |  x => local27
       |  default => local28
       |  Function1 => scala/Function1#
       |local26 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local39
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local27 => synthetic param x: A1
       |  A1 => local30
       |local28 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local30
       |  B1 => local29
       |local29 => typeparam B1 >: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local30 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local31 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local32 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local37
       |  Int => scala/Int#
       |  B1 => local36
       |  x$1 => local34
       |  default => local35
       |  Function1 => scala/Function1#
       |local33 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$1 => local38
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local34 => synthetic param x$1: A1
       |  A1 => local37
       |local35 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local37
       |  B1 => local36
       |local36 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local37 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local38 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local39 => param x: Int
       |  Int => scala/Int#
       |local40 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, PartialFunction[Int, Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  PartialFunction => scala/PartialFunction#
       |  Serializable => java/io/Serializable#
       |local41 => final method applyOrElse[A1 <: Int, B1 >: PartialFunction[Int, Int]](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local46
       |  Int => scala/Int#
       |  B1 => local45
       |  PartialFunction => scala/PartialFunction#
       |  x => local43
       |  default => local44
       |  Function1 => scala/Function1#
       |local42 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local54
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local43 => synthetic param x: A1
       |  A1 => local46
       |local44 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local46
       |  B1 => local45
       |local45 => typeparam B1 >: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local46 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local47 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local48 => final method applyOrElse[A1 <: Int, B1 >: Int](y: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local53
       |  Int => scala/Int#
       |  B1 => local52
       |  y => local50
       |  default => local51
       |  Function1 => scala/Function1#
       |local49 => final method isDefinedAt(y: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  y => local55
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local50 => synthetic param y: A1
       |  A1 => local53
       |local51 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local53
       |  B1 => local52
       |local52 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local53 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local54 => param x: Int
       |  Int => scala/Int#
       |local55 => param y: Int
       |  Int => scala/Int#
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
       |[1:38..1:50): (x) => x + 1 <= local0
       |[1:39..1:40): x <= local7
       |[1:45..1:46): x => local7
       |[1:47..1:48): + => scala/Int#`+`(+4).
       |[2:6..2:8): pb <= _empty_/t189.pb().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:38..2:39): x <= local15
       |[2:38..2:50): x => (x + 1) <= local8
       |[2:44..2:45): x => local15
       |[2:46..2:47): + => scala/Int#`+`(+4).
       |[3:6..3:8): pc <= _empty_/t189.pc().
       |[3:10..3:25): PartialFunction => scala/PartialFunction#
       |[3:26..3:29): Int => scala/Int#
       |[3:31..3:34): Int => scala/Int#
       |[3:38..3:39): x <= local23
       |[3:38..3:52): x => { x + 1 } <= local16
       |[3:45..3:46): x => local23
       |[3:47..3:48): + => scala/Int#`+`(+4).
       |[4:6..4:8): pd <= _empty_/t189.pd().
       |[4:10..4:25): PartialFunction => scala/PartialFunction#
       |[4:26..4:29): Int => scala/Int#
       |[4:31..4:46): PartialFunction => scala/PartialFunction#
       |[4:47..4:50): Int => scala/Int#
       |[4:52..4:55): Int => scala/Int#
       |[4:60..4:61): x <= local39
       |[4:60..4:70): x => _ + x <= local24
       |[4:65..4:66): _ <= local38
       |[4:65..4:70): _ + x <= local31
       |[4:67..4:68): + => scala/Int#`+`(+4).
       |[4:69..4:70): x => local39
       |[5:6..5:8): pe <= _empty_/t189.pe().
       |[5:10..5:25): PartialFunction => scala/PartialFunction#
       |[5:26..5:29): Int => scala/Int#
       |[5:31..5:46): PartialFunction => scala/PartialFunction#
       |[5:47..5:50): Int => scala/Int#
       |[5:52..5:55): Int => scala/Int#
       |[5:60..5:61): x <= local54
       |[5:60..5:75): x => y => x + y <= local40
       |[5:65..5:66): y <= local55
       |[5:65..5:75): y => x + y <= local47
       |[5:70..5:71): x => local54
       |[5:72..5:73): + => scala/Int#`+`(+4).
       |[5:74..5:75): y => local55
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
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
    """|_empty_/t190. => final object t190 extends AnyRef { +10 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t190.a(). => method a: Function1[Int, String]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |_empty_/t190.b(). => method b: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t190.c(). => method c: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t190.d(). => method d: Function1[Int, List[Int]]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  List => scala/package.List#
       |_empty_/t190.e(). => method e: Function1[Int, Function1[Int, Int]]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t190.pa(). => method pa: PartialFunction[Int, String]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |_empty_/t190.pb(). => method pb: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t190.pc(). => method pc: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t190.pd(). => method pd: PartialFunction[Int, List[Int]]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  List => scala/package.List#
       |_empty_/t190.pe(). => method pe: PartialFunction[Int, PartialFunction[Int, Int]]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, String] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |  Serializable => java/io/Serializable#
       |local2 => final method applyOrElse[A1 <: Int, B1 >: String](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local7
       |  Int => scala/Int#
       |  B1 => local6
       |  String => scala/Predef.String#
       |  x$2 => local4
       |  default => local5
       |  Function1 => scala/Function1#
       |local3 => final method isDefinedAt(x$2: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$2 => local8
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic param x$2: A1
       |  A1 => local7
       |local5 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local7
       |  B1 => local6
       |local6 => typeparam B1 >: String
       |  String => scala/Predef.String#
       |local7 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local8 => synthetic param x$2: Int
       |  Int => scala/Int#
       |local9 => synthetic param x$3: Int
       |  Int => scala/Int#
       |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local11 => final method applyOrElse[A1 <: Int, B1 >: Int](x$4: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local16
       |  Int => scala/Int#
       |  B1 => local15
       |  x$4 => local13
       |  default => local14
       |  Function1 => scala/Function1#
       |local12 => final method isDefinedAt(x$4: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$4 => local17
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local13 => synthetic param x$4: A1
       |  A1 => local16
       |local14 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local16
       |  B1 => local15
       |local15 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local16 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local17 => synthetic param x$4: Int
       |  Int => scala/Int#
       |local18 => synthetic param x$5: Int
       |  Int => scala/Int#
       |local19 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local20 => final method applyOrElse[A1 <: Int, B1 >: Int](x$6: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local25
       |  Int => scala/Int#
       |  B1 => local24
       |  x$6 => local22
       |  default => local23
       |  Function1 => scala/Function1#
       |local21 => final method isDefinedAt(x$6: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$6 => local26
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local22 => synthetic param x$6: A1
       |  A1 => local25
       |local23 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local25
       |  B1 => local24
       |local24 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local25 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local26 => synthetic param x$6: Int
       |  Int => scala/Int#
       |local27 => synthetic param x$7: Int
       |  Int => scala/Int#
       |local28 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, List[Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  List => scala/package.List#
       |  Serializable => java/io/Serializable#
       |local29 => final method applyOrElse[A1 <: Int, B1 >: List[Int]](x$8: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local34
       |  Int => scala/Int#
       |  B1 => local33
       |  List => scala/package.List#
       |  x$8 => local31
       |  default => local32
       |  Function1 => scala/Function1#
       |local30 => final method isDefinedAt(x$8: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$8 => local35
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local31 => synthetic param x$8: A1
       |  A1 => local34
       |local32 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local34
       |  B1 => local33
       |local33 => typeparam B1 >: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local34 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local35 => synthetic param x$8: Int
       |  Int => scala/Int#
       |local36 => param x$9: Int
       |  Int => scala/Int#
       |local37 => synthetic param x$10: Int
       |  Int => scala/Int#
       |local38 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, PartialFunction[Int, Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  PartialFunction => scala/PartialFunction#
       |  Serializable => java/io/Serializable#
       |local39 => final method applyOrElse[A1 <: Int, B1 >: PartialFunction[Int, Int]](x$11: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local44
       |  Int => scala/Int#
       |  B1 => local43
       |  PartialFunction => scala/PartialFunction#
       |  x$11 => local41
       |  default => local42
       |  Function1 => scala/Function1#
       |local40 => final method isDefinedAt(x$11: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$11 => local45
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local41 => synthetic param x$11: A1
       |  A1 => local44
       |local42 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local44
       |  B1 => local43
       |local43 => typeparam B1 >: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local44 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local45 => param x$11: Int
       |  Int => scala/Int#
       |local46 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local47 => final method applyOrElse[A1 <: Int, B1 >: Int](x$12: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local52
       |  Int => scala/Int#
       |  B1 => local51
       |  x$12 => local49
       |  default => local50
       |  Function1 => scala/Function1#
       |local48 => final method isDefinedAt(x$12: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$12 => local53
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local49 => synthetic param x$12: A1
       |  A1 => local52
       |local50 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local52
       |  B1 => local51
       |local51 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local52 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local53 => synthetic param x$12: Int
       |  Int => scala/Int#
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
       |[2:41..2:42): _ <= local8
       |[2:41..2:51): _.toString <= local1
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
       |[4:38..4:50): (_: Int) + 1 <= local10
       |[4:39..4:40): _ <= local17
       |[4:42..4:45): Int => scala/Int#
       |[4:47..4:48): + => scala/Int#`+`(+4).
       |[5:6..5:7): c <= _empty_/t190.c().
       |[5:10..5:14): List => scala/package.List.
       |[5:18..5:21): map => scala/collection/immutable/List#map().
       |[5:22..5:23): _ <= local18
       |[5:24..5:25): + => scala/Int#`+`(+4).
       |[6:6..6:8): pc <= _empty_/t190.pc().
       |[6:11..6:15): List => scala/package.List.
       |[6:19..6:26): collect => scala/collection/immutable/List#collect().
       |[6:27..6:28): _ <= local26
       |[6:27..6:32): _ + 1 <= local19
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
       |[8:44..8:51): List(_) <= local28
       |[8:49..8:50): _ <= local35
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
       |[10:60..10:61): _ <= local45
       |[10:60..10:70): _ => _ + 1 <= local38
       |[10:65..10:66): _ <= local53
       |[10:65..10:70): _ + 1 <= local46
       |[10:67..10:68): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  symbols(
    "partial function literals with a match on the parameter",
    """|object t196 {
       |  def pf: PartialFunction[Int, Int] = x => x match { case 1 => x }
       |  def qf: PartialFunction[Int, Int] = x => x.asInstanceOf[Int] match { case 1 => x }
       |  def rf: PartialFunction[Int, Int] = _ match { case 1 => 2 }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t196. => final object t196 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t196.pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t196.qf(). => method qf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t196.rf(). => method rf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
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
         |local7 => param x: Int
         |  Int => scala/Int#
         |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local9 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local14
         |  Int => scala/Int#
         |  B1 => local13
         |  x => local11
         |  default => local12
         |  Function1 => scala/Function1#
         |local10 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local15
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local11 => synthetic param x: A1
         |  A1 => local14
         |local12 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local14
         |  B1 => local13
         |local13 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local14 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local15 => param x: Int
         |  Int => scala/Int#
         |local16 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local17 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local22
         |  Int => scala/Int#
         |  B1 => local21
         |  x$1 => local19
         |  default => local20
         |  Function1 => scala/Function1#
         |local18 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local23
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local19 => synthetic param x$1: A1
         |  A1 => local22
         |local20 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local22
         |  B1 => local21
         |local21 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local22 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local23 => synthetic param x$1: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t196. => final object t196 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t196.pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t196.qf(). => method qf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t196.rf(). => method rf: PartialFunction[Int, Int]
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
         |local7 => param x: Int
         |  Int => scala/Int#
         |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local9 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local14
         |  Int => scala/Int#
         |  B1 => local13
         |  x => local11
         |  default => local12
         |  Function1 => scala/Function1#
         |local10 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local15
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local11 => synthetic param x: A1
         |  A1 => local14
         |local12 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local14
         |  B1 => local13
         |local13 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local14 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local15 => param x: Int
         |  Int => scala/Int#
         |local16 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local17 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local22
         |  Int => scala/Int#
         |  B1 => local21
         |  x$1 => local19
         |  default => local20
         |  Function1 => scala/Function1#
         |local18 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local23
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local19 => synthetic param x$1: A1
         |  A1 => local22
         |local20 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local22
         |  B1 => local21
         |local21 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local22 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local23 => synthetic param x$1: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "partial function literals with a match on the parameter",
    """|object t196 {
       |  def pf: PartialFunction[Int, Int] = x => x match { case 1 => x }
       |  def qf: PartialFunction[Int, Int] = x => x.asInstanceOf[Int] match { case 1 => x }
       |  def rf: PartialFunction[Int, Int] = _ match { case 1 => 2 }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t196 <= _empty_/t196.
       |[1:6..1:8): pf <= _empty_/t196.pf().
       |[1:10..1:25): PartialFunction => scala/PartialFunction#
       |[1:26..1:29): Int => scala/Int#
       |[1:31..1:34): Int => scala/Int#
       |[1:38..1:39): x <= local7
       |[1:38..1:66): x => x match { case 1 => x } <= local0
       |[1:43..1:44): x => local7
       |[1:63..1:64): x => local7
       |[2:6..2:8): qf <= _empty_/t196.qf().
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:34): Int => scala/Int#
       |[2:38..2:39): x <= local15
       |[2:38..2:84): x => x.asInstanceOf[Int] match { case 1 => x } <= local8
       |[2:43..2:44): x => local15
       |[2:45..2:57): asInstanceOf => scala/Any#asInstanceOf().
       |[2:58..2:61): Int => scala/Int#
       |[2:81..2:82): x => local15
       |[3:6..3:8): rf <= _empty_/t196.rf().
       |[3:10..3:25): PartialFunction => scala/PartialFunction#
       |[3:26..3:29): Int => scala/Int#
       |[3:31..3:34): Int => scala/Int#
       |[3:38..3:39): _ <= local23
       |[3:38..3:61): _ match { case 1 => 2 } <= local16
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "implicit parameters of lambdas",
    """|object t197 {
       |  val f: Int => Int = { implicit x: Int => x + 1 }
       |  val pf: PartialFunction[Int, Int] = { implicit x: Int => x + 1 }
       |}
       |""".stripMargin,
    """|_empty_/t197. => final object t197 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t197.f. => val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t197.pf. => val method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => implicit param x: Int
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
       |local3 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
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
       |local8 => implicit param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "implicit parameters of lambdas",
    """|object t197 {
       |  val f: Int => Int = { implicit x: Int => x + 1 }
       |  val pf: PartialFunction[Int, Int] = { implicit x: Int => x + 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
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
         |[2:40..2:64): implicit x: Int => x + 1 <= local1
         |[2:49..2:50): x <= local8
         |[2:52..2:55): Int => scala/Int#
         |[2:59..2:60): x => local8
         |[2:61..2:62): + => scala/Int#`+`(+4).
         |""".stripMargin
    else
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
         |[2:40..2:64): implicit x: Int => x + 1 <= local1
         |[2:49..2:50): x <= local8
         |[2:59..2:60): x => local8
         |[2:61..2:62): + => scala/Int#`+`(+4).
         |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "untyped implicit parameters of lambdas",
    """|object t198 {
       |  val f: Int => Int = { implicit x => x + 1 }
       |  val pf: PartialFunction[Int, Int] = { implicit x => x + 1 }
       |}
       |""".stripMargin,
    """|_empty_/t198. => final object t198 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t198.f. => val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t198.pf. => val method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => implicit param x: Int
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
       |local3 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
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
       |local8 => implicit param x: Int
       |  Int => scala/Int#
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
       |[2:40..2:59): implicit x => x + 1 <= local1
       |[2:49..2:50): x <= local8
       |[2:54..2:55): x => local8
       |[2:56..2:57): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  symbols(
    "implicit parameters of lambdas with a match",
    """|object t199 {
       |  val f: Int => Int = { implicit x: Int => x match { case 1 => x } }
       |  val pf: PartialFunction[Int, Int] = { implicit x: Int => x match { case 1 => x } }
       |  val g: Int => Int = { implicit x => x match { case 1 => x } }
       |  val pg: PartialFunction[Int, Int] = { implicit x => x match { case 1 => x } }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t199. => final object t199 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t199.f. => val method f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t199.g. => val method g: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t199.pf. => val method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t199.pg. => val method pg: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => implicit param x: Int
         |  Int => scala/Int#
         |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local2 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local7
         |  Int => scala/Int#
         |  B1 => local6
         |  x => local4
         |  default => local5
         |  Function1 => scala/Function1#
         |local3 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
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
         |local8 => implicit param x: Int
         |  Int => scala/Int#
         |local9 => implicit param x: Int
         |  Int => scala/Int#
         |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local11 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local16
         |  Int => scala/Int#
         |  B1 => local15
         |  x => local13
         |  default => local14
         |  Function1 => scala/Function1#
         |local12 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local13 => synthetic param x: A1
         |  A1 => local16
         |local14 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local16
         |  B1 => local15
         |local15 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local16 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local17 => implicit param x: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t199. => final object t199 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t199.f. => val method f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t199.g. => val method g: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t199.pf. => val method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t199.pg. => val method pg: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => implicit param x: Int
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
         |local3 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
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
         |local8 => implicit param x: Int
         |  Int => scala/Int#
         |local9 => implicit param x: Int
         |  Int => scala/Int#
         |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local11 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local16
         |  Int => scala/Int#
         |  B1 => local15
         |  x => local13
         |  default => local14
         |  Function1 => scala/Function1#
         |local12 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local13 => synthetic param x: A1
         |  A1 => local16
         |local14 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local16
         |  B1 => local15
         |local15 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local16 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local17 => implicit param x: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "implicit parameters of lambdas with a match",
    """|object t199 {
       |  val f: Int => Int = { implicit x: Int => x match { case 1 => x } }
       |  val pf: PartialFunction[Int, Int] = { implicit x: Int => x match { case 1 => x } }
       |  val g: Int => Int = { implicit x => x match { case 1 => x } }
       |  val pg: PartialFunction[Int, Int] = { implicit x => x match { case 1 => x } }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t199 <= _empty_/t199.
         |[1:6..1:7): f <= _empty_/t199.f.
         |[1:9..1:12): Int => scala/Int#
         |[1:16..1:19): Int => scala/Int#
         |[1:33..1:34): x <= local0
         |[1:36..1:39): Int => scala/Int#
         |[1:43..1:44): x => local0
         |[1:63..1:64): x => local0
         |[2:6..2:8): pf <= _empty_/t199.pf.
         |[2:10..2:25): PartialFunction => scala/PartialFunction#
         |[2:26..2:29): Int => scala/Int#
         |[2:31..2:34): Int => scala/Int#
         |[2:40..2:82): implicit x: Int => x match { case 1 => x } <= local1
         |[2:49..2:50): x <= local8
         |[2:52..2:55): Int => scala/Int#
         |[2:59..2:60): x => local8
         |[2:79..2:80): x => local8
         |[3:6..3:7): g <= _empty_/t199.g.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:33..3:34): x <= local9
         |[3:38..3:39): x => local9
         |[3:58..3:59): x => local9
         |[4:6..4:8): pg <= _empty_/t199.pg.
         |[4:10..4:25): PartialFunction => scala/PartialFunction#
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:34): Int => scala/Int#
         |[4:40..4:77): implicit x => x match { case 1 => x } <= local10
         |[4:49..4:50): x <= local17
         |[4:54..4:55): x => local17
         |[4:74..4:75): x => local17
         |""".stripMargin
    else
      """|[0:7..0:11): t199 <= _empty_/t199.
         |[1:6..1:7): f <= _empty_/t199.f.
         |[1:9..1:12): Int => scala/Int#
         |[1:16..1:19): Int => scala/Int#
         |[1:33..1:34): x <= local0
         |[1:36..1:39): Int => scala/Int#
         |[1:43..1:44): x => local0
         |[1:63..1:64): x => local0
         |[2:6..2:8): pf <= _empty_/t199.pf.
         |[2:10..2:25): PartialFunction => scala/PartialFunction#
         |[2:26..2:29): Int => scala/Int#
         |[2:31..2:34): Int => scala/Int#
         |[2:40..2:82): implicit x: Int => x match { case 1 => x } <= local1
         |[2:49..2:50): x <= local8
         |[2:59..2:60): x => local8
         |[2:79..2:80): x => local8
         |[3:6..3:7): g <= _empty_/t199.g.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:33..3:34): x <= local9
         |[3:38..3:39): x => local9
         |[3:58..3:59): x => local9
         |[4:6..4:8): pg <= _empty_/t199.pg.
         |[4:10..4:25): PartialFunction => scala/PartialFunction#
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:34): Int => scala/Int#
         |[4:40..4:77): implicit x => x match { case 1 => x } <= local10
         |[4:49..4:50): x <= local17
         |[4:54..4:55): x => local17
         |[4:74..4:75): x => local17
         |""".stripMargin,
  )

  if (ScalaVersion.is213) synthetics(
    "partial function lambdas and their default case",
    """|object t200 {
       |  val f: Int => Int = x => x + 1
       |  val pa: PartialFunction[Int, Int] = x => x + 1
       |  val pb: PartialFunction[Int, Int] = _ + 1
       |  val pc: PartialFunction[Int, Int] = (x: Int) => x
       |  val pd: PartialFunction[Int, Int] = { implicit x: Int => x }
       |  val pe: PartialFunction[Int, Int] = { case x => x }
       |}
       |""".stripMargin,
    """|[6:38..6:53): { case x => x } => *.apply
       |  apply => scala/Function1#apply().
       |""".stripMargin,
  )

  synthetics(
    "partial function lambdas with a match and their default case",
    """|object t201 {
       |  val pa: PartialFunction[Int, Int] = x => x match { case 1 => x }
       |  val pb: PartialFunction[Int, Int] = _ match { case 1 => 2 }
       |  val pc: PartialFunction[Int, Int] = { case 1 => 2 }
       |}
       |""".stripMargin,
    """|[3:38..3:53): { case 1 => 2 } => *.apply
       |  apply => scala/Function1#apply().
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

  if (ScalaVersion.is213) synthetics(
    "eta-expansions of a partial function",
    """|object t202 {
       |  def foo(x: Int) = x
       |  val pf: PartialFunction[Int, Int] = foo _
       |}
       |""".stripMargin,
    """|[2:38..2:41): foo => *.apply
       |  apply => scala/Function1#apply().
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function lambdas in braces with statements",
    """|object t204 {
       |  val f: Int => Int = { implicit x: Int => val y = x; y }
       |  val pa: PartialFunction[Int, Int] = { x => val y = x; y }
       |  val pb: PartialFunction[Int, Int] = { x: Int => val y = x; y }
       |  val pc: PartialFunction[Int, Int] = { implicit x: Int => val y = x; y }
       |  val pd: PartialFunction[Int, Int] = { implicit x => val y = x; y }
       |  val pe: PartialFunction[Int, Int] = x => { val y = x; y }
       |}
       |""".stripMargin,
    """|_empty_/t204. => final object t204 extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t204.f. => val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t204.pa. => val method pa: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t204.pb. => val method pb: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t204.pc. => val method pc: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t204.pd. => val method pd: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t204.pe. => val method pe: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => implicit param x: Int
       |  Int => scala/Int#
       |local1 => val local y: Int
       |  Int => scala/Int#
       |local2 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local3 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local8
       |  Int => scala/Int#
       |  B1 => local7
       |  x => local5
       |  default => local6
       |  Function1 => scala/Function1#
       |local4 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local9
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic param x: A1
       |  A1 => local8
       |local6 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local8
       |  B1 => local7
       |local7 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local8 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local9 => param x: Int
       |  Int => scala/Int#
       |local10 => val local y: A1
       |  A1 => local8
       |local11 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local12 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local17
       |  Int => scala/Int#
       |  B1 => local16
       |  x => local14
       |  default => local15
       |  Function1 => scala/Function1#
       |local13 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local18
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local14 => synthetic param x: A1
       |  A1 => local17
       |local15 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local17
       |  B1 => local16
       |local16 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local17 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local18 => param x: Int
       |  Int => scala/Int#
       |local19 => val local y: A1
       |  A1 => local17
       |local20 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local21 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local26
       |  Int => scala/Int#
       |  B1 => local25
       |  x => local23
       |  default => local24
       |  Function1 => scala/Function1#
       |local22 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local27
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local23 => synthetic param x: A1
       |  A1 => local26
       |local24 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local26
       |  B1 => local25
       |local25 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local26 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local27 => implicit param x: Int
       |  Int => scala/Int#
       |local28 => val local y: A1
       |  A1 => local26
       |local29 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local30 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local35
       |  Int => scala/Int#
       |  B1 => local34
       |  x => local32
       |  default => local33
       |  Function1 => scala/Function1#
       |local31 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local36
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local32 => synthetic param x: A1
       |  A1 => local35
       |local33 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local35
       |  B1 => local34
       |local34 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local35 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local36 => implicit param x: Int
       |  Int => scala/Int#
       |local37 => val local y: A1
       |  A1 => local35
       |local38 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local39 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local44
       |  Int => scala/Int#
       |  B1 => local43
       |  x => local41
       |  default => local42
       |  Function1 => scala/Function1#
       |local40 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local45
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local41 => synthetic param x: A1
       |  A1 => local44
       |local42 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local44
       |  B1 => local43
       |local43 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local44 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local45 => param x: Int
       |  Int => scala/Int#
       |local46 => val local y: A1
       |  A1 => local44
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function lambdas in braces with statements",
    """|object t204 {
       |  val f: Int => Int = { implicit x: Int => val y = x; y }
       |  val pa: PartialFunction[Int, Int] = { x => val y = x; y }
       |  val pb: PartialFunction[Int, Int] = { x: Int => val y = x; y }
       |  val pc: PartialFunction[Int, Int] = { implicit x: Int => val y = x; y }
       |  val pd: PartialFunction[Int, Int] = { implicit x => val y = x; y }
       |  val pe: PartialFunction[Int, Int] = x => { val y = x; y }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t204 <= _empty_/t204.
         |[1:6..1:7): f <= _empty_/t204.f.
         |[1:9..1:12): Int => scala/Int#
         |[1:16..1:19): Int => scala/Int#
         |[1:33..1:34): x <= local0
         |[1:36..1:39): Int => scala/Int#
         |[1:47..1:48): y <= local1
         |[1:51..1:52): x => local0
         |[1:54..1:55): y => local1
         |[2:6..2:8): pa <= _empty_/t204.pa.
         |[2:10..2:25): PartialFunction => scala/PartialFunction#
         |[2:26..2:29): Int => scala/Int#
         |[2:31..2:34): Int => scala/Int#
         |[2:40..2:41): x <= local9
         |[2:40..2:57): x => val y = x; y <= local2
         |[2:49..2:50): y <= local10
         |[2:53..2:54): x => local9
         |[2:56..2:57): y => local10
         |[3:6..3:8): pb <= _empty_/t204.pb.
         |[3:10..3:25): PartialFunction => scala/PartialFunction#
         |[3:26..3:29): Int => scala/Int#
         |[3:31..3:34): Int => scala/Int#
         |[3:40..3:41): x <= local18
         |[3:40..3:62): x: Int => val y = x; y <= local11
         |[3:43..3:46): Int => scala/Int#
         |[3:54..3:55): y <= local19
         |[3:58..3:59): x => local18
         |[3:61..3:62): y => local19
         |[4:6..4:8): pc <= _empty_/t204.pc.
         |[4:10..4:25): PartialFunction => scala/PartialFunction#
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:34): Int => scala/Int#
         |[4:40..4:71): implicit x: Int => val y = x; y <= local20
         |[4:49..4:50): x <= local27
         |[4:52..4:55): Int => scala/Int#
         |[4:63..4:64): y <= local28
         |[4:67..4:68): x => local27
         |[4:70..4:71): y => local28
         |[5:6..5:8): pd <= _empty_/t204.pd.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:29): Int => scala/Int#
         |[5:31..5:34): Int => scala/Int#
         |[5:40..5:66): implicit x => val y = x; y <= local29
         |[5:49..5:50): x <= local36
         |[5:58..5:59): y <= local37
         |[5:62..5:63): x => local36
         |[5:65..5:66): y => local37
         |[6:6..6:8): pe <= _empty_/t204.pe.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:29): Int => scala/Int#
         |[6:31..6:34): Int => scala/Int#
         |[6:38..6:39): x <= local45
         |[6:38..6:59): x => { val y = x; y } <= local38
         |[6:49..6:50): y <= local46
         |[6:53..6:54): x => local45
         |[6:56..6:57): y => local46
         |""".stripMargin
    else
      """|[0:7..0:11): t204 <= _empty_/t204.
         |[1:6..1:7): f <= _empty_/t204.f.
         |[1:9..1:12): Int => scala/Int#
         |[1:16..1:19): Int => scala/Int#
         |[1:33..1:34): x <= local0
         |[1:36..1:39): Int => scala/Int#
         |[1:47..1:48): y <= local1
         |[1:51..1:52): x => local0
         |[1:54..1:55): y => local1
         |[2:6..2:8): pa <= _empty_/t204.pa.
         |[2:10..2:25): PartialFunction => scala/PartialFunction#
         |[2:26..2:29): Int => scala/Int#
         |[2:31..2:34): Int => scala/Int#
         |[2:40..2:41): x <= local9
         |[2:40..2:57): x => val y = x; y <= local2
         |[2:49..2:50): y <= local10
         |[2:53..2:54): x => local9
         |[2:56..2:57): y => local10
         |[3:6..3:8): pb <= _empty_/t204.pb.
         |[3:10..3:25): PartialFunction => scala/PartialFunction#
         |[3:26..3:29): Int => scala/Int#
         |[3:31..3:34): Int => scala/Int#
         |[3:40..3:41): x <= local18
         |[3:40..3:62): x: Int => val y = x; y <= local11
         |[3:54..3:55): y <= local19
         |[3:58..3:59): x => local18
         |[3:61..3:62): y => local19
         |[4:6..4:8): pc <= _empty_/t204.pc.
         |[4:10..4:25): PartialFunction => scala/PartialFunction#
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:34): Int => scala/Int#
         |[4:40..4:71): implicit x: Int => val y = x; y <= local20
         |[4:49..4:50): x <= local27
         |[4:63..4:64): y <= local28
         |[4:67..4:68): x => local27
         |[4:70..4:71): y => local28
         |[5:6..5:8): pd <= _empty_/t204.pd.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:29): Int => scala/Int#
         |[5:31..5:34): Int => scala/Int#
         |[5:40..5:66): implicit x => val y = x; y <= local29
         |[5:49..5:50): x <= local36
         |[5:58..5:59): y <= local37
         |[5:62..5:63): x => local36
         |[5:65..5:66): y => local37
         |[6:6..6:8): pe <= _empty_/t204.pe.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:29): Int => scala/Int#
         |[6:31..6:34): Int => scala/Int#
         |[6:38..6:39): x <= local45
         |[6:38..6:59): x => { val y = x; y } <= local38
         |[6:49..6:50): y <= local46
         |[6:53..6:54): x => local45
         |[6:56..6:57): y => local46
         |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
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
    """|_empty_/t205. => final object t205 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t205.pa. => val method pa: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t205.pb. => val method pb: PartialFunction[Int, Unit]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/t205.pc. => val method pc: PartialFunction[Int, Unit]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/t205.pd. => val method pd: List[Int]
       |  List => scala/collection/immutable/List#
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
       |local7 => param x: Int
       |  Int => scala/Int#
       |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Unit] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |  Serializable => java/io/Serializable#
       |local9 => final method applyOrElse[A1 <: Int, B1 >: Unit](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local14
       |  Int => scala/Int#
       |  B1 => local13
       |  Unit => scala/Unit#
       |  x => local11
       |  default => local12
       |  Function1 => scala/Function1#
       |local10 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local16
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local11 => synthetic param x: A1
       |  A1 => local14
       |local12 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local14
       |  B1 => local13
       |local13 => typeparam B1 >: Unit
       |  Unit => scala/Unit#
       |local14 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local15 => method y: A1
       |  A1 => local14
       |local16 => param x: Int
       |  Int => scala/Int#
       |local17 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Unit] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |  Serializable => java/io/Serializable#
       |local18 => final method applyOrElse[A1 <: Int, B1 >: Unit](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local23
       |  Int => scala/Int#
       |  B1 => local22
       |  Unit => scala/Unit#
       |  x => local20
       |  default => local21
       |  Function1 => scala/Function1#
       |local19 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local24
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local20 => synthetic param x: A1
       |  A1 => local23
       |local21 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local23
       |  B1 => local22
       |local22 => typeparam B1 >: Unit
       |  Unit => scala/Unit#
       |local23 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local24 => param x: Int
       |  Int => scala/Int#
       |local25 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local26 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  A1 => local25
       |  Serializable => java/io/Serializable#
       |local27 => final method applyOrElse[A1 <: Int, B1 >: A1](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local25
       |  Int => scala/Int#
       |  B1 => local31
       |  x => local29
       |  default => local30
       |  Function1 => scala/Function1#
       |local28 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local32
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local29 => synthetic param x: A1
       |  A1 => local25
       |local30 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local25
       |  B1 => local31
       |local31 => typeparam B1 >: A1
       |  A1 => local25
       |local32 => param x: Int
       |  Int => scala/Int#
       |local33 => val local y: A1
       |  A1 => local25
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
       |[1:40..1:41): x <= local7
       |[1:40..1:50): x => x + 1 <= local0
       |[1:45..1:46): x => local7
       |[1:47..1:48): + => scala/Int#`+`(+4).
       |[2:6..2:8): pb <= _empty_/t205.pb.
       |[2:10..2:25): PartialFunction => scala/PartialFunction#
       |[2:26..2:29): Int => scala/Int#
       |[2:31..2:35): Unit => scala/Unit#
       |[2:41..2:42): x <= local16
       |[2:41..2:55): x => def y = x <= local8
       |[2:50..2:51): y <= local15
       |[2:54..2:55): x => local16
       |[3:6..3:8): pc <= _empty_/t205.pc.
       |[3:10..3:25): PartialFunction => scala/PartialFunction#
       |[3:26..3:29): Int => scala/Int#
       |[3:31..3:35): Unit => scala/Unit#
       |[3:41..3:42): x <= local24
       |[3:41..3:45): x => <= local17
       |[4:6..4:8): pd <= _empty_/t205.pd.
       |[4:11..4:15): List => scala/package.List.
       |[4:19..4:26): collect => scala/collection/immutable/List#collect().
       |[4:29..4:30): x <= local32
       |[4:29..6:5): x =>
       |    val y = x
       |    y <= local26
       |[5:8..5:9): y <= local33
       |[5:12..5:13): x => local32
       |[6:4..6:5): y => local33
       |""".stripMargin,
  )

  occurrences(
    "partial function lambdas in braces with a match",
    """|object t206 {
       |  val pa: PartialFunction[Int, Int] = { x => x match { case 1 => x } }
       |  val pb: PartialFunction[Int, Int] = { x: Int =>
       |    x match { case 1 => x }
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t206 <= _empty_/t206.
         |[1:6..1:8): pa <= _empty_/t206.pa.
         |[1:10..1:25): PartialFunction => scala/PartialFunction#
         |[1:26..1:29): Int => scala/Int#
         |[1:31..1:34): Int => scala/Int#
         |[1:40..1:41): x <= local7
         |[1:40..1:68): x => x match { case 1 => x } <= local0
         |[1:45..1:46): x => local7
         |[1:65..1:66): x => local7
         |[2:6..2:8): pb <= _empty_/t206.pb.
         |[2:10..2:25): PartialFunction => scala/PartialFunction#
         |[2:26..2:29): Int => scala/Int#
         |[2:31..2:34): Int => scala/Int#
         |[2:40..2:41): x <= local15
         |[2:40..3:27): x: Int =>
         |    x match { case 1 => x } <= local8
         |[2:43..2:46): Int => scala/Int#
         |[3:4..3:5): x => local15
         |[3:24..3:25): x => local15
         |""".stripMargin
    else
      """|[0:7..0:11): t206 <= _empty_/t206.
         |[1:6..1:8): pa <= _empty_/t206.pa.
         |[1:10..1:25): PartialFunction => scala/PartialFunction#
         |[1:26..1:29): Int => scala/Int#
         |[1:31..1:34): Int => scala/Int#
         |[1:40..1:41): x <= local7
         |[1:40..1:68): x => x match { case 1 => x } <= local0
         |[1:45..1:46): x => local7
         |[1:65..1:66): x => local7
         |[2:6..2:8): pb <= _empty_/t206.pb.
         |[2:10..2:25): PartialFunction => scala/PartialFunction#
         |[2:26..2:29): Int => scala/Int#
         |[2:31..2:34): Int => scala/Int#
         |[2:40..2:41): x <= local15
         |[2:40..3:27): x: Int =>
         |    x match { case 1 => x } <= local8
         |[3:4..3:5): x => local15
         |[3:24..3:25): x => local15
         |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "partial function lambdas in braces with typed or implicit parameters",
    """|object t207 {
       |  val pa: PartialFunction[Int, Unit] = { x: Int => }
       |  val pb: PartialFunction[Int, Unit] = { implicit x: Int => }
       |  val pc: PartialFunction[Int, Unit] = { implicit x => }
       |  val pd: PartialFunction[Int, Unit] = { x: Int => def y = x }
       |  val pe: PartialFunction[Int, Int] = { x : Int => val y = x; y }
       |  val pf: PartialFunction[Int, Int] = { x /* c */: Int => val y = x; y }
       |}
       |""".stripMargin,
    """|_empty_/t207. => final object t207 extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t207.pa. => val method pa: PartialFunction[Int, Unit]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/t207.pb. => val method pb: PartialFunction[Int, Unit]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/t207.pc. => val method pc: PartialFunction[Int, Unit]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/t207.pd. => val method pd: PartialFunction[Int, Unit]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/t207.pe. => val method pe: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |_empty_/t207.pf. => val method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Unit] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |  Serializable => java/io/Serializable#
       |local1 => final method applyOrElse[A1 <: Int, B1 >: Unit](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local6
       |  Int => scala/Int#
       |  B1 => local5
       |  Unit => scala/Unit#
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
       |local5 => typeparam B1 >: Unit
       |  Unit => scala/Unit#
       |local6 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local7 => param x: Int
       |  Int => scala/Int#
       |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Unit] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |  Serializable => java/io/Serializable#
       |local9 => final method applyOrElse[A1 <: Int, B1 >: Unit](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local14
       |  Int => scala/Int#
       |  B1 => local13
       |  Unit => scala/Unit#
       |  x => local11
       |  default => local12
       |  Function1 => scala/Function1#
       |local10 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local15
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local11 => synthetic param x: A1
       |  A1 => local14
       |local12 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local14
       |  B1 => local13
       |local13 => typeparam B1 >: Unit
       |  Unit => scala/Unit#
       |local14 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local15 => implicit param x: Int
       |  Int => scala/Int#
       |local16 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Unit] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |  Serializable => java/io/Serializable#
       |local17 => final method applyOrElse[A1 <: Int, B1 >: Unit](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local22
       |  Int => scala/Int#
       |  B1 => local21
       |  Unit => scala/Unit#
       |  x => local19
       |  default => local20
       |  Function1 => scala/Function1#
       |local18 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local23
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local19 => synthetic param x: A1
       |  A1 => local22
       |local20 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local22
       |  B1 => local21
       |local21 => typeparam B1 >: Unit
       |  Unit => scala/Unit#
       |local22 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local23 => implicit param x: Int
       |  Int => scala/Int#
       |local24 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Unit] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |  Serializable => java/io/Serializable#
       |local25 => final method applyOrElse[A1 <: Int, B1 >: Unit](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local30
       |  Int => scala/Int#
       |  B1 => local29
       |  Unit => scala/Unit#
       |  x => local27
       |  default => local28
       |  Function1 => scala/Function1#
       |local26 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local32
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local27 => synthetic param x: A1
       |  A1 => local30
       |local28 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local30
       |  B1 => local29
       |local29 => typeparam B1 >: Unit
       |  Unit => scala/Unit#
       |local30 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local31 => method y: A1
       |  A1 => local30
       |local32 => param x: Int
       |  Int => scala/Int#
       |local33 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local34 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local39
       |  Int => scala/Int#
       |  B1 => local38
       |  x => local36
       |  default => local37
       |  Function1 => scala/Function1#
       |local35 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local40
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local36 => synthetic param x: A1
       |  A1 => local39
       |local37 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local39
       |  B1 => local38
       |local38 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local39 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local40 => param x: Int
       |  Int => scala/Int#
       |local41 => val local y: A1
       |  A1 => local39
       |local42 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local43 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local48
       |  Int => scala/Int#
       |  B1 => local47
       |  x => local45
       |  default => local46
       |  Function1 => scala/Function1#
       |local44 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local49
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local45 => synthetic param x: A1
       |  A1 => local48
       |local46 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local48
       |  B1 => local47
       |local47 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local48 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local49 => param x: Int
       |  Int => scala/Int#
       |local50 => val local y: A1
       |  A1 => local48
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "partial function lambdas in braces with typed or implicit parameters",
    """|object t207 {
       |  val pa: PartialFunction[Int, Unit] = { x: Int => }
       |  val pb: PartialFunction[Int, Unit] = { implicit x: Int => }
       |  val pc: PartialFunction[Int, Unit] = { implicit x => }
       |  val pd: PartialFunction[Int, Unit] = { x: Int => def y = x }
       |  val pe: PartialFunction[Int, Int] = { x : Int => val y = x; y }
       |  val pf: PartialFunction[Int, Int] = { x /* c */: Int => val y = x; y }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t207 <= _empty_/t207.
         |[1:6..1:8): pa <= _empty_/t207.pa.
         |[1:10..1:25): PartialFunction => scala/PartialFunction#
         |[1:26..1:29): Int => scala/Int#
         |[1:31..1:35): Unit => scala/Unit#
         |[1:41..1:42): x <= local7
         |[1:41..1:50): x: Int => <= local0
         |[1:44..1:47): Int => scala/Int#
         |[2:6..2:8): pb <= _empty_/t207.pb.
         |[2:10..2:25): PartialFunction => scala/PartialFunction#
         |[2:26..2:29): Int => scala/Int#
         |[2:31..2:35): Unit => scala/Unit#
         |[2:41..2:59): implicit x: Int => <= local8
         |[2:50..2:51): x <= local15
         |[2:53..2:56): Int => scala/Int#
         |[3:6..3:8): pc <= _empty_/t207.pc.
         |[3:10..3:25): PartialFunction => scala/PartialFunction#
         |[3:26..3:29): Int => scala/Int#
         |[3:31..3:35): Unit => scala/Unit#
         |[3:41..3:54): implicit x => <= local16
         |[3:50..3:51): x <= local23
         |[4:6..4:8): pd <= _empty_/t207.pd.
         |[4:10..4:25): PartialFunction => scala/PartialFunction#
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:35): Unit => scala/Unit#
         |[4:41..4:42): x <= local32
         |[4:41..4:60): x: Int => def y = x <= local24
         |[4:44..4:47): Int => scala/Int#
         |[4:55..4:56): y <= local31
         |[4:59..4:60): x => local32
         |[5:6..5:8): pe <= _empty_/t207.pe.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:29): Int => scala/Int#
         |[5:31..5:34): Int => scala/Int#
         |[5:40..5:41): x <= local40
         |[5:40..5:63): x : Int => val y = x; y <= local33
         |[5:44..5:47): Int => scala/Int#
         |[5:55..5:56): y <= local41
         |[5:59..5:60): x => local40
         |[5:62..5:63): y => local41
         |[6:6..6:8): pf <= _empty_/t207.pf.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:29): Int => scala/Int#
         |[6:31..6:34): Int => scala/Int#
         |[6:40..6:41): x <= local49
         |[6:40..6:70): x /* c */: Int => val y = x; y <= local42
         |[6:51..6:54): Int => scala/Int#
         |[6:62..6:63): y <= local50
         |[6:66..6:67): x => local49
         |[6:69..6:70): y => local50
         |""".stripMargin
    else
      """|[0:7..0:11): t207 <= _empty_/t207.
         |[1:6..1:8): pa <= _empty_/t207.pa.
         |[1:10..1:25): PartialFunction => scala/PartialFunction#
         |[1:26..1:29): Int => scala/Int#
         |[1:31..1:35): Unit => scala/Unit#
         |[1:41..1:42): x <= local7
         |[1:41..1:50): x: Int => <= local0
         |[2:6..2:8): pb <= _empty_/t207.pb.
         |[2:10..2:25): PartialFunction => scala/PartialFunction#
         |[2:26..2:29): Int => scala/Int#
         |[2:31..2:35): Unit => scala/Unit#
         |[2:41..2:59): implicit x: Int => <= local8
         |[2:50..2:51): x <= local15
         |[3:6..3:8): pc <= _empty_/t207.pc.
         |[3:10..3:25): PartialFunction => scala/PartialFunction#
         |[3:26..3:29): Int => scala/Int#
         |[3:31..3:35): Unit => scala/Unit#
         |[3:41..3:54): implicit x => <= local16
         |[3:50..3:51): x <= local23
         |[4:6..4:8): pd <= _empty_/t207.pd.
         |[4:10..4:25): PartialFunction => scala/PartialFunction#
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:35): Unit => scala/Unit#
         |[4:41..4:42): x <= local32
         |[4:41..4:60): x: Int => def y = x <= local24
         |[4:55..4:56): y <= local31
         |[4:59..4:60): x => local32
         |[5:6..5:8): pe <= _empty_/t207.pe.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:29): Int => scala/Int#
         |[5:31..5:34): Int => scala/Int#
         |[5:40..5:41): x <= local40
         |[5:40..5:63): x : Int => val y = x; y <= local33
         |[5:55..5:56): y <= local41
         |[5:59..5:60): x => local40
         |[5:62..5:63): y => local41
         |[6:6..6:8): pf <= _empty_/t207.pf.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:29): Int => scala/Int#
         |[6:31..6:34): Int => scala/Int#
         |[6:40..6:41): x <= local49
         |[6:40..6:70): x /* c */: Int => val y = x; y <= local42
         |[6:62..6:63): y <= local50
         |[6:66..6:67): x => local49
         |[6:69..6:70): y => local50
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
       |[3:15..3:16): x <= local7
       |[3:20..3:21): c => _empty_/t208.c.
       |[3:29..3:30): x => local7
       |[3:31..3:31):  <= local0
       |[3:31..3:32): + => scala/Int#`+`(+4).
       |[4:6..4:7): b <= _empty_/t208.b.
       |[4:9..4:12): Int => scala/Int#
       |[4:16..4:20): List => scala/package.List#
       |[4:21..4:24): Int => scala/Int#
       |[4:28..4:29): _ <= local8
       |[4:30..4:32): :: => scala/collection/immutable/List#`::`().
       |[4:39..4:40): x <= local16
       |[4:44..4:45): c => _empty_/t208.c.
       |[4:53..4:54): x => local16
       |[4:55..4:55):  <= local9
       |[4:55..4:56): + => scala/Int#`+`(+4).
       |[5:6..5:7): d <= _empty_/t208.d.
       |[5:9..5:12): Int => scala/Int#
       |[5:16..5:20): List => scala/package.List#
       |[5:21..5:24): Int => scala/Int#
       |[5:28..5:29): y <= local17
       |[5:33..5:34): y => local17
       |[5:35..5:37): :: => scala/collection/immutable/List#`::`().
       |[5:44..5:45): x <= local25
       |[5:49..5:50): c => _empty_/t208.c.
       |[5:58..5:59): x => local25
       |[5:60..5:60):  <= local18
       |[5:60..5:61): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  occurrences(
    "a for comprehension with a match over a map of a partial function",
    """|object t209 {
       |  class C { def map(f: PartialFunction[Int, Int]): List[Int] = ??? }
       |  val c = new C
       |  val a = for (x <- c) yield x match { case z => z }
       |  val b: Int => List[Int] = _ :: (for (x <- c) yield x match { case z => z })
       |  val d: Int => List[Int] = y => y :: (for (x <- c) yield x match { case z => z })
       |}
       |""".stripMargin,
    """|[0:7..0:11): t209 <= _empty_/t209.
       |[1:8..1:9): C <= _empty_/t209.C#
       |[1:10..1:10):  <= _empty_/t209.C#`<init>`().
       |[1:16..1:19): map <= _empty_/t209.C#map().
       |[1:20..1:21): f <= _empty_/t209.C#map().(f)
       |[1:23..1:38): PartialFunction => scala/PartialFunction#
       |[1:39..1:42): Int => scala/Int#
       |[1:44..1:47): Int => scala/Int#
       |[1:51..1:55): List => scala/package.List#
       |[1:56..1:59): Int => scala/Int#
       |[1:63..1:66): ??? => scala/Predef.`???`().
       |[2:6..2:7): c <= _empty_/t209.c.
       |[2:14..2:15): C => _empty_/t209.C#
       |[2:15..2:15):  => _empty_/t209.C#`<init>`().
       |[3:6..3:7): a <= _empty_/t209.a.
       |[3:15..3:16): x <= local7
       |[3:20..3:21): c => _empty_/t209.c.
       |[3:29..3:30): x => local7
       |[3:31..3:31):  <= local0
       |[3:44..3:45): z <= local8
       |[3:49..3:50): z => local8
       |[4:6..4:7): b <= _empty_/t209.b.
       |[4:9..4:12): Int => scala/Int#
       |[4:16..4:20): List => scala/package.List#
       |[4:21..4:24): Int => scala/Int#
       |[4:28..4:29): _ <= local9
       |[4:30..4:32): :: => scala/collection/immutable/List#`::`().
       |[4:39..4:40): x <= local17
       |[4:44..4:45): c => _empty_/t209.c.
       |[4:53..4:54): x => local17
       |[4:55..4:55):  <= local10
       |[4:68..4:69): z <= local18
       |[4:73..4:74): z => local18
       |[5:6..5:7): d <= _empty_/t209.d.
       |[5:9..5:12): Int => scala/Int#
       |[5:16..5:20): List => scala/package.List#
       |[5:21..5:24): Int => scala/Int#
       |[5:28..5:29): y <= local19
       |[5:33..5:34): y => local19
       |[5:35..5:37): :: => scala/collection/immutable/List#`::`().
       |[5:44..5:45): x <= local27
       |[5:49..5:50): c => _empty_/t209.c.
       |[5:58..5:59): x => local27
       |[5:60..5:60):  <= local20
       |[5:73..5:74): z <= local28
       |[5:78..5:79): z => local28
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "a for comprehension over a map of a partial function in a lambda's parameter type",
    """|object t210 {
       |  class ann(a: Any) extends scala.annotation.StaticAnnotation
       |  class C { def map(f: PartialFunction[Int, Int]): List[Int] = ??? }
       |  val c = new C
       |  val f: Int => Int = (x: Int @ann(for (y <- c) yield y + 1)) => x
       |  val g: PartialFunction[Int, Int] = (x: Int @ann(for (y <- c) yield y + 1)) => x
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|_empty_/t210. => final object t210 extends AnyRef { +5 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t210.C# => class C extends AnyRef { +2 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t210.C#`<init>`(). => primary ctor <init>()
             |_empty_/t210.C#map(). => method map(f: PartialFunction[Int, Int]): List[Int]
             |  f => _empty_/t210.C#map().(f)
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |  List => scala/package.List#
             |_empty_/t210.C#map().(f) => param f: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t210.ann# => class ann extends Annotation with StaticAnnotation { +2 decls }
             |  Annotation => scala/annotation/Annotation#
             |  StaticAnnotation => scala/annotation/StaticAnnotation#
             |_empty_/t210.ann#`<init>`(). => primary ctor <init>(a: Any)
             |  a => _empty_/t210.ann#`<init>`().(a)
             |  Any => scala/Any#
             |_empty_/t210.ann#`<init>`().(a) => param a: Any
             |  Any => scala/Any#
             |_empty_/t210.ann#a. => private[this] val method a: Any
             |  Any => scala/Any#
             |_empty_/t210.c. => val method c: C
             |  C => _empty_/t210.C#
             |_empty_/t210.f. => val method f: Function1[Int, Int]
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |_empty_/t210.g. => val method g: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |local0 => param x: Int @ann
             |  Int => scala/Int#
             |  ann => _empty_/t210.ann#
             |local1 => param y: Int
             |  Int => scala/Int#
             |local2 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local3 => final method applyOrElse[A1 <: Int, B1 >: Int](y: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local8
             |  Int => scala/Int#
             |  B1 => local7
             |  y => local5
             |  default => local6
             |  Function1 => scala/Function1#
             |local4 => final method isDefinedAt(y: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  y => local1
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local5 => synthetic param y: A1
             |  A1 => local8
             |local6 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local8
             |  B1 => local7
             |local7 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local8 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local15
             |  Int => scala/Int#
             |  B1 => local14
             |  x => local12
             |  default => local13
             |  Function1 => scala/Function1#
             |local11 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local16
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local12 => synthetic param x: A1
             |  A1 => local15
             |local13 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local15
             |  B1 => local14
             |local14 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local15 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local16 => param x: Int
             |  Int => scala/Int#
             |""".stripMargin
      },
      """|_empty_/t210. => final object t210 extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t210.C# => class C extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t210.C#`<init>`(). => primary ctor <init>()
         |_empty_/t210.C#map(). => method map(f: PartialFunction[Int, Int]): List[Int]
         |  f => _empty_/t210.C#map().(f)
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |_empty_/t210.C#map().(f) => param f: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t210.ann# => class ann extends Annotation with StaticAnnotation { +2 decls }
         |  Annotation => scala/annotation/Annotation#
         |  StaticAnnotation => scala/annotation/StaticAnnotation#
         |_empty_/t210.ann#`<init>`(). => primary ctor <init>(a: Any)
         |  a => _empty_/t210.ann#`<init>`().(a)
         |  Any => scala/Any#
         |_empty_/t210.ann#`<init>`().(a) => param a: Any
         |  Any => scala/Any#
         |_empty_/t210.ann#a. => private[this] val method a: Any
         |  Any => scala/Any#
         |_empty_/t210.c. => val method c: C
         |  C => _empty_/t210.C#
         |_empty_/t210.f. => val method f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t210.g. => val method g: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => param x: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t210.ann#
         |local1 => param y: Int
         |  Int => scala/Int#
         |local2 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local3 => final method applyOrElse[A1 <: Int, B1 >: Int](y: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local8
         |  Int => scala/Int#
         |  B1 => local7
         |  y => local5
         |  default => local6
         |  Function1 => scala/Function1#
         |local4 => final method isDefinedAt(y: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  y => local1
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local5 => synthetic param y: A1
         |  A1 => local8
         |local6 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local8
         |  B1 => local7
         |local7 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local8 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int @ann, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  ann => _empty_/t210.ann#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: Int @ann, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  ann => _empty_/t210.ann#
         |  B1 => local14
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: Int @ann): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local24
         |  Int => scala/Int#
         |  ann => _empty_/t210.ann#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t210.ann#
         |local16 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local17 => final method applyOrElse[A1 <: Int, B1 >: Int](y: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local22
         |  Int => scala/Int#
         |  B1 => local21
         |  y => local19
         |  default => local20
         |  Function1 => scala/Function1#
         |local18 => final method isDefinedAt(y: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  y => local23
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local19 => synthetic param y: A1
         |  A1 => local22
         |local20 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local22
         |  B1 => local21
         |local21 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local22 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local23 => param y: Int
         |  Int => scala/Int#
         |local24 => param x: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t210.ann#
         |""".stripMargin,
    ),
  )

  if (ScalaVersion.is213) occurrences(
    "a for comprehension over a map of a partial function in a lambda's parameter type",
    """|object t210 {
       |  class ann(a: Any) extends scala.annotation.StaticAnnotation
       |  class C { def map(f: PartialFunction[Int, Int]): List[Int] = ??? }
       |  val c = new C
       |  val f: Int => Int = (x: Int @ann(for (y <- c) yield y + 1)) => x
       |  val g: PartialFunction[Int, Int] = (x: Int @ann(for (y <- c) yield y + 1)) => x
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|[0:7..0:11): t210 <= _empty_/t210.
             |[1:8..1:11): ann <= _empty_/t210.ann#
             |[1:11..1:11):  <= _empty_/t210.ann#`<init>`().
             |[1:12..1:13): a <= _empty_/t210.ann#`<init>`().(a)
             |[1:12..1:13): a <= _empty_/t210.ann#a.
             |[1:15..1:18): Any => scala/Any#
             |[1:28..1:33): scala => scala/
             |[1:34..1:44): annotation => scala/annotation/
             |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
             |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
             |[2:8..2:9): C <= _empty_/t210.C#
             |[2:10..2:10):  <= _empty_/t210.C#`<init>`().
             |[2:16..2:19): map <= _empty_/t210.C#map().
             |[2:20..2:21): f <= _empty_/t210.C#map().(f)
             |[2:23..2:38): PartialFunction => scala/PartialFunction#
             |[2:39..2:42): Int => scala/Int#
             |[2:44..2:47): Int => scala/Int#
             |[2:51..2:55): List => scala/package.List#
             |[2:56..2:59): Int => scala/Int#
             |[2:63..2:66): ??? => scala/Predef.`???`().
             |[3:6..3:7): c <= _empty_/t210.c.
             |[3:14..3:15): C => _empty_/t210.C#
             |[3:15..3:15):  => _empty_/t210.C#`<init>`().
             |[4:6..4:7): f <= _empty_/t210.f.
             |[4:9..4:12): Int => scala/Int#
             |[4:16..4:19): Int => scala/Int#
             |[4:23..4:24): x <= local0
             |[4:26..4:29): Int => scala/Int#
             |[4:31..4:34): ann => _empty_/t210.ann#
             |[4:34..4:34):  => _empty_/t210.ann#`<init>`().
             |[4:40..4:41): y <= local1
             |[4:45..4:46): c => _empty_/t210.c.
             |[4:54..4:55): y => local1
             |[4:56..4:56):  <= local2
             |[4:56..4:57): + => scala/Int#`+`(+4).
             |[4:65..4:66): x => local0
             |[5:6..5:7): g <= _empty_/t210.g.
             |[5:9..5:24): PartialFunction => scala/PartialFunction#
             |[5:25..5:28): Int => scala/Int#
             |[5:30..5:33): Int => scala/Int#
             |[5:37..5:81): (x: Int @ann(for (y <- c) yield y + 1)) => x <= local9
             |[5:38..5:39): x <= local16
             |[5:80..5:81): x => local16
             |""".stripMargin
      },
      """|[0:7..0:11): t210 <= _empty_/t210.
         |[1:8..1:11): ann <= _empty_/t210.ann#
         |[1:11..1:11):  <= _empty_/t210.ann#`<init>`().
         |[1:12..1:13): a <= _empty_/t210.ann#`<init>`().(a)
         |[1:12..1:13): a <= _empty_/t210.ann#a.
         |[1:15..1:18): Any => scala/Any#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[2:8..2:9): C <= _empty_/t210.C#
         |[2:10..2:10):  <= _empty_/t210.C#`<init>`().
         |[2:16..2:19): map <= _empty_/t210.C#map().
         |[2:20..2:21): f <= _empty_/t210.C#map().(f)
         |[2:23..2:38): PartialFunction => scala/PartialFunction#
         |[2:39..2:42): Int => scala/Int#
         |[2:44..2:47): Int => scala/Int#
         |[2:51..2:55): List => scala/package.List#
         |[2:56..2:59): Int => scala/Int#
         |[2:63..2:66): ??? => scala/Predef.`???`().
         |[3:6..3:7): c <= _empty_/t210.c.
         |[3:14..3:15): C => _empty_/t210.C#
         |[3:15..3:15):  => _empty_/t210.C#`<init>`().
         |[4:6..4:7): f <= _empty_/t210.f.
         |[4:9..4:12): Int => scala/Int#
         |[4:16..4:19): Int => scala/Int#
         |[4:23..4:24): x <= local0
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:34): ann => _empty_/t210.ann#
         |[4:34..4:34):  => _empty_/t210.ann#`<init>`().
         |[4:40..4:41): y <= local1
         |[4:45..4:46): c => _empty_/t210.c.
         |[4:54..4:55): y => local1
         |[4:56..4:56):  <= local2
         |[4:56..4:57): + => scala/Int#`+`(+4).
         |[4:65..4:66): x => local0
         |[5:6..5:7): g <= _empty_/t210.g.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[5:37..5:81): (x: Int @ann(for (y <- c) yield y + 1)) => x <= local9
         |[5:38..5:39): x <= local24
         |[5:41..5:44): Int => scala/Int#
         |[5:46..5:49): ann => _empty_/t210.ann#
         |[5:49..5:49):  => _empty_/t210.ann#`<init>`().
         |[5:55..5:56): y <= local23
         |[5:60..5:61): c => _empty_/t210.c.
         |[5:69..5:70): y => local23
         |[5:71..5:71):  <= local16
         |[5:71..5:72): + => scala/Int#`+`(+4).
         |[5:80..5:81): x => local24
         |""".stripMargin,
    ),
  )

  occurrences(
    "a for comprehension with a match over a map of a partial function in a lambda's parameter type",
    """|object t211 {
       |  class ann(a: Any) extends scala.annotation.StaticAnnotation
       |  class C { def map(f: PartialFunction[Int, Int]): List[Int] = ??? }
       |  val c = new C
       |  val f: Int => Int = (x: Int @ann(for (y <- c) yield y match { case z => z })) => x
       |  val g: PartialFunction[Int, Int] =
       |    (x: Int @ann(for (y <- c) yield y match { case z => z })) => x match { case w => w }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): t211 <= _empty_/t211.
         |[1:8..1:11): ann <= _empty_/t211.ann#
         |[1:11..1:11):  <= _empty_/t211.ann#`<init>`().
         |[1:12..1:13): a <= _empty_/t211.ann#`<init>`().(a)
         |[1:12..1:13): a <= _empty_/t211.ann#a.
         |[1:15..1:18): Any => scala/Any#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[2:8..2:9): C <= _empty_/t211.C#
         |[2:10..2:10):  <= _empty_/t211.C#`<init>`().
         |[2:16..2:19): map <= _empty_/t211.C#map().
         |[2:20..2:21): f <= _empty_/t211.C#map().(f)
         |[2:23..2:38): PartialFunction => scala/PartialFunction#
         |[2:39..2:42): Int => scala/Int#
         |[2:44..2:47): Int => scala/Int#
         |[2:51..2:55): List => scala/package.List#
         |[2:56..2:59): Int => scala/Int#
         |[2:63..2:66): ??? => scala/Predef.`???`().
         |[3:6..3:7): c <= _empty_/t211.c.
         |[3:14..3:15): C => _empty_/t211.C#
         |[3:15..3:15):  => _empty_/t211.C#`<init>`().
         |[4:6..4:7): f <= _empty_/t211.f.
         |[4:9..4:12): Int => scala/Int#
         |[4:16..4:19): Int => scala/Int#
         |[4:23..4:24): x <= local0
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:34): ann => _empty_/t211.ann#
         |[4:34..4:34):  => _empty_/t211.ann#`<init>`().
         |[4:40..4:41): y <= local9
         |[4:45..4:46): c => _empty_/t211.c.
         |[4:54..4:55): y => local9
         |[4:56..4:56):  <= local3
         |[4:69..4:70): z <= local1
         |[4:74..4:75): z => local1
         |[4:83..4:84): x => local0
         |[5:6..5:7): g <= _empty_/t211.g.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[6:4..6:88): (x: Int @ann(for (y <- c) yield y match { case z => z })) => x match { case w => w } <= local10
         |[6:5..6:6): x <= local17
         |[6:65..6:66): x => local17
         |[6:80..6:81): w <= local18
         |[6:85..6:86): w => local18
         |""".stripMargin
    else ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|[0:7..0:11): t211 <= _empty_/t211.
             |[1:8..1:11): ann <= _empty_/t211.ann#
             |[1:11..1:11):  <= _empty_/t211.ann#`<init>`().
             |[1:12..1:13): a <= _empty_/t211.ann#`<init>`().(a)
             |[1:12..1:13): a <= _empty_/t211.ann#a.
             |[1:15..1:18): Any => scala/Any#
             |[1:28..1:33): scala => scala/
             |[1:34..1:44): annotation => scala/annotation/
             |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
             |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
             |[2:8..2:9): C <= _empty_/t211.C#
             |[2:10..2:10):  <= _empty_/t211.C#`<init>`().
             |[2:16..2:19): map <= _empty_/t211.C#map().
             |[2:20..2:21): f <= _empty_/t211.C#map().(f)
             |[2:23..2:38): PartialFunction => scala/PartialFunction#
             |[2:39..2:42): Int => scala/Int#
             |[2:44..2:47): Int => scala/Int#
             |[2:51..2:55): List => scala/package.List#
             |[2:56..2:59): Int => scala/Int#
             |[2:63..2:66): ??? => scala/Predef.`???`().
             |[3:6..3:7): c <= _empty_/t211.c.
             |[3:14..3:15): C => _empty_/t211.C#
             |[3:15..3:15):  => _empty_/t211.C#`<init>`().
             |[4:6..4:7): f <= _empty_/t211.f.
             |[4:9..4:12): Int => scala/Int#
             |[4:16..4:19): Int => scala/Int#
             |[4:23..4:24): x <= local0
             |[4:26..4:29): Int => scala/Int#
             |[4:31..4:34): ann => _empty_/t211.ann#
             |[4:34..4:34):  => _empty_/t211.ann#`<init>`().
             |[4:40..4:41): y <= local9
             |[4:45..4:46): c => _empty_/t211.c.
             |[4:54..4:55): y => local9
             |[4:56..4:56):  <= local3
             |[4:69..4:70): z <= local1
             |[4:74..4:75): z => local1
             |[4:83..4:84): x => local0
             |[5:6..5:7): g <= _empty_/t211.g.
             |[5:9..5:24): PartialFunction => scala/PartialFunction#
             |[5:25..5:28): Int => scala/Int#
             |[5:30..5:33): Int => scala/Int#
             |[6:4..6:88): (x: Int @ann(for (y <- c) yield y match { case z => z })) => x match { case w => w } <= local10
             |[6:5..6:6): x <= local17
             |[6:65..6:66): x => local17
             |[6:80..6:81): w <= local18
             |[6:85..6:86): w => local18
             |""".stripMargin
      },
      """|[0:7..0:11): t211 <= _empty_/t211.
         |[1:8..1:11): ann <= _empty_/t211.ann#
         |[1:11..1:11):  <= _empty_/t211.ann#`<init>`().
         |[1:12..1:13): a <= _empty_/t211.ann#`<init>`().(a)
         |[1:12..1:13): a <= _empty_/t211.ann#a.
         |[1:15..1:18): Any => scala/Any#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[2:8..2:9): C <= _empty_/t211.C#
         |[2:10..2:10):  <= _empty_/t211.C#`<init>`().
         |[2:16..2:19): map <= _empty_/t211.C#map().
         |[2:20..2:21): f <= _empty_/t211.C#map().(f)
         |[2:23..2:38): PartialFunction => scala/PartialFunction#
         |[2:39..2:42): Int => scala/Int#
         |[2:44..2:47): Int => scala/Int#
         |[2:51..2:55): List => scala/package.List#
         |[2:56..2:59): Int => scala/Int#
         |[2:63..2:66): ??? => scala/Predef.`???`().
         |[3:6..3:7): c <= _empty_/t211.c.
         |[3:14..3:15): C => _empty_/t211.C#
         |[3:15..3:15):  => _empty_/t211.C#`<init>`().
         |[4:6..4:7): f <= _empty_/t211.f.
         |[4:9..4:12): Int => scala/Int#
         |[4:16..4:19): Int => scala/Int#
         |[4:23..4:24): x <= local0
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:34): ann => _empty_/t211.ann#
         |[4:34..4:34):  => _empty_/t211.ann#`<init>`().
         |[4:40..4:41): y <= local9
         |[4:45..4:46): c => _empty_/t211.c.
         |[4:54..4:55): y => local9
         |[4:56..4:56):  <= local3
         |[4:69..4:70): z <= local1
         |[4:74..4:75): z => local1
         |[4:83..4:84): x => local0
         |[5:6..5:7): g <= _empty_/t211.g.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[6:4..6:88): (x: Int @ann(for (y <- c) yield y match { case z => z })) => x match { case w => w } <= local10
         |[6:5..6:6): x <= local17
         |[6:8..6:11): Int => scala/Int#
         |[6:13..6:16): ann => _empty_/t211.ann#
         |[6:16..6:16):  => _empty_/t211.ann#`<init>`().
         |[6:22..6:23): y <= local26
         |[6:27..6:28): c => _empty_/t211.c.
         |[6:36..6:37): y => local26
         |[6:38..6:38):  <= local19
         |[6:51..6:52): z <= local27
         |[6:56..6:57): z => local27
         |[6:65..6:66): x => local17
         |[6:80..6:81): w <= local18
         |[6:85..6:86): w => local18
         |""".stripMargin,
    ),
  )

  occurrences(
    "a lambda and a placeholder over a map of a partial function in a lambda's parameter type",
    """|object t212 {
       |  class ann(a: Any) extends scala.annotation.StaticAnnotation
       |  class C { def map(f: PartialFunction[Int, Int]): List[Int] = ??? }
       |  val c = new C
       |  val f: Int => Int = (x: Int @ann(c.map(y => y match { case z => z }))) => x
       |  val g: PartialFunction[Int, Int] =
       |    (x: Int @ann(c.map(y => y match { case z => z }))) => x match { case w => w }
       |  val h: PartialFunction[Int, Int] =
       |    (x: Int @ann(c.map(_ match { case z => z }))) => x match { case w => w }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): t212 <= _empty_/t212.
         |[1:8..1:11): ann <= _empty_/t212.ann#
         |[1:11..1:11):  <= _empty_/t212.ann#`<init>`().
         |[1:12..1:13): a <= _empty_/t212.ann#`<init>`().(a)
         |[1:12..1:13): a <= _empty_/t212.ann#a.
         |[1:15..1:18): Any => scala/Any#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[2:8..2:9): C <= _empty_/t212.C#
         |[2:10..2:10):  <= _empty_/t212.C#`<init>`().
         |[2:16..2:19): map <= _empty_/t212.C#map().
         |[2:20..2:21): f <= _empty_/t212.C#map().(f)
         |[2:23..2:38): PartialFunction => scala/PartialFunction#
         |[2:39..2:42): Int => scala/Int#
         |[2:44..2:47): Int => scala/Int#
         |[2:51..2:55): List => scala/package.List#
         |[2:56..2:59): Int => scala/Int#
         |[2:63..2:66): ??? => scala/Predef.`???`().
         |[3:6..3:7): c <= _empty_/t212.c.
         |[3:14..3:15): C => _empty_/t212.C#
         |[3:15..3:15):  => _empty_/t212.C#`<init>`().
         |[4:6..4:7): f <= _empty_/t212.f.
         |[4:9..4:12): Int => scala/Int#
         |[4:16..4:19): Int => scala/Int#
         |[4:23..4:24): x <= local0
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:34): ann => _empty_/t212.ann#
         |[4:34..4:34):  => _empty_/t212.ann#`<init>`().
         |[4:35..4:36): c => _empty_/t212.c.
         |[4:37..4:40): map => _empty_/t212.C#map().
         |[4:41..4:42): y <= local9
         |[4:41..4:69): y => y match { case z => z } <= local3
         |[4:46..4:47): y => local9
         |[4:61..4:62): z <= local1
         |[4:66..4:67): z => local1
         |[4:76..4:77): x => local0
         |[5:6..5:7): g <= _empty_/t212.g.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[6:4..6:81): (x: Int @ann(c.map(y => y match { case z => z }))) => x match { case w => w } <= local10
         |[6:5..6:6): x <= local17
         |[6:58..6:59): x => local17
         |[6:73..6:74): w <= local18
         |[6:78..6:79): w => local18
         |[7:6..7:7): h <= _empty_/t212.h.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:28): Int => scala/Int#
         |[7:30..7:33): Int => scala/Int#
         |[8:4..8:76): (x: Int @ann(c.map(_ match { case z => z }))) => x match { case w => w } <= local19
         |[8:5..8:6): x <= local26
         |[8:53..8:54): x => local26
         |[8:68..8:69): w <= local27
         |[8:73..8:74): w => local27
         |""".stripMargin
    else ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|[0:7..0:11): t212 <= _empty_/t212.
             |[1:8..1:11): ann <= _empty_/t212.ann#
             |[1:11..1:11):  <= _empty_/t212.ann#`<init>`().
             |[1:12..1:13): a <= _empty_/t212.ann#`<init>`().(a)
             |[1:12..1:13): a <= _empty_/t212.ann#a.
             |[1:15..1:18): Any => scala/Any#
             |[1:28..1:33): scala => scala/
             |[1:34..1:44): annotation => scala/annotation/
             |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
             |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
             |[2:8..2:9): C <= _empty_/t212.C#
             |[2:10..2:10):  <= _empty_/t212.C#`<init>`().
             |[2:16..2:19): map <= _empty_/t212.C#map().
             |[2:20..2:21): f <= _empty_/t212.C#map().(f)
             |[2:23..2:38): PartialFunction => scala/PartialFunction#
             |[2:39..2:42): Int => scala/Int#
             |[2:44..2:47): Int => scala/Int#
             |[2:51..2:55): List => scala/package.List#
             |[2:56..2:59): Int => scala/Int#
             |[2:63..2:66): ??? => scala/Predef.`???`().
             |[3:6..3:7): c <= _empty_/t212.c.
             |[3:14..3:15): C => _empty_/t212.C#
             |[3:15..3:15):  => _empty_/t212.C#`<init>`().
             |[4:6..4:7): f <= _empty_/t212.f.
             |[4:9..4:12): Int => scala/Int#
             |[4:16..4:19): Int => scala/Int#
             |[4:23..4:24): x <= local0
             |[4:26..4:29): Int => scala/Int#
             |[4:31..4:34): ann => _empty_/t212.ann#
             |[4:34..4:34):  => _empty_/t212.ann#`<init>`().
             |[4:35..4:36): c => _empty_/t212.c.
             |[4:37..4:40): map => _empty_/t212.C#map().
             |[4:41..4:42): y <= local9
             |[4:41..4:69): y => y match { case z => z } <= local3
             |[4:46..4:47): y => local9
             |[4:61..4:62): z <= local1
             |[4:66..4:67): z => local1
             |[4:76..4:77): x => local0
             |[5:6..5:7): g <= _empty_/t212.g.
             |[5:9..5:24): PartialFunction => scala/PartialFunction#
             |[5:25..5:28): Int => scala/Int#
             |[5:30..5:33): Int => scala/Int#
             |[6:4..6:81): (x: Int @ann(c.map(y => y match { case z => z }))) => x match { case w => w } <= local10
             |[6:5..6:6): x <= local17
             |[6:58..6:59): x => local17
             |[6:73..6:74): w <= local18
             |[6:78..6:79): w => local18
             |[7:6..7:7): h <= _empty_/t212.h.
             |[7:9..7:24): PartialFunction => scala/PartialFunction#
             |[7:25..7:28): Int => scala/Int#
             |[7:30..7:33): Int => scala/Int#
             |[8:4..8:76): (x: Int @ann(c.map(_ match { case z => z }))) => x match { case w => w } <= local19
             |[8:5..8:6): x <= local26
             |[8:53..8:54): x => local26
             |[8:68..8:69): w <= local27
             |[8:73..8:74): w => local27
             |""".stripMargin
      },
      """|[0:7..0:11): t212 <= _empty_/t212.
         |[1:8..1:11): ann <= _empty_/t212.ann#
         |[1:11..1:11):  <= _empty_/t212.ann#`<init>`().
         |[1:12..1:13): a <= _empty_/t212.ann#`<init>`().(a)
         |[1:12..1:13): a <= _empty_/t212.ann#a.
         |[1:15..1:18): Any => scala/Any#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[2:8..2:9): C <= _empty_/t212.C#
         |[2:10..2:10):  <= _empty_/t212.C#`<init>`().
         |[2:16..2:19): map <= _empty_/t212.C#map().
         |[2:20..2:21): f <= _empty_/t212.C#map().(f)
         |[2:23..2:38): PartialFunction => scala/PartialFunction#
         |[2:39..2:42): Int => scala/Int#
         |[2:44..2:47): Int => scala/Int#
         |[2:51..2:55): List => scala/package.List#
         |[2:56..2:59): Int => scala/Int#
         |[2:63..2:66): ??? => scala/Predef.`???`().
         |[3:6..3:7): c <= _empty_/t212.c.
         |[3:14..3:15): C => _empty_/t212.C#
         |[3:15..3:15):  => _empty_/t212.C#`<init>`().
         |[4:6..4:7): f <= _empty_/t212.f.
         |[4:9..4:12): Int => scala/Int#
         |[4:16..4:19): Int => scala/Int#
         |[4:23..4:24): x <= local0
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:34): ann => _empty_/t212.ann#
         |[4:34..4:34):  => _empty_/t212.ann#`<init>`().
         |[4:35..4:36): c => _empty_/t212.c.
         |[4:37..4:40): map => _empty_/t212.C#map().
         |[4:41..4:42): y <= local9
         |[4:41..4:69): y => y match { case z => z } <= local3
         |[4:46..4:47): y => local9
         |[4:61..4:62): z <= local1
         |[4:66..4:67): z => local1
         |[4:76..4:77): x => local0
         |[5:6..5:7): g <= _empty_/t212.g.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[6:4..6:81): (x: Int @ann(c.map(y => y match { case z => z }))) => x match { case w => w } <= local10
         |[6:5..6:6): x <= local17
         |[6:8..6:11): Int => scala/Int#
         |[6:13..6:16): ann => _empty_/t212.ann#
         |[6:16..6:16):  => _empty_/t212.ann#`<init>`().
         |[6:17..6:18): c => _empty_/t212.c.
         |[6:19..6:22): map => _empty_/t212.C#map().
         |[6:23..6:24): y <= local26
         |[6:23..6:51): y => y match { case z => z } <= local19
         |[6:28..6:29): y => local26
         |[6:43..6:44): z <= local27
         |[6:48..6:49): z => local27
         |[6:58..6:59): x => local17
         |[6:73..6:74): w <= local18
         |[6:78..6:79): w => local18
         |[7:6..7:7): h <= _empty_/t212.h.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:28): Int => scala/Int#
         |[7:30..7:33): Int => scala/Int#
         |[8:4..8:76): (x: Int @ann(c.map(_ match { case z => z }))) => x match { case w => w } <= local28
         |[8:5..8:6): x <= local35
         |[8:8..8:11): Int => scala/Int#
         |[8:13..8:16): ann => _empty_/t212.ann#
         |[8:16..8:16):  => _empty_/t212.ann#`<init>`().
         |[8:17..8:18): c => _empty_/t212.c.
         |[8:19..8:22): map => _empty_/t212.C#map().
         |[8:23..8:24): _ <= local44
         |[8:23..8:46): _ match { case z => z } <= local37
         |[8:38..8:39): z <= local45
         |[8:43..8:44): z => local45
         |[8:53..8:54): x => local35
         |[8:68..8:69): w <= local36
         |[8:73..8:74): w => local36
         |""".stripMargin,
    ),
  )

  if (ScalaVersion.is213) occurrences(
    "a for comprehension with a value over a map of a partial function",
    """|object t213 {
       |  class ann(a: Any) extends scala.annotation.StaticAnnotation
       |  class C { def map[B](f: PartialFunction[Int, B]): List[B] = ??? }
       |  val c = new C
       |  val a = for (x <- c; y = x + 1) yield y
       |  val b: Int => Any = _ -> (for (x <- c; y = x + 1) yield y)
       |  val g: PartialFunction[Int, Int] = (z: Int @ann(for (x <- c; y = x) yield y)) => z
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|[0:7..0:11): t213 <= _empty_/t213.
             |[1:8..1:11): ann <= _empty_/t213.ann#
             |[1:11..1:11):  <= _empty_/t213.ann#`<init>`().
             |[1:12..1:13): a <= _empty_/t213.ann#`<init>`().(a)
             |[1:12..1:13): a <= _empty_/t213.ann#a.
             |[1:15..1:18): Any => scala/Any#
             |[1:28..1:33): scala => scala/
             |[1:34..1:44): annotation => scala/annotation/
             |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
             |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
             |[2:8..2:9): C <= _empty_/t213.C#
             |[2:10..2:10):  <= _empty_/t213.C#`<init>`().
             |[2:16..2:19): map <= _empty_/t213.C#map().
             |[2:20..2:21): B <= _empty_/t213.C#map().[B]
             |[2:23..2:24): f <= _empty_/t213.C#map().(f)
             |[2:26..2:41): PartialFunction => scala/PartialFunction#
             |[2:42..2:45): Int => scala/Int#
             |[2:47..2:48): B => _empty_/t213.C#map().[B]
             |[2:52..2:56): List => scala/package.List#
             |[2:57..2:58): B => _empty_/t213.C#map().[B]
             |[2:62..2:65): ??? => scala/Predef.`???`().
             |[3:6..3:7): c <= _empty_/t213.c.
             |[3:14..3:15): C => _empty_/t213.C#
             |[3:15..3:15):  => _empty_/t213.C#`<init>`().
             |[4:6..4:7): a <= _empty_/t213.a.
             |[4:15..4:16): x <= local8
             |[4:20..4:21): c => _empty_/t213.c.
             |[4:23..4:23):  <= local2
             |[4:23..4:24): y <= local9
             |[4:27..4:28): x => local8
             |[4:29..4:30): + => scala/Int#`+`(+4).
             |[4:40..4:41): y => local9
             |[5:6..5:7): b <= _empty_/t213.b.
             |[5:9..5:12): Int => scala/Int#
             |[5:16..5:19): Any => scala/Any#
             |[5:22..5:23): _ <= local10
             |[5:24..5:26): -> => scala/Predef.ArrowAssoc#`->`().
             |[5:33..5:34): x <= local19
             |[5:38..5:39): c => _empty_/t213.c.
             |[5:41..5:41):  <= local13
             |[5:41..5:42): y <= local20
             |[5:45..5:46): x => local19
             |[5:47..5:48): + => scala/Int#`+`(+4).
             |[5:58..5:59): y => local20
             |[6:6..6:7): g <= _empty_/t213.g.
             |[6:9..6:24): PartialFunction => scala/PartialFunction#
             |[6:25..6:28): Int => scala/Int#
             |[6:30..6:33): Int => scala/Int#
             |[6:37..6:84): (z: Int @ann(for (x <- c; y = x) yield y)) => z <= local21
             |[6:38..6:39): z <= local28
             |[6:83..6:84): z => local28
             |""".stripMargin
      },
      """|[0:7..0:11): t213 <= _empty_/t213.
         |[1:8..1:11): ann <= _empty_/t213.ann#
         |[1:11..1:11):  <= _empty_/t213.ann#`<init>`().
         |[1:12..1:13): a <= _empty_/t213.ann#`<init>`().(a)
         |[1:12..1:13): a <= _empty_/t213.ann#a.
         |[1:15..1:18): Any => scala/Any#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[2:8..2:9): C <= _empty_/t213.C#
         |[2:10..2:10):  <= _empty_/t213.C#`<init>`().
         |[2:16..2:19): map <= _empty_/t213.C#map().
         |[2:20..2:21): B <= _empty_/t213.C#map().[B]
         |[2:23..2:24): f <= _empty_/t213.C#map().(f)
         |[2:26..2:41): PartialFunction => scala/PartialFunction#
         |[2:42..2:45): Int => scala/Int#
         |[2:47..2:48): B => _empty_/t213.C#map().[B]
         |[2:52..2:56): List => scala/package.List#
         |[2:57..2:58): B => _empty_/t213.C#map().[B]
         |[2:62..2:65): ??? => scala/Predef.`???`().
         |[3:6..3:7): c <= _empty_/t213.c.
         |[3:14..3:15): C => _empty_/t213.C#
         |[3:15..3:15):  => _empty_/t213.C#`<init>`().
         |[4:6..4:7): a <= _empty_/t213.a.
         |[4:15..4:16): x <= local8
         |[4:20..4:21): c => _empty_/t213.c.
         |[4:23..4:23):  <= local2
         |[4:23..4:24): y <= local9
         |[4:27..4:28): x => local8
         |[4:29..4:30): + => scala/Int#`+`(+4).
         |[4:40..4:41): y => local9
         |[5:6..5:7): b <= _empty_/t213.b.
         |[5:9..5:12): Int => scala/Int#
         |[5:16..5:19): Any => scala/Any#
         |[5:22..5:23): _ <= local10
         |[5:24..5:26): -> => scala/Predef.ArrowAssoc#`->`().
         |[5:33..5:34): x <= local19
         |[5:38..5:39): c => _empty_/t213.c.
         |[5:41..5:41):  <= local13
         |[5:41..5:42): y <= local20
         |[5:45..5:46): x => local19
         |[5:47..5:48): + => scala/Int#`+`(+4).
         |[5:58..5:59): y => local20
         |[6:6..6:7): g <= _empty_/t213.g.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:28): Int => scala/Int#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:84): (z: Int @ann(for (x <- c; y = x) yield y)) => z <= local21
         |[6:38..6:39): z <= local38
         |[6:41..6:44): Int => scala/Int#
         |[6:46..6:49): ann => _empty_/t213.ann#
         |[6:49..6:49):  => _empty_/t213.ann#`<init>`().
         |[6:55..6:56): x <= local36
         |[6:60..6:61): c => _empty_/t213.c.
         |[6:63..6:63):  <= local30
         |[6:63..6:64): y <= local37
         |[6:67..6:68): x => local36
         |[6:76..6:77): y => local37
         |[6:83..6:84): z => local38
         |""".stripMargin,
    ),
  )

  if (ScalaVersion.is213) symbols(
    "a for comprehension with a value over a map of a partial function",
    """|object t214 {
       |  class C { def map[B](f: PartialFunction[Int, B]): List[B] = ??? }
       |  val a = for (x <- new C; y = x + 1) yield y
       |}
       |""".stripMargin,
    """|_empty_/t214. => final object t214 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t214.C# => class C extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t214.C#`<init>`(). => primary ctor <init>()
       |_empty_/t214.C#map(). => method map[B](f: PartialFunction[Int, B]): List[B]
       |  B => _empty_/t214.C#map().[B]
       |  f => _empty_/t214.C#map().(f)
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  List => scala/package.List#
       |_empty_/t214.C#map().(f) => param f: PartialFunction[Int, B]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  B => _empty_/t214.C#map().[B]
       |_empty_/t214.C#map().[B] => typeparam B
       |_empty_/t214.a. => val method a: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => synthetic param x$1: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local1
       |  Int => scala/Int#
       |local1 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local2 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Tuple2[A1, Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local1
       |  Serializable => java/io/Serializable#
       |local3 => final method applyOrElse[A1 <: Int, B1 >: Tuple2[A1, Int]](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local1
       |  Int => scala/Int#
       |  B1 => local7
       |  Tuple2 => scala/Tuple2#
       |  x => local5
       |  default => local6
       |  Function1 => scala/Function1#
       |local4 => final method isDefinedAt(val x: A1): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local8
       |  A1 => local1
       |  Boolean => scala/Boolean#
       |local5 => synthetic param x: A1
       |  A1 => local1
       |local6 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local1
       |  B1 => local7
       |local7 => typeparam B1 >: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local1
       |  Int => scala/Int#
       |local8 => val local x: A1
       |  A1 => local1
       |local9 => val local y: Int
       |  Int => scala/Int#
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
       |[8:15..8:16): x <= local8
       |[8:20..8:21): c => _empty_/t215.c.
       |[8:23..8:23):  <= local2
       |[8:23..8:24): y <= local9
       |[8:27..8:28): x => local8
       |[8:29..8:30): + => scala/Int#`+`(+4).
       |[8:34..8:35): z <= local10
       |[8:38..8:39): y => local9
       |[8:40..8:41): + => scala/Int#`+`(+4).
       |[8:51..8:52): z => local10
       |[9:6..9:7): b <= _empty_/t215.b.
       |[9:15..9:16): x <= local20
       |[9:20..9:21): c => _empty_/t215.c.
       |[9:25..9:26): x => local20
       |[9:27..9:27):  <= local13
       |[9:27..9:28): > => scala/Int#`>`(+3).
       |[9:32..9:32):  <= local21
       |[9:32..9:33): y <= local27
       |[9:36..9:37): x => local20
       |[9:38..9:39): + => scala/Int#`+`(+4).
       |[9:49..9:50): y => local27
       |[10:6..10:7): f <= _empty_/t215.f.
       |[10:15..10:16): x <= local37
       |[10:20..10:21): c => _empty_/t215.c.
       |[10:23..10:23):  <= local31
       |[10:23..10:24): y <= local38
       |[10:27..10:28): x => local37
       |[10:29..10:30): + => scala/Int#`+`(+4).
       |[10:36..10:37): y => local38
       |[10:38..10:39): > => scala/Int#`>`(+3).
       |[10:49..10:50): y => local38
       |[11:6..11:7): g <= _empty_/t215.g.
       |[11:15..11:16): x <= local48
       |[11:20..11:21): c => _empty_/t215.c.
       |[11:23..11:24): z <= local46
       |[11:25..11:25):  <= local40
       |[11:28..11:32): List => scala/package.List.
       |[11:33..11:34): x => local48
       |[11:37..11:38): y <= local49
       |[11:41..11:42): z => local46
       |[11:50..11:51): y => local49
       |[12:7..12:8): x <= local58
       |[12:12..12:13): c => _empty_/t215.c.
       |[12:15..12:15):  <= local52
       |[12:15..12:16): y <= local59
       |[12:19..12:20): x => local58
       |[12:21..12:22): + => scala/Int#`+`(+4).
       |[12:26..12:33): println => scala/Predef.println(+1).
       |[12:34..12:35): y => local59
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "a for comprehension with values over methods of partial functions",
    """|object t216 {
       |  class D[A] {
       |    def map[B](f: PartialFunction[A, B]): D[B] = ???
       |    def flatMap[B](f: PartialFunction[A, D[B]]): D[B] = ???
       |    def foreach(f: PartialFunction[A, Unit]): Unit = ???
       |    def withFilter(f: PartialFunction[A, Boolean]): D[A] = ???
       |  }
       |  val d = new D[Int]
       |  val e = new D[(Int, Int)]
       |  val a = for (x <- d; y = x + 1) yield y
       |  val b = for (x <- d; z <- d; y = z + x) yield y
       |  for (x <- d; y = x + 1) println(y)
       |  val f = for (x <- d; (y, w) = (x, x)) yield y + w
       |  val g = for (x: Int <- d; y = x) yield y
       |  val h = for ((x, w) <- e; y = x + w) yield y
       |}
       |""".stripMargin,
    """|[0:7..0:11): t216 <= _empty_/t216.
       |[1:8..1:9): D <= _empty_/t216.D#
       |[1:10..1:11): A <= _empty_/t216.D#[A]
       |[1:13..1:13):  <= _empty_/t216.D#`<init>`().
       |[2:8..2:11): map <= _empty_/t216.D#map().
       |[2:12..2:13): B <= _empty_/t216.D#map().[B]
       |[2:15..2:16): f <= _empty_/t216.D#map().(f)
       |[2:18..2:33): PartialFunction => scala/PartialFunction#
       |[2:34..2:35): A => _empty_/t216.D#[A]
       |[2:37..2:38): B => _empty_/t216.D#map().[B]
       |[2:42..2:43): D => _empty_/t216.D#
       |[2:44..2:45): B => _empty_/t216.D#map().[B]
       |[2:49..2:52): ??? => scala/Predef.`???`().
       |[3:8..3:15): flatMap <= _empty_/t216.D#flatMap().
       |[3:16..3:17): B <= _empty_/t216.D#flatMap().[B]
       |[3:19..3:20): f <= _empty_/t216.D#flatMap().(f)
       |[3:22..3:37): PartialFunction => scala/PartialFunction#
       |[3:38..3:39): A => _empty_/t216.D#[A]
       |[3:41..3:42): D => _empty_/t216.D#
       |[3:43..3:44): B => _empty_/t216.D#flatMap().[B]
       |[3:49..3:50): D => _empty_/t216.D#
       |[3:51..3:52): B => _empty_/t216.D#flatMap().[B]
       |[3:56..3:59): ??? => scala/Predef.`???`().
       |[4:8..4:15): foreach <= _empty_/t216.D#foreach().
       |[4:16..4:17): f <= _empty_/t216.D#foreach().(f)
       |[4:19..4:34): PartialFunction => scala/PartialFunction#
       |[4:35..4:36): A => _empty_/t216.D#[A]
       |[4:38..4:42): Unit => scala/Unit#
       |[4:46..4:50): Unit => scala/Unit#
       |[4:53..4:56): ??? => scala/Predef.`???`().
       |[5:8..5:18): withFilter <= _empty_/t216.D#withFilter().
       |[5:19..5:20): f <= _empty_/t216.D#withFilter().(f)
       |[5:22..5:37): PartialFunction => scala/PartialFunction#
       |[5:38..5:39): A => _empty_/t216.D#[A]
       |[5:41..5:48): Boolean => scala/Boolean#
       |[5:52..5:53): D => _empty_/t216.D#
       |[5:54..5:55): A => _empty_/t216.D#[A]
       |[5:59..5:62): ??? => scala/Predef.`???`().
       |[7:6..7:7): d <= _empty_/t216.d.
       |[7:14..7:15): D => _empty_/t216.D#
       |[7:16..7:19): Int => scala/Int#
       |[7:20..7:20):  => _empty_/t216.D#`<init>`().
       |[8:6..8:7): e <= _empty_/t216.e.
       |[8:14..8:15): D => _empty_/t216.D#
       |[8:17..8:20): Int => scala/Int#
       |[8:22..8:25): Int => scala/Int#
       |[8:27..8:27):  => _empty_/t216.D#`<init>`().
       |[9:6..9:7): a <= _empty_/t216.a.
       |[9:15..9:16): x <= local7
       |[9:17..9:17):  <= local9
       |[9:20..9:21): d => _empty_/t216.d.
       |[9:23..9:23):  <= local1
       |[9:23..9:24): y <= local8
       |[9:27..9:28): x => local7
       |[9:29..9:30): + => scala/Int#`+`(+4).
       |[9:40..9:41): y => local8
       |[10:6..10:7): b <= _empty_/t216.b.
       |[10:15..10:16): x <= local32
       |[10:20..10:21): d => _empty_/t216.d.
       |[10:23..10:24): z <= local31
       |[10:25..10:25):  <= local34
       |[10:28..10:29): d => _empty_/t216.d.
       |[10:31..10:31):  <= local25
       |[10:31..10:32): y <= local33
       |[10:35..10:36): z => local31
       |[10:37..10:38): + => scala/Int#`+`(+4).
       |[10:39..10:40): x => local32
       |[10:48..10:49): y => local33
       |[11:7..11:8): x <= local49
       |[11:9..11:9):  <= local51
       |[11:12..11:13): d => _empty_/t216.d.
       |[11:15..11:15):  <= local43
       |[11:15..11:16): y <= local50
       |[11:19..11:20): x => local49
       |[11:21..11:22): + => scala/Int#`+`(+4).
       |[11:26..11:33): println => scala/Predef.println(+1).
       |[11:34..11:35): y => local50
       |[12:6..12:7): f <= _empty_/t216.f.
       |[12:15..12:16): x <= local66
       |[12:17..12:17):  <= local69
       |[12:20..12:21): d => _empty_/t216.d.
       |[12:23..12:23):  <= local60
       |[12:24..12:25): y <= local67
       |[12:27..12:28): w <= local68
       |[12:33..12:34): x => local66
       |[12:36..12:37): x => local66
       |[12:46..12:47): y => local67
       |[12:48..12:49): + => scala/Int#`+`(+4).
       |[12:50..12:51): w => local68
       |[13:6..13:7): g <= _empty_/t216.g.
       |[13:15..13:16): x <= local85
       |[13:18..13:21): Int => scala/Int#
       |[13:22..13:22):  <= local94
       |[13:25..13:25):  <= local78
       |[13:25..13:26): d => _empty_/t216.d.
       |[13:28..13:28):  <= local87
       |[13:28..13:29): y <= local93
       |[13:32..13:33): x => local85
       |[13:41..13:42): y => local93
       |[14:6..14:7): h <= _empty_/t216.h.
       |[14:16..14:17): x <= local110
       |[14:19..14:20): w <= local111
       |[14:22..14:22):  <= local122
       |[14:25..14:25):  <= local103
       |[14:25..14:26): e => _empty_/t216.e.
       |[14:28..14:29): y <= local120
       |[14:32..14:33): x => local110
       |[14:34..14:35): + => scala/Int#`+`(+4).
       |[14:36..14:37): w => local111
       |[14:45..14:46): y => local120
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "a placeholder in an enumerator of a for over partial functions",
    """|object t217 {
       |  val a: PartialFunction[List[Int], List[Int]] = for (x <- _) yield x + 1
       |  val b: PartialFunction[Int, List[Int]] = for (x <- List(1, 2) if x > _) yield x
       |  val c: PartialFunction[Int, List[Int]] = for (x <- List(1, 2); y = _) yield x + y
       |}
       |""".stripMargin,
    """|[0:7..0:11): t217 <= _empty_/t217.
       |[1:6..1:7): a <= _empty_/t217.a.
       |[1:9..1:24): PartialFunction => scala/PartialFunction#
       |[1:25..1:29): List => scala/package.List#
       |[1:30..1:33): Int => scala/Int#
       |[1:36..1:40): List => scala/package.List#
       |[1:41..1:44): Int => scala/Int#
       |[1:49..1:73): for (x <- _) yield x + 1 <= local0
       |[1:54..1:55): x <= local8
       |[1:59..1:60): _ <= local7
       |[1:68..1:69): x => local8
       |[1:70..1:71): + => scala/Int#`+`(+4).
       |[2:6..2:7): b <= _empty_/t217.b.
       |[2:9..2:24): PartialFunction => scala/PartialFunction#
       |[2:25..2:28): Int => scala/Int#
       |[2:30..2:34): List => scala/package.List#
       |[2:35..2:38): Int => scala/Int#
       |[2:43..2:81): for (x <- List(1, 2) if x > _) yield x <= local9
       |[2:48..2:49): x <= local17
       |[2:53..2:57): List => scala/package.List.
       |[2:67..2:68): x => local17
       |[2:69..2:70): > => scala/Int#`>`(+3).
       |[2:71..2:72): _ <= local16
       |[2:80..2:81): x => local17
       |[3:6..3:7): c <= _empty_/t217.c.
       |[3:9..3:24): PartialFunction => scala/PartialFunction#
       |[3:25..3:28): Int => scala/Int#
       |[3:30..3:34): List => scala/package.List#
       |[3:35..3:38): Int => scala/Int#
       |[3:43..3:83): for (x <- List(1, 2); y = _) yield x + y <= local18
       |[3:48..3:49): x <= local26
       |[3:53..3:57): List => scala/package.List.
       |[3:65..3:66): y <= local28
       |[3:69..3:70): _ <= local25
       |[3:78..3:79): x => local26
       |[3:80..3:81): + => scala/Int#`+`(+4).
       |[3:82..3:83): y => local28
       |""".stripMargin,
  )

  occurrences(
    "a typed parameter of a partial function lambda with a match",
    """|object t218 {
       |  type I = Int
       |  class ann extends scala.annotation.StaticAnnotation
       |  val a: PartialFunction[Int, Int] = (x: I) => x match { case w => w }
       |  val b: PartialFunction[List[Int], Int] = (x: Seq[Int]) => x match { case w => w.size }
       |  val c: PartialFunction[Int, Int] = (x: Int @ann) => x match { case w => w }
       |  val d: PartialFunction[Int, Int] = (_: I) match { case w => w }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): t218 <= _empty_/t218.
         |[1:7..1:8): I <= _empty_/t218.I#
         |[1:11..1:14): Int => scala/Int#
         |[2:8..2:11): ann <= _empty_/t218.ann#
         |[2:12..2:12):  <= _empty_/t218.ann#`<init>`().
         |[2:20..2:25): scala => scala/
         |[2:26..2:36): annotation => scala/annotation/
         |[2:37..2:53): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[2:53..2:53):  => scala/annotation/Annotation#`<init>`().
         |[3:6..3:7): a <= _empty_/t218.a.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:28): Int => scala/Int#
         |[3:30..3:33): Int => scala/Int#
         |[3:37..3:70): (x: I) => x match { case w => w } <= local0
         |[3:38..3:39): x <= local7
         |[3:47..3:48): x => local7
         |[3:62..3:63): w <= local8
         |[3:67..3:68): w => local8
         |[4:6..4:7): b <= _empty_/t218.b.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:29): List => scala/package.List#
         |[4:30..4:33): Int => scala/Int#
         |[4:36..4:39): Int => scala/Int#
         |[4:43..4:88): (x: Seq[Int]) => x match { case w => w.size } <= local9
         |[4:44..4:45): x <= local16
         |[4:60..4:61): x => local16
         |[4:75..4:76): w <= local17
         |[4:80..4:81): w => local17
         |[4:82..4:86): size => scala/collection/SeqLike#size().
         |[5:6..5:7): c <= _empty_/t218.c.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[5:37..5:77): (x: Int @ann) => x match { case w => w } <= local18
         |[5:38..5:39): x <= local25
         |[5:54..5:55): x => local25
         |[5:69..5:70): w <= local26
         |[5:74..5:75): w => local26
         |[6:6..6:7): d <= _empty_/t218.d.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:28): Int => scala/Int#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:65): (_: I) match { case w => w } <= local27
         |[6:38..6:39): _ <= local34
         |[6:41..6:42): I => _empty_/t218.I#
         |[6:57..6:58): w <= local35
         |[6:62..6:63): w => local35
         |""".stripMargin
    else ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|[0:7..0:11): t218 <= _empty_/t218.
             |[1:7..1:8): I <= _empty_/t218.I#
             |[1:11..1:14): Int => scala/Int#
             |[2:8..2:11): ann <= _empty_/t218.ann#
             |[2:12..2:12):  <= _empty_/t218.ann#`<init>`().
             |[2:20..2:25): scala => scala/
             |[2:26..2:36): annotation => scala/annotation/
             |[2:37..2:53): StaticAnnotation => scala/annotation/StaticAnnotation#
             |[2:53..2:53):  => scala/annotation/Annotation#`<init>`().
             |[3:6..3:7): a <= _empty_/t218.a.
             |[3:9..3:24): PartialFunction => scala/PartialFunction#
             |[3:25..3:28): Int => scala/Int#
             |[3:30..3:33): Int => scala/Int#
             |[3:37..3:70): (x: I) => x match { case w => w } <= local0
             |[3:38..3:39): x <= local7
             |[3:47..3:48): x => local7
             |[3:62..3:63): w <= local8
             |[3:67..3:68): w => local8
             |[4:6..4:7): b <= _empty_/t218.b.
             |[4:9..4:24): PartialFunction => scala/PartialFunction#
             |[4:25..4:29): List => scala/package.List#
             |[4:30..4:33): Int => scala/Int#
             |[4:36..4:39): Int => scala/Int#
             |[4:43..4:88): (x: Seq[Int]) => x match { case w => w.size } <= local9
             |[4:44..4:45): x <= local16
             |[4:60..4:61): x => local16
             |[4:75..4:76): w <= local17
             |[4:80..4:81): w => local17
             |[4:82..4:86): size => scala/collection/SeqOps#size().
             |[5:6..5:7): c <= _empty_/t218.c.
             |[5:9..5:24): PartialFunction => scala/PartialFunction#
             |[5:25..5:28): Int => scala/Int#
             |[5:30..5:33): Int => scala/Int#
             |[5:37..5:77): (x: Int @ann) => x match { case w => w } <= local18
             |[5:38..5:39): x <= local25
             |[5:54..5:55): x => local25
             |[5:69..5:70): w <= local26
             |[5:74..5:75): w => local26
             |[6:6..6:7): d <= _empty_/t218.d.
             |[6:9..6:24): PartialFunction => scala/PartialFunction#
             |[6:25..6:28): Int => scala/Int#
             |[6:30..6:33): Int => scala/Int#
             |[6:37..6:65): (_: I) match { case w => w } <= local27
             |[6:38..6:39): _ <= local34
             |[6:41..6:42): I => _empty_/t218.I#
             |[6:57..6:58): w <= local35
             |[6:62..6:63): w => local35
             |""".stripMargin
      },
      """|[0:7..0:11): t218 <= _empty_/t218.
         |[1:7..1:8): I <= _empty_/t218.I#
         |[1:11..1:14): Int => scala/Int#
         |[2:8..2:11): ann <= _empty_/t218.ann#
         |[2:12..2:12):  <= _empty_/t218.ann#`<init>`().
         |[2:20..2:25): scala => scala/
         |[2:26..2:36): annotation => scala/annotation/
         |[2:37..2:53): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[2:53..2:53):  => scala/annotation/Annotation#`<init>`().
         |[3:6..3:7): a <= _empty_/t218.a.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:28): Int => scala/Int#
         |[3:30..3:33): Int => scala/Int#
         |[3:37..3:70): (x: I) => x match { case w => w } <= local0
         |[3:38..3:39): x <= local7
         |[3:41..3:42): I => _empty_/t218.I#
         |[3:47..3:48): x => local7
         |[3:62..3:63): w <= local8
         |[3:67..3:68): w => local8
         |[4:6..4:7): b <= _empty_/t218.b.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:29): List => scala/package.List#
         |[4:30..4:33): Int => scala/Int#
         |[4:36..4:39): Int => scala/Int#
         |[4:43..4:88): (x: Seq[Int]) => x match { case w => w.size } <= local9
         |[4:44..4:45): x <= local16
         |[4:47..4:50): Seq => scala/package.Seq#
         |[4:51..4:54): Int => scala/Int#
         |[4:60..4:61): x => local16
         |[4:75..4:76): w <= local17
         |[4:80..4:81): w => local17
         |[4:82..4:86): size => scala/collection/SeqOps#size().
         |[5:6..5:7): c <= _empty_/t218.c.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[5:37..5:77): (x: Int @ann) => x match { case w => w } <= local18
         |[5:38..5:39): x <= local25
         |[5:41..5:44): Int => scala/Int#
         |[5:46..5:49): ann => _empty_/t218.ann#
         |[5:49..5:49):  => _empty_/t218.ann#`<init>`().
         |[5:54..5:55): x => local25
         |[5:69..5:70): w <= local26
         |[5:74..5:75): w => local26
         |[6:6..6:7): d <= _empty_/t218.d.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:28): Int => scala/Int#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:65): (_: I) match { case w => w } <= local27
         |[6:38..6:39): _ <= local34
         |[6:41..6:42): I => _empty_/t218.I#
         |[6:57..6:58): w <= local35
         |[6:62..6:63): w => local35
         |""".stripMargin,
    ),
  )

  symbols(
    "a for comprehension with a match over a map of a partial function",
    """|object t209 {
       |  class C { def map(f: PartialFunction[Int, Int]): List[Int] = ??? }
       |  val c = new C
       |  val a = for (x <- c) yield x match { case z => z }
       |  val b: Int => List[Int] = _ :: (for (x <- c) yield x match { case z => z })
       |  val d: Int => List[Int] = y => y :: (for (x <- c) yield x match { case z => z })
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t209. => final object t209 extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t209.C# => class C extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t209.C#`<init>`(). => primary ctor <init>()
         |_empty_/t209.C#map(). => method map(f: PartialFunction[Int, Int]): List[Int]
         |  f => _empty_/t209.C#map().(f)
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |_empty_/t209.C#map().(f) => param f: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t209.a. => val method a: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t209.b. => val method b: Function1[Int, List[Int]]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |_empty_/t209.c. => val method c: C
         |  C => _empty_/t209.C#
         |_empty_/t209.d. => val method d: Function1[Int, List[Int]]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
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
         |local7 => param x: Int
         |  Int => scala/Int#
         |local8 => val local z: A1
         |  A1 => local6
         |local9 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local11 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local16
         |  Int => scala/Int#
         |  B1 => local15
         |  x => local13
         |  default => local14
         |  Function1 => scala/Function1#
         |local12 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local13 => synthetic param x: A1
         |  A1 => local16
         |local14 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local16
         |  B1 => local15
         |local15 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local16 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local17 => param x: Int
         |  Int => scala/Int#
         |local18 => val local z: A1
         |  A1 => local16
         |local19 => param y: Int
         |  Int => scala/Int#
         |local20 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local21 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local26
         |  Int => scala/Int#
         |  B1 => local25
         |  x => local23
         |  default => local24
         |  Function1 => scala/Function1#
         |local22 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local27
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local23 => synthetic param x: A1
         |  A1 => local26
         |local24 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local26
         |  B1 => local25
         |local25 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local26 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local27 => param x: Int
         |  Int => scala/Int#
         |local28 => val local z: A1
         |  A1 => local26
         |""".stripMargin
    else
      """|_empty_/t209. => final object t209 extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t209.C# => class C extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t209.C#`<init>`(). => primary ctor <init>()
         |_empty_/t209.C#map(). => method map(f: PartialFunction[Int, Int]): List[Int]
         |  f => _empty_/t209.C#map().(f)
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |_empty_/t209.C#map().(f) => param f: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t209.a. => val method a: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t209.b. => val method b: Function1[Int, List[Int]]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |_empty_/t209.c. => val method c: C
         |  C => _empty_/t209.C#
         |_empty_/t209.d. => val method d: Function1[Int, List[Int]]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  List => scala/package.List#
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
         |local7 => param x: Int
         |  Int => scala/Int#
         |local8 => val local z: A1
         |  A1 => local6
         |local9 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local11 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local16
         |  Int => scala/Int#
         |  B1 => local15
         |  x => local13
         |  default => local14
         |  Function1 => scala/Function1#
         |local12 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local13 => synthetic param x: A1
         |  A1 => local16
         |local14 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local16
         |  B1 => local15
         |local15 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local16 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local17 => param x: Int
         |  Int => scala/Int#
         |local18 => val local z: A1
         |  A1 => local16
         |local19 => param y: Int
         |  Int => scala/Int#
         |local20 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local21 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local26
         |  Int => scala/Int#
         |  B1 => local25
         |  x => local23
         |  default => local24
         |  Function1 => scala/Function1#
         |local22 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local27
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local23 => synthetic param x: A1
         |  A1 => local26
         |local24 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local26
         |  B1 => local25
         |local25 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local26 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local27 => param x: Int
         |  Int => scala/Int#
         |local28 => val local z: A1
         |  A1 => local26
         |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "a placeholder in an enumerator of a for over partial functions",
    """|object t217 {
       |  val a: PartialFunction[List[Int], List[Int]] = for (x <- _) yield x + 1
       |  val b: PartialFunction[Int, List[Int]] = for (x <- List(1, 2) if x > _) yield x
       |  val c: PartialFunction[Int, List[Int]] = for (x <- List(1, 2); y = _) yield x + y
       |}
       |""".stripMargin,
    """|_empty_/t217. => final object t217 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t217.a. => val method a: PartialFunction[List[Int], List[Int]]
       |  PartialFunction => scala/PartialFunction#
       |  List => scala/package.List#
       |  Int => scala/Int#
       |_empty_/t217.b. => val method b: PartialFunction[Int, List[Int]]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  List => scala/package.List#
       |_empty_/t217.c. => val method c: PartialFunction[Int, List[Int]]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |  List => scala/package.List#
       |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], List[Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  List => scala/package.List#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local1 => final method applyOrElse[A1 <: List[Int], B1 >: List[Int]](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local6
       |  List => scala/package.List#
       |  Int => scala/Int#
       |  B1 => local5
       |  x$1 => local3
       |  default => local4
       |  Function1 => scala/Function1#
       |local2 => final method isDefinedAt(x$1: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$1 => local7
       |  List => scala/package.List#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic param x$1: A1
       |  A1 => local6
       |local4 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local6
       |  B1 => local5
       |local5 => typeparam B1 >: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local6 => typeparam A1 <: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local7 => synthetic param x$1: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local8 => param x: Int
       |  Int => scala/Int#
       |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, List[Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  List => scala/package.List#
       |  Serializable => java/io/Serializable#
       |local10 => final method applyOrElse[A1 <: Int, B1 >: List[Int]](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local15
       |  Int => scala/Int#
       |  B1 => local14
       |  List => scala/package.List#
       |  x$2 => local12
       |  default => local13
       |  Function1 => scala/Function1#
       |local11 => final method isDefinedAt(x$2: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$2 => local16
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local12 => synthetic param x$2: A1
       |  A1 => local15
       |local13 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local15
       |  B1 => local14
       |local14 => typeparam B1 >: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local15 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local16 => synthetic param x$2: Int
       |  Int => scala/Int#
       |local17 => param x: Int
       |  Int => scala/Int#
       |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, List[Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  List => scala/package.List#
       |  Serializable => java/io/Serializable#
       |local19 => final method applyOrElse[A1 <: Int, B1 >: List[Int]](x$3: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local24
       |  Int => scala/Int#
       |  B1 => local23
       |  List => scala/package.List#
       |  x$3 => local21
       |  default => local22
       |  Function1 => scala/Function1#
       |local20 => final method isDefinedAt(x$3: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$3 => local25
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local21 => synthetic param x$3: A1
       |  A1 => local24
       |local22 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local24
       |  B1 => local23
       |local23 => typeparam B1 >: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local24 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local25 => synthetic param x$3: Int
       |  Int => scala/Int#
       |local26 => val local x: Int
       |  Int => scala/Int#
       |local27 => synthetic param x$4: Tuple2[Int, A1]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  A1 => local24
       |local28 => val local y: A1
       |  A1 => local24
       |""".stripMargin,
  )

  symbols(
    "a typed parameter of a partial function lambda with a match",
    """|object t218 {
       |  type I = Int
       |  class ann extends scala.annotation.StaticAnnotation
       |  val a: PartialFunction[Int, Int] = (x: I) => x match { case w => w }
       |  val b: PartialFunction[List[Int], Int] = (x: Seq[Int]) => x match { case w => w.size }
       |  val c: PartialFunction[Int, Int] = (x: Int @ann) => x match { case w => w }
       |  val d: PartialFunction[Int, Int] = (_: I) match { case w => w }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t218. => final object t218 extends AnyRef { +6 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t218.I# => type I = Int
         |  Int => scala/Int#
         |_empty_/t218.a. => val method a: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t218.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
         |  Annotation => scala/annotation/Annotation#
         |  StaticAnnotation => scala/annotation/StaticAnnotation#
         |_empty_/t218.ann#`<init>`(). => primary ctor <init>()
         |_empty_/t218.b. => val method b: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t218.c. => val method c: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t218.d. => val method d: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
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
         |local7 => param x: Int
         |  Int => scala/Int#
         |local8 => val local w: A1
         |  A1 => local6
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local10 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  B1 => local14
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local16
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local16 => param x: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local17 => val local w: A1
         |  A1 => local15
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local19 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  Int => scala/Int#
         |  B1 => local23
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local25 => param x: Int
         |  Int => scala/Int#
         |local26 => val local w: A1
         |  A1 => local24
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local28 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  Int => scala/Int#
         |  B1 => local32
         |  x$1 => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local34
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param x$1: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local34 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local35 => val local w: I
         |  I => _empty_/t218.I#
         |""".stripMargin
    else ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|_empty_/t218. => final object t218 extends AnyRef { +6 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t218.I# => type I = Int
             |  Int => scala/Int#
             |_empty_/t218.a. => val method a: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t218.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
             |  Annotation => scala/annotation/Annotation#
             |  StaticAnnotation => scala/annotation/StaticAnnotation#
             |_empty_/t218.ann#`<init>`(). => primary ctor <init>()
             |_empty_/t218.b. => val method b: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t218.c. => val method c: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t218.d. => val method d: PartialFunction[Int, Int]
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
             |local7 => param x: Int
             |  Int => scala/Int#
             |local8 => val local w: A1
             |  A1 => local6
             |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local10 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local15
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local14
             |  x => local12
             |  default => local13
             |  Function1 => scala/Function1#
             |local11 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local16
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local12 => synthetic param x: A1
             |  A1 => local15
             |local13 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local15
             |  B1 => local14
             |local14 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local15 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local16 => param x: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local17 => val local w: A1
             |  A1 => local15
             |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local19 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local24
             |  Int => scala/Int#
             |  B1 => local23
             |  x => local21
             |  default => local22
             |  Function1 => scala/Function1#
             |local20 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local25
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local21 => synthetic param x: A1
             |  A1 => local24
             |local22 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local24
             |  B1 => local23
             |local23 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local24 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local25 => param x: Int
             |  Int => scala/Int#
             |local26 => val local w: A1
             |  A1 => local24
             |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local28 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local33
             |  Int => scala/Int#
             |  B1 => local32
             |  x$1 => local30
             |  default => local31
             |  Function1 => scala/Function1#
             |local29 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x$1 => local34
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local30 => synthetic param x$1: A1
             |  A1 => local33
             |local31 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local33
             |  B1 => local32
             |local32 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local33 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local34 => synthetic param x$1: Int
             |  Int => scala/Int#
             |local35 => val local w: I
             |  I => _empty_/t218.I#
             |""".stripMargin
      },
      """|_empty_/t218. => final object t218 extends AnyRef { +6 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t218.I# => type I = Int
         |  Int => scala/Int#
         |_empty_/t218.a. => val method a: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t218.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
         |  Annotation => scala/annotation/Annotation#
         |  StaticAnnotation => scala/annotation/StaticAnnotation#
         |_empty_/t218.ann#`<init>`(). => primary ctor <init>()
         |_empty_/t218.b. => val method b: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t218.c. => val method c: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t218.d. => val method d: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[I, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  I => _empty_/t218.I#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: I, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  I => _empty_/t218.I#
         |  B1 => local5
         |  Int => scala/Int#
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: I): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  I => _empty_/t218.I#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: I
         |  I => _empty_/t218.I#
         |local7 => param x: I
         |  I => _empty_/t218.I#
         |local8 => val local w: A1
         |  A1 => local6
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Seq[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: Seq[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  B1 => local14
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: Seq[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local16
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Seq[Int]
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |local16 => param x: Seq[Int]
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |local17 => val local w: A1
         |  A1 => local15
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int @ann, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  ann => _empty_/t218.ann#
         |  Serializable => java/io/Serializable#
         |local19 => final method applyOrElse[A1 <: Int @ann, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  Int => scala/Int#
         |  ann => _empty_/t218.ann#
         |  B1 => local23
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(x: Int @ann): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  Int => scala/Int#
         |  ann => _empty_/t218.ann#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t218.ann#
         |local25 => param x: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t218.ann#
         |local26 => val local w: A1
         |  A1 => local24
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[I, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  I => _empty_/t218.I#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local28 => final method applyOrElse[A1 <: I, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  I => _empty_/t218.I#
         |  B1 => local32
         |  Int => scala/Int#
         |  x$1 => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(x$1: I): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local34
         |  I => _empty_/t218.I#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param x$1: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: I
         |  I => _empty_/t218.I#
         |local34 => synthetic param x$1: I
         |  I => _empty_/t218.I#
         |local35 => val local w: I
         |  I => _empty_/t218.I#
         |""".stripMargin,
    ),
  )

  if (ScalaVersion.is213) symbols(
    "typed parameters of partial function lambdas with aliases, type constructors and annotations",
    """|object t219 {
       |  type I = Int
       |  class ann extends scala.annotation.StaticAnnotation
       |  val f: Int => Int = (x: I) => x + 1
       |  val pa: PartialFunction[Int, Int] = (x: I) => x + 1
       |  val pb: PartialFunction[List[Int], Int] = (x: Seq[Int]) => x.size
       |  val pc: PartialFunction[Int, Int] = (x: Int @ann) => x + 1
       |  val pd: PartialFunction[Int, Int] = (_: I) + 1
       |  val pe: PartialFunction[List[Int], Int] = (_: Seq[Int]).size
       |  val pf: PartialFunction[Int, Int] = { implicit x: I => x + 1 }
       |  def pg[A, B <: A]: PartialFunction[B, Int] = (x: A) => 1
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|_empty_/t219. => final object t219 extends AnyRef { +10 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t219.I# => type I = Int
             |  Int => scala/Int#
             |_empty_/t219.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
             |  Annotation => scala/annotation/Annotation#
             |  StaticAnnotation => scala/annotation/StaticAnnotation#
             |_empty_/t219.ann#`<init>`(). => primary ctor <init>()
             |_empty_/t219.f. => val method f: Function1[Int, Int]
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |_empty_/t219.pa. => val method pa: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t219.pb. => val method pb: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t219.pc. => val method pc: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t219.pd. => val method pd: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t219.pe. => val method pe: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t219.pf. => val method pf: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t219.pg(). => method pg[A, B <: A]: PartialFunction[B, Int]
             |  A => _empty_/t219.pg().[A]
             |  B => _empty_/t219.pg().[B]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t219.pg().[A] => typeparam A
             |_empty_/t219.pg().[B] => typeparam B <: A
             |  A => _empty_/t219.pg().[A]
             |local0 => param x: I
             |  I => _empty_/t219.I#
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
             |local8 => param x: Int
             |  Int => scala/Int#
             |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local10 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local15
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local14
             |  x => local12
             |  default => local13
             |  Function1 => scala/Function1#
             |local11 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local16
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local12 => synthetic param x: A1
             |  A1 => local15
             |local13 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local15
             |  B1 => local14
             |local14 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local15 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local16 => param x: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local17 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local18 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local23
             |  Int => scala/Int#
             |  B1 => local22
             |  x => local20
             |  default => local21
             |  Function1 => scala/Function1#
             |local19 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local24
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local20 => synthetic param x: A1
             |  A1 => local23
             |local21 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local23
             |  B1 => local22
             |local22 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local23 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local24 => param x: Int
             |  Int => scala/Int#
             |local25 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local26 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local31
             |  Int => scala/Int#
             |  B1 => local30
             |  x$1 => local28
             |  default => local29
             |  Function1 => scala/Function1#
             |local27 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x$1 => local32
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local28 => synthetic param x$1: A1
             |  A1 => local31
             |local29 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local31
             |  B1 => local30
             |local30 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local31 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local32 => synthetic param x$1: Int
             |  Int => scala/Int#
             |local33 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local34 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local39
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local38
             |  x$2 => local36
             |  default => local37
             |  Function1 => scala/Function1#
             |local35 => final method isDefinedAt(x$2: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x$2 => local40
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local36 => synthetic param x$2: A1
             |  A1 => local39
             |local37 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local39
             |  B1 => local38
             |local38 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local39 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local40 => synthetic param x$2: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local41 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local42 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local47
             |  Int => scala/Int#
             |  B1 => local46
             |  x => local44
             |  default => local45
             |  Function1 => scala/Function1#
             |local43 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local48
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local44 => synthetic param x: A1
             |  A1 => local47
             |local45 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local47
             |  B1 => local46
             |local46 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local47 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local48 => implicit param x: Int
             |  Int => scala/Int#
             |local49 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  B => _empty_/t219.pg().[B]
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local50 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local55
             |  B => _empty_/t219.pg().[B]
             |  B1 => local54
             |  Int => scala/Int#
             |  x => local52
             |  default => local53
             |  Function1 => scala/Function1#
             |local51 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local56
             |  B => _empty_/t219.pg().[B]
             |  Boolean => scala/Boolean#
             |local52 => synthetic param x: A1
             |  A1 => local55
             |local53 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local55
             |  B1 => local54
             |local54 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local55 => typeparam A1 <: B
             |  B => _empty_/t219.pg().[B]
             |local56 => param x: B
             |  B => _empty_/t219.pg().[B]
             |""".stripMargin
      },
      """|_empty_/t219. => final object t219 extends AnyRef { +10 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t219.I# => type I = Int
         |  Int => scala/Int#
         |_empty_/t219.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
         |  Annotation => scala/annotation/Annotation#
         |  StaticAnnotation => scala/annotation/StaticAnnotation#
         |_empty_/t219.ann#`<init>`(). => primary ctor <init>()
         |_empty_/t219.f. => val method f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t219.pa. => val method pa: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t219.pb. => val method pb: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t219.pc. => val method pc: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t219.pd. => val method pd: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t219.pe. => val method pe: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t219.pf. => val method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t219.pg(). => method pg[A, B <: A]: PartialFunction[B, Int]
         |  A => _empty_/t219.pg().[A]
         |  B => _empty_/t219.pg().[B]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t219.pg().[A] => typeparam A
         |_empty_/t219.pg().[B] => typeparam B <: A
         |  A => _empty_/t219.pg().[A]
         |local0 => param x: I
         |  I => _empty_/t219.I#
         |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[I, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  I => _empty_/t219.I#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local2 => final method applyOrElse[A1 <: I, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local7
         |  I => _empty_/t219.I#
         |  B1 => local6
         |  Int => scala/Int#
         |  x => local4
         |  default => local5
         |  Function1 => scala/Function1#
         |local3 => final method isDefinedAt(x: I): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local8
         |  I => _empty_/t219.I#
         |  Boolean => scala/Boolean#
         |local4 => synthetic param x: A1
         |  A1 => local7
         |local5 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local7
         |  B1 => local6
         |local6 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local7 => typeparam A1 <: I
         |  I => _empty_/t219.I#
         |local8 => param x: I
         |  I => _empty_/t219.I#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Seq[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: Seq[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  B1 => local14
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: Seq[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local16
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Seq[Int]
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |local16 => param x: Seq[Int]
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |local17 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int @ann, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  ann => _empty_/t219.ann#
         |  Serializable => java/io/Serializable#
         |local18 => final method applyOrElse[A1 <: Int @ann, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local23
         |  Int => scala/Int#
         |  ann => _empty_/t219.ann#
         |  B1 => local22
         |  x => local20
         |  default => local21
         |  Function1 => scala/Function1#
         |local19 => final method isDefinedAt(x: Int @ann): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local24
         |  Int => scala/Int#
         |  ann => _empty_/t219.ann#
         |  Boolean => scala/Boolean#
         |local20 => synthetic param x: A1
         |  A1 => local23
         |local21 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local23
         |  B1 => local22
         |local22 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local23 => typeparam A1 <: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t219.ann#
         |local24 => param x: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t219.ann#
         |local25 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[I, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  I => _empty_/t219.I#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local26 => final method applyOrElse[A1 <: I, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local31
         |  I => _empty_/t219.I#
         |  B1 => local30
         |  Int => scala/Int#
         |  x$1 => local28
         |  default => local29
         |  Function1 => scala/Function1#
         |local27 => final method isDefinedAt(x$1: I): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local32
         |  I => _empty_/t219.I#
         |  Boolean => scala/Boolean#
         |local28 => synthetic param x$1: A1
         |  A1 => local31
         |local29 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local31
         |  B1 => local30
         |local30 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local31 => typeparam A1 <: I
         |  I => _empty_/t219.I#
         |local32 => synthetic param x$1: I
         |  I => _empty_/t219.I#
         |local33 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Seq[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local34 => final method applyOrElse[A1 <: Seq[Int], B1 >: Int](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local39
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  B1 => local38
         |  x$2 => local36
         |  default => local37
         |  Function1 => scala/Function1#
         |local35 => final method isDefinedAt(x$2: Seq[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$2 => local40
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local36 => synthetic param x$2: A1
         |  A1 => local39
         |local37 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local39
         |  B1 => local38
         |local38 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local39 => typeparam A1 <: Seq[Int]
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |local40 => synthetic param x$2: Seq[Int]
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |local41 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[I, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  I => _empty_/t219.I#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local42 => final method applyOrElse[A1 <: I, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local47
         |  I => _empty_/t219.I#
         |  B1 => local46
         |  Int => scala/Int#
         |  x => local44
         |  default => local45
         |  Function1 => scala/Function1#
         |local43 => final method isDefinedAt(implicit x: I): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local48
         |  I => _empty_/t219.I#
         |  Boolean => scala/Boolean#
         |local44 => synthetic param x: A1
         |  A1 => local47
         |local45 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local47
         |  B1 => local46
         |local46 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local47 => typeparam A1 <: I
         |  I => _empty_/t219.I#
         |local48 => implicit param x: I
         |  I => _empty_/t219.I#
         |local49 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[A, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  A => _empty_/t219.pg().[A]
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local50 => final method applyOrElse[A1 <: A, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local55
         |  A => _empty_/t219.pg().[A]
         |  B1 => local54
         |  Int => scala/Int#
         |  x => local52
         |  default => local53
         |  Function1 => scala/Function1#
         |local51 => final method isDefinedAt(x: A): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local56
         |  A => _empty_/t219.pg().[A]
         |  Boolean => scala/Boolean#
         |local52 => synthetic param x: A1
         |  A1 => local55
         |local53 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local55
         |  B1 => local54
         |local54 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local55 => typeparam A1 <: A
         |  A => _empty_/t219.pg().[A]
         |local56 => param x: A
         |  A => _empty_/t219.pg().[A]
         |""".stripMargin,
    ),
  )

  if (ScalaVersion.is213) occurrences(
    "typed parameters of partial function lambdas with aliases, type constructors and annotations",
    """|object t219 {
       |  type I = Int
       |  class ann extends scala.annotation.StaticAnnotation
       |  val f: Int => Int = (x: I) => x + 1
       |  val pa: PartialFunction[Int, Int] = (x: I) => x + 1
       |  val pb: PartialFunction[List[Int], Int] = (x: Seq[Int]) => x.size
       |  val pc: PartialFunction[Int, Int] = (x: Int @ann) => x + 1
       |  val pd: PartialFunction[Int, Int] = (_: I) + 1
       |  val pe: PartialFunction[List[Int], Int] = (_: Seq[Int]).size
       |  val pf: PartialFunction[Int, Int] = { implicit x: I => x + 1 }
       |  def pg[A, B <: A]: PartialFunction[B, Int] = (x: A) => 1
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|[0:7..0:11): t219 <= _empty_/t219.
             |[1:7..1:8): I <= _empty_/t219.I#
             |[1:11..1:14): Int => scala/Int#
             |[2:8..2:11): ann <= _empty_/t219.ann#
             |[2:12..2:12):  <= _empty_/t219.ann#`<init>`().
             |[2:20..2:25): scala => scala/
             |[2:26..2:36): annotation => scala/annotation/
             |[2:37..2:53): StaticAnnotation => scala/annotation/StaticAnnotation#
             |[2:53..2:53):  => scala/annotation/Annotation#`<init>`().
             |[3:6..3:7): f <= _empty_/t219.f.
             |[3:9..3:12): Int => scala/Int#
             |[3:16..3:19): Int => scala/Int#
             |[3:23..3:24): x <= local0
             |[3:26..3:27): I => _empty_/t219.I#
             |[3:32..3:33): x => local0
             |[3:34..3:35): + => scala/Int#`+`(+4).
             |[4:6..4:8): pa <= _empty_/t219.pa.
             |[4:10..4:25): PartialFunction => scala/PartialFunction#
             |[4:26..4:29): Int => scala/Int#
             |[4:31..4:34): Int => scala/Int#
             |[4:38..4:53): (x: I) => x + 1 <= local1
             |[4:39..4:40): x <= local8
             |[4:48..4:49): x => local8
             |[4:50..4:51): + => scala/Int#`+`(+4).
             |[5:6..5:8): pb <= _empty_/t219.pb.
             |[5:10..5:25): PartialFunction => scala/PartialFunction#
             |[5:26..5:30): List => scala/package.List#
             |[5:31..5:34): Int => scala/Int#
             |[5:37..5:40): Int => scala/Int#
             |[5:44..5:67): (x: Seq[Int]) => x.size <= local9
             |[5:45..5:46): x <= local16
             |[5:61..5:62): x => local16
             |[5:63..5:67): size => scala/collection/SeqOps#size().
             |[6:6..6:8): pc <= _empty_/t219.pc.
             |[6:10..6:25): PartialFunction => scala/PartialFunction#
             |[6:26..6:29): Int => scala/Int#
             |[6:31..6:34): Int => scala/Int#
             |[6:38..6:60): (x: Int @ann) => x + 1 <= local17
             |[6:39..6:40): x <= local24
             |[6:55..6:56): x => local24
             |[6:57..6:58): + => scala/Int#`+`(+4).
             |[7:6..7:8): pd <= _empty_/t219.pd.
             |[7:10..7:25): PartialFunction => scala/PartialFunction#
             |[7:26..7:29): Int => scala/Int#
             |[7:31..7:34): Int => scala/Int#
             |[7:38..7:48): (_: I) + 1 <= local25
             |[7:39..7:40): _ <= local32
             |[7:42..7:43): I => _empty_/t219.I#
             |[7:45..7:46): + => scala/Int#`+`(+4).
             |[8:6..8:8): pe <= _empty_/t219.pe.
             |[8:10..8:25): PartialFunction => scala/PartialFunction#
             |[8:26..8:30): List => scala/package.List#
             |[8:31..8:34): Int => scala/Int#
             |[8:37..8:40): Int => scala/Int#
             |[8:44..8:62): (_: Seq[Int]).size <= local33
             |[8:45..8:46): _ <= local40
             |[8:48..8:51): Seq => scala/package.Seq#
             |[8:52..8:55): Int => scala/Int#
             |[8:58..8:62): size => scala/collection/SeqOps#size().
             |[9:6..9:8): pf <= _empty_/t219.pf.
             |[9:10..9:25): PartialFunction => scala/PartialFunction#
             |[9:26..9:29): Int => scala/Int#
             |[9:31..9:34): Int => scala/Int#
             |[9:40..9:62): implicit x: I => x + 1 <= local41
             |[9:49..9:50): x <= local48
             |[9:57..9:58): x => local48
             |[9:59..9:60): + => scala/Int#`+`(+4).
             |[10:6..10:8): pg <= _empty_/t219.pg().
             |[10:9..10:10): A <= _empty_/t219.pg().[A]
             |[10:12..10:13): B <= _empty_/t219.pg().[B]
             |[10:17..10:18): A => _empty_/t219.pg().[A]
             |[10:21..10:36): PartialFunction => scala/PartialFunction#
             |[10:37..10:38): B => _empty_/t219.pg().[B]
             |[10:40..10:43): Int => scala/Int#
             |[10:47..10:58): (x: A) => 1 <= local49
             |[10:48..10:49): x <= local56
             |""".stripMargin
      },
      """|[0:7..0:11): t219 <= _empty_/t219.
         |[1:7..1:8): I <= _empty_/t219.I#
         |[1:11..1:14): Int => scala/Int#
         |[2:8..2:11): ann <= _empty_/t219.ann#
         |[2:12..2:12):  <= _empty_/t219.ann#`<init>`().
         |[2:20..2:25): scala => scala/
         |[2:26..2:36): annotation => scala/annotation/
         |[2:37..2:53): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[2:53..2:53):  => scala/annotation/Annotation#`<init>`().
         |[3:6..3:7): f <= _empty_/t219.f.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:24): x <= local0
         |[3:26..3:27): I => _empty_/t219.I#
         |[3:32..3:33): x => local0
         |[3:34..3:35): + => scala/Int#`+`(+4).
         |[4:6..4:8): pa <= _empty_/t219.pa.
         |[4:10..4:25): PartialFunction => scala/PartialFunction#
         |[4:26..4:29): Int => scala/Int#
         |[4:31..4:34): Int => scala/Int#
         |[4:38..4:53): (x: I) => x + 1 <= local1
         |[4:39..4:40): x <= local8
         |[4:42..4:43): I => _empty_/t219.I#
         |[4:48..4:49): x => local8
         |[4:50..4:51): + => scala/Int#`+`(+4).
         |[5:6..5:8): pb <= _empty_/t219.pb.
         |[5:10..5:25): PartialFunction => scala/PartialFunction#
         |[5:26..5:30): List => scala/package.List#
         |[5:31..5:34): Int => scala/Int#
         |[5:37..5:40): Int => scala/Int#
         |[5:44..5:67): (x: Seq[Int]) => x.size <= local9
         |[5:45..5:46): x <= local16
         |[5:48..5:51): Seq => scala/package.Seq#
         |[5:52..5:55): Int => scala/Int#
         |[5:61..5:62): x => local16
         |[5:63..5:67): size => scala/collection/SeqOps#size().
         |[6:6..6:8): pc <= _empty_/t219.pc.
         |[6:10..6:25): PartialFunction => scala/PartialFunction#
         |[6:26..6:29): Int => scala/Int#
         |[6:31..6:34): Int => scala/Int#
         |[6:38..6:60): (x: Int @ann) => x + 1 <= local17
         |[6:39..6:40): x <= local24
         |[6:42..6:45): Int => scala/Int#
         |[6:47..6:50): ann => _empty_/t219.ann#
         |[6:50..6:50):  => _empty_/t219.ann#`<init>`().
         |[6:55..6:56): x => local24
         |[6:57..6:58): + => scala/Int#`+`(+4).
         |[7:6..7:8): pd <= _empty_/t219.pd.
         |[7:10..7:25): PartialFunction => scala/PartialFunction#
         |[7:26..7:29): Int => scala/Int#
         |[7:31..7:34): Int => scala/Int#
         |[7:38..7:48): (_: I) + 1 <= local25
         |[7:39..7:40): _ <= local32
         |[7:42..7:43): I => _empty_/t219.I#
         |[7:45..7:46): + => scala/Int#`+`(+4).
         |[8:6..8:8): pe <= _empty_/t219.pe.
         |[8:10..8:25): PartialFunction => scala/PartialFunction#
         |[8:26..8:30): List => scala/package.List#
         |[8:31..8:34): Int => scala/Int#
         |[8:37..8:40): Int => scala/Int#
         |[8:44..8:62): (_: Seq[Int]).size <= local33
         |[8:45..8:46): _ <= local40
         |[8:48..8:51): Seq => scala/package.Seq#
         |[8:52..8:55): Int => scala/Int#
         |[8:58..8:62): size => scala/collection/SeqOps#size().
         |[9:6..9:8): pf <= _empty_/t219.pf.
         |[9:10..9:25): PartialFunction => scala/PartialFunction#
         |[9:26..9:29): Int => scala/Int#
         |[9:31..9:34): Int => scala/Int#
         |[9:40..9:62): implicit x: I => x + 1 <= local41
         |[9:49..9:50): x <= local48
         |[9:52..9:53): I => _empty_/t219.I#
         |[9:57..9:58): x => local48
         |[9:59..9:60): + => scala/Int#`+`(+4).
         |[10:6..10:8): pg <= _empty_/t219.pg().
         |[10:9..10:10): A <= _empty_/t219.pg().[A]
         |[10:12..10:13): B <= _empty_/t219.pg().[B]
         |[10:17..10:18): A => _empty_/t219.pg().[A]
         |[10:21..10:36): PartialFunction => scala/PartialFunction#
         |[10:37..10:38): B => _empty_/t219.pg().[B]
         |[10:40..10:43): Int => scala/Int#
         |[10:47..10:58): (x: A) => 1 <= local49
         |[10:48..10:49): x <= local56
         |[10:51..10:52): A => _empty_/t219.pg().[A]
         |""".stripMargin,
    ),
  )

  symbols(
    "typed parameters of plain lambdas with a match",
    """|object t220 {
       |  type I = Int
       |  class ann extends scala.annotation.StaticAnnotation
       |  val a: Int => Int = (x: I) => x match { case w => w }
       |  val b: List[Int] => Int = (x: Seq[Int]) => x match { case w => w.size }
       |  val c: Int => Int = (x: Int @ann) => x match { case w => w }
       |  val d: Int => Int = (_: I) match { case w => w }
       |}
       |""".stripMargin,
    """|_empty_/t220. => final object t220 extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t220.I# => type I = Int
       |  Int => scala/Int#
       |_empty_/t220.a. => val method a: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t220.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
       |  Annotation => scala/annotation/Annotation#
       |  StaticAnnotation => scala/annotation/StaticAnnotation#
       |_empty_/t220.ann#`<init>`(). => primary ctor <init>()
       |_empty_/t220.b. => val method b: Function1[List[Int], Int]
       |  Function1 => scala/Function1#
       |  List => scala/package.List#
       |  Int => scala/Int#
       |_empty_/t220.c. => val method c: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t220.d. => val method d: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |local0 => param x: I
       |  I => _empty_/t220.I#
       |local1 => val local w: I
       |  I => _empty_/t220.I#
       |local2 => param x: Seq[Int]
       |  Seq => scala/package.Seq#
       |  Int => scala/Int#
       |local3 => val local w: Seq[Int]
       |  Seq => scala/package.Seq#
       |  Int => scala/Int#
       |local4 => param x: Int @ann
       |  Int => scala/Int#
       |  ann => _empty_/t220.ann#
       |local5 => val local w: Int
       |  Int => scala/Int#
       |local6 => synthetic param x$1: I
       |  I => _empty_/t220.I#
       |local7 => val local w: I
       |  I => _empty_/t220.I#
       |""".stripMargin,
  )

  occurrences(
    "typed parameters of plain lambdas with a match",
    """|object t220 {
       |  type I = Int
       |  class ann extends scala.annotation.StaticAnnotation
       |  val a: Int => Int = (x: I) => x match { case w => w }
       |  val b: List[Int] => Int = (x: Seq[Int]) => x match { case w => w.size }
       |  val c: Int => Int = (x: Int @ann) => x match { case w => w }
       |  val d: Int => Int = (_: I) match { case w => w }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): t220 <= _empty_/t220.
         |[1:7..1:8): I <= _empty_/t220.I#
         |[1:11..1:14): Int => scala/Int#
         |[2:8..2:11): ann <= _empty_/t220.ann#
         |[2:12..2:12):  <= _empty_/t220.ann#`<init>`().
         |[2:20..2:25): scala => scala/
         |[2:26..2:36): annotation => scala/annotation/
         |[2:37..2:53): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[2:53..2:53):  => scala/annotation/Annotation#`<init>`().
         |[3:6..3:7): a <= _empty_/t220.a.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:24): x <= local0
         |[3:26..3:27): I => _empty_/t220.I#
         |[3:32..3:33): x => local0
         |[3:47..3:48): w <= local1
         |[3:52..3:53): w => local1
         |[4:6..4:7): b <= _empty_/t220.b.
         |[4:9..4:13): List => scala/package.List#
         |[4:14..4:17): Int => scala/Int#
         |[4:22..4:25): Int => scala/Int#
         |[4:29..4:30): x <= local2
         |[4:32..4:35): Seq => scala/package.Seq#
         |[4:36..4:39): Int => scala/Int#
         |[4:45..4:46): x => local2
         |[4:60..4:61): w <= local3
         |[4:65..4:66): w => local3
         |[4:67..4:71): size => scala/collection/SeqLike#size().
         |[5:6..5:7): c <= _empty_/t220.c.
         |[5:9..5:12): Int => scala/Int#
         |[5:16..5:19): Int => scala/Int#
         |[5:23..5:24): x <= local4
         |[5:26..5:29): Int => scala/Int#
         |[5:31..5:34): ann => _empty_/t220.ann#
         |[5:34..5:34):  => _empty_/t220.ann#`<init>`().
         |[5:39..5:40): x => local4
         |[5:54..5:55): w <= local5
         |[5:59..5:60): w => local5
         |[6:6..6:7): d <= _empty_/t220.d.
         |[6:9..6:12): Int => scala/Int#
         |[6:16..6:19): Int => scala/Int#
         |[6:23..6:24): _ <= local6
         |[6:26..6:27): I => _empty_/t220.I#
         |[6:42..6:43): w <= local7
         |[6:47..6:48): w => local7
         |""".stripMargin
    else
      """|[0:7..0:11): t220 <= _empty_/t220.
         |[1:7..1:8): I <= _empty_/t220.I#
         |[1:11..1:14): Int => scala/Int#
         |[2:8..2:11): ann <= _empty_/t220.ann#
         |[2:12..2:12):  <= _empty_/t220.ann#`<init>`().
         |[2:20..2:25): scala => scala/
         |[2:26..2:36): annotation => scala/annotation/
         |[2:37..2:53): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[2:53..2:53):  => scala/annotation/Annotation#`<init>`().
         |[3:6..3:7): a <= _empty_/t220.a.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:24): x <= local0
         |[3:26..3:27): I => _empty_/t220.I#
         |[3:32..3:33): x => local0
         |[3:47..3:48): w <= local1
         |[3:52..3:53): w => local1
         |[4:6..4:7): b <= _empty_/t220.b.
         |[4:9..4:13): List => scala/package.List#
         |[4:14..4:17): Int => scala/Int#
         |[4:22..4:25): Int => scala/Int#
         |[4:29..4:30): x <= local2
         |[4:32..4:35): Seq => scala/package.Seq#
         |[4:36..4:39): Int => scala/Int#
         |[4:45..4:46): x => local2
         |[4:60..4:61): w <= local3
         |[4:65..4:66): w => local3
         |[4:67..4:71): size => scala/collection/SeqOps#size().
         |[5:6..5:7): c <= _empty_/t220.c.
         |[5:9..5:12): Int => scala/Int#
         |[5:16..5:19): Int => scala/Int#
         |[5:23..5:24): x <= local4
         |[5:26..5:29): Int => scala/Int#
         |[5:31..5:34): ann => _empty_/t220.ann#
         |[5:34..5:34):  => _empty_/t220.ann#`<init>`().
         |[5:39..5:40): x => local4
         |[5:54..5:55): w <= local5
         |[5:59..5:60): w => local5
         |[6:6..6:7): d <= _empty_/t220.d.
         |[6:9..6:12): Int => scala/Int#
         |[6:16..6:19): Int => scala/Int#
         |[6:23..6:24): _ <= local6
         |[6:26..6:27): I => _empty_/t220.I#
         |[6:42..6:43): w <= local7
         |[6:47..6:48): w => local7
         |""".stripMargin,
  )

  symbols(
    "typed parameters of partial function lambdas with a match, in braces, with a path or a type parameter",
    """|object t221 {
       |  type I = Int
       |  type L[A] = List[A]
       |  val a: PartialFunction[List[Int], Int] = (x: L[Int]) => x match { case w => w.size }
       |  val b: PartialFunction[Int, Int] = { x: I => x match { case w => w } }
       |  val c: PartialFunction[Int, Int] = { implicit x: I => x match { case w => w } }
       |  val d: PartialFunction[List[Int], Int] = (_: Seq[Int]) match { case w => w.size }
       |  val e: PartialFunction[List[Int], Int] =
       |    (x: scala.collection.Seq[Int]) => x match { case w => w.size }
       |  val f: PartialFunction[List[Int], Int] = (x: List[_]) => x match { case w => w.size }
       |  val g: PartialFunction[Int => Int, Int] = (x: Int => Int) => x match { case w => w(1) }
       |  val h: PartialFunction[(Int, Int), Int] = (x: (Int, Int)) => x match { case w => w._1 }
       |  def k[A, B <: A]: PartialFunction[B, Int] = (x: A) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t221. => final object t221 extends AnyRef { +11 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t221.I# => type I = Int
         |  Int => scala/Int#
         |_empty_/t221.L# => type L[A] = List[A]
         |  A => _empty_/t221.L#[A]
         |  List => scala/package.List#
         |_empty_/t221.L#[A] => typeparam A
         |_empty_/t221.a. => val method a: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t221.b. => val method b: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t221.c. => val method c: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t221.d. => val method d: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t221.e. => val method e: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t221.f. => val method f: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t221.g. => val method g: PartialFunction[Function1[Int, Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t221.h. => val method h: PartialFunction[Tuple2[Int, Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t221.k(). => method k[A, B <: A]: PartialFunction[B, Int]
         |  A => _empty_/t221.k().[A]
         |  B => _empty_/t221.k().[B]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t221.k().[A] => typeparam A
         |_empty_/t221.k().[B] => typeparam B <: A
         |  A => _empty_/t221.k().[A]
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  B1 => local5
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  List => scala/package.List#
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
         |local6 => typeparam A1 <: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local7 => param x: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local8 => val local w: A1
         |  A1 => local6
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local16
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => param x: Int
         |  Int => scala/Int#
         |local17 => val local w: A1
         |  A1 => local15
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local19 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  Int => scala/Int#
         |  B1 => local23
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local25 => implicit param x: Int
         |  Int => scala/Int#
         |local26 => val local w: A1
         |  A1 => local24
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local28 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  B1 => local32
         |  x$1 => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(x$1: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local34
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param x$1: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local34 => synthetic param x$1: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local35 => val local w: Seq[Int]
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local37 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local42
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  B1 => local41
         |  x => local39
         |  default => local40
         |  Function1 => scala/Function1#
         |local38 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local43
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local39 => synthetic param x: A1
         |  A1 => local42
         |local40 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local42
         |  B1 => local41
         |local41 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local42 => typeparam A1 <: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local43 => param x: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local44 => val local w: A1
         |  A1 => local42
         |local45 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local46 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local51
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  B1 => local50
         |  x => local48
         |  default => local49
         |  Function1 => scala/Function1#
         |local47 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local52
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local48 => synthetic param x: A1
         |  A1 => local51
         |local49 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local51
         |  B1 => local50
         |local50 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local51 => typeparam A1 <: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local52 => param x: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local53 => val local w: A1
         |  A1 => local51
         |local54 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Function1[Int, Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local55 => final method applyOrElse[A1 <: Function1[Int, Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local60
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  B1 => local59
         |  x => local57
         |  default => local58
         |local56 => final method isDefinedAt(x: Function1[Int, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local61
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local57 => synthetic param x: A1
         |  A1 => local60
         |local58 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local60
         |  B1 => local59
         |local59 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local60 => typeparam A1 <: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |local61 => param x: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |local62 => val local w: A1
         |  A1 => local60
         |local63 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[Int, Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local64 => final method applyOrElse[A1 <: Tuple2[Int, Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local69
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  B1 => local68
         |  x => local66
         |  default => local67
         |  Function1 => scala/Function1#
         |local65 => final method isDefinedAt(x: Tuple2[Int, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local70
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local66 => synthetic param x: A1
         |  A1 => local69
         |local67 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local69
         |  B1 => local68
         |local68 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local69 => typeparam A1 <: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local70 => param x: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local71 => val local w: A1
         |  A1 => local69
         |local72 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t221.k().[B]
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local73 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local78
         |  B => _empty_/t221.k().[B]
         |  B1 => local77
         |  Int => scala/Int#
         |  x => local75
         |  default => local76
         |  Function1 => scala/Function1#
         |local74 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local79
         |  B => _empty_/t221.k().[B]
         |  Boolean => scala/Boolean#
         |local75 => synthetic param x: A1
         |  A1 => local78
         |local76 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local78
         |  B1 => local77
         |local77 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local78 => typeparam A1 <: B
         |  B => _empty_/t221.k().[B]
         |local79 => param x: B
         |  B => _empty_/t221.k().[B]
         |""".stripMargin
    else ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|_empty_/t221. => final object t221 extends AnyRef { +11 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t221.I# => type I = Int
             |  Int => scala/Int#
             |_empty_/t221.L# => type L[A] = List[A]
             |  A => _empty_/t221.L#[A]
             |  List => scala/package.List#
             |_empty_/t221.L#[A] => typeparam A
             |_empty_/t221.a. => val method a: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t221.b. => val method b: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t221.c. => val method c: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t221.d. => val method d: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t221.e. => val method e: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t221.f. => val method f: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t221.g. => val method g: PartialFunction[Function1[Int, Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |_empty_/t221.h. => val method h: PartialFunction[Tuple2[Int, Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  Tuple2 => scala/Tuple2#
             |  Int => scala/Int#
             |_empty_/t221.k(). => method k[A, B <: A]: PartialFunction[B, Int]
             |  A => _empty_/t221.k().[A]
             |  B => _empty_/t221.k().[B]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t221.k().[A] => typeparam A
             |_empty_/t221.k().[B] => typeparam B <: A
             |  A => _empty_/t221.k().[A]
             |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local1 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local6
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local5
             |  x => local3
             |  default => local4
             |  Function1 => scala/Function1#
             |local2 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local7
             |  List => scala/package.List#
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
             |local6 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local7 => param x: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local8 => val local w: A1
             |  A1 => local6
             |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local15
             |  Int => scala/Int#
             |  B1 => local14
             |  x => local12
             |  default => local13
             |  Function1 => scala/Function1#
             |local11 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local16
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local12 => synthetic param x: A1
             |  A1 => local15
             |local13 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local15
             |  B1 => local14
             |local14 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local15 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local16 => param x: Int
             |  Int => scala/Int#
             |local17 => val local w: A1
             |  A1 => local15
             |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local19 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local24
             |  Int => scala/Int#
             |  B1 => local23
             |  x => local21
             |  default => local22
             |  Function1 => scala/Function1#
             |local20 => final method isDefinedAt(implicit x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local25
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local21 => synthetic param x: A1
             |  A1 => local24
             |local22 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local24
             |  B1 => local23
             |local23 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local24 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local25 => implicit param x: Int
             |  Int => scala/Int#
             |local26 => val local w: A1
             |  A1 => local24
             |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local28 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local33
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local32
             |  x$1 => local30
             |  default => local31
             |  Function1 => scala/Function1#
             |local29 => final method isDefinedAt(x$1: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x$1 => local34
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local30 => synthetic param x$1: A1
             |  A1 => local33
             |local31 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local33
             |  B1 => local32
             |local32 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local33 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local34 => synthetic param x$1: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local35 => val local w: Seq[Int]
             |  Seq => scala/package.Seq#
             |  Int => scala/Int#
             |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local37 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local42
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local41
             |  x => local39
             |  default => local40
             |  Function1 => scala/Function1#
             |local38 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local43
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local39 => synthetic param x: A1
             |  A1 => local42
             |local40 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local42
             |  B1 => local41
             |local41 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local42 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local43 => param x: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local44 => val local w: A1
             |  A1 => local42
             |local45 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local46 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local51
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local50
             |  x => local48
             |  default => local49
             |  Function1 => scala/Function1#
             |local47 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local52
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local48 => synthetic param x: A1
             |  A1 => local51
             |local49 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local51
             |  B1 => local50
             |local50 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local51 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local52 => param x: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local53 => val local w: A1
             |  A1 => local51
             |local54 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Function1[Int, Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local55 => final method applyOrElse[A1 <: Function1[Int, Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local60
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |  B1 => local59
             |  x => local57
             |  default => local58
             |local56 => final method isDefinedAt(x: Function1[Int, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local61
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local57 => synthetic param x: A1
             |  A1 => local60
             |local58 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local60
             |  B1 => local59
             |local59 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local60 => typeparam A1 <: Function1[Int, Int]
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |local61 => param x: Function1[Int, Int]
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |local62 => val local w: A1
             |  A1 => local60
             |local63 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[Int, Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Tuple2 => scala/Tuple2#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local64 => final method applyOrElse[A1 <: Tuple2[Int, Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local69
             |  Tuple2 => scala/Tuple2#
             |  Int => scala/Int#
             |  B1 => local68
             |  x => local66
             |  default => local67
             |  Function1 => scala/Function1#
             |local65 => final method isDefinedAt(x: Tuple2[Int, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local70
             |  Tuple2 => scala/Tuple2#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local66 => synthetic param x: A1
             |  A1 => local69
             |local67 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local69
             |  B1 => local68
             |local68 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local69 => typeparam A1 <: Tuple2[Int, Int]
             |  Tuple2 => scala/Tuple2#
             |  Int => scala/Int#
             |local70 => param x: Tuple2[Int, Int]
             |  Tuple2 => scala/Tuple2#
             |  Int => scala/Int#
             |local71 => val local w: A1
             |  A1 => local69
             |local72 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  B => _empty_/t221.k().[B]
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local73 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local78
             |  B => _empty_/t221.k().[B]
             |  B1 => local77
             |  Int => scala/Int#
             |  x => local75
             |  default => local76
             |  Function1 => scala/Function1#
             |local74 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local79
             |  B => _empty_/t221.k().[B]
             |  Boolean => scala/Boolean#
             |local75 => synthetic param x: A1
             |  A1 => local78
             |local76 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local78
             |  B1 => local77
             |local77 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local78 => typeparam A1 <: B
             |  B => _empty_/t221.k().[B]
             |local79 => param x: B
             |  B => _empty_/t221.k().[B]
             |""".stripMargin
      },
      """|_empty_/t221. => final object t221 extends AnyRef { +11 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t221.I# => type I = Int
         |  Int => scala/Int#
         |_empty_/t221.L# => type L[A] = List[A]
         |  A => _empty_/t221.L#[A]
         |  List => scala/package.List#
         |_empty_/t221.L#[A] => typeparam A
         |_empty_/t221.a. => val method a: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t221.b. => val method b: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t221.c. => val method c: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t221.d. => val method d: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t221.e. => val method e: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t221.f. => val method f: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t221.g. => val method g: PartialFunction[Function1[Int, Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t221.h. => val method h: PartialFunction[Tuple2[Int, Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t221.k(). => method k[A, B <: A]: PartialFunction[B, Int]
         |  A => _empty_/t221.k().[A]
         |  B => _empty_/t221.k().[B]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t221.k().[A] => typeparam A
         |_empty_/t221.k().[B] => typeparam B <: A
         |  A => _empty_/t221.k().[A]
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[L[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  L => _empty_/t221.L#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: L[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  L => _empty_/t221.L#
         |  Int => scala/Int#
         |  B1 => local5
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: L[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  L => _empty_/t221.L#
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
         |local6 => typeparam A1 <: L[Int]
         |  L => _empty_/t221.L#
         |  Int => scala/Int#
         |local7 => param x: L[Int]
         |  L => _empty_/t221.L#
         |  Int => scala/Int#
         |local8 => val local w: A1
         |  A1 => local6
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[I, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  I => _empty_/t221.I#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: I, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  I => _empty_/t221.I#
         |  B1 => local14
         |  Int => scala/Int#
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: I): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local16
         |  I => _empty_/t221.I#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: I
         |  I => _empty_/t221.I#
         |local16 => param x: I
         |  I => _empty_/t221.I#
         |local17 => val local w: A1
         |  A1 => local15
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[I, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  I => _empty_/t221.I#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local19 => final method applyOrElse[A1 <: I, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  I => _empty_/t221.I#
         |  B1 => local23
         |  Int => scala/Int#
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(implicit x: I): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  I => _empty_/t221.I#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: I
         |  I => _empty_/t221.I#
         |local25 => implicit param x: I
         |  I => _empty_/t221.I#
         |local26 => val local w: A1
         |  A1 => local24
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Seq[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local28 => final method applyOrElse[A1 <: Seq[Int], B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  B1 => local32
         |  x$1 => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(x$1: Seq[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local34
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param x$1: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: Seq[Int]
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |local34 => synthetic param x$1: Seq[Int]
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |local35 => val local w: Seq[Int]
         |  Seq => scala/package.Seq#
         |  Int => scala/Int#
         |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Seq[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local37 => final method applyOrElse[A1 <: Seq[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local42
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |  B1 => local41
         |  x => local39
         |  default => local40
         |  Function1 => scala/Function1#
         |local38 => final method isDefinedAt(x: Seq[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local43
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local39 => synthetic param x: A1
         |  A1 => local42
         |local40 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local42
         |  B1 => local41
         |local41 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local42 => typeparam A1 <: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local43 => param x: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local44 => val local w: A1
         |  A1 => local42
         |local45 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  _ => local46
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local46 => abstract type _
         |local47 => final method applyOrElse[A1 <: List[_] forSome { type _ }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local52
         |  List => scala/package.List#
         |  _ => local46
         |  B1 => local51
         |  Int => scala/Int#
         |  x => local49
         |  default => local50
         |  Function1 => scala/Function1#
         |local48 => final method isDefinedAt(x: List[_] forSome { type _ }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local53
         |  List => scala/package.List#
         |  _ => local46
         |  Boolean => scala/Boolean#
         |local49 => synthetic param x: A1
         |  A1 => local52
         |local50 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local52
         |  B1 => local51
         |local51 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local52 => typeparam A1 <: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local46
         |local53 => param x: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local46
         |local54 => val local w: A1
         |  A1 => local52
         |local55 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Function1[Int, Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local56 => final method applyOrElse[A1 <: Function1[Int, Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local61
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  B1 => local60
         |  x => local58
         |  default => local59
         |local57 => final method isDefinedAt(x: Function1[Int, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local62
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local58 => synthetic param x: A1
         |  A1 => local61
         |local59 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local61
         |  B1 => local60
         |local60 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local61 => typeparam A1 <: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |local62 => param x: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |local63 => val local w: A1
         |  A1 => local61
         |local64 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[Int, Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local65 => final method applyOrElse[A1 <: Tuple2[Int, Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local70
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  B1 => local69
         |  x => local67
         |  default => local68
         |  Function1 => scala/Function1#
         |local66 => final method isDefinedAt(x: Tuple2[Int, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local71
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local67 => synthetic param x: A1
         |  A1 => local70
         |local68 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local70
         |  B1 => local69
         |local69 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local70 => typeparam A1 <: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local71 => param x: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local72 => val local w: A1
         |  A1 => local70
         |local73 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[A, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  A => _empty_/t221.k().[A]
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local74 => final method applyOrElse[A1 <: A, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local79
         |  A => _empty_/t221.k().[A]
         |  B1 => local78
         |  Int => scala/Int#
         |  x => local76
         |  default => local77
         |  Function1 => scala/Function1#
         |local75 => final method isDefinedAt(x: A): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local80
         |  A => _empty_/t221.k().[A]
         |  Boolean => scala/Boolean#
         |local76 => synthetic param x: A1
         |  A1 => local79
         |local77 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local79
         |  B1 => local78
         |local78 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local79 => typeparam A1 <: A
         |  A => _empty_/t221.k().[A]
         |local80 => param x: A
         |  A => _empty_/t221.k().[A]
         |""".stripMargin,
    ),
  )

  occurrences(
    "typed parameters of partial function lambdas with a match, in braces, with a path or a type parameter",
    """|object t221 {
       |  type I = Int
       |  type L[A] = List[A]
       |  val a: PartialFunction[List[Int], Int] = (x: L[Int]) => x match { case w => w.size }
       |  val b: PartialFunction[Int, Int] = { x: I => x match { case w => w } }
       |  val c: PartialFunction[Int, Int] = { implicit x: I => x match { case w => w } }
       |  val d: PartialFunction[List[Int], Int] = (_: Seq[Int]) match { case w => w.size }
       |  val e: PartialFunction[List[Int], Int] =
       |    (x: scala.collection.Seq[Int]) => x match { case w => w.size }
       |  val f: PartialFunction[List[Int], Int] = (x: List[_]) => x match { case w => w.size }
       |  val g: PartialFunction[Int => Int, Int] = (x: Int => Int) => x match { case w => w(1) }
       |  val h: PartialFunction[(Int, Int), Int] = (x: (Int, Int)) => x match { case w => w._1 }
       |  def k[A, B <: A]: PartialFunction[B, Int] = (x: A) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): t221 <= _empty_/t221.
         |[1:7..1:8): I <= _empty_/t221.I#
         |[1:11..1:14): Int => scala/Int#
         |[2:7..2:8): L <= _empty_/t221.L#
         |[2:9..2:10): A <= _empty_/t221.L#[A]
         |[2:14..2:18): List => scala/package.List#
         |[2:19..2:20): A => _empty_/t221.L#[A]
         |[3:6..3:7): a <= _empty_/t221.a.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:29): List => scala/package.List#
         |[3:30..3:33): Int => scala/Int#
         |[3:36..3:39): Int => scala/Int#
         |[3:43..3:86): (x: L[Int]) => x match { case w => w.size } <= local0
         |[3:44..3:45): x <= local7
         |[3:58..3:59): x => local7
         |[3:73..3:74): w <= local8
         |[3:78..3:79): w => local8
         |[3:80..3:84): size => scala/collection/SeqLike#size().
         |[4:6..4:7): b <= _empty_/t221.b.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:28): Int => scala/Int#
         |[4:30..4:33): Int => scala/Int#
         |[4:39..4:40): x <= local16
         |[4:39..4:70): x: I => x match { case w => w } <= local9
         |[4:47..4:48): x => local16
         |[4:62..4:63): w <= local17
         |[4:67..4:68): w => local17
         |[5:6..5:7): c <= _empty_/t221.c.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[5:39..5:79): implicit x: I => x match { case w => w } <= local18
         |[5:48..5:49): x <= local25
         |[5:56..5:57): x => local25
         |[5:71..5:72): w <= local26
         |[5:76..5:77): w => local26
         |[6:6..6:7): d <= _empty_/t221.d.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): List => scala/package.List#
         |[6:30..6:33): Int => scala/Int#
         |[6:36..6:39): Int => scala/Int#
         |[6:43..6:83): (_: Seq[Int]) match { case w => w.size } <= local27
         |[6:44..6:45): _ <= local34
         |[6:47..6:50): Seq => scala/package.Seq#
         |[6:51..6:54): Int => scala/Int#
         |[6:70..6:71): w <= local35
         |[6:75..6:76): w => local35
         |[6:77..6:81): size => scala/collection/SeqLike#size().
         |[7:6..7:7): e <= _empty_/t221.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:29): List => scala/package.List#
         |[7:30..7:33): Int => scala/Int#
         |[7:36..7:39): Int => scala/Int#
         |[8:4..8:66): (x: scala.collection.Seq[Int]) => x match { case w => w.size } <= local36
         |[8:5..8:6): x <= local43
         |[8:38..8:39): x => local43
         |[8:53..8:54): w <= local44
         |[8:58..8:59): w => local44
         |[8:60..8:64): size => scala/collection/SeqLike#size().
         |[9:6..9:7): f <= _empty_/t221.f.
         |[9:9..9:24): PartialFunction => scala/PartialFunction#
         |[9:25..9:29): List => scala/package.List#
         |[9:30..9:33): Int => scala/Int#
         |[9:36..9:39): Int => scala/Int#
         |[9:43..9:87): (x: List[_]) => x match { case w => w.size } <= local45
         |[9:44..9:45): x <= local52
         |[9:59..9:60): x => local52
         |[9:74..9:75): w <= local53
         |[9:79..9:80): w => local53
         |[9:81..9:85): size => scala/collection/SeqLike#size().
         |[10:6..10:7): g <= _empty_/t221.g.
         |[10:9..10:24): PartialFunction => scala/PartialFunction#
         |[10:25..10:28): Int => scala/Int#
         |[10:32..10:35): Int => scala/Int#
         |[10:37..10:40): Int => scala/Int#
         |[10:44..10:89): (x: Int => Int) => x match { case w => w(1) } <= local54
         |[10:45..10:46): x <= local61
         |[10:63..10:64): x => local61
         |[10:78..10:79): w <= local62
         |[10:83..10:84): w => local62
         |[11:6..11:7): h <= _empty_/t221.h.
         |[11:9..11:24): PartialFunction => scala/PartialFunction#
         |[11:26..11:29): Int => scala/Int#
         |[11:31..11:34): Int => scala/Int#
         |[11:37..11:40): Int => scala/Int#
         |[11:44..11:89): (x: (Int, Int)) => x match { case w => w._1 } <= local63
         |[11:45..11:46): x <= local70
         |[11:63..11:64): x => local70
         |[11:78..11:79): w <= local71
         |[11:83..11:84): w => local71
         |[11:85..11:87): _1 => scala/Tuple2#_1.
         |[12:6..12:7): k <= _empty_/t221.k().
         |[12:8..12:9): A <= _empty_/t221.k().[A]
         |[12:11..12:12): B <= _empty_/t221.k().[B]
         |[12:16..12:17): A => _empty_/t221.k().[A]
         |[12:20..12:35): PartialFunction => scala/PartialFunction#
         |[12:36..12:37): B => _empty_/t221.k().[B]
         |[12:39..12:42): Int => scala/Int#
         |[12:46..12:79): (x: A) => x match { case _ => 1 } <= local72
         |[12:47..12:48): x <= local79
         |[12:56..12:57): x => local79
         |""".stripMargin
    else ScalaVersion.getExpected(
      List {
        ScalaVersion.AtMost213(16) ->
          """|[0:7..0:11): t221 <= _empty_/t221.
             |[1:7..1:8): I <= _empty_/t221.I#
             |[1:11..1:14): Int => scala/Int#
             |[2:7..2:8): L <= _empty_/t221.L#
             |[2:9..2:10): A <= _empty_/t221.L#[A]
             |[2:14..2:18): List => scala/package.List#
             |[2:19..2:20): A => _empty_/t221.L#[A]
             |[3:6..3:7): a <= _empty_/t221.a.
             |[3:9..3:24): PartialFunction => scala/PartialFunction#
             |[3:25..3:29): List => scala/package.List#
             |[3:30..3:33): Int => scala/Int#
             |[3:36..3:39): Int => scala/Int#
             |[3:43..3:86): (x: L[Int]) => x match { case w => w.size } <= local0
             |[3:44..3:45): x <= local7
             |[3:58..3:59): x => local7
             |[3:73..3:74): w <= local8
             |[3:78..3:79): w => local8
             |[3:80..3:84): size => scala/collection/SeqOps#size().
             |[4:6..4:7): b <= _empty_/t221.b.
             |[4:9..4:24): PartialFunction => scala/PartialFunction#
             |[4:25..4:28): Int => scala/Int#
             |[4:30..4:33): Int => scala/Int#
             |[4:39..4:40): x <= local16
             |[4:39..4:70): x: I => x match { case w => w } <= local9
             |[4:47..4:48): x => local16
             |[4:62..4:63): w <= local17
             |[4:67..4:68): w => local17
             |[5:6..5:7): c <= _empty_/t221.c.
             |[5:9..5:24): PartialFunction => scala/PartialFunction#
             |[5:25..5:28): Int => scala/Int#
             |[5:30..5:33): Int => scala/Int#
             |[5:39..5:79): implicit x: I => x match { case w => w } <= local18
             |[5:48..5:49): x <= local25
             |[5:56..5:57): x => local25
             |[5:71..5:72): w <= local26
             |[5:76..5:77): w => local26
             |[6:6..6:7): d <= _empty_/t221.d.
             |[6:9..6:24): PartialFunction => scala/PartialFunction#
             |[6:25..6:29): List => scala/package.List#
             |[6:30..6:33): Int => scala/Int#
             |[6:36..6:39): Int => scala/Int#
             |[6:43..6:83): (_: Seq[Int]) match { case w => w.size } <= local27
             |[6:44..6:45): _ <= local34
             |[6:47..6:50): Seq => scala/package.Seq#
             |[6:51..6:54): Int => scala/Int#
             |[6:70..6:71): w <= local35
             |[6:75..6:76): w => local35
             |[6:77..6:81): size => scala/collection/SeqOps#size().
             |[7:6..7:7): e <= _empty_/t221.e.
             |[7:9..7:24): PartialFunction => scala/PartialFunction#
             |[7:25..7:29): List => scala/package.List#
             |[7:30..7:33): Int => scala/Int#
             |[7:36..7:39): Int => scala/Int#
             |[8:4..8:66): (x: scala.collection.Seq[Int]) => x match { case w => w.size } <= local36
             |[8:5..8:6): x <= local43
             |[8:38..8:39): x => local43
             |[8:53..8:54): w <= local44
             |[8:58..8:59): w => local44
             |[8:60..8:64): size => scala/collection/SeqOps#size().
             |[9:6..9:7): f <= _empty_/t221.f.
             |[9:9..9:24): PartialFunction => scala/PartialFunction#
             |[9:25..9:29): List => scala/package.List#
             |[9:30..9:33): Int => scala/Int#
             |[9:36..9:39): Int => scala/Int#
             |[9:43..9:87): (x: List[_]) => x match { case w => w.size } <= local45
             |[9:44..9:45): x <= local52
             |[9:59..9:60): x => local52
             |[9:74..9:75): w <= local53
             |[9:79..9:80): w => local53
             |[9:81..9:85): size => scala/collection/SeqOps#size().
             |[10:6..10:7): g <= _empty_/t221.g.
             |[10:9..10:24): PartialFunction => scala/PartialFunction#
             |[10:25..10:28): Int => scala/Int#
             |[10:32..10:35): Int => scala/Int#
             |[10:37..10:40): Int => scala/Int#
             |[10:44..10:89): (x: Int => Int) => x match { case w => w(1) } <= local54
             |[10:45..10:46): x <= local61
             |[10:63..10:64): x => local61
             |[10:78..10:79): w <= local62
             |[10:83..10:84): w => local62
             |[11:6..11:7): h <= _empty_/t221.h.
             |[11:9..11:24): PartialFunction => scala/PartialFunction#
             |[11:26..11:29): Int => scala/Int#
             |[11:31..11:34): Int => scala/Int#
             |[11:37..11:40): Int => scala/Int#
             |[11:44..11:89): (x: (Int, Int)) => x match { case w => w._1 } <= local63
             |[11:45..11:46): x <= local70
             |[11:63..11:64): x => local70
             |[11:78..11:79): w <= local71
             |[11:83..11:84): w => local71
             |[11:85..11:87): _1 => scala/Tuple2#_1.
             |[12:6..12:7): k <= _empty_/t221.k().
             |[12:8..12:9): A <= _empty_/t221.k().[A]
             |[12:11..12:12): B <= _empty_/t221.k().[B]
             |[12:16..12:17): A => _empty_/t221.k().[A]
             |[12:20..12:35): PartialFunction => scala/PartialFunction#
             |[12:36..12:37): B => _empty_/t221.k().[B]
             |[12:39..12:42): Int => scala/Int#
             |[12:46..12:79): (x: A) => x match { case _ => 1 } <= local72
             |[12:47..12:48): x <= local79
             |[12:56..12:57): x => local79
             |""".stripMargin
      },
      """|[0:7..0:11): t221 <= _empty_/t221.
         |[1:7..1:8): I <= _empty_/t221.I#
         |[1:11..1:14): Int => scala/Int#
         |[2:7..2:8): L <= _empty_/t221.L#
         |[2:9..2:10): A <= _empty_/t221.L#[A]
         |[2:14..2:18): List => scala/package.List#
         |[2:19..2:20): A => _empty_/t221.L#[A]
         |[3:6..3:7): a <= _empty_/t221.a.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:29): List => scala/package.List#
         |[3:30..3:33): Int => scala/Int#
         |[3:36..3:39): Int => scala/Int#
         |[3:43..3:86): (x: L[Int]) => x match { case w => w.size } <= local0
         |[3:44..3:45): x <= local7
         |[3:47..3:48): L => _empty_/t221.L#
         |[3:49..3:52): Int => scala/Int#
         |[3:58..3:59): x => local7
         |[3:73..3:74): w <= local8
         |[3:78..3:79): w => local8
         |[3:80..3:84): size => scala/collection/SeqOps#size().
         |[4:6..4:7): b <= _empty_/t221.b.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:28): Int => scala/Int#
         |[4:30..4:33): Int => scala/Int#
         |[4:39..4:40): x <= local16
         |[4:39..4:70): x: I => x match { case w => w } <= local9
         |[4:42..4:43): I => _empty_/t221.I#
         |[4:47..4:48): x => local16
         |[4:62..4:63): w <= local17
         |[4:67..4:68): w => local17
         |[5:6..5:7): c <= _empty_/t221.c.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[5:39..5:79): implicit x: I => x match { case w => w } <= local18
         |[5:48..5:49): x <= local25
         |[5:51..5:52): I => _empty_/t221.I#
         |[5:56..5:57): x => local25
         |[5:71..5:72): w <= local26
         |[5:76..5:77): w => local26
         |[6:6..6:7): d <= _empty_/t221.d.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): List => scala/package.List#
         |[6:30..6:33): Int => scala/Int#
         |[6:36..6:39): Int => scala/Int#
         |[6:43..6:83): (_: Seq[Int]) match { case w => w.size } <= local27
         |[6:44..6:45): _ <= local34
         |[6:47..6:50): Seq => scala/package.Seq#
         |[6:51..6:54): Int => scala/Int#
         |[6:70..6:71): w <= local35
         |[6:75..6:76): w => local35
         |[6:77..6:81): size => scala/collection/SeqOps#size().
         |[7:6..7:7): e <= _empty_/t221.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:29): List => scala/package.List#
         |[7:30..7:33): Int => scala/Int#
         |[7:36..7:39): Int => scala/Int#
         |[8:4..8:66): (x: scala.collection.Seq[Int]) => x match { case w => w.size } <= local36
         |[8:5..8:6): x <= local43
         |[8:8..8:13): scala => scala/
         |[8:14..8:24): collection => scala/collection/
         |[8:25..8:28): Seq => scala/collection/Seq#
         |[8:29..8:32): Int => scala/Int#
         |[8:38..8:39): x => local43
         |[8:53..8:54): w <= local44
         |[8:58..8:59): w => local44
         |[8:60..8:64): size => scala/collection/SeqOps#size().
         |[9:6..9:7): f <= _empty_/t221.f.
         |[9:9..9:24): PartialFunction => scala/PartialFunction#
         |[9:25..9:29): List => scala/package.List#
         |[9:30..9:33): Int => scala/Int#
         |[9:36..9:39): Int => scala/Int#
         |[9:43..9:87): (x: List[_]) => x match { case w => w.size } <= local45
         |[9:44..9:45): x <= local53
         |[9:47..9:51): List => scala/package.List#
         |[9:52..9:53): _ <= local46
         |[9:59..9:60): x => local53
         |[9:74..9:75): w <= local54
         |[9:79..9:80): w => local54
         |[9:81..9:85): size => scala/collection/SeqOps#size().
         |[10:6..10:7): g <= _empty_/t221.g.
         |[10:9..10:24): PartialFunction => scala/PartialFunction#
         |[10:25..10:28): Int => scala/Int#
         |[10:32..10:35): Int => scala/Int#
         |[10:37..10:40): Int => scala/Int#
         |[10:44..10:89): (x: Int => Int) => x match { case w => w(1) } <= local55
         |[10:45..10:46): x <= local62
         |[10:48..10:51): Int => scala/Int#
         |[10:55..10:58): Int => scala/Int#
         |[10:63..10:64): x => local62
         |[10:78..10:79): w <= local63
         |[10:83..10:84): w => local63
         |[11:6..11:7): h <= _empty_/t221.h.
         |[11:9..11:24): PartialFunction => scala/PartialFunction#
         |[11:26..11:29): Int => scala/Int#
         |[11:31..11:34): Int => scala/Int#
         |[11:37..11:40): Int => scala/Int#
         |[11:44..11:89): (x: (Int, Int)) => x match { case w => w._1 } <= local64
         |[11:45..11:46): x <= local71
         |[11:49..11:52): Int => scala/Int#
         |[11:54..11:57): Int => scala/Int#
         |[11:63..11:64): x => local71
         |[11:78..11:79): w <= local72
         |[11:83..11:84): w => local72
         |[11:85..11:87): _1 => scala/Tuple2#_1.
         |[12:6..12:7): k <= _empty_/t221.k().
         |[12:8..12:9): A <= _empty_/t221.k().[A]
         |[12:11..12:12): B <= _empty_/t221.k().[B]
         |[12:16..12:17): A => _empty_/t221.k().[A]
         |[12:20..12:35): PartialFunction => scala/PartialFunction#
         |[12:36..12:37): B => _empty_/t221.k().[B]
         |[12:39..12:42): Int => scala/Int#
         |[12:46..12:79): (x: A) => x match { case _ => 1 } <= local73
         |[12:47..12:48): x <= local80
         |[12:50..12:51): A => _empty_/t221.k().[A]
         |[12:56..12:57): x => local80
         |""".stripMargin,
    ),
  )

  symbols(
    "wildcard, placeholder, nested and argument plain lambdas with a match",
    """|object t223 {
       |  val a: Int => Int = _ => 1 match { case w => w }
       |  val b: Int => String = _.toString match { case s => s }
       |  val c: Int => Int => Int =
       |    x => x match { case v => y => y match { case w => x + v + w } }
       |  val d: Int => Int => Int =
       |    _ match { case v => _ match { case w => v + w } }
       |  val e = List(1).map(x => x match { case w => w })
       |  val f = List(1).map { _ match { case w => w } }
       |}
       |""".stripMargin,
    """|_empty_/t223. => final object t223 extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t223.a. => val method a: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t223.b. => val method b: Function1[Int, String]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |_empty_/t223.c. => val method c: Function1[Int, Function1[Int, Int]]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t223.d. => val method d: Function1[Int, Function1[Int, Int]]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t223.e. => val method e: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t223.f. => val method f: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => param x$1: Int
       |  Int => scala/Int#
       |local1 => val local w: Int
       |  Int => scala/Int#
       |local2 => synthetic param x$2: Int
       |  Int => scala/Int#
       |local3 => val local s: String
       |  String => java/lang/String#
       |local4 => param x: Int
       |  Int => scala/Int#
       |local5 => val local v: Int
       |  Int => scala/Int#
       |local6 => param y: Int
       |  Int => scala/Int#
       |local7 => val local w: Int
       |  Int => scala/Int#
       |local8 => synthetic param x$3: Int
       |  Int => scala/Int#
       |local9 => val local v: Int
       |  Int => scala/Int#
       |local10 => synthetic param x$4: Int
       |  Int => scala/Int#
       |local11 => val local w: Int
       |  Int => scala/Int#
       |local12 => param x: Int
       |  Int => scala/Int#
       |local13 => val local w: Int
       |  Int => scala/Int#
       |local14 => synthetic param x$5: Int
       |  Int => scala/Int#
       |local15 => val local w: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "wildcard, placeholder, nested and argument plain lambdas with a match",
    """|object t223 {
       |  val a: Int => Int = _ => 1 match { case w => w }
       |  val b: Int => String = _.toString match { case s => s }
       |  val c: Int => Int => Int =
       |    x => x match { case v => y => y match { case w => x + v + w } }
       |  val d: Int => Int => Int =
       |    _ match { case v => _ match { case w => v + w } }
       |  val e = List(1).map(x => x match { case w => w })
       |  val f = List(1).map { _ match { case w => w } }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): t223 <= _empty_/t223.
         |[1:6..1:7): a <= _empty_/t223.a.
         |[1:9..1:12): Int => scala/Int#
         |[1:16..1:19): Int => scala/Int#
         |[1:22..1:23): _ <= local0
         |[1:42..1:43): w <= local1
         |[1:47..1:48): w => local1
         |[2:6..2:7): b <= _empty_/t223.b.
         |[2:9..2:12): Int => scala/Int#
         |[2:16..2:22): String => scala/Predef.String#
         |[2:25..2:26): _ <= local2
         |[2:27..2:35): toString => scala/Any#toString().
         |[2:49..2:50): s <= local3
         |[2:54..2:55): s => local3
         |[3:6..3:7): c <= _empty_/t223.c.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:26): Int => scala/Int#
         |[4:4..4:5): x <= local4
         |[4:9..4:10): x => local4
         |[4:24..4:25): v <= local5
         |[4:29..4:30): y <= local6
         |[4:34..4:35): y => local6
         |[4:49..4:50): w <= local7
         |[4:54..4:55): x => local4
         |[4:56..4:57): + => scala/Int#`+`(+4).
         |[4:58..4:59): v => local5
         |[4:60..4:61): + => scala/Int#`+`(+4).
         |[4:62..4:63): w => local7
         |[5:6..5:7): d <= _empty_/t223.d.
         |[5:9..5:12): Int => scala/Int#
         |[5:16..5:19): Int => scala/Int#
         |[5:23..5:26): Int => scala/Int#
         |[6:4..6:5): _ <= local8
         |[6:19..6:20): v <= local9
         |[6:24..6:25): _ <= local10
         |[6:39..6:40): w <= local11
         |[6:44..6:45): v => local9
         |[6:46..6:47): + => scala/Int#`+`(+4).
         |[6:48..6:49): w => local11
         |[7:6..7:7): e <= _empty_/t223.e.
         |[7:10..7:14): List => scala/collection/immutable/List.
         |[7:18..7:21): map => scala/collection/immutable/List#map().
         |[7:22..7:23): x <= local12
         |[7:27..7:28): x => local12
         |[7:42..7:43): w <= local13
         |[7:47..7:48): w => local13
         |[8:6..8:7): f <= _empty_/t223.f.
         |[8:10..8:14): List => scala/collection/immutable/List.
         |[8:18..8:21): map => scala/collection/immutable/List#map().
         |[8:24..8:25): _ <= local14
         |[8:39..8:40): w <= local15
         |[8:44..8:45): w => local15
         |""".stripMargin
    else
      """|[0:7..0:11): t223 <= _empty_/t223.
         |[1:6..1:7): a <= _empty_/t223.a.
         |[1:9..1:12): Int => scala/Int#
         |[1:16..1:19): Int => scala/Int#
         |[1:22..1:23): _ <= local0
         |[1:42..1:43): w <= local1
         |[1:47..1:48): w => local1
         |[2:6..2:7): b <= _empty_/t223.b.
         |[2:9..2:12): Int => scala/Int#
         |[2:16..2:22): String => scala/Predef.String#
         |[2:25..2:26): _ <= local2
         |[2:27..2:35): toString => scala/Any#toString().
         |[2:49..2:50): s <= local3
         |[2:54..2:55): s => local3
         |[3:6..3:7): c <= _empty_/t223.c.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:26): Int => scala/Int#
         |[4:4..4:5): x <= local4
         |[4:9..4:10): x => local4
         |[4:24..4:25): v <= local5
         |[4:29..4:30): y <= local6
         |[4:34..4:35): y => local6
         |[4:49..4:50): w <= local7
         |[4:54..4:55): x => local4
         |[4:56..4:57): + => scala/Int#`+`(+4).
         |[4:58..4:59): v => local5
         |[4:60..4:61): + => scala/Int#`+`(+4).
         |[4:62..4:63): w => local7
         |[5:6..5:7): d <= _empty_/t223.d.
         |[5:9..5:12): Int => scala/Int#
         |[5:16..5:19): Int => scala/Int#
         |[5:23..5:26): Int => scala/Int#
         |[6:4..6:5): _ <= local8
         |[6:19..6:20): v <= local9
         |[6:24..6:25): _ <= local10
         |[6:39..6:40): w <= local11
         |[6:44..6:45): v => local9
         |[6:46..6:47): + => scala/Int#`+`(+4).
         |[6:48..6:49): w => local11
         |[7:6..7:7): e <= _empty_/t223.e.
         |[7:10..7:14): List => scala/package.List.
         |[7:18..7:21): map => scala/collection/immutable/List#map().
         |[7:22..7:23): x <= local12
         |[7:27..7:28): x => local12
         |[7:42..7:43): w <= local13
         |[7:47..7:48): w => local13
         |[8:6..8:7): f <= _empty_/t223.f.
         |[8:10..8:14): List => scala/package.List.
         |[8:18..8:21): map => scala/collection/immutable/List#map().
         |[8:24..8:25): _ <= local14
         |[8:39..8:40): w <= local15
         |[8:44..8:45): w => local15
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
       |local18 => param z: Int
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
       |local28 => param z: Int
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
       |local37 => param z: Int
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
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t224 <= _empty_/t224.
         |[1:6..1:7): a <= _empty_/t224.a.
         |[1:15..1:16): x <= local0
         |[1:20..1:24): List => scala/package.List.
         |[1:28..1:35): collect => scala/collection/immutable/List#collect().
         |[1:36..1:37): _ <= local8
         |[1:36..1:41): _ + 1 <= local1
         |[1:38..1:39): + => scala/Int#`+`(+4).
         |[1:50..1:51): x => local0
         |[2:6..2:7): b <= _empty_/t224.b.
         |[2:15..2:16): x <= local9
         |[2:20..2:24): List => scala/package.List.
         |[2:29..2:30): y <= local19
         |[2:33..2:37): List => scala/package.List.
         |[2:38..2:39): x => local9
         |[2:41..2:48): collect => scala/collection/immutable/List#collect().
         |[2:49..2:50): z <= local18
         |[2:49..2:59): z => z + 1 <= local11
         |[2:54..2:55): z => local18
         |[2:56..2:57): + => scala/Int#`+`(+4).
         |[2:68..2:69): y => local19
         |[3:6..3:7): c <= _empty_/t224.c.
         |[3:15..3:16): x <= local20
         |[3:20..3:24): List => scala/package.List.
         |[3:31..3:35): List => scala/package.List.
         |[3:36..3:37): x => local20
         |[3:39..3:46): collect => scala/collection/immutable/List#collect().
         |[3:47..3:60): (z: Int) => z <= local22
         |[3:48..3:49): z <= local28
         |[3:51..3:54): Int => scala/Int#
         |[3:59..3:60): z => local28
         |[3:62..3:70): nonEmpty => scala/collection/IterableOnceOps#nonEmpty().
         |[3:78..3:79): x => local20
         |[4:6..4:7): d <= _empty_/t224.d.
         |[4:15..4:16): x <= local29
         |[4:20..4:24): List => scala/package.List.
         |[4:35..4:39): List => scala/package.List.
         |[4:40..4:41): x => local29
         |[4:43..4:50): collect => scala/collection/immutable/List#collect().
         |[4:51..4:52): z <= local37
         |[4:51..4:61): z => z + x <= local30
         |[4:56..4:57): z => local37
         |[4:58..4:59): + => scala/Int#`+`(+4).
         |[4:60..4:61): x => local29
         |""".stripMargin
    else
      """|[0:7..0:11): t224 <= _empty_/t224.
         |[1:6..1:7): a <= _empty_/t224.a.
         |[1:15..1:16): x <= local0
         |[1:20..1:24): List => scala/package.List.
         |[1:28..1:35): collect => scala/collection/immutable/List#collect().
         |[1:36..1:37): _ <= local8
         |[1:36..1:41): _ + 1 <= local1
         |[1:38..1:39): + => scala/Int#`+`(+4).
         |[1:50..1:51): x => local0
         |[2:6..2:7): b <= _empty_/t224.b.
         |[2:15..2:16): x <= local9
         |[2:20..2:24): List => scala/package.List.
         |[2:29..2:30): y <= local19
         |[2:33..2:37): List => scala/package.List.
         |[2:38..2:39): x => local9
         |[2:41..2:48): collect => scala/collection/immutable/List#collect().
         |[2:49..2:50): z <= local18
         |[2:49..2:59): z => z + 1 <= local11
         |[2:54..2:55): z => local18
         |[2:56..2:57): + => scala/Int#`+`(+4).
         |[2:68..2:69): y => local19
         |[3:6..3:7): c <= _empty_/t224.c.
         |[3:15..3:16): x <= local20
         |[3:20..3:24): List => scala/package.List.
         |[3:31..3:35): List => scala/package.List.
         |[3:36..3:37): x => local20
         |[3:39..3:46): collect => scala/collection/immutable/List#collect().
         |[3:47..3:60): (z: Int) => z <= local22
         |[3:48..3:49): z <= local28
         |[3:59..3:60): z => local28
         |[3:62..3:70): nonEmpty => scala/collection/IterableOnceOps#nonEmpty().
         |[3:78..3:79): x => local20
         |[4:6..4:7): d <= _empty_/t224.d.
         |[4:15..4:16): x <= local29
         |[4:20..4:24): List => scala/package.List.
         |[4:35..4:39): List => scala/package.List.
         |[4:40..4:41): x => local29
         |[4:43..4:50): collect => scala/collection/immutable/List#collect().
         |[4:51..4:52): z <= local37
         |[4:51..4:61): z => z + x <= local30
         |[4:56..4:57): z => local37
         |[4:58..4:59): + => scala/Int#`+`(+4).
         |[4:60..4:61): x => local29
         |""".stripMargin,
  )

  symbols(
    "partial function lambdas with a match in the enumerators of a for",
    """|object t225 {
       |  val a = for (x <- List(1).collect(_ match { case w => w })) yield x
       |  val b = for (x <- List(1); y = List(x).collect(z => z match { case w => w })) yield y
       |  val c = for (x <- List(1) if List(x).collect((z: Int) => z match { case w => w }).nonEmpty) yield x
       |  val d = for (x <- List(1)) yield List(x).collect(z => z match { case w => w + x })
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t225. => final object t225 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t225.a. => val method a: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t225.b. => val method b: List[List[Int]]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t225.c. => val method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t225.d. => val method d: List[List[Int]]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |local0 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local1 => param x: A1
         |  A1 => local0
         |local2 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local0
         |  Serializable => scala/Serializable#
         |local3 => final method applyOrElse[A1 <: Int, B1 >: A1](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local0
         |  Int => scala/Int#
         |  B1 => local7
         |  x$1 => local5
         |  default => local6
         |  Function1 => scala/Function1#
         |local4 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local5 => synthetic param x$1: A1
         |  A1 => local0
         |local6 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local0
         |  B1 => local7
         |local7 => typeparam B1 >: A1
         |  A1 => local0
         |local8 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local9 => val local w: A1
         |  A1 => local0
         |local10 => val local x: Int
         |  Int => scala/Int#
         |local11 => synthetic param x$2: Tuple2[Int, List[Int]]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |local12 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local13 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local12
         |  Serializable => scala/Serializable#
         |local14 => final method applyOrElse[A1 <: Int, B1 >: A1](z: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local12
         |  Int => scala/Int#
         |  B1 => local18
         |  z => local16
         |  default => local17
         |  Function1 => scala/Function1#
         |local15 => final method isDefinedAt(z: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  z => local19
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local16 => synthetic param z: A1
         |  A1 => local12
         |local17 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local12
         |  B1 => local18
         |local18 => typeparam B1 >: A1
         |  A1 => local12
         |local19 => param z: Int
         |  Int => scala/Int#
         |local20 => val local w: A1
         |  A1 => local12
         |local21 => val local y: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |local22 => param x: Int
         |  Int => scala/Int#
         |local23 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local24 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local23
         |  Serializable => scala/Serializable#
         |local25 => final method applyOrElse[A1 <: Int, B1 >: A1](z: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local23
         |  Int => scala/Int#
         |  B1 => local29
         |  z => local27
         |  default => local28
         |  Function1 => scala/Function1#
         |local26 => final method isDefinedAt(z: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  z => local30
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local27 => synthetic param z: A1
         |  A1 => local23
         |local28 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local23
         |  B1 => local29
         |local29 => typeparam B1 >: A1
         |  A1 => local23
         |local30 => param z: Int
         |  Int => scala/Int#
         |local31 => val local w: A1
         |  A1 => local23
         |local32 => param x: Int
         |  Int => scala/Int#
         |local33 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local34 => final method applyOrElse[A1 <: Int, B1 >: Int](z: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local39
         |  Int => scala/Int#
         |  B1 => local38
         |  z => local36
         |  default => local37
         |  Function1 => scala/Function1#
         |local35 => final method isDefinedAt(z: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  z => local40
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local36 => synthetic param z: A1
         |  A1 => local39
         |local37 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local39
         |  B1 => local38
         |local38 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local39 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local40 => param z: Int
         |  Int => scala/Int#
         |local41 => val local w: A1
         |  A1 => local39
         |""".stripMargin
    else
      """|_empty_/t225. => final object t225 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t225.a. => val method a: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t225.b. => val method b: List[List[Int]]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t225.c. => val method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t225.d. => val method d: List[List[Int]]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |local0 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local1 => param x: A1
         |  A1 => local0
         |local2 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local0
         |  Serializable => java/io/Serializable#
         |local3 => final method applyOrElse[A1 <: Int, B1 >: A1](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local0
         |  Int => scala/Int#
         |  B1 => local7
         |  x$1 => local5
         |  default => local6
         |  Function1 => scala/Function1#
         |local4 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local5 => synthetic param x$1: A1
         |  A1 => local0
         |local6 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local0
         |  B1 => local7
         |local7 => typeparam B1 >: A1
         |  A1 => local0
         |local8 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local9 => val local w: A1
         |  A1 => local0
         |local10 => val local x: Int
         |  Int => scala/Int#
         |local11 => synthetic param x$2: Tuple2[Int, List[Int]]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |local12 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local13 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local12
         |  Serializable => java/io/Serializable#
         |local14 => final method applyOrElse[A1 <: Int, B1 >: A1](z: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local12
         |  Int => scala/Int#
         |  B1 => local18
         |  z => local16
         |  default => local17
         |  Function1 => scala/Function1#
         |local15 => final method isDefinedAt(z: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  z => local19
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local16 => synthetic param z: A1
         |  A1 => local12
         |local17 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local12
         |  B1 => local18
         |local18 => typeparam B1 >: A1
         |  A1 => local12
         |local19 => param z: Int
         |  Int => scala/Int#
         |local20 => val local w: A1
         |  A1 => local12
         |local21 => val local y: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |local22 => param x: Int
         |  Int => scala/Int#
         |local23 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local24 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local23
         |  Serializable => java/io/Serializable#
         |local25 => final method applyOrElse[A1 <: Int, B1 >: A1](z: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local23
         |  Int => scala/Int#
         |  B1 => local29
         |  z => local27
         |  default => local28
         |  Function1 => scala/Function1#
         |local26 => final method isDefinedAt(z: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  z => local30
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local27 => synthetic param z: A1
         |  A1 => local23
         |local28 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local23
         |  B1 => local29
         |local29 => typeparam B1 >: A1
         |  A1 => local23
         |local30 => param z: Int
         |  Int => scala/Int#
         |local31 => val local w: A1
         |  A1 => local23
         |local32 => param x: Int
         |  Int => scala/Int#
         |local33 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local34 => final method applyOrElse[A1 <: Int, B1 >: Int](z: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local39
         |  Int => scala/Int#
         |  B1 => local38
         |  z => local36
         |  default => local37
         |  Function1 => scala/Function1#
         |local35 => final method isDefinedAt(z: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  z => local40
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local36 => synthetic param z: A1
         |  A1 => local39
         |local37 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local39
         |  B1 => local38
         |local38 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local39 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local40 => param z: Int
         |  Int => scala/Int#
         |local41 => val local w: A1
         |  A1 => local39
         |""".stripMargin,
  )

  occurrences(
    "partial function lambdas with a match in the enumerators of a for",
    """|object t225 {
       |  val a = for (x <- List(1).collect(_ match { case w => w })) yield x
       |  val b = for (x <- List(1); y = List(x).collect(z => z match { case w => w })) yield y
       |  val c = for (x <- List(1) if List(x).collect((z: Int) => z match { case w => w }).nonEmpty) yield x
       |  val d = for (x <- List(1)) yield List(x).collect(z => z match { case w => w + x })
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): t225 <= _empty_/t225.
         |[1:6..1:7): a <= _empty_/t225.a.
         |[1:15..1:16): x <= local1
         |[1:20..1:24): List => scala/collection/immutable/List.
         |[1:28..1:35): collect => scala/collection/immutable/List#collect().
         |[1:36..1:37): _ <= local8
         |[1:36..1:59): _ match { case w => w } <= local2
         |[1:51..1:52): w <= local9
         |[1:56..1:57): w => local9
         |[1:68..1:69): x => local1
         |[2:6..2:7): b <= _empty_/t225.b.
         |[2:15..2:16): x <= local10
         |[2:20..2:24): List => scala/collection/immutable/List.
         |[2:29..2:30): y <= local21
         |[2:33..2:37): List => scala/collection/immutable/List.
         |[2:38..2:39): x => local10
         |[2:41..2:48): collect => scala/collection/immutable/List#collect().
         |[2:49..2:50): z <= local19
         |[2:49..2:77): z => z match { case w => w } <= local13
         |[2:54..2:55): z => local19
         |[2:69..2:70): w <= local20
         |[2:74..2:75): w => local20
         |[2:86..2:87): y => local21
         |[3:6..3:7): c <= _empty_/t225.c.
         |[3:15..3:16): x <= local22
         |[3:20..3:24): List => scala/collection/immutable/List.
         |[3:31..3:35): List => scala/collection/immutable/List.
         |[3:36..3:37): x => local22
         |[3:39..3:46): collect => scala/collection/immutable/List#collect().
         |[3:47..3:82): (z: Int) => z match { case w => w } <= local24
         |[3:48..3:49): z <= local30
         |[3:59..3:60): z => local30
         |[3:74..3:75): w <= local31
         |[3:79..3:80): w => local31
         |[3:84..3:92): nonEmpty => scala/collection/TraversableOnce#nonEmpty().
         |[3:100..3:101): x => local22
         |[4:6..4:7): d <= _empty_/t225.d.
         |[4:15..4:16): x <= local32
         |[4:20..4:24): List => scala/collection/immutable/List.
         |[4:35..4:39): List => scala/collection/immutable/List.
         |[4:40..4:41): x => local32
         |[4:43..4:50): collect => scala/collection/immutable/List#collect().
         |[4:51..4:52): z <= local40
         |[4:51..4:83): z => z match { case w => w + x } <= local33
         |[4:56..4:57): z => local40
         |[4:71..4:72): w <= local41
         |[4:76..4:77): w => local41
         |[4:78..4:79): + => scala/Int#`+`(+4).
         |[4:80..4:81): x => local32
         |""".stripMargin
    else if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t225 <= _empty_/t225.
         |[1:6..1:7): a <= _empty_/t225.a.
         |[1:15..1:16): x <= local1
         |[1:20..1:24): List => scala/package.List.
         |[1:28..1:35): collect => scala/collection/immutable/List#collect().
         |[1:36..1:37): _ <= local8
         |[1:36..1:59): _ match { case w => w } <= local2
         |[1:51..1:52): w <= local9
         |[1:56..1:57): w => local9
         |[1:68..1:69): x => local1
         |[2:6..2:7): b <= _empty_/t225.b.
         |[2:15..2:16): x <= local10
         |[2:20..2:24): List => scala/package.List.
         |[2:29..2:30): y <= local21
         |[2:33..2:37): List => scala/package.List.
         |[2:38..2:39): x => local10
         |[2:41..2:48): collect => scala/collection/immutable/List#collect().
         |[2:49..2:50): z <= local19
         |[2:49..2:77): z => z match { case w => w } <= local13
         |[2:54..2:55): z => local19
         |[2:69..2:70): w <= local20
         |[2:74..2:75): w => local20
         |[2:86..2:87): y => local21
         |[3:6..3:7): c <= _empty_/t225.c.
         |[3:15..3:16): x <= local22
         |[3:20..3:24): List => scala/package.List.
         |[3:31..3:35): List => scala/package.List.
         |[3:36..3:37): x => local22
         |[3:39..3:46): collect => scala/collection/immutable/List#collect().
         |[3:47..3:82): (z: Int) => z match { case w => w } <= local24
         |[3:48..3:49): z <= local30
         |[3:51..3:54): Int => scala/Int#
         |[3:59..3:60): z => local30
         |[3:74..3:75): w <= local31
         |[3:79..3:80): w => local31
         |[3:84..3:92): nonEmpty => scala/collection/IterableOnceOps#nonEmpty().
         |[3:100..3:101): x => local22
         |[4:6..4:7): d <= _empty_/t225.d.
         |[4:15..4:16): x <= local32
         |[4:20..4:24): List => scala/package.List.
         |[4:35..4:39): List => scala/package.List.
         |[4:40..4:41): x => local32
         |[4:43..4:50): collect => scala/collection/immutable/List#collect().
         |[4:51..4:52): z <= local40
         |[4:51..4:83): z => z match { case w => w + x } <= local33
         |[4:56..4:57): z => local40
         |[4:71..4:72): w <= local41
         |[4:76..4:77): w => local41
         |[4:78..4:79): + => scala/Int#`+`(+4).
         |[4:80..4:81): x => local32
         |""".stripMargin
    else
      """|[0:7..0:11): t225 <= _empty_/t225.
         |[1:6..1:7): a <= _empty_/t225.a.
         |[1:15..1:16): x <= local1
         |[1:20..1:24): List => scala/package.List.
         |[1:28..1:35): collect => scala/collection/immutable/List#collect().
         |[1:36..1:37): _ <= local8
         |[1:36..1:59): _ match { case w => w } <= local2
         |[1:51..1:52): w <= local9
         |[1:56..1:57): w => local9
         |[1:68..1:69): x => local1
         |[2:6..2:7): b <= _empty_/t225.b.
         |[2:15..2:16): x <= local10
         |[2:20..2:24): List => scala/package.List.
         |[2:29..2:30): y <= local21
         |[2:33..2:37): List => scala/package.List.
         |[2:38..2:39): x => local10
         |[2:41..2:48): collect => scala/collection/immutable/List#collect().
         |[2:49..2:50): z <= local19
         |[2:49..2:77): z => z match { case w => w } <= local13
         |[2:54..2:55): z => local19
         |[2:69..2:70): w <= local20
         |[2:74..2:75): w => local20
         |[2:86..2:87): y => local21
         |[3:6..3:7): c <= _empty_/t225.c.
         |[3:15..3:16): x <= local22
         |[3:20..3:24): List => scala/package.List.
         |[3:31..3:35): List => scala/package.List.
         |[3:36..3:37): x => local22
         |[3:39..3:46): collect => scala/collection/immutable/List#collect().
         |[3:47..3:82): (z: Int) => z match { case w => w } <= local24
         |[3:48..3:49): z <= local30
         |[3:59..3:60): z => local30
         |[3:74..3:75): w <= local31
         |[3:79..3:80): w => local31
         |[3:84..3:92): nonEmpty => scala/collection/IterableOnceOps#nonEmpty().
         |[3:100..3:101): x => local22
         |[4:6..4:7): d <= _empty_/t225.d.
         |[4:15..4:16): x <= local32
         |[4:20..4:24): List => scala/package.List.
         |[4:35..4:39): List => scala/package.List.
         |[4:40..4:41): x => local32
         |[4:43..4:50): collect => scala/collection/immutable/List#collect().
         |[4:51..4:52): z <= local40
         |[4:51..4:83): z => z match { case w => w + x } <= local33
         |[4:56..4:57): z => local40
         |[4:71..4:72): w <= local41
         |[4:76..4:77): w => local41
         |[4:78..4:79): + => scala/Int#`+`(+4).
         |[4:80..4:81): x => local32
         |""".stripMargin,
  )

  symbols(
    "a for comprehension with a match, a guard and two generators over partial functions",
    """|object t226 {
       |  class C {
       |    def flatMap[B](f: Int => List[B]): List[B] = ???
       |    def map[B](f: PartialFunction[Int, B]): List[B] = ???
       |    def withFilter(f: PartialFunction[Int, Boolean]): C = ???
       |  }
       |  val c = new C
       |  val a = for (x <- c if (x match { case v => v > 0 })) yield x match { case w => w }
       |  val b = for (x <- c; y <- c) yield y match { case w => x + w }
       |  val d = for (x: Int <- c) yield x match { case w => w }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t226. => final object t226 extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t226.C# => class C extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t226.C#`<init>`(). => primary ctor <init>()
         |_empty_/t226.C#flatMap(). => method flatMap[B](f: Function1[Int, List[B]]): List[B]
         |  B => _empty_/t226.C#flatMap().[B]
         |  f => _empty_/t226.C#flatMap().(f)
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |_empty_/t226.C#flatMap().(f) => param f: Function1[Int, List[B]]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |  B => _empty_/t226.C#flatMap().[B]
         |_empty_/t226.C#flatMap().[B] => typeparam B
         |_empty_/t226.C#map(). => method map[B](f: PartialFunction[Int, B]): List[B]
         |  B => _empty_/t226.C#map().[B]
         |  f => _empty_/t226.C#map().(f)
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |_empty_/t226.C#map().(f) => param f: PartialFunction[Int, B]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  B => _empty_/t226.C#map().[B]
         |_empty_/t226.C#map().[B] => typeparam B
         |_empty_/t226.C#withFilter(). => method withFilter(f: PartialFunction[Int, Boolean]): C
         |  f => _empty_/t226.C#withFilter().(f)
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |  C => _empty_/t226.C#
         |_empty_/t226.C#withFilter().(f) => param f: PartialFunction[Int, Boolean]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |_empty_/t226.a. => val method a: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t226.b. => val method b: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t226.c. => val method c: C
         |  C => _empty_/t226.C#
         |_empty_/t226.d. => val method d: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |local0 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Boolean] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |  Serializable => scala/Serializable#
         |local2 => final method applyOrElse[A1 <: Int, B1 >: Boolean](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local7
         |  Int => scala/Int#
         |  B1 => local6
         |  Boolean => scala/Boolean#
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
         |local6 => typeparam B1 >: Boolean
         |  Boolean => scala/Boolean#
         |local7 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local8 => param x: Int
         |  Int => scala/Int#
         |local9 => val local v: A1
         |  A1 => local7
         |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local0
         |  Serializable => scala/Serializable#
         |local11 => final method applyOrElse[A1 <: Int, B1 >: A1](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local0
         |  Int => scala/Int#
         |  B1 => local15
         |  x => local13
         |  default => local14
         |  Function1 => scala/Function1#
         |local12 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local13 => synthetic param x: A1
         |  A1 => local0
         |local14 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local0
         |  B1 => local15
         |local15 => typeparam B1 >: A1
         |  A1 => local0
         |local16 => val local w: A1
         |  A1 => local0
         |local17 => param x: Int
         |  Int => scala/Int#
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local19 => final method applyOrElse[A1 <: Int, B1 >: Int](y: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  Int => scala/Int#
         |  B1 => local23
         |  y => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(y: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  y => local25
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param y: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local25 => param y: Int
         |  Int => scala/Int#
         |local26 => val local w: A1
         |  A1 => local24
         |local27 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local28 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Boolean] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |  Serializable => scala/Serializable#
         |local29 => final method applyOrElse[A1 <: Int, B1 >: Boolean](check$ifrefutable$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local34
         |  Int => scala/Int#
         |  B1 => local33
         |  Boolean => scala/Boolean#
         |  check$ifrefutable$1 => local31
         |  default => local32
         |  Function1 => scala/Function1#
         |local30 => final method isDefinedAt(check$ifrefutable$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  check$ifrefutable$1 => local36
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local31 => synthetic param check$ifrefutable$1: A1
         |  A1 => local34
         |local32 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local34
         |  B1 => local33
         |local33 => typeparam B1 >: Boolean
         |  Boolean => scala/Boolean#
         |local34 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local35 => param x: Int
         |  Int => scala/Int#
         |local36 => synthetic param check$ifrefutable$1: Int
         |  Int => scala/Int#
         |local37 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local27
         |  Serializable => scala/Serializable#
         |local38 => final method applyOrElse[A1 <: Int, B1 >: A1](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local27
         |  Int => scala/Int#
         |  B1 => local42
         |  x => local40
         |  default => local41
         |  Function1 => scala/Function1#
         |local39 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local35
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local40 => synthetic param x: A1
         |  A1 => local27
         |local41 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local27
         |  B1 => local42
         |local42 => typeparam B1 >: A1
         |  A1 => local27
         |local43 => val local w: A1
         |  A1 => local27
         |""".stripMargin
    else
      """|_empty_/t226. => final object t226 extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t226.C# => class C extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t226.C#`<init>`(). => primary ctor <init>()
         |_empty_/t226.C#flatMap(). => method flatMap[B](f: Function1[Int, List[B]]): List[B]
         |  B => _empty_/t226.C#flatMap().[B]
         |  f => _empty_/t226.C#flatMap().(f)
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |_empty_/t226.C#flatMap().(f) => param f: Function1[Int, List[B]]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |  B => _empty_/t226.C#flatMap().[B]
         |_empty_/t226.C#flatMap().[B] => typeparam B
         |_empty_/t226.C#map(). => method map[B](f: PartialFunction[Int, B]): List[B]
         |  B => _empty_/t226.C#map().[B]
         |  f => _empty_/t226.C#map().(f)
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  List => scala/package.List#
         |_empty_/t226.C#map().(f) => param f: PartialFunction[Int, B]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  B => _empty_/t226.C#map().[B]
         |_empty_/t226.C#map().[B] => typeparam B
         |_empty_/t226.C#withFilter(). => method withFilter(f: PartialFunction[Int, Boolean]): C
         |  f => _empty_/t226.C#withFilter().(f)
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |  C => _empty_/t226.C#
         |_empty_/t226.C#withFilter().(f) => param f: PartialFunction[Int, Boolean]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |_empty_/t226.a. => val method a: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t226.b. => val method b: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t226.c. => val method c: C
         |  C => _empty_/t226.C#
         |_empty_/t226.d. => val method d: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |local0 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Boolean] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |  Serializable => java/io/Serializable#
         |local2 => final method applyOrElse[A1 <: Int, B1 >: Boolean](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local7
         |  Int => scala/Int#
         |  B1 => local6
         |  Boolean => scala/Boolean#
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
         |local6 => typeparam B1 >: Boolean
         |  Boolean => scala/Boolean#
         |local7 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local8 => param x: Int
         |  Int => scala/Int#
         |local9 => val local v: A1
         |  A1 => local7
         |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local0
         |  Serializable => java/io/Serializable#
         |local11 => final method applyOrElse[A1 <: Int, B1 >: A1](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local0
         |  Int => scala/Int#
         |  B1 => local15
         |  x => local13
         |  default => local14
         |  Function1 => scala/Function1#
         |local12 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local13 => synthetic param x: A1
         |  A1 => local0
         |local14 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local0
         |  B1 => local15
         |local15 => typeparam B1 >: A1
         |  A1 => local0
         |local16 => val local w: A1
         |  A1 => local0
         |local17 => param x: Int
         |  Int => scala/Int#
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local19 => final method applyOrElse[A1 <: Int, B1 >: Int](y: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  Int => scala/Int#
         |  B1 => local23
         |  y => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(y: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  y => local25
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param y: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local25 => param y: Int
         |  Int => scala/Int#
         |local26 => val local w: A1
         |  A1 => local24
         |local27 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local28 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Boolean] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |  Serializable => java/io/Serializable#
         |local29 => final method applyOrElse[A1 <: Int, B1 >: Boolean](check$ifrefutable$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local34
         |  Int => scala/Int#
         |  B1 => local33
         |  Boolean => scala/Boolean#
         |  check$ifrefutable$1 => local31
         |  default => local32
         |  Function1 => scala/Function1#
         |local30 => final method isDefinedAt(check$ifrefutable$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  check$ifrefutable$1 => local36
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local31 => synthetic param check$ifrefutable$1: A1
         |  A1 => local34
         |local32 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local34
         |  B1 => local33
         |local33 => typeparam B1 >: Boolean
         |  Boolean => scala/Boolean#
         |local34 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local35 => param x: Int
         |  Int => scala/Int#
         |local36 => synthetic param check$ifrefutable$1: Int
         |  Int => scala/Int#
         |local37 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local27
         |  Serializable => java/io/Serializable#
         |local38 => final method applyOrElse[A1 <: Int, B1 >: A1](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local27
         |  Int => scala/Int#
         |  B1 => local42
         |  x => local40
         |  default => local41
         |  Function1 => scala/Function1#
         |local39 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local35
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local40 => synthetic param x: A1
         |  A1 => local27
         |local41 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local27
         |  B1 => local42
         |local42 => typeparam B1 >: A1
         |  A1 => local27
         |local43 => val local w: A1
         |  A1 => local27
         |""".stripMargin,
  )

  occurrences(
    "a for comprehension with a match, a guard and two generators over partial functions",
    """|object t226 {
       |  class C {
       |    def flatMap[B](f: Int => List[B]): List[B] = ???
       |    def map[B](f: PartialFunction[Int, B]): List[B] = ???
       |    def withFilter(f: PartialFunction[Int, Boolean]): C = ???
       |  }
       |  val c = new C
       |  val a = for (x <- c if (x match { case v => v > 0 })) yield x match { case w => w }
       |  val b = for (x <- c; y <- c) yield y match { case w => x + w }
       |  val d = for (x: Int <- c) yield x match { case w => w }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t226 <= _empty_/t226.
       |[1:8..1:9): C <= _empty_/t226.C#
       |[1:10..1:10):  <= _empty_/t226.C#`<init>`().
       |[2:8..2:15): flatMap <= _empty_/t226.C#flatMap().
       |[2:16..2:17): B <= _empty_/t226.C#flatMap().[B]
       |[2:19..2:20): f <= _empty_/t226.C#flatMap().(f)
       |[2:22..2:25): Int => scala/Int#
       |[2:29..2:33): List => scala/package.List#
       |[2:34..2:35): B => _empty_/t226.C#flatMap().[B]
       |[2:39..2:43): List => scala/package.List#
       |[2:44..2:45): B => _empty_/t226.C#flatMap().[B]
       |[2:49..2:52): ??? => scala/Predef.`???`().
       |[3:8..3:11): map <= _empty_/t226.C#map().
       |[3:12..3:13): B <= _empty_/t226.C#map().[B]
       |[3:15..3:16): f <= _empty_/t226.C#map().(f)
       |[3:18..3:33): PartialFunction => scala/PartialFunction#
       |[3:34..3:37): Int => scala/Int#
       |[3:39..3:40): B => _empty_/t226.C#map().[B]
       |[3:44..3:48): List => scala/package.List#
       |[3:49..3:50): B => _empty_/t226.C#map().[B]
       |[3:54..3:57): ??? => scala/Predef.`???`().
       |[4:8..4:18): withFilter <= _empty_/t226.C#withFilter().
       |[4:19..4:20): f <= _empty_/t226.C#withFilter().(f)
       |[4:22..4:37): PartialFunction => scala/PartialFunction#
       |[4:38..4:41): Int => scala/Int#
       |[4:43..4:50): Boolean => scala/Boolean#
       |[4:54..4:55): C => _empty_/t226.C#
       |[4:58..4:61): ??? => scala/Predef.`???`().
       |[6:6..6:7): c <= _empty_/t226.c.
       |[6:14..6:15): C => _empty_/t226.C#
       |[6:15..6:15):  => _empty_/t226.C#`<init>`().
       |[7:6..7:7): a <= _empty_/t226.a.
       |[7:15..7:16): x <= local8
       |[7:20..7:21): c => _empty_/t226.c.
       |[7:26..7:27): x => local8
       |[7:28..7:28):  <= local1
       |[7:41..7:42): v <= local9
       |[7:46..7:47): v => local9
       |[7:48..7:49): > => scala/Int#`>`(+3).
       |[7:62..7:63): x => local8
       |[7:64..7:64):  <= local10
       |[7:77..7:78): w <= local16
       |[7:82..7:83): w => local16
       |[8:6..8:7): b <= _empty_/t226.b.
       |[8:15..8:16): x <= local17
       |[8:20..8:21): c => _empty_/t226.c.
       |[8:23..8:24): y <= local25
       |[8:28..8:29): c => _empty_/t226.c.
       |[8:37..8:38): y => local25
       |[8:39..8:39):  <= local18
       |[8:52..8:53): w <= local26
       |[8:57..8:58): x => local17
       |[8:59..8:60): + => scala/Int#`+`(+4).
       |[8:61..8:62): w => local26
       |[9:6..9:7): d <= _empty_/t226.d.
       |[9:15..9:16): x <= local35
       |[9:18..9:21): Int => scala/Int#
       |[9:25..9:25):  <= local28
       |[9:25..9:26): c => _empty_/t226.c.
       |[9:34..9:35): x => local35
       |[9:36..9:36):  <= local37
       |[9:49..9:50): w <= local43
       |[9:54..9:55): w => local43
       |""".stripMargin,
  )

  symbols(
    "a placeholder in an enumerator of a for over plain functions",
    """|object t227 {
       |  val a: List[Int] => List[Int] = for (x <- _) yield x + 1
       |  val b: Int => List[Int] = for (x <- List(1, 2) if x > _) yield x
       |  val c: Int => List[Int] = for (x <- List(1, 2); y = _) yield x + y
       |}
       |""".stripMargin,
    """|_empty_/t227. => final object t227 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t227.a. => val method a: Function1[List[Int], List[Int]]
       |  Function1 => scala/Function1#
       |  List => scala/package.List#
       |  Int => scala/Int#
       |_empty_/t227.b. => val method b: Function1[Int, List[Int]]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  List => scala/package.List#
       |_empty_/t227.c. => val method c: Function1[Int, List[Int]]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  List => scala/package.List#
       |local0 => synthetic param x$1: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local1 => param x: Int
       |  Int => scala/Int#
       |local2 => synthetic param x$2: Int
       |  Int => scala/Int#
       |local3 => param x: Int
       |  Int => scala/Int#
       |local4 => synthetic param x$3: Int
       |  Int => scala/Int#
       |local5 => val local x: Int
       |  Int => scala/Int#
       |local6 => synthetic param x$4: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local7 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "a placeholder in an enumerator of a for over plain functions",
    """|object t227 {
       |  val a: List[Int] => List[Int] = for (x <- _) yield x + 1
       |  val b: Int => List[Int] = for (x <- List(1, 2) if x > _) yield x
       |  val c: Int => List[Int] = for (x <- List(1, 2); y = _) yield x + y
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): t227 <= _empty_/t227.
         |[1:6..1:7): a <= _empty_/t227.a.
         |[1:9..1:13): List => scala/package.List#
         |[1:14..1:17): Int => scala/Int#
         |[1:22..1:26): List => scala/package.List#
         |[1:27..1:30): Int => scala/Int#
         |[1:39..1:40): x <= local1
         |[1:44..1:45): _ <= local0
         |[1:53..1:54): x => local1
         |[1:55..1:56): + => scala/Int#`+`(+4).
         |[2:6..2:7): b <= _empty_/t227.b.
         |[2:9..2:12): Int => scala/Int#
         |[2:16..2:20): List => scala/package.List#
         |[2:21..2:24): Int => scala/Int#
         |[2:33..2:34): x <= local3
         |[2:38..2:42): List => scala/collection/immutable/List.
         |[2:52..2:53): x => local3
         |[2:54..2:55): > => scala/Int#`>`(+3).
         |[2:56..2:57): _ <= local2
         |[2:65..2:66): x => local3
         |[3:6..3:7): c <= _empty_/t227.c.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:20): List => scala/package.List#
         |[3:21..3:24): Int => scala/Int#
         |[3:33..3:34): x <= local5
         |[3:38..3:42): List => scala/collection/immutable/List.
         |[3:50..3:51): y <= local7
         |[3:54..3:55): _ <= local4
         |[3:63..3:64): x => local5
         |[3:65..3:66): + => scala/Int#`+`(+4).
         |[3:67..3:68): y => local7
         |""".stripMargin
    else
      """|[0:7..0:11): t227 <= _empty_/t227.
         |[1:6..1:7): a <= _empty_/t227.a.
         |[1:9..1:13): List => scala/package.List#
         |[1:14..1:17): Int => scala/Int#
         |[1:22..1:26): List => scala/package.List#
         |[1:27..1:30): Int => scala/Int#
         |[1:39..1:40): x <= local1
         |[1:44..1:45): _ <= local0
         |[1:53..1:54): x => local1
         |[1:55..1:56): + => scala/Int#`+`(+4).
         |[2:6..2:7): b <= _empty_/t227.b.
         |[2:9..2:12): Int => scala/Int#
         |[2:16..2:20): List => scala/package.List#
         |[2:21..2:24): Int => scala/Int#
         |[2:33..2:34): x <= local3
         |[2:38..2:42): List => scala/package.List.
         |[2:52..2:53): x => local3
         |[2:54..2:55): > => scala/Int#`>`(+3).
         |[2:56..2:57): _ <= local2
         |[2:65..2:66): x => local3
         |[3:6..3:7): c <= _empty_/t227.c.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:20): List => scala/package.List#
         |[3:21..3:24): Int => scala/Int#
         |[3:33..3:34): x <= local5
         |[3:38..3:42): List => scala/package.List.
         |[3:50..3:51): y <= local7
         |[3:54..3:55): _ <= local4
         |[3:63..3:64): x => local5
         |[3:65..3:66): + => scala/Int#`+`(+4).
         |[3:67..3:68): y => local7
         |""".stripMargin,
  )

  occurrences(
    "a tuple that scalac builds for a pattern definition and a for",
    """|object t230 {
       |  case class Foo(a: Int, b: Int)
       |  val (a, b) = (1, 2)
       |  val Foo(d, e) = Foo(1, 2)
       |  def m = { val Foo(f, g) = Foo(1, 2); f + g }
       |  val h = for (x <- Option(1); Foo(i, j) = Foo(x, x)) yield i + j
       |  val k = for (x <- Option(1); (y, z) = (x, x); w = y) yield w + z
       |  object Term { case class Tuple(a: Int, b: Int) }
       |  val Term.Tuple(l, n) = Term.Tuple(1, 2)
       |  def o = { val Term.Tuple(q, r) = Term.Tuple(1, 2); q + r }
       |  val s = for (x <- Option(1); Term.Tuple(t, u) = Term.Tuple(x, x)) yield t + u
       |}
       |""".stripMargin,
    """|[0:7..0:11): t230 <= _empty_/t230.
       |[1:13..1:16): Foo <= _empty_/t230.Foo#
       |[1:16..1:16):  <= _empty_/t230.Foo#`<init>`().
       |[1:17..1:18): a <= _empty_/t230.Foo#`<init>`().(a)
       |[1:17..1:18): a <= _empty_/t230.Foo#a.
       |[1:20..1:23): Int => scala/Int#
       |[1:25..1:26): b <= _empty_/t230.Foo#`<init>`().(b)
       |[1:25..1:26): b <= _empty_/t230.Foo#b.
       |[1:28..1:31): Int => scala/Int#
       |[2:7..2:8): a <= _empty_/t230.a.
       |[2:10..2:11): b <= _empty_/t230.b.
       |[3:6..3:9): Foo => _empty_/t230.Foo.
       |[3:10..3:11): d <= _empty_/t230.d.
       |[3:13..3:14): e <= _empty_/t230.e.
       |[3:18..3:21): Foo => _empty_/t230.Foo.
       |[4:6..4:7): m <= _empty_/t230.m().
       |[4:16..4:19): Foo => _empty_/t230.Foo.
       |[4:20..4:21): f <= local0
       |[4:23..4:24): g <= local1
       |[4:28..4:31): Foo => _empty_/t230.Foo.
       |[4:39..4:40): f => local0
       |[4:41..4:42): + => scala/Int#`+`(+4).
       |[4:43..4:44): g => local1
       |[5:6..5:7): h <= _empty_/t230.h.
       |[5:15..5:16): x <= local2
       |[5:20..5:26): Option => scala/Option.
       |[5:31..5:34): Foo => _empty_/t230.Foo.
       |[5:35..5:36): i <= local4
       |[5:38..5:39): j <= local5
       |[5:43..5:46): Foo => _empty_/t230.Foo.
       |[5:47..5:48): x => local2
       |[5:50..5:51): x => local2
       |[5:60..5:61): i => local4
       |[5:62..5:63): + => scala/Int#`+`(+4).
       |[5:64..5:65): j => local5
       |[6:6..6:7): k <= _empty_/t230.k.
       |[6:15..6:16): x <= local6
       |[6:20..6:26): Option => scala/Option.
       |[6:32..6:33): y <= local8
       |[6:35..6:36): z <= local9
       |[6:41..6:42): x => local6
       |[6:44..6:45): x => local6
       |[6:48..6:49): w <= local10
       |[6:52..6:53): y => local8
       |[6:61..6:62): w => local10
       |[6:63..6:64): + => scala/Int#`+`(+4).
       |[6:65..6:66): z => local9
       |[7:9..7:13): Term <= _empty_/t230.Term.
       |[7:27..7:32): Tuple <= _empty_/t230.Term.Tuple#
       |[7:32..7:32):  <= _empty_/t230.Term.Tuple#`<init>`().
       |[7:33..7:34): a <= _empty_/t230.Term.Tuple#`<init>`().(a)
       |[7:33..7:34): a <= _empty_/t230.Term.Tuple#a.
       |[7:36..7:39): Int => scala/Int#
       |[7:41..7:42): b <= _empty_/t230.Term.Tuple#`<init>`().(b)
       |[7:41..7:42): b <= _empty_/t230.Term.Tuple#b.
       |[7:44..7:47): Int => scala/Int#
       |[8:6..8:10): Term => _empty_/t230.Term.
       |[8:11..8:16): Tuple => _empty_/t230.Term.Tuple.
       |[8:17..8:18): l <= _empty_/t230.l.
       |[8:20..8:21): n <= _empty_/t230.n.
       |[8:25..8:29): Term => _empty_/t230.Term.
       |[8:30..8:35): Tuple => _empty_/t230.Term.Tuple.
       |[9:6..9:7): o <= _empty_/t230.o().
       |[9:16..9:20): Term => _empty_/t230.Term.
       |[9:21..9:26): Tuple => _empty_/t230.Term.Tuple.
       |[9:27..9:28): q <= local11
       |[9:30..9:31): r <= local12
       |[9:35..9:39): Term => _empty_/t230.Term.
       |[9:40..9:45): Tuple => _empty_/t230.Term.Tuple.
       |[9:53..9:54): q => local11
       |[9:55..9:56): + => scala/Int#`+`(+4).
       |[9:57..9:58): r => local12
       |[10:6..10:7): s <= _empty_/t230.s.
       |[10:15..10:16): x <= local13
       |[10:20..10:26): Option => scala/Option.
       |[10:31..10:35): Term => _empty_/t230.Term.
       |[10:36..10:41): Tuple => _empty_/t230.Term.Tuple.
       |[10:42..10:43): t <= local15
       |[10:45..10:46): u <= local16
       |[10:50..10:54): Term => _empty_/t230.Term.
       |[10:55..10:60): Tuple => _empty_/t230.Term.Tuple.
       |[10:61..10:62): x => local13
       |[10:64..10:65): x => local13
       |[10:74..10:75): t => local15
       |[10:76..10:77): + => scala/Int#`+`(+4).
       |[10:78..10:79): u => local16
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
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
    if (!ScalaVersion.atLeast213_17)
      """|_empty_/t231. => final object t231 extends AnyRef { +7 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t231.Int# => class Int extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t231.Int#`<init>`(). => primary ctor <init>()
         |_empty_/t231.List# => type List[A] = Seq[A]
         |  A => _empty_/t231.List#[A]
         |  Seq => scala/collection/Seq#
         |_empty_/t231.List#[A] => typeparam A
         |_empty_/t231.a. => val method a: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => _empty_/t231.o.Int#
         |  Int => scala/Int#
         |_empty_/t231.b. => val method b: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t231.c. => val method c: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => _empty_/t231.o.Int#
         |  Int => scala/Int#
         |_empty_/t231.d. => val method d: Function1[List[Int], Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t231.o. => final object o extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t231.o.Int# => class Int extends Int { +1 decls }
         |  Int => _empty_/t231.Int#
         |_empty_/t231.o.Int#`<init>`(). => primary ctor <init>()
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => _empty_/t231.o.Int#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => _empty_/t231.o.Int#
         |  B1 => local5
         |  Int => scala/Int#
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  Int => _empty_/t231.o.Int#
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
         |  Int => _empty_/t231.o.Int#
         |local7 => param x: Int
         |  Int => _empty_/t231.o.Int#
         |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local9 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local14
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  B1 => local13
         |  x => local11
         |  default => local12
         |  Function1 => scala/Function1#
         |local10 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local15
         |  List => scala/package.List#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local11 => synthetic param x: A1
         |  A1 => local14
         |local12 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local14
         |  B1 => local13
         |local13 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local14 => typeparam A1 <: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local15 => param x: List[Int]
         |  List => scala/package.List#
         |  Int => scala/Int#
         |local16 => param x: Int
         |  Int => _empty_/t231.Int#
         |local17 => param x: List[Int]
         |  List => _empty_/t231.List#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t231. => final object t231 extends AnyRef { +7 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t231.Int# => class Int extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t231.Int#`<init>`(). => primary ctor <init>()
         |_empty_/t231.List# => type List[A] = Seq[A]
         |  A => _empty_/t231.List#[A]
         |  Seq => scala/collection/Seq#
         |_empty_/t231.List#[A] => typeparam A
         |_empty_/t231.a. => val method a: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => _empty_/t231.o.Int#
         |  Int => scala/Int#
         |_empty_/t231.b. => val method b: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t231.c. => val method c: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => _empty_/t231.o.Int#
         |  Int => scala/Int#
         |_empty_/t231.d. => val method d: Function1[List[Int], Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t231.o. => final object o extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t231.o.Int# => class Int extends Int { +1 decls }
         |  Int => _empty_/t231.Int#
         |_empty_/t231.o.Int#`<init>`(). => primary ctor <init>()
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => _empty_/t231.Int#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => _empty_/t231.Int#
         |  B1 => local5
         |  Int => scala/Int#
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  Int => _empty_/t231.Int#
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
         |  Int => _empty_/t231.Int#
         |local7 => param x: Int
         |  Int => _empty_/t231.Int#
         |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => _empty_/t231.List#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local9 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local14
         |  List => _empty_/t231.List#
         |  Int => scala/Int#
         |  B1 => local13
         |  x => local11
         |  default => local12
         |  Function1 => scala/Function1#
         |local10 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local15
         |  List => _empty_/t231.List#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local11 => synthetic param x: A1
         |  A1 => local14
         |local12 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local14
         |  B1 => local13
         |local13 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local14 => typeparam A1 <: List[Int]
         |  List => _empty_/t231.List#
         |  Int => scala/Int#
         |local15 => param x: List[Int]
         |  List => _empty_/t231.List#
         |  Int => scala/Int#
         |local16 => param x: Int
         |  Int => _empty_/t231.Int#
         |local17 => param x: List[Int]
         |  List => _empty_/t231.List#
         |  Int => scala/Int#
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
    if (!ScalaVersion.atLeast213_17)
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
         |[4:45..4:58): (x: Int) => 1 <= local0
         |[4:46..4:47): x <= local7
         |[5:6..5:7): b <= _empty_/t231.b.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:30): scala => scala/
         |[5:31..5:35): List => scala/package.List#
         |[5:36..5:41): scala => scala/
         |[5:42..5:45): Int => scala/Int#
         |[5:48..5:53): scala => scala/
         |[5:54..5:57): Int => scala/Int#
         |[5:61..5:95): (x: List[scala.Int]) => x.hashCode <= local8
         |[5:62..5:63): x <= local15
         |[5:85..5:86): x => local15
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
         |""".stripMargin
    else
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
         |[4:45..4:58): (x: Int) => 1 <= local0
         |[4:46..4:47): x <= local7
         |[4:49..4:52): Int => _empty_/t231.Int#
         |[5:6..5:7): b <= _empty_/t231.b.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:30): scala => scala/
         |[5:31..5:35): List => scala/package.List#
         |[5:36..5:41): scala => scala/
         |[5:42..5:45): Int => scala/Int#
         |[5:48..5:53): scala => scala/
         |[5:54..5:57): Int => scala/Int#
         |[5:61..5:95): (x: List[scala.Int]) => x.hashCode <= local8
         |[5:62..5:63): x <= local15
         |[5:65..5:69): List => _empty_/t231.List#
         |[5:70..5:75): scala => scala/
         |[5:76..5:79): Int => scala/Int#
         |[5:85..5:86): x => local15
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

  symbols(
    "typed parameters of partial function lambdas with a match and names of other symbols",
    """|object t232 {
       |  class Int
       |  object o { class Int extends t232.Int }
       |  type List[A] = scala.collection.Seq[A]
       |  val a: PartialFunction[o.Int, scala.Int] = (x: Int) => x match { case _ => 1 }
       |  val b: PartialFunction[scala.List[scala.Int], scala.Int] =
       |    (x: List[scala.Int]) => x match { case w => w.hashCode }
       |  val c: o.Int => scala.Int = (x: Int) => x match { case _ => 1 }
       |  val d: scala.List[scala.Int] => scala.Int =
       |    (x: List[scala.Int]) => x match { case w => w.hashCode }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Scala212 ->
          """|_empty_/t232. => final object t232 extends AnyRef { +7 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t232.Int# => class Int extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t232.Int#`<init>`(). => primary ctor <init>()
             |_empty_/t232.List# => type List[A] = Seq[A]
             |  A => _empty_/t232.List#[A]
             |  Seq => scala/collection/Seq#
             |_empty_/t232.List#[A] => typeparam A
             |_empty_/t232.a. => val method a: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => _empty_/t232.o.Int#
             |  Int => scala/Int#
             |_empty_/t232.b. => val method b: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t232.c. => val method c: Function1[Int, Int]
             |  Function1 => scala/Function1#
             |  Int => _empty_/t232.o.Int#
             |  Int => scala/Int#
             |_empty_/t232.d. => val method d: Function1[List[Int], Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t232.o. => final object o extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t232.o.Int# => class Int extends Int { +1 decls }
             |  Int => _empty_/t232.Int#
             |_empty_/t232.o.Int#`<init>`(). => primary ctor <init>()
             |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => _empty_/t232.o.Int#
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local6
             |  Int => _empty_/t232.o.Int#
             |  B1 => local5
             |  Int => scala/Int#
             |  x => local3
             |  default => local4
             |  Function1 => scala/Function1#
             |local2 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local7
             |  Int => _empty_/t232.o.Int#
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
             |  Int => _empty_/t232.o.Int#
             |local7 => param x: Int
             |  Int => _empty_/t232.o.Int#
             |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local9 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local14
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local13
             |  x => local11
             |  default => local12
             |  Function1 => scala/Function1#
             |local10 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local15
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local11 => synthetic param x: A1
             |  A1 => local14
             |local12 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local14
             |  B1 => local13
             |local13 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local14 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local15 => param x: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local16 => val local w: A1
             |  A1 => local14
             |local17 => param x: Int
             |  Int => _empty_/t232.Int#
             |local18 => param x: List[Int]
             |  List => _empty_/t232.List#
             |  Int => scala/Int#
             |local19 => val local w: List[Int]
             |  List => _empty_/t232.List#
             |  Int => scala/Int#
             |""".stripMargin,
        ScalaVersion.AtMost213(16) ->
          """|_empty_/t232. => final object t232 extends AnyRef { +7 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t232.Int# => class Int extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t232.Int#`<init>`(). => primary ctor <init>()
             |_empty_/t232.List# => type List[A] = Seq[A]
             |  A => _empty_/t232.List#[A]
             |  Seq => scala/collection/Seq#
             |_empty_/t232.List#[A] => typeparam A
             |_empty_/t232.a. => val method a: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => _empty_/t232.o.Int#
             |  Int => scala/Int#
             |_empty_/t232.b. => val method b: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t232.c. => val method c: Function1[Int, Int]
             |  Function1 => scala/Function1#
             |  Int => _empty_/t232.o.Int#
             |  Int => scala/Int#
             |_empty_/t232.d. => val method d: Function1[List[Int], Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t232.o. => final object o extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t232.o.Int# => class Int extends Int { +1 decls }
             |  Int => _empty_/t232.Int#
             |_empty_/t232.o.Int#`<init>`(). => primary ctor <init>()
             |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => _empty_/t232.o.Int#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local6
             |  Int => _empty_/t232.o.Int#
             |  B1 => local5
             |  Int => scala/Int#
             |  x => local3
             |  default => local4
             |  Function1 => scala/Function1#
             |local2 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local7
             |  Int => _empty_/t232.o.Int#
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
             |  Int => _empty_/t232.o.Int#
             |local7 => param x: Int
             |  Int => _empty_/t232.o.Int#
             |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local9 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local14
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local13
             |  x => local11
             |  default => local12
             |  Function1 => scala/Function1#
             |local10 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local15
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local11 => synthetic param x: A1
             |  A1 => local14
             |local12 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local14
             |  B1 => local13
             |local13 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local14 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local15 => param x: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local16 => val local w: A1
             |  A1 => local14
             |local17 => param x: Int
             |  Int => _empty_/t232.Int#
             |local18 => param x: List[Int]
             |  List => _empty_/t232.List#
             |  Int => scala/Int#
             |local19 => val local w: List[Int]
             |  List => _empty_/t232.List#
             |  Int => scala/Int#
             |""".stripMargin,
      ),
      """|_empty_/t232. => final object t232 extends AnyRef { +7 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t232.Int# => class Int extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t232.Int#`<init>`(). => primary ctor <init>()
         |_empty_/t232.List# => type List[A] = Seq[A]
         |  A => _empty_/t232.List#[A]
         |  Seq => scala/collection/Seq#
         |_empty_/t232.List#[A] => typeparam A
         |_empty_/t232.a. => val method a: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => _empty_/t232.o.Int#
         |  Int => scala/Int#
         |_empty_/t232.b. => val method b: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t232.c. => val method c: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => _empty_/t232.o.Int#
         |  Int => scala/Int#
         |_empty_/t232.d. => val method d: Function1[List[Int], Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t232.o. => final object o extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t232.o.Int# => class Int extends Int { +1 decls }
         |  Int => _empty_/t232.Int#
         |_empty_/t232.o.Int#`<init>`(). => primary ctor <init>()
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => _empty_/t232.Int#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => _empty_/t232.Int#
         |  B1 => local5
         |  Int => scala/Int#
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  Int => _empty_/t232.Int#
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
         |  Int => _empty_/t232.Int#
         |local7 => param x: Int
         |  Int => _empty_/t232.Int#
         |local8 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => _empty_/t232.List#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local9 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local14
         |  List => _empty_/t232.List#
         |  Int => scala/Int#
         |  B1 => local13
         |  x => local11
         |  default => local12
         |  Function1 => scala/Function1#
         |local10 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local15
         |  List => _empty_/t232.List#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local11 => synthetic param x: A1
         |  A1 => local14
         |local12 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local14
         |  B1 => local13
         |local13 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local14 => typeparam A1 <: List[Int]
         |  List => _empty_/t232.List#
         |  Int => scala/Int#
         |local15 => param x: List[Int]
         |  List => _empty_/t232.List#
         |  Int => scala/Int#
         |local16 => val local w: A1
         |  A1 => local14
         |local17 => param x: Int
         |  Int => _empty_/t232.Int#
         |local18 => param x: List[Int]
         |  List => _empty_/t232.List#
         |  Int => scala/Int#
         |local19 => val local w: List[Int]
         |  List => _empty_/t232.List#
         |  Int => scala/Int#
         |""".stripMargin,
    ),
  )

  occurrences(
    "typed parameters of partial function lambdas with a match and names of other symbols",
    """|object t232 {
       |  class Int
       |  object o { class Int extends t232.Int }
       |  type List[A] = scala.collection.Seq[A]
       |  val a: PartialFunction[o.Int, scala.Int] = (x: Int) => x match { case _ => 1 }
       |  val b: PartialFunction[scala.List[scala.Int], scala.Int] =
       |    (x: List[scala.Int]) => x match { case w => w.hashCode }
       |  val c: o.Int => scala.Int = (x: Int) => x match { case _ => 1 }
       |  val d: scala.List[scala.Int] => scala.Int =
       |    (x: List[scala.Int]) => x match { case w => w.hashCode }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Scala212 ->
          """|[0:7..0:11): t232 <= _empty_/t232.
             |[1:8..1:11): Int <= _empty_/t232.Int#
             |[1:11..1:11):  <= _empty_/t232.Int#`<init>`().
             |[2:9..2:10): o <= _empty_/t232.o.
             |[2:19..2:22): Int <= _empty_/t232.o.Int#
             |[2:23..2:23):  <= _empty_/t232.o.Int#`<init>`().
             |[2:31..2:35): t232 => _empty_/t232.
             |[2:36..2:39): Int => _empty_/t232.Int#
             |[2:39..2:39):  => _empty_/t232.Int#`<init>`().
             |[3:7..3:11): List <= _empty_/t232.List#
             |[3:12..3:13): A <= _empty_/t232.List#[A]
             |[3:17..3:22): scala => scala/
             |[3:23..3:33): collection => scala/collection/
             |[3:34..3:37): Seq => scala/collection/Seq#
             |[3:38..3:39): A => _empty_/t232.List#[A]
             |[4:6..4:7): a <= _empty_/t232.a.
             |[4:9..4:24): PartialFunction => scala/PartialFunction#
             |[4:25..4:26): o => _empty_/t232.o.
             |[4:27..4:30): Int => _empty_/t232.o.Int#
             |[4:32..4:37): scala => scala/
             |[4:38..4:41): Int => scala/Int#
             |[4:45..4:80): (x: Int) => x match { case _ => 1 } <= local0
             |[4:46..4:47): x <= local7
             |[4:57..4:58): x => local7
             |[5:6..5:7): b <= _empty_/t232.b.
             |[5:9..5:24): PartialFunction => scala/PartialFunction#
             |[5:25..5:30): scala => scala/
             |[5:31..5:35): List => scala/package.List#
             |[5:36..5:41): scala => scala/
             |[5:42..5:45): Int => scala/Int#
             |[5:48..5:53): scala => scala/
             |[5:54..5:57): Int => scala/Int#
             |[6:4..6:60): (x: List[scala.Int]) => x match { case w => w.hashCode } <= local8
             |[6:5..6:6): x <= local15
             |[6:28..6:29): x => local15
             |[6:43..6:44): w <= local16
             |[6:48..6:49): w => local16
             |[6:50..6:58): hashCode => scala/collection/LinearSeqLike#hashCode().
             |[7:6..7:7): c <= _empty_/t232.c.
             |[7:9..7:10): o => _empty_/t232.o.
             |[7:11..7:14): Int => _empty_/t232.o.Int#
             |[7:18..7:23): scala => scala/
             |[7:24..7:27): Int => scala/Int#
             |[7:31..7:32): x <= local17
             |[7:34..7:37): Int => _empty_/t232.Int#
             |[7:42..7:43): x => local17
             |[8:6..8:7): d <= _empty_/t232.d.
             |[8:9..8:14): scala => scala/
             |[8:15..8:19): List => scala/package.List#
             |[8:20..8:25): scala => scala/
             |[8:26..8:29): Int => scala/Int#
             |[8:34..8:39): scala => scala/
             |[8:40..8:43): Int => scala/Int#
             |[9:5..9:6): x <= local18
             |[9:8..9:12): List => _empty_/t232.List#
             |[9:13..9:18): scala => scala/
             |[9:19..9:22): Int => scala/Int#
             |[9:28..9:29): x => local18
             |[9:43..9:44): w <= local19
             |[9:48..9:49): w => local19
             |[9:50..9:58): hashCode => scala/collection/GenSeqLike#hashCode().
             |""".stripMargin,
        ScalaVersion.AtMost213(16) ->
          """|[0:7..0:11): t232 <= _empty_/t232.
             |[1:8..1:11): Int <= _empty_/t232.Int#
             |[1:11..1:11):  <= _empty_/t232.Int#`<init>`().
             |[2:9..2:10): o <= _empty_/t232.o.
             |[2:19..2:22): Int <= _empty_/t232.o.Int#
             |[2:23..2:23):  <= _empty_/t232.o.Int#`<init>`().
             |[2:31..2:35): t232 => _empty_/t232.
             |[2:36..2:39): Int => _empty_/t232.Int#
             |[2:39..2:39):  => _empty_/t232.Int#`<init>`().
             |[3:7..3:11): List <= _empty_/t232.List#
             |[3:12..3:13): A <= _empty_/t232.List#[A]
             |[3:17..3:22): scala => scala/
             |[3:23..3:33): collection => scala/collection/
             |[3:34..3:37): Seq => scala/collection/Seq#
             |[3:38..3:39): A => _empty_/t232.List#[A]
             |[4:6..4:7): a <= _empty_/t232.a.
             |[4:9..4:24): PartialFunction => scala/PartialFunction#
             |[4:25..4:26): o => _empty_/t232.o.
             |[4:27..4:30): Int => _empty_/t232.o.Int#
             |[4:32..4:37): scala => scala/
             |[4:38..4:41): Int => scala/Int#
             |[4:45..4:80): (x: Int) => x match { case _ => 1 } <= local0
             |[4:46..4:47): x <= local7
             |[4:57..4:58): x => local7
             |[5:6..5:7): b <= _empty_/t232.b.
             |[5:9..5:24): PartialFunction => scala/PartialFunction#
             |[5:25..5:30): scala => scala/
             |[5:31..5:35): List => scala/package.List#
             |[5:36..5:41): scala => scala/
             |[5:42..5:45): Int => scala/Int#
             |[5:48..5:53): scala => scala/
             |[5:54..5:57): Int => scala/Int#
             |[6:4..6:60): (x: List[scala.Int]) => x match { case w => w.hashCode } <= local8
             |[6:5..6:6): x <= local15
             |[6:28..6:29): x => local15
             |[6:43..6:44): w <= local16
             |[6:48..6:49): w => local16
             |[6:50..6:58): hashCode => scala/collection/Seq#hashCode().
             |[7:6..7:7): c <= _empty_/t232.c.
             |[7:9..7:10): o => _empty_/t232.o.
             |[7:11..7:14): Int => _empty_/t232.o.Int#
             |[7:18..7:23): scala => scala/
             |[7:24..7:27): Int => scala/Int#
             |[7:31..7:32): x <= local17
             |[7:34..7:37): Int => _empty_/t232.Int#
             |[7:42..7:43): x => local17
             |[8:6..8:7): d <= _empty_/t232.d.
             |[8:9..8:14): scala => scala/
             |[8:15..8:19): List => scala/package.List#
             |[8:20..8:25): scala => scala/
             |[8:26..8:29): Int => scala/Int#
             |[8:34..8:39): scala => scala/
             |[8:40..8:43): Int => scala/Int#
             |[9:5..9:6): x <= local18
             |[9:8..9:12): List => _empty_/t232.List#
             |[9:13..9:18): scala => scala/
             |[9:19..9:22): Int => scala/Int#
             |[9:28..9:29): x => local18
             |[9:43..9:44): w <= local19
             |[9:48..9:49): w => local19
             |[9:50..9:58): hashCode => scala/collection/Seq#hashCode().
             |""".stripMargin,
      ),
      """|[0:7..0:11): t232 <= _empty_/t232.
         |[1:8..1:11): Int <= _empty_/t232.Int#
         |[1:11..1:11):  <= _empty_/t232.Int#`<init>`().
         |[2:9..2:10): o <= _empty_/t232.o.
         |[2:19..2:22): Int <= _empty_/t232.o.Int#
         |[2:23..2:23):  <= _empty_/t232.o.Int#`<init>`().
         |[2:31..2:35): t232 => _empty_/t232.
         |[2:36..2:39): Int => _empty_/t232.Int#
         |[2:39..2:39):  => _empty_/t232.Int#`<init>`().
         |[3:7..3:11): List <= _empty_/t232.List#
         |[3:12..3:13): A <= _empty_/t232.List#[A]
         |[3:17..3:22): scala => scala/
         |[3:23..3:33): collection => scala/collection/
         |[3:34..3:37): Seq => scala/collection/Seq#
         |[3:38..3:39): A => _empty_/t232.List#[A]
         |[4:6..4:7): a <= _empty_/t232.a.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:26): o => _empty_/t232.o.
         |[4:27..4:30): Int => _empty_/t232.o.Int#
         |[4:32..4:37): scala => scala/
         |[4:38..4:41): Int => scala/Int#
         |[4:45..4:80): (x: Int) => x match { case _ => 1 } <= local0
         |[4:46..4:47): x <= local7
         |[4:49..4:52): Int => _empty_/t232.Int#
         |[4:57..4:58): x => local7
         |[5:6..5:7): b <= _empty_/t232.b.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:30): scala => scala/
         |[5:31..5:35): List => scala/package.List#
         |[5:36..5:41): scala => scala/
         |[5:42..5:45): Int => scala/Int#
         |[5:48..5:53): scala => scala/
         |[5:54..5:57): Int => scala/Int#
         |[6:4..6:60): (x: List[scala.Int]) => x match { case w => w.hashCode } <= local8
         |[6:5..6:6): x <= local15
         |[6:8..6:12): List => _empty_/t232.List#
         |[6:13..6:18): scala => scala/
         |[6:19..6:22): Int => scala/Int#
         |[6:28..6:29): x => local15
         |[6:43..6:44): w <= local16
         |[6:48..6:49): w => local16
         |[6:50..6:58): hashCode => scala/collection/Seq#hashCode().
         |[7:6..7:7): c <= _empty_/t232.c.
         |[7:9..7:10): o => _empty_/t232.o.
         |[7:11..7:14): Int => _empty_/t232.o.Int#
         |[7:18..7:23): scala => scala/
         |[7:24..7:27): Int => scala/Int#
         |[7:31..7:32): x <= local17
         |[7:34..7:37): Int => _empty_/t232.Int#
         |[7:42..7:43): x => local17
         |[8:6..8:7): d <= _empty_/t232.d.
         |[8:9..8:14): scala => scala/
         |[8:15..8:19): List => scala/package.List#
         |[8:20..8:25): scala => scala/
         |[8:26..8:29): Int => scala/Int#
         |[8:34..8:39): scala => scala/
         |[8:40..8:43): Int => scala/Int#
         |[9:5..9:6): x <= local18
         |[9:8..9:12): List => _empty_/t232.List#
         |[9:13..9:18): scala => scala/
         |[9:19..9:22): Int => scala/Int#
         |[9:28..9:29): x => local18
         |[9:43..9:44): w <= local19
         |[9:48..9:49): w => local19
         |[9:50..9:58): hashCode => scala/collection/Seq#hashCode().
         |""".stripMargin,
    ),
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
    if (!ScalaVersion.atLeast213_17)
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
         |[3:37..3:48): (x: C) => 1 <= local0
         |[3:38..3:39): x <= local7
         |[4:6..4:7): b <= _empty_/t233.b.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:28): Int => scala/Int#
         |[4:30..4:33): Int => scala/Int#
         |[4:37..4:48): (x: J) => x <= local8
         |[4:38..4:39): x <= local15
         |[4:47..4:48): x => local15
         |[5:6..5:7): c <= _empty_/t233.c.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:26): p => _empty_/t233.p.
         |[5:27..5:28): B => _empty_/t233.p.B#
         |[5:30..5:33): Int => scala/Int#
         |[5:37..5:70): (x: C) => x match { case _ => 1 } <= local16
         |[5:38..5:39): x <= local23
         |[5:47..5:48): x => local23
         |[6:6..6:7): d <= _empty_/t233.d.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:28): Int => scala/Int#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:70): (x: J) => x match { case y => y } <= local24
         |[6:38..6:39): x <= local31
         |[6:47..6:48): x => local31
         |[6:62..6:63): y <= local32
         |[6:67..6:68): y => local32
         |""".stripMargin
    else
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
         |[3:37..3:48): (x: C) => 1 <= local0
         |[3:38..3:39): x <= local7
         |[3:41..3:42): C => _empty_/t233.p.B#
         |[4:6..4:7): b <= _empty_/t233.b.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:28): Int => scala/Int#
         |[4:30..4:33): Int => scala/Int#
         |[4:37..4:48): (x: J) => x <= local8
         |[4:38..4:39): x <= local15
         |[4:41..4:42): J => _empty_/t233.p.I#
         |[4:47..4:48): x => local15
         |[5:6..5:7): c <= _empty_/t233.c.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:26): p => _empty_/t233.p.
         |[5:27..5:28): B => _empty_/t233.p.B#
         |[5:30..5:33): Int => scala/Int#
         |[5:37..5:70): (x: C) => x match { case _ => 1 } <= local16
         |[5:38..5:39): x <= local23
         |[5:41..5:42): C => _empty_/t233.p.B#
         |[5:47..5:48): x => local23
         |[6:6..6:7): d <= _empty_/t233.d.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:28): Int => scala/Int#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:70): (x: J) => x match { case y => y } <= local24
         |[6:38..6:39): x <= local31
         |[6:41..6:42): J => _empty_/t233.p.I#
         |[6:47..6:48): x => local31
         |[6:62..6:63): y <= local32
         |[6:67..6:68): y => local32
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas through a self alias",
    """|class t234 { self =>
       |  type T = Int
       |  val a: PartialFunction[Int, Int] = (x: self.T) => x match { case y => y }
       |  val b: Int => Int = (x: self.T) => x match { case y => y }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:6..0:10): t234 <= _empty_/t234#
         |[0:11..0:11):  <= _empty_/t234#`<init>`().
         |[0:13..0:17): self <= local0
         |[1:7..1:8): T <= _empty_/t234#T#
         |[1:11..1:14): Int => scala/Int#
         |[2:6..2:7): a <= _empty_/t234#a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:28): Int => scala/Int#
         |[2:30..2:33): Int => scala/Int#
         |[2:37..2:75): (x: self.T) => x match { case y => y } <= local1
         |[2:38..2:39): x <= local8
         |[2:41..2:45): self => local0
         |[2:46..2:47): T => _empty_/t234#T#
         |[2:52..2:53): x => local8
         |[2:67..2:68): y <= local9
         |[2:72..2:73): y => local9
         |[3:6..3:7): b <= _empty_/t234#b.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:24): x <= local10
         |[3:26..3:30): self => local0
         |[3:31..3:32): T => _empty_/t234#T#
         |[3:37..3:38): x => local10
         |[3:52..3:53): y <= local11
         |[3:57..3:58): y => local11
         |""".stripMargin
    else
      """|[0:6..0:10): t234 <= _empty_/t234#
         |[0:11..0:11):  <= _empty_/t234#`<init>`().
         |[0:13..0:17): self <= local0
         |[1:7..1:8): T <= _empty_/t234#T#
         |[1:11..1:14): Int => scala/Int#
         |[2:6..2:7): a <= _empty_/t234#a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:28): Int => scala/Int#
         |[2:30..2:33): Int => scala/Int#
         |[2:37..2:75): (x: self.T) => x match { case y => y } <= local1
         |[2:38..2:39): x <= local8
         |[2:52..2:53): x => local8
         |[2:67..2:68): y <= local9
         |[2:72..2:73): y => local9
         |[3:6..3:7): b <= _empty_/t234#b.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:24): x <= local10
         |[3:26..3:30): self => local0
         |[3:31..3:32): T => _empty_/t234#T#
         |[3:37..3:38): x => local10
         |[3:52..3:53): y <= local11
         |[3:57..3:58): y => local11
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas through a package object",
    """|package object t235p { type T = Int }
       |object t235 {
       |  val a: PartialFunction[Int, Int] = (x: t235p.T) => x match { case y => y }
       |  val b: Int => Int = (x: t235p.T) => x match { case y => y }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:15..0:20): t235p <= t235p/package.
         |[0:28..0:29): T <= t235p/package.T#
         |[0:32..0:35): Int => scala/Int#
         |[1:7..1:11): t235 <= _empty_/t235.
         |[2:6..2:7): a <= _empty_/t235.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:28): Int => scala/Int#
         |[2:30..2:33): Int => scala/Int#
         |[2:37..2:76): (x: t235p.T) => x match { case y => y } <= local0
         |[2:38..2:39): x <= local7
         |[2:41..2:46): t235p => t235p/
         |[2:47..2:48): T => t235p/package.T#
         |[2:53..2:54): x => local7
         |[2:68..2:69): y <= local8
         |[2:73..2:74): y => local8
         |[3:6..3:7): b <= _empty_/t235.b.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:24): x <= local9
         |[3:26..3:31): t235p => t235p/
         |[3:32..3:33): T => t235p/package.T#
         |[3:38..3:39): x => local9
         |[3:53..3:54): y <= local10
         |[3:58..3:59): y => local10
         |""".stripMargin
    else
      """|[0:15..0:20): t235p <= t235p/package.
         |[0:28..0:29): T <= t235p/package.T#
         |[0:32..0:35): Int => scala/Int#
         |[1:7..1:11): t235 <= _empty_/t235.
         |[2:6..2:7): a <= _empty_/t235.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:28): Int => scala/Int#
         |[2:30..2:33): Int => scala/Int#
         |[2:37..2:76): (x: t235p.T) => x match { case y => y } <= local0
         |[2:38..2:39): x <= local7
         |[2:53..2:54): x => local7
         |[2:68..2:69): y <= local8
         |[2:73..2:74): y => local8
         |[3:6..3:7): b <= _empty_/t235.b.
         |[3:9..3:12): Int => scala/Int#
         |[3:16..3:19): Int => scala/Int#
         |[3:23..3:24): x <= local9
         |[3:26..3:31): t235p => t235p/
         |[3:32..3:33): T => t235p/package.T#
         |[3:38..3:39): x => local9
         |[3:53..3:54): y <= local10
         |[3:58..3:59): y => local10
         |""".stripMargin,
  )

  symbols(
    "typed parameters of partial function lambdas through a renamed package",
    """|import java.{util => ju}
       |object t236 {
       |  val a: PartialFunction[ju.List[Int], Int] = (x: ju.List[Int]) => x match { case _ => 1 }
       |  val b: ju.List[Int] => Int = (x: ju.List[Int]) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t236. => final object t236 extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t236.a. => val method a: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => java/util/List#
         |  Int => scala/Int#
         |_empty_/t236.b. => val method b: Function1[List[Int], Int]
         |  Function1 => scala/Function1#
         |  List => java/util/List#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => java/util/List#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  List => java/util/List#
         |  Int => scala/Int#
         |  B1 => local5
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  List => java/util/List#
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
         |local6 => typeparam A1 <: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |local7 => param x: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |local8 => param x: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t236. => final object t236 extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t236.a. => val method a: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => java/util/List#
         |  Int => scala/Int#
         |_empty_/t236.b. => val method b: Function1[List[Int], Int]
         |  Function1 => scala/Function1#
         |  List => java/util/List#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => java/util/List#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  List => java/util/List#
         |  Int => scala/Int#
         |  B1 => local5
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  List => java/util/List#
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
         |local6 => typeparam A1 <: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |local7 => param x: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |local8 => param x: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas through a renamed package",
    """|import java.{util => ju}
       |object t236 {
       |  val a: PartialFunction[ju.List[Int], Int] = (x: ju.List[Int]) => x match { case _ => 1 }
       |  val b: ju.List[Int] => Int = (x: ju.List[Int]) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): java => java/
         |[0:13..0:17): util => java/util/
         |[1:7..1:11): t236 <= _empty_/t236.
         |[2:6..2:7): a <= _empty_/t236.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:27): ju => java/util/
         |[2:28..2:32): List => java/util/List#
         |[2:33..2:36): Int => scala/Int#
         |[2:39..2:42): Int => scala/Int#
         |[2:46..2:90): (x: ju.List[Int]) => x match { case _ => 1 } <= local0
         |[2:47..2:48): x <= local7
         |[2:50..2:52): ju => java/util/
         |[2:53..2:57): List => java/util/List#
         |[2:58..2:61): Int => scala/Int#
         |[2:67..2:68): x => local7
         |[3:6..3:7): b <= _empty_/t236.b.
         |[3:9..3:11): ju => java/util/
         |[3:12..3:16): List => java/util/List#
         |[3:17..3:20): Int => scala/Int#
         |[3:25..3:28): Int => scala/Int#
         |[3:32..3:33): x <= local8
         |[3:35..3:37): ju => java/util/
         |[3:38..3:42): List => java/util/List#
         |[3:43..3:46): Int => scala/Int#
         |[3:52..3:53): x => local8
         |""".stripMargin
    else
      """|[0:7..0:11): java => java/
         |[0:13..0:17): util => java/util/
         |[1:7..1:11): t236 <= _empty_/t236.
         |[2:6..2:7): a <= _empty_/t236.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:27): ju => java/util/
         |[2:28..2:32): List => java/util/List#
         |[2:33..2:36): Int => scala/Int#
         |[2:39..2:42): Int => scala/Int#
         |[2:46..2:90): (x: ju.List[Int]) => x match { case _ => 1 } <= local0
         |[2:47..2:48): x <= local7
         |[2:67..2:68): x => local7
         |[3:6..3:7): b <= _empty_/t236.b.
         |[3:9..3:11): ju => java/util/
         |[3:12..3:16): List => java/util/List#
         |[3:17..3:20): Int => scala/Int#
         |[3:25..3:28): Int => scala/Int#
         |[3:32..3:33): x <= local8
         |[3:35..3:37): ju => java/util/
         |[3:38..3:42): List => java/util/List#
         |[3:43..3:46): Int => scala/Int#
         |[3:52..3:53): x => local8
         |""".stripMargin,
  )

  symbols(
    "typed parameters of partial function lambdas with an existential of the expected type",
    """|object t237 {
       |  val a: PartialFunction[List[_], Int] = (x: List[_]) => x match { case _ => 1 }
       |  val b: List[_] => Int = (x: List[_]) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Scala212 ->
          """|_empty_/t237. => final object t237 extends AnyRef { +2 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t237.a. => val method a: PartialFunction[List[_] forSome { type _ }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  _ => local1
             |  Int => scala/Int#
             |_empty_/t237.b. => val method b: Function1[List[_] forSome { type _ }, Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  _ => local11
             |  Int => scala/Int#
             |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  _ => local1
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local1 => synthetic abstract type _
             |local2 => final method applyOrElse[A1 <: List[_] forSome { type _ }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local7
             |  List => scala/package.List#
             |  _ => local1
             |  B1 => local6
             |  Int => scala/Int#
             |  x => local4
             |  default => local5
             |  Function1 => scala/Function1#
             |local3 => final method isDefinedAt(x: List[_] forSome { type _ }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local8
             |  List => scala/package.List#
             |  _ => local1
             |  Boolean => scala/Boolean#
             |local4 => synthetic param x: A1
             |  A1 => local7
             |local5 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local7
             |  B1 => local6
             |local6 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local7 => typeparam A1 <: List[_] forSome { type _ }
             |  List => scala/package.List#
             |  _ => local1
             |local8 => param x: List[_] forSome { type _ }
             |  List => scala/package.List#
             |  _ => local1
             |local9 => param x: List[_] forSome { type _ }
             |  List => scala/package.List#
             |  _ => local10
             |local10 => synthetic abstract type _
             |local11 => synthetic abstract type _
             |""".stripMargin,
        ScalaVersion.AtMost213(16) ->
          """|_empty_/t237. => final object t237 extends AnyRef { +2 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t237.a. => val method a: PartialFunction[List[_] forSome { type _ }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  _ => local1
             |  Int => scala/Int#
             |_empty_/t237.b. => val method b: Function1[List[_] forSome { type _ }, Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  _ => local11
             |  Int => scala/Int#
             |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  _ => local1
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local1 => synthetic abstract type _
             |local2 => final method applyOrElse[A1 <: List[_] forSome { type _ }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local7
             |  List => scala/package.List#
             |  _ => local1
             |  B1 => local6
             |  Int => scala/Int#
             |  x => local4
             |  default => local5
             |  Function1 => scala/Function1#
             |local3 => final method isDefinedAt(x: List[_] forSome { type _ }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local8
             |  List => scala/package.List#
             |  _ => local1
             |  Boolean => scala/Boolean#
             |local4 => synthetic param x: A1
             |  A1 => local7
             |local5 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local7
             |  B1 => local6
             |local6 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local7 => typeparam A1 <: List[_] forSome { type _ }
             |  List => scala/package.List#
             |  _ => local1
             |local8 => param x: List[_] forSome { type _ }
             |  List => scala/package.List#
             |  _ => local1
             |local9 => param x: List[_] forSome { type _ }
             |  List => scala/package.List#
             |  _ => local10
             |local10 => synthetic abstract type _
             |local11 => synthetic abstract type _
             |""".stripMargin,
      ),
      """|_empty_/t237. => final object t237 extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t237.a. => val method a: PartialFunction[List[_] forSome { type _ }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  _ => local9
         |  Int => scala/Int#
         |_empty_/t237.b. => val method b: Function1[List[_] forSome { type _ }, Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  _ => local12
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  _ => local1
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => abstract type _
         |local2 => final method applyOrElse[A1 <: List[_] forSome { type _ }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local7
         |  List => scala/package.List#
         |  _ => local1
         |  B1 => local6
         |  Int => scala/Int#
         |  x => local4
         |  default => local5
         |  Function1 => scala/Function1#
         |local3 => final method isDefinedAt(x: List[_] forSome { type _ }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local8
         |  List => scala/package.List#
         |  _ => local1
         |  Boolean => scala/Boolean#
         |local4 => synthetic param x: A1
         |  A1 => local7
         |local5 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local7
         |  B1 => local6
         |local6 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local7 => typeparam A1 <: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local1
         |local8 => param x: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local1
         |local9 => synthetic abstract type _
         |local10 => param x: List[_] forSome { type _ }
         |  List => scala/package.List#
         |  _ => local11
         |local11 => synthetic abstract type _
         |local12 => synthetic abstract type _
         |""".stripMargin,
    ),
  )

  occurrences(
    "typed parameters of partial function lambdas with an existential of the expected type",
    """|object t237 {
       |  val a: PartialFunction[List[_], Int] = (x: List[_]) => x match { case _ => 1 }
       |  val b: List[_] => Int = (x: List[_]) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t237 <= _empty_/t237.
         |[1:6..1:7): a <= _empty_/t237.a.
         |[1:9..1:24): PartialFunction => scala/PartialFunction#
         |[1:25..1:29): List => scala/package.List#
         |[1:30..1:31): _ <= local9
         |[1:34..1:37): Int => scala/Int#
         |[1:41..1:80): (x: List[_]) => x match { case _ => 1 } <= local0
         |[1:42..1:43): x <= local8
         |[1:45..1:49): List => scala/package.List#
         |[1:50..1:51): _ <= local1
         |[1:57..1:58): x => local8
         |[2:6..2:7): b <= _empty_/t237.b.
         |[2:9..2:13): List => scala/package.List#
         |[2:14..2:15): _ <= local12
         |[2:20..2:23): Int => scala/Int#
         |[2:27..2:28): x <= local10
         |[2:30..2:34): List => scala/package.List#
         |[2:35..2:36): _ <= local11
         |[2:42..2:43): x => local10
         |""".stripMargin
    else
      """|[0:7..0:11): t237 <= _empty_/t237.
         |[1:6..1:7): a <= _empty_/t237.a.
         |[1:9..1:24): PartialFunction => scala/PartialFunction#
         |[1:25..1:29): List => scala/package.List#
         |[1:30..1:31): _ <= local1
         |[1:34..1:37): Int => scala/Int#
         |[1:41..1:80): (x: List[_]) => x match { case _ => 1 } <= local0
         |[1:42..1:43): x <= local8
         |[1:57..1:58): x => local8
         |[2:6..2:7): b <= _empty_/t237.b.
         |[2:9..2:13): List => scala/package.List#
         |[2:14..2:15): _ <= local11
         |[2:20..2:23): Int => scala/Int#
         |[2:27..2:28): x <= local9
         |[2:30..2:34): List => scala/package.List#
         |[2:35..2:36): _ <= local10
         |[2:42..2:43): x => local9
         |""".stripMargin,
  )

  symbols(
    "wildcard, placeholder, nested and argument partial function lambdas with a match",
    """|object t222 {
       |  val a: PartialFunction[Int, Int] = _ => 1 match { case w => w }
       |  val b: PartialFunction[Int, String] = _.toString match { case s => s }
       |  val c: PartialFunction[Int, PartialFunction[Int, Int]] =
       |    x => x match { case v => y => y match { case w => x + v + w } }
       |  val d: PartialFunction[Int, PartialFunction[Int, Int]] =
       |    _ match { case v => _ match { case w => v + w } }
       |  val e = List(1).collect(x => x match { case w => w })
       |  val f = List(1).collect { _ match { case w => w } }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t222. => final object t222 extends AnyRef { +6 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t222.a. => val method a: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t222.b. => val method b: PartialFunction[Int, String]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  String => scala/Predef.String#
         |_empty_/t222.c. => val method c: PartialFunction[Int, PartialFunction[Int, Int]]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t222.d. => val method d: PartialFunction[Int, PartialFunction[Int, Int]]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t222.e. => val method e: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t222.f. => val method f: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x$1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local7
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x$1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => param x$1: Int
         |  Int => scala/Int#
         |local8 => val local w: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, String] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  String => scala/Predef.String#
         |  Serializable => scala/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: String](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  String => scala/Predef.String#
         |  x$2 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x$2: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$2 => local16
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x$2: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: String
         |  String => scala/Predef.String#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$2: Int
         |  Int => scala/Int#
         |local17 => val local s: String
         |  String => java/lang/String#
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, PartialFunction[Int, Int]] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  PartialFunction => scala/PartialFunction#
         |  Serializable => scala/Serializable#
         |local19 => final method applyOrElse[A1 <: Int, B1 >: PartialFunction[Int, Int]](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  Int => scala/Int#
         |  B1 => local23
         |  PartialFunction => scala/PartialFunction#
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local24 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local25 => param x: Int
         |  Int => scala/Int#
         |local26 => val local v: A1
         |  A1 => local24
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local28 => final method applyOrElse[A1 <: Int, B1 >: Int](y: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  Int => scala/Int#
         |  B1 => local32
         |  y => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(y: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  y => local34
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param y: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local34 => param y: Int
         |  Int => scala/Int#
         |local35 => val local w: A1
         |  A1 => local33
         |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, PartialFunction[Int, Int]] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  PartialFunction => scala/PartialFunction#
         |  Serializable => scala/Serializable#
         |local37 => final method applyOrElse[A1 <: Int, B1 >: PartialFunction[Int, Int]](x$3: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local42
         |  Int => scala/Int#
         |  B1 => local41
         |  PartialFunction => scala/PartialFunction#
         |  x$3 => local39
         |  default => local40
         |  Function1 => scala/Function1#
         |local38 => final method isDefinedAt(x$3: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$3 => local43
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local39 => synthetic param x$3: A1
         |  A1 => local42
         |local40 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local42
         |  B1 => local41
         |local41 => typeparam B1 >: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local42 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local43 => synthetic param x$3: Int
         |  Int => scala/Int#
         |local44 => val local v: A1
         |  A1 => local42
         |local45 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local46 => final method applyOrElse[A1 <: Int, B1 >: Int](x$4: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local51
         |  Int => scala/Int#
         |  B1 => local50
         |  x$4 => local48
         |  default => local49
         |  Function1 => scala/Function1#
         |local47 => final method isDefinedAt(x$4: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$4 => local52
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local48 => synthetic param x$4: A1
         |  A1 => local51
         |local49 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local51
         |  B1 => local50
         |local50 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local51 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local52 => synthetic param x$4: Int
         |  Int => scala/Int#
         |local53 => val local w: A1
         |  A1 => local51
         |local54 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local55 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local54
         |  Serializable => scala/Serializable#
         |local56 => final method applyOrElse[A1 <: Int, B1 >: A1](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local54
         |  Int => scala/Int#
         |  B1 => local60
         |  x => local58
         |  default => local59
         |  Function1 => scala/Function1#
         |local57 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local61
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local58 => synthetic param x: A1
         |  A1 => local54
         |local59 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local54
         |  B1 => local60
         |local60 => typeparam B1 >: A1
         |  A1 => local54
         |local61 => param x: Int
         |  Int => scala/Int#
         |local62 => val local w: A1
         |  A1 => local54
         |local63 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local64 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local63
         |  Serializable => scala/Serializable#
         |local65 => final method applyOrElse[A1 <: Int, B1 >: A1](x$5: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local63
         |  Int => scala/Int#
         |  B1 => local69
         |  x$5 => local67
         |  default => local68
         |  Function1 => scala/Function1#
         |local66 => final method isDefinedAt(x$5: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$5 => local70
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local67 => synthetic param x$5: A1
         |  A1 => local63
         |local68 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local63
         |  B1 => local69
         |local69 => typeparam B1 >: A1
         |  A1 => local63
         |local70 => synthetic param x$5: Int
         |  Int => scala/Int#
         |local71 => val local w: A1
         |  A1 => local63
         |""".stripMargin
    else
      """|_empty_/t222. => final object t222 extends AnyRef { +6 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t222.a. => val method a: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t222.b. => val method b: PartialFunction[Int, String]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  String => scala/Predef.String#
         |_empty_/t222.c. => val method c: PartialFunction[Int, PartialFunction[Int, Int]]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t222.d. => val method d: PartialFunction[Int, PartialFunction[Int, Int]]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t222.e. => val method e: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t222.f. => val method f: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x$1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local7
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x$1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => param x$1: Int
         |  Int => scala/Int#
         |local8 => val local w: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, String] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  String => scala/Predef.String#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: String](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  String => scala/Predef.String#
         |  x$2 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x$2: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$2 => local16
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x$2: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: String
         |  String => scala/Predef.String#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$2: Int
         |  Int => scala/Int#
         |local17 => val local s: String
         |  String => java/lang/String#
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, PartialFunction[Int, Int]] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  PartialFunction => scala/PartialFunction#
         |  Serializable => java/io/Serializable#
         |local19 => final method applyOrElse[A1 <: Int, B1 >: PartialFunction[Int, Int]](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  Int => scala/Int#
         |  B1 => local23
         |  PartialFunction => scala/PartialFunction#
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local24 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local25 => param x: Int
         |  Int => scala/Int#
         |local26 => val local v: A1
         |  A1 => local24
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local28 => final method applyOrElse[A1 <: Int, B1 >: Int](y: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  Int => scala/Int#
         |  B1 => local32
         |  y => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(y: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  y => local34
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param y: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local34 => param y: Int
         |  Int => scala/Int#
         |local35 => val local w: A1
         |  A1 => local33
         |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, PartialFunction[Int, Int]] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  PartialFunction => scala/PartialFunction#
         |  Serializable => java/io/Serializable#
         |local37 => final method applyOrElse[A1 <: Int, B1 >: PartialFunction[Int, Int]](x$3: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local42
         |  Int => scala/Int#
         |  B1 => local41
         |  PartialFunction => scala/PartialFunction#
         |  x$3 => local39
         |  default => local40
         |  Function1 => scala/Function1#
         |local38 => final method isDefinedAt(x$3: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$3 => local43
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local39 => synthetic param x$3: A1
         |  A1 => local42
         |local40 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local42
         |  B1 => local41
         |local41 => typeparam B1 >: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local42 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local43 => synthetic param x$3: Int
         |  Int => scala/Int#
         |local44 => val local v: A1
         |  A1 => local42
         |local45 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local46 => final method applyOrElse[A1 <: Int, B1 >: Int](x$4: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local51
         |  Int => scala/Int#
         |  B1 => local50
         |  x$4 => local48
         |  default => local49
         |  Function1 => scala/Function1#
         |local47 => final method isDefinedAt(x$4: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$4 => local52
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local48 => synthetic param x$4: A1
         |  A1 => local51
         |local49 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local51
         |  B1 => local50
         |local50 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local51 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local52 => synthetic param x$4: Int
         |  Int => scala/Int#
         |local53 => val local w: A1
         |  A1 => local51
         |local54 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local55 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local54
         |  Serializable => java/io/Serializable#
         |local56 => final method applyOrElse[A1 <: Int, B1 >: A1](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local54
         |  Int => scala/Int#
         |  B1 => local60
         |  x => local58
         |  default => local59
         |  Function1 => scala/Function1#
         |local57 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local61
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local58 => synthetic param x: A1
         |  A1 => local54
         |local59 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local54
         |  B1 => local60
         |local60 => typeparam B1 >: A1
         |  A1 => local54
         |local61 => param x: Int
         |  Int => scala/Int#
         |local62 => val local w: A1
         |  A1 => local54
         |local63 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local64 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, A1] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  A1 => local63
         |  Serializable => java/io/Serializable#
         |local65 => final method applyOrElse[A1 <: Int, B1 >: A1](x$5: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local63
         |  Int => scala/Int#
         |  B1 => local69
         |  x$5 => local67
         |  default => local68
         |  Function1 => scala/Function1#
         |local66 => final method isDefinedAt(x$5: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$5 => local70
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local67 => synthetic param x$5: A1
         |  A1 => local63
         |local68 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local63
         |  B1 => local69
         |local69 => typeparam B1 >: A1
         |  A1 => local63
         |local70 => synthetic param x$5: Int
         |  Int => scala/Int#
         |local71 => val local w: A1
         |  A1 => local63
         |""".stripMargin,
  )

  occurrences(
    "wildcard, placeholder, nested and argument partial function lambdas with a match",
    """|object t222 {
       |  val a: PartialFunction[Int, Int] = _ => 1 match { case w => w }
       |  val b: PartialFunction[Int, String] = _.toString match { case s => s }
       |  val c: PartialFunction[Int, PartialFunction[Int, Int]] =
       |    x => x match { case v => y => y match { case w => x + v + w } }
       |  val d: PartialFunction[Int, PartialFunction[Int, Int]] =
       |    _ match { case v => _ match { case w => v + w } }
       |  val e = List(1).collect(x => x match { case w => w })
       |  val f = List(1).collect { _ match { case w => w } }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:11): t222 <= _empty_/t222.
         |[1:6..1:7): a <= _empty_/t222.a.
         |[1:9..1:24): PartialFunction => scala/PartialFunction#
         |[1:25..1:28): Int => scala/Int#
         |[1:30..1:33): Int => scala/Int#
         |[1:37..1:38): _ <= local7
         |[1:37..1:65): _ => 1 match { case w => w } <= local0
         |[1:57..1:58): w <= local8
         |[1:62..1:63): w => local8
         |[2:6..2:7): b <= _empty_/t222.b.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:28): Int => scala/Int#
         |[2:30..2:36): String => scala/Predef.String#
         |[2:40..2:41): _ <= local16
         |[2:40..2:72): _.toString match { case s => s } <= local9
         |[2:42..2:50): toString => scala/Any#toString().
         |[2:64..2:65): s <= local17
         |[2:69..2:70): s => local17
         |[3:6..3:7): c <= _empty_/t222.c.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:28): Int => scala/Int#
         |[3:30..3:45): PartialFunction => scala/PartialFunction#
         |[3:46..3:49): Int => scala/Int#
         |[3:51..3:54): Int => scala/Int#
         |[4:4..4:5): x <= local25
         |[4:4..4:67): x => x match { case v => y => y match { case w => x + v + w } } <= local18
         |[4:9..4:10): x => local25
         |[4:24..4:25): v <= local26
         |[4:29..4:30): y <= local34
         |[4:29..4:65): y => y match { case w => x + v + w } <= local27
         |[4:34..4:35): y => local34
         |[4:49..4:50): w <= local35
         |[4:54..4:55): x => local25
         |[4:56..4:57): + => scala/Int#`+`(+4).
         |[4:58..4:59): v => local26
         |[4:60..4:61): + => scala/Int#`+`(+4).
         |[4:62..4:63): w => local35
         |[5:6..5:7): d <= _empty_/t222.d.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:45): PartialFunction => scala/PartialFunction#
         |[5:46..5:49): Int => scala/Int#
         |[5:51..5:54): Int => scala/Int#
         |[6:4..6:5): _ <= local43
         |[6:4..6:53): _ match { case v => _ match { case w => v + w } } <= local36
         |[6:19..6:20): v <= local44
         |[6:24..6:25): _ <= local52
         |[6:24..6:51): _ match { case w => v + w } <= local45
         |[6:39..6:40): w <= local53
         |[6:44..6:45): v => local44
         |[6:46..6:47): + => scala/Int#`+`(+4).
         |[6:48..6:49): w => local53
         |[7:6..7:7): e <= _empty_/t222.e.
         |[7:10..7:14): List => scala/collection/immutable/List.
         |[7:18..7:25): collect => scala/collection/immutable/List#collect().
         |[7:26..7:27): x <= local61
         |[7:26..7:54): x => x match { case w => w } <= local55
         |[7:31..7:32): x => local61
         |[7:46..7:47): w <= local62
         |[7:51..7:52): w => local62
         |[8:6..8:7): f <= _empty_/t222.f.
         |[8:10..8:14): List => scala/collection/immutable/List.
         |[8:18..8:25): collect => scala/collection/immutable/List#collect().
         |[8:28..8:29): _ <= local70
         |[8:28..8:51): _ match { case w => w } <= local64
         |[8:43..8:44): w <= local71
         |[8:48..8:49): w => local71
         |""".stripMargin
    else
      """|[0:7..0:11): t222 <= _empty_/t222.
         |[1:6..1:7): a <= _empty_/t222.a.
         |[1:9..1:24): PartialFunction => scala/PartialFunction#
         |[1:25..1:28): Int => scala/Int#
         |[1:30..1:33): Int => scala/Int#
         |[1:37..1:38): _ <= local7
         |[1:37..1:65): _ => 1 match { case w => w } <= local0
         |[1:57..1:58): w <= local8
         |[1:62..1:63): w => local8
         |[2:6..2:7): b <= _empty_/t222.b.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:28): Int => scala/Int#
         |[2:30..2:36): String => scala/Predef.String#
         |[2:40..2:41): _ <= local16
         |[2:40..2:72): _.toString match { case s => s } <= local9
         |[2:42..2:50): toString => scala/Any#toString().
         |[2:64..2:65): s <= local17
         |[2:69..2:70): s => local17
         |[3:6..3:7): c <= _empty_/t222.c.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:28): Int => scala/Int#
         |[3:30..3:45): PartialFunction => scala/PartialFunction#
         |[3:46..3:49): Int => scala/Int#
         |[3:51..3:54): Int => scala/Int#
         |[4:4..4:5): x <= local25
         |[4:4..4:67): x => x match { case v => y => y match { case w => x + v + w } } <= local18
         |[4:9..4:10): x => local25
         |[4:24..4:25): v <= local26
         |[4:29..4:30): y <= local34
         |[4:29..4:65): y => y match { case w => x + v + w } <= local27
         |[4:34..4:35): y => local34
         |[4:49..4:50): w <= local35
         |[4:54..4:55): x => local25
         |[4:56..4:57): + => scala/Int#`+`(+4).
         |[4:58..4:59): v => local26
         |[4:60..4:61): + => scala/Int#`+`(+4).
         |[4:62..4:63): w => local35
         |[5:6..5:7): d <= _empty_/t222.d.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:45): PartialFunction => scala/PartialFunction#
         |[5:46..5:49): Int => scala/Int#
         |[5:51..5:54): Int => scala/Int#
         |[6:4..6:5): _ <= local43
         |[6:4..6:53): _ match { case v => _ match { case w => v + w } } <= local36
         |[6:19..6:20): v <= local44
         |[6:24..6:25): _ <= local52
         |[6:24..6:51): _ match { case w => v + w } <= local45
         |[6:39..6:40): w <= local53
         |[6:44..6:45): v => local44
         |[6:46..6:47): + => scala/Int#`+`(+4).
         |[6:48..6:49): w => local53
         |[7:6..7:7): e <= _empty_/t222.e.
         |[7:10..7:14): List => scala/package.List.
         |[7:18..7:25): collect => scala/collection/immutable/List#collect().
         |[7:26..7:27): x <= local61
         |[7:26..7:54): x => x match { case w => w } <= local55
         |[7:31..7:32): x => local61
         |[7:46..7:47): w <= local62
         |[7:51..7:52): w => local62
         |[8:6..8:7): f <= _empty_/t222.f.
         |[8:10..8:14): List => scala/package.List.
         |[8:18..8:25): collect => scala/collection/immutable/List#collect().
         |[8:28..8:29): _ <= local70
         |[8:28..8:51): _ match { case w => w } <= local64
         |[8:43..8:44): w <= local71
         |[8:48..8:49): w => local71
         |""".stripMargin,
  )

  symbols(
    "placeholders in the selector of a match",
    """|object t229 {
       |  def f(x: Int) = x
       |  val a: PartialFunction[Int, Int] = _ + 1 match { case w => w }
       |  val b: PartialFunction[Int, Int] = f(_) match { case w => w }
       |  val c: Int => Int = _ + 1 match { case w => w }
       |  val d: Int => Int = f(_) match { case w => w }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t229. => final object t229 extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t229.a. => val method a: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t229.b. => val method b: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t229.c. => val method c: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t229.d. => val method d: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t229.f(). => method f(x: Int): Int
         |  x => _empty_/t229.f().(x)
         |  Int => scala/Int#
         |_empty_/t229.f().(x) => param x: Int
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x$1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local7
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x$1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local8 => val local w: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x$2 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x$2: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$2 => local16
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x$2: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$2: Int
         |  Int => scala/Int#
         |local17 => val local w: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$3: Int
         |  Int => scala/Int#
         |local19 => val local w: Int
         |  Int => scala/Int#
         |local20 => synthetic param x$4: Int
         |  Int => scala/Int#
         |local21 => val local w: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t229. => final object t229 extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t229.a. => val method a: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t229.b. => val method b: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t229.c. => val method c: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t229.d. => val method d: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t229.f(). => method f(x: Int): Int
         |  x => _empty_/t229.f().(x)
         |  Int => scala/Int#
         |_empty_/t229.f().(x) => param x: Int
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x$1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$1 => local7
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x$1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local8 => val local w: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x$2 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x$2: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x$2 => local16
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x$2: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$2: Int
         |  Int => scala/Int#
         |local17 => val local w: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$3: Int
         |  Int => scala/Int#
         |local19 => val local w: Int
         |  Int => scala/Int#
         |local20 => synthetic param x$4: Int
         |  Int => scala/Int#
         |local21 => val local w: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "placeholders in the selector of a match",
    """|object t229 {
       |  def f(x: Int) = x
       |  val a: PartialFunction[Int, Int] = _ + 1 match { case w => w }
       |  val b: PartialFunction[Int, Int] = f(_) match { case w => w }
       |  val c: Int => Int = _ + 1 match { case w => w }
       |  val d: Int => Int = f(_) match { case w => w }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t229 <= _empty_/t229.
       |[1:6..1:7): f <= _empty_/t229.f().
       |[1:8..1:9): x <= _empty_/t229.f().(x)
       |[1:11..1:14): Int => scala/Int#
       |[1:18..1:19): x => _empty_/t229.f().(x)
       |[2:6..2:7): a <= _empty_/t229.a.
       |[2:9..2:24): PartialFunction => scala/PartialFunction#
       |[2:25..2:28): Int => scala/Int#
       |[2:30..2:33): Int => scala/Int#
       |[2:37..2:38): _ <= local7
       |[2:37..2:64): _ + 1 match { case w => w } <= local0
       |[2:39..2:40): + => scala/Int#`+`(+4).
       |[2:56..2:57): w <= local8
       |[2:61..2:62): w => local8
       |[3:6..3:7): b <= _empty_/t229.b.
       |[3:9..3:24): PartialFunction => scala/PartialFunction#
       |[3:25..3:28): Int => scala/Int#
       |[3:30..3:33): Int => scala/Int#
       |[3:37..3:38): f => _empty_/t229.f().
       |[3:37..3:63): f(_) match { case w => w } <= local9
       |[3:39..3:40): _ <= local16
       |[3:55..3:56): w <= local17
       |[3:60..3:61): w => local17
       |[4:6..4:7): c <= _empty_/t229.c.
       |[4:9..4:12): Int => scala/Int#
       |[4:16..4:19): Int => scala/Int#
       |[4:22..4:23): _ <= local18
       |[4:24..4:25): + => scala/Int#`+`(+4).
       |[4:41..4:42): w <= local19
       |[4:46..4:47): w => local19
       |[5:6..5:7): d <= _empty_/t229.d.
       |[5:9..5:12): Int => scala/Int#
       |[5:16..5:19): Int => scala/Int#
       |[5:22..5:23): f => _empty_/t229.f().
       |[5:24..5:25): _ <= local20
       |[5:40..5:41): w <= local21
       |[5:45..5:46): w => local21
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "a for comprehension with values over methods of partial functions",
    """|object t216 {
       |  class D[A] {
       |    def map[B](f: PartialFunction[A, B]): D[B] = ???
       |    def flatMap[B](f: PartialFunction[A, D[B]]): D[B] = ???
       |    def foreach(f: PartialFunction[A, Unit]): Unit = ???
       |    def withFilter(f: PartialFunction[A, Boolean]): D[A] = ???
       |  }
       |  val d = new D[Int]
       |  val e = new D[(Int, Int)]
       |  val a = for (x <- d; y = x + 1) yield y
       |  val b = for (x <- d; z <- d; y = z + x) yield y
       |  for (x <- d; y = x + 1) println(y)
       |  val f = for (x <- d; (y, w) = (x, x)) yield y + w
       |  val g = for (x: Int <- d; y = x) yield y
       |  val h = for ((x, w) <- e; y = x + w) yield y
       |}
       |""".stripMargin,
    """|_empty_/t216. => final object t216 extends AnyRef { +8 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t216.D# => class D[A] extends AnyRef { +5 decls }
       |  A => _empty_/t216.D#[A]
       |  AnyRef => scala/AnyRef#
       |_empty_/t216.D#[A] => typeparam A
       |_empty_/t216.D#`<init>`(). => primary ctor <init>()
       |_empty_/t216.D#flatMap(). => method flatMap[B](f: PartialFunction[A, D[B]]): D[B]
       |  B => _empty_/t216.D#flatMap().[B]
       |  f => _empty_/t216.D#flatMap().(f)
       |  PartialFunction => scala/PartialFunction#
       |  A => _empty_/t216.D#[A]
       |  D => _empty_/t216.D#
       |_empty_/t216.D#flatMap().(f) => param f: PartialFunction[A, D[B]]
       |  PartialFunction => scala/PartialFunction#
       |  A => _empty_/t216.D#[A]
       |  D => _empty_/t216.D#
       |  B => _empty_/t216.D#flatMap().[B]
       |_empty_/t216.D#flatMap().[B] => typeparam B
       |_empty_/t216.D#foreach(). => method foreach(f: PartialFunction[A, Unit]): Unit
       |  f => _empty_/t216.D#foreach().(f)
       |  PartialFunction => scala/PartialFunction#
       |  A => _empty_/t216.D#[A]
       |  Unit => scala/Unit#
       |_empty_/t216.D#foreach().(f) => param f: PartialFunction[A, Unit]
       |  PartialFunction => scala/PartialFunction#
       |  A => _empty_/t216.D#[A]
       |  Unit => scala/Unit#
       |_empty_/t216.D#map(). => method map[B](f: PartialFunction[A, B]): D[B]
       |  B => _empty_/t216.D#map().[B]
       |  f => _empty_/t216.D#map().(f)
       |  PartialFunction => scala/PartialFunction#
       |  A => _empty_/t216.D#[A]
       |  D => _empty_/t216.D#
       |_empty_/t216.D#map().(f) => param f: PartialFunction[A, B]
       |  PartialFunction => scala/PartialFunction#
       |  A => _empty_/t216.D#[A]
       |  B => _empty_/t216.D#map().[B]
       |_empty_/t216.D#map().[B] => typeparam B
       |_empty_/t216.D#withFilter(). => method withFilter(f: PartialFunction[A, Boolean]): D[A]
       |  f => _empty_/t216.D#withFilter().(f)
       |  PartialFunction => scala/PartialFunction#
       |  A => _empty_/t216.D#[A]
       |  Boolean => scala/Boolean#
       |  D => _empty_/t216.D#
       |_empty_/t216.D#withFilter().(f) => param f: PartialFunction[A, Boolean]
       |  PartialFunction => scala/PartialFunction#
       |  A => _empty_/t216.D#[A]
       |  Boolean => scala/Boolean#
       |_empty_/t216.a. => val method a: D[Int]
       |  D => _empty_/t216.D#
       |  Int => scala/Int#
       |_empty_/t216.b. => val method b: D[Int]
       |  D => _empty_/t216.D#
       |  Int => scala/Int#
       |_empty_/t216.d. => val method d: D[Int]
       |  D => _empty_/t216.D#
       |  Int => scala/Int#
       |_empty_/t216.e. => val method e: D[Tuple2[Int, Int]]
       |  D => _empty_/t216.D#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/t216.f. => val method f: D[Int]
       |  D => _empty_/t216.D#
       |  Int => scala/Int#
       |_empty_/t216.g. => val method g: D[A1] forSome { type A1 <: Int }
       |  D => _empty_/t216.D#
       |  A1 => local102
       |  Int => scala/Int#
       |_empty_/t216.h. => val method h: D[Int]
       |  D => _empty_/t216.D#
       |  Int => scala/Int#
       |local0 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local1 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Tuple2[A1, Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local0
       |  Serializable => java/io/Serializable#
       |local2 => final method applyOrElse[A1 <: Int, B1 >: Tuple2[A1, Int]](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local0
       |  Int => scala/Int#
       |  B1 => local6
       |  Tuple2 => scala/Tuple2#
       |  x => local4
       |  default => local5
       |  Function1 => scala/Function1#
       |local3 => final method isDefinedAt(val x: A1): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local7
       |  A1 => local0
       |  Boolean => scala/Boolean#
       |local4 => synthetic param x: A1
       |  A1 => local0
       |local5 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local0
       |  B1 => local6
       |local6 => typeparam B1 >: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local0
       |  Int => scala/Int#
       |local7 => val local x: A1
       |  A1 => local0
       |local8 => val local y: Int
       |  Int => scala/Int#
       |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[A1, Int], Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local0
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local10 => final method applyOrElse[A1 <: Tuple2[A1, Int], B1 >: Int](x$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local15
       |  Tuple2 => scala/Tuple2#
       |  A1 => local0
       |  Int => scala/Int#
       |  B1 => local14
       |  x$1 => local12
       |  default => local13
       |  Function1 => scala/Function1#
       |local11 => final method isDefinedAt(x$1: Tuple2[A1, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$1 => local16
       |  Tuple2 => scala/Tuple2#
       |  A1 => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local12 => synthetic param x$1: A1
       |  A1 => local15
       |local13 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local15
       |  B1 => local14
       |local14 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local15 => typeparam A1 <: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local0
       |  Int => scala/Int#
       |local16 => synthetic param x$1: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local0
       |  Int => scala/Int#
       |local17 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, D[Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  D => _empty_/t216.D#
       |  Serializable => java/io/Serializable#
       |local18 => final method applyOrElse[A1 <: Int, B1 >: D[Int]](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local23
       |  Int => scala/Int#
       |  B1 => local22
       |  D => _empty_/t216.D#
       |  x => local20
       |  default => local21
       |  Function1 => scala/Function1#
       |local19 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local32
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local20 => synthetic param x: A1
       |  A1 => local23
       |local21 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local23
       |  B1 => local22
       |local22 => typeparam B1 >: D[Int]
       |  D => _empty_/t216.D#
       |  Int => scala/Int#
       |local23 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local24 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local25 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Tuple2[A1, Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local24
       |  Serializable => java/io/Serializable#
       |local26 => final method applyOrElse[A1 <: Int, B1 >: Tuple2[A1, Int]](z: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local24
       |  Int => scala/Int#
       |  B1 => local30
       |  Tuple2 => scala/Tuple2#
       |  z => local28
       |  default => local29
       |  Function1 => scala/Function1#
       |local27 => final method isDefinedAt(val z: A1): Boolean <: scala/PartialFunction#isDefinedAt().
       |  z => local31
       |  A1 => local24
       |  Boolean => scala/Boolean#
       |local28 => synthetic param z: A1
       |  A1 => local24
       |local29 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local24
       |  B1 => local30
       |local30 => typeparam B1 >: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local24
       |  Int => scala/Int#
       |local31 => val local z: A1
       |  A1 => local24
       |local32 => param x: Int
       |  Int => scala/Int#
       |local33 => val local y: Int
       |  Int => scala/Int#
       |local34 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[A1, Int], Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local24
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local35 => final method applyOrElse[A1 <: Tuple2[A1, Int], B1 >: Int](x$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local40
       |  Tuple2 => scala/Tuple2#
       |  A1 => local24
       |  Int => scala/Int#
       |  B1 => local39
       |  x$2 => local37
       |  default => local38
       |  Function1 => scala/Function1#
       |local36 => final method isDefinedAt(x$2: Tuple2[A1, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$2 => local41
       |  Tuple2 => scala/Tuple2#
       |  A1 => local24
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local37 => synthetic param x$2: A1
       |  A1 => local40
       |local38 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local40
       |  B1 => local39
       |local39 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local40 => typeparam A1 <: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local24
       |  Int => scala/Int#
       |local41 => synthetic param x$2: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local24
       |  Int => scala/Int#
       |local42 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local43 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Tuple2[A1, Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local42
       |  Serializable => java/io/Serializable#
       |local44 => final method applyOrElse[A1 <: Int, B1 >: Tuple2[A1, Int]](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local42
       |  Int => scala/Int#
       |  B1 => local48
       |  Tuple2 => scala/Tuple2#
       |  x => local46
       |  default => local47
       |  Function1 => scala/Function1#
       |local45 => final method isDefinedAt(val x: A1): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local49
       |  A1 => local42
       |  Boolean => scala/Boolean#
       |local46 => synthetic param x: A1
       |  A1 => local42
       |local47 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local42
       |  B1 => local48
       |local48 => typeparam B1 >: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local42
       |  Int => scala/Int#
       |local49 => val local x: A1
       |  A1 => local42
       |local50 => val local y: Int
       |  Int => scala/Int#
       |local51 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[A1, Int], Unit] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local42
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |  Serializable => java/io/Serializable#
       |local52 => final method applyOrElse[A1 <: Tuple2[A1, Int], B1 >: Unit](x$3: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local57
       |  Tuple2 => scala/Tuple2#
       |  A1 => local42
       |  Int => scala/Int#
       |  B1 => local56
       |  Unit => scala/Unit#
       |  x$3 => local54
       |  default => local55
       |  Function1 => scala/Function1#
       |local53 => final method isDefinedAt(x$3: Tuple2[A1, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$3 => local58
       |  Tuple2 => scala/Tuple2#
       |  A1 => local42
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local54 => synthetic param x$3: A1
       |  A1 => local57
       |local55 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local57
       |  B1 => local56
       |local56 => typeparam B1 >: Unit
       |  Unit => scala/Unit#
       |local57 => typeparam A1 <: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local42
       |  Int => scala/Int#
       |local58 => synthetic param x$3: Tuple2[A1, Int]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local42
       |  Int => scala/Int#
       |local59 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local60 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Tuple2[A1, Tuple2[A1, A1]]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local59
       |  Serializable => java/io/Serializable#
       |local61 => final method applyOrElse[A1 <: Int, B1 >: Tuple2[A1, Tuple2[A1, A1]]](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local59
       |  Int => scala/Int#
       |  B1 => local65
       |  Tuple2 => scala/Tuple2#
       |  x => local63
       |  default => local64
       |  Function1 => scala/Function1#
       |local62 => final method isDefinedAt(val x: A1): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local66
       |  A1 => local59
       |  Boolean => scala/Boolean#
       |local63 => synthetic param x: A1
       |  A1 => local59
       |local64 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local59
       |  B1 => local65
       |local65 => typeparam B1 >: Tuple2[A1, Tuple2[A1, A1]]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local59
       |local66 => val local x: A1
       |  A1 => local59
       |local67 => val local y: A1
       |  A1 => local59
       |local68 => val local w: A1
       |  A1 => local59
       |local69 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[A1, Tuple2[A1, A1]], Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local59
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local70 => final method applyOrElse[A1 <: Tuple2[A1, Tuple2[A1, A1]], B1 >: Int](x$6: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local75
       |  Tuple2 => scala/Tuple2#
       |  A1 => local59
       |  B1 => local74
       |  Int => scala/Int#
       |  x$6 => local72
       |  default => local73
       |  Function1 => scala/Function1#
       |local71 => final method isDefinedAt(x$6: Tuple2[A1, Tuple2[A1, A1]]): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$6 => local76
       |  Tuple2 => scala/Tuple2#
       |  A1 => local59
       |  Boolean => scala/Boolean#
       |local72 => synthetic param x$6: A1
       |  A1 => local75
       |local73 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local75
       |  B1 => local74
       |local74 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local75 => typeparam A1 <: Tuple2[A1, Tuple2[A1, A1]]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local59
       |local76 => synthetic param x$6: Tuple2[A1, Tuple2[A1, A1]]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local59
       |local77 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local78 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Boolean] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |  Serializable => java/io/Serializable#
       |local79 => final method applyOrElse[A1 <: Int, B1 >: Boolean](check$ifrefutable$1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local84
       |  Int => scala/Int#
       |  B1 => local83
       |  Boolean => scala/Boolean#
       |  check$ifrefutable$1 => local81
       |  default => local82
       |  Function1 => scala/Function1#
       |local80 => final method isDefinedAt(check$ifrefutable$1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  check$ifrefutable$1 => local86
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local81 => synthetic param check$ifrefutable$1: A1
       |  A1 => local84
       |local82 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local84
       |  B1 => local83
       |local83 => typeparam B1 >: Boolean
       |  Boolean => scala/Boolean#
       |local84 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local85 => val local x: Int
       |  Int => scala/Int#
       |local86 => synthetic param check$ifrefutable$1: Int
       |  Int => scala/Int#
       |local87 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Tuple2[A1, A1]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local77
       |  Serializable => java/io/Serializable#
       |local88 => final method applyOrElse[A1 <: Int, B1 >: Tuple2[A1, A1]](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local77
       |  Int => scala/Int#
       |  B1 => local92
       |  Tuple2 => scala/Tuple2#
       |  x => local90
       |  default => local91
       |  Function1 => scala/Function1#
       |local89 => final method isDefinedAt(val x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local85
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local90 => synthetic param x: A1
       |  A1 => local77
       |local91 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local77
       |  B1 => local92
       |local92 => typeparam B1 >: Tuple2[A1, A1]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local77
       |local93 => val local y: A1
       |  A1 => local77
       |local94 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[A1, A1], A1] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Tuple2 => scala/Tuple2#
       |  A1 => local77
       |  Serializable => java/io/Serializable#
       |local95 => final method applyOrElse[A1 <: Tuple2[A1, A1], B1 >: A1](x$7: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local100
       |  Tuple2 => scala/Tuple2#
       |  A1 => local77
       |  B1 => local99
       |  x$7 => local97
       |  default => local98
       |  Function1 => scala/Function1#
       |local96 => final method isDefinedAt(x$7: Tuple2[A1, A1]): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$7 => local101
       |  Tuple2 => scala/Tuple2#
       |  A1 => local77
       |  Boolean => scala/Boolean#
       |local97 => synthetic param x$7: A1
       |  A1 => local100
       |local98 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local100
       |  B1 => local99
       |local99 => typeparam B1 >: A1
       |  A1 => local77
       |local100 => typeparam A1 <: Tuple2[A1, A1]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local77
       |local101 => synthetic param x$7: Tuple2[A1, A1]
       |  Tuple2 => scala/Tuple2#
       |  A1 => local77
       |local103 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[Int, Int], Boolean] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |  Serializable => java/io/Serializable#
       |local104 => final method applyOrElse[A1 <: Tuple2[Int, Int], B1 >: Boolean](check$ifrefutable$2: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local109
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  B1 => local108
       |  Boolean => scala/Boolean#
       |  check$ifrefutable$2 => local106
       |  default => local107
       |  Function1 => scala/Function1#
       |local105 => final method isDefinedAt(check$ifrefutable$2: Tuple2[Int, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
       |  check$ifrefutable$2 => local112
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local106 => synthetic param check$ifrefutable$2: A1
       |  A1 => local109
       |local107 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local109
       |  B1 => local108
       |local108 => typeparam B1 >: Boolean
       |  Boolean => scala/Boolean#
       |local109 => typeparam A1 <: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local110 => val local x: Int
       |  Int => scala/Int#
       |local111 => val local w: Int
       |  Int => scala/Int#
       |local112 => synthetic param check$ifrefutable$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local113 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[Int, Int], Tuple2[Tuple2[Int, Int], Int]] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local114 => final method applyOrElse[A1 <: Tuple2[Int, Int], B1 >: Tuple2[Tuple2[Int, Int], Int]](x$9: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local119
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  B1 => local118
       |  x$9 => local116
       |  default => local117
       |  Function1 => scala/Function1#
       |local115 => final method isDefinedAt(x$9: Tuple2[Int, Int]): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$9 => local121
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local116 => synthetic param x$9: A1
       |  A1 => local119
       |local117 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local119
       |  B1 => local118
       |local118 => typeparam B1 >: Tuple2[Tuple2[Int, Int], Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local119 => typeparam A1 <: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local120 => val local y: Int
       |  Int => scala/Int#
       |local121 => synthetic param x$9: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local122 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[Tuple2[Int, Int], Int], Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local123 => final method applyOrElse[A1 <: Tuple2[Tuple2[Int, Int], Int], B1 >: Int](x$10: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local128
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  B1 => local127
       |  x$10 => local125
       |  default => local126
       |  Function1 => scala/Function1#
       |local124 => final method isDefinedAt(x$10: Tuple2[Tuple2[Int, Int], Int]): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x$10 => local129
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local125 => synthetic param x$10: A1
       |  A1 => local128
       |local126 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local128
       |  B1 => local127
       |local127 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local128 => typeparam A1 <: Tuple2[Tuple2[Int, Int], Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local129 => synthetic param x$10: Tuple2[Tuple2[Int, Int], Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
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
       |[3:37..3:43): foo(_) <= local8
       |[3:41..3:42): _ <= local15
       |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas through renamed packages and objects",
    """|import java.{util => ju}
       |import scala.{annotation => sa, collection => sc}
       |object t238 {
       |  object o { class B { class C }; object v; class ann extends sa.StaticAnnotation }
       |  import t238.{o => p}
       |  val a: PartialFunction[sc.Seq[Int], Int] = (x: sc.Seq[Int]) => x match { case _ => 1 }
       |  val b: sc.Seq[Int] => Int = (x: sc.Seq[Int]) => x match { case _ => 1 }
       |  val c: PartialFunction[p.B, Int] = (x: p.B) => x match { case _ => 1 }
       |  val d: p.B => Int = (x: p.B) => x match { case _ => 1 }
       |  val e: PartialFunction[p.v.type, Int] = (x: p.v.type) => x match { case _ => 1 }
       |  val f: p.v.type => Int = (x: p.v.type) => x match { case _ => 1 }
       |  val g: PartialFunction[p.B#C, Int] = (x: p.B#C) => x match { case _ => 1 }
       |  val h: p.B#C => Int = (x: p.B#C) => x match { case _ => 1 }
       |  val i: PartialFunction[List[ju.List[p.B]], Int] =
       |    (x: List[ju.List[p.B]]) => x match { case _ => 1 }
       |  val j: List[ju.List[p.B]] => Int = (x: List[ju.List[p.B]]) => x match { case _ => 1 }
       |  val k: PartialFunction[Int @p.ann, Int] = (x: Int @p.ann) => x match { case _ => 1 }
       |  val l: (Int @p.ann) => Int = (x: Int @p.ann) => x match { case _ => 1 }
       |  val m: PartialFunction[Int @sa.unchecked.uncheckedVariance, Int] =
       |    (x: Int @sa.unchecked.uncheckedVariance) => x match { case _ => 1 }
       |  val n: (Int @sa.unchecked.uncheckedVariance) => Int =
       |    (x: Int @sa.unchecked.uncheckedVariance) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): java => java/
         |[0:13..0:17): util => java/util/
         |[1:7..1:12): scala => scala/
         |[1:14..1:24): annotation => scala/annotation/
         |[1:32..1:42): collection => scala/collection/
         |[2:7..2:11): t238 <= _empty_/t238.
         |[3:9..3:10): o <= _empty_/t238.o.
         |[3:19..3:20): B <= _empty_/t238.o.B#
         |[3:21..3:21):  <= _empty_/t238.o.B#`<init>`().
         |[3:29..3:30): C <= _empty_/t238.o.B#C#
         |[3:30..3:30):  <= _empty_/t238.o.B#C#`<init>`().
         |[3:41..3:42): v <= _empty_/t238.o.v.
         |[3:50..3:53): ann <= _empty_/t238.o.ann#
         |[3:54..3:54):  <= _empty_/t238.o.ann#`<init>`().
         |[3:62..3:64): sa => scala/annotation/
         |[3:65..3:81): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[3:81..3:81):  => scala/annotation/Annotation#`<init>`().
         |[4:9..4:13): t238 => _empty_/t238.
         |[4:15..4:16): o => _empty_/t238.o.
         |[5:6..5:7): a <= _empty_/t238.a.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:27): sc => scala/collection/
         |[5:28..5:31): Seq => scala/collection/Seq#
         |[5:32..5:35): Int => scala/Int#
         |[5:38..5:41): Int => scala/Int#
         |[5:45..5:88): (x: sc.Seq[Int]) => x match { case _ => 1 } <= local0
         |[5:46..5:47): x <= local7
         |[5:49..5:51): sc => scala/collection/
         |[5:52..5:55): Seq => scala/collection/Seq#
         |[5:56..5:59): Int => scala/Int#
         |[5:65..5:66): x => local7
         |[6:6..6:7): b <= _empty_/t238.b.
         |[6:9..6:11): sc => scala/collection/
         |[6:12..6:15): Seq => scala/collection/Seq#
         |[6:16..6:19): Int => scala/Int#
         |[6:24..6:27): Int => scala/Int#
         |[6:31..6:32): x <= local8
         |[6:34..6:36): sc => scala/collection/
         |[6:37..6:40): Seq => scala/collection/Seq#
         |[6:41..6:44): Int => scala/Int#
         |[6:50..6:51): x => local8
         |[7:6..7:7): c <= _empty_/t238.c.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:26): p => _empty_/t238.o.
         |[7:27..7:28): B => _empty_/t238.o.B#
         |[7:30..7:33): Int => scala/Int#
         |[7:37..7:72): (x: p.B) => x match { case _ => 1 } <= local9
         |[7:38..7:39): x <= local16
         |[7:41..7:42): p => _empty_/t238.o.
         |[7:43..7:44): B => _empty_/t238.o.B#
         |[7:49..7:50): x => local16
         |[8:6..8:7): d <= _empty_/t238.d.
         |[8:9..8:10): p => _empty_/t238.o.
         |[8:11..8:12): B => _empty_/t238.o.B#
         |[8:16..8:19): Int => scala/Int#
         |[8:23..8:24): x <= local17
         |[8:26..8:27): p => _empty_/t238.o.
         |[8:28..8:29): B => _empty_/t238.o.B#
         |[8:34..8:35): x => local17
         |[9:6..9:7): e <= _empty_/t238.e.
         |[9:9..9:24): PartialFunction => scala/PartialFunction#
         |[9:25..9:26): p => _empty_/t238.o.
         |[9:27..9:28): v => _empty_/t238.o.v.
         |[9:35..9:38): Int => scala/Int#
         |[9:42..9:82): (x: p.v.type) => x match { case _ => 1 } <= local18
         |[9:43..9:44): x <= local25
         |[9:46..9:47): p => _empty_/t238.o.
         |[9:48..9:49): v => _empty_/t238.o.v.
         |[9:59..9:60): x => local25
         |[10:6..10:7): f <= _empty_/t238.f.
         |[10:9..10:10): p => _empty_/t238.o.
         |[10:11..10:12): v => _empty_/t238.o.v.
         |[10:21..10:24): Int => scala/Int#
         |[10:28..10:29): x <= local26
         |[10:31..10:32): p => _empty_/t238.o.
         |[10:33..10:34): v => _empty_/t238.o.v.
         |[10:44..10:45): x => local26
         |[11:6..11:7): g <= _empty_/t238.g.
         |[11:9..11:24): PartialFunction => scala/PartialFunction#
         |[11:25..11:26): p => _empty_/t238.o.
         |[11:27..11:28): B => _empty_/t238.o.B#
         |[11:29..11:30): C => _empty_/t238.o.B#C#
         |[11:32..11:35): Int => scala/Int#
         |[11:39..11:76): (x: p.B#C) => x match { case _ => 1 } <= local27
         |[11:40..11:41): x <= local34
         |[11:43..11:44): p => _empty_/t238.o.
         |[11:45..11:46): B => _empty_/t238.o.B#
         |[11:47..11:48): C => _empty_/t238.o.B#C#
         |[11:53..11:54): x => local34
         |[12:6..12:7): h <= _empty_/t238.h.
         |[12:9..12:10): p => _empty_/t238.o.
         |[12:11..12:12): B => _empty_/t238.o.B#
         |[12:13..12:14): C => _empty_/t238.o.B#C#
         |[12:18..12:21): Int => scala/Int#
         |[12:25..12:26): x <= local35
         |[12:28..12:29): p => _empty_/t238.o.
         |[12:30..12:31): B => _empty_/t238.o.B#
         |[12:32..12:33): C => _empty_/t238.o.B#C#
         |[12:38..12:39): x => local35
         |[13:6..13:7): i <= _empty_/t238.i.
         |[13:9..13:24): PartialFunction => scala/PartialFunction#
         |[13:25..13:29): List => scala/package.List#
         |[13:30..13:32): ju => java/util/
         |[13:33..13:37): List => java/util/List#
         |[13:38..13:39): p => _empty_/t238.o.
         |[13:40..13:41): B => _empty_/t238.o.B#
         |[13:45..13:48): Int => scala/Int#
         |[14:4..14:54): (x: List[ju.List[p.B]]) => x match { case _ => 1 } <= local36
         |[14:5..14:6): x <= local43
         |[14:8..14:12): List => scala/package.List#
         |[14:13..14:15): ju => java/util/
         |[14:16..14:20): List => java/util/List#
         |[14:21..14:22): p => _empty_/t238.o.
         |[14:23..14:24): B => _empty_/t238.o.B#
         |[14:31..14:32): x => local43
         |[15:6..15:7): j <= _empty_/t238.j.
         |[15:9..15:13): List => scala/package.List#
         |[15:14..15:16): ju => java/util/
         |[15:17..15:21): List => java/util/List#
         |[15:22..15:23): p => _empty_/t238.o.
         |[15:24..15:25): B => _empty_/t238.o.B#
         |[15:31..15:34): Int => scala/Int#
         |[15:38..15:39): x <= local44
         |[15:41..15:45): List => scala/package.List#
         |[15:46..15:48): ju => java/util/
         |[15:49..15:53): List => java/util/List#
         |[15:54..15:55): p => _empty_/t238.o.
         |[15:56..15:57): B => _empty_/t238.o.B#
         |[15:64..15:65): x => local44
         |[16:6..16:7): k <= _empty_/t238.k.
         |[16:9..16:24): PartialFunction => scala/PartialFunction#
         |[16:25..16:28): Int => scala/Int#
         |[16:30..16:31): p => _empty_/t238.o.
         |[16:32..16:35): ann => _empty_/t238.o.ann#
         |[16:35..16:35):  => _empty_/t238.o.ann#`<init>`().
         |[16:37..16:40): Int => scala/Int#
         |[16:44..16:86): (x: Int @p.ann) => x match { case _ => 1 } <= local45
         |[16:45..16:46): x <= local52
         |[16:48..16:51): Int => scala/Int#
         |[16:53..16:54): p => _empty_/t238.o.
         |[16:55..16:58): ann => _empty_/t238.o.ann#
         |[16:58..16:58):  => _empty_/t238.o.ann#`<init>`().
         |[16:63..16:64): x => local52
         |[17:6..17:7): l <= _empty_/t238.l.
         |[17:10..17:13): Int => scala/Int#
         |[17:15..17:16): p => _empty_/t238.o.
         |[17:17..17:20): ann => _empty_/t238.o.ann#
         |[17:20..17:20):  => _empty_/t238.o.ann#`<init>`().
         |[17:25..17:28): Int => scala/Int#
         |[17:32..17:33): x <= local53
         |[17:35..17:38): Int => scala/Int#
         |[17:40..17:41): p => _empty_/t238.o.
         |[17:42..17:45): ann => _empty_/t238.o.ann#
         |[17:45..17:45):  => _empty_/t238.o.ann#`<init>`().
         |[17:50..17:51): x => local53
         |[18:6..18:7): m <= _empty_/t238.m.
         |[18:9..18:24): PartialFunction => scala/PartialFunction#
         |[18:25..18:28): Int => scala/Int#
         |[18:30..18:32): sa => scala/annotation/
         |[18:33..18:42): unchecked => scala/annotation/unchecked/
         |[18:43..18:60): uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |[18:60..18:60):  => scala/annotation/unchecked/uncheckedVariance#`<init>`().
         |[18:62..18:65): Int => scala/Int#
         |[19:4..19:71): (x: Int @sa.unchecked.uncheckedVariance) => x match { case _ => 1 } <= local54
         |[19:5..19:6): x <= local61
         |[19:8..19:11): Int => scala/Int#
         |[19:13..19:15): sa => scala/annotation/
         |[19:16..19:25): unchecked => scala/annotation/unchecked/
         |[19:26..19:43): uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |[19:43..19:43):  => scala/annotation/unchecked/uncheckedVariance#`<init>`().
         |[19:48..19:49): x => local61
         |[20:6..20:7): n <= _empty_/t238.n.
         |[20:10..20:13): Int => scala/Int#
         |[20:15..20:17): sa => scala/annotation/
         |[20:18..20:27): unchecked => scala/annotation/unchecked/
         |[20:28..20:45): uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |[20:45..20:45):  => scala/annotation/unchecked/uncheckedVariance#`<init>`().
         |[20:50..20:53): Int => scala/Int#
         |[21:5..21:6): x <= local62
         |[21:8..21:11): Int => scala/Int#
         |[21:13..21:15): sa => scala/annotation/
         |[21:16..21:25): unchecked => scala/annotation/unchecked/
         |[21:26..21:43): uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |[21:43..21:43):  => scala/annotation/unchecked/uncheckedVariance#`<init>`().
         |[21:48..21:49): x => local62
         |""".stripMargin
    else
      """|[0:7..0:11): java => java/
         |[0:13..0:17): util => java/util/
         |[1:7..1:12): scala => scala/
         |[1:14..1:24): annotation => scala/annotation/
         |[1:32..1:42): collection => scala/collection/
         |[2:7..2:11): t238 <= _empty_/t238.
         |[3:9..3:10): o <= _empty_/t238.o.
         |[3:19..3:20): B <= _empty_/t238.o.B#
         |[3:21..3:21):  <= _empty_/t238.o.B#`<init>`().
         |[3:29..3:30): C <= _empty_/t238.o.B#C#
         |[3:30..3:30):  <= _empty_/t238.o.B#C#`<init>`().
         |[3:41..3:42): v <= _empty_/t238.o.v.
         |[3:50..3:53): ann <= _empty_/t238.o.ann#
         |[3:54..3:54):  <= _empty_/t238.o.ann#`<init>`().
         |[3:62..3:64): sa => scala/annotation/
         |[3:65..3:81): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[3:81..3:81):  => scala/annotation/Annotation#`<init>`().
         |[4:9..4:13): t238 => _empty_/t238.
         |[4:15..4:16): o => _empty_/t238.o.
         |[5:6..5:7): a <= _empty_/t238.a.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:27): sc => scala/collection/
         |[5:28..5:31): Seq => scala/collection/Seq#
         |[5:32..5:35): Int => scala/Int#
         |[5:38..5:41): Int => scala/Int#
         |[5:45..5:88): (x: sc.Seq[Int]) => x match { case _ => 1 } <= local0
         |[5:46..5:47): x <= local7
         |[5:65..5:66): x => local7
         |[6:6..6:7): b <= _empty_/t238.b.
         |[6:9..6:11): sc => scala/collection/
         |[6:12..6:15): Seq => scala/collection/Seq#
         |[6:16..6:19): Int => scala/Int#
         |[6:24..6:27): Int => scala/Int#
         |[6:31..6:32): x <= local8
         |[6:34..6:36): sc => scala/collection/
         |[6:37..6:40): Seq => scala/collection/Seq#
         |[6:41..6:44): Int => scala/Int#
         |[6:50..6:51): x => local8
         |[7:6..7:7): c <= _empty_/t238.c.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:26): p => _empty_/t238.o.
         |[7:27..7:28): B => _empty_/t238.o.B#
         |[7:30..7:33): Int => scala/Int#
         |[7:37..7:72): (x: p.B) => x match { case _ => 1 } <= local9
         |[7:38..7:39): x <= local16
         |[7:49..7:50): x => local16
         |[8:6..8:7): d <= _empty_/t238.d.
         |[8:9..8:10): p => _empty_/t238.o.
         |[8:11..8:12): B => _empty_/t238.o.B#
         |[8:16..8:19): Int => scala/Int#
         |[8:23..8:24): x <= local17
         |[8:26..8:27): p => _empty_/t238.o.
         |[8:28..8:29): B => _empty_/t238.o.B#
         |[8:34..8:35): x => local17
         |[9:6..9:7): e <= _empty_/t238.e.
         |[9:9..9:24): PartialFunction => scala/PartialFunction#
         |[9:25..9:26): p => _empty_/t238.o.
         |[9:27..9:28): v => _empty_/t238.o.v.
         |[9:35..9:38): Int => scala/Int#
         |[9:42..9:82): (x: p.v.type) => x match { case _ => 1 } <= local18
         |[9:43..9:44): x <= local25
         |[9:59..9:60): x => local25
         |[10:6..10:7): f <= _empty_/t238.f.
         |[10:9..10:10): p => _empty_/t238.o.
         |[10:11..10:12): v => _empty_/t238.o.v.
         |[10:21..10:24): Int => scala/Int#
         |[10:28..10:29): x <= local26
         |[10:31..10:32): p => _empty_/t238.o.
         |[10:33..10:34): v => _empty_/t238.o.v.
         |[10:44..10:45): x => local26
         |[11:6..11:7): g <= _empty_/t238.g.
         |[11:9..11:24): PartialFunction => scala/PartialFunction#
         |[11:25..11:26): p => _empty_/t238.o.
         |[11:27..11:28): B => _empty_/t238.o.B#
         |[11:29..11:30): C => _empty_/t238.o.B#C#
         |[11:32..11:35): Int => scala/Int#
         |[11:39..11:76): (x: p.B#C) => x match { case _ => 1 } <= local27
         |[11:40..11:41): x <= local34
         |[11:53..11:54): x => local34
         |[12:6..12:7): h <= _empty_/t238.h.
         |[12:9..12:10): p => _empty_/t238.o.
         |[12:11..12:12): B => _empty_/t238.o.B#
         |[12:13..12:14): C => _empty_/t238.o.B#C#
         |[12:18..12:21): Int => scala/Int#
         |[12:25..12:26): x <= local35
         |[12:28..12:29): p => _empty_/t238.o.
         |[12:30..12:31): B => _empty_/t238.o.B#
         |[12:32..12:33): C => _empty_/t238.o.B#C#
         |[12:38..12:39): x => local35
         |[13:6..13:7): i <= _empty_/t238.i.
         |[13:9..13:24): PartialFunction => scala/PartialFunction#
         |[13:25..13:29): List => scala/package.List#
         |[13:30..13:32): ju => java/util/
         |[13:33..13:37): List => java/util/List#
         |[13:38..13:39): p => _empty_/t238.o.
         |[13:40..13:41): B => _empty_/t238.o.B#
         |[13:45..13:48): Int => scala/Int#
         |[14:4..14:54): (x: List[ju.List[p.B]]) => x match { case _ => 1 } <= local36
         |[14:5..14:6): x <= local43
         |[14:31..14:32): x => local43
         |[15:6..15:7): j <= _empty_/t238.j.
         |[15:9..15:13): List => scala/package.List#
         |[15:14..15:16): ju => java/util/
         |[15:17..15:21): List => java/util/List#
         |[15:22..15:23): p => _empty_/t238.o.
         |[15:24..15:25): B => _empty_/t238.o.B#
         |[15:31..15:34): Int => scala/Int#
         |[15:38..15:39): x <= local44
         |[15:41..15:45): List => scala/package.List#
         |[15:46..15:48): ju => java/util/
         |[15:49..15:53): List => java/util/List#
         |[15:54..15:55): p => _empty_/t238.o.
         |[15:56..15:57): B => _empty_/t238.o.B#
         |[15:64..15:65): x => local44
         |[16:6..16:7): k <= _empty_/t238.k.
         |[16:9..16:24): PartialFunction => scala/PartialFunction#
         |[16:25..16:28): Int => scala/Int#
         |[16:30..16:31): p => _empty_/t238.o.
         |[16:32..16:35): ann => _empty_/t238.o.ann#
         |[16:35..16:35):  => _empty_/t238.o.ann#`<init>`().
         |[16:37..16:40): Int => scala/Int#
         |[16:44..16:86): (x: Int @p.ann) => x match { case _ => 1 } <= local45
         |[16:45..16:46): x <= local52
         |[16:63..16:64): x => local52
         |[17:6..17:7): l <= _empty_/t238.l.
         |[17:10..17:13): Int => scala/Int#
         |[17:15..17:16): p => _empty_/t238.o.
         |[17:17..17:20): ann => _empty_/t238.o.ann#
         |[17:20..17:20):  => _empty_/t238.o.ann#`<init>`().
         |[17:25..17:28): Int => scala/Int#
         |[17:32..17:33): x <= local53
         |[17:35..17:38): Int => scala/Int#
         |[17:40..17:41): p => _empty_/t238.o.
         |[17:42..17:45): ann => _empty_/t238.o.ann#
         |[17:45..17:45):  => _empty_/t238.o.ann#`<init>`().
         |[17:50..17:51): x => local53
         |[18:6..18:7): m <= _empty_/t238.m.
         |[18:9..18:24): PartialFunction => scala/PartialFunction#
         |[18:25..18:28): Int => scala/Int#
         |[18:30..18:32): sa => scala/annotation/
         |[18:33..18:42): unchecked => scala/annotation/unchecked/
         |[18:43..18:60): uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |[18:60..18:60):  => scala/annotation/unchecked/uncheckedVariance#`<init>`().
         |[18:62..18:65): Int => scala/Int#
         |[19:4..19:71): (x: Int @sa.unchecked.uncheckedVariance) => x match { case _ => 1 } <= local54
         |[19:5..19:6): x <= local61
         |[19:48..19:49): x => local61
         |[20:6..20:7): n <= _empty_/t238.n.
         |[20:10..20:13): Int => scala/Int#
         |[20:15..20:17): sa => scala/annotation/
         |[20:18..20:27): unchecked => scala/annotation/unchecked/
         |[20:28..20:45): uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |[20:45..20:45):  => scala/annotation/unchecked/uncheckedVariance#`<init>`().
         |[20:50..20:53): Int => scala/Int#
         |[21:5..21:6): x <= local62
         |[21:8..21:11): Int => scala/Int#
         |[21:13..21:15): sa => scala/annotation/
         |[21:16..21:25): unchecked => scala/annotation/unchecked/
         |[21:26..21:43): uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |[21:43..21:43):  => scala/annotation/unchecked/uncheckedVariance#`<init>`().
         |[21:48..21:49): x => local62
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas through renamed packages and objects of a supertype",
    """|import java.{util => ju}
       |import scala.{annotation => sa, collection => sc}
       |object t239 {
       |  object o { class B { class C }; class D extends B { class E extends C } }
       |  object q { class ann extends sa.StaticAnnotation }
       |  import t239.{o => p, q => r}
       |  val a: PartialFunction[List[Int], Int] = (x: sc.Seq[Int]) => x match { case _ => 1 }
       |  val b: List[Int] => Int = (x: sc.Seq[Int]) => x match { case _ => 1 }
       |  val c: PartialFunction[p.D, Int] = (x: p.B) => x match { case _ => 1 }
       |  val d: p.D => Int = (x: p.B) => x match { case _ => 1 }
       |  val e: PartialFunction[p.D#E, Int] = (x: p.B#C) => x match { case _ => 1 }
       |  val f: p.D#E => Int = (x: p.B#C) => x match { case _ => 1 }
       |  val g: PartialFunction[List[ju.ArrayList[Int]], Int] =
       |    (x: List[ju.AbstractList[Int]]) => x match { case _ => 1 }
       |  val h: List[ju.ArrayList[Int]] => Int =
       |    (x: List[ju.AbstractList[Int]]) => x match { case _ => 1 }
       |  val i: PartialFunction[Int, Int] = (x: Int @r.ann) => x match { case _ => 1 }
       |  val j: Int => Int = (x: Int @r.ann) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): java => java/
         |[0:13..0:17): util => java/util/
         |[1:7..1:12): scala => scala/
         |[1:14..1:24): annotation => scala/annotation/
         |[1:32..1:42): collection => scala/collection/
         |[2:7..2:11): t239 <= _empty_/t239.
         |[3:9..3:10): o <= _empty_/t239.o.
         |[3:19..3:20): B <= _empty_/t239.o.B#
         |[3:21..3:21):  <= _empty_/t239.o.B#`<init>`().
         |[3:29..3:30): C <= _empty_/t239.o.B#C#
         |[3:30..3:30):  <= _empty_/t239.o.B#C#`<init>`().
         |[3:40..3:41): D <= _empty_/t239.o.D#
         |[3:42..3:42):  <= _empty_/t239.o.D#`<init>`().
         |[3:50..3:51): B => _empty_/t239.o.B#
         |[3:51..3:51):  => _empty_/t239.o.B#`<init>`().
         |[3:60..3:61): E <= _empty_/t239.o.D#E#
         |[3:62..3:62):  <= _empty_/t239.o.D#E#`<init>`().
         |[3:70..3:71): C => _empty_/t239.o.B#C#
         |[3:71..3:71):  => _empty_/t239.o.B#C#`<init>`().
         |[4:9..4:10): q <= _empty_/t239.q.
         |[4:19..4:22): ann <= _empty_/t239.q.ann#
         |[4:23..4:23):  <= _empty_/t239.q.ann#`<init>`().
         |[4:31..4:33): sa => scala/annotation/
         |[4:34..4:50): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[4:50..4:50):  => scala/annotation/Annotation#`<init>`().
         |[5:9..5:13): t239 => _empty_/t239.
         |[5:15..5:16): o => _empty_/t239.o.
         |[5:23..5:24): q => _empty_/t239.q.
         |[6:6..6:7): a <= _empty_/t239.a.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): List => scala/package.List#
         |[6:30..6:33): Int => scala/Int#
         |[6:36..6:39): Int => scala/Int#
         |[6:43..6:86): (x: sc.Seq[Int]) => x match { case _ => 1 } <= local0
         |[6:44..6:45): x <= local7
         |[6:47..6:49): sc => scala/collection/
         |[6:50..6:53): Seq => scala/collection/Seq#
         |[6:54..6:57): Int => scala/Int#
         |[6:63..6:64): x => local7
         |[7:6..7:7): b <= _empty_/t239.b.
         |[7:9..7:13): List => scala/package.List#
         |[7:14..7:17): Int => scala/Int#
         |[7:22..7:25): Int => scala/Int#
         |[7:29..7:30): x <= local8
         |[7:32..7:34): sc => scala/collection/
         |[7:35..7:38): Seq => scala/collection/Seq#
         |[7:39..7:42): Int => scala/Int#
         |[7:48..7:49): x => local8
         |[8:6..8:7): c <= _empty_/t239.c.
         |[8:9..8:24): PartialFunction => scala/PartialFunction#
         |[8:25..8:26): p => _empty_/t239.o.
         |[8:27..8:28): D => _empty_/t239.o.D#
         |[8:30..8:33): Int => scala/Int#
         |[8:37..8:72): (x: p.B) => x match { case _ => 1 } <= local9
         |[8:38..8:39): x <= local16
         |[8:41..8:42): p => _empty_/t239.o.
         |[8:43..8:44): B => _empty_/t239.o.B#
         |[8:49..8:50): x => local16
         |[9:6..9:7): d <= _empty_/t239.d.
         |[9:9..9:10): p => _empty_/t239.o.
         |[9:11..9:12): D => _empty_/t239.o.D#
         |[9:16..9:19): Int => scala/Int#
         |[9:23..9:24): x <= local17
         |[9:26..9:27): p => _empty_/t239.o.
         |[9:28..9:29): B => _empty_/t239.o.B#
         |[9:34..9:35): x => local17
         |[10:6..10:7): e <= _empty_/t239.e.
         |[10:9..10:24): PartialFunction => scala/PartialFunction#
         |[10:25..10:26): p => _empty_/t239.o.
         |[10:27..10:28): D => _empty_/t239.o.D#
         |[10:29..10:30): E => _empty_/t239.o.D#E#
         |[10:32..10:35): Int => scala/Int#
         |[10:39..10:76): (x: p.B#C) => x match { case _ => 1 } <= local18
         |[10:40..10:41): x <= local25
         |[10:43..10:44): p => _empty_/t239.o.
         |[10:45..10:46): B => _empty_/t239.o.B#
         |[10:47..10:48): C => _empty_/t239.o.B#C#
         |[10:53..10:54): x => local25
         |[11:6..11:7): f <= _empty_/t239.f.
         |[11:9..11:10): p => _empty_/t239.o.
         |[11:11..11:12): D => _empty_/t239.o.D#
         |[11:13..11:14): E => _empty_/t239.o.D#E#
         |[11:18..11:21): Int => scala/Int#
         |[11:25..11:26): x <= local26
         |[11:28..11:29): p => _empty_/t239.o.
         |[11:30..11:31): B => _empty_/t239.o.B#
         |[11:32..11:33): C => _empty_/t239.o.B#C#
         |[11:38..11:39): x => local26
         |[12:6..12:7): g <= _empty_/t239.g.
         |[12:9..12:24): PartialFunction => scala/PartialFunction#
         |[12:25..12:29): List => scala/package.List#
         |[12:30..12:32): ju => java/util/
         |[12:33..12:42): ArrayList => java/util/ArrayList#
         |[12:43..12:46): Int => scala/Int#
         |[12:50..12:53): Int => scala/Int#
         |[13:4..13:62): (x: List[ju.AbstractList[Int]]) => x match { case _ => 1 } <= local27
         |[13:5..13:6): x <= local34
         |[13:8..13:12): List => scala/package.List#
         |[13:13..13:15): ju => java/util/
         |[13:16..13:28): AbstractList => java/util/AbstractList#
         |[13:29..13:32): Int => scala/Int#
         |[13:39..13:40): x => local34
         |[14:6..14:7): h <= _empty_/t239.h.
         |[14:9..14:13): List => scala/package.List#
         |[14:14..14:16): ju => java/util/
         |[14:17..14:26): ArrayList => java/util/ArrayList#
         |[14:27..14:30): Int => scala/Int#
         |[14:36..14:39): Int => scala/Int#
         |[15:5..15:6): x <= local35
         |[15:8..15:12): List => scala/package.List#
         |[15:13..15:15): ju => java/util/
         |[15:16..15:28): AbstractList => java/util/AbstractList#
         |[15:29..15:32): Int => scala/Int#
         |[15:39..15:40): x => local35
         |[16:6..16:7): i <= _empty_/t239.i.
         |[16:9..16:24): PartialFunction => scala/PartialFunction#
         |[16:25..16:28): Int => scala/Int#
         |[16:30..16:33): Int => scala/Int#
         |[16:37..16:79): (x: Int @r.ann) => x match { case _ => 1 } <= local36
         |[16:38..16:39): x <= local43
         |[16:41..16:44): Int => scala/Int#
         |[16:46..16:47): r => _empty_/t239.q.
         |[16:48..16:51): ann => _empty_/t239.q.ann#
         |[16:51..16:51):  => _empty_/t239.q.ann#`<init>`().
         |[16:56..16:57): x => local43
         |[17:6..17:7): j <= _empty_/t239.j.
         |[17:9..17:12): Int => scala/Int#
         |[17:16..17:19): Int => scala/Int#
         |[17:23..17:24): x <= local44
         |[17:26..17:29): Int => scala/Int#
         |[17:31..17:32): r => _empty_/t239.q.
         |[17:33..17:36): ann => _empty_/t239.q.ann#
         |[17:36..17:36):  => _empty_/t239.q.ann#`<init>`().
         |[17:41..17:42): x => local44
         |""".stripMargin
    else
      """|[0:7..0:11): java => java/
         |[0:13..0:17): util => java/util/
         |[1:7..1:12): scala => scala/
         |[1:14..1:24): annotation => scala/annotation/
         |[1:32..1:42): collection => scala/collection/
         |[2:7..2:11): t239 <= _empty_/t239.
         |[3:9..3:10): o <= _empty_/t239.o.
         |[3:19..3:20): B <= _empty_/t239.o.B#
         |[3:21..3:21):  <= _empty_/t239.o.B#`<init>`().
         |[3:29..3:30): C <= _empty_/t239.o.B#C#
         |[3:30..3:30):  <= _empty_/t239.o.B#C#`<init>`().
         |[3:40..3:41): D <= _empty_/t239.o.D#
         |[3:42..3:42):  <= _empty_/t239.o.D#`<init>`().
         |[3:50..3:51): B => _empty_/t239.o.B#
         |[3:51..3:51):  => _empty_/t239.o.B#`<init>`().
         |[3:60..3:61): E <= _empty_/t239.o.D#E#
         |[3:62..3:62):  <= _empty_/t239.o.D#E#`<init>`().
         |[3:70..3:71): C => _empty_/t239.o.B#C#
         |[3:71..3:71):  => _empty_/t239.o.B#C#`<init>`().
         |[4:9..4:10): q <= _empty_/t239.q.
         |[4:19..4:22): ann <= _empty_/t239.q.ann#
         |[4:23..4:23):  <= _empty_/t239.q.ann#`<init>`().
         |[4:31..4:33): sa => scala/annotation/
         |[4:34..4:50): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[4:50..4:50):  => scala/annotation/Annotation#`<init>`().
         |[5:9..5:13): t239 => _empty_/t239.
         |[5:15..5:16): o => _empty_/t239.o.
         |[5:23..5:24): q => _empty_/t239.q.
         |[6:6..6:7): a <= _empty_/t239.a.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): List => scala/package.List#
         |[6:30..6:33): Int => scala/Int#
         |[6:36..6:39): Int => scala/Int#
         |[6:43..6:86): (x: sc.Seq[Int]) => x match { case _ => 1 } <= local0
         |[6:44..6:45): x <= local7
         |[6:63..6:64): x => local7
         |[7:6..7:7): b <= _empty_/t239.b.
         |[7:9..7:13): List => scala/package.List#
         |[7:14..7:17): Int => scala/Int#
         |[7:22..7:25): Int => scala/Int#
         |[7:29..7:30): x <= local8
         |[7:32..7:34): sc => scala/collection/
         |[7:35..7:38): Seq => scala/collection/Seq#
         |[7:39..7:42): Int => scala/Int#
         |[7:48..7:49): x => local8
         |[8:6..8:7): c <= _empty_/t239.c.
         |[8:9..8:24): PartialFunction => scala/PartialFunction#
         |[8:25..8:26): p => _empty_/t239.o.
         |[8:27..8:28): D => _empty_/t239.o.D#
         |[8:30..8:33): Int => scala/Int#
         |[8:37..8:72): (x: p.B) => x match { case _ => 1 } <= local9
         |[8:38..8:39): x <= local16
         |[8:49..8:50): x => local16
         |[9:6..9:7): d <= _empty_/t239.d.
         |[9:9..9:10): p => _empty_/t239.o.
         |[9:11..9:12): D => _empty_/t239.o.D#
         |[9:16..9:19): Int => scala/Int#
         |[9:23..9:24): x <= local17
         |[9:26..9:27): p => _empty_/t239.o.
         |[9:28..9:29): B => _empty_/t239.o.B#
         |[9:34..9:35): x => local17
         |[10:6..10:7): e <= _empty_/t239.e.
         |[10:9..10:24): PartialFunction => scala/PartialFunction#
         |[10:25..10:26): p => _empty_/t239.o.
         |[10:27..10:28): D => _empty_/t239.o.D#
         |[10:29..10:30): E => _empty_/t239.o.D#E#
         |[10:32..10:35): Int => scala/Int#
         |[10:39..10:76): (x: p.B#C) => x match { case _ => 1 } <= local18
         |[10:40..10:41): x <= local25
         |[10:53..10:54): x => local25
         |[11:6..11:7): f <= _empty_/t239.f.
         |[11:9..11:10): p => _empty_/t239.o.
         |[11:11..11:12): D => _empty_/t239.o.D#
         |[11:13..11:14): E => _empty_/t239.o.D#E#
         |[11:18..11:21): Int => scala/Int#
         |[11:25..11:26): x <= local26
         |[11:28..11:29): p => _empty_/t239.o.
         |[11:30..11:31): B => _empty_/t239.o.B#
         |[11:32..11:33): C => _empty_/t239.o.B#C#
         |[11:38..11:39): x => local26
         |[12:6..12:7): g <= _empty_/t239.g.
         |[12:9..12:24): PartialFunction => scala/PartialFunction#
         |[12:25..12:29): List => scala/package.List#
         |[12:30..12:32): ju => java/util/
         |[12:33..12:42): ArrayList => java/util/ArrayList#
         |[12:43..12:46): Int => scala/Int#
         |[12:50..12:53): Int => scala/Int#
         |[13:4..13:62): (x: List[ju.AbstractList[Int]]) => x match { case _ => 1 } <= local27
         |[13:5..13:6): x <= local34
         |[13:39..13:40): x => local34
         |[14:6..14:7): h <= _empty_/t239.h.
         |[14:9..14:13): List => scala/package.List#
         |[14:14..14:16): ju => java/util/
         |[14:17..14:26): ArrayList => java/util/ArrayList#
         |[14:27..14:30): Int => scala/Int#
         |[14:36..14:39): Int => scala/Int#
         |[15:5..15:6): x <= local35
         |[15:8..15:12): List => scala/package.List#
         |[15:13..15:15): ju => java/util/
         |[15:16..15:28): AbstractList => java/util/AbstractList#
         |[15:29..15:32): Int => scala/Int#
         |[15:39..15:40): x => local35
         |[16:6..16:7): i <= _empty_/t239.i.
         |[16:9..16:24): PartialFunction => scala/PartialFunction#
         |[16:25..16:28): Int => scala/Int#
         |[16:30..16:33): Int => scala/Int#
         |[16:37..16:79): (x: Int @r.ann) => x match { case _ => 1 } <= local36
         |[16:38..16:39): x <= local43
         |[16:56..16:57): x => local43
         |[17:6..17:7): j <= _empty_/t239.j.
         |[17:9..17:12): Int => scala/Int#
         |[17:16..17:19): Int => scala/Int#
         |[17:23..17:24): x <= local44
         |[17:26..17:29): Int => scala/Int#
         |[17:31..17:32): r => _empty_/t239.q.
         |[17:33..17:36): ann => _empty_/t239.q.ann#
         |[17:36..17:36):  => _empty_/t239.q.ann#`<init>`().
         |[17:41..17:42): x => local44
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas through an object or a package as this",
    """|object t240 {
       |  object o {
       |    class B
       |    import t240.{o => p}
       |    val a: PartialFunction[B, Int] = (x: o.B) => x match { case _ => 1 }
       |    val b: B => Int = (x: o.B) => x match { case _ => 1 }
       |    val c: PartialFunction[B, Int] = (x: p.B) => x match { case _ => 1 }
       |    val d: B => Int = (x: p.B) => x match { case _ => 1 }
       |    val e: PartialFunction[B, Int] = (x: this.B) => x match { case _ => 1 }
       |    val f: B => Int = (x: this.B) => x match { case _ => 1 }
       |    val g: PartialFunction[B, Int] = (x: o.this.B) => x match { case _ => 1 }
       |    val h: B => Int = (x: o.this.B) => x match { case _ => 1 }
       |    val i: PartialFunction[B, Int] = (x: t240.o.B) => x match { case _ => 1 }
       |    val j: B => Int = (x: t240.o.B) => x match { case _ => 1 }
       |  }
       |  val k: PartialFunction[java.util.List[Int], Int] =
       |    (x: _root_.java.util.List[Int]) => x match { case _ => 1 }
       |  val l: java.util.List[Int] => Int = (x: _root_.java.util.List[Int]) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t240 <= _empty_/t240.
         |[1:9..1:10): o <= _empty_/t240.o.
         |[2:10..2:11): B <= _empty_/t240.o.B#
         |[2:11..2:11):  <= _empty_/t240.o.B#`<init>`().
         |[3:11..3:15): t240 => _empty_/t240.
         |[3:17..3:18): o => _empty_/t240.o.
         |[4:8..4:9): a <= _empty_/t240.o.a.
         |[4:11..4:26): PartialFunction => scala/PartialFunction#
         |[4:27..4:28): B => _empty_/t240.o.B#
         |[4:30..4:33): Int => scala/Int#
         |[4:37..4:72): (x: o.B) => x match { case _ => 1 } <= local0
         |[4:38..4:39): x <= local7
         |[4:41..4:42): o => _empty_/t240.o.
         |[4:43..4:44): B => _empty_/t240.o.B#
         |[4:49..4:50): x => local7
         |[5:8..5:9): b <= _empty_/t240.o.b.
         |[5:11..5:12): B => _empty_/t240.o.B#
         |[5:16..5:19): Int => scala/Int#
         |[5:23..5:24): x <= local8
         |[5:26..5:27): o => _empty_/t240.o.
         |[5:28..5:29): B => _empty_/t240.o.B#
         |[5:34..5:35): x => local8
         |[6:8..6:9): c <= _empty_/t240.o.c.
         |[6:11..6:26): PartialFunction => scala/PartialFunction#
         |[6:27..6:28): B => _empty_/t240.o.B#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:72): (x: p.B) => x match { case _ => 1 } <= local9
         |[6:38..6:39): x <= local16
         |[6:41..6:42): p => _empty_/t240.o.
         |[6:43..6:44): B => _empty_/t240.o.B#
         |[6:49..6:50): x => local16
         |[7:8..7:9): d <= _empty_/t240.o.d.
         |[7:11..7:12): B => _empty_/t240.o.B#
         |[7:16..7:19): Int => scala/Int#
         |[7:23..7:24): x <= local17
         |[7:26..7:27): p => _empty_/t240.o.
         |[7:28..7:29): B => _empty_/t240.o.B#
         |[7:34..7:35): x => local17
         |[8:8..8:9): e <= _empty_/t240.o.e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): B => _empty_/t240.o.B#
         |[8:30..8:33): Int => scala/Int#
         |[8:37..8:75): (x: this.B) => x match { case _ => 1 } <= local18
         |[8:38..8:39): x <= local25
         |[8:46..8:47): B => _empty_/t240.o.B#
         |[8:52..8:53): x => local25
         |[9:8..9:9): f <= _empty_/t240.o.f.
         |[9:11..9:12): B => _empty_/t240.o.B#
         |[9:16..9:19): Int => scala/Int#
         |[9:23..9:24): x <= local26
         |[9:31..9:32): B => _empty_/t240.o.B#
         |[9:37..9:38): x => local26
         |[10:8..10:9): g <= _empty_/t240.o.g.
         |[10:11..10:26): PartialFunction => scala/PartialFunction#
         |[10:27..10:28): B => _empty_/t240.o.B#
         |[10:30..10:33): Int => scala/Int#
         |[10:37..10:77): (x: o.this.B) => x match { case _ => 1 } <= local27
         |[10:38..10:39): x <= local34
         |[10:41..10:42): o => _empty_/t240.o.
         |[10:48..10:49): B => _empty_/t240.o.B#
         |[10:54..10:55): x => local34
         |[11:8..11:9): h <= _empty_/t240.o.h.
         |[11:11..11:12): B => _empty_/t240.o.B#
         |[11:16..11:19): Int => scala/Int#
         |[11:23..11:24): x <= local35
         |[11:26..11:27): o => _empty_/t240.o.
         |[11:33..11:34): B => _empty_/t240.o.B#
         |[11:39..11:40): x => local35
         |[12:8..12:9): i <= _empty_/t240.o.i.
         |[12:11..12:26): PartialFunction => scala/PartialFunction#
         |[12:27..12:28): B => _empty_/t240.o.B#
         |[12:30..12:33): Int => scala/Int#
         |[12:37..12:77): (x: t240.o.B) => x match { case _ => 1 } <= local36
         |[12:38..12:39): x <= local43
         |[12:41..12:45): t240 => _empty_/t240.
         |[12:46..12:47): o => _empty_/t240.o.
         |[12:48..12:49): B => _empty_/t240.o.B#
         |[12:54..12:55): x => local43
         |[13:8..13:9): j <= _empty_/t240.o.j.
         |[13:11..13:12): B => _empty_/t240.o.B#
         |[13:16..13:19): Int => scala/Int#
         |[13:23..13:24): x <= local44
         |[13:26..13:30): t240 => _empty_/t240.
         |[13:31..13:32): o => _empty_/t240.o.
         |[13:33..13:34): B => _empty_/t240.o.B#
         |[13:39..13:40): x => local44
         |[15:6..15:7): k <= _empty_/t240.k.
         |[15:9..15:24): PartialFunction => scala/PartialFunction#
         |[15:25..15:29): java => java/
         |[15:30..15:34): util => java/util/
         |[15:35..15:39): List => java/util/List#
         |[15:40..15:43): Int => scala/Int#
         |[15:46..15:49): Int => scala/Int#
         |[16:4..16:62): (x: _root_.java.util.List[Int]) => x match { case _ => 1 } <= local45
         |[16:5..16:6): x <= local52
         |[16:8..16:14): _root_ => _root_/
         |[16:15..16:19): java => java/
         |[16:20..16:24): util => java/util/
         |[16:25..16:29): List => java/util/List#
         |[16:30..16:33): Int => scala/Int#
         |[16:39..16:40): x => local52
         |[17:6..17:7): l <= _empty_/t240.l.
         |[17:9..17:13): java => java/
         |[17:14..17:18): util => java/util/
         |[17:19..17:23): List => java/util/List#
         |[17:24..17:27): Int => scala/Int#
         |[17:32..17:35): Int => scala/Int#
         |[17:39..17:40): x <= local53
         |[17:49..17:53): java => java/
         |[17:54..17:58): util => java/util/
         |[17:59..17:63): List => java/util/List#
         |[17:64..17:67): Int => scala/Int#
         |[17:73..17:74): x => local53
         |""".stripMargin
    else
      """|[0:7..0:11): t240 <= _empty_/t240.
         |[1:9..1:10): o <= _empty_/t240.o.
         |[2:10..2:11): B <= _empty_/t240.o.B#
         |[2:11..2:11):  <= _empty_/t240.o.B#`<init>`().
         |[3:11..3:15): t240 => _empty_/t240.
         |[3:17..3:18): o => _empty_/t240.o.
         |[4:8..4:9): a <= _empty_/t240.o.a.
         |[4:11..4:26): PartialFunction => scala/PartialFunction#
         |[4:27..4:28): B => _empty_/t240.o.B#
         |[4:30..4:33): Int => scala/Int#
         |[4:37..4:72): (x: o.B) => x match { case _ => 1 } <= local0
         |[4:38..4:39): x <= local7
         |[4:49..4:50): x => local7
         |[5:8..5:9): b <= _empty_/t240.o.b.
         |[5:11..5:12): B => _empty_/t240.o.B#
         |[5:16..5:19): Int => scala/Int#
         |[5:23..5:24): x <= local8
         |[5:26..5:27): o => _empty_/t240.o.
         |[5:28..5:29): B => _empty_/t240.o.B#
         |[5:34..5:35): x => local8
         |[6:8..6:9): c <= _empty_/t240.o.c.
         |[6:11..6:26): PartialFunction => scala/PartialFunction#
         |[6:27..6:28): B => _empty_/t240.o.B#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:72): (x: p.B) => x match { case _ => 1 } <= local9
         |[6:38..6:39): x <= local16
         |[6:49..6:50): x => local16
         |[7:8..7:9): d <= _empty_/t240.o.d.
         |[7:11..7:12): B => _empty_/t240.o.B#
         |[7:16..7:19): Int => scala/Int#
         |[7:23..7:24): x <= local17
         |[7:26..7:27): p => _empty_/t240.o.
         |[7:28..7:29): B => _empty_/t240.o.B#
         |[7:34..7:35): x => local17
         |[8:8..8:9): e <= _empty_/t240.o.e.
         |[8:11..8:26): PartialFunction => scala/PartialFunction#
         |[8:27..8:28): B => _empty_/t240.o.B#
         |[8:30..8:33): Int => scala/Int#
         |[8:37..8:75): (x: this.B) => x match { case _ => 1 } <= local18
         |[8:38..8:39): x <= local25
         |[8:52..8:53): x => local25
         |[9:8..9:9): f <= _empty_/t240.o.f.
         |[9:11..9:12): B => _empty_/t240.o.B#
         |[9:16..9:19): Int => scala/Int#
         |[9:23..9:24): x <= local26
         |[9:31..9:32): B => _empty_/t240.o.B#
         |[9:37..9:38): x => local26
         |[10:8..10:9): g <= _empty_/t240.o.g.
         |[10:11..10:26): PartialFunction => scala/PartialFunction#
         |[10:27..10:28): B => _empty_/t240.o.B#
         |[10:30..10:33): Int => scala/Int#
         |[10:37..10:77): (x: o.this.B) => x match { case _ => 1 } <= local27
         |[10:38..10:39): x <= local34
         |[10:54..10:55): x => local34
         |[11:8..11:9): h <= _empty_/t240.o.h.
         |[11:11..11:12): B => _empty_/t240.o.B#
         |[11:16..11:19): Int => scala/Int#
         |[11:23..11:24): x <= local35
         |[11:26..11:27): o => _empty_/t240.o.
         |[11:33..11:34): B => _empty_/t240.o.B#
         |[11:39..11:40): x => local35
         |[12:8..12:9): i <= _empty_/t240.o.i.
         |[12:11..12:26): PartialFunction => scala/PartialFunction#
         |[12:27..12:28): B => _empty_/t240.o.B#
         |[12:30..12:33): Int => scala/Int#
         |[12:37..12:77): (x: t240.o.B) => x match { case _ => 1 } <= local36
         |[12:38..12:39): x <= local43
         |[12:54..12:55): x => local43
         |[13:8..13:9): j <= _empty_/t240.o.j.
         |[13:11..13:12): B => _empty_/t240.o.B#
         |[13:16..13:19): Int => scala/Int#
         |[13:23..13:24): x <= local44
         |[13:26..13:30): t240 => _empty_/t240.
         |[13:31..13:32): o => _empty_/t240.o.
         |[13:33..13:34): B => _empty_/t240.o.B#
         |[13:39..13:40): x => local44
         |[15:6..15:7): k <= _empty_/t240.k.
         |[15:9..15:24): PartialFunction => scala/PartialFunction#
         |[15:25..15:29): java => java/
         |[15:30..15:34): util => java/util/
         |[15:35..15:39): List => java/util/List#
         |[15:40..15:43): Int => scala/Int#
         |[15:46..15:49): Int => scala/Int#
         |[16:4..16:62): (x: _root_.java.util.List[Int]) => x match { case _ => 1 } <= local45
         |[16:5..16:6): x <= local52
         |[16:39..16:40): x => local52
         |[17:6..17:7): l <= _empty_/t240.l.
         |[17:9..17:13): java => java/
         |[17:14..17:18): util => java/util/
         |[17:19..17:23): List => java/util/List#
         |[17:24..17:27): Int => scala/Int#
         |[17:32..17:35): Int => scala/Int#
         |[17:39..17:40): x <= local53
         |[17:49..17:53): java => java/
         |[17:54..17:58): util => java/util/
         |[17:59..17:63): List => java/util/List#
         |[17:64..17:67): Int => scala/Int#
         |[17:73..17:74): x => local53
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas with wildcards, existentials and refinements",
    """|object t241 {
       |  type R = AnyRef { type T; def f(x: Int): Int; val v: Int }
       |  val a: PartialFunction[List[_ >: Null <: AnyRef], Int] =
       |    (x: List[_ >: Null <: AnyRef]) => x match { case _ => 1 }
       |  val b: List[_ >: Null <: AnyRef] => Int =
       |    (x: List[_ >: Null <: AnyRef]) => x match { case _ => 1 }
       |  val c: PartialFunction[(List[T], v.type) forSome { type T <: AnyRef; val v: T }, Int] =
       |    (x: (List[T], v.type) forSome { type T <: AnyRef; val v: T }) => x match { case _ => 1 }
       |  val d: ((List[T], v.type) forSome { type T <: AnyRef; val v: T }) => Int =
       |    (x: (List[T], v.type) forSome { type T <: AnyRef; val v: T }) => x match { case _ => 1 }
       |  val e: PartialFunction[AnyRef { type T; def f(x: Int): Int; val v: Int }, Int] =
       |    (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 }
       |  val f: (AnyRef { type T; def f(x: Int): Int; val v: Int }) => Int =
       |    (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 }
       |  val g: PartialFunction[AnyRef { def g[A: Ordering](a: A): Int; def h[B <% Int](b: B): Int }, Int] =
       |    (x: AnyRef { def g[A: Ordering](a: A): Int; def h[B <% Int](b: B): Int }) => x match { case _ => 1 }
       |  val h: (AnyRef { def g[A: Ordering](a: A): Int; def h[B <% Int](b: B): Int }) => Int =
       |    (x: AnyRef { def g[A: Ordering](a: A): Int; def h[B <% Int](b: B): Int }) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t241 <= _empty_/t241.
         |[1:7..1:8): R <= _empty_/t241.R#
         |[1:11..1:17): AnyRef => scala/AnyRef#
         |[1:25..1:26): T <= local0
         |[1:32..1:33): f <= local1
         |[1:34..1:35): x <= local2
         |[1:37..1:40): Int => scala/Int#
         |[1:43..1:46): Int => scala/Int#
         |[1:52..1:53): v <= local3
         |[1:55..1:58): Int => scala/Int#
         |[2:6..2:7): a <= _empty_/t241.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:29): List => scala/package.List#
         |[2:30..2:31): _ <= local13
         |[2:52..2:55): Int => scala/Int#
         |[3:4..3:61): (x: List[_ >: Null <: AnyRef]) => x match { case _ => 1 } <= local4
         |[3:5..3:6): x <= local12
         |[3:8..3:12): List => scala/package.List#
         |[3:13..3:14): _ <= local5
         |[3:18..3:22): Null => scala/Null#
         |[3:26..3:32): AnyRef => scala/AnyRef#
         |[3:38..3:39): x => local12
         |[4:6..4:7): b <= _empty_/t241.b.
         |[4:9..4:13): List => scala/package.List#
         |[4:14..4:15): _ <= local16
         |[4:38..4:41): Int => scala/Int#
         |[5:5..5:6): x <= local14
         |[5:8..5:12): List => scala/package.List#
         |[5:13..5:14): _ <= local15
         |[5:38..5:39): x => local14
         |[6:6..6:7): c <= _empty_/t241.c.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:26..6:30): List => scala/package.List#
         |[6:31..6:32): T => local28
         |[6:35..6:36): v => local29
         |[6:58..6:59): T <= local28
         |[6:63..6:69): AnyRef => scala/AnyRef#
         |[6:75..6:76): v <= local29
         |[6:78..6:79): T => local28
         |[6:83..6:86): Int => scala/Int#
         |[7:4..7:92): (x: (List[T], v.type) forSome { type T <: AnyRef; val v: T }) => x match { case _ => 1 } <= local17
         |[7:5..7:6): x <= local26
         |[7:9..7:13): List => scala/package.List#
         |[7:14..7:15): T => local18
         |[7:18..7:19): v => local27
         |[7:41..7:42): T <= local18
         |[7:46..7:52): AnyRef => scala/AnyRef#
         |[7:58..7:59): v <= local27
         |[7:61..7:62): T => local18
         |[7:69..7:70): x => local26
         |[8:6..8:7): d <= _empty_/t241.d.
         |[8:11..8:15): List => scala/package.List#
         |[8:16..8:17): T => local35
         |[8:20..8:21): v => local36
         |[8:43..8:44): T <= local35
         |[8:48..8:54): AnyRef => scala/AnyRef#
         |[8:60..8:61): v <= local36
         |[8:63..8:64): T => local35
         |[8:71..8:74): Int => scala/Int#
         |[9:5..9:6): x <= local31
         |[9:9..9:13): List => scala/package.List#
         |[9:14..9:15): T => local32
         |[9:18..9:19): v => local34
         |[9:41..9:42): T <= local32
         |[9:46..9:52): AnyRef => scala/AnyRef#
         |[9:58..9:59): v <= local34
         |[9:61..9:62): T => local32
         |[9:69..9:70): x => local31
         |[10:6..10:7): e <= _empty_/t241.e.
         |[10:9..10:24): PartialFunction => scala/PartialFunction#
         |[10:25..10:31): AnyRef => scala/AnyRef#
         |[10:39..10:40): T <= local50
         |[10:46..10:47): f <= local51
         |[10:48..10:49): x <= local52
         |[10:51..10:54): Int => scala/Int#
         |[10:57..10:60): Int => scala/Int#
         |[10:66..10:67): v <= local53
         |[10:69..10:72): Int => scala/Int#
         |[10:76..10:79): Int => scala/Int#
         |[11:4..11:85): (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 } <= local38
         |[11:5..11:6): x <= local49
         |[11:8..11:14): AnyRef => scala/AnyRef#
         |[11:22..11:23): T <= local39
         |[11:29..11:30): f <= local40
         |[11:31..11:32): x <= local41
         |[11:34..11:37): Int => scala/Int#
         |[11:40..11:43): Int => scala/Int#
         |[11:49..11:50): v <= local42
         |[11:52..11:55): Int => scala/Int#
         |[11:62..11:63): x => local49
         |[12:6..12:7): f <= _empty_/t241.f.
         |[12:10..12:16): AnyRef => scala/AnyRef#
         |[12:24..12:25): T <= local59
         |[12:31..12:32): f <= local60
         |[12:33..12:34): x <= local61
         |[12:36..12:39): Int => scala/Int#
         |[12:42..12:45): Int => scala/Int#
         |[12:51..12:52): v <= local62
         |[12:54..12:57): Int => scala/Int#
         |[12:64..12:67): Int => scala/Int#
         |[13:5..13:6): x <= local54
         |[13:8..13:14): AnyRef => scala/AnyRef#
         |[13:22..13:23): T <= local55
         |[13:29..13:30): f <= local56
         |[13:31..13:32): x <= local57
         |[13:34..13:37): Int => scala/Int#
         |[13:40..13:43): Int => scala/Int#
         |[13:49..13:50): v <= local58
         |[13:52..13:55): Int => scala/Int#
         |[13:62..13:63): x => local54
         |[14:6..14:7): g <= _empty_/t241.g.
         |[14:9..14:24): PartialFunction => scala/PartialFunction#
         |[14:25..14:31): AnyRef => scala/AnyRef#
         |[14:38..14:39): g <= local79
         |[14:40..14:41): A <= local82
         |[14:43..14:43):  <= local81
         |[14:43..14:51): Ordering => scala/package.Ordering#
         |[14:53..14:54): a <= local80
         |[14:56..14:57): A => local82
         |[14:60..14:63): Int => scala/Int#
         |[14:69..14:70): h <= local83
         |[14:71..14:72): B <= local86
         |[14:76..14:76):  <= local85
         |[14:76..14:79): Int => scala/Int#
         |[14:81..14:82): b <= local84
         |[14:84..14:85): B => local86
         |[14:88..14:91): Int => scala/Int#
         |[14:95..14:98): Int => scala/Int#
         |[15:4..15:104): (x: AnyRef { def g[A: Ordering](a: A): Int; def h[B <% Int](b: B): Int }) => x match { case _ => 1 } <= local63
         |[15:5..15:6): x <= local78
         |[15:8..15:14): AnyRef => scala/AnyRef#
         |[15:21..15:22): g <= local64
         |[15:23..15:24): A <= local66
         |[15:26..15:26):  <= local67
         |[15:26..15:34): Ordering => scala/package.Ordering#
         |[15:36..15:37): a <= local65
         |[15:39..15:40): A => local66
         |[15:43..15:46): Int => scala/Int#
         |[15:52..15:53): h <= local68
         |[15:54..15:55): B <= local70
         |[15:59..15:59):  <= local71
         |[15:59..15:62): Int => scala/Int#
         |[15:64..15:65): b <= local69
         |[15:67..15:68): B => local70
         |[15:71..15:74): Int => scala/Int#
         |[15:81..15:82): x => local78
         |[16:6..16:7): h <= _empty_/t241.h.
         |[16:10..16:16): AnyRef => scala/AnyRef#
         |[16:23..16:24): g <= local96
         |[16:25..16:26): A <= local99
         |[16:28..16:28):  <= local98
         |[16:28..16:36): Ordering => scala/package.Ordering#
         |[16:38..16:39): a <= local97
         |[16:41..16:42): A => local99
         |[16:45..16:48): Int => scala/Int#
         |[16:54..16:55): h <= local100
         |[16:56..16:57): B <= local103
         |[16:61..16:61):  <= local102
         |[16:61..16:64): Int => scala/Int#
         |[16:66..16:67): b <= local101
         |[16:69..16:70): B => local103
         |[16:73..16:76): Int => scala/Int#
         |[16:83..16:86): Int => scala/Int#
         |[17:5..17:6): x <= local87
         |[17:8..17:14): AnyRef => scala/AnyRef#
         |[17:21..17:22): g <= local88
         |[17:23..17:24): A <= local90
         |[17:26..17:26):  <= local91
         |[17:26..17:34): Ordering => scala/package.Ordering#
         |[17:36..17:37): a <= local89
         |[17:39..17:40): A => local90
         |[17:43..17:46): Int => scala/Int#
         |[17:52..17:53): h <= local92
         |[17:54..17:55): B <= local94
         |[17:59..17:59):  <= local95
         |[17:59..17:62): Int => scala/Int#
         |[17:64..17:65): b <= local93
         |[17:67..17:68): B => local94
         |[17:71..17:74): Int => scala/Int#
         |[17:81..17:82): x => local87
         |""".stripMargin
    else
      """|[0:7..0:11): t241 <= _empty_/t241.
         |[1:7..1:8): R <= _empty_/t241.R#
         |[1:11..1:17): AnyRef => scala/AnyRef#
         |[1:25..1:26): T <= local0
         |[1:32..1:33): f <= local1
         |[1:34..1:35): x <= local2
         |[1:37..1:40): Int => scala/Int#
         |[1:43..1:46): Int => scala/Int#
         |[1:52..1:53): v <= local3
         |[1:55..1:58): Int => scala/Int#
         |[2:6..2:7): a <= _empty_/t241.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:29): List => scala/package.List#
         |[2:30..2:31): _ <= local5
         |[2:52..2:55): Int => scala/Int#
         |[3:4..3:61): (x: List[_ >: Null <: AnyRef]) => x match { case _ => 1 } <= local4
         |[3:5..3:6): x <= local12
         |[3:38..3:39): x => local12
         |[4:6..4:7): b <= _empty_/t241.b.
         |[4:9..4:13): List => scala/package.List#
         |[4:14..4:15): _ <= local15
         |[4:38..4:41): Int => scala/Int#
         |[5:5..5:6): x <= local13
         |[5:8..5:12): List => scala/package.List#
         |[5:13..5:14): _ <= local14
         |[5:38..5:39): x => local13
         |[6:6..6:7): c <= _empty_/t241.c.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:26..6:30): List => scala/package.List#
         |[6:31..6:32): T => local17
         |[6:35..6:36): v => local26
         |[6:58..6:59): T <= local17
         |[6:63..6:69): AnyRef => scala/AnyRef#
         |[6:75..6:76): v <= local26
         |[6:78..6:79): T => local17
         |[6:83..6:86): Int => scala/Int#
         |[7:4..7:92): (x: (List[T], v.type) forSome { type T <: AnyRef; val v: T }) => x match { case _ => 1 } <= local16
         |[7:5..7:6): x <= local25
         |[7:69..7:70): x => local25
         |[8:6..8:7): d <= _empty_/t241.d.
         |[8:11..8:15): List => scala/package.List#
         |[8:16..8:17): T => local31
         |[8:20..8:21): v => local32
         |[8:43..8:44): T <= local31
         |[8:48..8:54): AnyRef => scala/AnyRef#
         |[8:60..8:61): v <= local32
         |[8:63..8:64): T => local31
         |[8:71..8:74): Int => scala/Int#
         |[9:5..9:6): x <= local27
         |[9:9..9:13): List => scala/package.List#
         |[9:14..9:15): T => local28
         |[9:18..9:19): v => local30
         |[9:41..9:42): T <= local28
         |[9:46..9:52): AnyRef => scala/AnyRef#
         |[9:58..9:59): v <= local30
         |[9:61..9:62): T => local28
         |[9:69..9:70): x => local27
         |[10:6..10:7): e <= _empty_/t241.e.
         |[10:9..10:24): PartialFunction => scala/PartialFunction#
         |[10:25..10:31): AnyRef => scala/AnyRef#
         |[10:39..10:40): T <= local35
         |[10:46..10:47): f <= local36
         |[10:48..10:49): x <= local37
         |[10:51..10:54): Int => scala/Int#
         |[10:57..10:60): Int => scala/Int#
         |[10:66..10:67): v <= local38
         |[10:69..10:72): Int => scala/Int#
         |[10:76..10:79): Int => scala/Int#
         |[11:4..11:85): (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 } <= local34
         |[11:5..11:6): x <= local45
         |[11:62..11:63): x => local45
         |[12:6..12:7): f <= _empty_/t241.f.
         |[12:10..12:16): AnyRef => scala/AnyRef#
         |[12:24..12:25): T <= local51
         |[12:31..12:32): f <= local52
         |[12:33..12:34): x <= local53
         |[12:36..12:39): Int => scala/Int#
         |[12:42..12:45): Int => scala/Int#
         |[12:51..12:52): v <= local54
         |[12:54..12:57): Int => scala/Int#
         |[12:64..12:67): Int => scala/Int#
         |[13:5..13:6): x <= local46
         |[13:8..13:14): AnyRef => scala/AnyRef#
         |[13:22..13:23): T <= local47
         |[13:29..13:30): f <= local48
         |[13:31..13:32): x <= local49
         |[13:34..13:37): Int => scala/Int#
         |[13:40..13:43): Int => scala/Int#
         |[13:49..13:50): v <= local50
         |[13:52..13:55): Int => scala/Int#
         |[13:62..13:63): x => local46
         |[14:6..14:7): g <= _empty_/t241.g.
         |[14:9..14:24): PartialFunction => scala/PartialFunction#
         |[14:25..14:31): AnyRef => scala/AnyRef#
         |[14:38..14:39): g <= local56
         |[14:40..14:41): A <= local58
         |[14:43..14:43):  <= local59
         |[14:43..14:51): Ordering => scala/package.Ordering#
         |[14:53..14:54): a <= local57
         |[14:56..14:57): A => local58
         |[14:60..14:63): Int => scala/Int#
         |[14:69..14:70): h <= local60
         |[14:71..14:72): B <= local62
         |[14:76..14:76):  <= local63
         |[14:76..14:79): Int => scala/Int#
         |[14:81..14:82): b <= local61
         |[14:84..14:85): B => local62
         |[14:88..14:91): Int => scala/Int#
         |[14:95..14:98): Int => scala/Int#
         |[15:4..15:104): (x: AnyRef { def g[A: Ordering](a: A): Int; def h[B <% Int](b: B): Int }) => x match { case _ => 1 } <= local55
         |[15:5..15:6): x <= local70
         |[15:81..15:82): x => local70
         |[16:6..16:7): h <= _empty_/t241.h.
         |[16:10..16:16): AnyRef => scala/AnyRef#
         |[16:23..16:24): g <= local80
         |[16:25..16:26): A <= local83
         |[16:28..16:28):  <= local82
         |[16:28..16:36): Ordering => scala/package.Ordering#
         |[16:38..16:39): a <= local81
         |[16:41..16:42): A => local83
         |[16:45..16:48): Int => scala/Int#
         |[16:54..16:55): h <= local84
         |[16:56..16:57): B <= local87
         |[16:61..16:61):  <= local86
         |[16:61..16:64): Int => scala/Int#
         |[16:66..16:67): b <= local85
         |[16:69..16:70): B => local87
         |[16:73..16:76): Int => scala/Int#
         |[16:83..16:86): Int => scala/Int#
         |[17:5..17:6): x <= local71
         |[17:8..17:14): AnyRef => scala/AnyRef#
         |[17:21..17:22): g <= local72
         |[17:23..17:24): A <= local74
         |[17:26..17:26):  <= local75
         |[17:26..17:34): Ordering => scala/package.Ordering#
         |[17:36..17:37): a <= local73
         |[17:39..17:40): A => local74
         |[17:43..17:46): Int => scala/Int#
         |[17:52..17:53): h <= local76
         |[17:54..17:55): B <= local78
         |[17:59..17:59):  <= local79
         |[17:59..17:62): Int => scala/Int#
         |[17:64..17:65): b <= local77
         |[17:67..17:68): B => local78
         |[17:71..17:74): Int => scala/Int#
         |[17:81..17:82): x => local71
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas with wildcards, existentials and refinements of a supertype",
    """|object t242 {
       |  type L[A] = List[A]
       |  val a: PartialFunction[List[_], Int] = (x: L[_]) => x match { case _ => 1 }
       |  val b: List[_] => Int = (x: L[_]) => x match { case _ => 1 }
       |  val c: PartialFunction[List[_ <: Int], Int] = (x: Seq[_ <: AnyVal]) => x match { case _ => 1 }
       |  val d: List[_ <: Int] => Int = (x: Seq[_ <: AnyVal]) => x match { case _ => 1 }
       |  val e: PartialFunction[List[T] forSome { type T }, Int] =
       |    (x: Seq[T] forSome { type T }) => x match { case _ => 1 }
       |  val f: (List[T] forSome { type T }) => Int = (x: Seq[T] forSome { type T }) => x match { case _ => 1 }
       |  val g: PartialFunction[AnyRef { type T; def f(x: Int): Int; val v: Int; def k: Int }, Int] =
       |    (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 }
       |  val h: (AnyRef { type T; def f(x: Int): Int; val v: Int; def k: Int }) => Int =
       |    (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 }
       |  val i: PartialFunction[AnyRef { def g[A: Ordering](a: A): Int; def k: Int }, Int] =
       |    (x: AnyRef { def g[A: Ordering](a: A): Int }) => x match { case _ => 1 }
       |  val j: (AnyRef { def g[A: Ordering](a: A): Int; def k: Int }) => Int =
       |    (x: AnyRef { def g[A: Ordering](a: A): Int }) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t242 <= _empty_/t242.
         |[1:7..1:8): L <= _empty_/t242.L#
         |[1:9..1:10): A <= _empty_/t242.L#[A]
         |[1:14..1:18): List => scala/package.List#
         |[1:19..1:20): A => _empty_/t242.L#[A]
         |[2:6..2:7): a <= _empty_/t242.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:29): List => scala/package.List#
         |[2:30..2:31): _ <= local9
         |[2:34..2:37): Int => scala/Int#
         |[2:41..2:77): (x: L[_]) => x match { case _ => 1 } <= local0
         |[2:42..2:43): x <= local8
         |[2:45..2:46): L => _empty_/t242.L#
         |[2:47..2:48): _ <= local1
         |[2:54..2:55): x => local8
         |[3:6..3:7): b <= _empty_/t242.b.
         |[3:9..3:13): List => scala/package.List#
         |[3:14..3:15): _ <= local12
         |[3:20..3:23): Int => scala/Int#
         |[3:27..3:28): x <= local10
         |[3:30..3:31): L => _empty_/t242.L#
         |[3:32..3:33): _ <= local11
         |[3:39..3:40): x => local10
         |[4:6..4:7): c <= _empty_/t242.c.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:29): List => scala/package.List#
         |[4:30..4:31): _ <= local22
         |[4:41..4:44): Int => scala/Int#
         |[4:48..4:96): (x: Seq[_ <: AnyVal]) => x match { case _ => 1 } <= local13
         |[4:49..4:50): x <= local21
         |[4:52..4:55): Seq => scala/package.Seq#
         |[4:56..4:57): _ <= local14
         |[4:61..4:67): AnyVal => scala/AnyVal#
         |[4:73..4:74): x => local21
         |[5:6..5:7): d <= _empty_/t242.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): _ <= local25
         |[5:27..5:30): Int => scala/Int#
         |[5:34..5:35): x <= local23
         |[5:37..5:40): Seq => scala/package.Seq#
         |[5:41..5:42): _ <= local24
         |[5:58..5:59): x => local23
         |[6:6..6:7): e <= _empty_/t242.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): List => scala/package.List#
         |[6:30..6:31): T => local35
         |[6:48..6:49): T <= local35
         |[6:53..6:56): Int => scala/Int#
         |[7:4..7:61): (x: Seq[T] forSome { type T }) => x match { case _ => 1 } <= local26
         |[7:5..7:6): x <= local34
         |[7:8..7:11): Seq => scala/package.Seq#
         |[7:12..7:13): T => local27
         |[7:30..7:31): T <= local27
         |[7:38..7:39): x => local34
         |[8:6..8:7): f <= _empty_/t242.f.
         |[8:10..8:14): List => scala/package.List#
         |[8:15..8:16): T => local38
         |[8:33..8:34): T <= local38
         |[8:41..8:44): Int => scala/Int#
         |[8:48..8:49): x <= local36
         |[8:51..8:54): Seq => scala/package.Seq#
         |[8:55..8:56): T => local37
         |[8:73..8:74): T <= local37
         |[8:81..8:82): x => local36
         |[9:6..9:7): g <= _empty_/t242.g.
         |[9:9..9:24): PartialFunction => scala/PartialFunction#
         |[9:25..9:31): AnyRef => scala/AnyRef#
         |[9:39..9:40): T <= local51
         |[9:46..9:47): f <= local52
         |[9:48..9:49): x <= local53
         |[9:51..9:54): Int => scala/Int#
         |[9:57..9:60): Int => scala/Int#
         |[9:66..9:67): v <= local54
         |[9:69..9:72): Int => scala/Int#
         |[9:78..9:79): k <= local55
         |[9:81..9:84): Int => scala/Int#
         |[9:88..9:91): Int => scala/Int#
         |[10:4..10:85): (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 } <= local39
         |[10:5..10:6): x <= local50
         |[10:8..10:14): AnyRef => scala/AnyRef#
         |[10:22..10:23): T <= local40
         |[10:29..10:30): f <= local41
         |[10:31..10:32): x <= local42
         |[10:34..10:37): Int => scala/Int#
         |[10:40..10:43): Int => scala/Int#
         |[10:49..10:50): v <= local43
         |[10:52..10:55): Int => scala/Int#
         |[10:62..10:63): x => local50
         |[11:6..11:7): h <= _empty_/t242.h.
         |[11:10..11:16): AnyRef => scala/AnyRef#
         |[11:24..11:25): T <= local61
         |[11:31..11:32): f <= local62
         |[11:33..11:34): x <= local63
         |[11:36..11:39): Int => scala/Int#
         |[11:42..11:45): Int => scala/Int#
         |[11:51..11:52): v <= local64
         |[11:54..11:57): Int => scala/Int#
         |[11:63..11:64): k <= local65
         |[11:66..11:69): Int => scala/Int#
         |[11:76..11:79): Int => scala/Int#
         |[12:5..12:6): x <= local56
         |[12:8..12:14): AnyRef => scala/AnyRef#
         |[12:22..12:23): T <= local57
         |[12:29..12:30): f <= local58
         |[12:31..12:32): x <= local59
         |[12:34..12:37): Int => scala/Int#
         |[12:40..12:43): Int => scala/Int#
         |[12:49..12:50): v <= local60
         |[12:52..12:55): Int => scala/Int#
         |[12:62..12:63): x => local56
         |[13:6..13:7): i <= _empty_/t242.i.
         |[13:9..13:24): PartialFunction => scala/PartialFunction#
         |[13:25..13:31): AnyRef => scala/AnyRef#
         |[13:38..13:39): g <= local78
         |[13:40..13:41): A <= local81
         |[13:43..13:43):  <= local80
         |[13:43..13:51): Ordering => scala/package.Ordering#
         |[13:53..13:54): a <= local79
         |[13:56..13:57): A => local81
         |[13:60..13:63): Int => scala/Int#
         |[13:69..13:70): k <= local82
         |[13:72..13:75): Int => scala/Int#
         |[13:79..13:82): Int => scala/Int#
         |[14:4..14:76): (x: AnyRef { def g[A: Ordering](a: A): Int }) => x match { case _ => 1 } <= local66
         |[14:5..14:6): x <= local77
         |[14:8..14:14): AnyRef => scala/AnyRef#
         |[14:21..14:22): g <= local67
         |[14:23..14:24): A <= local69
         |[14:26..14:26):  <= local70
         |[14:26..14:34): Ordering => scala/package.Ordering#
         |[14:36..14:37): a <= local68
         |[14:39..14:40): A => local69
         |[14:43..14:46): Int => scala/Int#
         |[14:53..14:54): x => local77
         |[15:6..15:7): j <= _empty_/t242.j.
         |[15:10..15:16): AnyRef => scala/AnyRef#
         |[15:23..15:24): g <= local88
         |[15:25..15:26): A <= local91
         |[15:28..15:28):  <= local90
         |[15:28..15:36): Ordering => scala/package.Ordering#
         |[15:38..15:39): a <= local89
         |[15:41..15:42): A => local91
         |[15:45..15:48): Int => scala/Int#
         |[15:54..15:55): k <= local92
         |[15:57..15:60): Int => scala/Int#
         |[15:67..15:70): Int => scala/Int#
         |[16:5..16:6): x <= local83
         |[16:8..16:14): AnyRef => scala/AnyRef#
         |[16:21..16:22): g <= local84
         |[16:23..16:24): A <= local86
         |[16:26..16:26):  <= local87
         |[16:26..16:34): Ordering => scala/package.Ordering#
         |[16:36..16:37): a <= local85
         |[16:39..16:40): A => local86
         |[16:43..16:46): Int => scala/Int#
         |[16:53..16:54): x => local83
         |""".stripMargin
    else
      """|[0:7..0:11): t242 <= _empty_/t242.
         |[1:7..1:8): L <= _empty_/t242.L#
         |[1:9..1:10): A <= _empty_/t242.L#[A]
         |[1:14..1:18): List => scala/package.List#
         |[1:19..1:20): A => _empty_/t242.L#[A]
         |[2:6..2:7): a <= _empty_/t242.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:29): List => scala/package.List#
         |[2:30..2:31): _ <= local1
         |[2:34..2:37): Int => scala/Int#
         |[2:41..2:77): (x: L[_]) => x match { case _ => 1 } <= local0
         |[2:42..2:43): x <= local8
         |[2:54..2:55): x => local8
         |[3:6..3:7): b <= _empty_/t242.b.
         |[3:9..3:13): List => scala/package.List#
         |[3:14..3:15): _ <= local11
         |[3:20..3:23): Int => scala/Int#
         |[3:27..3:28): x <= local9
         |[3:30..3:31): L => _empty_/t242.L#
         |[3:32..3:33): _ <= local10
         |[3:39..3:40): x => local9
         |[4:6..4:7): c <= _empty_/t242.c.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:29): List => scala/package.List#
         |[4:30..4:31): _ <= local13
         |[4:41..4:44): Int => scala/Int#
         |[4:48..4:96): (x: Seq[_ <: AnyVal]) => x match { case _ => 1 } <= local12
         |[4:49..4:50): x <= local20
         |[4:73..4:74): x => local20
         |[5:6..5:7): d <= _empty_/t242.d.
         |[5:9..5:13): List => scala/package.List#
         |[5:14..5:15): _ <= local23
         |[5:27..5:30): Int => scala/Int#
         |[5:34..5:35): x <= local21
         |[5:37..5:40): Seq => scala/package.Seq#
         |[5:41..5:42): _ <= local22
         |[5:58..5:59): x => local21
         |[6:6..6:7): e <= _empty_/t242.e.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:29): List => scala/package.List#
         |[6:30..6:31): T => local25
         |[6:48..6:49): T <= local25
         |[6:53..6:56): Int => scala/Int#
         |[7:4..7:61): (x: Seq[T] forSome { type T }) => x match { case _ => 1 } <= local24
         |[7:5..7:6): x <= local32
         |[7:38..7:39): x => local32
         |[8:6..8:7): f <= _empty_/t242.f.
         |[8:10..8:14): List => scala/package.List#
         |[8:15..8:16): T => local35
         |[8:33..8:34): T <= local35
         |[8:41..8:44): Int => scala/Int#
         |[8:48..8:49): x <= local33
         |[8:51..8:54): Seq => scala/package.Seq#
         |[8:55..8:56): T => local34
         |[8:73..8:74): T <= local34
         |[8:81..8:82): x => local33
         |[9:6..9:7): g <= _empty_/t242.g.
         |[9:9..9:24): PartialFunction => scala/PartialFunction#
         |[9:25..9:31): AnyRef => scala/AnyRef#
         |[9:39..9:40): T <= local37
         |[9:46..9:47): f <= local38
         |[9:48..9:49): x <= local39
         |[9:51..9:54): Int => scala/Int#
         |[9:57..9:60): Int => scala/Int#
         |[9:66..9:67): v <= local40
         |[9:69..9:72): Int => scala/Int#
         |[9:78..9:79): k <= local41
         |[9:81..9:84): Int => scala/Int#
         |[9:88..9:91): Int => scala/Int#
         |[10:4..10:85): (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 } <= local36
         |[10:5..10:6): x <= local48
         |[10:62..10:63): x => local48
         |[11:6..11:7): h <= _empty_/t242.h.
         |[11:10..11:16): AnyRef => scala/AnyRef#
         |[11:24..11:25): T <= local54
         |[11:31..11:32): f <= local55
         |[11:33..11:34): x <= local56
         |[11:36..11:39): Int => scala/Int#
         |[11:42..11:45): Int => scala/Int#
         |[11:51..11:52): v <= local57
         |[11:54..11:57): Int => scala/Int#
         |[11:63..11:64): k <= local58
         |[11:66..11:69): Int => scala/Int#
         |[11:76..11:79): Int => scala/Int#
         |[12:5..12:6): x <= local49
         |[12:8..12:14): AnyRef => scala/AnyRef#
         |[12:22..12:23): T <= local50
         |[12:29..12:30): f <= local51
         |[12:31..12:32): x <= local52
         |[12:34..12:37): Int => scala/Int#
         |[12:40..12:43): Int => scala/Int#
         |[12:49..12:50): v <= local53
         |[12:52..12:55): Int => scala/Int#
         |[12:62..12:63): x => local49
         |[13:6..13:7): i <= _empty_/t242.i.
         |[13:9..13:24): PartialFunction => scala/PartialFunction#
         |[13:25..13:31): AnyRef => scala/AnyRef#
         |[13:38..13:39): g <= local60
         |[13:40..13:41): A <= local62
         |[13:43..13:43):  <= local63
         |[13:43..13:51): Ordering => scala/package.Ordering#
         |[13:53..13:54): a <= local61
         |[13:56..13:57): A => local62
         |[13:60..13:63): Int => scala/Int#
         |[13:69..13:70): k <= local64
         |[13:72..13:75): Int => scala/Int#
         |[13:79..13:82): Int => scala/Int#
         |[14:4..14:76): (x: AnyRef { def g[A: Ordering](a: A): Int }) => x match { case _ => 1 } <= local59
         |[14:5..14:6): x <= local71
         |[14:53..14:54): x => local71
         |[15:6..15:7): j <= _empty_/t242.j.
         |[15:10..15:16): AnyRef => scala/AnyRef#
         |[15:23..15:24): g <= local77
         |[15:25..15:26): A <= local80
         |[15:28..15:28):  <= local79
         |[15:28..15:36): Ordering => scala/package.Ordering#
         |[15:38..15:39): a <= local78
         |[15:41..15:42): A => local80
         |[15:45..15:48): Int => scala/Int#
         |[15:54..15:55): k <= local81
         |[15:57..15:60): Int => scala/Int#
         |[15:67..15:70): Int => scala/Int#
         |[16:5..16:6): x <= local72
         |[16:8..16:14): AnyRef => scala/AnyRef#
         |[16:21..16:22): g <= local73
         |[16:23..16:24): A <= local75
         |[16:26..16:26):  <= local76
         |[16:26..16:34): Ordering => scala/package.Ordering#
         |[16:36..16:37): a <= local74
         |[16:39..16:40): A => local75
         |[16:43..16:46): Int => scala/Int#
         |[16:53..16:54): x => local72
         |""".stripMargin,
  )

  symbols(
    "typed parameters of partial function lambdas through renamed packages and objects",
    """|import java.{util => ju}
       |import scala.{annotation => sa, collection => sc}
       |object t238 {
       |  object o { class B { class C }; object v; class ann extends sa.StaticAnnotation }
       |  import t238.{o => p}
       |  val a: PartialFunction[sc.Seq[Int], Int] = (x: sc.Seq[Int]) => x match { case _ => 1 }
       |  val b: sc.Seq[Int] => Int = (x: sc.Seq[Int]) => x match { case _ => 1 }
       |  val c: PartialFunction[p.B, Int] = (x: p.B) => x match { case _ => 1 }
       |  val d: p.B => Int = (x: p.B) => x match { case _ => 1 }
       |  val e: PartialFunction[p.v.type, Int] = (x: p.v.type) => x match { case _ => 1 }
       |  val f: p.v.type => Int = (x: p.v.type) => x match { case _ => 1 }
       |  val g: PartialFunction[p.B#C, Int] = (x: p.B#C) => x match { case _ => 1 }
       |  val h: p.B#C => Int = (x: p.B#C) => x match { case _ => 1 }
       |  val i: PartialFunction[List[ju.List[p.B]], Int] =
       |    (x: List[ju.List[p.B]]) => x match { case _ => 1 }
       |  val j: List[ju.List[p.B]] => Int = (x: List[ju.List[p.B]]) => x match { case _ => 1 }
       |  val k: PartialFunction[Int @p.ann, Int] = (x: Int @p.ann) => x match { case _ => 1 }
       |  val l: (Int @p.ann) => Int = (x: Int @p.ann) => x match { case _ => 1 }
       |  val m: PartialFunction[Int @sa.unchecked.uncheckedVariance, Int] =
       |    (x: Int @sa.unchecked.uncheckedVariance) => x match { case _ => 1 }
       |  val n: (Int @sa.unchecked.uncheckedVariance) => Int =
       |    (x: Int @sa.unchecked.uncheckedVariance) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t238. => final object t238 extends AnyRef { +15 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t238.a. => val method a: PartialFunction[Seq[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |_empty_/t238.b. => val method b: Function1[Seq[Int], Int]
         |  Function1 => scala/Function1#
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |_empty_/t238.c. => val method c: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |_empty_/t238.d. => val method d: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |_empty_/t238.e. => val method e: PartialFunction[v.type, Int]
         |  PartialFunction => scala/PartialFunction#
         |  v => _empty_/t238.o.v.
         |  Int => scala/Int#
         |_empty_/t238.f. => val method f: Function1[v.type, Int]
         |  Function1 => scala/Function1#
         |  v => _empty_/t238.o.v.
         |  Int => scala/Int#
         |_empty_/t238.g. => val method g: PartialFunction[B#C, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |  Int => scala/Int#
         |_empty_/t238.h. => val method h: Function1[B#C, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |  Int => scala/Int#
         |_empty_/t238.i. => val method i: PartialFunction[List[List[B]], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |_empty_/t238.j. => val method j: Function1[List[List[B]], Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |_empty_/t238.k. => val method k: PartialFunction[Int @ann, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |_empty_/t238.l. => val method l: Function1[Int @ann, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |_empty_/t238.m. => val method m: PartialFunction[Int @uncheckedVariance, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t238.n. => val method n: Function1[Int @uncheckedVariance, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t238.o. => final object o extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t238.o.B# => class B extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t238.o.B#C# => class C extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t238.o.B#C#`<init>`(). => primary ctor <init>()
         |_empty_/t238.o.B#`<init>`(). => primary ctor <init>()
         |_empty_/t238.o.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
         |  Annotation => scala/annotation/Annotation#
         |  StaticAnnotation => scala/annotation/StaticAnnotation#
         |_empty_/t238.o.ann#`<init>`(). => primary ctor <init>()
         |_empty_/t238.o.v. => final object v extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Seq[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => final method applyOrElse[A1 <: Seq[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |  B1 => local5
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: Seq[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  Seq => scala/collection/Seq#
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
         |local6 => typeparam A1 <: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local7 => param x: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local8 => param x: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local10 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  B => _empty_/t238.o.B#
         |  B1 => local14
         |  Int => scala/Int#
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local16
         |  B => _empty_/t238.o.B#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: B
         |  B => _empty_/t238.o.B#
         |local16 => param x: B
         |  B => _empty_/t238.o.B#
         |local17 => param x: B
         |  B => _empty_/t238.o.B#
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[v.type, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  v => _empty_/t238.o.v.
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local19 => final method applyOrElse[A1 <: v.type, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  v => _empty_/t238.o.v.
         |  B1 => local23
         |  Int => scala/Int#
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(x: v.type): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  v => _empty_/t238.o.v.
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: v.type
         |  v => _empty_/t238.o.v.
         |local25 => param x: v.type
         |  v => _empty_/t238.o.v.
         |local26 => param x: v.type
         |  v => _empty_/t238.o.v.
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B#C, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local28 => final method applyOrElse[A1 <: B#C, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |  B1 => local32
         |  Int => scala/Int#
         |  x => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(x: B#C): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local34
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param x: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: B#C
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |local34 => param x: B#C
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |local35 => param x: B#C
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[List[B]], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local37 => final method applyOrElse[A1 <: List[List[B]], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local42
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |  B1 => local41
         |  Int => scala/Int#
         |  x => local39
         |  default => local40
         |  Function1 => scala/Function1#
         |local38 => final method isDefinedAt(x: List[List[B]]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local43
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |  Boolean => scala/Boolean#
         |local39 => synthetic param x: A1
         |  A1 => local42
         |local40 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local42
         |  B1 => local41
         |local41 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local42 => typeparam A1 <: List[List[B]]
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |local43 => param x: List[List[B]]
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |local44 => param x: List[List[B]]
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |local45 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int @ann, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |  Serializable => scala/Serializable#
         |local46 => final method applyOrElse[A1 <: Int @ann, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local51
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |  B1 => local50
         |  x => local48
         |  default => local49
         |  Function1 => scala/Function1#
         |local47 => final method isDefinedAt(x: Int @ann): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local52
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |  Boolean => scala/Boolean#
         |local48 => synthetic param x: A1
         |  A1 => local51
         |local49 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local51
         |  B1 => local50
         |local50 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local51 => typeparam A1 <: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |local52 => param x: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |local53 => param x: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |local54 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int @uncheckedVariance, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |  Serializable => scala/Serializable#
         |local55 => final method applyOrElse[A1 <: Int @uncheckedVariance, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local60
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |  B1 => local59
         |  x => local57
         |  default => local58
         |  Function1 => scala/Function1#
         |local56 => final method isDefinedAt(x: Int @uncheckedVariance): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local61
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |  Boolean => scala/Boolean#
         |local57 => synthetic param x: A1
         |  A1 => local60
         |local58 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local60
         |  B1 => local59
         |local59 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local60 => typeparam A1 <: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local61 => param x: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local62 => param x: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |""".stripMargin
    else
      """|_empty_/t238. => final object t238 extends AnyRef { +15 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t238.a. => val method a: PartialFunction[Seq[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |_empty_/t238.b. => val method b: Function1[Seq[Int], Int]
         |  Function1 => scala/Function1#
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |_empty_/t238.c. => val method c: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |_empty_/t238.d. => val method d: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |_empty_/t238.e. => val method e: PartialFunction[v.type, Int]
         |  PartialFunction => scala/PartialFunction#
         |  v => _empty_/t238.o.v.
         |  Int => scala/Int#
         |_empty_/t238.f. => val method f: Function1[v.type, Int]
         |  Function1 => scala/Function1#
         |  v => _empty_/t238.o.v.
         |  Int => scala/Int#
         |_empty_/t238.g. => val method g: PartialFunction[B#C, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |  Int => scala/Int#
         |_empty_/t238.h. => val method h: Function1[B#C, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |  Int => scala/Int#
         |_empty_/t238.i. => val method i: PartialFunction[List[List[B]], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |_empty_/t238.j. => val method j: Function1[List[List[B]], Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |_empty_/t238.k. => val method k: PartialFunction[Int @ann, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |_empty_/t238.l. => val method l: Function1[Int @ann, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |_empty_/t238.m. => val method m: PartialFunction[Int @uncheckedVariance, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t238.n. => val method n: Function1[Int @uncheckedVariance, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t238.o. => final object o extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t238.o.B# => class B extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t238.o.B#C# => class C extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t238.o.B#C#`<init>`(). => primary ctor <init>()
         |_empty_/t238.o.B#`<init>`(). => primary ctor <init>()
         |_empty_/t238.o.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
         |  Annotation => scala/annotation/Annotation#
         |  StaticAnnotation => scala/annotation/StaticAnnotation#
         |_empty_/t238.o.ann#`<init>`(). => primary ctor <init>()
         |_empty_/t238.o.v. => final object v extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Seq[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Seq[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |  B1 => local5
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: Seq[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  Seq => scala/collection/Seq#
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
         |local6 => typeparam A1 <: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local7 => param x: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local8 => param x: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  B => _empty_/t238.o.B#
         |  B1 => local14
         |  Int => scala/Int#
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local16
         |  B => _empty_/t238.o.B#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: B
         |  B => _empty_/t238.o.B#
         |local16 => param x: B
         |  B => _empty_/t238.o.B#
         |local17 => param x: B
         |  B => _empty_/t238.o.B#
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[v.type, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  v => _empty_/t238.o.v.
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local19 => final method applyOrElse[A1 <: v.type, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  v => _empty_/t238.o.v.
         |  B1 => local23
         |  Int => scala/Int#
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(x: v.type): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  v => _empty_/t238.o.v.
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: v.type
         |  v => _empty_/t238.o.v.
         |local25 => param x: v.type
         |  v => _empty_/t238.o.v.
         |local26 => param x: v.type
         |  v => _empty_/t238.o.v.
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B#C, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local28 => final method applyOrElse[A1 <: B#C, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |  B1 => local32
         |  Int => scala/Int#
         |  x => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(x: B#C): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local34
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param x: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: B#C
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |local34 => param x: B#C
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |local35 => param x: B#C
         |  B => _empty_/t238.o.B#
         |  C => _empty_/t238.o.B#C#
         |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[List[B]], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local37 => final method applyOrElse[A1 <: List[List[B]], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local42
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |  B1 => local41
         |  Int => scala/Int#
         |  x => local39
         |  default => local40
         |  Function1 => scala/Function1#
         |local38 => final method isDefinedAt(x: List[List[B]]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local43
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |  Boolean => scala/Boolean#
         |local39 => synthetic param x: A1
         |  A1 => local42
         |local40 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local42
         |  B1 => local41
         |local41 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local42 => typeparam A1 <: List[List[B]]
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |local43 => param x: List[List[B]]
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |local44 => param x: List[List[B]]
         |  List => scala/package.List#
         |  List => java/util/List#
         |  B => _empty_/t238.o.B#
         |local45 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int @ann, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |  Serializable => java/io/Serializable#
         |local46 => final method applyOrElse[A1 <: Int @ann, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local51
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |  B1 => local50
         |  x => local48
         |  default => local49
         |  Function1 => scala/Function1#
         |local47 => final method isDefinedAt(x: Int @ann): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local52
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |  Boolean => scala/Boolean#
         |local48 => synthetic param x: A1
         |  A1 => local51
         |local49 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local51
         |  B1 => local50
         |local50 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local51 => typeparam A1 <: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |local52 => param x: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |local53 => param x: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t238.o.ann#
         |local54 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int @uncheckedVariance, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |  Serializable => java/io/Serializable#
         |local55 => final method applyOrElse[A1 <: Int @uncheckedVariance, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local60
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |  B1 => local59
         |  x => local57
         |  default => local58
         |  Function1 => scala/Function1#
         |local56 => final method isDefinedAt(x: Int @uncheckedVariance): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local61
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |  Boolean => scala/Boolean#
         |local57 => synthetic param x: A1
         |  A1 => local60
         |local58 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local60
         |  B1 => local59
         |local59 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local60 => typeparam A1 <: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local61 => param x: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local62 => param x: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |""".stripMargin,
  )

  symbols(
    "typed parameters of partial function lambdas through renamed packages and objects of a supertype",
    """|import java.{util => ju}
       |import scala.{annotation => sa, collection => sc}
       |object t239 {
       |  object o { class B { class C }; class D extends B { class E extends C } }
       |  object q { class ann extends sa.StaticAnnotation }
       |  import t239.{o => p, q => r}
       |  val a: PartialFunction[List[Int], Int] = (x: sc.Seq[Int]) => x match { case _ => 1 }
       |  val b: List[Int] => Int = (x: sc.Seq[Int]) => x match { case _ => 1 }
       |  val c: PartialFunction[p.D, Int] = (x: p.B) => x match { case _ => 1 }
       |  val d: p.D => Int = (x: p.B) => x match { case _ => 1 }
       |  val e: PartialFunction[p.D#E, Int] = (x: p.B#C) => x match { case _ => 1 }
       |  val f: p.D#E => Int = (x: p.B#C) => x match { case _ => 1 }
       |  val g: PartialFunction[List[ju.ArrayList[Int]], Int] =
       |    (x: List[ju.AbstractList[Int]]) => x match { case _ => 1 }
       |  val h: List[ju.ArrayList[Int]] => Int =
       |    (x: List[ju.AbstractList[Int]]) => x match { case _ => 1 }
       |  val i: PartialFunction[Int, Int] = (x: Int @r.ann) => x match { case _ => 1 }
       |  val j: Int => Int = (x: Int @r.ann) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Scala212 ->
          """|_empty_/t239. => final object t239 extends AnyRef { +12 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t239.a. => val method a: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t239.b. => val method b: Function1[List[Int], Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t239.c. => val method c: PartialFunction[D, Int]
             |  PartialFunction => scala/PartialFunction#
             |  D => _empty_/t239.o.D#
             |  Int => scala/Int#
             |_empty_/t239.d. => val method d: Function1[D, Int]
             |  Function1 => scala/Function1#
             |  D => _empty_/t239.o.D#
             |  Int => scala/Int#
             |_empty_/t239.e. => val method e: PartialFunction[D#E, Int]
             |  PartialFunction => scala/PartialFunction#
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |  Int => scala/Int#
             |_empty_/t239.f. => val method f: Function1[D#E, Int]
             |  Function1 => scala/Function1#
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |  Int => scala/Int#
             |_empty_/t239.g. => val method g: PartialFunction[List[ArrayList[Int]], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |_empty_/t239.h. => val method h: Function1[List[ArrayList[Int]], Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |_empty_/t239.i. => val method i: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t239.j. => val method j: Function1[Int, Int]
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |_empty_/t239.o. => final object o extends AnyRef { +2 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t239.o.B# => class B extends AnyRef { +2 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t239.o.B#C# => class C extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t239.o.B#C#`<init>`(). => primary ctor <init>()
             |_empty_/t239.o.B#`<init>`(). => primary ctor <init>()
             |_empty_/t239.o.D# => class D extends B { +2 decls }
             |  B => _empty_/t239.o.B#
             |_empty_/t239.o.D#E# => class E extends D.this.C { +1 decls }
             |  D => _empty_/t239.o.D#
             |  C => _empty_/t239.o.B#C#
             |_empty_/t239.o.D#E#`<init>`(). => primary ctor <init>()
             |_empty_/t239.o.D#`<init>`(). => primary ctor <init>()
             |_empty_/t239.q. => final object q extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t239.q.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
             |  Annotation => scala/annotation/Annotation#
             |  StaticAnnotation => scala/annotation/StaticAnnotation#
             |_empty_/t239.q.ann#`<init>`(). => primary ctor <init>()
             |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local1 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local6
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local5
             |  x => local3
             |  default => local4
             |  Function1 => scala/Function1#
             |local2 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local7
             |  List => scala/package.List#
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
             |local6 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local7 => param x: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local8 => param x: Seq[Int]
             |  Seq => scala/collection/Seq#
             |  Int => scala/Int#
             |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[D, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  D => _empty_/t239.o.D#
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local10 => final method applyOrElse[A1 <: D, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local15
             |  D => _empty_/t239.o.D#
             |  B1 => local14
             |  Int => scala/Int#
             |  x => local12
             |  default => local13
             |  Function1 => scala/Function1#
             |local11 => final method isDefinedAt(x: D): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local16
             |  D => _empty_/t239.o.D#
             |  Boolean => scala/Boolean#
             |local12 => synthetic param x: A1
             |  A1 => local15
             |local13 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local15
             |  B1 => local14
             |local14 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local15 => typeparam A1 <: D
             |  D => _empty_/t239.o.D#
             |local16 => param x: D
             |  D => _empty_/t239.o.D#
             |local17 => param x: B
             |  B => _empty_/t239.o.B#
             |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[D#E, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local19 => final method applyOrElse[A1 <: D#E, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local24
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |  B1 => local23
             |  Int => scala/Int#
             |  x => local21
             |  default => local22
             |  Function1 => scala/Function1#
             |local20 => final method isDefinedAt(x: D#E): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local25
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |  Boolean => scala/Boolean#
             |local21 => synthetic param x: A1
             |  A1 => local24
             |local22 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local24
             |  B1 => local23
             |local23 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local24 => typeparam A1 <: D#E
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |local25 => param x: D#E
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |local26 => param x: B#C
             |  B => _empty_/t239.o.B#
             |  C => _empty_/t239.o.B#C#
             |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[ArrayList[Int]], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local28 => final method applyOrElse[A1 <: List[ArrayList[Int]], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local33
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |  B1 => local32
             |  x => local30
             |  default => local31
             |  Function1 => scala/Function1#
             |local29 => final method isDefinedAt(x: List[ArrayList[Int]]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local34
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local30 => synthetic param x: A1
             |  A1 => local33
             |local31 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local33
             |  B1 => local32
             |local32 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local33 => typeparam A1 <: List[ArrayList[Int]]
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |local34 => param x: List[ArrayList[Int]]
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |local35 => param x: List[AbstractList[Int]]
             |  List => scala/package.List#
             |  AbstractList => java/util/AbstractList#
             |  Int => scala/Int#
             |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local37 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local42
             |  Int => scala/Int#
             |  B1 => local41
             |  x => local39
             |  default => local40
             |  Function1 => scala/Function1#
             |local38 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local43
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local39 => synthetic param x: A1
             |  A1 => local42
             |local40 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local42
             |  B1 => local41
             |local41 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local42 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local43 => param x: Int
             |  Int => scala/Int#
             |local44 => param x: Int @ann
             |  Int => scala/Int#
             |  ann => _empty_/t239.q.ann#
             |""".stripMargin,
        ScalaVersion.AtMost213(16) ->
          """|_empty_/t239. => final object t239 extends AnyRef { +12 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t239.a. => val method a: PartialFunction[List[Int], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t239.b. => val method b: Function1[List[Int], Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |_empty_/t239.c. => val method c: PartialFunction[D, Int]
             |  PartialFunction => scala/PartialFunction#
             |  D => _empty_/t239.o.D#
             |  Int => scala/Int#
             |_empty_/t239.d. => val method d: Function1[D, Int]
             |  Function1 => scala/Function1#
             |  D => _empty_/t239.o.D#
             |  Int => scala/Int#
             |_empty_/t239.e. => val method e: PartialFunction[D#E, Int]
             |  PartialFunction => scala/PartialFunction#
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |  Int => scala/Int#
             |_empty_/t239.f. => val method f: Function1[D#E, Int]
             |  Function1 => scala/Function1#
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |  Int => scala/Int#
             |_empty_/t239.g. => val method g: PartialFunction[List[ArrayList[Int]], Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |_empty_/t239.h. => val method h: Function1[List[ArrayList[Int]], Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |_empty_/t239.i. => val method i: PartialFunction[Int, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Int => scala/Int#
             |_empty_/t239.j. => val method j: Function1[Int, Int]
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |_empty_/t239.o. => final object o extends AnyRef { +2 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t239.o.B# => class B extends AnyRef { +2 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t239.o.B#C# => class C extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t239.o.B#C#`<init>`(). => primary ctor <init>()
             |_empty_/t239.o.B#`<init>`(). => primary ctor <init>()
             |_empty_/t239.o.D# => class D extends B { +2 decls }
             |  B => _empty_/t239.o.B#
             |_empty_/t239.o.D#E# => class E extends D.this.C { +1 decls }
             |  D => _empty_/t239.o.D#
             |  C => _empty_/t239.o.B#C#
             |_empty_/t239.o.D#E#`<init>`(). => primary ctor <init>()
             |_empty_/t239.o.D#`<init>`(). => primary ctor <init>()
             |_empty_/t239.q. => final object q extends AnyRef { +1 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t239.q.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
             |  Annotation => scala/annotation/Annotation#
             |  StaticAnnotation => scala/annotation/StaticAnnotation#
             |_empty_/t239.q.ann#`<init>`(). => primary ctor <init>()
             |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local1 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local6
             |  List => scala/package.List#
             |  Int => scala/Int#
             |  B1 => local5
             |  x => local3
             |  default => local4
             |  Function1 => scala/Function1#
             |local2 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local7
             |  List => scala/package.List#
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
             |local6 => typeparam A1 <: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local7 => param x: List[Int]
             |  List => scala/package.List#
             |  Int => scala/Int#
             |local8 => param x: Seq[Int]
             |  Seq => scala/collection/Seq#
             |  Int => scala/Int#
             |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[D, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  D => _empty_/t239.o.D#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local10 => final method applyOrElse[A1 <: D, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local15
             |  D => _empty_/t239.o.D#
             |  B1 => local14
             |  Int => scala/Int#
             |  x => local12
             |  default => local13
             |  Function1 => scala/Function1#
             |local11 => final method isDefinedAt(x: D): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local16
             |  D => _empty_/t239.o.D#
             |  Boolean => scala/Boolean#
             |local12 => synthetic param x: A1
             |  A1 => local15
             |local13 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local15
             |  B1 => local14
             |local14 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local15 => typeparam A1 <: D
             |  D => _empty_/t239.o.D#
             |local16 => param x: D
             |  D => _empty_/t239.o.D#
             |local17 => param x: B
             |  B => _empty_/t239.o.B#
             |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[D#E, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local19 => final method applyOrElse[A1 <: D#E, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local24
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |  B1 => local23
             |  Int => scala/Int#
             |  x => local21
             |  default => local22
             |  Function1 => scala/Function1#
             |local20 => final method isDefinedAt(x: D#E): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local25
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |  Boolean => scala/Boolean#
             |local21 => synthetic param x: A1
             |  A1 => local24
             |local22 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local24
             |  B1 => local23
             |local23 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local24 => typeparam A1 <: D#E
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |local25 => param x: D#E
             |  D => _empty_/t239.o.D#
             |  E => _empty_/t239.o.D#E#
             |local26 => param x: B#C
             |  B => _empty_/t239.o.B#
             |  C => _empty_/t239.o.B#C#
             |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[ArrayList[Int]], Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local28 => final method applyOrElse[A1 <: List[ArrayList[Int]], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local33
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |  B1 => local32
             |  x => local30
             |  default => local31
             |  Function1 => scala/Function1#
             |local29 => final method isDefinedAt(x: List[ArrayList[Int]]): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local34
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local30 => synthetic param x: A1
             |  A1 => local33
             |local31 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local33
             |  B1 => local32
             |local32 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local33 => typeparam A1 <: List[ArrayList[Int]]
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |local34 => param x: List[ArrayList[Int]]
             |  List => scala/package.List#
             |  ArrayList => java/util/ArrayList#
             |  Int => scala/Int#
             |local35 => param x: List[AbstractList[Int]]
             |  List => scala/package.List#
             |  AbstractList => java/util/AbstractList#
             |  Int => scala/Int#
             |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local37 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local42
             |  Int => scala/Int#
             |  B1 => local41
             |  x => local39
             |  default => local40
             |  Function1 => scala/Function1#
             |local38 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local43
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local39 => synthetic param x: A1
             |  A1 => local42
             |local40 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local42
             |  B1 => local41
             |local41 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local42 => typeparam A1 <: Int
             |  Int => scala/Int#
             |local43 => param x: Int
             |  Int => scala/Int#
             |local44 => param x: Int @ann
             |  Int => scala/Int#
             |  ann => _empty_/t239.q.ann#
             |""".stripMargin,
      ),
      """|_empty_/t239. => final object t239 extends AnyRef { +12 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t239.a. => val method a: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t239.b. => val method b: Function1[List[Int], Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  Int => scala/Int#
         |_empty_/t239.c. => val method c: PartialFunction[D, Int]
         |  PartialFunction => scala/PartialFunction#
         |  D => _empty_/t239.o.D#
         |  Int => scala/Int#
         |_empty_/t239.d. => val method d: Function1[D, Int]
         |  Function1 => scala/Function1#
         |  D => _empty_/t239.o.D#
         |  Int => scala/Int#
         |_empty_/t239.e. => val method e: PartialFunction[D#E, Int]
         |  PartialFunction => scala/PartialFunction#
         |  D => _empty_/t239.o.D#
         |  E => _empty_/t239.o.D#E#
         |  Int => scala/Int#
         |_empty_/t239.f. => val method f: Function1[D#E, Int]
         |  Function1 => scala/Function1#
         |  D => _empty_/t239.o.D#
         |  E => _empty_/t239.o.D#E#
         |  Int => scala/Int#
         |_empty_/t239.g. => val method g: PartialFunction[List[ArrayList[Int]], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  ArrayList => java/util/ArrayList#
         |  Int => scala/Int#
         |_empty_/t239.h. => val method h: Function1[List[ArrayList[Int]], Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  ArrayList => java/util/ArrayList#
         |  Int => scala/Int#
         |_empty_/t239.i. => val method i: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t239.j. => val method j: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |_empty_/t239.o. => final object o extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t239.o.B# => class B extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t239.o.B#C# => class C extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t239.o.B#C#`<init>`(). => primary ctor <init>()
         |_empty_/t239.o.B#`<init>`(). => primary ctor <init>()
         |_empty_/t239.o.D# => class D extends B { +2 decls }
         |  B => _empty_/t239.o.B#
         |_empty_/t239.o.D#E# => class E extends D.this.C { +1 decls }
         |  D => _empty_/t239.o.D#
         |  C => _empty_/t239.o.B#C#
         |_empty_/t239.o.D#E#`<init>`(). => primary ctor <init>()
         |_empty_/t239.o.D#`<init>`(). => primary ctor <init>()
         |_empty_/t239.q. => final object q extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t239.q.ann# => class ann extends Annotation with StaticAnnotation { +1 decls }
         |  Annotation => scala/annotation/Annotation#
         |  StaticAnnotation => scala/annotation/StaticAnnotation#
         |_empty_/t239.q.ann#`<init>`(). => primary ctor <init>()
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Seq[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Seq[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |  B1 => local5
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: Seq[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  Seq => scala/collection/Seq#
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
         |local6 => typeparam A1 <: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local7 => param x: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local8 => param x: Seq[Int]
         |  Seq => scala/collection/Seq#
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t239.o.B#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  B => _empty_/t239.o.B#
         |  B1 => local14
         |  Int => scala/Int#
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local16
         |  B => _empty_/t239.o.B#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: B
         |  B => _empty_/t239.o.B#
         |local16 => param x: B
         |  B => _empty_/t239.o.B#
         |local17 => param x: B
         |  B => _empty_/t239.o.B#
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B#C, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t239.o.B#
         |  C => _empty_/t239.o.B#C#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local19 => final method applyOrElse[A1 <: B#C, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  B => _empty_/t239.o.B#
         |  C => _empty_/t239.o.B#C#
         |  B1 => local23
         |  Int => scala/Int#
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(x: B#C): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  B => _empty_/t239.o.B#
         |  C => _empty_/t239.o.B#C#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: B#C
         |  B => _empty_/t239.o.B#
         |  C => _empty_/t239.o.B#C#
         |local25 => param x: B#C
         |  B => _empty_/t239.o.B#
         |  C => _empty_/t239.o.B#C#
         |local26 => param x: B#C
         |  B => _empty_/t239.o.B#
         |  C => _empty_/t239.o.B#C#
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[AbstractList[Int]], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  AbstractList => java/util/AbstractList#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local28 => final method applyOrElse[A1 <: List[AbstractList[Int]], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  List => scala/package.List#
         |  AbstractList => java/util/AbstractList#
         |  Int => scala/Int#
         |  B1 => local32
         |  x => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(x: List[AbstractList[Int]]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local34
         |  List => scala/package.List#
         |  AbstractList => java/util/AbstractList#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param x: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: List[AbstractList[Int]]
         |  List => scala/package.List#
         |  AbstractList => java/util/AbstractList#
         |  Int => scala/Int#
         |local34 => param x: List[AbstractList[Int]]
         |  List => scala/package.List#
         |  AbstractList => java/util/AbstractList#
         |  Int => scala/Int#
         |local35 => param x: List[AbstractList[Int]]
         |  List => scala/package.List#
         |  AbstractList => java/util/AbstractList#
         |  Int => scala/Int#
         |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int @ann, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  ann => _empty_/t239.q.ann#
         |  Serializable => java/io/Serializable#
         |local37 => final method applyOrElse[A1 <: Int @ann, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local42
         |  Int => scala/Int#
         |  ann => _empty_/t239.q.ann#
         |  B1 => local41
         |  x => local39
         |  default => local40
         |  Function1 => scala/Function1#
         |local38 => final method isDefinedAt(x: Int @ann): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local43
         |  Int => scala/Int#
         |  ann => _empty_/t239.q.ann#
         |  Boolean => scala/Boolean#
         |local39 => synthetic param x: A1
         |  A1 => local42
         |local40 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local42
         |  B1 => local41
         |local41 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local42 => typeparam A1 <: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t239.q.ann#
         |local43 => param x: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t239.q.ann#
         |local44 => param x: Int @ann
         |  Int => scala/Int#
         |  ann => _empty_/t239.q.ann#
         |""".stripMargin,
    ),
  )

  symbols(
    "typed parameters of partial function lambdas through an object or a package as this",
    """|object t240 {
       |  object o {
       |    class B
       |    import t240.{o => p}
       |    val a: PartialFunction[B, Int] = (x: o.B) => x match { case _ => 1 }
       |    val b: B => Int = (x: o.B) => x match { case _ => 1 }
       |    val c: PartialFunction[B, Int] = (x: p.B) => x match { case _ => 1 }
       |    val d: B => Int = (x: p.B) => x match { case _ => 1 }
       |    val e: PartialFunction[B, Int] = (x: this.B) => x match { case _ => 1 }
       |    val f: B => Int = (x: this.B) => x match { case _ => 1 }
       |    val g: PartialFunction[B, Int] = (x: o.this.B) => x match { case _ => 1 }
       |    val h: B => Int = (x: o.this.B) => x match { case _ => 1 }
       |    val i: PartialFunction[B, Int] = (x: t240.o.B) => x match { case _ => 1 }
       |    val j: B => Int = (x: t240.o.B) => x match { case _ => 1 }
       |  }
       |  val k: PartialFunction[java.util.List[Int], Int] =
       |    (x: _root_.java.util.List[Int]) => x match { case _ => 1 }
       |  val l: java.util.List[Int] => Int = (x: _root_.java.util.List[Int]) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t240. => final object t240 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t240.k. => val method k: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => java/util/List#
         |  Int => scala/Int#
         |_empty_/t240.l. => val method l: Function1[List[Int], Int]
         |  Function1 => scala/Function1#
         |  List => java/util/List#
         |  Int => scala/Int#
         |_empty_/t240.o. => final object o extends AnyRef { +11 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t240.o.B# => class B extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t240.o.B#`<init>`(). => primary ctor <init>()
         |_empty_/t240.o.a. => val method a: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.b. => val method b: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.c. => val method c: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.d. => val method d: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.e. => val method e: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.f. => val method f: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.g. => val method g: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.h. => val method h: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.i. => val method i: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.j. => val method j: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  B => _empty_/t240.o.B#
         |  B1 => local5
         |  Int => scala/Int#
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  B => _empty_/t240.o.B#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: B
         |  B => _empty_/t240.o.B#
         |local7 => param x: B
         |  B => _empty_/t240.o.B#
         |local8 => param x: B
         |  B => _empty_/t240.o.B#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local10 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  B => _empty_/t240.o.B#
         |  B1 => local14
         |  Int => scala/Int#
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local16
         |  B => _empty_/t240.o.B#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: B
         |  B => _empty_/t240.o.B#
         |local16 => param x: B
         |  B => _empty_/t240.o.B#
         |local17 => param x: B
         |  B => _empty_/t240.o.B#
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local19 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  B => _empty_/t240.o.B#
         |  B1 => local23
         |  Int => scala/Int#
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  B => _empty_/t240.o.B#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: B
         |  B => _empty_/t240.o.B#
         |local25 => param x: B
         |  B => _empty_/t240.o.B#
         |local26 => param x: B
         |  B => _empty_/t240.o.B#
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local28 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  B => _empty_/t240.o.B#
         |  B1 => local32
         |  Int => scala/Int#
         |  x => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local34
         |  B => _empty_/t240.o.B#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param x: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: B
         |  B => _empty_/t240.o.B#
         |local34 => param x: B
         |  B => _empty_/t240.o.B#
         |local35 => param x: B
         |  B => _empty_/t240.o.B#
         |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local37 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local42
         |  B => _empty_/t240.o.B#
         |  B1 => local41
         |  Int => scala/Int#
         |  x => local39
         |  default => local40
         |  Function1 => scala/Function1#
         |local38 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local43
         |  B => _empty_/t240.o.B#
         |  Boolean => scala/Boolean#
         |local39 => synthetic param x: A1
         |  A1 => local42
         |local40 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local42
         |  B1 => local41
         |local41 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local42 => typeparam A1 <: B
         |  B => _empty_/t240.o.B#
         |local43 => param x: B
         |  B => _empty_/t240.o.B#
         |local44 => param x: B
         |  B => _empty_/t240.o.B#
         |local45 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => java/util/List#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local46 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local51
         |  List => java/util/List#
         |  Int => scala/Int#
         |  B1 => local50
         |  x => local48
         |  default => local49
         |  Function1 => scala/Function1#
         |local47 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local52
         |  List => java/util/List#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local48 => synthetic param x: A1
         |  A1 => local51
         |local49 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local51
         |  B1 => local50
         |local50 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local51 => typeparam A1 <: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |local52 => param x: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |local53 => param x: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t240. => final object t240 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t240.k. => val method k: PartialFunction[List[Int], Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => java/util/List#
         |  Int => scala/Int#
         |_empty_/t240.l. => val method l: Function1[List[Int], Int]
         |  Function1 => scala/Function1#
         |  List => java/util/List#
         |  Int => scala/Int#
         |_empty_/t240.o. => final object o extends AnyRef { +11 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t240.o.B# => class B extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t240.o.B#`<init>`(). => primary ctor <init>()
         |_empty_/t240.o.a. => val method a: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.b. => val method b: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.c. => val method c: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.d. => val method d: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.e. => val method e: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.f. => val method f: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.g. => val method g: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.h. => val method h: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.i. => val method i: PartialFunction[B, Int]
         |  PartialFunction => scala/PartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |_empty_/t240.o.j. => val method j: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  B => _empty_/t240.o.B#
         |  B1 => local5
         |  Int => scala/Int#
         |  x => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local7
         |  B => _empty_/t240.o.B#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: B
         |  B => _empty_/t240.o.B#
         |local7 => param x: B
         |  B => _empty_/t240.o.B#
         |local8 => param x: B
         |  B => _empty_/t240.o.B#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  B => _empty_/t240.o.B#
         |  B1 => local14
         |  Int => scala/Int#
         |  x => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local16
         |  B => _empty_/t240.o.B#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: B
         |  B => _empty_/t240.o.B#
         |local16 => param x: B
         |  B => _empty_/t240.o.B#
         |local17 => param x: B
         |  B => _empty_/t240.o.B#
         |local18 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local19 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local24
         |  B => _empty_/t240.o.B#
         |  B1 => local23
         |  Int => scala/Int#
         |  x => local21
         |  default => local22
         |  Function1 => scala/Function1#
         |local20 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local25
         |  B => _empty_/t240.o.B#
         |  Boolean => scala/Boolean#
         |local21 => synthetic param x: A1
         |  A1 => local24
         |local22 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local24
         |  B1 => local23
         |local23 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local24 => typeparam A1 <: B
         |  B => _empty_/t240.o.B#
         |local25 => param x: B
         |  B => _empty_/t240.o.B#
         |local26 => param x: B
         |  B => _empty_/t240.o.B#
         |local27 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local28 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  B => _empty_/t240.o.B#
         |  B1 => local32
         |  Int => scala/Int#
         |  x => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local34
         |  B => _empty_/t240.o.B#
         |  Boolean => scala/Boolean#
         |local30 => synthetic param x: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: B
         |  B => _empty_/t240.o.B#
         |local34 => param x: B
         |  B => _empty_/t240.o.B#
         |local35 => param x: B
         |  B => _empty_/t240.o.B#
         |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[B, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  B => _empty_/t240.o.B#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local37 => final method applyOrElse[A1 <: B, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local42
         |  B => _empty_/t240.o.B#
         |  B1 => local41
         |  Int => scala/Int#
         |  x => local39
         |  default => local40
         |  Function1 => scala/Function1#
         |local38 => final method isDefinedAt(x: B): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local43
         |  B => _empty_/t240.o.B#
         |  Boolean => scala/Boolean#
         |local39 => synthetic param x: A1
         |  A1 => local42
         |local40 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local42
         |  B1 => local41
         |local41 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local42 => typeparam A1 <: B
         |  B => _empty_/t240.o.B#
         |local43 => param x: B
         |  B => _empty_/t240.o.B#
         |local44 => param x: B
         |  B => _empty_/t240.o.B#
         |local45 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[Int], Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => java/util/List#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local46 => final method applyOrElse[A1 <: List[Int], B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local51
         |  List => java/util/List#
         |  Int => scala/Int#
         |  B1 => local50
         |  x => local48
         |  default => local49
         |  Function1 => scala/Function1#
         |local47 => final method isDefinedAt(x: List[Int]): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local52
         |  List => java/util/List#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local48 => synthetic param x: A1
         |  A1 => local51
         |local49 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local51
         |  B1 => local50
         |local50 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local51 => typeparam A1 <: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |local52 => param x: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |local53 => param x: List[Int]
         |  List => java/util/List#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  symbols(
    "typed parameters of partial function lambdas with wildcards, existentials and refinements",
    """|object t241 {
       |  type R = AnyRef { type T; def f(x: Int): Int; val v: Int }
       |  val a: PartialFunction[List[_ >: Null <: AnyRef], Int] =
       |    (x: List[_ >: Null <: AnyRef]) => x match { case _ => 1 }
       |  val b: List[_ >: Null <: AnyRef] => Int =
       |    (x: List[_ >: Null <: AnyRef]) => x match { case _ => 1 }
       |  val c: PartialFunction[(List[T], v.type) forSome { type T <: AnyRef; val v: T }, Int] =
       |    (x: (List[T], v.type) forSome { type T <: AnyRef; val v: T }) => x match { case _ => 1 }
       |  val d: ((List[T], v.type) forSome { type T <: AnyRef; val v: T }) => Int =
       |    (x: (List[T], v.type) forSome { type T <: AnyRef; val v: T }) => x match { case _ => 1 }
       |  val e: PartialFunction[AnyRef { type T; def f(x: Int): Int; val v: Int }, Int] =
       |    (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 }
       |  val f: (AnyRef { type T; def f(x: Int): Int; val v: Int }) => Int =
       |    (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 }
       |  val g: PartialFunction[AnyRef { def g[A: Ordering](a: A): Int; def h[B <% Int](b: B): Int }, Int] =
       |    (x: AnyRef { def g[A: Ordering](a: A): Int; def h[B <% Int](b: B): Int }) => x match { case _ => 1 }
       |  val h: (AnyRef { def g[A: Ordering](a: A): Int; def h[B <% Int](b: B): Int }) => Int =
       |    (x: AnyRef { def g[A: Ordering](a: A): Int; def h[B <% Int](b: B): Int }) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Scala212 ->
          """|_empty_/t241. => final object t241 extends AnyRef { +9 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t241.R# => type R = AnyRef { type T; def f(x: Int): Int; val def v: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local0
             |  f => local1
             |  x => local2
             |  Int => scala/Int#
             |  v => local3
             |_empty_/t241.a. => val method a: PartialFunction[List[_] forSome { type _ >: Null <: AnyRef }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |  Int => scala/Int#
             |_empty_/t241.b. => val method b: Function1[List[_] forSome { type _ >: Null <: AnyRef }, Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  _ => local15
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |  Int => scala/Int#
             |_empty_/t241.c. => val method c: PartialFunction[Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |  Int => scala/Int#
             |_empty_/t241.d. => val method d: Function1[Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, Int]
             |  Function1 => scala/Function1#
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local31
             |  v.type => local33
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |  Int => scala/Int#
             |_empty_/t241.e. => val method e: PartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |_empty_/t241.f. => val method f: Function1[AnyRef { type T; def f(x: Int): Int; val def v: Int }, Int]
             |  Function1 => scala/Function1#
             |  AnyRef => scala/AnyRef#
             |  T => local51
             |  f => local52
             |  x => local53
             |  Int => scala/Int#
             |  v => local54
             |_empty_/t241.g. => val method g: PartialFunction[AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |_empty_/t241.h. => val method h: Function1[AnyRef { def g[A](a: A)(implicit evidence$5: Ordering[A]): Int; def h[B](b: B)(implicit evidence$6: Function1[B, Int]): Int }, Int]
             |  Function1 => scala/Function1#
             |  AnyRef => scala/AnyRef#
             |  g => local80
             |  A => local83
             |  a => local81
             |  evidence$5 => local82
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local84
             |  B => local87
             |  b => local85
             |  evidence$6 => local86
             |local0 => abstract type T
             |local1 => abstract method f(x: Int): Int
             |  x => local2
             |  Int => scala/Int#
             |local2 => param x: Int
             |  Int => scala/Int#
             |local3 => abstract val method v: Int
             |  Int => scala/Int#
             |local4 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ >: Null <: AnyRef }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local5 => synthetic abstract type _ >: Null <: AnyRef
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local6 => final method applyOrElse[A1 <: List[_] forSome { type _ >: Null <: AnyRef }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local11
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |  B1 => local10
             |  Int => scala/Int#
             |  x => local8
             |  default => local9
             |  Function1 => scala/Function1#
             |local7 => final method isDefinedAt(x: List[_] forSome { type _ >: Null <: AnyRef }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local12
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |  Boolean => scala/Boolean#
             |local8 => synthetic param x: A1
             |  A1 => local11
             |local9 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local11
             |  B1 => local10
             |local10 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local11 => typeparam A1 <: List[_] forSome { type _ >: Null <: AnyRef }
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local12 => param x: List[_] forSome { type _ >: Null <: AnyRef }
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local13 => param x: List[_] forSome { type _ >: Null <: AnyRef }
             |  List => scala/package.List#
             |  _ => local14
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local14 => synthetic abstract type _ >: Null <: AnyRef
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local15 => synthetic abstract type _ >: Null <: AnyRef
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local16 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local17 => abstract type T <: AnyRef
             |  AnyRef => scala/AnyRef#
             |local19 => final method applyOrElse[A1 <: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local24
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |  B1 => local23
             |  Int => scala/Int#
             |  x => local21
             |  default => local22
             |  Function1 => scala/Function1#
             |local20 => final method isDefinedAt(x: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local25
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |  Boolean => scala/Boolean#
             |local21 => synthetic param x: A1
             |  A1 => local24
             |local22 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local24
             |  B1 => local23
             |local23 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local24 => typeparam A1 <: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |local25 => param x: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |local26 => val local v: T
             |  T => local17
             |local27 => param x: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local28
             |  v.type => local29
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |local28 => abstract type T <: AnyRef
             |  AnyRef => scala/AnyRef#
             |local30 => val local v: T
             |  T => local28
             |local31 => abstract type T <: AnyRef
             |  AnyRef => scala/AnyRef#
             |local32 => val local v: T
             |  T => local31
             |local34 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |  Serializable => scala/Serializable#
             |local35 => abstract type T
             |local36 => abstract method f(x: Int): Int
             |  x => local37
             |  Int => scala/Int#
             |local37 => param x: Int
             |  Int => scala/Int#
             |local38 => abstract val method v: Int
             |  Int => scala/Int#
             |local39 => final method applyOrElse[A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local44
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |  B1 => local43
             |  x => local41
             |  default => local42
             |  Function1 => scala/Function1#
             |local40 => final method isDefinedAt(x: AnyRef { type T; def f(x: Int): Int; val def v: Int }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local45
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |  Boolean => scala/Boolean#
             |local41 => synthetic param x: A1
             |  A1 => local44
             |local42 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local44
             |  B1 => local43
             |local43 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local44 => typeparam A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |local45 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |local46 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local47
             |  f => local48
             |  x => local49
             |  Int => scala/Int#
             |  v => local50
             |local47 => abstract type T
             |local48 => abstract method f(x: Int): Int
             |  x => local49
             |  Int => scala/Int#
             |local49 => param x: Int
             |  Int => scala/Int#
             |local50 => abstract val method v: Int
             |  Int => scala/Int#
             |local51 => abstract type T
             |local52 => abstract method f(x: Int): Int
             |  x => local53
             |  Int => scala/Int#
             |local53 => param x: Int
             |  Int => scala/Int#
             |local54 => abstract val method v: Int
             |  Int => scala/Int#
             |local55 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |  Serializable => scala/Serializable#
             |local56 => abstract method g[A](a: A)(implicit evidence$1: Ordering[A]): Int
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local57 => param a: A
             |  A => local58
             |local58 => typeparam A
             |local59 => synthetic implicit param evidence$1: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local58
             |local60 => abstract method h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |local61 => param b: B
             |  B => local62
             |local62 => typeparam B
             |local63 => synthetic implicit param evidence$2: Function1[B, Int]
             |  Function1 => scala/Function1#
             |  B => local62
             |  Int => scala/Int#
             |local64 => final method applyOrElse[A1 <: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local69
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |  B1 => local68
             |  x => local66
             |  default => local67
             |local65 => final method isDefinedAt(x: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local70
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |  Boolean => scala/Boolean#
             |local66 => synthetic param x: A1
             |  A1 => local69
             |local67 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local69
             |  B1 => local68
             |local68 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local69 => typeparam A1 <: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |local70 => param x: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |local71 => param x: AnyRef { def g[A](a: A)(implicit evidence$7: Ordering[A]): Int; def h[B](b: B)(implicit evidence$8: Function1[B, Int]): Int }
             |  AnyRef => scala/AnyRef#
             |  g => local72
             |  A => local74
             |  a => local73
             |  evidence$7 => local75
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local76
             |  B => local78
             |  b => local77
             |  evidence$8 => local79
             |  Function1 => scala/Function1#
             |local72 => abstract method g[A](a: A)(implicit evidence$7: Ordering[A]): Int
             |  A => local74
             |  a => local73
             |  evidence$7 => local75
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local73 => param a: A
             |  A => local74
             |local74 => typeparam A
             |local75 => synthetic implicit param evidence$7: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local74
             |local76 => abstract method h[B](b: B)(implicit evidence$8: Function1[B, Int]): Int
             |  B => local78
             |  b => local77
             |  evidence$8 => local79
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |local77 => param b: B
             |  B => local78
             |local78 => typeparam B
             |local79 => synthetic implicit param evidence$8: Function1[B, Int]
             |  Function1 => scala/Function1#
             |  B => local78
             |  Int => scala/Int#
             |local80 => abstract method g[A](a: A)(implicit evidence$5: Ordering[A]): Int
             |  A => local83
             |  a => local81
             |  evidence$5 => local82
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local81 => param a: A
             |  A => local83
             |local82 => synthetic implicit param evidence$5: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local83
             |local83 => typeparam A
             |local84 => abstract method h[B](b: B)(implicit evidence$6: Function1[B, Int]): Int
             |  B => local87
             |  b => local85
             |  evidence$6 => local86
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |local85 => param b: B
             |  B => local87
             |local86 => synthetic implicit param evidence$6: Function1[B, Int]
             |  Function1 => scala/Function1#
             |  B => local87
             |  Int => scala/Int#
             |local87 => typeparam B
             |""".stripMargin,
        ScalaVersion.AtMost213(16) ->
          """|_empty_/t241. => final object t241 extends AnyRef { +9 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t241.R# => type R = AnyRef { type T; def f(x: Int): Int; val def v: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local0
             |  f => local1
             |  x => local2
             |  Int => scala/Int#
             |  v => local3
             |_empty_/t241.a. => val method a: PartialFunction[List[_] forSome { type _ >: Null <: AnyRef }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |  Int => scala/Int#
             |_empty_/t241.b. => val method b: Function1[List[_] forSome { type _ >: Null <: AnyRef }, Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  _ => local15
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |  Int => scala/Int#
             |_empty_/t241.c. => val method c: PartialFunction[Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |  Int => scala/Int#
             |_empty_/t241.d. => val method d: Function1[Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, Int]
             |  Function1 => scala/Function1#
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local31
             |  v.type => local33
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |  Int => scala/Int#
             |_empty_/t241.e. => val method e: PartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |_empty_/t241.f. => val method f: Function1[AnyRef { type T; def f(x: Int): Int; val def v: Int }, Int]
             |  Function1 => scala/Function1#
             |  AnyRef => scala/AnyRef#
             |  T => local51
             |  f => local52
             |  x => local53
             |  Int => scala/Int#
             |  v => local54
             |_empty_/t241.g. => val method g: PartialFunction[AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |_empty_/t241.h. => val method h: Function1[AnyRef { def g[A](a: A)(implicit evidence$5: Ordering[A]): Int; def h[B](b: B)(implicit evidence$6: Function1[B, Int]): Int }, Int]
             |  Function1 => scala/Function1#
             |  AnyRef => scala/AnyRef#
             |  g => local80
             |  A => local83
             |  a => local81
             |  evidence$5 => local82
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local84
             |  B => local87
             |  b => local85
             |  evidence$6 => local86
             |local0 => abstract type T
             |local1 => abstract method f(x: Int): Int
             |  x => local2
             |  Int => scala/Int#
             |local2 => param x: Int
             |  Int => scala/Int#
             |local3 => abstract val method v: Int
             |  Int => scala/Int#
             |local4 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ >: Null <: AnyRef }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local5 => synthetic abstract type _ >: Null <: AnyRef
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local6 => final method applyOrElse[A1 <: List[_] forSome { type _ >: Null <: AnyRef }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local11
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |  B1 => local10
             |  Int => scala/Int#
             |  x => local8
             |  default => local9
             |  Function1 => scala/Function1#
             |local7 => final method isDefinedAt(x: List[_] forSome { type _ >: Null <: AnyRef }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local12
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |  Boolean => scala/Boolean#
             |local8 => synthetic param x: A1
             |  A1 => local11
             |local9 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local11
             |  B1 => local10
             |local10 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local11 => typeparam A1 <: List[_] forSome { type _ >: Null <: AnyRef }
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local12 => param x: List[_] forSome { type _ >: Null <: AnyRef }
             |  List => scala/package.List#
             |  _ => local5
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local13 => param x: List[_] forSome { type _ >: Null <: AnyRef }
             |  List => scala/package.List#
             |  _ => local14
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local14 => synthetic abstract type _ >: Null <: AnyRef
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local15 => synthetic abstract type _ >: Null <: AnyRef
             |  Null => scala/Null#
             |  AnyRef => scala/AnyRef#
             |local16 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local17 => abstract type T <: AnyRef
             |  AnyRef => scala/AnyRef#
             |local19 => final method applyOrElse[A1 <: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local24
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |  B1 => local23
             |  Int => scala/Int#
             |  x => local21
             |  default => local22
             |  Function1 => scala/Function1#
             |local20 => final method isDefinedAt(x: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local25
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |  Boolean => scala/Boolean#
             |local21 => synthetic param x: A1
             |  A1 => local24
             |local22 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local24
             |  B1 => local23
             |local23 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local24 => typeparam A1 <: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |local25 => param x: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local17
             |  v.type => local18
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |local26 => val local v: T
             |  T => local17
             |local27 => param x: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }
             |  Tuple2 => scala/Tuple2#
             |  List => scala/package.List#
             |  T => local28
             |  v.type => local29
             |  AnyRef => scala/AnyRef#
             |  Singleton => scala/Singleton#
             |local28 => abstract type T <: AnyRef
             |  AnyRef => scala/AnyRef#
             |local30 => val local v: T
             |  T => local28
             |local31 => abstract type T <: AnyRef
             |  AnyRef => scala/AnyRef#
             |local32 => val local v: T
             |  T => local31
             |local34 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |  Serializable => java/io/Serializable#
             |local35 => abstract type T
             |local36 => abstract method f(x: Int): Int
             |  x => local37
             |  Int => scala/Int#
             |local37 => param x: Int
             |  Int => scala/Int#
             |local38 => abstract val method v: Int
             |  Int => scala/Int#
             |local39 => final method applyOrElse[A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local44
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |  B1 => local43
             |  x => local41
             |  default => local42
             |  Function1 => scala/Function1#
             |local40 => final method isDefinedAt(x: AnyRef { type T; def f(x: Int): Int; val def v: Int }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local45
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |  Boolean => scala/Boolean#
             |local41 => synthetic param x: A1
             |  A1 => local44
             |local42 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local44
             |  B1 => local43
             |local43 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local44 => typeparam A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |local45 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local35
             |  f => local36
             |  x => local37
             |  Int => scala/Int#
             |  v => local38
             |local46 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local47
             |  f => local48
             |  x => local49
             |  Int => scala/Int#
             |  v => local50
             |local47 => abstract type T
             |local48 => abstract method f(x: Int): Int
             |  x => local49
             |  Int => scala/Int#
             |local49 => param x: Int
             |  Int => scala/Int#
             |local50 => abstract val method v: Int
             |  Int => scala/Int#
             |local51 => abstract type T
             |local52 => abstract method f(x: Int): Int
             |  x => local53
             |  Int => scala/Int#
             |local53 => param x: Int
             |  Int => scala/Int#
             |local54 => abstract val method v: Int
             |  Int => scala/Int#
             |local55 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |  Serializable => java/io/Serializable#
             |local56 => abstract method g[A](a: A)(implicit evidence$1: Ordering[A]): Int
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local57 => param a: A
             |  A => local58
             |local58 => typeparam A
             |local59 => synthetic implicit param evidence$1: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local58
             |local60 => abstract method h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |local61 => param b: B
             |  B => local62
             |local62 => typeparam B
             |local63 => synthetic implicit param evidence$2: Function1[B, Int]
             |  Function1 => scala/Function1#
             |  B => local62
             |  Int => scala/Int#
             |local64 => final method applyOrElse[A1 <: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local69
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |  B1 => local68
             |  x => local66
             |  default => local67
             |local65 => final method isDefinedAt(x: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local70
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |  Boolean => scala/Boolean#
             |local66 => synthetic param x: A1
             |  A1 => local69
             |local67 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local69
             |  B1 => local68
             |local68 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local69 => typeparam A1 <: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |local70 => param x: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }
             |  AnyRef => scala/AnyRef#
             |  g => local56
             |  A => local58
             |  a => local57
             |  evidence$1 => local59
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local60
             |  B => local62
             |  b => local61
             |  evidence$2 => local63
             |  Function1 => scala/Function1#
             |local71 => param x: AnyRef { def g[A](a: A)(implicit evidence$7: Ordering[A]): Int; def h[B](b: B)(implicit evidence$8: Function1[B, Int]): Int }
             |  AnyRef => scala/AnyRef#
             |  g => local72
             |  A => local74
             |  a => local73
             |  evidence$7 => local75
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  h => local76
             |  B => local78
             |  b => local77
             |  evidence$8 => local79
             |  Function1 => scala/Function1#
             |local72 => abstract method g[A](a: A)(implicit evidence$7: Ordering[A]): Int
             |  A => local74
             |  a => local73
             |  evidence$7 => local75
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local73 => param a: A
             |  A => local74
             |local74 => typeparam A
             |local75 => synthetic implicit param evidence$7: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local74
             |local76 => abstract method h[B](b: B)(implicit evidence$8: Function1[B, Int]): Int
             |  B => local78
             |  b => local77
             |  evidence$8 => local79
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |local77 => param b: B
             |  B => local78
             |local78 => typeparam B
             |local79 => synthetic implicit param evidence$8: Function1[B, Int]
             |  Function1 => scala/Function1#
             |  B => local78
             |  Int => scala/Int#
             |local80 => abstract method g[A](a: A)(implicit evidence$5: Ordering[A]): Int
             |  A => local83
             |  a => local81
             |  evidence$5 => local82
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local81 => param a: A
             |  A => local83
             |local82 => synthetic implicit param evidence$5: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local83
             |local83 => typeparam A
             |local84 => abstract method h[B](b: B)(implicit evidence$6: Function1[B, Int]): Int
             |  B => local87
             |  b => local85
             |  evidence$6 => local86
             |  Function1 => scala/Function1#
             |  Int => scala/Int#
             |local85 => param b: B
             |  B => local87
             |local86 => synthetic implicit param evidence$6: Function1[B, Int]
             |  Function1 => scala/Function1#
             |  B => local87
             |  Int => scala/Int#
             |local87 => typeparam B
             |""".stripMargin,
      ),
      """|_empty_/t241. => final object t241 extends AnyRef { +9 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t241.R# => type R = AnyRef { type T; def f(x: Int): Int; val def v: Int }
         |  AnyRef => scala/AnyRef#
         |  T => local0
         |  f => local1
         |  x => local2
         |  Int => scala/Int#
         |  v => local3
         |_empty_/t241.a. => val method a: PartialFunction[List[_] forSome { type _ >: Null <: AnyRef }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  _ => local13
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |  Int => scala/Int#
         |_empty_/t241.b. => val method b: Function1[List[_] forSome { type _ >: Null <: AnyRef }, Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  _ => local16
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |  Int => scala/Int#
         |_empty_/t241.c. => val method c: PartialFunction[Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local28
         |  v.type => local30
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |_empty_/t241.d. => val method d: Function1[Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, Int]
         |  Function1 => scala/Function1#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local35
         |  v.type => local37
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |_empty_/t241.e. => val method e: PartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  AnyRef => scala/AnyRef#
         |  T => local50
         |  f => local51
         |  x => local52
         |  Int => scala/Int#
         |  v => local53
         |_empty_/t241.f. => val method f: Function1[AnyRef { type T; def f(x: Int): Int; val def v: Int }, Int]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  T => local59
         |  f => local60
         |  x => local61
         |  Int => scala/Int#
         |  v => local62
         |_empty_/t241.g. => val method g: PartialFunction[AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  AnyRef => scala/AnyRef#
         |  g => local79
         |  A => local82
         |  a => local80
         |  evidence$1 => local81
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  h => local83
         |  B => local86
         |  b => local84
         |  evidence$2 => local85
         |  Function1 => scala/Function1#
         |_empty_/t241.h. => val method h: Function1[AnyRef { def g[A](a: A)(implicit evidence$5: Ordering[A]): Int; def h[B](b: B)(implicit evidence$6: Function1[B, Int]): Int }, Int]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  g => local96
         |  A => local99
         |  a => local97
         |  evidence$5 => local98
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  h => local100
         |  B => local103
         |  b => local101
         |  evidence$6 => local102
         |local0 => abstract type T
         |local1 => abstract method f(x: Int): Int
         |  x => local2
         |  Int => scala/Int#
         |local2 => param x: Int
         |  Int => scala/Int#
         |local3 => abstract val method v: Int
         |  Int => scala/Int#
         |local4 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ >: Null <: AnyRef }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  List => scala/package.List#
         |  _ => local5
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local5 => abstract type _ >: Null <: AnyRef
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |local6 => final method applyOrElse[A1 <: List[_] forSome { type _ >: Null <: AnyRef }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local11
         |  List => scala/package.List#
         |  _ => local5
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |  B1 => local10
         |  Int => scala/Int#
         |  x => local8
         |  default => local9
         |  Function1 => scala/Function1#
         |local7 => final method isDefinedAt(x: List[_] forSome { type _ >: Null <: AnyRef }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local12
         |  List => scala/package.List#
         |  _ => local5
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |  Boolean => scala/Boolean#
         |local8 => synthetic param x: A1
         |  A1 => local11
         |local9 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local11
         |  B1 => local10
         |local10 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local11 => typeparam A1 <: List[_] forSome { type _ >: Null <: AnyRef }
         |  List => scala/package.List#
         |  _ => local5
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |local12 => param x: List[_] forSome { type _ >: Null <: AnyRef }
         |  List => scala/package.List#
         |  _ => local5
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |local13 => synthetic abstract type _ >: Null <: AnyRef
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |local14 => param x: List[_] forSome { type _ >: Null <: AnyRef }
         |  List => scala/package.List#
         |  _ => local15
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |local15 => synthetic abstract type _ >: Null <: AnyRef
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |local16 => synthetic abstract type _ >: Null <: AnyRef
         |  Null => scala/Null#
         |  AnyRef => scala/AnyRef#
         |local17 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local18
         |  v.type => local19
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local18 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local20 => final method applyOrElse[A1 <: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local25
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local18
         |  v.type => local19
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  B1 => local24
         |  Int => scala/Int#
         |  x => local22
         |  default => local23
         |  Function1 => scala/Function1#
         |local21 => final method isDefinedAt(x: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local26
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local18
         |  v.type => local19
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Boolean => scala/Boolean#
         |local22 => synthetic param x: A1
         |  A1 => local25
         |local23 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local25
         |  B1 => local24
         |local24 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local25 => typeparam A1 <: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local18
         |  v.type => local19
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local26 => param x: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local18
         |  v.type => local19
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local27 => val local v: T
         |  T => local18
         |local28 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local29 => val local v: T
         |  T => local28
         |local31 => param x: Tuple2[List[T], v.type] forSome { type T <: AnyRef; type v.type <: AnyRef with Singleton {} }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local32
         |  v.type => local33
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local32 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local34 => val local v: T
         |  T => local32
         |local35 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local36 => val local v: T
         |  T => local35
         |local38 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  AnyRef => scala/AnyRef#
         |  T => local39
         |  f => local40
         |  x => local41
         |  Int => scala/Int#
         |  v => local42
         |  Serializable => java/io/Serializable#
         |local39 => abstract type T
         |local40 => abstract method f(x: Int): Int
         |  x => local41
         |  Int => scala/Int#
         |local41 => param x: Int
         |  Int => scala/Int#
         |local42 => abstract val method v: Int
         |  Int => scala/Int#
         |local43 => final method applyOrElse[A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local48
         |  AnyRef => scala/AnyRef#
         |  T => local39
         |  f => local40
         |  x => local41
         |  Int => scala/Int#
         |  v => local42
         |  B1 => local47
         |  x => local45
         |  default => local46
         |  Function1 => scala/Function1#
         |local44 => final method isDefinedAt(x: AnyRef { type T; def f(x: Int): Int; val def v: Int }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local49
         |  AnyRef => scala/AnyRef#
         |  T => local39
         |  f => local40
         |  x => local41
         |  Int => scala/Int#
         |  v => local42
         |  Boolean => scala/Boolean#
         |local45 => synthetic param x: A1
         |  A1 => local48
         |local46 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local48
         |  B1 => local47
         |local47 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local48 => typeparam A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int }
         |  AnyRef => scala/AnyRef#
         |  T => local39
         |  f => local40
         |  x => local41
         |  Int => scala/Int#
         |  v => local42
         |local49 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int }
         |  AnyRef => scala/AnyRef#
         |  T => local39
         |  f => local40
         |  x => local41
         |  Int => scala/Int#
         |  v => local42
         |local50 => abstract type T
         |local51 => abstract method f(x: Int): Int
         |  x => local52
         |  Int => scala/Int#
         |local52 => param x: Int
         |  Int => scala/Int#
         |local53 => abstract val method v: Int
         |  Int => scala/Int#
         |local54 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int }
         |  AnyRef => scala/AnyRef#
         |  T => local55
         |  f => local56
         |  x => local57
         |  Int => scala/Int#
         |  v => local58
         |local55 => abstract type T
         |local56 => abstract method f(x: Int): Int
         |  x => local57
         |  Int => scala/Int#
         |local57 => param x: Int
         |  Int => scala/Int#
         |local58 => abstract val method v: Int
         |  Int => scala/Int#
         |local59 => abstract type T
         |local60 => abstract method f(x: Int): Int
         |  x => local61
         |  Int => scala/Int#
         |local61 => param x: Int
         |  Int => scala/Int#
         |local62 => abstract val method v: Int
         |  Int => scala/Int#
         |local63 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { def g[A](a: A)(implicit evidence$3: Ordering[A]): Int; def h[B](b: B)(implicit evidence$4: Function1[B, Int]): Int }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  AnyRef => scala/AnyRef#
         |  g => local64
         |  A => local66
         |  a => local65
         |  evidence$3 => local67
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  h => local68
         |  B => local70
         |  b => local69
         |  evidence$4 => local71
         |  Function1 => scala/Function1#
         |  Serializable => java/io/Serializable#
         |local64 => abstract method g[A](a: A)(implicit evidence$3: Ordering[A]): Int
         |  A => local66
         |  a => local65
         |  evidence$3 => local67
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local65 => param a: A
         |  A => local66
         |local66 => typeparam A
         |local67 => synthetic implicit param evidence$3: Ordering[A]
         |  Ordering => scala/package.Ordering#
         |  A => local66
         |local68 => abstract method h[B](b: B)(implicit evidence$4: Function1[B, Int]): Int
         |  B => local70
         |  b => local69
         |  evidence$4 => local71
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |local69 => param b: B
         |  B => local70
         |local70 => typeparam B
         |local71 => synthetic implicit param evidence$4: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => local70
         |  Int => scala/Int#
         |local72 => final method applyOrElse[A1 <: AnyRef { def g[A](a: A)(implicit evidence$3: Ordering[A]): Int; def h[B](b: B)(implicit evidence$4: Function1[B, Int]): Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local77
         |  AnyRef => scala/AnyRef#
         |  g => local64
         |  A => local66
         |  a => local65
         |  evidence$3 => local67
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  h => local68
         |  B => local70
         |  b => local69
         |  evidence$4 => local71
         |  Function1 => scala/Function1#
         |  B1 => local76
         |  x => local74
         |  default => local75
         |local73 => final method isDefinedAt(x: AnyRef { def g[A](a: A)(implicit evidence$3: Ordering[A]): Int; def h[B](b: B)(implicit evidence$4: Function1[B, Int]): Int }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local78
         |  AnyRef => scala/AnyRef#
         |  g => local64
         |  A => local66
         |  a => local65
         |  evidence$3 => local67
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  h => local68
         |  B => local70
         |  b => local69
         |  evidence$4 => local71
         |  Function1 => scala/Function1#
         |  Boolean => scala/Boolean#
         |local74 => synthetic param x: A1
         |  A1 => local77
         |local75 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local77
         |  B1 => local76
         |local76 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local77 => typeparam A1 <: AnyRef { def g[A](a: A)(implicit evidence$3: Ordering[A]): Int; def h[B](b: B)(implicit evidence$4: Function1[B, Int]): Int }
         |  AnyRef => scala/AnyRef#
         |  g => local64
         |  A => local66
         |  a => local65
         |  evidence$3 => local67
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  h => local68
         |  B => local70
         |  b => local69
         |  evidence$4 => local71
         |  Function1 => scala/Function1#
         |local78 => param x: AnyRef { def g[A](a: A)(implicit evidence$3: Ordering[A]): Int; def h[B](b: B)(implicit evidence$4: Function1[B, Int]): Int }
         |  AnyRef => scala/AnyRef#
         |  g => local64
         |  A => local66
         |  a => local65
         |  evidence$3 => local67
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  h => local68
         |  B => local70
         |  b => local69
         |  evidence$4 => local71
         |  Function1 => scala/Function1#
         |local79 => abstract method g[A](a: A)(implicit evidence$1: Ordering[A]): Int
         |  A => local82
         |  a => local80
         |  evidence$1 => local81
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local80 => param a: A
         |  A => local82
         |local81 => synthetic implicit param evidence$1: Ordering[A]
         |  Ordering => scala/package.Ordering#
         |  A => local82
         |local82 => typeparam A
         |local83 => abstract method h[B](b: B)(implicit evidence$2: Function1[B, Int]): Int
         |  B => local86
         |  b => local84
         |  evidence$2 => local85
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |local84 => param b: B
         |  B => local86
         |local85 => synthetic implicit param evidence$2: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => local86
         |  Int => scala/Int#
         |local86 => typeparam B
         |local87 => param x: AnyRef { def g[A](a: A)(implicit evidence$7: Ordering[A]): Int; def h[B](b: B)(implicit evidence$8: Function1[B, Int]): Int }
         |  AnyRef => scala/AnyRef#
         |  g => local88
         |  A => local90
         |  a => local89
         |  evidence$7 => local91
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  h => local92
         |  B => local94
         |  b => local93
         |  evidence$8 => local95
         |  Function1 => scala/Function1#
         |local88 => abstract method g[A](a: A)(implicit evidence$7: Ordering[A]): Int
         |  A => local90
         |  a => local89
         |  evidence$7 => local91
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local89 => param a: A
         |  A => local90
         |local90 => typeparam A
         |local91 => synthetic implicit param evidence$7: Ordering[A]
         |  Ordering => scala/package.Ordering#
         |  A => local90
         |local92 => abstract method h[B](b: B)(implicit evidence$8: Function1[B, Int]): Int
         |  B => local94
         |  b => local93
         |  evidence$8 => local95
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |local93 => param b: B
         |  B => local94
         |local94 => typeparam B
         |local95 => synthetic implicit param evidence$8: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => local94
         |  Int => scala/Int#
         |local96 => abstract method g[A](a: A)(implicit evidence$5: Ordering[A]): Int
         |  A => local99
         |  a => local97
         |  evidence$5 => local98
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local97 => param a: A
         |  A => local99
         |local98 => synthetic implicit param evidence$5: Ordering[A]
         |  Ordering => scala/package.Ordering#
         |  A => local99
         |local99 => typeparam A
         |local100 => abstract method h[B](b: B)(implicit evidence$6: Function1[B, Int]): Int
         |  B => local103
         |  b => local101
         |  evidence$6 => local102
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |local101 => param b: B
         |  B => local103
         |local102 => synthetic implicit param evidence$6: Function1[B, Int]
         |  Function1 => scala/Function1#
         |  B => local103
         |  Int => scala/Int#
         |local103 => typeparam B
         |""".stripMargin,
    ),
  )

  symbols(
    "typed parameters of partial function lambdas with wildcards, existentials and refinements of a supertype",
    """|object t242 {
       |  type L[A] = List[A]
       |  val a: PartialFunction[List[_], Int] = (x: L[_]) => x match { case _ => 1 }
       |  val b: List[_] => Int = (x: L[_]) => x match { case _ => 1 }
       |  val c: PartialFunction[List[_ <: Int], Int] = (x: Seq[_ <: AnyVal]) => x match { case _ => 1 }
       |  val d: List[_ <: Int] => Int = (x: Seq[_ <: AnyVal]) => x match { case _ => 1 }
       |  val e: PartialFunction[List[T] forSome { type T }, Int] =
       |    (x: Seq[T] forSome { type T }) => x match { case _ => 1 }
       |  val f: (List[T] forSome { type T }) => Int = (x: Seq[T] forSome { type T }) => x match { case _ => 1 }
       |  val g: PartialFunction[AnyRef { type T; def f(x: Int): Int; val v: Int; def k: Int }, Int] =
       |    (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 }
       |  val h: (AnyRef { type T; def f(x: Int): Int; val v: Int; def k: Int }) => Int =
       |    (x: AnyRef { type T; def f(x: Int): Int; val v: Int }) => x match { case _ => 1 }
       |  val i: PartialFunction[AnyRef { def g[A: Ordering](a: A): Int; def k: Int }, Int] =
       |    (x: AnyRef { def g[A: Ordering](a: A): Int }) => x match { case _ => 1 }
       |  val j: (AnyRef { def g[A: Ordering](a: A): Int; def k: Int }) => Int =
       |    (x: AnyRef { def g[A: Ordering](a: A): Int }) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List(
        ScalaVersion.Scala212 ->
          """|_empty_/t242. => final object t242 extends AnyRef { +11 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t242.L# => type L[A] = List[A]
             |  A => _empty_/t242.L#[A]
             |  List => scala/package.List#
             |_empty_/t242.L#[A] => typeparam A
             |_empty_/t242.a. => val method a: PartialFunction[List[_] forSome { type _ }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  _ => local1
             |  Int => scala/Int#
             |_empty_/t242.b. => val method b: Function1[List[_] forSome { type _ }, Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  _ => local11
             |  Int => scala/Int#
             |_empty_/t242.c. => val method c: PartialFunction[List[_] forSome { type _ <: Int }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |_empty_/t242.d. => val method d: Function1[List[_] forSome { type _ <: Int }, Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  _ => local23
             |  Int => scala/Int#
             |_empty_/t242.e. => val method e: PartialFunction[List[T] forSome { type T }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  T => local25
             |  Int => scala/Int#
             |_empty_/t242.f. => val method f: Function1[List[T] forSome { type T }, Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  T => local35
             |  Int => scala/Int#
             |_empty_/t242.g. => val method g: PartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |_empty_/t242.h. => val method h: Function1[AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }, Int]
             |  Function1 => scala/Function1#
             |  AnyRef => scala/AnyRef#
             |  T => local54
             |  f => local55
             |  x => local56
             |  Int => scala/Int#
             |  v => local57
             |  k => local58
             |_empty_/t242.i. => val method i: PartialFunction[AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |_empty_/t242.j. => val method j: Function1[AnyRef { def g[A](a: A)(implicit evidence$3: Ordering[A]): Int; def k: Int }, Int]
             |  Function1 => scala/Function1#
             |  AnyRef => scala/AnyRef#
             |  g => local77
             |  A => local80
             |  a => local78
             |  evidence$3 => local79
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local81
             |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  _ => local1
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local1 => synthetic abstract type _
             |local2 => final method applyOrElse[A1 <: List[_] forSome { type _ }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local7
             |  List => scala/package.List#
             |  _ => local1
             |  B1 => local6
             |  Int => scala/Int#
             |  x => local4
             |  default => local5
             |  Function1 => scala/Function1#
             |local3 => final method isDefinedAt(x: List[_] forSome { type _ }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local8
             |  List => scala/package.List#
             |  _ => local1
             |  Boolean => scala/Boolean#
             |local4 => synthetic param x: A1
             |  A1 => local7
             |local5 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local7
             |  B1 => local6
             |local6 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local7 => typeparam A1 <: List[_] forSome { type _ }
             |  List => scala/package.List#
             |  _ => local1
             |local8 => param x: List[_] forSome { type _ }
             |  List => scala/package.List#
             |  _ => local1
             |local9 => param x: L[_] forSome { type _ }
             |  L => _empty_/t242.L#
             |  _ => local10
             |local10 => synthetic abstract type _
             |local11 => synthetic abstract type _
             |local12 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ <: Int }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local13 => synthetic abstract type _ <: Int
             |  Int => scala/Int#
             |local14 => final method applyOrElse[A1 <: List[_] forSome { type _ <: Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local19
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |  B1 => local18
             |  x => local16
             |  default => local17
             |  Function1 => scala/Function1#
             |local15 => final method isDefinedAt(x: List[_] forSome { type _ <: Int }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local20
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local16 => synthetic param x: A1
             |  A1 => local19
             |local17 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local19
             |  B1 => local18
             |local18 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local19 => typeparam A1 <: List[_] forSome { type _ <: Int }
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |local20 => param x: List[_] forSome { type _ <: Int }
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |local21 => param x: Seq[_] forSome { type _ <: AnyVal }
             |  Seq => scala/package.Seq#
             |  _ => local22
             |  AnyVal => scala/AnyVal#
             |local22 => synthetic abstract type _ <: AnyVal
             |  AnyVal => scala/AnyVal#
             |local23 => synthetic abstract type _ <: Int
             |  Int => scala/Int#
             |local24 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[T] forSome { type T }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  T => local25
             |  Int => scala/Int#
             |  Serializable => scala/Serializable#
             |local25 => abstract type T
             |local26 => final method applyOrElse[A1 <: List[T] forSome { type T }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local31
             |  List => scala/package.List#
             |  T => local25
             |  B1 => local30
             |  Int => scala/Int#
             |  x => local28
             |  default => local29
             |  Function1 => scala/Function1#
             |local27 => final method isDefinedAt(x: List[T] forSome { type T }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local32
             |  List => scala/package.List#
             |  T => local25
             |  Boolean => scala/Boolean#
             |local28 => synthetic param x: A1
             |  A1 => local31
             |local29 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local31
             |  B1 => local30
             |local30 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local31 => typeparam A1 <: List[T] forSome { type T }
             |  List => scala/package.List#
             |  T => local25
             |local32 => param x: List[T] forSome { type T }
             |  List => scala/package.List#
             |  T => local25
             |local33 => param x: Seq[T] forSome { type T }
             |  Seq => scala/package.Seq#
             |  T => local34
             |local34 => abstract type T
             |local35 => abstract type T
             |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |  Serializable => scala/Serializable#
             |local37 => abstract type T
             |local38 => abstract method f(x: Int): Int
             |  x => local39
             |  Int => scala/Int#
             |local39 => param x: Int
             |  Int => scala/Int#
             |local40 => abstract val method v: Int
             |  Int => scala/Int#
             |local41 => abstract method k: Int
             |  Int => scala/Int#
             |local42 => final method applyOrElse[A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local47
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |  B1 => local46
             |  x => local44
             |  default => local45
             |  Function1 => scala/Function1#
             |local43 => final method isDefinedAt(x: AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local48
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |  Boolean => scala/Boolean#
             |local44 => synthetic param x: A1
             |  A1 => local47
             |local45 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local47
             |  B1 => local46
             |local46 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local47 => typeparam A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |local48 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |local49 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local50
             |  f => local51
             |  x => local52
             |  Int => scala/Int#
             |  v => local53
             |local50 => abstract type T
             |local51 => abstract method f(x: Int): Int
             |  x => local52
             |  Int => scala/Int#
             |local52 => param x: Int
             |  Int => scala/Int#
             |local53 => abstract val method v: Int
             |  Int => scala/Int#
             |local54 => abstract type T
             |local55 => abstract method f(x: Int): Int
             |  x => local56
             |  Int => scala/Int#
             |local56 => param x: Int
             |  Int => scala/Int#
             |local57 => abstract val method v: Int
             |  Int => scala/Int#
             |local58 => abstract method k: Int
             |  Int => scala/Int#
             |local59 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |  Serializable => scala/Serializable#
             |local60 => abstract method g[A](a: A)(implicit evidence$1: Ordering[A]): Int
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local61 => param a: A
             |  A => local62
             |local62 => typeparam A
             |local63 => synthetic implicit param evidence$1: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local62
             |local64 => abstract method k: Int
             |  Int => scala/Int#
             |local65 => final method applyOrElse[A1 <: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local70
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |  B1 => local69
             |  x => local67
             |  default => local68
             |  Function1 => scala/Function1#
             |local66 => final method isDefinedAt(x: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local71
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |  Boolean => scala/Boolean#
             |local67 => synthetic param x: A1
             |  A1 => local70
             |local68 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local70
             |  B1 => local69
             |local69 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local70 => typeparam A1 <: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |local71 => param x: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |local72 => param x: AnyRef { def g[A](a: A)(implicit evidence$4: Ordering[A]): Int }
             |  AnyRef => scala/AnyRef#
             |  g => local73
             |  A => local75
             |  a => local74
             |  evidence$4 => local76
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local73 => abstract method g[A](a: A)(implicit evidence$4: Ordering[A]): Int
             |  A => local75
             |  a => local74
             |  evidence$4 => local76
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local74 => param a: A
             |  A => local75
             |local75 => typeparam A
             |local76 => synthetic implicit param evidence$4: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local75
             |local77 => abstract method g[A](a: A)(implicit evidence$3: Ordering[A]): Int
             |  A => local80
             |  a => local78
             |  evidence$3 => local79
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local78 => param a: A
             |  A => local80
             |local79 => synthetic implicit param evidence$3: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local80
             |local80 => typeparam A
             |local81 => abstract method k: Int
             |  Int => scala/Int#
             |""".stripMargin,
        ScalaVersion.AtMost213(16) ->
          """|_empty_/t242. => final object t242 extends AnyRef { +11 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t242.L# => type L[A] = List[A]
             |  A => _empty_/t242.L#[A]
             |  List => scala/package.List#
             |_empty_/t242.L#[A] => typeparam A
             |_empty_/t242.a. => val method a: PartialFunction[List[_] forSome { type _ }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  _ => local1
             |  Int => scala/Int#
             |_empty_/t242.b. => val method b: Function1[List[_] forSome { type _ }, Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  _ => local11
             |  Int => scala/Int#
             |_empty_/t242.c. => val method c: PartialFunction[List[_] forSome { type _ <: Int }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |_empty_/t242.d. => val method d: Function1[List[_] forSome { type _ <: Int }, Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  _ => local23
             |  Int => scala/Int#
             |_empty_/t242.e. => val method e: PartialFunction[List[T] forSome { type T }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  List => scala/package.List#
             |  T => local25
             |  Int => scala/Int#
             |_empty_/t242.f. => val method f: Function1[List[T] forSome { type T }, Int]
             |  Function1 => scala/Function1#
             |  List => scala/package.List#
             |  T => local35
             |  Int => scala/Int#
             |_empty_/t242.g. => val method g: PartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |_empty_/t242.h. => val method h: Function1[AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }, Int]
             |  Function1 => scala/Function1#
             |  AnyRef => scala/AnyRef#
             |  T => local54
             |  f => local55
             |  x => local56
             |  Int => scala/Int#
             |  v => local57
             |  k => local58
             |_empty_/t242.i. => val method i: PartialFunction[AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }, Int]
             |  PartialFunction => scala/PartialFunction#
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |_empty_/t242.j. => val method j: Function1[AnyRef { def g[A](a: A)(implicit evidence$3: Ordering[A]): Int; def k: Int }, Int]
             |  Function1 => scala/Function1#
             |  AnyRef => scala/AnyRef#
             |  g => local77
             |  A => local80
             |  a => local78
             |  evidence$3 => local79
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local81
             |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  _ => local1
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local1 => synthetic abstract type _
             |local2 => final method applyOrElse[A1 <: List[_] forSome { type _ }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local7
             |  List => scala/package.List#
             |  _ => local1
             |  B1 => local6
             |  Int => scala/Int#
             |  x => local4
             |  default => local5
             |  Function1 => scala/Function1#
             |local3 => final method isDefinedAt(x: List[_] forSome { type _ }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local8
             |  List => scala/package.List#
             |  _ => local1
             |  Boolean => scala/Boolean#
             |local4 => synthetic param x: A1
             |  A1 => local7
             |local5 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local7
             |  B1 => local6
             |local6 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local7 => typeparam A1 <: List[_] forSome { type _ }
             |  List => scala/package.List#
             |  _ => local1
             |local8 => param x: List[_] forSome { type _ }
             |  List => scala/package.List#
             |  _ => local1
             |local9 => param x: L[_] forSome { type _ }
             |  L => _empty_/t242.L#
             |  _ => local10
             |local10 => synthetic abstract type _
             |local11 => synthetic abstract type _
             |local12 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[_] forSome { type _ <: Int }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local13 => synthetic abstract type _ <: Int
             |  Int => scala/Int#
             |local14 => final method applyOrElse[A1 <: List[_] forSome { type _ <: Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local19
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |  B1 => local18
             |  x => local16
             |  default => local17
             |  Function1 => scala/Function1#
             |local15 => final method isDefinedAt(x: List[_] forSome { type _ <: Int }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local20
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |  Boolean => scala/Boolean#
             |local16 => synthetic param x: A1
             |  A1 => local19
             |local17 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local19
             |  B1 => local18
             |local18 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local19 => typeparam A1 <: List[_] forSome { type _ <: Int }
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |local20 => param x: List[_] forSome { type _ <: Int }
             |  List => scala/package.List#
             |  _ => local13
             |  Int => scala/Int#
             |local21 => param x: Seq[_] forSome { type _ <: AnyVal }
             |  Seq => scala/package.Seq#
             |  _ => local22
             |  AnyVal => scala/AnyVal#
             |local22 => synthetic abstract type _ <: AnyVal
             |  AnyVal => scala/AnyVal#
             |local23 => synthetic abstract type _ <: Int
             |  Int => scala/Int#
             |local24 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[List[T] forSome { type T }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  List => scala/package.List#
             |  T => local25
             |  Int => scala/Int#
             |  Serializable => java/io/Serializable#
             |local25 => abstract type T
             |local26 => final method applyOrElse[A1 <: List[T] forSome { type T }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local31
             |  List => scala/package.List#
             |  T => local25
             |  B1 => local30
             |  Int => scala/Int#
             |  x => local28
             |  default => local29
             |  Function1 => scala/Function1#
             |local27 => final method isDefinedAt(x: List[T] forSome { type T }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local32
             |  List => scala/package.List#
             |  T => local25
             |  Boolean => scala/Boolean#
             |local28 => synthetic param x: A1
             |  A1 => local31
             |local29 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local31
             |  B1 => local30
             |local30 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local31 => typeparam A1 <: List[T] forSome { type T }
             |  List => scala/package.List#
             |  T => local25
             |local32 => param x: List[T] forSome { type T }
             |  List => scala/package.List#
             |  T => local25
             |local33 => param x: Seq[T] forSome { type T }
             |  Seq => scala/package.Seq#
             |  T => local34
             |local34 => abstract type T
             |local35 => abstract type T
             |local36 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |  Serializable => java/io/Serializable#
             |local37 => abstract type T
             |local38 => abstract method f(x: Int): Int
             |  x => local39
             |  Int => scala/Int#
             |local39 => param x: Int
             |  Int => scala/Int#
             |local40 => abstract val method v: Int
             |  Int => scala/Int#
             |local41 => abstract method k: Int
             |  Int => scala/Int#
             |local42 => final method applyOrElse[A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local47
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |  B1 => local46
             |  x => local44
             |  default => local45
             |  Function1 => scala/Function1#
             |local43 => final method isDefinedAt(x: AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local48
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |  Boolean => scala/Boolean#
             |local44 => synthetic param x: A1
             |  A1 => local47
             |local45 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local47
             |  B1 => local46
             |local46 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local47 => typeparam A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |local48 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local37
             |  f => local38
             |  x => local39
             |  Int => scala/Int#
             |  v => local40
             |  k => local41
             |local49 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int }
             |  AnyRef => scala/AnyRef#
             |  T => local50
             |  f => local51
             |  x => local52
             |  Int => scala/Int#
             |  v => local53
             |local50 => abstract type T
             |local51 => abstract method f(x: Int): Int
             |  x => local52
             |  Int => scala/Int#
             |local52 => param x: Int
             |  Int => scala/Int#
             |local53 => abstract val method v: Int
             |  Int => scala/Int#
             |local54 => abstract type T
             |local55 => abstract method f(x: Int): Int
             |  x => local56
             |  Int => scala/Int#
             |local56 => param x: Int
             |  Int => scala/Int#
             |local57 => abstract val method v: Int
             |  Int => scala/Int#
             |local58 => abstract method k: Int
             |  Int => scala/Int#
             |local59 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }, Int] with Serializable { +2 decls }
             |  SerialVersionUID => scala/SerialVersionUID#
             |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |  Serializable => java/io/Serializable#
             |local60 => abstract method g[A](a: A)(implicit evidence$1: Ordering[A]): Int
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local61 => param a: A
             |  A => local62
             |local62 => typeparam A
             |local63 => synthetic implicit param evidence$1: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local62
             |local64 => abstract method k: Int
             |  Int => scala/Int#
             |local65 => final method applyOrElse[A1 <: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
             |  A1 => local70
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |  B1 => local69
             |  x => local67
             |  default => local68
             |  Function1 => scala/Function1#
             |local66 => final method isDefinedAt(x: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }): Boolean <: scala/PartialFunction#isDefinedAt().
             |  x => local71
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |  Boolean => scala/Boolean#
             |local67 => synthetic param x: A1
             |  A1 => local70
             |local68 => synthetic param default: Function1[A1, B1]
             |  Function1 => scala/Function1#
             |  A1 => local70
             |  B1 => local69
             |local69 => typeparam B1 >: Int
             |  Int => scala/Int#
             |local70 => typeparam A1 <: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |local71 => param x: AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }
             |  AnyRef => scala/AnyRef#
             |  g => local60
             |  A => local62
             |  a => local61
             |  evidence$1 => local63
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |  k => local64
             |local72 => param x: AnyRef { def g[A](a: A)(implicit evidence$4: Ordering[A]): Int }
             |  AnyRef => scala/AnyRef#
             |  g => local73
             |  A => local75
             |  a => local74
             |  evidence$4 => local76
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local73 => abstract method g[A](a: A)(implicit evidence$4: Ordering[A]): Int
             |  A => local75
             |  a => local74
             |  evidence$4 => local76
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local74 => param a: A
             |  A => local75
             |local75 => typeparam A
             |local76 => synthetic implicit param evidence$4: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local75
             |local77 => abstract method g[A](a: A)(implicit evidence$3: Ordering[A]): Int
             |  A => local80
             |  a => local78
             |  evidence$3 => local79
             |  Ordering => scala/package.Ordering#
             |  Int => scala/Int#
             |local78 => param a: A
             |  A => local80
             |local79 => synthetic implicit param evidence$3: Ordering[A]
             |  Ordering => scala/package.Ordering#
             |  A => local80
             |local80 => typeparam A
             |local81 => abstract method k: Int
             |  Int => scala/Int#
             |""".stripMargin,
      ),
      """|_empty_/t242. => final object t242 extends AnyRef { +11 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t242.L# => type L[A] = List[A]
         |  A => _empty_/t242.L#[A]
         |  List => scala/package.List#
         |_empty_/t242.L#[A] => typeparam A
         |_empty_/t242.a. => val method a: PartialFunction[List[_] forSome { type _ }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  _ => local9
         |  Int => scala/Int#
         |_empty_/t242.b. => val method b: Function1[List[_] forSome { type _ }, Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  _ => local12
         |  Int => scala/Int#
         |_empty_/t242.c. => val method c: PartialFunction[List[_] forSome { type _ <: Int }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  _ => local22
         |  Int => scala/Int#
         |_empty_/t242.d. => val method d: Function1[List[_] forSome { type _ <: Int }, Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  _ => local25
         |  Int => scala/Int#
         |_empty_/t242.e. => val method e: PartialFunction[List[T] forSome { type T }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  List => scala/package.List#
         |  T => local35
         |  Int => scala/Int#
         |_empty_/t242.f. => val method f: Function1[List[T] forSome { type T }, Int]
         |  Function1 => scala/Function1#
         |  List => scala/package.List#
         |  T => local38
         |  Int => scala/Int#
         |_empty_/t242.g. => val method g: PartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  AnyRef => scala/AnyRef#
         |  T => local51
         |  f => local52
         |  x => local53
         |  Int => scala/Int#
         |  v => local54
         |  k => local55
         |_empty_/t242.h. => val method h: Function1[AnyRef { type T; def f(x: Int): Int; val def v: Int; def k: Int }, Int]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  T => local61
         |  f => local62
         |  x => local63
         |  Int => scala/Int#
         |  v => local64
         |  k => local65
         |_empty_/t242.i. => val method i: PartialFunction[AnyRef { def g[A](a: A)(implicit evidence$1: Ordering[A]): Int; def k: Int }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  AnyRef => scala/AnyRef#
         |  g => local78
         |  A => local81
         |  a => local79
         |  evidence$1 => local80
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  k => local82
         |_empty_/t242.j. => val method j: Function1[AnyRef { def g[A](a: A)(implicit evidence$3: Ordering[A]): Int; def k: Int }, Int]
         |  Function1 => scala/Function1#
         |  AnyRef => scala/AnyRef#
         |  g => local88
         |  A => local91
         |  a => local89
         |  evidence$3 => local90
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  k => local92
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[L[_] forSome { type _ }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  L => _empty_/t242.L#
         |  _ => local1
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => abstract type _
         |local2 => final method applyOrElse[A1 <: L[_] forSome { type _ }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local7
         |  L => _empty_/t242.L#
         |  _ => local1
         |  B1 => local6
         |  Int => scala/Int#
         |  x => local4
         |  default => local5
         |  Function1 => scala/Function1#
         |local3 => final method isDefinedAt(x: L[_] forSome { type _ }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local8
         |  L => _empty_/t242.L#
         |  _ => local1
         |  Boolean => scala/Boolean#
         |local4 => synthetic param x: A1
         |  A1 => local7
         |local5 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local7
         |  B1 => local6
         |local6 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local7 => typeparam A1 <: L[_] forSome { type _ }
         |  L => _empty_/t242.L#
         |  _ => local1
         |local8 => param x: L[_] forSome { type _ }
         |  L => _empty_/t242.L#
         |  _ => local1
         |local9 => synthetic abstract type _
         |local10 => param x: L[_] forSome { type _ }
         |  L => _empty_/t242.L#
         |  _ => local11
         |local11 => synthetic abstract type _
         |local12 => synthetic abstract type _
         |local13 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Seq[_] forSome { type _ <: AnyVal }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Seq => scala/package.Seq#
         |  _ => local14
         |  AnyVal => scala/AnyVal#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local14 => abstract type _ <: AnyVal
         |  AnyVal => scala/AnyVal#
         |local15 => final method applyOrElse[A1 <: Seq[_] forSome { type _ <: AnyVal }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local20
         |  Seq => scala/package.Seq#
         |  _ => local14
         |  AnyVal => scala/AnyVal#
         |  B1 => local19
         |  Int => scala/Int#
         |  x => local17
         |  default => local18
         |  Function1 => scala/Function1#
         |local16 => final method isDefinedAt(x: Seq[_] forSome { type _ <: AnyVal }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local21
         |  Seq => scala/package.Seq#
         |  _ => local14
         |  AnyVal => scala/AnyVal#
         |  Boolean => scala/Boolean#
         |local17 => synthetic param x: A1
         |  A1 => local20
         |local18 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local20
         |  B1 => local19
         |local19 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local20 => typeparam A1 <: Seq[_] forSome { type _ <: AnyVal }
         |  Seq => scala/package.Seq#
         |  _ => local14
         |  AnyVal => scala/AnyVal#
         |local21 => param x: Seq[_] forSome { type _ <: AnyVal }
         |  Seq => scala/package.Seq#
         |  _ => local14
         |  AnyVal => scala/AnyVal#
         |local22 => synthetic abstract type _ <: Int
         |  Int => scala/Int#
         |local23 => param x: Seq[_] forSome { type _ <: AnyVal }
         |  Seq => scala/package.Seq#
         |  _ => local24
         |  AnyVal => scala/AnyVal#
         |local24 => synthetic abstract type _ <: AnyVal
         |  AnyVal => scala/AnyVal#
         |local25 => synthetic abstract type _ <: Int
         |  Int => scala/Int#
         |local26 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Seq[T] forSome { type T }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Seq => scala/package.Seq#
         |  T => local27
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local27 => abstract type T
         |local28 => final method applyOrElse[A1 <: Seq[T] forSome { type T }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local33
         |  Seq => scala/package.Seq#
         |  T => local27
         |  B1 => local32
         |  Int => scala/Int#
         |  x => local30
         |  default => local31
         |  Function1 => scala/Function1#
         |local29 => final method isDefinedAt(x: Seq[T] forSome { type T }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local34
         |  Seq => scala/package.Seq#
         |  T => local27
         |  Boolean => scala/Boolean#
         |local30 => synthetic param x: A1
         |  A1 => local33
         |local31 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local33
         |  B1 => local32
         |local32 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local33 => typeparam A1 <: Seq[T] forSome { type T }
         |  Seq => scala/package.Seq#
         |  T => local27
         |local34 => param x: Seq[T] forSome { type T }
         |  Seq => scala/package.Seq#
         |  T => local27
         |local35 => abstract type T
         |local36 => param x: Seq[T] forSome { type T }
         |  Seq => scala/package.Seq#
         |  T => local37
         |local37 => abstract type T
         |local38 => abstract type T
         |local39 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { type T; def f(x: Int): Int; val def v: Int }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  AnyRef => scala/AnyRef#
         |  T => local40
         |  f => local41
         |  x => local42
         |  Int => scala/Int#
         |  v => local43
         |  Serializable => java/io/Serializable#
         |local40 => abstract type T
         |local41 => abstract method f(x: Int): Int
         |  x => local42
         |  Int => scala/Int#
         |local42 => param x: Int
         |  Int => scala/Int#
         |local43 => abstract val method v: Int
         |  Int => scala/Int#
         |local44 => final method applyOrElse[A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local49
         |  AnyRef => scala/AnyRef#
         |  T => local40
         |  f => local41
         |  x => local42
         |  Int => scala/Int#
         |  v => local43
         |  B1 => local48
         |  x => local46
         |  default => local47
         |  Function1 => scala/Function1#
         |local45 => final method isDefinedAt(x: AnyRef { type T; def f(x: Int): Int; val def v: Int }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local50
         |  AnyRef => scala/AnyRef#
         |  T => local40
         |  f => local41
         |  x => local42
         |  Int => scala/Int#
         |  v => local43
         |  Boolean => scala/Boolean#
         |local46 => synthetic param x: A1
         |  A1 => local49
         |local47 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local49
         |  B1 => local48
         |local48 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local49 => typeparam A1 <: AnyRef { type T; def f(x: Int): Int; val def v: Int }
         |  AnyRef => scala/AnyRef#
         |  T => local40
         |  f => local41
         |  x => local42
         |  Int => scala/Int#
         |  v => local43
         |local50 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int }
         |  AnyRef => scala/AnyRef#
         |  T => local40
         |  f => local41
         |  x => local42
         |  Int => scala/Int#
         |  v => local43
         |local51 => abstract type T
         |local52 => abstract method f(x: Int): Int
         |  x => local53
         |  Int => scala/Int#
         |local53 => param x: Int
         |  Int => scala/Int#
         |local54 => abstract val method v: Int
         |  Int => scala/Int#
         |local55 => abstract method k: Int
         |  Int => scala/Int#
         |local56 => param x: AnyRef { type T; def f(x: Int): Int; val def v: Int }
         |  AnyRef => scala/AnyRef#
         |  T => local57
         |  f => local58
         |  x => local59
         |  Int => scala/Int#
         |  v => local60
         |local57 => abstract type T
         |local58 => abstract method f(x: Int): Int
         |  x => local59
         |  Int => scala/Int#
         |local59 => param x: Int
         |  Int => scala/Int#
         |local60 => abstract val method v: Int
         |  Int => scala/Int#
         |local61 => abstract type T
         |local62 => abstract method f(x: Int): Int
         |  x => local63
         |  Int => scala/Int#
         |local63 => param x: Int
         |  Int => scala/Int#
         |local64 => abstract val method v: Int
         |  Int => scala/Int#
         |local65 => abstract method k: Int
         |  Int => scala/Int#
         |local66 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[AnyRef { def g[A](a: A)(implicit evidence$2: Ordering[A]): Int }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  AnyRef => scala/AnyRef#
         |  g => local67
         |  A => local69
         |  a => local68
         |  evidence$2 => local70
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local67 => abstract method g[A](a: A)(implicit evidence$2: Ordering[A]): Int
         |  A => local69
         |  a => local68
         |  evidence$2 => local70
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local68 => param a: A
         |  A => local69
         |local69 => typeparam A
         |local70 => synthetic implicit param evidence$2: Ordering[A]
         |  Ordering => scala/package.Ordering#
         |  A => local69
         |local71 => final method applyOrElse[A1 <: AnyRef { def g[A](a: A)(implicit evidence$2: Ordering[A]): Int }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local76
         |  AnyRef => scala/AnyRef#
         |  g => local67
         |  A => local69
         |  a => local68
         |  evidence$2 => local70
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  B1 => local75
         |  x => local73
         |  default => local74
         |  Function1 => scala/Function1#
         |local72 => final method isDefinedAt(x: AnyRef { def g[A](a: A)(implicit evidence$2: Ordering[A]): Int }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local77
         |  AnyRef => scala/AnyRef#
         |  g => local67
         |  A => local69
         |  a => local68
         |  evidence$2 => local70
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local73 => synthetic param x: A1
         |  A1 => local76
         |local74 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local76
         |  B1 => local75
         |local75 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local76 => typeparam A1 <: AnyRef { def g[A](a: A)(implicit evidence$2: Ordering[A]): Int }
         |  AnyRef => scala/AnyRef#
         |  g => local67
         |  A => local69
         |  a => local68
         |  evidence$2 => local70
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local77 => param x: AnyRef { def g[A](a: A)(implicit evidence$2: Ordering[A]): Int }
         |  AnyRef => scala/AnyRef#
         |  g => local67
         |  A => local69
         |  a => local68
         |  evidence$2 => local70
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local78 => abstract method g[A](a: A)(implicit evidence$1: Ordering[A]): Int
         |  A => local81
         |  a => local79
         |  evidence$1 => local80
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local79 => param a: A
         |  A => local81
         |local80 => synthetic implicit param evidence$1: Ordering[A]
         |  Ordering => scala/package.Ordering#
         |  A => local81
         |local81 => typeparam A
         |local82 => abstract method k: Int
         |  Int => scala/Int#
         |local83 => param x: AnyRef { def g[A](a: A)(implicit evidence$4: Ordering[A]): Int }
         |  AnyRef => scala/AnyRef#
         |  g => local84
         |  A => local86
         |  a => local85
         |  evidence$4 => local87
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local84 => abstract method g[A](a: A)(implicit evidence$4: Ordering[A]): Int
         |  A => local86
         |  a => local85
         |  evidence$4 => local87
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local85 => param a: A
         |  A => local86
         |local86 => typeparam A
         |local87 => synthetic implicit param evidence$4: Ordering[A]
         |  Ordering => scala/package.Ordering#
         |  A => local86
         |local88 => abstract method g[A](a: A)(implicit evidence$3: Ordering[A]): Int
         |  A => local91
         |  a => local89
         |  evidence$3 => local90
         |  Ordering => scala/package.Ordering#
         |  Int => scala/Int#
         |local89 => param a: A
         |  A => local91
         |local90 => synthetic implicit param evidence$3: Ordering[A]
         |  Ordering => scala/package.Ordering#
         |  A => local91
         |local91 => typeparam A
         |local92 => abstract method k: Int
         |  Int => scala/Int#
         |""".stripMargin,
    ),
  )

  occurrences(
    "typed parameters of partial function lambdas with an annotation of overloaded constructors",
    """|object t243 {
       |  class ann(x: Int) extends scala.annotation.StaticAnnotation { def this() = this(0) }
       |  val a: PartialFunction[Int @ann, Int] = (x: Int @ann) => x match { case _ => 1 }
       |  val b: (Int @ann) => Int = (x: Int @ann) => x match { case _ => 1 }
       |  val c: PartialFunction[Int @ann(1), Int] = (x: Int @ann(1)) => x match { case _ => 1 }
       |  val d: (Int @ann(1)) => Int = (x: Int @ann(1)) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t243 <= _empty_/t243.
         |[1:8..1:11): ann <= _empty_/t243.ann#
         |[1:11..1:11):  <= _empty_/t243.ann#`<init>`().
         |[1:12..1:13): x <= _empty_/t243.ann#`<init>`().(x)
         |[1:12..1:13): x <= _empty_/t243.ann#x.
         |[1:15..1:18): Int => scala/Int#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[1:68..1:72): this <= _empty_/t243.ann#`<init>`(+1).
         |[1:81..1:81):  => _empty_/t243.ann#`<init>`().
         |[2:6..2:7): a <= _empty_/t243.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:28): Int => scala/Int#
         |[2:30..2:33): ann => _empty_/t243.ann#
         |[2:33..2:33):  => _empty_/t243.ann#`<init>`(+1).
         |[2:35..2:38): Int => scala/Int#
         |[2:42..2:82): (x: Int @ann) => x match { case _ => 1 } <= local0
         |[2:43..2:44): x <= local7
         |[2:46..2:49): Int => scala/Int#
         |[2:51..2:54): ann => _empty_/t243.ann#
         |[2:54..2:54):  => _empty_/t243.ann#`<init>`(+1).
         |[2:59..2:60): x => local7
         |[3:6..3:7): b <= _empty_/t243.b.
         |[3:10..3:13): Int => scala/Int#
         |[3:15..3:18): ann => _empty_/t243.ann#
         |[3:18..3:18):  => _empty_/t243.ann#`<init>`(+1).
         |[3:23..3:26): Int => scala/Int#
         |[3:30..3:31): x <= local8
         |[3:33..3:36): Int => scala/Int#
         |[3:38..3:41): ann => _empty_/t243.ann#
         |[3:41..3:41):  => _empty_/t243.ann#`<init>`(+1).
         |[3:46..3:47): x => local8
         |[4:6..4:7): c <= _empty_/t243.c.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:28): Int => scala/Int#
         |[4:30..4:33): ann => _empty_/t243.ann#
         |[4:33..4:33):  => _empty_/t243.ann#`<init>`().
         |[4:38..4:41): Int => scala/Int#
         |[4:45..4:88): (x: Int @ann(1)) => x match { case _ => 1 } <= local9
         |[4:46..4:47): x <= local16
         |[4:49..4:52): Int => scala/Int#
         |[4:54..4:57): ann => _empty_/t243.ann#
         |[4:57..4:57):  => _empty_/t243.ann#`<init>`().
         |[4:65..4:66): x => local16
         |[5:6..5:7): d <= _empty_/t243.d.
         |[5:10..5:13): Int => scala/Int#
         |[5:15..5:18): ann => _empty_/t243.ann#
         |[5:18..5:18):  => _empty_/t243.ann#`<init>`().
         |[5:26..5:29): Int => scala/Int#
         |[5:33..5:34): x <= local17
         |[5:36..5:39): Int => scala/Int#
         |[5:41..5:44): ann => _empty_/t243.ann#
         |[5:44..5:44):  => _empty_/t243.ann#`<init>`().
         |[5:52..5:53): x => local17
         |""".stripMargin
    else
      """|[0:7..0:11): t243 <= _empty_/t243.
         |[1:8..1:11): ann <= _empty_/t243.ann#
         |[1:11..1:11):  <= _empty_/t243.ann#`<init>`().
         |[1:12..1:13): x <= _empty_/t243.ann#`<init>`().(x)
         |[1:12..1:13): x <= _empty_/t243.ann#x.
         |[1:15..1:18): Int => scala/Int#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[1:68..1:72): this <= _empty_/t243.ann#`<init>`(+1).
         |[1:81..1:81):  => _empty_/t243.ann#`<init>`().
         |[2:6..2:7): a <= _empty_/t243.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:28): Int => scala/Int#
         |[2:30..2:33): ann => _empty_/t243.ann#
         |[2:33..2:33):  => _empty_/t243.ann#`<init>`(+1).
         |[2:35..2:38): Int => scala/Int#
         |[2:42..2:82): (x: Int @ann) => x match { case _ => 1 } <= local0
         |[2:43..2:44): x <= local7
         |[2:59..2:60): x => local7
         |[3:6..3:7): b <= _empty_/t243.b.
         |[3:10..3:13): Int => scala/Int#
         |[3:15..3:18): ann => _empty_/t243.ann#
         |[3:18..3:18):  => _empty_/t243.ann#`<init>`(+1).
         |[3:23..3:26): Int => scala/Int#
         |[3:30..3:31): x <= local8
         |[3:33..3:36): Int => scala/Int#
         |[3:38..3:41): ann => _empty_/t243.ann#
         |[3:41..3:41):  => _empty_/t243.ann#`<init>`(+1).
         |[3:46..3:47): x => local8
         |[4:6..4:7): c <= _empty_/t243.c.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:28): Int => scala/Int#
         |[4:30..4:33): ann => _empty_/t243.ann#
         |[4:33..4:33):  => _empty_/t243.ann#`<init>`().
         |[4:38..4:41): Int => scala/Int#
         |[4:45..4:88): (x: Int @ann(1)) => x match { case _ => 1 } <= local9
         |[4:46..4:47): x <= local16
         |[4:65..4:66): x => local16
         |[5:6..5:7): d <= _empty_/t243.d.
         |[5:10..5:13): Int => scala/Int#
         |[5:15..5:18): ann => _empty_/t243.ann#
         |[5:18..5:18):  => _empty_/t243.ann#`<init>`().
         |[5:26..5:29): Int => scala/Int#
         |[5:33..5:34): x <= local17
         |[5:36..5:39): Int => scala/Int#
         |[5:41..5:44): ann => _empty_/t243.ann#
         |[5:44..5:44):  => _empty_/t243.ann#`<init>`().
         |[5:52..5:53): x => local17
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas with names of the lambda's class and parameter",
    """|object t244 {
       |  object x { class T; class U extends T }
       |  object lift { class T; class U extends T }
       |  object isDefinedAt { class T; class U extends T }
       |  class A1
       |  class B1 extends A1
       |  val a: PartialFunction[x.U, Int] = (x: x.T) => x match { case _ => 1 }
       |  val b: PartialFunction[lift.U, Int] = (y: lift.T) => y match { case _ => 1 }
       |  val c: lift.U => Int = (y: lift.T) => y match { case _ => 1 }
       |  val d: PartialFunction[isDefinedAt.U, Int] = (y: isDefinedAt.T) => y match { case _ => 1 }
       |  val e: PartialFunction[B1, Int] = (y: A1) => y match { case _ => 1 }
       |  val f: B1 => Int = (y: A1) => y match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t244 <= _empty_/t244.
         |[1:9..1:10): x <= _empty_/t244.x.
         |[1:19..1:20): T <= _empty_/t244.x.T#
         |[1:20..1:20):  <= _empty_/t244.x.T#`<init>`().
         |[1:28..1:29): U <= _empty_/t244.x.U#
         |[1:30..1:30):  <= _empty_/t244.x.U#`<init>`().
         |[1:38..1:39): T => _empty_/t244.x.T#
         |[1:39..1:39):  => _empty_/t244.x.T#`<init>`().
         |[2:9..2:13): lift <= _empty_/t244.lift.
         |[2:22..2:23): T <= _empty_/t244.lift.T#
         |[2:23..2:23):  <= _empty_/t244.lift.T#`<init>`().
         |[2:31..2:32): U <= _empty_/t244.lift.U#
         |[2:33..2:33):  <= _empty_/t244.lift.U#`<init>`().
         |[2:41..2:42): T => _empty_/t244.lift.T#
         |[2:42..2:42):  => _empty_/t244.lift.T#`<init>`().
         |[3:9..3:20): isDefinedAt <= _empty_/t244.isDefinedAt.
         |[3:29..3:30): T <= _empty_/t244.isDefinedAt.T#
         |[3:30..3:30):  <= _empty_/t244.isDefinedAt.T#`<init>`().
         |[3:38..3:39): U <= _empty_/t244.isDefinedAt.U#
         |[3:40..3:40):  <= _empty_/t244.isDefinedAt.U#`<init>`().
         |[3:48..3:49): T => _empty_/t244.isDefinedAt.T#
         |[3:49..3:49):  => _empty_/t244.isDefinedAt.T#`<init>`().
         |[4:8..4:10): A1 <= _empty_/t244.A1#
         |[4:10..4:10):  <= _empty_/t244.A1#`<init>`().
         |[5:8..5:10): B1 <= _empty_/t244.B1#
         |[5:11..5:11):  <= _empty_/t244.B1#`<init>`().
         |[5:19..5:21): A1 => _empty_/t244.A1#
         |[5:21..5:21):  => _empty_/t244.A1#`<init>`().
         |[6:6..6:7): a <= _empty_/t244.a.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:26): x => _empty_/t244.x.
         |[6:27..6:28): U => _empty_/t244.x.U#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:72): (x: x.T) => x match { case _ => 1 } <= local0
         |[6:38..6:39): x <= local7
         |[6:41..6:42): x => _empty_/t244.x.
         |[6:43..6:44): T => _empty_/t244.x.T#
         |[6:49..6:50): x => local7
         |[7:6..7:7): b <= _empty_/t244.b.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:29): lift => _empty_/t244.lift.
         |[7:30..7:31): U => _empty_/t244.lift.U#
         |[7:33..7:36): Int => scala/Int#
         |[7:40..7:78): (y: lift.T) => y match { case _ => 1 } <= local8
         |[7:41..7:42): y <= local15
         |[7:44..7:48): lift => _empty_/t244.lift.
         |[7:49..7:50): T => _empty_/t244.lift.T#
         |[7:55..7:56): y => local15
         |[8:6..8:7): c <= _empty_/t244.c.
         |[8:9..8:13): lift => _empty_/t244.lift.
         |[8:14..8:15): U => _empty_/t244.lift.U#
         |[8:19..8:22): Int => scala/Int#
         |[8:26..8:27): y <= local16
         |[8:29..8:33): lift => _empty_/t244.lift.
         |[8:34..8:35): T => _empty_/t244.lift.T#
         |[8:40..8:41): y => local16
         |[9:6..9:7): d <= _empty_/t244.d.
         |[9:9..9:24): PartialFunction => scala/PartialFunction#
         |[9:25..9:36): isDefinedAt => _empty_/t244.isDefinedAt.
         |[9:37..9:38): U => _empty_/t244.isDefinedAt.U#
         |[9:40..9:43): Int => scala/Int#
         |[9:47..9:92): (y: isDefinedAt.T) => y match { case _ => 1 } <= local17
         |[9:48..9:49): y <= local24
         |[9:51..9:62): isDefinedAt => _empty_/t244.isDefinedAt.
         |[9:63..9:64): T => _empty_/t244.isDefinedAt.T#
         |[9:69..9:70): y => local24
         |[10:6..10:7): e <= _empty_/t244.e.
         |[10:9..10:24): PartialFunction => scala/PartialFunction#
         |[10:25..10:27): B1 => _empty_/t244.B1#
         |[10:29..10:32): Int => scala/Int#
         |[10:36..10:70): (y: A1) => y match { case _ => 1 } <= local25
         |[10:37..10:38): y <= local32
         |[10:40..10:42): A1 => _empty_/t244.A1#
         |[10:47..10:48): y => local32
         |[11:6..11:7): f <= _empty_/t244.f.
         |[11:9..11:11): B1 => _empty_/t244.B1#
         |[11:15..11:18): Int => scala/Int#
         |[11:22..11:23): y <= local33
         |[11:25..11:27): A1 => _empty_/t244.A1#
         |[11:32..11:33): y => local33
         |""".stripMargin
    else
      """|[0:7..0:11): t244 <= _empty_/t244.
         |[1:9..1:10): x <= _empty_/t244.x.
         |[1:19..1:20): T <= _empty_/t244.x.T#
         |[1:20..1:20):  <= _empty_/t244.x.T#`<init>`().
         |[1:28..1:29): U <= _empty_/t244.x.U#
         |[1:30..1:30):  <= _empty_/t244.x.U#`<init>`().
         |[1:38..1:39): T => _empty_/t244.x.T#
         |[1:39..1:39):  => _empty_/t244.x.T#`<init>`().
         |[2:9..2:13): lift <= _empty_/t244.lift.
         |[2:22..2:23): T <= _empty_/t244.lift.T#
         |[2:23..2:23):  <= _empty_/t244.lift.T#`<init>`().
         |[2:31..2:32): U <= _empty_/t244.lift.U#
         |[2:33..2:33):  <= _empty_/t244.lift.U#`<init>`().
         |[2:41..2:42): T => _empty_/t244.lift.T#
         |[2:42..2:42):  => _empty_/t244.lift.T#`<init>`().
         |[3:9..3:20): isDefinedAt <= _empty_/t244.isDefinedAt.
         |[3:29..3:30): T <= _empty_/t244.isDefinedAt.T#
         |[3:30..3:30):  <= _empty_/t244.isDefinedAt.T#`<init>`().
         |[3:38..3:39): U <= _empty_/t244.isDefinedAt.U#
         |[3:40..3:40):  <= _empty_/t244.isDefinedAt.U#`<init>`().
         |[3:48..3:49): T => _empty_/t244.isDefinedAt.T#
         |[3:49..3:49):  => _empty_/t244.isDefinedAt.T#`<init>`().
         |[4:8..4:10): A1 <= _empty_/t244.A1#
         |[4:10..4:10):  <= _empty_/t244.A1#`<init>`().
         |[5:8..5:10): B1 <= _empty_/t244.B1#
         |[5:11..5:11):  <= _empty_/t244.B1#`<init>`().
         |[5:19..5:21): A1 => _empty_/t244.A1#
         |[5:21..5:21):  => _empty_/t244.A1#`<init>`().
         |[6:6..6:7): a <= _empty_/t244.a.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:26): x => _empty_/t244.x.
         |[6:27..6:28): U => _empty_/t244.x.U#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:72): (x: x.T) => x match { case _ => 1 } <= local0
         |[6:38..6:39): x <= local7
         |[6:49..6:50): x => local7
         |[7:6..7:7): b <= _empty_/t244.b.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:29): lift => _empty_/t244.lift.
         |[7:30..7:31): U => _empty_/t244.lift.U#
         |[7:33..7:36): Int => scala/Int#
         |[7:40..7:78): (y: lift.T) => y match { case _ => 1 } <= local8
         |[7:41..7:42): y <= local15
         |[7:55..7:56): y => local15
         |[8:6..8:7): c <= _empty_/t244.c.
         |[8:9..8:13): lift => _empty_/t244.lift.
         |[8:14..8:15): U => _empty_/t244.lift.U#
         |[8:19..8:22): Int => scala/Int#
         |[8:26..8:27): y <= local16
         |[8:29..8:33): lift => _empty_/t244.lift.
         |[8:34..8:35): T => _empty_/t244.lift.T#
         |[8:40..8:41): y => local16
         |[9:6..9:7): d <= _empty_/t244.d.
         |[9:9..9:24): PartialFunction => scala/PartialFunction#
         |[9:25..9:36): isDefinedAt => _empty_/t244.isDefinedAt.
         |[9:37..9:38): U => _empty_/t244.isDefinedAt.U#
         |[9:40..9:43): Int => scala/Int#
         |[9:47..9:92): (y: isDefinedAt.T) => y match { case _ => 1 } <= local17
         |[9:48..9:49): y <= local24
         |[9:69..9:70): y => local24
         |[10:6..10:7): e <= _empty_/t244.e.
         |[10:9..10:24): PartialFunction => scala/PartialFunction#
         |[10:25..10:27): B1 => _empty_/t244.B1#
         |[10:29..10:32): Int => scala/Int#
         |[10:36..10:70): (y: A1) => y match { case _ => 1 } <= local25
         |[10:37..10:38): y <= local32
         |[10:47..10:48): y => local32
         |[11:6..11:7): f <= _empty_/t244.f.
         |[11:9..11:11): B1 => _empty_/t244.B1#
         |[11:15..11:18): Int => scala/Int#
         |[11:22..11:23): y <= local33
         |[11:25..11:27): A1 => _empty_/t244.A1#
         |[11:32..11:33): y => local33
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas with names that the written type binds",
    """|object t245 {
       |  class T
       |  class A
       |  class B
       |  object v { class W }
       |  val a: PartialFunction[List[Int], Int] = (x: List[T] forSome { type T }) => x match { case _ => 1 }
       |  val b: List[Int] => Int = (x: List[T] forSome { type T }) => x match { case _ => 1 }
       |  val c: PartialFunction[AnyRef { type T; def f: T; def g: Int }, Int] =
       |    (x: AnyRef { type T; def f: T }) => x match { case _ => 1 }
       |  val d: PartialFunction[AnyRef { def g[A](a: A): A; def k: Int }, Int] =
       |    (x: AnyRef { def g[A](a: A): A }) => x match { case _ => 1 }
       |  val e: PartialFunction[AnyRef { type B; def f: this.B; def k: Int }, Int] =
       |    (x: AnyRef { type B; def f: this.B }) => x match { case _ => 1 }
       |  val f: PartialFunction[(v.type, Int) forSome { val v: AnyRef }, Int] =
       |    (x: (v.type, Int) forSome { val v: AnyRef }) => x match { case _ => 1 }
       |  val g: PartialFunction[AnyRef { def h(v: AnyRef): v.type; def k: Int }, Int] =
       |    (x: AnyRef { def h(v: AnyRef): v.type }) => x match { case _ => 1 }
       |  val h: PartialFunction[List[U[Int]] forSome { type U[A] <: List[A] }, Int] =
       |    (x: List[U[Int]] forSome { type U[A] <: List[A] }) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t245 <= _empty_/t245.
         |[1:8..1:9): T <= _empty_/t245.T#
         |[1:9..1:9):  <= _empty_/t245.T#`<init>`().
         |[2:8..2:9): A <= _empty_/t245.A#
         |[2:9..2:9):  <= _empty_/t245.A#`<init>`().
         |[3:8..3:9): B <= _empty_/t245.B#
         |[3:9..3:9):  <= _empty_/t245.B#`<init>`().
         |[4:9..4:10): v <= _empty_/t245.v.
         |[4:19..4:20): W <= _empty_/t245.v.W#
         |[4:20..4:20):  <= _empty_/t245.v.W#`<init>`().
         |[5:6..5:7): a <= _empty_/t245.a.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:29): List => scala/package.List#
         |[5:30..5:33): Int => scala/Int#
         |[5:36..5:39): Int => scala/Int#
         |[5:43..5:101): (x: List[T] forSome { type T }) => x match { case _ => 1 } <= local0
         |[5:44..5:45): x <= local8
         |[5:47..5:51): List => scala/package.List#
         |[5:52..5:53): T => local1
         |[5:70..5:71): T <= local1
         |[5:78..5:79): x => local8
         |[6:6..6:7): b <= _empty_/t245.b.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:17): Int => scala/Int#
         |[6:22..6:25): Int => scala/Int#
         |[6:29..6:30): x <= local9
         |[6:32..6:36): List => scala/package.List#
         |[6:37..6:38): T => local10
         |[6:55..6:56): T <= local10
         |[6:63..6:64): x => local9
         |[7:6..7:7): c <= _empty_/t245.c.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:31): AnyRef => scala/AnyRef#
         |[7:39..7:40): T <= local21
         |[7:46..7:47): f <= local22
         |[7:49..7:50): T => local21
         |[7:56..7:57): g <= local23
         |[7:59..7:62): Int => scala/Int#
         |[7:66..7:69): Int => scala/Int#
         |[8:4..8:63): (x: AnyRef { type T; def f: T }) => x match { case _ => 1 } <= local11
         |[8:5..8:6): x <= local20
         |[8:8..8:14): AnyRef => scala/AnyRef#
         |[8:22..8:23): T <= local12
         |[8:29..8:30): f <= local13
         |[8:32..8:33): T => local12
         |[8:40..8:41): x => local20
         |[9:6..9:7): d <= _empty_/t245.d.
         |[9:9..9:24): PartialFunction => scala/PartialFunction#
         |[9:25..9:31): AnyRef => scala/AnyRef#
         |[9:38..9:39): g <= local35
         |[9:40..9:41): A <= local37
         |[9:43..9:44): a <= local36
         |[9:46..9:47): A => local37
         |[9:50..9:51): A => local37
         |[9:57..9:58): k <= local38
         |[9:60..9:63): Int => scala/Int#
         |[9:67..9:70): Int => scala/Int#
         |[10:4..10:64): (x: AnyRef { def g[A](a: A): A }) => x match { case _ => 1 } <= local24
         |[10:5..10:6): x <= local34
         |[10:8..10:14): AnyRef => scala/AnyRef#
         |[10:21..10:22): g <= local25
         |[10:23..10:24): A <= local27
         |[10:26..10:27): a <= local26
         |[10:29..10:30): A => local27
         |[10:33..10:34): A => local27
         |[10:41..10:42): x => local34
         |[11:6..11:7): e <= _empty_/t245.e.
         |[11:9..11:24): PartialFunction => scala/PartialFunction#
         |[11:25..11:31): AnyRef => scala/AnyRef#
         |[11:39..11:40): B <= local49
         |[11:46..11:47): f <= local50
         |[11:54..11:55): B => local49
         |[11:61..11:62): k <= local51
         |[11:64..11:67): Int => scala/Int#
         |[11:71..11:74): Int => scala/Int#
         |[12:4..12:68): (x: AnyRef { type B; def f: this.B }) => x match { case _ => 1 } <= local39
         |[12:5..12:6): x <= local48
         |[12:8..12:14): AnyRef => scala/AnyRef#
         |[12:22..12:23): B <= local40
         |[12:29..12:30): f <= local41
         |[12:37..12:38): B => local40
         |[12:45..12:46): x => local48
         |[13:6..13:7): f <= _empty_/t245.f.
         |[13:9..13:24): PartialFunction => scala/PartialFunction#
         |[13:26..13:27): v => local62
         |[13:34..13:37): Int => scala/Int#
         |[13:53..13:54): v <= local62
         |[13:56..13:62): AnyRef => scala/AnyRef#
         |[13:66..13:69): Int => scala/Int#
         |[14:4..14:75): (x: (v.type, Int) forSome { val v: AnyRef }) => x match { case _ => 1 } <= local52
         |[14:5..14:6): x <= local60
         |[14:9..14:10): v => local61
         |[14:17..14:20): Int => scala/Int#
         |[14:36..14:37): v <= local61
         |[14:39..14:45): AnyRef => scala/AnyRef#
         |[14:52..14:53): x => local60
         |[15:6..15:7): g <= _empty_/t245.g.
         |[15:9..15:24): PartialFunction => scala/PartialFunction#
         |[15:25..15:31): AnyRef => scala/AnyRef#
         |[15:38..15:39): h <= local74
         |[15:40..15:41): v <= local75
         |[15:43..15:49): AnyRef => scala/AnyRef#
         |[15:52..15:53): v => local75
         |[15:64..15:65): k <= local76
         |[15:67..15:70): Int => scala/Int#
         |[15:74..15:77): Int => scala/Int#
         |[16:4..16:71): (x: AnyRef { def h(v: AnyRef): v.type }) => x match { case _ => 1 } <= local64
         |[16:5..16:6): x <= local73
         |[16:8..16:14): AnyRef => scala/AnyRef#
         |[16:21..16:22): h <= local65
         |[16:23..16:24): v <= local66
         |[16:26..16:32): AnyRef => scala/AnyRef#
         |[16:35..16:36): v => local66
         |[16:48..16:49): x => local73
         |[17:6..17:7): h <= _empty_/t245.h.
         |[17:9..17:24): PartialFunction => scala/PartialFunction#
         |[17:25..17:29): List => scala/package.List#
         |[17:30..17:31): U => local87
         |[17:32..17:35): Int => scala/Int#
         |[17:53..17:54): U <= local87
         |[17:55..17:56): A <= local88
         |[17:61..17:65): List => scala/package.List#
         |[17:66..17:67): A => local88
         |[17:72..17:75): Int => scala/Int#
         |[18:4..18:81): (x: List[U[Int]] forSome { type U[A] <: List[A] }) => x match { case _ => 1 } <= local77
         |[18:5..18:6): x <= local86
         |[18:8..18:12): List => scala/package.List#
         |[18:13..18:14): U => local78
         |[18:15..18:18): Int => scala/Int#
         |[18:36..18:37): U <= local78
         |[18:38..18:39): A <= local79
         |[18:44..18:48): List => scala/package.List#
         |[18:49..18:50): A => local79
         |[18:58..18:59): x => local86
         |""".stripMargin
    else
      """|[0:7..0:11): t245 <= _empty_/t245.
         |[1:8..1:9): T <= _empty_/t245.T#
         |[1:9..1:9):  <= _empty_/t245.T#`<init>`().
         |[2:8..2:9): A <= _empty_/t245.A#
         |[2:9..2:9):  <= _empty_/t245.A#`<init>`().
         |[3:8..3:9): B <= _empty_/t245.B#
         |[3:9..3:9):  <= _empty_/t245.B#`<init>`().
         |[4:9..4:10): v <= _empty_/t245.v.
         |[4:19..4:20): W <= _empty_/t245.v.W#
         |[4:20..4:20):  <= _empty_/t245.v.W#`<init>`().
         |[5:6..5:7): a <= _empty_/t245.a.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:29): List => scala/package.List#
         |[5:30..5:33): Int => scala/Int#
         |[5:36..5:39): Int => scala/Int#
         |[5:43..5:101): (x: List[T] forSome { type T }) => x match { case _ => 1 } <= local0
         |[5:44..5:45): x <= local7
         |[5:78..5:79): x => local7
         |[6:6..6:7): b <= _empty_/t245.b.
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:17): Int => scala/Int#
         |[6:22..6:25): Int => scala/Int#
         |[6:29..6:30): x <= local8
         |[6:32..6:36): List => scala/package.List#
         |[6:37..6:38): T => local9
         |[6:55..6:56): T <= local9
         |[6:63..6:64): x => local8
         |[7:6..7:7): c <= _empty_/t245.c.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:31): AnyRef => scala/AnyRef#
         |[7:39..7:40): T <= local11
         |[7:46..7:47): f <= local12
         |[7:49..7:50): T => local11
         |[7:56..7:57): g <= local13
         |[7:59..7:62): Int => scala/Int#
         |[7:66..7:69): Int => scala/Int#
         |[8:4..8:63): (x: AnyRef { type T; def f: T }) => x match { case _ => 1 } <= local10
         |[8:5..8:6): x <= local20
         |[8:40..8:41): x => local20
         |[9:6..9:7): d <= _empty_/t245.d.
         |[9:9..9:24): PartialFunction => scala/PartialFunction#
         |[9:25..9:31): AnyRef => scala/AnyRef#
         |[9:38..9:39): g <= local22
         |[9:40..9:41): A <= local24
         |[9:43..9:44): a <= local23
         |[9:46..9:47): A => local24
         |[9:50..9:51): A => local24
         |[9:57..9:58): k <= local25
         |[9:60..9:63): Int => scala/Int#
         |[9:67..9:70): Int => scala/Int#
         |[10:4..10:64): (x: AnyRef { def g[A](a: A): A }) => x match { case _ => 1 } <= local21
         |[10:5..10:6): x <= local32
         |[10:41..10:42): x => local32
         |[11:6..11:7): e <= _empty_/t245.e.
         |[11:9..11:24): PartialFunction => scala/PartialFunction#
         |[11:25..11:31): AnyRef => scala/AnyRef#
         |[11:39..11:40): B <= local34
         |[11:46..11:47): f <= local35
         |[11:54..11:55): B => local34
         |[11:61..11:62): k <= local36
         |[11:64..11:67): Int => scala/Int#
         |[11:71..11:74): Int => scala/Int#
         |[12:4..12:68): (x: AnyRef { type B; def f: this.B }) => x match { case _ => 1 } <= local33
         |[12:5..12:6): x <= local43
         |[12:45..12:46): x => local43
         |[13:6..13:7): f <= _empty_/t245.f.
         |[13:9..13:24): PartialFunction => scala/PartialFunction#
         |[13:26..13:27): v => local53
         |[13:34..13:37): Int => scala/Int#
         |[13:53..13:54): v <= local53
         |[13:56..13:62): AnyRef => scala/AnyRef#
         |[13:66..13:69): Int => scala/Int#
         |[14:4..14:75): (x: (v.type, Int) forSome { val v: AnyRef }) => x match { case _ => 1 } <= local44
         |[14:5..14:6): x <= local52
         |[14:52..14:53): x => local52
         |[15:6..15:7): g <= _empty_/t245.g.
         |[15:9..15:24): PartialFunction => scala/PartialFunction#
         |[15:25..15:31): AnyRef => scala/AnyRef#
         |[15:38..15:39): h <= local55
         |[15:40..15:41): v <= local56
         |[15:43..15:49): AnyRef => scala/AnyRef#
         |[15:52..15:53): v => local56
         |[15:64..15:65): k <= local57
         |[15:67..15:70): Int => scala/Int#
         |[15:74..15:77): Int => scala/Int#
         |[16:4..16:71): (x: AnyRef { def h(v: AnyRef): v.type }) => x match { case _ => 1 } <= local54
         |[16:5..16:6): x <= local64
         |[16:48..16:49): x => local64
         |[17:6..17:7): h <= _empty_/t245.h.
         |[17:9..17:24): PartialFunction => scala/PartialFunction#
         |[17:25..17:29): List => scala/package.List#
         |[17:30..17:31): U => local66
         |[17:32..17:35): Int => scala/Int#
         |[17:53..17:54): U <= local66
         |[17:55..17:56): A <= local67
         |[17:61..17:65): List => scala/package.List#
         |[17:66..17:67): A => local67
         |[17:72..17:75): Int => scala/Int#
         |[18:4..18:81): (x: List[U[Int]] forSome { type U[A] <: List[A] }) => x match { case _ => 1 } <= local65
         |[18:5..18:6): x <= local74
         |[18:58..18:59): x => local74
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas with annotations of other symbols",
    """|object t246 {
       |  class ann extends scala.annotation.StaticAnnotation
       |  object o { class ann extends scala.annotation.StaticAnnotation }
       |  val a: PartialFunction[Int @o.ann, Int] = (x: Int @ann) => x match { case _ => 1 }
       |  val b: (Int @o.ann) => Int = (x: Int @ann) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t246 <= _empty_/t246.
         |[1:8..1:11): ann <= _empty_/t246.ann#
         |[1:12..1:12):  <= _empty_/t246.ann#`<init>`().
         |[1:20..1:25): scala => scala/
         |[1:26..1:36): annotation => scala/annotation/
         |[1:37..1:53): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:53..1:53):  => scala/annotation/Annotation#`<init>`().
         |[2:9..2:10): o <= _empty_/t246.o.
         |[2:19..2:22): ann <= _empty_/t246.o.ann#
         |[2:23..2:23):  <= _empty_/t246.o.ann#`<init>`().
         |[2:31..2:36): scala => scala/
         |[2:37..2:47): annotation => scala/annotation/
         |[2:48..2:64): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[2:64..2:64):  => scala/annotation/Annotation#`<init>`().
         |[3:6..3:7): a <= _empty_/t246.a.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:28): Int => scala/Int#
         |[3:30..3:31): o => _empty_/t246.o.
         |[3:32..3:35): ann => _empty_/t246.o.ann#
         |[3:35..3:35):  => _empty_/t246.o.ann#`<init>`().
         |[3:37..3:40): Int => scala/Int#
         |[3:44..3:84): (x: Int @ann) => x match { case _ => 1 } <= local0
         |[3:45..3:46): x <= local7
         |[3:48..3:51): Int => scala/Int#
         |[3:53..3:56): ann => _empty_/t246.ann#
         |[3:56..3:56):  => _empty_/t246.ann#`<init>`().
         |[3:61..3:62): x => local7
         |[4:6..4:7): b <= _empty_/t246.b.
         |[4:10..4:13): Int => scala/Int#
         |[4:15..4:16): o => _empty_/t246.o.
         |[4:17..4:20): ann => _empty_/t246.o.ann#
         |[4:20..4:20):  => _empty_/t246.o.ann#`<init>`().
         |[4:25..4:28): Int => scala/Int#
         |[4:32..4:33): x <= local8
         |[4:35..4:38): Int => scala/Int#
         |[4:40..4:43): ann => _empty_/t246.ann#
         |[4:43..4:43):  => _empty_/t246.ann#`<init>`().
         |[4:48..4:49): x => local8
         |""".stripMargin
    else
      """|[0:7..0:11): t246 <= _empty_/t246.
         |[1:8..1:11): ann <= _empty_/t246.ann#
         |[1:12..1:12):  <= _empty_/t246.ann#`<init>`().
         |[1:20..1:25): scala => scala/
         |[1:26..1:36): annotation => scala/annotation/
         |[1:37..1:53): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:53..1:53):  => scala/annotation/Annotation#`<init>`().
         |[2:9..2:10): o <= _empty_/t246.o.
         |[2:19..2:22): ann <= _empty_/t246.o.ann#
         |[2:23..2:23):  <= _empty_/t246.o.ann#`<init>`().
         |[2:31..2:36): scala => scala/
         |[2:37..2:47): annotation => scala/annotation/
         |[2:48..2:64): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[2:64..2:64):  => scala/annotation/Annotation#`<init>`().
         |[3:6..3:7): a <= _empty_/t246.a.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:28): Int => scala/Int#
         |[3:30..3:31): o => _empty_/t246.o.
         |[3:32..3:35): ann => _empty_/t246.o.ann#
         |[3:35..3:35):  => _empty_/t246.o.ann#`<init>`().
         |[3:37..3:40): Int => scala/Int#
         |[3:44..3:84): (x: Int @ann) => x match { case _ => 1 } <= local0
         |[3:45..3:46): x <= local7
         |[3:61..3:62): x => local7
         |[4:6..4:7): b <= _empty_/t246.b.
         |[4:10..4:13): Int => scala/Int#
         |[4:15..4:16): o => _empty_/t246.o.
         |[4:17..4:20): ann => _empty_/t246.o.ann#
         |[4:20..4:20):  => _empty_/t246.o.ann#`<init>`().
         |[4:25..4:28): Int => scala/Int#
         |[4:32..4:33): x <= local8
         |[4:35..4:38): Int => scala/Int#
         |[4:40..4:43): ann => _empty_/t246.ann#
         |[4:43..4:43):  => _empty_/t246.ann#`<init>`().
         |[4:48..4:49): x => local8
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas in constructor calls",
    """|object t247 {
       |  class T
       |  class B(f: PartialFunction[T, Int])
       |  class C(f: T => Int)
       |  class D extends B((x: T) => x match { case _ => 1 }) { class T }
       |  class E extends C((x: T) => x match { case _ => 1 }) { class T }
       |  class F(f: PartialFunction[t247.T, Int]) {
       |    class T
       |    def this() = this(((x: T) => x match { case _ => 1 }): PartialFunction[t247.T, Int])
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t247 <= _empty_/t247.
         |[1:8..1:9): T <= _empty_/t247.T#
         |[1:9..1:9):  <= _empty_/t247.T#`<init>`().
         |[2:8..2:9): B <= _empty_/t247.B#
         |[2:9..2:9):  <= _empty_/t247.B#`<init>`().
         |[2:10..2:11): f <= _empty_/t247.B#`<init>`().(f)
         |[2:10..2:11): f <= _empty_/t247.B#f.
         |[2:13..2:28): PartialFunction => scala/PartialFunction#
         |[2:29..2:30): T => _empty_/t247.T#
         |[2:32..2:35): Int => scala/Int#
         |[3:8..3:9): C <= _empty_/t247.C#
         |[3:9..3:9):  <= _empty_/t247.C#`<init>`().
         |[3:10..3:11): f <= _empty_/t247.C#`<init>`().(f)
         |[3:10..3:11): f <= _empty_/t247.C#f.
         |[3:13..3:14): T => _empty_/t247.T#
         |[3:18..3:21): Int => scala/Int#
         |[4:8..4:9): D <= _empty_/t247.D#
         |[4:10..4:10):  <= _empty_/t247.D#`<init>`().
         |[4:18..4:19): B => _empty_/t247.B#
         |[4:19..4:19):  => _empty_/t247.B#`<init>`().
         |[4:20..4:53): (x: T) => x match { case _ => 1 } <= local0
         |[4:21..4:22): x <= local7
         |[4:24..4:25): T => _empty_/t247.T#
         |[4:30..4:31): x => local7
         |[4:63..4:64): T <= _empty_/t247.D#T#
         |[4:64..4:64):  <= _empty_/t247.D#T#`<init>`().
         |[5:8..5:9): E <= _empty_/t247.E#
         |[5:10..5:10):  <= _empty_/t247.E#`<init>`().
         |[5:18..5:19): C => _empty_/t247.C#
         |[5:19..5:19):  => _empty_/t247.C#`<init>`().
         |[5:21..5:22): x <= local8
         |[5:24..5:25): T => _empty_/t247.T#
         |[5:30..5:31): x => local8
         |[5:63..5:64): T <= _empty_/t247.E#T#
         |[5:64..5:64):  <= _empty_/t247.E#T#`<init>`().
         |[6:8..6:9): F <= _empty_/t247.F#
         |[6:9..6:9):  <= _empty_/t247.F#`<init>`().
         |[6:10..6:11): f <= _empty_/t247.F#`<init>`().(f)
         |[6:10..6:11): f <= _empty_/t247.F#f.
         |[6:13..6:28): PartialFunction => scala/PartialFunction#
         |[6:29..6:33): t247 => _empty_/t247.
         |[6:34..6:35): T => _empty_/t247.T#
         |[6:37..6:40): Int => scala/Int#
         |[7:10..7:11): T <= _empty_/t247.F#T#
         |[7:11..7:11):  <= _empty_/t247.F#T#`<init>`().
         |[8:8..8:12): this <= _empty_/t247.F#`<init>`(+1).
         |[8:21..8:21):  => _empty_/t247.F#`<init>`().
         |[8:23..8:56): (x: T) => x match { case _ => 1 } <= local9
         |[8:24..8:25): x <= local16
         |[8:27..8:28): T => _empty_/t247.T#
         |[8:33..8:34): x => local16
         |[8:59..8:74): PartialFunction => scala/PartialFunction#
         |[8:75..8:79): t247 => _empty_/t247.
         |[8:80..8:81): T => _empty_/t247.T#
         |[8:83..8:86): Int => scala/Int#
         |""".stripMargin
    else
      """|[0:7..0:11): t247 <= _empty_/t247.
         |[1:8..1:9): T <= _empty_/t247.T#
         |[1:9..1:9):  <= _empty_/t247.T#`<init>`().
         |[2:8..2:9): B <= _empty_/t247.B#
         |[2:9..2:9):  <= _empty_/t247.B#`<init>`().
         |[2:10..2:11): f <= _empty_/t247.B#`<init>`().(f)
         |[2:10..2:11): f <= _empty_/t247.B#f.
         |[2:13..2:28): PartialFunction => scala/PartialFunction#
         |[2:29..2:30): T => _empty_/t247.T#
         |[2:32..2:35): Int => scala/Int#
         |[3:8..3:9): C <= _empty_/t247.C#
         |[3:9..3:9):  <= _empty_/t247.C#`<init>`().
         |[3:10..3:11): f <= _empty_/t247.C#`<init>`().(f)
         |[3:10..3:11): f <= _empty_/t247.C#f.
         |[3:13..3:14): T => _empty_/t247.T#
         |[3:18..3:21): Int => scala/Int#
         |[4:8..4:9): D <= _empty_/t247.D#
         |[4:10..4:10):  <= _empty_/t247.D#`<init>`().
         |[4:18..4:19): B => _empty_/t247.B#
         |[4:19..4:19):  => _empty_/t247.B#`<init>`().
         |[4:20..4:53): (x: T) => x match { case _ => 1 } <= local0
         |[4:21..4:22): x <= local7
         |[4:30..4:31): x => local7
         |[4:63..4:64): T <= _empty_/t247.D#T#
         |[4:64..4:64):  <= _empty_/t247.D#T#`<init>`().
         |[5:8..5:9): E <= _empty_/t247.E#
         |[5:10..5:10):  <= _empty_/t247.E#`<init>`().
         |[5:18..5:19): C => _empty_/t247.C#
         |[5:19..5:19):  => _empty_/t247.C#`<init>`().
         |[5:21..5:22): x <= local8
         |[5:24..5:25): T => _empty_/t247.T#
         |[5:30..5:31): x => local8
         |[5:63..5:64): T <= _empty_/t247.E#T#
         |[5:64..5:64):  <= _empty_/t247.E#T#`<init>`().
         |[6:8..6:9): F <= _empty_/t247.F#
         |[6:9..6:9):  <= _empty_/t247.F#`<init>`().
         |[6:10..6:11): f <= _empty_/t247.F#`<init>`().(f)
         |[6:10..6:11): f <= _empty_/t247.F#f.
         |[6:13..6:28): PartialFunction => scala/PartialFunction#
         |[6:29..6:33): t247 => _empty_/t247.
         |[6:34..6:35): T => _empty_/t247.T#
         |[6:37..6:40): Int => scala/Int#
         |[7:10..7:11): T <= _empty_/t247.F#T#
         |[7:11..7:11):  <= _empty_/t247.F#T#`<init>`().
         |[8:8..8:12): this <= _empty_/t247.F#`<init>`(+1).
         |[8:21..8:21):  => _empty_/t247.F#`<init>`().
         |[8:23..8:56): (x: T) => x match { case _ => 1 } <= local9
         |[8:24..8:25): x <= local16
         |[8:33..8:34): x => local16
         |[8:59..8:74): PartialFunction => scala/PartialFunction#
         |[8:75..8:79): t247 => _empty_/t247.
         |[8:80..8:81): T => _empty_/t247.T#
         |[8:83..8:86): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of nested partial function lambdas",
    """|object t248 {
       |  class A1
       |  val a: PartialFunction[Int, PartialFunction[A1, Int]] =
       |    (y: Int) => y match { case _ => (x: A1) => x match { case _ => 1 } }
       |  val b: Int => A1 => Int = (y: Int) => y match { case _ => (x: A1) => x match { case _ => 1 } }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t248 <= _empty_/t248.
         |[1:8..1:10): A1 <= _empty_/t248.A1#
         |[1:10..1:10):  <= _empty_/t248.A1#`<init>`().
         |[2:6..2:7): a <= _empty_/t248.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:28): Int => scala/Int#
         |[2:30..2:45): PartialFunction => scala/PartialFunction#
         |[2:46..2:48): A1 => _empty_/t248.A1#
         |[2:50..2:53): Int => scala/Int#
         |[3:4..3:72): (y: Int) => y match { case _ => (x: A1) => x match { case _ => 1 } } <= local0
         |[3:5..3:6): y <= local7
         |[3:8..3:11): Int => scala/Int#
         |[3:16..3:17): y => local7
         |[3:36..3:70): (x: A1) => x match { case _ => 1 } <= local8
         |[3:37..3:38): x <= local15
         |[3:40..3:42): A1 => _empty_/t248.A1#
         |[3:47..3:48): x => local15
         |[4:6..4:7): b <= _empty_/t248.b.
         |[4:9..4:12): Int => scala/Int#
         |[4:16..4:18): A1 => _empty_/t248.A1#
         |[4:22..4:25): Int => scala/Int#
         |[4:29..4:30): y <= local16
         |[4:32..4:35): Int => scala/Int#
         |[4:40..4:41): y => local16
         |[4:61..4:62): x <= local17
         |[4:64..4:66): A1 => _empty_/t248.A1#
         |[4:71..4:72): x => local17
         |""".stripMargin
    else
      """|[0:7..0:11): t248 <= _empty_/t248.
         |[1:8..1:10): A1 <= _empty_/t248.A1#
         |[1:10..1:10):  <= _empty_/t248.A1#`<init>`().
         |[2:6..2:7): a <= _empty_/t248.a.
         |[2:9..2:24): PartialFunction => scala/PartialFunction#
         |[2:25..2:28): Int => scala/Int#
         |[2:30..2:45): PartialFunction => scala/PartialFunction#
         |[2:46..2:48): A1 => _empty_/t248.A1#
         |[2:50..2:53): Int => scala/Int#
         |[3:4..3:72): (y: Int) => y match { case _ => (x: A1) => x match { case _ => 1 } } <= local0
         |[3:5..3:6): y <= local7
         |[3:16..3:17): y => local7
         |[3:36..3:70): (x: A1) => x match { case _ => 1 } <= local8
         |[3:37..3:38): x <= local15
         |[3:47..3:48): x => local15
         |[4:6..4:7): b <= _empty_/t248.b.
         |[4:9..4:12): Int => scala/Int#
         |[4:16..4:18): A1 => _empty_/t248.A1#
         |[4:22..4:25): Int => scala/Int#
         |[4:29..4:30): y <= local16
         |[4:32..4:35): Int => scala/Int#
         |[4:40..4:41): y => local16
         |[4:61..4:62): x <= local17
         |[4:64..4:66): A1 => _empty_/t248.A1#
         |[4:71..4:72): x => local17
         |""".stripMargin,
  )

  occurrences(
    "typed parameters of partial function lambdas with a refinement value of a type's name",
    """|object t249 {
       |  class Int
       |  object o { class Int extends t249.Int }
       |  val a: PartialFunction[AnyRef { val Int: AnyRef; def f: o.Int }, scala.Int] =
       |    (x: AnyRef { val Int: AnyRef; def f: Int }) => x match { case _ => 1 }
       |  val b: (AnyRef { val Int: AnyRef; def f: o.Int }) => scala.Int =
       |    (x: AnyRef { val Int: AnyRef; def f: Int }) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:11): t249 <= _empty_/t249.
         |[1:8..1:11): Int <= _empty_/t249.Int#
         |[1:11..1:11):  <= _empty_/t249.Int#`<init>`().
         |[2:9..2:10): o <= _empty_/t249.o.
         |[2:19..2:22): Int <= _empty_/t249.o.Int#
         |[2:23..2:23):  <= _empty_/t249.o.Int#`<init>`().
         |[2:31..2:35): t249 => _empty_/t249.
         |[2:36..2:39): Int => _empty_/t249.Int#
         |[2:39..2:39):  => _empty_/t249.Int#`<init>`().
         |[3:6..3:7): a <= _empty_/t249.a.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:31): AnyRef => scala/AnyRef#
         |[3:38..3:41): Int <= local10
         |[3:43..3:49): AnyRef => scala/AnyRef#
         |[3:55..3:56): f <= local11
         |[3:58..3:59): o => _empty_/t249.o.
         |[3:60..3:63): Int => _empty_/t249.o.Int#
         |[3:67..3:72): scala => scala/
         |[3:73..3:76): Int => scala/Int#
         |[4:4..4:74): (x: AnyRef { val Int: AnyRef; def f: Int }) => x match { case _ => 1 } <= local0
         |[4:5..4:6): x <= local9
         |[4:8..4:14): AnyRef => scala/AnyRef#
         |[4:21..4:24): Int <= local1
         |[4:26..4:32): AnyRef => scala/AnyRef#
         |[4:38..4:39): f <= local2
         |[4:41..4:44): Int => _empty_/t249.Int#
         |[4:51..4:52): x => local9
         |[5:6..5:7): b <= _empty_/t249.b.
         |[5:10..5:16): AnyRef => scala/AnyRef#
         |[5:23..5:26): Int <= local15
         |[5:28..5:34): AnyRef => scala/AnyRef#
         |[5:40..5:41): f <= local16
         |[5:43..5:44): o => _empty_/t249.o.
         |[5:45..5:48): Int => _empty_/t249.o.Int#
         |[5:55..5:60): scala => scala/
         |[5:61..5:64): Int => scala/Int#
         |[6:5..6:6): x <= local12
         |[6:8..6:14): AnyRef => scala/AnyRef#
         |[6:21..6:24): Int <= local13
         |[6:26..6:32): AnyRef => scala/AnyRef#
         |[6:38..6:39): f <= local14
         |[6:41..6:44): Int => _empty_/t249.Int#
         |[6:51..6:52): x => local12
         |""".stripMargin
    else
      """|[0:7..0:11): t249 <= _empty_/t249.
         |[1:8..1:11): Int <= _empty_/t249.Int#
         |[1:11..1:11):  <= _empty_/t249.Int#`<init>`().
         |[2:9..2:10): o <= _empty_/t249.o.
         |[2:19..2:22): Int <= _empty_/t249.o.Int#
         |[2:23..2:23):  <= _empty_/t249.o.Int#`<init>`().
         |[2:31..2:35): t249 => _empty_/t249.
         |[2:36..2:39): Int => _empty_/t249.Int#
         |[2:39..2:39):  => _empty_/t249.Int#`<init>`().
         |[3:6..3:7): a <= _empty_/t249.a.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:31): AnyRef => scala/AnyRef#
         |[3:38..3:41): Int <= local1
         |[3:43..3:49): AnyRef => scala/AnyRef#
         |[3:55..3:56): f <= local2
         |[3:58..3:59): o => _empty_/t249.o.
         |[3:60..3:63): Int => _empty_/t249.o.Int#
         |[3:67..3:72): scala => scala/
         |[3:73..3:76): Int => scala/Int#
         |[4:4..4:74): (x: AnyRef { val Int: AnyRef; def f: Int }) => x match { case _ => 1 } <= local0
         |[4:5..4:6): x <= local9
         |[4:51..4:52): x => local9
         |[5:6..5:7): b <= _empty_/t249.b.
         |[5:10..5:16): AnyRef => scala/AnyRef#
         |[5:23..5:26): Int <= local13
         |[5:28..5:34): AnyRef => scala/AnyRef#
         |[5:40..5:41): f <= local14
         |[5:43..5:44): o => _empty_/t249.o.
         |[5:45..5:48): Int => _empty_/t249.o.Int#
         |[5:55..5:60): scala => scala/
         |[5:61..5:64): Int => scala/Int#
         |[6:5..6:6): x <= local10
         |[6:8..6:14): AnyRef => scala/AnyRef#
         |[6:21..6:24): Int <= local11
         |[6:26..6:32): AnyRef => scala/AnyRef#
         |[6:38..6:39): f <= local12
         |[6:41..6:44): Int => _empty_/t249.Int#
         |[6:51..6:52): x => local10
         |""".stripMargin,
  )

  occurrences(
    "members of a package object in typed parameters of partial function lambdas",
    """|package object po16p { type T = Int; class C; val v = "" }
       |package po16a { package object po16b { type T = Int } }
       |object po16 {
       |  val a: PartialFunction[po16p.C, Int] = (x: po16p.C) => x match { case _ => 1 }
       |  val b: PartialFunction[Int, Int] = (x: po16a.po16b.T) => x match { case y => y }
       |  val c: PartialFunction[Int, Int] = (x: _root_.po16p.T) => x match { case y => y }
       |  val d: PartialFunction[Int, Int] = (x: po16p.`package`.T) => x match { case y => y }
       |  val e: PartialFunction[po16p.v.type, Int] = (x: po16p.v.type) => x match { case _ => 1 }
       |  val f: Int => Int = (x: po16a.po16b.T) => x match { case y => y }
       |  val g: po16p.v.type => Int = (x: po16p.v.type) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:15..0:20): po16p <= po16p/package.
         |[0:28..0:29): T <= po16p/package.T#
         |[0:32..0:35): Int => scala/Int#
         |[0:43..0:44): C <= po16p/package.C#
         |[0:44..0:44):  <= po16p/package.C#`<init>`().
         |[0:50..0:51): v <= po16p/package.v.
         |[1:8..1:13): po16a <= po16a/
         |[1:31..1:36): po16b <= po16a/po16b/package.
         |[1:44..1:45): T <= po16a/po16b/package.T#
         |[1:48..1:51): Int => scala/Int#
         |[2:7..2:11): po16 <= _empty_/po16.
         |[3:6..3:7): a <= _empty_/po16.a.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:30): po16p => po16p/
         |[3:31..3:32): C => po16p/package.C#
         |[3:34..3:37): Int => scala/Int#
         |[3:41..3:80): (x: po16p.C) => x match { case _ => 1 } <= local0
         |[3:42..3:43): x <= local7
         |[3:45..3:50): po16p => po16p/
         |[3:51..3:52): C => po16p/package.C#
         |[3:57..3:58): x => local7
         |[4:6..4:7): b <= _empty_/po16.b.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:28): Int => scala/Int#
         |[4:30..4:33): Int => scala/Int#
         |[4:37..4:82): (x: po16a.po16b.T) => x match { case y => y } <= local8
         |[4:38..4:39): x <= local15
         |[4:41..4:46): po16a => po16a/
         |[4:47..4:52): po16b => po16a/po16b/
         |[4:53..4:54): T => po16a/po16b/package.T#
         |[4:59..4:60): x => local15
         |[4:74..4:75): y <= local16
         |[4:79..4:80): y => local16
         |[5:6..5:7): c <= _empty_/po16.c.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[5:37..5:83): (x: _root_.po16p.T) => x match { case y => y } <= local17
         |[5:38..5:39): x <= local24
         |[5:41..5:47): _root_ => _root_/
         |[5:48..5:53): po16p => po16p/
         |[5:54..5:55): T => po16p/package.T#
         |[5:60..5:61): x => local24
         |[5:75..5:76): y <= local25
         |[5:80..5:81): y => local25
         |[6:6..6:7): d <= _empty_/po16.d.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:28): Int => scala/Int#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:86): (x: po16p.`package`.T) => x match { case y => y } <= local26
         |[6:38..6:39): x <= local33
         |[6:41..6:46): po16p => po16p/
         |[6:47..6:56): `package` => po16p/package.
         |[6:57..6:58): T => po16p/package.T#
         |[6:63..6:64): x => local33
         |[6:78..6:79): y <= local34
         |[6:83..6:84): y => local34
         |[7:6..7:7): e <= _empty_/po16.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:30): po16p => po16p/
         |[7:31..7:32): v => po16p/package.v.
         |[7:39..7:42): Int => scala/Int#
         |[7:46..7:90): (x: po16p.v.type) => x match { case _ => 1 } <= local35
         |[7:47..7:48): x <= local42
         |[7:50..7:55): po16p => po16p/
         |[7:56..7:57): v => po16p/package.v.
         |[7:67..7:68): x => local42
         |[8:6..8:7): f <= _empty_/po16.f.
         |[8:9..8:12): Int => scala/Int#
         |[8:16..8:19): Int => scala/Int#
         |[8:23..8:24): x <= local43
         |[8:26..8:31): po16a => po16a/
         |[8:32..8:37): po16b => po16a/po16b/
         |[8:38..8:39): T => po16a/po16b/package.T#
         |[8:44..8:45): x => local43
         |[8:59..8:60): y <= local44
         |[8:64..8:65): y => local44
         |[9:6..9:7): g <= _empty_/po16.g.
         |[9:9..9:14): po16p => po16p/
         |[9:15..9:16): v => po16p/package.v.
         |[9:25..9:28): Int => scala/Int#
         |[9:32..9:33): x <= local45
         |[9:35..9:40): po16p => po16p/
         |[9:41..9:42): v => po16p/package.v.
         |[9:52..9:53): x => local45
         |""".stripMargin
    else
      """|[0:15..0:20): po16p <= po16p/package.
         |[0:28..0:29): T <= po16p/package.T#
         |[0:32..0:35): Int => scala/Int#
         |[0:43..0:44): C <= po16p/package.C#
         |[0:44..0:44):  <= po16p/package.C#`<init>`().
         |[0:50..0:51): v <= po16p/package.v.
         |[1:8..1:13): po16a <= po16a/
         |[1:31..1:36): po16b <= po16a/po16b/package.
         |[1:44..1:45): T <= po16a/po16b/package.T#
         |[1:48..1:51): Int => scala/Int#
         |[2:7..2:11): po16 <= _empty_/po16.
         |[3:6..3:7): a <= _empty_/po16.a.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:25..3:30): po16p => po16p/
         |[3:31..3:32): C => po16p/package.C#
         |[3:34..3:37): Int => scala/Int#
         |[3:41..3:80): (x: po16p.C) => x match { case _ => 1 } <= local0
         |[3:42..3:43): x <= local7
         |[3:57..3:58): x => local7
         |[4:6..4:7): b <= _empty_/po16.b.
         |[4:9..4:24): PartialFunction => scala/PartialFunction#
         |[4:25..4:28): Int => scala/Int#
         |[4:30..4:33): Int => scala/Int#
         |[4:37..4:82): (x: po16a.po16b.T) => x match { case y => y } <= local8
         |[4:38..4:39): x <= local15
         |[4:59..4:60): x => local15
         |[4:74..4:75): y <= local16
         |[4:79..4:80): y => local16
         |[5:6..5:7): c <= _empty_/po16.c.
         |[5:9..5:24): PartialFunction => scala/PartialFunction#
         |[5:25..5:28): Int => scala/Int#
         |[5:30..5:33): Int => scala/Int#
         |[5:37..5:83): (x: _root_.po16p.T) => x match { case y => y } <= local17
         |[5:38..5:39): x <= local24
         |[5:60..5:61): x => local24
         |[5:75..5:76): y <= local25
         |[5:80..5:81): y => local25
         |[6:6..6:7): d <= _empty_/po16.d.
         |[6:9..6:24): PartialFunction => scala/PartialFunction#
         |[6:25..6:28): Int => scala/Int#
         |[6:30..6:33): Int => scala/Int#
         |[6:37..6:86): (x: po16p.`package`.T) => x match { case y => y } <= local26
         |[6:38..6:39): x <= local33
         |[6:63..6:64): x => local33
         |[6:78..6:79): y <= local34
         |[6:83..6:84): y => local34
         |[7:6..7:7): e <= _empty_/po16.e.
         |[7:9..7:24): PartialFunction => scala/PartialFunction#
         |[7:25..7:30): po16p => po16p/
         |[7:31..7:32): v => po16p/package.v.
         |[7:39..7:42): Int => scala/Int#
         |[7:46..7:90): (x: po16p.v.type) => x match { case _ => 1 } <= local35
         |[7:47..7:48): x <= local42
         |[7:67..7:68): x => local42
         |[8:6..8:7): f <= _empty_/po16.f.
         |[8:9..8:12): Int => scala/Int#
         |[8:16..8:19): Int => scala/Int#
         |[8:23..8:24): x <= local43
         |[8:26..8:31): po16a => po16a/
         |[8:32..8:37): po16b => po16a/po16b/
         |[8:38..8:39): T => po16a/po16b/package.T#
         |[8:44..8:45): x => local43
         |[8:59..8:60): y <= local44
         |[8:64..8:65): y => local44
         |[9:6..9:7): g <= _empty_/po16.g.
         |[9:9..9:14): po16p => po16p/
         |[9:15..9:16): v => po16p/package.v.
         |[9:25..9:28): Int => scala/Int#
         |[9:32..9:33): x <= local45
         |[9:35..9:40): po16p => po16p/
         |[9:41..9:42): v => po16p/package.v.
         |[9:52..9:53): x => local45
         |""".stripMargin,
  )

  occurrences(
    "values typed by types of the same forSome in typed parameters of partial function lambdas",
    """|object e65 {
       |  val a: PartialFunction[(List[T], v.type) forSome { val v: T; type T <: AnyRef }, Int] =
       |    (x: (List[T], v.type) forSome { val v: T; type T <: AnyRef }) => x match { case _ => 1 }
       |  val b: PartialFunction[(v.type, w.type) forSome { type T <: AnyRef; val w: v.type; val v: T }, Int] =
       |    (x: (v.type, w.type) forSome { type T <: AnyRef; val w: v.type; val v: T }) => x match { case _ => 1 }
       |  val c: ((List[T], v.type) forSome { val v: T; type T <: AnyRef }) => Int =
       |    (x: (List[T], v.type) forSome { val v: T; type T <: AnyRef }) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.atLeast213_17)
      """|[0:7..0:10): e65 <= _empty_/e65.
         |[1:6..1:7): a <= _empty_/e65.a.
         |[1:9..1:24): PartialFunction => scala/PartialFunction#
         |[1:26..1:30): List => scala/package.List#
         |[1:31..1:32): T => local12
         |[1:35..1:36): v => local11
         |[1:57..1:58): v <= local11
         |[1:60..1:61): T => local12
         |[1:68..1:69): T <= local12
         |[1:73..1:79): AnyRef => scala/AnyRef#
         |[1:83..1:86): Int => scala/Int#
         |[2:4..2:92): (x: (List[T], v.type) forSome { val v: T; type T <: AnyRef }) => x match { case _ => 1 } <= local0
         |[2:5..2:6): x <= local9
         |[2:9..2:13): List => scala/package.List#
         |[2:14..2:15): T => local1
         |[2:18..2:19): v => local10
         |[2:40..2:41): v <= local10
         |[2:43..2:44): T => local1
         |[2:51..2:52): T <= local1
         |[2:56..2:62): AnyRef => scala/AnyRef#
         |[2:69..2:70): x => local9
         |[3:6..3:7): b <= _empty_/e65.b.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:26..3:27): v => local29
         |[3:34..3:35): w => local28
         |[3:57..3:58): T <= local27
         |[3:62..3:68): AnyRef => scala/AnyRef#
         |[3:74..3:75): w <= local28
         |[3:77..3:78): v => local29
         |[3:89..3:90): v <= local29
         |[3:92..3:93): T => local27
         |[3:97..3:100): Int => scala/Int#
         |[4:4..4:106): (x: (v.type, w.type) forSome { type T <: AnyRef; val w: v.type; val v: T }) => x match { case _ => 1 } <= local14
         |[4:5..4:6): x <= local24
         |[4:9..4:10): v => local26
         |[4:17..4:18): w => local25
         |[4:40..4:41): T <= local17
         |[4:45..4:51): AnyRef => scala/AnyRef#
         |[4:57..4:58): w <= local25
         |[4:60..4:61): v => local26
         |[4:72..4:73): v <= local26
         |[4:75..4:76): T => local17
         |[4:83..4:84): x => local24
         |[5:6..5:7): c <= _empty_/e65.c.
         |[5:11..5:15): List => scala/package.List#
         |[5:16..5:17): T => local37
         |[5:20..5:21): v => local36
         |[5:42..5:43): v <= local36
         |[5:45..5:46): T => local37
         |[5:53..5:54): T <= local37
         |[5:58..5:64): AnyRef => scala/AnyRef#
         |[5:71..5:74): Int => scala/Int#
         |[6:5..6:6): x <= local32
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): T => local33
         |[6:18..6:19): v => local35
         |[6:40..6:41): v <= local35
         |[6:43..6:44): T => local33
         |[6:51..6:52): T <= local33
         |[6:56..6:62): AnyRef => scala/AnyRef#
         |[6:69..6:70): x => local32
         |""".stripMargin
    else
      """|[0:7..0:10): e65 <= _empty_/e65.
         |[1:6..1:7): a <= _empty_/e65.a.
         |[1:9..1:24): PartialFunction => scala/PartialFunction#
         |[1:26..1:30): List => scala/package.List#
         |[1:31..1:32): T => local1
         |[1:35..1:36): v => local10
         |[1:57..1:58): v <= local10
         |[1:60..1:61): T => local1
         |[1:68..1:69): T <= local1
         |[1:73..1:79): AnyRef => scala/AnyRef#
         |[1:83..1:86): Int => scala/Int#
         |[2:4..2:92): (x: (List[T], v.type) forSome { val v: T; type T <: AnyRef }) => x match { case _ => 1 } <= local0
         |[2:5..2:6): x <= local9
         |[2:69..2:70): x => local9
         |[3:6..3:7): b <= _empty_/e65.b.
         |[3:9..3:24): PartialFunction => scala/PartialFunction#
         |[3:26..3:27): v => local23
         |[3:34..3:35): w => local22
         |[3:57..3:58): T <= local14
         |[3:62..3:68): AnyRef => scala/AnyRef#
         |[3:74..3:75): w <= local22
         |[3:77..3:78): v => local23
         |[3:89..3:90): v <= local23
         |[3:92..3:93): T => local14
         |[3:97..3:100): Int => scala/Int#
         |[4:4..4:106): (x: (v.type, w.type) forSome { type T <: AnyRef; val w: v.type; val v: T }) => x match { case _ => 1 } <= local11
         |[4:5..4:6): x <= local21
         |[4:83..4:84): x => local21
         |[5:6..5:7): c <= _empty_/e65.c.
         |[5:11..5:15): List => scala/package.List#
         |[5:16..5:17): T => local29
         |[5:20..5:21): v => local28
         |[5:42..5:43): v <= local28
         |[5:45..5:46): T => local29
         |[5:53..5:54): T <= local29
         |[5:58..5:64): AnyRef => scala/AnyRef#
         |[5:71..5:74): Int => scala/Int#
         |[6:5..6:6): x <= local24
         |[6:9..6:13): List => scala/package.List#
         |[6:14..6:15): T => local25
         |[6:18..6:19): v => local27
         |[6:40..6:41): v <= local27
         |[6:43..6:44): T => local25
         |[6:51..6:52): T <= local25
         |[6:56..6:62): AnyRef => scala/AnyRef#
         |[6:69..6:70): x => local24
         |""".stripMargin,
  )

  symbols(
    "values typed by types of the same forSome in typed parameters of partial function lambdas",
    """|object e65 {
       |  val a: PartialFunction[(List[T], v.type) forSome { val v: T; type T <: AnyRef }, Int] =
       |    (x: (List[T], v.type) forSome { val v: T; type T <: AnyRef }) => x match { case _ => 1 }
       |  val b: PartialFunction[(v.type, w.type) forSome { type T <: AnyRef; val w: v.type; val v: T }, Int] =
       |    (x: (v.type, w.type) forSome { type T <: AnyRef; val w: v.type; val v: T }) => x match { case _ => 1 }
       |  val c: ((List[T], v.type) forSome { val v: T; type T <: AnyRef }) => Int =
       |    (x: (List[T], v.type) forSome { val v: T; type T <: AnyRef }) => x match { case _ => 1 }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/e65. => final object e65 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e65.a. => val method a: PartialFunction[Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |_empty_/e65.b. => val method b: PartialFunction[Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |_empty_/e65.c. => val method c: Function1[Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, Int]
         |  Function1 => scala/Function1#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local29
         |  v.type => local30
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local3 => final method applyOrElse[A1 <: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local8
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  B1 => local7
         |  Int => scala/Int#
         |  x => local5
         |  default => local6
         |  Function1 => scala/Function1#
         |local4 => final method isDefinedAt(x: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local9
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Boolean => scala/Boolean#
         |local5 => synthetic param x: A1
         |  A1 => local8
         |local6 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local8
         |  B1 => local7
         |local7 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local8 => typeparam A1 <: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local9 => param x: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local10 => val local v: T
         |  T => local1
         |local11 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local14 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local15 => final method applyOrElse[A1 <: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local20
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  B1 => local19
         |  Int => scala/Int#
         |  x => local17
         |  default => local18
         |  Function1 => scala/Function1#
         |local16 => final method isDefinedAt(x: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local21
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Boolean => scala/Boolean#
         |local17 => synthetic param x: A1
         |  A1 => local20
         |local18 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local20
         |  B1 => local19
         |local19 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local20 => typeparam A1 <: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local21 => param x: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local22 => val local w: v.type
         |  v => local23
         |local23 => val local v: T
         |  T => local14
         |local24 => param x: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local25
         |  v.type => local26
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local25 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local27 => val local v: T
         |  T => local25
         |local28 => val local v: T
         |  T => local29
         |local29 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin
    else if (ScalaVersion.atLeast213_17)
      """|_empty_/e65. => final object e65 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e65.a. => val method a: PartialFunction[Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local12
         |  v.type => local13
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |_empty_/e65.b. => val method b: PartialFunction[Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  v.type => local30
         |  w.type => local31
         |  T => local27
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |_empty_/e65.c. => val method c: Function1[Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, Int]
         |  Function1 => scala/Function1#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local37
         |  v.type => local38
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local3 => final method applyOrElse[A1 <: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local8
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  B1 => local7
         |  Int => scala/Int#
         |  x => local5
         |  default => local6
         |  Function1 => scala/Function1#
         |local4 => final method isDefinedAt(x: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local9
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Boolean => scala/Boolean#
         |local5 => synthetic param x: A1
         |  A1 => local8
         |local6 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local8
         |  B1 => local7
         |local7 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local8 => typeparam A1 <: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local9 => param x: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local10 => val local v: T
         |  T => local1
         |local11 => val local v: T
         |  T => local12
         |local12 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local14 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  v.type => local15
         |  w.type => local16
         |  T => local17
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local17 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local18 => final method applyOrElse[A1 <: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local23
         |  Tuple2 => scala/Tuple2#
         |  v.type => local15
         |  w.type => local16
         |  T => local17
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  B1 => local22
         |  Int => scala/Int#
         |  x => local20
         |  default => local21
         |  Function1 => scala/Function1#
         |local19 => final method isDefinedAt(x: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local24
         |  Tuple2 => scala/Tuple2#
         |  v.type => local15
         |  w.type => local16
         |  T => local17
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Boolean => scala/Boolean#
         |local20 => synthetic param x: A1
         |  A1 => local23
         |local21 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local23
         |  B1 => local22
         |local22 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local23 => typeparam A1 <: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }
         |  Tuple2 => scala/Tuple2#
         |  v.type => local15
         |  w.type => local16
         |  T => local17
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local24 => param x: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }
         |  Tuple2 => scala/Tuple2#
         |  v.type => local15
         |  w.type => local16
         |  T => local17
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local25 => val local w: v.type
         |  v => local26
         |local26 => val local v: T
         |  T => local17
         |local27 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local28 => val local w: v.type
         |  v => local29
         |local29 => val local v: T
         |  T => local27
         |local32 => param x: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local33
         |  v.type => local34
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local33 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local35 => val local v: T
         |  T => local33
         |local36 => val local v: T
         |  T => local37
         |local37 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin
    else
      """|_empty_/e65. => final object e65 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e65.a. => val method a: PartialFunction[Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |_empty_/e65.b. => val method b: PartialFunction[Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |_empty_/e65.c. => val method c: Function1[Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, Int]
         |  Function1 => scala/Function1#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local29
         |  v.type => local30
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local3 => final method applyOrElse[A1 <: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local8
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  B1 => local7
         |  Int => scala/Int#
         |  x => local5
         |  default => local6
         |  Function1 => scala/Function1#
         |local4 => final method isDefinedAt(x: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local9
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Boolean => scala/Boolean#
         |local5 => synthetic param x: A1
         |  A1 => local8
         |local6 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local8
         |  B1 => local7
         |local7 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local8 => typeparam A1 <: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local9 => param x: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local1
         |  v.type => local2
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local10 => val local v: T
         |  T => local1
         |local11 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local14 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local15 => final method applyOrElse[A1 <: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local20
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  B1 => local19
         |  Int => scala/Int#
         |  x => local17
         |  default => local18
         |  Function1 => scala/Function1#
         |local16 => final method isDefinedAt(x: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x => local21
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |  Boolean => scala/Boolean#
         |local17 => synthetic param x: A1
         |  A1 => local20
         |local18 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local20
         |  B1 => local19
         |local19 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local20 => typeparam A1 <: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local21 => param x: Tuple2[v.type, w.type] forSome { type T <: AnyRef; type w.type <: AnyRef with Singleton {}; type v.type <: AnyRef with Singleton {} }
         |  Tuple2 => scala/Tuple2#
         |  v.type => local12
         |  w.type => local13
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local22 => val local w: v.type
         |  v => local23
         |local23 => val local v: T
         |  T => local14
         |local24 => param x: Tuple2[List[T], v.type] forSome { type v.type <: AnyRef with Singleton {}; type T <: AnyRef }
         |  Tuple2 => scala/Tuple2#
         |  List => scala/package.List#
         |  T => local25
         |  v.type => local26
         |  AnyRef => scala/AnyRef#
         |  Singleton => scala/Singleton#
         |local25 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |local27 => val local v: T
         |  T => local25
         |local28 => val local v: T
         |  T => local29
         |local29 => abstract type T <: AnyRef
         |  AnyRef => scala/AnyRef#
         |""".stripMargin,
  )

}

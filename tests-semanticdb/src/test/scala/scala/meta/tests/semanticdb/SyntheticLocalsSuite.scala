package scala.meta.tests.semanticdb

class SyntheticLocalsSuite extends SemanticdbSuite {

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  private def synthetics(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: synthetics", code, expected, "Synthetics")

  symbols(
    "var in a local refinement",
    """|abstract class s1 {
       |  def m = {
       |    type S = { var vs: Int }
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|_empty_/s1# => abstract class s1 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s1#`<init>`(). => primary ctor <init>()
       |_empty_/s1#m(). => method m: Unit
       |  Unit => scala/Unit#
       |local0 => type S = AnyRef { var def vs: Int; var def vs_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vs => local1
       |  Int => scala/Int#
       |  vs_= => local2
       |  x$1 => local3
       |  Unit => scala/Unit#
       |local1 => abstract var method vs: Int
       |  Int => scala/Int#
       |local2 => abstract var method vs_=(x$1: Int): Unit
       |  x$1 => local3
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local3 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "vars in member refinements",
    """|class s2 {
       |  type S = { var vs: Int }
       |  val n: { var vn: Int } = ???
       |  def rs(r: { var vr: Int }) = r.vr
       |}
       |""".stripMargin,
    """|_empty_/s2# => class s2 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s2#S# => type S = AnyRef { var def vs: Int; var def vs_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vs => local0
       |  Int => scala/Int#
       |  vs_= => local1
       |  x$1 => local2
       |  Unit => scala/Unit#
       |_empty_/s2#`<init>`(). => primary ctor <init>()
       |_empty_/s2#n. => val method n: AnyRef { var def vn: Int; var def vn_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vn => local3
       |  Int => scala/Int#
       |  vn_= => local4
       |  x$1 => local5
       |  Unit => scala/Unit#
       |_empty_/s2#rs(). => method rs(r: AnyRef { var def vr: Int; var def vr_=(x$1: Int): Unit }): Int
       |  r => _empty_/s2#rs().(r)
       |  AnyRef => scala/AnyRef#
       |  vr => local6
       |  Int => scala/Int#
       |  vr_= => local7
       |  x$1 => local8
       |  Unit => scala/Unit#
       |_empty_/s2#rs().(r) => param r: AnyRef { var def vr: Int; var def vr_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vr => local6
       |  Int => scala/Int#
       |  vr_= => local7
       |  x$1 => local8
       |  Unit => scala/Unit#
       |local0 => abstract var method vs: Int
       |  Int => scala/Int#
       |local1 => abstract var method vs_=(x$1: Int): Unit
       |  x$1 => local2
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => abstract var method vn: Int
       |  Int => scala/Int#
       |local4 => abstract var method vn_=(x$1: Int): Unit
       |  x$1 => local5
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local5 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local6 => abstract var method vr: Int
       |  Int => scala/Int#
       |local7 => abstract var method vr_=(x$1: Int): Unit
       |  x$1 => local8
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local8 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "vars in an anonymous and a local class",
    """|class s3 {
       |  def anon = new { var va = 1 }
       |  def m = {
       |    class c { var v = 1 }
       |    new c
       |  }
       |}
       |""".stripMargin,
    """|_empty_/s3# => class s3 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s3#`<init>`(). => primary ctor <init>()
       |_empty_/s3#anon(). => method anon: AnyRef { var def va: Int; var def va_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  va => local0
       |  Int => scala/Int#
       |  va_= => local1
       |  x$1 => local2
       |  Unit => scala/Unit#
       |_empty_/s3#m(). => method m: AnyRef { var def v: Int; var def v_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  v => local4
       |  Int => scala/Int#
       |  v_= => local5
       |  x$1 => local6
       |  Unit => scala/Unit#
       |local0 => var method va: Int
       |  Int => scala/Int#
       |local1 => var method va_=(x$1: Int): Unit
       |  x$1 => local2
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => final class $anon extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local4 => var method v: Int
       |  Int => scala/Int#
       |local5 => var method v_=(x$1: Int): Unit
       |  x$1 => local6
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local6 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local7 => class c extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local8 => primary ctor <init>()
       |""".stripMargin,
  )

  test("parameter of a local case class with a companion that defines apply") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object t31 {
         |  def m: Any = {
         |    case class k(i: Int)
         |    object k { def apply(i: Int): k = new k(i) }
         |    k(1)
         |  }
         |}
         |""".stripMargin,
      "Symbols",
    )
    assertNoDiff(
      load().linesIterator.filter(_.startsWith("local1 =>")).mkString("\n"),
      "local1 => val param i: Int",
    )
  }

  occurrences(
    "companion object before its case class",
    """|object t126
       |case class t126(a: Int)
       |""".stripMargin,
    """|[0:7..0:11): t126 <= _empty_/t126.
       |[1:11..1:15): t126 <= _empty_/t126#
       |[1:15..1:15):  <= _empty_/t126#`<init>`().
       |[1:16..1:17): a <= _empty_/t126#`<init>`().(a)
       |[1:16..1:17): a <= _empty_/t126#a.
       |[1:19..1:22): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "local companion object before its case class",
    """|object t127 {
       |  def m = {
       |    object C
       |    case class C(a: Int)
       |    C(1)
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t127 <= _empty_/t127.
       |[1:6..1:7): m <= _empty_/t127.m().
       |[2:11..2:12): C <= local5
       |[3:15..3:16): C <= local0
       |[3:16..3:16):  <= local10
       |[3:17..3:18): a <= local1
       |[3:20..3:23): Int => scala/Int#
       |[4:4..4:5): C => local5
       |""".stripMargin,
  )

  symbols(
    "companion object before its case class",
    """|object t126
       |case class t126(a: Int)
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t126# => case class t126 extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |_empty_/t126#`<init>`(). => primary ctor <init>(val a: Int)
         |  a => _empty_/t126#`<init>`().(a)
         |  Int => scala/Int#
         |_empty_/t126#`<init>`().(a) => val param a: Int
         |  Int => scala/Int#
         |_empty_/t126#a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/t126#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t126#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t126#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t126#copy$default$1(). => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t126#copy(). => synthetic method copy(a: Int): t126
         |  a => _empty_/t126#copy().(a)
         |  Int => scala/Int#
         |  t126 => _empty_/t126#
         |_empty_/t126#copy().(a) => default param a: Int
         |  Int => scala/Int#
         |_empty_/t126#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t126#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t126#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t126#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t126#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t126#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t126#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t126#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t126#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t126#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t126#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t126. => final object t126 extends AnyRef with Serializable { +3 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |_empty_/t126.apply(). => synthetic method apply(a: Int): t126
         |  a => _empty_/t126.apply().(a)
         |  Int => scala/Int#
         |  t126 => _empty_/t126#
         |_empty_/t126.apply().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t126.readResolve(). => private synthetic method readResolve(): Object
         |  Object => java/lang/Object#
         |_empty_/t126.unapply(). => synthetic method unapply(x$0: t126): Option[Int]
         |  x$0 => _empty_/t126.unapply().(x$0)
         |  t126 => _empty_/t126#
         |  Option => scala/Option#
         |  Int => scala/Int#
         |_empty_/t126.unapply().(x$0) => synthetic param x$0: t126
         |  t126 => _empty_/t126#
         |local0 => synthetic var local acc: Int
         |  Int => scala/Int#
         |local1 => synthetic val local t126$1: t126
         |  t126 => _empty_/t126#
         |""".stripMargin
    else
      """|_empty_/t126# => case class t126 extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |_empty_/t126#`<init>`(). => primary ctor <init>(val a: Int)
         |  a => _empty_/t126#`<init>`().(a)
         |  Int => scala/Int#
         |_empty_/t126#`<init>`().(a) => val param a: Int
         |  Int => scala/Int#
         |_empty_/t126#a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/t126#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t126#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t126#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t126#copy$default$1(). => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t126#copy(). => synthetic method copy(a: Int): t126
         |  a => _empty_/t126#copy().(a)
         |  Int => scala/Int#
         |  t126 => _empty_/t126#
         |_empty_/t126#copy().(a) => default param a: Int
         |  Int => scala/Int#
         |_empty_/t126#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t126#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t126#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t126#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t126#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t126#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t126#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t126#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t126#productElementName(). => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => _empty_/t126#productElementName().(x$1)
         |  Int => scala/Int#
         |  String => java/lang/String#
         |_empty_/t126#productElementName().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t126#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t126#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t126#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t126. => final object t126 extends AnyRef with Serializable { +3 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => java/io/Serializable#
         |_empty_/t126.apply(). => synthetic method apply(a: Int): t126
         |  a => _empty_/t126.apply().(a)
         |  Int => scala/Int#
         |  t126 => _empty_/t126#
         |_empty_/t126.apply().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t126.unapply(). => synthetic method unapply(x$0: t126): Option[Int]
         |  x$0 => _empty_/t126.unapply().(x$0)
         |  t126 => _empty_/t126#
         |  Option => scala/Option#
         |  Int => scala/Int#
         |_empty_/t126.unapply().(x$0) => synthetic param x$0: t126
         |  t126 => _empty_/t126#
         |_empty_/t126.writeReplace(). => private synthetic method writeReplace(): Object
         |  Object => java/lang/Object#
         |local0 => synthetic var local acc: Int
         |  Int => scala/Int#
         |local1 => synthetic val local t126$1: t126
         |  t126 => _empty_/t126#
         |""".stripMargin,
  )

  symbols(
    "local companion object before its case class",
    """|object t127 {
       |  def m = {
       |    object C
       |    case class C(a: Int)
       |    C(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t127. => final object t127 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t127.m(). => method m: C forSome { case class C extends AnyRef with Product with Serializable { +12 decls } }
         |  C => local0
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local0 => case class C extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val param a: Int
         |  Int => scala/Int#
         |local2 => synthetic method copy(a: Int): C
         |  a => local3
         |  Int => scala/Int#
         |  C => local0
         |local3 => default param a: Int
         |  Int => scala/Int#
         |local4 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => final object C extends AnyRef with Serializable { +2 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |local6 => synthetic method apply(a: Int): C
         |  a => local8
         |  Int => scala/Int#
         |  C => local0
         |local7 => synthetic method unapply(x$0: C): Option[Int]
         |  x$0 => local9
         |  C => local0
         |  Option => scala/Option#
         |  Int => scala/Int#
         |local8 => param a: Int
         |  Int => scala/Int#
         |local9 => synthetic param x$0: C
         |  C => local0
         |local10 => primary ctor <init>(val a: Int)
         |  a => local1
         |  Int => scala/Int#
         |local11 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local12 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local13 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local19
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local14 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local15 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local20
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local16 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local17 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local18 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local22
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local19 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local20 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local21 => synthetic var local acc: Int
         |  Int => scala/Int#
         |local22 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local23 => synthetic val local C$1: C
         |  C => local0
         |""".stripMargin
    else
      """|_empty_/t127. => final object t127 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t127.m(). => method m: C forSome { case class C extends AnyRef with Product with Serializable { +13 decls } }
         |  C => local0
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local0 => case class C extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val param a: Int
         |  Int => scala/Int#
         |local2 => synthetic method copy(a: Int): C
         |  a => local3
         |  Int => scala/Int#
         |  C => local0
         |local3 => default param a: Int
         |  Int => scala/Int#
         |local4 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => final object C extends AnyRef with Serializable { +2 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => java/io/Serializable#
         |local6 => synthetic method apply(a: Int): C
         |  a => local8
         |  Int => scala/Int#
         |  C => local0
         |local7 => synthetic method unapply(x$0: C): Option[Int]
         |  x$0 => local9
         |  C => local0
         |  Option => scala/Option#
         |  Int => scala/Int#
         |local8 => param a: Int
         |  Int => scala/Int#
         |local9 => synthetic param x$0: C
         |  C => local0
         |local10 => primary ctor <init>(val a: Int)
         |  a => local1
         |  Int => scala/Int#
         |local11 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local12 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local13 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local20
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local14 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local15 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local21
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local16 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local22
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local17 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local18 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local19 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local24
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local20 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local21 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local22 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local23 => synthetic var local acc: Int
         |  Int => scala/Int#
         |local24 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local25 => synthetic val local C$1: C
         |  C => local0
         |""".stripMargin,
  )

  symbols(
    "local definitions with synthetic parameters",
    """|object s4 {
       |  def m: Any = {
       |    def g[T: Ordering](t: T) = t
       |    class c[T: Ordering](t: T)
       |    case class k(i: Int)
       |    val f: Int => Int = _ + 1
       |    (g(1), new c(1), k(1), f)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/s4. => final object s4 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/s4.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => method g[T](t: T)(implicit evidence$1: Ordering[T]): T
         |  T => local3
         |  t => local1
         |  evidence$1 => local2
         |  Ordering => scala/package.Ordering#
         |local1 => param t: T
         |  T => local3
         |local2 => synthetic implicit param evidence$1: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local3
         |local3 => typeparam T
         |local4 => class c[T] extends AnyRef { +3 decls }
         |  T => local8
         |  AnyRef => scala/AnyRef#
         |local5 => param t: T
         |  T => local8
         |local6 => implicit param evidence$2: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local8
         |local7 => primary ctor <init>(t: T)(implicit evidence$2: Ordering[T])
         |  t => local5
         |  T => local8
         |  evidence$2 => local6
         |  Ordering => scala/package.Ordering#
         |local8 => typeparam T
         |local9 => case class k extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local10 => val param i: Int
         |  Int => scala/Int#
         |local11 => primary ctor <init>(val i: Int)
         |  i => local10
         |  Int => scala/Int#
         |local12 => synthetic method copy(i: Int): k
         |  i => local22
         |  Int => scala/Int#
         |  k => local9
         |local13 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local14 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local15 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local16 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local23
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local17 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local18 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local24
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local19 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local20 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local21 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local26
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local22 => default param i: Int
         |  Int => scala/Int#
         |local23 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local24 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local25 => synthetic var local acc: Int
         |  Int => scala/Int#
         |local26 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local27 => synthetic val local k$1: k
         |  k => local9
         |local28 => synthetic final object k extends AbstractFunction1[Int, k] with Serializable { +3 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  Int => scala/Int#
         |  k => local9
         |  Serializable => scala/Serializable#
         |local29 => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local30 => synthetic method apply(i: Int): k <: scala/Function1#apply().
         |  i => local32
         |  Int => scala/Int#
         |  k => local9
         |local31 => synthetic method unapply(x$0: k): Option[Int]
         |  x$0 => local33
         |  k => local9
         |  Option => scala/Option#
         |  Int => scala/Int#
         |local32 => param i: Int
         |  Int => scala/Int#
         |local33 => synthetic param x$0: k
         |  k => local9
         |local34 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local35 => val local f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/s4. => final object s4 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/s4.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => method g[T](t: T)(implicit evidence$1: Ordering[T]): T
         |  T => local3
         |  t => local1
         |  evidence$1 => local2
         |  Ordering => scala/package.Ordering#
         |local1 => param t: T
         |  T => local3
         |local2 => synthetic implicit param evidence$1: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local3
         |local3 => typeparam T
         |local4 => class c[T] extends AnyRef { +3 decls }
         |  T => local8
         |  AnyRef => scala/AnyRef#
         |local5 => param t: T
         |  T => local8
         |local6 => implicit param evidence$2: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local8
         |local7 => primary ctor <init>(t: T)(implicit evidence$2: Ordering[T])
         |  t => local5
         |  T => local8
         |  evidence$2 => local6
         |  Ordering => scala/package.Ordering#
         |local8 => typeparam T
         |local9 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local10 => val param i: Int
         |  Int => scala/Int#
         |local11 => primary ctor <init>(val i: Int)
         |  i => local10
         |  Int => scala/Int#
         |local12 => synthetic method copy(i: Int): k
         |  i => local23
         |  Int => scala/Int#
         |  k => local9
         |local13 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local14 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local15 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local16 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local24
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local17 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local18 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local25
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local19 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local26
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local20 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local21 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local22 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local28
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local23 => default param i: Int
         |  Int => scala/Int#
         |local24 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local25 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local26 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local27 => synthetic var local acc: Int
         |  Int => scala/Int#
         |local28 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local29 => synthetic val local k$1: k
         |  k => local9
         |local30 => synthetic final object k extends AbstractFunction1[Int, k] with Serializable { +3 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  Int => scala/Int#
         |  k => local9
         |  Serializable => java/io/Serializable#
         |local31 => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local32 => synthetic method apply(i: Int): k <: scala/Function1#apply().
         |  i => local34
         |  Int => scala/Int#
         |  k => local9
         |local33 => synthetic method unapply(x$0: k): Option[Int]
         |  x$0 => local35
         |  k => local9
         |  Option => scala/Option#
         |  Int => scala/Int#
         |local34 => param i: Int
         |  Int => scala/Int#
         |local35 => synthetic param x$0: k
         |  k => local9
         |local36 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local37 => val local f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  symbols(
    "default argument that refers to a parameter",
    """|object s7 {
       |  def m = { def f(a: Int = 1)(b: Int = a) = b; f()() }
       |}
       |""".stripMargin,
    """|_empty_/s7. => final object s7 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s7.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => method f(a: Int)(b: Int): Int
       |  a => local1
       |  Int => scala/Int#
       |  b => local2
       |local1 => default param a: Int
       |  Int => scala/Int#
       |local2 => default param b: Int
       |  Int => scala/Int#
       |local3 => synthetic method f$default$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |local4 => synthetic method f$default$2(a: Int): Int @uncheckedVariance
       |  a => local1
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |local5 => val local x$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |local6 => val local x$2: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "local implicit class",
    """|object s8 {
       |  def m = { implicit class R(val x: Int) { def d = x }; 1.d + new R(x = 2).x }
       |}
       |""".stripMargin,
    """|_empty_/s8. => final object s8 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s8.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => implicit class R extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => val param x: Int
       |  Int => scala/Int#
       |local2 => primary ctor <init>(val x: Int)
       |  x => local1
       |  Int => scala/Int#
       |local3 => method d: Int
       |  Int => scala/Int#
       |local4 => synthetic implicit method R(x: Int): R
       |  x => local5
       |  Int => scala/Int#
       |  R => local0
       |local5 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  test("named arguments of a local case class") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object s9 {
         |  def m: Any = { case class k(i: Int); val x = k(i = 1); x.copy(i = 3).i }
         |}
         |""".stripMargin,
      "Occurrences",
    )
    assertNoDiff(
      load().linesIterator.filter(_.contains("): i ")).mkString("\n"),
      if (ScalaVersion.is212)
        """|[1:30..1:31): i <= local1
           |[1:49..1:50): i => local23
           |[1:64..1:65): i => local13
           |[1:71..1:72): i => local1
           |""".stripMargin
      else
        """|[1:30..1:31): i <= local1
           |[1:49..1:50): i => local25
           |[1:64..1:65): i => local14
           |[1:71..1:72): i => local1
           |""".stripMargin,
    )
  }

  symbols(
    "var in a refinement of a val parameter",
    """|class s5(val n: { var vn: Int })
       |""".stripMargin,
    """|_empty_/s5# => class s5 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s5#`<init>`(). => primary ctor <init>(val n: AnyRef { var def vn: Int; var def vn_=(x$1: Int): Unit })
       |  n => _empty_/s5#`<init>`().(n)
       |  AnyRef => scala/AnyRef#
       |  vn => local0
       |  Int => scala/Int#
       |  vn_= => local1
       |  x$1 => local2
       |  Unit => scala/Unit#
       |_empty_/s5#`<init>`().(n) => val param n: AnyRef { var def vn: Int; var def vn_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vn => local0
       |  Int => scala/Int#
       |  vn_= => local1
       |  x$1 => local2
       |  Unit => scala/Unit#
       |_empty_/s5#n. => val method n: AnyRef { var def vn: Int; var def vn_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vn => local0
       |  Int => scala/Int#
       |  vn_= => local1
       |  x$1 => local2
       |  Unit => scala/Unit#
       |local0 => abstract var method vn: Int
       |  Int => scala/Int#
       |local1 => abstract var method vn_=(x$1: Int): Unit
       |  x$1 => local2
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "var in a refinement of a local val",
    """|class s6 {
       |  def m = {
       |    val n: { var vn: Int } = ???
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|_empty_/s6# => class s6 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s6#`<init>`(). => primary ctor <init>()
       |_empty_/s6#m(). => method m: Unit
       |  Unit => scala/Unit#
       |local0 => val local n: AnyRef { var def vn: Int; var def vn_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vn => local1
       |  Int => scala/Int#
       |  vn_= => local2
       |  x$1 => local3
       |  Unit => scala/Unit#
       |local1 => abstract var method vn: Int
       |  Int => scala/Int#
       |local2 => abstract var method vn_=(x$1: Int): Unit
       |  x$1 => local3
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local3 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "pattern definitions in a method",
    """|object t1 {
       |  def m = {
       |    val (a, b) = (1, 2)
       |    var (c, d) = (1, 2)
       |    lazy val (e, f) = (1, 2)
       |    val _ = 1
       |    a + b + c + d + e + f
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t1. => final object t1 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t1.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => val local a: Int
         |  Int => scala/Int#
         |local1 => val local b: Int
         |  Int => scala/Int#
         |local2 => synthetic val local x$1: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local3 => var local c: Int
         |  Int => scala/Int#
         |local4 => var local d: Int
         |  Int => scala/Int#
         |local5 => synthetic val local x$2: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local6 => lazy val local e: Int
         |  Int => scala/Int#
         |local7 => lazy val local f: Int
         |  Int => scala/Int#
         |local8 => synthetic lazy val local x$3: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local9 => val local _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t1. => final object t1 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t1.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => val local a: Int
         |  Int => scala/Int#
         |local1 => val local b: Int
         |  Int => scala/Int#
         |local2 => synthetic val local x$1: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local3 => var local c: Int
         |  Int => scala/Int#
         |local4 => var local d: Int
         |  Int => scala/Int#
         |local5 => synthetic val local x$2: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local6 => lazy val local e: Int
         |  Int => scala/Int#
         |local7 => lazy val local f: Int
         |  Int => scala/Int#
         |local8 => synthetic lazy val local x$3: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local9 => synthetic val local x$4: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  occurrences(
    "pattern definitions in a method",
    """|object t1 {
       |  def m = {
       |    val (a, b) = (1, 2)
       |    var (c, d) = (1, 2)
       |    lazy val (e, f) = (1, 2)
       |    val _ = 1
       |    a + b + c + d + e + f
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): t1 <= _empty_/t1.
       |[1:6..1:7): m <= _empty_/t1.m().
       |[2:9..2:10): a <= local0
       |[2:12..2:13): b <= local1
       |[3:9..3:10): c <= local3
       |[3:12..3:13): d <= local4
       |[4:14..4:15): e <= local6
       |[4:17..4:18): f <= local7
       |[5:8..5:9): _ <= local9
       |[6:4..6:5): a => local0
       |[6:6..6:7): + => scala/Int#`+`(+4).
       |[6:8..6:9): b => local1
       |[6:10..6:11): + => scala/Int#`+`(+4).
       |[6:12..6:13): c => local3
       |[6:14..6:15): + => scala/Int#`+`(+4).
       |[6:16..6:17): d => local4
       |[6:18..6:19): + => scala/Int#`+`(+4).
       |[6:20..6:21): e => local6
       |[6:22..6:23): + => scala/Int#`+`(+4).
       |[6:24..6:25): f => local7
       |""".stripMargin,
  )

  synthetics(
    "pattern definitions in a method",
    """|object t1 {
       |  def m = {
       |    val (a, b) = (1, 2)
       |    var (c, d) = (1, 2)
       |    lazy val (e, f) = (1, 2)
       |    val _ = 1
       |    a + b + c + d + e + f
       |  }
       |}
       |""".stripMargin,
    """|[2:8..2:14): (a, b) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[3:8..3:14): (c, d) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[4:13..4:19): (e, f) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "pattern definitions in a class body",
    """|class t2 {
       |  val (a, b) = (1, 2)
       |  var (c, d) = (1, 2)
       |  lazy val (e, f) = (1, 2)
       |  private val (g, h) = (1, 2)
       |  val _ = 1
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t2# => class t2 extends AnyRef { +16 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t2#_. => val method _: Int
         |  Int => scala/Int#
         |_empty_/t2#`<init>`(). => primary ctor <init>()
         |_empty_/t2#`c_=`(). => var method c_=(x$1: Int): Unit
         |  x$1 => _empty_/t2#`c_=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/t2#`c_=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t2#`d_=`(). => var method d_=(x$1: Int): Unit
         |  x$1 => _empty_/t2#`d_=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/t2#`d_=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t2#a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/t2#b. => val method b: Int
         |  Int => scala/Int#
         |_empty_/t2#c(). => var method c: Int
         |  Int => scala/Int#
         |_empty_/t2#d(). => var method d: Int
         |  Int => scala/Int#
         |_empty_/t2#e. => lazy val method e: Int
         |  Int => scala/Int#
         |_empty_/t2#f. => lazy val method f: Int
         |  Int => scala/Int#
         |_empty_/t2#g. => private val method g: Int
         |  Int => scala/Int#
         |_empty_/t2#h. => private val method h: Int
         |  Int => scala/Int#
         |_empty_/t2#x$1. => synthetic val method x$1: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$2. => synthetic val method x$2: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$3. => synthetic lazy val method x$3: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$4. => synthetic val method x$4: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t2# => class t2 extends AnyRef { +16 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t2#`<init>`(). => primary ctor <init>()
         |_empty_/t2#`c_=`(). => var method c_=(x$1: Int): Unit
         |  x$1 => _empty_/t2#`c_=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/t2#`c_=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t2#`d_=`(). => var method d_=(x$1: Int): Unit
         |  x$1 => _empty_/t2#`d_=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/t2#`d_=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t2#a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/t2#b. => val method b: Int
         |  Int => scala/Int#
         |_empty_/t2#c(). => var method c: Int
         |  Int => scala/Int#
         |_empty_/t2#d(). => var method d: Int
         |  Int => scala/Int#
         |_empty_/t2#e. => lazy val method e: Int
         |  Int => scala/Int#
         |_empty_/t2#f. => lazy val method f: Int
         |  Int => scala/Int#
         |_empty_/t2#g. => private val method g: Int
         |  Int => scala/Int#
         |_empty_/t2#h. => private val method h: Int
         |  Int => scala/Int#
         |_empty_/t2#x$1. => synthetic val method x$1: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$2. => synthetic val method x$2: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$3. => synthetic lazy val method x$3: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$4. => synthetic val method x$4: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$5. => synthetic val method x$5: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  occurrences(
    "pattern definitions in a class body",
    """|class t2 {
       |  val (a, b) = (1, 2)
       |  var (c, d) = (1, 2)
       |  lazy val (e, f) = (1, 2)
       |  private val (g, h) = (1, 2)
       |  val _ = 1
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:6..0:8): t2 <= _empty_/t2#
         |[0:9..0:9):  <= _empty_/t2#`<init>`().
         |[1:7..1:8): a <= _empty_/t2#a.
         |[1:10..1:11): b <= _empty_/t2#b.
         |[2:7..2:8): c <= _empty_/t2#c().
         |[2:10..2:11): d <= _empty_/t2#d().
         |[3:12..3:13): e <= _empty_/t2#e.
         |[3:15..3:16): f <= _empty_/t2#f.
         |[4:15..4:16): g <= _empty_/t2#g.
         |[4:18..4:19): h <= _empty_/t2#h.
         |[5:6..5:7): _ <= _empty_/t2#_.
         |""".stripMargin
    else
      """|[0:6..0:8): t2 <= _empty_/t2#
         |[0:9..0:9):  <= _empty_/t2#`<init>`().
         |[1:7..1:8): a <= _empty_/t2#a.
         |[1:10..1:11): b <= _empty_/t2#b.
         |[2:7..2:8): c <= _empty_/t2#c().
         |[2:10..2:11): d <= _empty_/t2#d().
         |[3:12..3:13): e <= _empty_/t2#e.
         |[3:15..3:16): f <= _empty_/t2#f.
         |[4:15..4:16): g <= _empty_/t2#g.
         |[4:18..4:19): h <= _empty_/t2#h.
         |[5:6..5:7): _ <= _empty_/t2#x$5.
         |""".stripMargin,
  )

  synthetics(
    "pattern definitions in a class body",
    """|class t2 {
       |  val (a, b) = (1, 2)
       |  var (c, d) = (1, 2)
       |  lazy val (e, f) = (1, 2)
       |  private val (g, h) = (1, 2)
       |  val _ = 1
       |}
       |""".stripMargin,
    """|[1:6..1:12): (a, b) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[2:6..2:12): (c, d) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[3:11..3:17): (e, f) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[4:14..4:20): (g, h) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "pattern definitions in a local class",
    """|object t3 {
       |  def m = {
       |    class c { val (a, b) = (1, 2); var (p, q) = (1, 2) }
       |    new c
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t3. => final object t3 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t3.m(). => method m: AnyRef { val def a: Int; val def b: Int; var def p: Int; var def p_=(x$1: Int): Unit; var def q: Int; var def q_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  a => local0
       |  Int => scala/Int#
       |  b => local1
       |  p => local2
       |  p_= => local3
       |  x$1 => local4
       |  Unit => scala/Unit#
       |  q => local5
       |  q_= => local6
       |  x$1 => local7
       |local0 => val method a: Int
       |  Int => scala/Int#
       |local1 => val method b: Int
       |  Int => scala/Int#
       |local2 => var method p: Int
       |  Int => scala/Int#
       |local3 => var method p_=(x$1: Int): Unit
       |  x$1 => local4
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local4 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local5 => var method q: Int
       |  Int => scala/Int#
       |local6 => var method q_=(x$1: Int): Unit
       |  x$1 => local7
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local7 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local8 => class c extends AnyRef { +9 decls }
       |  AnyRef => scala/AnyRef#
       |local9 => primary ctor <init>()
       |local10 => synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local11 => synthetic val method x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "pattern definitions in a local class",
    """|object t3 {
       |  def m = {
       |    class c { val (a, b) = (1, 2); var (p, q) = (1, 2) }
       |    new c
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): t3 <= _empty_/t3.
       |[1:6..1:7): m <= _empty_/t3.m().
       |[2:10..2:11): c <= local8
       |[2:12..2:12):  <= local9
       |[2:19..2:20): a <= local0
       |[2:22..2:23): b <= local1
       |[2:40..2:41): p <= local2
       |[2:43..2:44): q <= local5
       |[3:8..3:9): c => local8
       |[3:9..3:9):  => local9
       |""".stripMargin,
  )

  synthetics(
    "pattern definitions in a local class",
    """|object t3 {
       |  def m = {
       |    class c { val (a, b) = (1, 2); var (p, q) = (1, 2) }
       |    new c
       |  }
       |}
       |""".stripMargin,
    """|[2:18..2:24): (a, b) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[2:39..2:45): (p, q) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |""".stripMargin,
  )

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
         |  x1 => local9
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
         |local8 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local9 => synthetic param x1: Int
         |  Int => scala/Int#
         |local10 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local11 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local12 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local17
         |  Int => scala/Int#
         |  B1 => local16
         |  x1 => local14
         |  default => local15
         |  Function1 => scala/Function1#
         |local13 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local20
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local14 => synthetic param x1: A1
         |  A1 => local17
         |local15 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local17
         |  B1 => local16
         |local16 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local17 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local18 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local19 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local20 => synthetic param x1: Int
         |  Int => scala/Int#
         |local21 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local22 => val local defaultCase$: Int
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
         |  x1 => local9
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
         |local8 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local9 => synthetic param x1: Int
         |  Int => scala/Int#
         |local10 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local11 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local12 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local17
         |  Int => scala/Int#
         |  B1 => local16
         |  x1 => local14
         |  default => local15
         |  Function1 => scala/Function1#
         |local13 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local20
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local14 => synthetic param x1: A1
         |  A1 => local17
         |local15 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local17
         |  B1 => local16
         |local16 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local17 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local18 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local19 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local20 => synthetic param x1: Int
         |  Int => scala/Int#
         |local21 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local22 => val local defaultCase$: Int
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
         |[2:26..2:26):  <= local11
         |[2:33..2:45): defaultCase$ <= local18
         |[2:49..2:61): defaultCase$ => local18
         |[2:62..2:63): > => scala/Int#`>`(+3).
         |[2:69..2:81): defaultCase$ => local18
         |[3:6..3:7): m <= _empty_/t14#m().
         |[3:8..3:9): x <= _empty_/t14#m().(x)
         |[3:11..3:14): Int => scala/Int#
         |[3:18..3:19): x => _empty_/t14#m().(x)
         |[3:33..3:45): defaultCase$ <= local22
         |[3:49..3:61): defaultCase$ => local22
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
         |[2:26..2:26):  <= local11
         |[2:33..2:45): defaultCase$ <= local18
         |[2:49..2:61): defaultCase$ => local18
         |[2:62..2:63): > => scala/Int#`>`(+3).
         |[2:69..2:81): defaultCase$ => local18
         |[3:6..3:7): m <= _empty_/t14#m().
         |[3:8..3:9): x <= _empty_/t14#m().(x)
         |[3:11..3:14): Int => scala/Int#
         |[3:18..3:19): x => _empty_/t14#m().(x)
         |[3:33..3:45): defaultCase$ <= local22
         |[3:49..3:61): defaultCase$ => local22
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
         |  x1 => local9
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
         |local8 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local9 => synthetic param x1: Int
         |  Int => scala/Int#
         |local10 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local11 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local12 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local17
         |  Int => scala/Int#
         |  B1 => local16
         |  x1 => local14
         |  default => local15
         |  Function1 => scala/Function1#
         |local13 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local20
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local14 => synthetic param x1: A1
         |  A1 => local17
         |local15 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local17
         |  B1 => local16
         |local16 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local17 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local18 => val local i: Int
         |  Int => scala/Int#
         |local19 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local20 => synthetic param x1: Int
         |  Int => scala/Int#
         |local21 => val local defaultCase$: Int
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
         |  x1 => local9
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
         |local8 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local9 => synthetic param x1: Int
         |  Int => scala/Int#
         |local10 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local11 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local12 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local17
         |  Int => scala/Int#
         |  B1 => local16
         |  x1 => local14
         |  default => local15
         |  Function1 => scala/Function1#
         |local13 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local20
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local14 => synthetic param x1: A1
         |  A1 => local17
         |local15 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local17
         |  B1 => local16
         |local16 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local17 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local18 => val local i: Int
         |  Int => scala/Int#
         |local19 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local20 => synthetic param x1: Int
         |  Int => scala/Int#
         |local21 => val local defaultCase$: Int
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
       |  x => local9
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
       |local7 => val local defaultCase$: Int
       |  Int => scala/Int#
       |local8 => val local defaultCase$: Int
       |  Int => scala/Int#
       |local9 => synthetic param x: Int
       |  Int => scala/Int#
       |local10 => val local defaultCase$: Int
       |  Int => scala/Int#
       |local11 => val local defaultCase$: Int
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
       |[1:43..1:44): x => scala/Any#asInstanceOf().
       |[1:45..1:45):  <= local0
       |[1:45..1:46): + => local1
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
       |[1:16..1:17): f <= local13
       |[1:19..1:34): PartialFunction => scala/PartialFunction#
       |[1:35..1:38): Int => scala/Int#
       |[1:40..1:43): Int => scala/Int#
       |[1:52..1:52):  <= local0
       |[1:52..1:53): x => local3
       |[1:55..1:56): f => local13
       |""".stripMargin,
  )

  symbols(
    "patterns, eta expansion and updates",
    """|class t13 {
       |  def f = for ((a, b) <- List((1, 2))) yield a
       |  def g(a: Int)(b: Int) = a + b
       |  def e = List(1).map(g(1 + 1))
       |  def h = { val arr = Array(1); var i = 0; arr(i + 0) += 1; i }
       |  def m(x: Any) = x match { case (a: Int, b) => a; case l @ List(_) => l; case _ => 0 }
       |}
       |""".stripMargin,
    """|_empty_/t13# => class t13 extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t13#`<init>`(). => primary ctor <init>()
       |_empty_/t13#e(). => method e: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t13#f(). => method f: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t13#g(). => method g(a: Int)(b: Int): Int
       |  a => _empty_/t13#g().(a)
       |  Int => scala/Int#
       |  b => _empty_/t13#g().(b)
       |_empty_/t13#g().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t13#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t13#h(). => method h: Int
       |  Int => scala/Int#
       |_empty_/t13#m(). => method m(x: Any): Any
       |  x => _empty_/t13#m().(x)
       |  Any => scala/Any#
       |_empty_/t13#m().(x) => param x: Any
       |  Any => scala/Any#
       |local0 => synthetic param check$ifrefutable$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local b: Int
       |  Int => scala/Int#
       |local4 => val local arr: Array[Int]
       |  Array => scala/Array#
       |  Int => scala/Int#
       |local5 => var local i: Int
       |  Int => scala/Int#
       |local6 => synthetic val local ev$1: Int
       |  Int => scala/Int#
       |local7 => val local a: Int
       |  Int => scala/Int#
       |local8 => val local b: Any
       |  Any => scala/Any#
       |local9 => val local l: List[A]
       |  List => scala/collection/immutable/List#
       |  A => local10
       |local10 => abstract type A
       |""".stripMargin,
  )

  symbols(
    "value definitions in a for comprehension",
    """|object t4 {
       |  def m = for { x <- List(1); (y, z) = (x, x); w :: Nil = List(y) } yield (y, z, w)
       |}
       |""".stripMargin,
    """|_empty_/t4. => final object t4 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t4.m(). => method m: List[Tuple3[Int, Int, Int]]
       |  List => scala/collection/immutable/List#
       |  Tuple3 => scala/Tuple3#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$5: Tuple3[Int, Tuple2[Int, Int], ::[Int]]
       |  Tuple3 => scala/Tuple3#
       |  Int => scala/Int#
       |  Tuple2 => scala/Tuple2#
       |  :: => scala/collection/immutable/`::`#
       |local2 => val local y: Int
       |  Int => scala/Int#
       |local3 => val local z: Int
       |  Int => scala/Int#
       |local4 => synthetic val local x$3: Tuple3[Tuple2[Int, Int], Int, Int]
       |  Tuple3 => scala/Tuple3#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local5 => val local w: Int
       |  Int => scala/Int#
       |local6 => synthetic val local x$4: Tuple2[::[Int], Int]
       |  Tuple2 => scala/Tuple2#
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "value definitions in a for comprehension",
    """|object t4 {
       |  def m = for { x <- List(1); (y, z) = (x, x); w :: Nil = List(y) } yield (y, z, w)
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:9): t4 <= _empty_/t4.
         |[1:6..1:7): m <= _empty_/t4.m().
         |[1:16..1:17): x <= local0
         |[1:21..1:25): List => scala/collection/immutable/List.
         |[1:31..1:32): y <= local2
         |[1:34..1:35): z <= local3
         |[1:40..1:41): x => local0
         |[1:43..1:44): x => local0
         |[1:47..1:48): w <= local5
         |[1:49..1:51): :: => scala/package.`::`.
         |[1:52..1:55): Nil => scala/collection/immutable/Nil.
         |[1:58..1:62): List => scala/collection/immutable/List.
         |[1:63..1:64): y => local2
         |[1:75..1:76): y => local2
         |[1:78..1:79): z => local3
         |[1:81..1:82): w => local5
         |""".stripMargin
    else
      """|[0:7..0:9): t4 <= _empty_/t4.
         |[1:6..1:7): m <= _empty_/t4.m().
         |[1:16..1:17): x <= local0
         |[1:21..1:25): List => scala/package.List.
         |[1:31..1:32): y <= local2
         |[1:34..1:35): z <= local3
         |[1:40..1:41): x => local0
         |[1:43..1:44): x => local0
         |[1:47..1:48): w <= local5
         |[1:49..1:51): :: => scala/package.`::`.
         |[1:52..1:55): Nil => scala/package.Nil.
         |[1:58..1:62): List => scala/package.List.
         |[1:63..1:64): y => local2
         |[1:75..1:76): y => local2
         |[1:78..1:79): z => local3
         |[1:81..1:82): w => local5
         |""".stripMargin,
  )

  synthetics(
    "value definitions in a for comprehension",
    """|object t4 {
       |  def m = for { x <- List(1); (y, z) = (x, x); w :: Nil = List(y) } yield (y, z, w)
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:10..1:83): for { x <- List(1); (y, z) = (x, x); w :: Nil = List(y) } yield (y, z, w) => orig(List(1)).map[Tuple3[Int, Tuple2[Int, Int], ::[Int]], List[Tuple3[Int, Tuple2[Int, Int], ::[Int]]]]({(x) => orig((y, z) = (x, x); w :: Nil = List(y))})(List.canBuildFrom[Tuple3[Int, Tuple2[Int, Int], ::[Int]]]).map[Tuple3[Int, Int, Int], List[Tuple3[Int, Int, Int]]]({(x$5) => orig((y, z, w))})(List.canBuildFrom[Tuple3[Int, Int, Int]])
         |  map => scala/collection/immutable/List#map().
         |  Tuple3 => scala/Tuple3#
         |  Int => scala/Int#
         |  Tuple2 => scala/Tuple2#
         |  :: => scala/collection/immutable/`::`#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$5 => local1
         |[1:21..1:25): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:30..1:36): (y, z) => *.apply[Tuple2[Int, Int], Int, Int]
         |  apply => scala/Tuple3.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[1:47..1:55): w :: Nil => *.apply[::[Int], Int]
         |  apply => scala/Tuple2.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |[1:58..1:62): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:10..1:83): for { x <- List(1); (y, z) = (x, x); w :: Nil = List(y) } yield (y, z, w) => orig(List(1)).map[Tuple3[Int, Tuple2[Int, Int], ::[Int]]]({(x) => orig((y, z) = (x, x); w :: Nil = List(y))}).map[Tuple3[Int, Int, Int]]({(x$5) => orig((y, z, w))})
         |  map => scala/collection/immutable/List#map().
         |  Tuple3 => scala/Tuple3#
         |  Int => scala/Int#
         |  Tuple2 => scala/Tuple2#
         |  :: => scala/collection/immutable/`::`#
         |  x => local0
         |  x$5 => local1
         |[1:21..1:25): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:30..1:36): (y, z) => *.apply[Tuple2[Int, Int], Int, Int]
         |  apply => scala/Tuple3.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[1:47..1:55): w :: Nil => *.apply[::[Int], Int]
         |  apply => scala/Tuple2.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |[1:58..1:62): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

}

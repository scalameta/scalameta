package scala.meta.tests.semanticdb

class DefaultParameterSuite extends SemanticdbSuite {

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  private def synthetics(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: synthetics", code, expected, "Synthetics")

  symbols(
    "a local in a default of a generic method",
    """|object t250 {
       |  def m[T](x: T)(y: List[T] = { val z = List(x); z }) = y
       |}
       |""".stripMargin,
    """|_empty_/t250. => final object t250 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t250.m$default$2(). => synthetic method m$default$2[T](x: T): List[T] @uncheckedVariance
       |  T => _empty_/t250.m$default$2().[T]
       |  x => _empty_/t250.m$default$2().(x)
       |  List => scala/collection/immutable/List#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/t250.m$default$2().(x) => param x: T
       |  T => _empty_/t250.m$default$2().[T]
       |_empty_/t250.m$default$2().[T] => typeparam T
       |_empty_/t250.m(). => method m[T](x: T)(y: List[T]): List[T]
       |  T => _empty_/t250.m().[T]
       |  x => _empty_/t250.m().(x)
       |  y => _empty_/t250.m().(y)
       |  List => scala/package.List#
       |_empty_/t250.m().(x) => param x: T
       |  T => _empty_/t250.m().[T]
       |_empty_/t250.m().(y) => default param y: List[T]
       |  List => scala/package.List#
       |  T => _empty_/t250.m().[T]
       |_empty_/t250.m().[T] => typeparam T
       |local0 => val local z: List[T]
       |  List => scala/collection/immutable/List#
       |  T => _empty_/t250.m().[T]
       |""".stripMargin,
  )

  synthetics(
    "a local in a default of a generic method",
    """|object t250 {
       |  def m[T](x: T)(y: List[T] = { val z = List(x); z }) = y
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:40..1:44): List => *.apply[T]
         |  apply => scala/collection/immutable/List.apply().
         |  T => _empty_/t250.m().[T]
         |""".stripMargin
    else
      """|[1:40..1:44): List => *.apply[T]
         |  apply => scala/collection/IterableFactory#apply().
         |  T => _empty_/t250.m().[T]
         |""".stripMargin,
  )

  symbols(
    "a local in an overriding default of a generic method",
    """|trait t251a[A] { def m[T <: A](x: T)(y: Int = 0): Int }
       |class t251 extends t251a[Int] {
       |  override def m[T <: Int](x: T)(y: Int = { val z = List(x); z.size }) = y
       |}
       |""".stripMargin,
    """|_empty_/t251# => class t251 extends AnyRef with t251a[Int] { +3 decls }
       |  AnyRef => scala/AnyRef#
       |  t251a => _empty_/t251a#
       |  Int => scala/Int#
       |_empty_/t251#`<init>`(). => primary ctor <init>()
       |_empty_/t251#m$default$2(). => synthetic method m$default$2[T <: Int](x: T): Int @uncheckedVariance <: _empty_/t251a#m$default$2().
       |  T => _empty_/t251#m$default$2().[T]
       |  Int => scala/Int#
       |  x => _empty_/t251#m$default$2().(x)
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/t251#m$default$2().(x) => param x: T
       |  T => _empty_/t251#m$default$2().[T]
       |_empty_/t251#m$default$2().[T] => typeparam T <: Int
       |  Int => scala/Int#
       |_empty_/t251#m(). => method m[T <: Int](x: T)(y: Int): Int <: _empty_/t251a#m().
       |  T => _empty_/t251#m().[T]
       |  Int => scala/Int#
       |  x => _empty_/t251#m().(x)
       |  y => _empty_/t251#m().(y)
       |_empty_/t251#m().(x) => param x: T
       |  T => _empty_/t251#m().[T]
       |_empty_/t251#m().(y) => default param y: Int
       |  Int => scala/Int#
       |_empty_/t251#m().[T] => typeparam T <: Int
       |  Int => scala/Int#
       |_empty_/t251a# => trait t251a[A] extends AnyRef { +2 decls }
       |  A => _empty_/t251a#[A]
       |  AnyRef => scala/AnyRef#
       |_empty_/t251a#[A] => typeparam A
       |_empty_/t251a#m$default$2(). => synthetic method m$default$2[T <: A](x: T): Int @uncheckedVariance
       |  T => _empty_/t251a#m$default$2().[T]
       |  A => _empty_/t251a#[A]
       |  x => _empty_/t251a#m$default$2().(x)
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/t251a#m$default$2().(x) => param x: T
       |  T => _empty_/t251a#m$default$2().[T]
       |_empty_/t251a#m$default$2().[T] => typeparam T <: A
       |  A => _empty_/t251a#[A]
       |_empty_/t251a#m(). => abstract method m[T <: A](x: T)(y: Int): Int
       |  T => _empty_/t251a#m().[T]
       |  A => _empty_/t251a#[A]
       |  x => _empty_/t251a#m().(x)
       |  y => _empty_/t251a#m().(y)
       |  Int => scala/Int#
       |_empty_/t251a#m().(x) => param x: T
       |  T => _empty_/t251a#m().[T]
       |_empty_/t251a#m().(y) => default param y: Int
       |  Int => scala/Int#
       |_empty_/t251a#m().[T] => typeparam T <: A
       |  A => _empty_/t251a#[A]
       |local0 => val local z: List[T]
       |  List => scala/collection/immutable/List#
       |  T => _empty_/t251#m().[T]
       |""".stripMargin,
  )

  synthetics(
    "a local in an overriding default of a generic method",
    """|trait t251a[A] { def m[T <: A](x: T)(y: Int = 0): Int }
       |class t251 extends t251a[Int] {
       |  override def m[T <: Int](x: T)(y: Int = { val z = List(x); z.size }) = y
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[2:52..2:56): List => *.apply[T]
         |  apply => scala/collection/immutable/List.apply().
         |  T => _empty_/t251#m().[T]
         |""".stripMargin
    else
      """|[2:52..2:56): List => *.apply[T]
         |  apply => scala/collection/IterableFactory#apply().
         |  T => _empty_/t251#m().[T]
         |""".stripMargin,
  )

  symbols(
    "locals in defaults of a generic class's constructor and method",
    """|class t252[T](x: T)(y: List[T] = { val z = List(x); z }) {
       |  def m(v: List[T] = { val w = List(x); w }) = v
       |}
       |""".stripMargin,
    """|_empty_/t252# => class t252[T] extends AnyRef { +5 decls }
       |  T => _empty_/t252#[T]
       |  AnyRef => scala/AnyRef#
       |_empty_/t252#[T] => typeparam T
       |_empty_/t252#`<init>`(). => primary ctor <init>(x: T)(y: List[T])
       |  x => _empty_/t252#`<init>`().(x)
       |  T => _empty_/t252#[T]
       |  y => _empty_/t252#`<init>`().(y)
       |  List => scala/package.List#
       |_empty_/t252#`<init>`().(x) => param x: T
       |  T => _empty_/t252#[T]
       |_empty_/t252#`<init>`().(y) => default param y: List[T]
       |  List => scala/package.List#
       |  T => _empty_/t252#[T]
       |_empty_/t252#m$default$1(). => synthetic method m$default$1: List[T] @uncheckedVariance
       |  List => scala/package.List#
       |  T => _empty_/t252#[T]
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/t252#m(). => method m(v: List[T]): List[T]
       |  v => _empty_/t252#m().(v)
       |  List => scala/package.List#
       |  T => _empty_/t252#[T]
       |_empty_/t252#m().(v) => default param v: List[T]
       |  List => scala/package.List#
       |  T => _empty_/t252#[T]
       |_empty_/t252#x. => private[this] val method x: T
       |  T => _empty_/t252#[T]
       |_empty_/t252#y. => private[this] val method y: List[T]
       |  List => scala/package.List#
       |  T => _empty_/t252#[T]
       |_empty_/t252. => synthetic final object t252 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t252.`<init>$default$2`(). => synthetic method <init>$default$2[T](x: T): List[T] @uncheckedVariance
       |  T => _empty_/t252.`<init>$default$2`().[T]
       |  x => _empty_/t252.`<init>$default$2`().(x)
       |  List => scala/collection/immutable/List#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/t252.`<init>$default$2`().(x) => param x: T
       |  T => _empty_/t252.`<init>$default$2`().[T]
       |_empty_/t252.`<init>$default$2`().[T] => typeparam T
       |local0 => val local z: List[T]
       |  List => scala/collection/immutable/List#
       |  T => _empty_/t252#[T]
       |local1 => val local w: List[T]
       |  List => scala/collection/immutable/List#
       |  T => _empty_/t252#[T]
       |""".stripMargin,
  )

  synthetics(
    "locals in defaults of a generic class's constructor and method",
    """|class t252[T](x: T)(y: List[T] = { val z = List(x); z }) {
       |  def m(v: List[T] = { val w = List(x); w }) = v
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:43..0:47): List => *.apply[T]
         |  apply => scala/collection/immutable/List.apply().
         |  T => _empty_/t252#[T]
         |[1:31..1:35): List => *.apply[T]
         |  apply => scala/collection/immutable/List.apply().
         |  T => _empty_/t252#[T]
         |""".stripMargin
    else
      """|[0:43..0:47): List => *.apply[T]
         |  apply => scala/collection/IterableFactory#apply().
         |  T => _empty_/t252#[T]
         |[1:31..1:35): List => *.apply[T]
         |  apply => scala/collection/IterableFactory#apply().
         |  T => _empty_/t252#[T]
         |""".stripMargin,
  )

  symbols(
    "a local in a default of a generic case class",
    """|case class t259[T](x: T)(y: List[T] = { val z = List(x); z })
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t259# => case class t259[T] extends AnyRef with Product with Serializable { +13 decls }
         |  T => _empty_/t259#[T]
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |_empty_/t259#[T] => typeparam T
         |_empty_/t259#`<init>`(). => primary ctor <init>(val x: T)(y: List[T])
         |  x => _empty_/t259#`<init>`().(x)
         |  T => _empty_/t259#[T]
         |  y => _empty_/t259#`<init>`().(y)
         |  List => scala/package.List#
         |_empty_/t259#`<init>`().(x) => val param x: T
         |  T => _empty_/t259#[T]
         |_empty_/t259#`<init>`().(y) => default param y: List[T]
         |  List => scala/package.List#
         |  T => _empty_/t259#[T]
         |_empty_/t259#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t259#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t259#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t259#copy$default$1(). => synthetic method copy$default$1[T]: T @uncheckedVariance
         |  T => _empty_/t259#copy$default$1().[T]
         |  T => _empty_/t259#[T]
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t259#copy$default$1().[T] => typeparam T
         |_empty_/t259#copy(). => synthetic method copy[T](x: T)(y: List[T]): t259[T]
         |  T => _empty_/t259#copy().[T]
         |  x => _empty_/t259#copy().(x)
         |  y => _empty_/t259#copy().(y)
         |  List => scala/package.List#
         |  t259 => _empty_/t259#
         |_empty_/t259#copy().(x) => default param x: T
         |  T => _empty_/t259#copy().[T]
         |_empty_/t259#copy().(y) => param y: List[T]
         |  List => scala/package.List#
         |  T => _empty_/t259#copy().[T]
         |_empty_/t259#copy().[T] => typeparam T
         |_empty_/t259#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t259#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t259#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t259#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t259#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t259#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t259#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t259#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t259#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t259#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t259#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t259#x. => val method x: T
         |  T => _empty_/t259#[T]
         |_empty_/t259#y. => private[this] val method y: List[T]
         |  List => scala/package.List#
         |  T => _empty_/t259#[T]
         |_empty_/t259. => synthetic final object t259 extends AnyRef with Serializable { +6 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |_empty_/t259.`<init>$default$2`(). => synthetic method <init>$default$2[T](x: T): List[T] @uncheckedVariance
         |  T => _empty_/t259.`<init>$default$2`().[T]
         |  x => _empty_/t259.`<init>$default$2`().(x)
         |  List => scala/collection/immutable/List#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t259.`<init>$default$2`().(x) => param x: T
         |  T => _empty_/t259.`<init>$default$2`().[T]
         |_empty_/t259.`<init>$default$2`().[T] => typeparam T
         |_empty_/t259.apply$default$2(). => synthetic method apply$default$2[T](x: T): List[T] @uncheckedVariance
         |  T => _empty_/t259.apply$default$2().[T]
         |  x => _empty_/t259.apply$default$2().(x)
         |  List => scala/collection/immutable/List#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t259.apply$default$2().(x) => param x: T
         |  T => _empty_/t259.apply$default$2().[T]
         |_empty_/t259.apply$default$2().[T] => typeparam T
         |_empty_/t259.apply(). => synthetic method apply[T](x: T)(y: List[T]): t259[T]
         |  T => _empty_/t259.apply().[T]
         |  x => _empty_/t259.apply().(x)
         |  y => _empty_/t259.apply().(y)
         |  List => scala/package.List#
         |  t259 => _empty_/t259#
         |_empty_/t259.apply().(x) => param x: T
         |  T => _empty_/t259.apply().[T]
         |_empty_/t259.apply().(y) => default param y: List[T]
         |  List => scala/package.List#
         |  T => _empty_/t259.apply().[T]
         |_empty_/t259.apply().[T] => typeparam T
         |_empty_/t259.readResolve(). => private synthetic method readResolve(): Object
         |  Object => java/lang/Object#
         |_empty_/t259.toString(). => synthetic final method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t259.unapply(). => synthetic method unapply[T](x$0: t259[T]): Option[T]
         |  T => _empty_/t259.unapply().[T]
         |  x$0 => _empty_/t259.unapply().(x$0)
         |  t259 => _empty_/t259#
         |  Option => scala/Option#
         |_empty_/t259.unapply().(x$0) => synthetic param x$0: t259[T]
         |  t259 => _empty_/t259#
         |  T => _empty_/t259.unapply().[T]
         |_empty_/t259.unapply().[T] => typeparam T
         |local0 => val local z: List[T]
         |  List => scala/collection/immutable/List#
         |  T => _empty_/t259#[T]
         |""".stripMargin
    else
      """|_empty_/t259# => case class t259[T] extends AnyRef with Product with Serializable { +14 decls }
         |  T => _empty_/t259#[T]
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |_empty_/t259#[T] => typeparam T
         |_empty_/t259#`<init>`(). => primary ctor <init>(val x: T)(y: List[T])
         |  x => _empty_/t259#`<init>`().(x)
         |  T => _empty_/t259#[T]
         |  y => _empty_/t259#`<init>`().(y)
         |  List => scala/package.List#
         |_empty_/t259#`<init>`().(x) => val param x: T
         |  T => _empty_/t259#[T]
         |_empty_/t259#`<init>`().(y) => default param y: List[T]
         |  List => scala/package.List#
         |  T => _empty_/t259#[T]
         |_empty_/t259#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t259#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t259#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t259#copy$default$1(). => synthetic method copy$default$1[T]: T @uncheckedVariance
         |  T => _empty_/t259#copy$default$1().[T]
         |  T => _empty_/t259#[T]
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t259#copy$default$1().[T] => typeparam T
         |_empty_/t259#copy(). => synthetic method copy[T](x: T)(y: List[T]): t259[T]
         |  T => _empty_/t259#copy().[T]
         |  x => _empty_/t259#copy().(x)
         |  y => _empty_/t259#copy().(y)
         |  List => scala/package.List#
         |  t259 => _empty_/t259#
         |_empty_/t259#copy().(x) => default param x: T
         |  T => _empty_/t259#copy().[T]
         |_empty_/t259#copy().(y) => param y: List[T]
         |  List => scala/package.List#
         |  T => _empty_/t259#copy().[T]
         |_empty_/t259#copy().[T] => typeparam T
         |_empty_/t259#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t259#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t259#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t259#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t259#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t259#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t259#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t259#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t259#productElementName(). => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => _empty_/t259#productElementName().(x$1)
         |  Int => scala/Int#
         |  String => java/lang/String#
         |_empty_/t259#productElementName().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t259#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t259#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t259#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t259#x. => val method x: T
         |  T => _empty_/t259#[T]
         |_empty_/t259#y. => private[this] val method y: List[T]
         |  List => scala/package.List#
         |  T => _empty_/t259#[T]
         |_empty_/t259. => synthetic final object t259 extends AnyRef with Serializable { +6 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => java/io/Serializable#
         |_empty_/t259.`<init>$default$2`(). => synthetic method <init>$default$2[T](x: T): List[T] @uncheckedVariance
         |  T => _empty_/t259.`<init>$default$2`().[T]
         |  x => _empty_/t259.`<init>$default$2`().(x)
         |  List => scala/collection/immutable/List#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t259.`<init>$default$2`().(x) => param x: T
         |  T => _empty_/t259.`<init>$default$2`().[T]
         |_empty_/t259.`<init>$default$2`().[T] => typeparam T
         |_empty_/t259.apply$default$2(). => synthetic method apply$default$2[T](x: T): List[T] @uncheckedVariance
         |  T => _empty_/t259.apply$default$2().[T]
         |  x => _empty_/t259.apply$default$2().(x)
         |  List => scala/collection/immutable/List#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t259.apply$default$2().(x) => param x: T
         |  T => _empty_/t259.apply$default$2().[T]
         |_empty_/t259.apply$default$2().[T] => typeparam T
         |_empty_/t259.apply(). => synthetic method apply[T](x: T)(y: List[T]): t259[T]
         |  T => _empty_/t259.apply().[T]
         |  x => _empty_/t259.apply().(x)
         |  y => _empty_/t259.apply().(y)
         |  List => scala/package.List#
         |  t259 => _empty_/t259#
         |_empty_/t259.apply().(x) => param x: T
         |  T => _empty_/t259.apply().[T]
         |_empty_/t259.apply().(y) => default param y: List[T]
         |  List => scala/package.List#
         |  T => _empty_/t259.apply().[T]
         |_empty_/t259.apply().[T] => typeparam T
         |_empty_/t259.toString(). => synthetic final method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t259.unapply(). => synthetic method unapply[T](x$0: t259[T]): Option[T]
         |  T => _empty_/t259.unapply().[T]
         |  x$0 => _empty_/t259.unapply().(x$0)
         |  t259 => _empty_/t259#
         |  Option => scala/Option#
         |_empty_/t259.unapply().(x$0) => synthetic param x$0: t259[T]
         |  t259 => _empty_/t259#
         |  T => _empty_/t259.unapply().[T]
         |_empty_/t259.unapply().[T] => typeparam T
         |_empty_/t259.writeReplace(). => private synthetic method writeReplace(): Object
         |  Object => java/lang/Object#
         |local0 => val local z: List[T]
         |  List => scala/collection/immutable/List#
         |  T => _empty_/t259#[T]
         |""".stripMargin,
  )

  synthetics(
    "a local in a default of a generic case class",
    """|case class t259[T](x: T)(y: List[T] = { val z = List(x); z })
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:48..0:52): List => *.apply[T]
         |  apply => scala/collection/immutable/List.apply().
         |  T => _empty_/t259#[T]
         |""".stripMargin
    else
      """|[0:48..0:52): List => *.apply[T]
         |  apply => scala/collection/IterableFactory#apply().
         |  T => _empty_/t259#[T]
         |""".stripMargin,
  )

}

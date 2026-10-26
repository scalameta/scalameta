package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac._

class SyntheticLocalsWithoutSyntheticsSuite extends SemanticdbSuite {

  override def customizeConfig(config: SemanticdbConfig): SemanticdbConfig = config
    .copy(synthetics = BinaryMode.Off)

  checkSection(
    "local implicit class: symbols",
    """|object w1 {
       |  def m = { implicit class R(val x: Int) { def d = x }; 1.d }
       |}
       |""".stripMargin,
    """|_empty_/w1. => final object w1 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/w1.m(). => method m: Int
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
       |""".stripMargin,
    "Symbols",
  )

  checkSection(
    "local case class created with new: symbols",
    """|object w2 {
       |  def m: Any = {
       |    case class k(i: Int)
       |    new k(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/w2. => final object w2 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/w2.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => primary ctor <init>(val i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local3 => private synthetic method copy(i: Int): k
         |  i => local13
         |  Int => scala/Int#
         |  k => local0
         |local4 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local14
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local15
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local11 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local12 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local13 => default param i: Int
         |  Int => scala/Int#
         |local14 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin
    else
      """|_empty_/w2. => final object w2 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/w2.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => primary ctor <init>(val i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local3 => private synthetic method copy(i: Int): k
         |  i => local14
         |  Int => scala/Int#
         |  k => local0
         |local4 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local15
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local17
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => default param i: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local17 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin,
    "Symbols",
  )

  checkSection(
    "local case class with an implicit parameter created with new: symbols",
    """|object w3 {
       |  def m: Any = {
       |    case class k(i: Int)(implicit s: String)
       |    implicit val s: String = ""
       |    new k(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/w3. => final object w3 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/w3.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => implicit param s: String
         |  String => scala/Predef.String#
         |local3 => primary ctor <init>(val i: Int)(implicit s: String)
         |  i => local1
         |  Int => scala/Int#
         |  s => local2
         |  String => scala/Predef.String#
         |local4 => synthetic method copy(i: Int)(implicit s: String): k
         |  i => local14
         |  Int => scala/Int#
         |  s => local15
         |  String => scala/Predef.String#
         |  k => local0
         |local5 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local6 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local7 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local8 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local16
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local9 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local10 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local17
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => default param i: Int
         |  Int => scala/Int#
         |local15 => implicit param s: String
         |  String => scala/Predef.String#
         |local16 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local17 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local19 => implicit val local s: String
         |  String => scala/Predef.String#
         |""".stripMargin
    else
      """|_empty_/w3. => final object w3 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/w3.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +14 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => implicit param s: String
         |  String => scala/Predef.String#
         |local3 => primary ctor <init>(val i: Int)(implicit s: String)
         |  i => local1
         |  Int => scala/Int#
         |  s => local2
         |  String => scala/Predef.String#
         |local4 => synthetic method copy(i: Int)(implicit s: String): k
         |  i => local15
         |  Int => scala/Int#
         |  s => local16
         |  String => scala/Predef.String#
         |  k => local0
         |local5 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local6 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local7 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local8 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local17
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local9 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local10 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local11 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local19
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local12 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local13 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local14 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local20
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local15 => default param i: Int
         |  Int => scala/Int#
         |local16 => implicit param s: String
         |  String => scala/Predef.String#
         |local17 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local19 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local20 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local21 => implicit val local s: String
         |  String => scala/Predef.String#
         |""".stripMargin,
    "Symbols",
  )

}

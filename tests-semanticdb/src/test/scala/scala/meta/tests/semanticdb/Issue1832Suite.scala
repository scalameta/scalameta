package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1832
class Issue1832Suite extends SemanticdbSuite {

  symbols(
    """|package a
       |import scala.language.existentials
       |object Test {
       |  class M
       |  trait N
       |  class C {
       |    for {
       |      i <- 1.to(10)
       |      j <- 2.to(i)
       |    } yield j
       |    Option(null.asInstanceOf[{
       |      def bar(a: Int): Int
       |      def foo(a: Int): Int
       |    }])
       |    val compoundType4 = new { def k: Int = ??? }
       |    val compoundType6 = new M with N { def k: Int = ??? }
       |    val existentialType1: T forSome { type T } = ???
       |    val existentialType3 = Class.forName("foo.Bar")
       |  }
       |}
       |""".stripMargin,
    """|a/Test. => final object Test extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |a/Test.C# => class C extends AnyRef { +5 decls }
       |  AnyRef => scala/AnyRef#
       |a/Test.C#`<init>`(). => primary ctor <init>()
       |a/Test.C#compoundType4. => val method compoundType4: AnyRef { def k: Int }
       |  AnyRef => scala/AnyRef#
       |  k => local7
       |  Int => scala/Int#
       |a/Test.C#compoundType6. => val method compoundType6: M with N { def k: Int }
       |  M => a/Test.M#
       |  N => a/Test.N#
       |  k => local9
       |  Int => scala/Int#
       |a/Test.C#existentialType1. => val method existentialType1: T forSome { type T }
       |  T => local10
       |a/Test.C#existentialType3. => val method existentialType3: Class[?0] forSome { type ?0 }
       |  Class => java/lang/Class#
       |  ?0 => local11
       |a/Test.M# => class M extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |a/Test.M#`<init>`(). => primary ctor <init>()
       |a/Test.N# => trait N extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => param i: Int
       |  Int => scala/Int#
       |local1 => param j: Int
       |  Int => scala/Int#
       |local2 => abstract method bar(a: Int): Int
       |  a => local3
       |  Int => scala/Int#
       |local3 => param a: Int
       |  Int => scala/Int#
       |local4 => abstract method foo(a: Int): Int
       |  a => local5
       |  Int => scala/Int#
       |local5 => param a: Int
       |  Int => scala/Int#
       |local6 => final class $anon extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |local7 => method k: Int
       |  Int => scala/Int#
       |local8 => final class $anon extends M with N { +1 decls }
       |  M => a/Test.M#
       |  N => a/Test.N#
       |local9 => method k: Int
       |  Int => scala/Int#
       |local10 => abstract type T
       |""".stripMargin,
  )

}

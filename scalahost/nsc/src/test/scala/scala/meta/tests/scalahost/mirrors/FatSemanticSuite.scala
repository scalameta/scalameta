package scala.meta.tests.scalahost

import scala.meta.internal.semantic.SemanticdbMode

class FatSemanticSuite extends DatabaseSuite(SemanticdbMode.Fat) {

  denotations(
    """
      |object A {
      |  List.newBuilder[Int].result
      |  List(1).head
      |}""".stripMargin,
    """
      |_empty_.A. => final object A
      |_root_.scala.Int# => abstract final class Int
      |_root_.scala.Int#`<init>`()V. => primaryctor <init>: ()Int
      |_root_.scala.collection.IterableLike#head()Ljava/lang/Object;. => def head: A
      |_root_.scala.collection.immutable.List. => final object List
      |_root_.scala.collection.immutable.List.newBuilder()Lscala/collection/mutable/Builder;. => def newBuilder: [A]=> scala.collection.mutable.Builder[A,List[A]]
      |_root_.scala.collection.mutable.Builder#result()Ljava/lang/Object;. => abstract def result: ()To
    """.stripMargin.trim
  )
}

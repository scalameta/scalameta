package scala.meta.tests
package semanticdb

import scala.meta.internal.semanticdb.scalac._

class AnnotationsSuite extends DatabaseSuite(SemanticdbMode.Slim) {

  annotations(
    """
      |// Type annotation
      |class A[@specialized(Int, Long) T](v: T)
      |
      |// Class annotation
      |@deprecated("Use D", "1.0") class B { }
      |
      |object C {
      |  // Variable annotation
      |  @transient @volatile var m: Int = 0
      |
      |  // Expression annotation
      |  def f(x: Option[String]) = (x: @unchecked) match { case Some(y) => y }
      |}
    """.stripMargin,
    """
      |_root_.scala.volatile#{
      |  _root_.scala.annotation.meta.field#
      |}
      |_empty_.A#[T]{
      |  _root_.scala.specialized#
      |}
      |_root_.scala.Option#{
      |  _root_.scala.SerialVersionUID#
      |}
      |_empty_.B#{
      |  _root_.scala.deprecated#
      |}
      |_root_.scala.transient#{
      |  _root_.scala.annotation.meta.field#
      |}
      |_root_.scala.deprecated#{
      |  _root_.scala.annotation.meta.getter#
      |  _root_.scala.annotation.meta.setter#
      |  _root_.scala.annotation.meta.beanGetter#
      |  _root_.scala.annotation.meta.beanSetter#
      |}
    """.stripMargin
  )

  targeted(
    "class A[@specialized(Int, Long) <<T>>](v: T)", 
    { (db, tparam) =>
      val symbol = db.symbols.find(_.symbol == tparam).get
      val denotation = symbol.denotation
      assertNoDiff(
        symbol.syntax,
        """|_empty_.A#[T] => typeparam T
           |  @_root_.scala.specialized#""".stripMargin
      )
      assertNoDiff(
        denotation.syntax,
        """|typeparam T
           |  @_root_.scala.specialized#""".stripMargin
      )
      assert(denotation.isSpecialized)
    }
  )

  targeted(
    "class A[<<T>>](v: T)", 
    { (db, tparam) =>
      val symbol = db.symbols.find(_.symbol == tparam).get
      val denotation = symbol.denotation
      assert(!denotation.isSpecialized)
    }
  )
}

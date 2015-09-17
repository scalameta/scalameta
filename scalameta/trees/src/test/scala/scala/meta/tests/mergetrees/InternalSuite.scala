package scala.meta.tests
package mergetrees

import org.scalatest._
import scala.meta.ui.api._
import scala.meta.internal.semantic._
import scala.meta.internal.semantic.RuntimeConverters._
import scala.meta.internal.ui.Attributes

class InternalSuite extends FunSuite {
  test("RuntimeConverters work correctly in weird classloader configurations") {
    val Denotation.Single(Prefix.Type(tpe), _) = denot(typeOf[Object].member(u.TermName("<init>")))
    assert(tpe.show[Attributes] === """
      |Type.Singleton(Term.Name("lang")[1]{1}<>)
      |[1] {2}::java.lang
      |[2] {3}::java
      |[3] {0}::_root_
      |{1} Type.Singleton(Term.Name("lang")[1]{1}<>)
      |{2} Type.Singleton(Term.Name("java")[2]{2}<>)
      |{3} Type.Singleton(Term.Name("_root_")[3]{3}<>)
    """.trim.stripMargin)
  }
}

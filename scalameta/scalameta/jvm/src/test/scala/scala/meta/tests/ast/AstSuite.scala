package scala.meta.tests
package ast

import org.scalatest._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

class AstSuite extends FunSuite {
  object AstReflection extends {
    val u: ru.type = ru
    val mirror: u.Mirror = u.runtimeMirror(classOf[scala.meta.Tree].getClassLoader)
  } with scala.meta.internal.ast.Reflection
  def symbolOf[T: TypeTag]: TypeSymbol = ru.symbolOf[T]
}

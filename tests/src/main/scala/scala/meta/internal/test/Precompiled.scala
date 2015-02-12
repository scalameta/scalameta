package scala.meta.interpreter.internal.test

abstract class PrecompiledAbstractClass {

  def abstractMethod(x: PrecompiledAbstractClass): PrecompiledAbstractClass

  def methodThatCallsTheAbstractMethod(x: PrecompiledAbstractClass): PrecompiledAbstractClass =
    abstractMethod(x)

}

trait PrecompiledTrait {
  def interfaceMethod(x: PrecompiledTrait): PrecompiledTrait
}

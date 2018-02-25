package scala.meta.internal.metacp.asm

/** Helper to abstract over SignatureVisitors that produce a result of type T */
abstract class TypedSignatureVisitor[+T] extends FailFastSignatureVisitor {
  def result(): T
}

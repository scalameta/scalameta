package scala.meta
package internal
package ffi

import org.scalameta.adt._
import org.scalameta.invariants._

@root trait Ffi
object Ffi {
  @leaf object Zero extends Ffi
  @leaf class Intrinsic(className: String, methodName: String, signature: String) extends Ffi
  @leaf class JvmField(className: String, fieldName: String, signature: String) extends Ffi
  @leaf class JvmMethod(className: String, fieldName: String, signature: String) extends Ffi
  @leaf class JvmErasure(className: String) extends Ffi
  @leaf class JvmPackage(packageName: String) extends Ffi
}
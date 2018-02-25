package scala.meta.internal.metacp.asm

import java.util
import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.util.control.NoStackTrace

/** Utility to resolve abstract type variables in Java signatures to fully qualified symbols.
  *
  * Type variables are not fully resolves in generic java signatures.
  * For example, `A` and `B` in `Map<A, B>` below need to be resolved to
  * `Outer#[A]` and `Outer#move(Map).[B]` respectively.
  *
  * {{{
  *   class Outer<A> {
  *     void <B> move(Map<A, B> map) { }
  *   }
  * }}}
  *
  * @param owners Map from symbols to their owners.
  * @param scopes Map from symbols to the bindings in introduced by that symbol.
  */
class TypeVariableScopes(
    owners: util.Map[String, String] = new util.HashMap(),
    scopes: util.Map[String, util.Map[String, String]] = new util.HashMap()
) {

  override def toString: String = {
    s"""TypeVariableScopes(
       |  owners = $owners
       |  scopes = $scopes
       |)""".stripMargin
  }

  private[this] var myOwner: String = _

  /** Returns the latest entered owner */
  def owner: String = myOwner

  /** Register a binding from name to a symbol in a given owner. */
  def enterBinding(owner: String, name: String, symbol: String): Unit = {
    val scope = scopes.computeIfAbsent(owner, TypeVariableScopes.newTreeMap)
    scope.put(name, symbol)
  }

  /** Register a new current owner symbol along with it's parent. */
  def enterOwner(newOwner: String, newOwnerParent: String): Unit = {
    myOwner = newOwner
    owners.put(newOwner, newOwnerParent)
  }

  def clear(): Unit = {
    owners.clear()
    scopes.clear()
  }

  final def resolve(name: String): String = {
    try {
      resolve(name, owner)
    } catch {
      case e: NoSuchElementException =>
        throw new TypeVariableScopeResolutionError(name, owner, e)
    }
  }

  @tailrec
  final def resolve(name: String, inOwner: String): String = {
    val scope = scopes.computeIfAbsent(inOwner, TypeVariableScopes.newTreeMap)
    val hit = scope.get(name)
    if (hit == null) {
      val next = owners.get(inOwner)
      if (next == null) {
        throw new NoSuchElementException(s"Failed to resolve name '$name' in owner $inOwner")
      }
      resolve(name, next)
    } else {
      hit
    }
  }
}

object TypeVariableScopes {
  import java.util.function
  val newTreeMap: function.Function[String, util.Map[String, String]] =
    new function.Function[String, util.Map[String, String]] {
      override def apply(t: String): util.Map[String, String] = {
        new util.TreeMap()
      }
    }
}

class TypeVariableScopeResolutionError(identifier: String, owner: String, cause: Throwable)
    extends Exception(s"Failed to resolve identifier '$identifier' in owner $owner", cause)
    with NoStackTrace

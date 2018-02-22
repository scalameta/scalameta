package scala.meta.internal.metacp

import java.util
import java.util.NoSuchElementException
import java.util.function
import scala.annotation.tailrec
import scala.util.control.NoStackTrace

case class Binding(name: String, symbol: String)

class Scopes(
    owners: util.Map[String, String] = new util.HashMap(),
    scopes: util.Map[String, util.Map[String, String]] = new util.HashMap()
) {

  override def toString: String = {
    s"""Scopes(
       |  owners = $owners
       |  scopes = $scopes
       |)""".stripMargin
  }

  private[this] var myOwner: String = _

  def owner: String = myOwner

  def registerBinding(owner: String, name: String, symbol: String): Unit = {
    val scope = scopes.computeIfAbsent(owner, Scopes.newTreeMap)
    scope.put(name, symbol)
  }

  def registerOwner(newOwner: String, oldOwner: String): Unit = {
    myOwner = newOwner
    owners.put(newOwner, oldOwner)
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
        throw new IllegalArgumentException(owner, e) with NoStackTrace
    }
  }

  @tailrec
  final def resolve(name: String, inOwner: String): String = {
    val scope = scopes.computeIfAbsent(inOwner, Scopes.newTreeMap)
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

object Scopes {
  val newTreeMap: function.Function[String, util.Map[String, String]] =
    new function.Function[String, util.Map[String, String]] {
      override def apply(t: String): util.Map[String, String] = new util.TreeMap()
    }
}

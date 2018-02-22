package scala.meta.internal.metacp

import scala.annotation.tailrec
import scala.collection.mutable

case class Binding(name: String, symbol: String)

class Scopes(
    owners: mutable.Map[String, String] = mutable.Map.empty,
    scopes: mutable.Map[String, Seq[Binding]] = mutable.Map.empty
) {

  var owner: String = _

  def update(symbol: String, owner: String, bindings: Seq[Binding]): Unit = {
    owners(symbol) = owner
    scopes(symbol) = bindings
  }

  def clear(): Unit = {
    owners.clear()
    scopes.clear()
  }
  final def resolve(name: String): String = {
    resolve(name, this.owner)
  }

  @tailrec
  final def resolve(name: String, owner: String): String = {
    scopes(owner).find(_.name == name) match {
      case Some(hit) => hit.symbol
      case _ => resolve(name, owners(owner))
    }
  }
}

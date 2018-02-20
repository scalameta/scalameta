package scala.meta.internal.metacp

import scala.annotation.tailrec
import scala.collection.mutable

case class Binding(name: String, symbol: String)

case class Scopes(
    owners: mutable.Map[String, String] = mutable.Map.empty,
    scopes: mutable.Map[String, Seq[Binding]] = mutable.Map.empty
) {
  @tailrec
  final def resolve(name: String, owner: String): String = {
    scopes(owner).find(_.name == name) match {
      case Some(hit) => hit.symbol
      case _ => resolve(name, owners(owner))
    }
  }
}

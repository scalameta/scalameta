package scala.meta
package semantic

import org.scalameta.adt._

trait MacroHost extends Host {
  sealed trait Event
  object Event {
    case class Warning(msg: String) extends Event
    case class Error(msg: String) extends Event
    case class Fatal(msg: String) extends Event
  }
  def notify(event: Event): Unit
  def resources: Map[String, Array[Byte]]
}

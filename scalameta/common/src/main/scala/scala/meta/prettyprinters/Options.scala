package scala.meta
package prettyprinters

import org.scalameta.adt._

sealed trait Options {
  def isLazy: Boolean
}

private[meta] trait LowPriorityOptions {
  implicit case object Lazy extends Options {
    def isLazy = true
  }
}

object Options extends LowPriorityOptions {
  implicit case object Eager extends Options {
    def isLazy = false
  }
}

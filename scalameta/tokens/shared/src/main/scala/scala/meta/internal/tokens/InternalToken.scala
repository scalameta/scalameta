package scala.meta
package internal
package tokens

import scala.meta.tokens._

trait InternalToken {
  self: Token =>

  private[meta] def name: String
}

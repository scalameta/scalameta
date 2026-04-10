package scala.meta
package internal
package tokens

import scala.meta.tokens._

trait InternalToken {
  self: Token =>

  def name: String
}

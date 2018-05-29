package scala.meta
package common

trait Optional {
  def isEmpty: Boolean = false
  def nonEmpty: Boolean = !isEmpty
  def isDefined: Boolean = nonEmpty
}

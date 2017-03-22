package scala.meta
package common

// TODO: Figure out whether we should bring more Option[T] methods in.

trait Optional {
  def isEmpty: Boolean = false
  def nonEmpty: Boolean = !isEmpty
  def isDefined: Boolean = nonEmpty
}
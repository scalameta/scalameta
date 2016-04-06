package scala.meta
package internal
package debug

// TODO: we should switch to some real logging framework here
object Debug {
  def log(op: => Unit): Unit = {
    op
  }
}
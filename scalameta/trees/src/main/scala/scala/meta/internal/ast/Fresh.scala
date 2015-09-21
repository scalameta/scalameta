package scala.meta
package internal
package ast

object Fresh {
  private var id = new java.util.concurrent.atomic.AtomicInteger()
  def nextId() = id.incrementAndGet()
}

package scala.meta
package internal
package trees

object Fresh {
  private val id = new java.util.concurrent.atomic.AtomicInteger()
  def nextId() = id.incrementAndGet()
}

package scala.meta
package internal
package trees

object Fresh {
  private var id = new java.util.concurrent.atomic.AtomicInteger()
  def nextId() = id.incrementAndGet()
}

package scala.meta.internal.cli

import java.io._
import java.util.concurrent.atomic._
import scala.collection.GenSeq

case class Job[T](xs: GenSeq[T], stream: PrintStream) {
  def foreach(fn: T => Unit): Unit = {
    val startStamp = System.nanoTime()
    val lastStamp = new AtomicLong(startStamp)
    val n = xs.length
    val i = new AtomicInteger(0)
    xs.foreach { x =>
      fn(x)
      val i1 = i.incrementAndGet()
      val currStamp = System.nanoTime()
      val prevStamp = lastStamp.get()
      val silentSeconds = 1.0 * (currStamp - prevStamp) / 1000000000
      if (silentSeconds > 3.0 && i1 != n) {
        if (lastStamp.compareAndSet(prevStamp, currStamp)) {
          val elapsedSeconds = 1.0 * (currStamp - startStamp) / 1000000000
          val remainingSeconds = elapsedSeconds * (n - i1) / i1
          val remaining = "%.2f".format(remainingSeconds) + "s"
          stream.println(s"($i/$n) $remaining remaining")
        }
      }
    }
    val elapsedSeconds = 1.0 * (System.nanoTime() - startStamp) / 1000000000
    val elapsed = "%.2f".format(elapsedSeconds) + "s"
    stream.println(s"Job finished in $elapsed")
  }
}

package scala.meta.internal.cli

import java.io._
import java.util.concurrent.atomic._

object Job {
  def foreach[T](xs: collection.Seq[T], stream: PrintStream)(fn: T => Unit): Unit = {
    val track = new Track(xs.length, stream)
    xs.foreach { x =>
      fn(x)
      track.afterEach()
    }
    track.afterAll()
  }

  private class Track(n: Int, stream: PrintStream) {
    private val startStamp = System.nanoTime()
    private val lastStamp = new AtomicLong(startStamp)
    private val i = new AtomicInteger(0)

    def afterEach(): Unit = {
      val i1 = i.incrementAndGet()
      val currStamp = System.nanoTime()
      val prevStamp = lastStamp.get()
      val silentSeconds = 1.0 * (currStamp - prevStamp) / 1000000000
      if (silentSeconds > 3.0 && i1 != n) if (lastStamp.compareAndSet(prevStamp, currStamp)) {
        val elapsedSeconds = 1.0 * (currStamp - startStamp) / 1000000000
        val remainingSeconds = elapsedSeconds * (n - i1) / i1
        val remaining = "%.2f".format(remainingSeconds) + "s"
        stream.println(s"($i/$n) $remaining remaining")
      }
    }

    def afterAll(): Unit = {
      val elapsedSeconds = 1.0 * (System.nanoTime() - startStamp) / 1000000000
      val elapsed = "%.2f".format(elapsedSeconds) + "s"
      stream.println(s"Job finished in $elapsed")
    }
  }

}

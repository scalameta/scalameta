package scala.meta.internal.io

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object PlatformExecutionContexts {

  /**
   * Create a non-empty pool of non-daemon threads.
   *
   * @param useQueue
   *   if not set, wait until a thread is available, instead of queueing
   * @param namePrefix
   *   if set, use specified prefix for thread names
   */
  def createContext(
      threads: Int,
      useQueue: Boolean = true,
      namePrefix: Option[String] = None,
  ): ExecutionContextExecutorService = {
    val size = math.max(threads, 1)
    val queue =
      if (!useQueue) new SynchronousQueue[Runnable]() {
        override def offer(e: Runnable): Boolean = {
          put(e) // blocks
          true
        }
      }
      else new LinkedBlockingQueue[Runnable]()
    val factory = namePrefix match {
      case Some(prefix) => namedThreads(prefix)
      case None => Executors.defaultThreadFactory()
    }
    ExecutionContext.fromExecutorService(
      new ThreadPoolExecutor(size, size, 0L, TimeUnit.MILLISECONDS, queue, factory),
    )
  }

  private def namedThreads(name: String): ThreadFactory = new ThreadFactory {
    private val count = new AtomicInteger()
    override def newThread(r: Runnable): Thread = {
      val thread = new Thread(r, s"$name-${count.incrementAndGet()}")
      thread.setDaemon(false)
      thread
    }
  }

}

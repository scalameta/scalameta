package scala.meta.tests.io

import scala.meta.internal.io.{ExecutionContexts, PlatformExecutionContexts}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

import scala.concurrent.Future

import munit.FunSuite

class PlatformExecutionContextsSuite extends FunSuite {

  /** Returns the threads that ran four tasks on `createContext(2)`, after the pool shut down. */
  private def fourTasks(name: Option[String]): List[Thread] = {
    val pool = PlatformExecutionContexts.createContext(2, namePrefix = name)
    val threads = ConcurrentHashMap.newKeySet[Thread]()
    val release = new CountDownLatch(1)
    val timeouts = new AtomicInteger()
    (1 to 4).foreach(_ =>
      pool.execute { () =>
        threads.add(Thread.currentThread())
        // a timeout means that `execute` waited for a free thread
        if (!release.await(1, TimeUnit.MINUTES)) timeouts.incrementAndGet()
      },
    )
    release.countDown()
    pool.shutdown()
    assert(pool.awaitTermination(1, TimeUnit.MINUTES))
    assertEquals(timeouts.get(), 0)
    val obtained = threads.toArray(Array.empty[Thread]).toList
    obtained.foreach(_.join(TimeUnit.MINUTES.toMillis(1)))
    obtained
  }

  test("PlatformExecutionContexts.createContext with two threads and four tasks") {
    val obtained = fourTasks(None)
    assertEquals(
      obtained.map(t => s"daemon=${t.isDaemon} alive=${t.isAlive}"),
      List.fill(2)("daemon=false alive=false"),
    )
  }

  test("PlatformExecutionContexts.createContext with two named threads and four tasks") {
    val obtained = fourTasks(Some("x2"))
    assertEquals(
      obtained.map(t => s"${t.getName} daemon=${t.isDaemon} alive=${t.isAlive}").sorted,
      List("x2-1 daemon=false alive=false", "x2-2 daemon=false alive=false"),
    )
  }

  test("PlatformExecutionContexts.createContext with handoff and one busy thread") {
    val pool = PlatformExecutionContexts.createContext(1, useQueue = false)
    val release = new CountDownLatch(1)
    pool.execute(() => release.await())
    val producer = new Thread(() => pool.execute(() => ()))
    producer.start()
    val state = Iterator.continually {
      Thread.`yield`()
      producer.getState
    }.find(s => s == Thread.State.WAITING || s == Thread.State.TERMINATED)
    release.countDown()
    producer.join()
    pool.shutdown()
    pool.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS)
    assertEquals(state, Some(Thread.State.WAITING))
  }

  test("PlatformExecutionContexts.createContext with no threads") {
    val pool = PlatformExecutionContexts.createContext(0)
    val future = Future("ran")(pool)
    pool.shutdown()
    future.map(assertEquals(_, "ran"))(ExecutionContexts.parasitic)
  }

}

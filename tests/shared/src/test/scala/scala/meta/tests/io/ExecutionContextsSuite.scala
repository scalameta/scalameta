package scala.meta.tests.io

import scala.meta.internal.io.ExecutionContexts

import munit.FunSuite

class ExecutionContextsSuite extends FunSuite {

  test("ExecutionContexts.parasitic") {
    val caller = Thread.currentThread()
    val ran = new java.util.concurrent.atomic.AtomicReference[Thread]()
    ExecutionContexts.parasitic.execute(() => ran.set(Thread.currentThread()))
    assertEquals(ran.get(), caller)
  }

}

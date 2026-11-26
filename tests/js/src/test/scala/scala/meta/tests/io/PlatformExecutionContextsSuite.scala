package scala.meta.tests.io

import scala.meta.internal.io.PlatformExecutionContexts

import scala.scalajs.concurrent.JSExecutionContext

import munit.FunSuite

class PlatformExecutionContextsSuite extends FunSuite {

  test("PlatformExecutionContexts.createContext with four threads and handoff")(assertEquals(
    PlatformExecutionContexts.createContext(4, useQueue = false),
    JSExecutionContext.queue,
  ))

}

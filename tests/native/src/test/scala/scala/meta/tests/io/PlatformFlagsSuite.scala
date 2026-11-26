package scala.meta.tests.io

import scala.meta.internal.platform._

import munit.FunSuite

class PlatformFlagsSuite extends FunSuite {

  test("isJS")(assertEquals(isJS, false))
  test("isJVM")(assertEquals(isJVM, false))
  test("isNative")(assertEquals(isNative, true))

}

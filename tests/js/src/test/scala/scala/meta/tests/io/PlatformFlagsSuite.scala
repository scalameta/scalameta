package scala.meta.tests.io

import scala.meta.internal.io.PlatformFlags._

import munit.FunSuite

class PlatformFlagsSuite extends FunSuite {

  test("isJS")(assertEquals(isJS, true))
  test("isJVM")(assertEquals(isJVM, false))
  test("isNative")(assertEquals(isNative, false))
  test("isNativeOnWindows")(assertEquals(isNativeOnWindows, false))

}

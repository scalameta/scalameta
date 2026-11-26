package scala.meta.tests.io

import scala.meta.internal.io.PlatformFlags._

import java.io.File

import munit.FunSuite

class PlatformFlagsSuite extends FunSuite {

  test("isJS")(assertEquals(isJS, false))
  test("isJVM")(assertEquals(isJVM, false))
  test("isNative")(assertEquals(isNative, true))
  test("isNativeOnWindows")(assertEquals(isNativeOnWindows, File.separatorChar == '\\'))

}

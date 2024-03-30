package scala.meta.tests
package io

import scala.meta._

import java.io.File

import munit.FunSuite

class MultipathSuite extends FunSuite {
  val tmp: File = File.createTempFile("src", "main")
  assert(tmp.delete())
  assert(tmp.mkdirs())
  val files = List("a", "b")
  files.foreach(file => assert(new File(tmp, file).createNewFile()))

  test("Classpath.syntax") {
    val fromPath = Classpath(List(AbsolutePath(tmp)))
    val fromSyntax = Classpath(tmp.getAbsolutePath)
    assertEquals(fromPath, fromSyntax)
    assertEquals(fromPath.syntax, fromSyntax.syntax)
  }
}

package scala.meta.tests
package io

import java.io.File
import org.scalatest.FunSuite
import scala.meta._

class MultipathSuite extends FunSuite {
  val tmp: File = File.createTempFile("src", "main")
  assert(tmp.delete())
  assert(tmp.mkdirs())
  val files = List("a", "b")
  files.foreach(file => {
    assert(new File(tmp, file).createNewFile())
  })

  test("Classpath.syntax") {
    val fromPath = Classpath(List(AbsolutePath(tmp)))
    val fromSyntax = Classpath(tmp.getAbsolutePath)
    assert(fromPath == fromSyntax)
    assert(fromPath.syntax == fromSyntax.syntax)
  }
}

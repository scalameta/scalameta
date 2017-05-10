package scala.meta.tests.io

import scala.meta._

import java.io.File
import java.net.URI

import org.scalatest.FunSuite

class MultipathSuite extends FunSuite {
  val tmp = File.createTempFile("src", "main")
  assert(tmp.delete())
  assert(tmp.mkdirs())
  val files = List("a", "b")
  files.foreach(file => {
    assert(new File(tmp, file).createNewFile())
  })
  val multipath = Sourcepath(tmp.getAbsolutePath)

  test("Multipath.deep") {
    val obtained = multipath.deep.map(_.syntax)
    val expected = files.map(file => Fragment(AbsolutePath(tmp), RelativePath(file))).map(_.syntax)
    assert(obtained.sorted == expected.sorted)
  }

  test("Multipath.find") {
    assert(multipath.find(RelativePath("a")).nonEmpty)
  }

  test("Multipath.relativize") {
    assert(multipath.relativize(new URI("blah")).isEmpty)
    files.foreach { file =>
      assert(multipath.relativize(new File(tmp, file).toURI).get.value == file)
    }
  }
}

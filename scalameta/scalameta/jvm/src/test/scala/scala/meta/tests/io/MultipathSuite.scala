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

  test("Sourcepath.syntax") {
    assert(Sourcepath(".").syntax == "\".\"")
    val fromPath = Sourcepath(Seq(AbsolutePath(tmp)))
    val fromSyntax = Sourcepath(tmp.getAbsolutePath)
    assert(fromPath == fromSyntax)
    assert(fromPath.syntax == fromSyntax.syntax)
  }

  test("Classpath.syntax") {
    assert(Classpath("/Foo/Bar.jar").syntax == "/Foo/Bar.jar")
    val fromPath = Classpath(Seq(AbsolutePath(tmp)))
    val fromSyntax = Classpath(tmp.getAbsolutePath)
    assert(fromPath == fromSyntax)
    assert(fromPath.syntax == fromSyntax.syntax)
  }
}

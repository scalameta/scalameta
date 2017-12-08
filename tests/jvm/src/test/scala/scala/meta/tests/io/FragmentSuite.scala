package scala.meta.tests.io

import scala.meta._

import java.io.File

import org.scalatest.FunSuite

class FragmentSuite extends FunSuite {
  test("Fragment.normalize") {
    val obtained =
      Fragment(AbsolutePath(new File(".", "a")), RelativePath(new File(".."))).uri.toString
    val expected = new File(".").getAbsoluteFile.toURI.normalize().toString
    assert(obtained == expected)
  }

  test("Fragment.normalize jar") {
    val obtained =
      Fragment(AbsolutePath(new File(".", "foo.jar")), RelativePath(new File("MANIFEST"))).uri.toString
    assert(obtained.contains("!/"))
  }
}

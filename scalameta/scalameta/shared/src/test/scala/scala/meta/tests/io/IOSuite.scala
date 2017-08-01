package scala.meta.tests.io

import scala.meta.inputs.Input
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath
import scala.meta.io.RelativePath
import org.scalatest.FunSuite

class IOSuite extends FunSuite {
  val buildSbt = RelativePath("build.sbt").toAbsolute

  test("PathIO.workingDirectory") {
    val obtained = PathIO.workingDirectory.toString
    // assuming we never run tests from root directory, check that the
    // returned value is not the default value "/" when running outside node.
    assert(obtained != "/")
  }

  test("PathIO.isAbsolute") {
    assert(PathIO.isAbsolutePath(PathIO.workingDirectory.toString))
  }

  test("FileIO.listFiles(Directory)") {
    val obtained = FileIO.listFiles(PathIO.workingDirectory)
    assert(obtained.contains(buildSbt))
    assert(!obtained.contains("."))
  }

  test("FileIO.listFiles(File)") {
    assert(FileIO.listFiles(buildSbt).isEmpty)
  }

  test("FileIO.listAllFilesRecursively") {
    val resources = PathIO.workingDirectory.resolve("readme").resolve("resources")
    val obtained = FileIO.listAllFilesRecursively(resources)
    val semanticTooling =
      resources.resolve("talks").resolve("2017-04-21-SemanticToolingAtTwitter.pdf")
    assert(obtained.contains(semanticTooling))
  }

  test("FileIO.readAllBytes") {
    val obtained = new String(FileIO.readAllBytes(buildSbt))
    assert(obtained.contains("project"))
  }

  test("Input.File.slurp") {
    val obtained = new String(Input.File(buildSbt).chars)
    assert(obtained.contains("project"))
  }

  test("AbsolutePath(relpath)(customCwd)") {
    implicit val customWorkingDirectory = AbsolutePath.root
    val obtained = AbsolutePath("foo")
    assert(obtained == customWorkingDirectory.resolve("foo"))
  }

}

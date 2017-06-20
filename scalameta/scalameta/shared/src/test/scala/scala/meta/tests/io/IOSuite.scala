package scala.meta.tests.io

import scala.meta.inputs.Input
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath
import scala.meta.io.RelativePath
import org.scalameta.logger
import org.scalatest.FunSuite

class IOSuite extends FunSuite {
  val buildSbt = RelativePath("build.sbt").toAbsolute

  test("PathIO.workingDirectory") {
    val obtained = PathIO.workingDirectory.toString
    // assuming we never run tests from root directory, check that the
    // returned value is not the default value "/" when running outside node.
    assert(obtained != "/")
  }

  test("PathIO.pathSeparator") {
    val obtained = PathIO.pathSeparator
    assert(obtained == ":" || obtained == ";")
  }

  test("PathIO.fileSeparator") {
    val obtained = PathIO.fileSeparator
    assert(obtained == "/" || obtained == "\\")
  }

  test("PathIO.isAbsolute") {
    val obtained = PathIO.isAbsolutePath(PathIO.workingDirectory.toString)
    logger.elem(PathIO.workingDirectory)
    assert(obtained)
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

}

package scala.meta.tests.metacp

import java.nio.file.Files
import scala.meta.internal.metacp._
import scala.meta.tests.cli.BaseCliSuite

class MetacpSuite extends BaseCliSuite {
  private val tmp = Files.createTempDirectory("metacp")
  tmp.toFile.deleteOnExit()
  test("basic") {
    val args = Array[String](
      "-cp",
      scalaLibraryJar,
      "-d",
      tmp.toString
    )
    val settings = Settings.parse(args.toList).get
    val exit = Main.process(settings)
    assert(exit == 0)
  }

}

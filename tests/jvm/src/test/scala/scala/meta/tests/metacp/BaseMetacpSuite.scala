package scala.meta.tests.metacp

import java.io.File
import java.nio.file.Files
import scala.meta.internal.metacp.Main
import scala.meta.internal.metacp.Settings
import scala.meta.tests.cli.BaseCliSuite

abstract class BaseMetacpSuite extends BaseCliSuite {

  private val tmp = Files.createTempDirectory("metacp")
  tmp.toFile.deleteOnExit()

  def check(name: String, classpath: () => String): Unit = {
    test(name) {
      val cp = classpath()
      val args = Array[String](
        "-cp",
        cp,
        "-d",
        tmp.toString
      )
      println(tmp)
      val settings = Settings.parse(args.toList).get
      val exit = Main.process(settings)
      assert(exit == 0)
    }
  }

  def checkLibrary(organization: String, artifact: String, version: String): Unit = {
    check(
      List(organization, artifact, version).mkString(File.pathSeparator), { () =>
        val jars = Jars
          .fetch(organization, artifact, version)
          .filterNot(_.toString.contains("scala-lang"))
        jars.mkString(File.pathSeparator)
      }
    )
  }
}

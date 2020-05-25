package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import java.io.File

class DottyCodebaseParseSuite extends ParseSuite {
  
  test("parse-dotty-codebase") {
    val dottyPath = "/home/kpbochenek/vl/github/kris/dotty/"

    parseDir(new File(dottyPath))
  }

  private def parseDir(d: File): Unit = {
    for (f <- d.listFiles()) {
      if (f.isDirectory()) { parseDir(f) }
      else {
        if (f.getName().endsWith(".scala")) {
          println(s"Testing ${f.getAbsolutePath()}")
          val content = scala.io.Source.fromFile(f)(scala.io.Codec.UTF8).mkString
          source(content)(dialects.Dotty)
        }
      }
    }

  }
}

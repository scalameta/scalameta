package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import java.io.File

class DottyCodebaseSuite extends ParseSuite {

  var files = 0
  var errors: List[String] = List()

  test("parse-dotty-codebase") {
    val dottyPath = "/home/kpbochenek/vl/github/kris/dotty/"

    parseDir(new File(dottyPath))

    println(s"FILES: ${files} / ${errors.length}")
    errors.foreach(println)
  }

  private def parseDir(d: File): Unit = {
    for (f <- d.listFiles()) {
      if (f.isDirectory()) { parseDir(f) }
      else {
        if (f.getName().endsWith(".scala") &&
          !f.getAbsolutePath().contains("dotty/tests/") &&
          !f.getAbsolutePath().contains("streams/test") &&
          !f.getAbsolutePath().contains("target/") &&
          !f.getAbsolutePath().contains("out/bootstrap")) {
          println(s"Testing ${f.getAbsolutePath()}")
          val content = scala.io.Source.fromFile(f)(scala.io.Codec.UTF8).mkString
          files += 1
          try {
          source(content)(dialects.Dotty)
          } catch {
            case e: Exception => 
              throw e
              errors = f.getAbsolutePath() +: errors
          }
        }
      }
    }
  }
}
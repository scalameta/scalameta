package org.scalameta.adt

import java.io.File
import java.nio.file.Files
import scala.meta.{Tree => MetaTree}

object Main {
  def main(args: Array[String]): Unit = {
    println(args.toList)
    args.toList match {
      case path :: Nil if path.endsWith(".scala") =>
        val jfile = new File(path)
        jfile.getParentFile.mkdirs()
        Files.write(
          jfile.toPath,
          generateLifts().getBytes()
        )
    }
  }

  def generateLifts(): String = {
    ExprLiftsGenerate.materializeAst[MetaTree](false).mkString("")
  }
}


package org.scalameta.adt

import scala.meta.{Tree => MetaTree}

import java.io.File
import java.nio.file.Files

object Main {
  def main(args: Array[String]): Unit = args.toList match {
    case path :: Nil if path.endsWith(".scala") =>
      val jfile = new File(path)
      jfile.getParentFile.mkdirs()
      Files.write(jfile.toPath, generateLifts().getBytes())
    case _ => throw new Exception("File path argument expected.")
  }

  def generateLifts(): String = TreeLiftsGenerate.materializeAst[MetaTree](false)
}

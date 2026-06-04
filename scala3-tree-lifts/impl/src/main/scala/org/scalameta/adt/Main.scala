package org.scalameta.adt

import scala.meta.Tree

import java.nio.file.{Files, Path}

import scopt.OptionParser

object Main {
  def main(args: Array[String]): Unit = scoptParser.parse(args, CliOptions()).foreach(_.generate())

  private case class CliOptions(
      dir: Path = null,
      transformers: Option[String] = None,
      traversers: Option[String] = None,
      treelifts: Option[String] = None,
  ) {
    def generate(): Unit = {
      writeFile(treelifts, TreeLiftsGenerate.materializeAst[Tree](false))
      writeFile(transformers, TransversersGenerate.generateTransformerParts())
      writeFile(traversers, TransversersGenerate.generateTraverserParts())
    }

    private def writeFile(pathOpt: Option[String], content: => Iterable[String]): Unit = pathOpt
      .foreach { path =>
        val jfile = dir.resolve(path).toFile
        jfile.getParentFile.mkdirs()
        Files.write(jfile.toPath, content.mkString.getBytes())
      }
  }

  private val scoptParser: OptionParser[CliOptions] = new OptionParser[CliOptions]("scala3-codegen") {
    opt[Path]("dir").action((value, c) => c.copy(dir = value)).required()
    opt[String]("transformers").action((value, c) => c.copy(transformers = Option(value)))
    opt[String]("traversers").action((value, c) => c.copy(traversers = Option(value)))
    opt[String]("treelifts").action((value, c) => c.copy(treelifts = Option(value)))
  }

}

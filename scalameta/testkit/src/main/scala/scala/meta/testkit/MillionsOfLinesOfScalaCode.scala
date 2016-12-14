package scala.meta.testkit

import java.io.File

import geny.Generator

object MillionsOfLinesOfScalaCode {
  // Corpus of ~3 million lines of code, 22 mb compressed.
  // The list of repos is originally taken from fastparse:
  // https://github.com/lihaoyi/fastparse/blob/6cf2cb23cd5f628c4e956d5846228ee4ca988f5c/scalaparse/jvm/src/test/scala/scalaparse/ProjectTests.scala#L63-L115
  val scalafmtCorpus =
    "https://github.com/olafurpg/scalafmt/releases/download/v0.1.4/repos.tar.gz"

  private def downloadReposTar(url: String): Unit = {
    import sys.process._
    // Easier and less awkward than dealing with scala.io.Source
    Seq("wget", url).!
  }

  private def extractReposTar(): Unit = {
    import sys.process._
    Seq("tar", "xf", "repos.tar.gz").!
  }

  /** Downloads the zip file, extracts it and parses into a sequence of [[ScalaFile]].
    * @param corpus http url to zip file. The structure of the extracted directory
    *                   must match
    *                   https://github.com/olafurpg/scala-repos/tree/master/repos
    *                   In particular, each project directory must contain a
    *                   `COMMIT` and `URL` file. These files are used to construct
    *                   links to the source files on Github.
    * @return A generator of [[ScalaFile]]. Use [[Generator.take]] to limit the
    *         size of your experiment.
    */
  def files(corpus: String = scalafmtCorpus): Generator[ScalaFile] = {

    if (!FileOps.getFile("repos.tar.gz").isFile) {
      downloadReposTar(corpus)
    }
    if (!FileOps.getFile("target", "repos").isDirectory) {
      extractReposTar()
    }
    val repos = FileOps.getFile("target", "repos")
    val files = Option(repos.listFiles()).getOrElse {
      throw new IllegalStateException(
        s"${repos.getAbsolutePath} is not a directory! Please delete if it's a file and retry.")
    }
    Generator.fromIterable(files.toIterable).flatMap { repo =>
      val commit = FileOps.readFile(new File(repo, "COMMIT")).trim
      val url = FileOps.readFile(new File(repo, "URL")).trim
      FileOps
        .listFiles(repo)
        .filter(sourceFile => sourceFile.endsWith(".scala"))
        .map { sourceFile =>
          val filename = sourceFile.stripPrefix(repo.getPath)
          ScalaFile(filename.trim, url, commit)
        }
    }
  }
}

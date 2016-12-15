package scala.meta.testkit

import java.io.File

import geny.Generator

object MillionsOfLinesOfScalaCode {

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
    * @param corpus See [[Corpus]].
    * @return A generator of [[ScalaFile]]. Use [[Generator.take]] to limit the
    *         size of your experiment and [[Generator.toBuffer.par]] to run
    *         analysis using all available cores on the machine.
    */
  def files(corpus: Corpus = Corpus.fastparse): Generator[ScalaFile] = {

    if (!FileOps.getFile("repos.tar.gz").isFile) {
      downloadReposTar(corpus.url)
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
        .filter(corpus.filter)
        .map { sourceFile =>
          val filename = sourceFile.stripPrefix(repo.getPath)
          ScalaFile(filename.trim, url, commit)
        }
    }
  }
}

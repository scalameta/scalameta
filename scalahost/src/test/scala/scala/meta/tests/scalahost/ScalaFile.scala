package scala.meta.tests
package scalahost

import java.io.File

import geny.Generator

case class ScalaFile(filename: String, projectUrl: String, commit: String) {

  def rawUrl = {
    val raw = projectUrl.replace("github.com", "raw.githubusercontent.com")
    s"$raw/$commit$filename"
  }

  def read: String = {
    val toRead = new File(FileOps.getFile("target", "repos", repo), filename)
    FileOps.readFile(toRead)
  }
  def githubUrlAtLine(line: Int): String = s"$githubUrl#L$line"

  def githubUrl = s"$projectUrl/blob/$commit$filename"
  def userRepo  = projectUrl.stripPrefix("https://github.com/")
  def repo      = userRepo.split("/")(1)
  def user      = userRepo.split("/")(0)

  override def toString: String = s"""ScalaFile(
                                     |    project: $user
                                     |    github: $githubUrl
                                     |    raw: $rawUrl
                                     |)""".stripMargin
}

object ScalaFile {

  def downloadReposTar(): Unit = {
    import sys.process._
    // Easier and less awkward than dealing with scala.io.Source
    Seq("wget", "https://github.com/olafurpg/scalafmt/releases/download/v0.1.4/repos.tar.gz").!
  }

  def extractReposTar(): Unit = {
    import sys.process._
    Seq("tar", "xf", "repos.tar.gz").!
  }

  def getAll: Generator[ScalaFile] = {
    if (!FileOps.getFile("repos.tar.gz").isFile) {
      downloadReposTar()
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
      val repoPrefix = repo.getPath // + File.pathSeparator
      val commit     = FileOps.readFile(new File(repo, "COMMIT")).trim
      val url        = FileOps.readFile(new File(repo, "URL")).trim
      FileOps
        .listFiles(repo)
        .filter(sourceFile => sourceFile.endsWith(".scala"))
        .map { sourceFile =>
          val filename = sourceFile.stripPrefix(repoPrefix)
          ScalaFile(filename.trim, url, commit)
        }
    }
  }
}

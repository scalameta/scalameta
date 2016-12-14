package scala.meta.testkit

import java.io.File

case class ScalaFile(filename: String, projectUrl: String, commit: String) {

  def rawUrl: String = {
    val raw = projectUrl.replace("github.com", "raw.githubusercontent.com")
    s"$raw/$commit$filename"
  }

  def jFile: File =
    new File(FileOps.getFile("target", "repos", repo), filename)
  def read: String = FileOps.readFile(jFile)
  def githubUrlAtLine(line: Int): String = s"$githubUrl#L$line"
  def githubUrl = s"$projectUrl/blob/$commit$filename"
  def userRepo: String = projectUrl.stripPrefix("https://github.com/")
  def repo: String = userRepo.split("/")(1)
  def user: String = userRepo.split("/")(0)

  override def toString: String = s"""ScalaFile(
                                     |    project: $user
                                     |    github: $githubUrl
                                     |    raw: $rawUrl
                                     |)""".stripMargin
}

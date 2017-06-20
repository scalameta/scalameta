package scala.meta.testkit

import java.io.File

/** A Scala source file taken from some Github repo
  *
  * @param filename Path to the file relative to the "target/repos" root directory.
  * @param projectUrl The url of the Github project containing this source file.
  * @param commit The commit has at where this ScalaFile originates from.
  */
case class CorpusFile(filename: String, projectUrl: String, commit: String) {

  /** Github "raw" url for this ScalaFile */
  def rawUrl: String = {
    val raw = projectUrl.replace("github.com", "raw.githubusercontent.com")
    s"$raw/$commit$filename"
  }

  /** File on local filesystem */
  def jFile: File =
    new File(FileOps.getFile("target", "repos", repo), filename)

  /** Read string contents of this ScalaFile. */
  def read: String = FileOps.readFile(jFile)

  /** Same as [[githubUrl]] but highlighted at a given line-number. */
  def githubUrlAtLine(line: Int): String = link(s"$githubUrl#L${line + 1}")

  /** Github url to non-raw (i.e., syntax highlighted) version of this ScalaFile */
  def githubUrl = s"$projectUrl/blob/$commit$filename"

  /** username/repo pair */
  def userRepo: String = projectUrl.stripPrefix("https://github.com/")

  /** repo in username/repo */
  def repo: String = userRepo.split("/")(1)

  /** username in username/repo */
  def user: String = userRepo.split("/")(0)

  override def toString: String = s"""ScalaFile(
                                     |    project: $user
                                     |    github: $githubUrl
                                     |    raw: $rawUrl
                                     |)""".stripMargin

  private def link(url: String) =
    s"[$repo/${url.replaceFirst(".*/", "")}]($url)"
}

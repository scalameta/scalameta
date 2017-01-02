package scala.meta.testkit

import java.io.File

import geny.Generator

/** A collection of Scala source files to run [[SyntaxAnalysis]].
  *
  * @param url A zip file that matches the following structure:
  *            repos/
  *              project1/
  *                COMMIT // <- git commit hash of project1 snapshot
  *                URL    // <- Github project url
  *                src/main/...
  *                Code.scala
  *              project2/
  *              ...
  *              projectN/
  *            In particular, each project directory must contain a
  *            `COMMIT` and `URL` file. These files are used to construct
  *            links to the source files on Github.
  * @param filter Files that don't match the filter are excluded from the
  *               analysis.
  */
case class Corpus(url: String, filter: String => Boolean)

object Corpus {

  /** Corpus of ~3 million lines of code, 22 mb compressed.
    * The list of repos is originally taken from fastparse:
    * https://github.com/lihaoyi/fastparse/blob/6cf2cb23cd5f628c4e956d5846228ee4ca988f5c/scalaparse/jvm/src/test/scala/scalaparse/ProjectTests.scala#L63-L115
    */
  val fastparse = Corpus(
    url =
      "https://github.com/olafurpg/scalafmt/releases/download/v0.1.4/repos.tar.gz",
    x =>
      !Seq(
        // sbt/sbt
        // Unicode escapes in weird places
        "target/repos/sbt/main-settings/src/main/scala/sbt/std/InputWrapper.scala",
        // uses a package called `macro`
        "target/repos/sbt/sbt/src/sbt-test/source-dependencies/inherited-macros",
        "target/repos/sbt/sbt/src/sbt-test/source-dependencies/macro",
        // ornicar/lila
        "target/repos/lila/modules/lobby/src/main/SocketHandler.scala",
        // scalatest/scalatest
        "target/repos/scalatest/common-test/src/main/scala/org/scalatest/OperatorNames.scala",
        "target/repos/scalatest/scalatest-test/src/test/scala/org/scalatest/OperatorNames.scala",
        // scala/scala
        // This fella seems to make the scalac parser hang (???)
        "target/repos/scala/test/files/neg/t5510.scala",
        // Unicode escapes in weird places
        "target/repos/scala/test/files/neg/t8015-ffb.scala",
        "target/repos/scala/test/files/pos/t389.scala",
        "target/repos/scala/test/files/run/literals.scala",
        "target/repos/scala/test/files/run/t3835.scala",
        // Scalac parser seems to accept this, though it blows up later
        "target/repos/scala/test/files/neg/t8266-invalid-interp.scala",
        "target/repos/scala/test/disabled/",
        "target/repos/scala/test/files/neg/",
        // trailing . after number
        "target/repos/scala/test/files/presentation/infix-completion/src/Snippet.scala",
        // Not sure why this is failing but it's new, and earlier version of Scalaparse fail too
        "target/repos/scala/src/scaladoc/scala/tools/nsc/doc/html/page/Entity.scala",
        "target/repos/scala/src/scaladoc/scala/tools/nsc/doc/html/HtmlPage.scala",
        "target/repos/scala/src/scaladoc/scala/tools/nsc/doc/html/page/Template.scala"
      ).exists(x.startsWith)
  )

  private def downloadReposTar(url: String): Unit = {
    import sys.process._
    // Easier and less awkward than dealing with scala.io.Source
    Seq("wget", url).!
  }

  private def extractReposTar(): Unit = {
    import sys.process._
    Seq("tar", "xf", "repos.tar.gz").!
  }

  /** Downloads the zip file, extracts it and parses into a sequence of [[CorpusFile]].
 *
    * @param corpus See [[Corpus]].
    * @return A generator of [[CorpusFile]]. Use Generator.take to limit the
    *         size of your experiment and Generator.toBuffer.par to run
    *         analysis using all available cores on the machine.
    */
  def files(corpus: Corpus): Generator[CorpusFile] = {

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
          CorpusFile(filename.trim, url, commit)
        }
    }
  }
}

package scala.meta.testkit

import java.io.File
import java.net.URI
import java.net.URL

import org.rauschig.jarchivelib.ArchiverFactory

import sys.process._

/**
 * A collection of Scala source files to run [[SyntaxAnalysis]].
 *
 * @param url
 *   A zip file that matches the structure described below.
 * @param filter
 *   Files that don't match the filter are excluded from the analysis.
 *
 * Here's the structure for `url` files:
 * {{{
 *            repos/
 *              project1/
 *                COMMIT // <- git commit hash of project1 snapshot
 *                URL    // <- Github project url
 *                src/main/...
 *                Code.scala
 *              project2/
 *              ...
 *              projectN/
 * }}}
 * In particular, each project directory must contain a `COMMIT` and `URL` file. These files are
 * used to construct links to the source files on Github.
 */
case class Corpus(url: String, filter: String => Boolean)

object Corpus {

  /**
   * Corpus of ~3 million lines of code, 22 mb compressed. The list of repos is originally taken
   * from fastparse:
   * [[https://github.com/lihaoyi/fastparse/blob/6cf2cb23cd5f628c4e956d5846228ee4ca988f5c/scalaparse/jvm/src/test/scala/scalaparse/ProjectTests.scala#L63-L115]]
   */
  val fastparse = Corpus(
    url = "https://github.com/scalameta/scalafmt/releases/download/v0.1.4/repos.tar.gz",
    x =>
      !List(
        // sbt/sbt
        // Unicode escapes in weird places
        "target/repos/sbt/main-settings/src/main/scala/sbt/std/InputWrapper.scala",
        // uses a package called `macro`
        "target/repos/sbt/sbt/src/sbt-test/source-dependencies/inherited-macros",
        "target/repos/sbt/sbt/src/sbt-test/source-dependencies/macro",
        // ornicar/lila
        "target/repos/lila/modules/lobby/src/main/SocketHandler.scala",
        // scalatest/scalatest
        "target/repos/scalatest/common-test/src/main/scala/munit.OperatorNames.scala",
        "target/repos/scalatest/scalatest-test/src/test/scala/munit.OperatorNames.scala",
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
        "target/repos/scala/src/scaladoc/scala/tools/nsc/doc/html/page/Template.scala",
        // transformer fails with StackOverflow - https://github.com/scalameta/scalameta/issues/2509
        "target/repos/kafka/core/src/main/scala/kafka/server/KafkaConfig.scala"
      ).exists(x.startsWith)
  )

  /** If necessary, downloads and extracts the corpus files */
  private def createReposDir(corpus: Corpus): File = {
    val name = "repos"
    val localDirectory = FileOps.getFile("target", name)
    if (!localDirectory.isDirectory) {
      val localTarball = FileOps.getFile(s"$name.tar.gz")
      if (!localTarball.isFile) downloadReposTar(corpus, destination = localTarball)
      extractReposTar(localTarball, destination = FileOps.workingDirectory)
    }
    FileOps.getFile("target", name)
  }

  private def extractReposTar(tarball: File, destination: File): Unit = Phase
    .run(s"extract $tarball") {
      val archiver = ArchiverFactory.createArchiver("tar", "gz")
      archiver.extract(tarball, destination)
    }

  private def downloadReposTar(corpus: Corpus, destination: File): Unit = Phase
    .run(s"download ${corpus.url}")(new URI(corpus.url).toURL.#>(destination).!!)

  /**
   * Downloads the zip file, extracts it and parses into a list of [[CorpusFile]].
   *
   * @param corpus
   *   See [[Corpus]].
   * @return
   *   An iterator of [[CorpusFile]]. Use Iterator.take to limit the size of your experiment and
   *   Generator.toBuffer.par to run analysis using all available cores on the machine.
   */
  def files(corpus: Corpus): Iterator[CorpusFile] = {
    val repos = createReposDir(corpus)
    val files = Option(repos.listFiles()).getOrElse {
      throw new IllegalStateException(
        s"${repos.getAbsolutePath} is not a directory! Please delete if it's a file and retry."
      )
    }
    files.iterator.flatMap { repo =>
      val commit = FileOps.readFile(new File(repo, "COMMIT")).trim
      val url = FileOps.readFile(new File(repo, "URL")).trim
      FileOps.listFiles(repo).filter(sourceFile => sourceFile.endsWith(".scala"))
        .filter(corpus.filter).map { sourceFile =>
          val filename = sourceFile.stripPrefix(repo.getPath)
          CorpusFile(filename.trim, url, commit)
        }
    }
  }
}

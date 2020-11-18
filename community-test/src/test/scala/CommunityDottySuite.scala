package scala.meta.parser.dotty

import scala.meta._
import scala.meta.dialects.Dotty
import munit.FunSuite

import java.io.File

import sys.process._
import scala.language.postfixOps

import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.Path

class CommunityDottySuite extends FunSuite {

  val communityDirectory = Paths.get("community-projects")

  def fetchCommunityBuild(build: CommunityBuild): Unit = {
    if (!Files.exists(communityDirectory)) Files.createDirectory(communityDirectory)

    val folder = communityDirectory.resolve(build.name).toString
    if (!Files.exists(communityDirectory.resolve(build.name))) {
      val gclone = s"git clone ${build.giturl} ${folder}"
      val gchangecommit = s"""sh -c "cd ${folder} && git checkout ${build.commit} " """

      val result: Int = (gclone #&& gchangecommit) !

      assert(clue(result) == 0, s"Fetching community build ${build.name} failed")
    } else {
      val gchangecommit =
        s"""sh -c "cd ${folder} && git fetch origin && git checkout ${build.commit} " """
      val result: Int = gchangecommit !

      assert(clue(result) == 0, s"Checking out community build ${build.name} failed")
    }
  }

  case class CommunityBuild(giturl: String, commit: String, name: String, excluded: List[String])
  case class TestStats(
      checkedFiles: Int,
      errors: Int,
      lastError: Option[Throwable],
      timeTaken: Long,
      linesParsed: Int
  )

  final val InitTestStats = TestStats(0, 0, None, 0, 0)

  def merger(s1: TestStats, s2: TestStats): TestStats =
    TestStats(
      s1.checkedFiles + s2.checkedFiles,
      s1.errors + s2.errors,
      s1.lastError.orElse(s2.lastError),
      s1.timeTaken + s2.timeTaken,
      s1.linesParsed + s2.linesParsed
    )

  val communityBuilds = List(
    CommunityBuild(
      "https://github.com/lampepfl/dotty.git",
      //commit hash from 17.11.2020
      "cd15c992c1d9e18ebaad65b7bc1bff3eafa4605d",
      "dotty",
      dottyExclusionList
    ),
    CommunityBuild(
      "https://github.com/scalameta/munit.git",
      // latest commit from 27.09.2020
      "9107c110cefd18c1889e11c15b3b308bec74f24c",
      "munit",
      munitExclusionList
    )
  )

  for (build <- communityBuilds) {
    test(s"community-build-${build.name}") {
      check(build)
    }
  }

  def check(implicit build: CommunityBuild): Unit = {
    fetchCommunityBuild(build)

    val stats = checkFilesRecursive(communityDirectory.resolve(build.name))
    val timePer1KLines = stats.timeTaken / (stats.linesParsed / 1000)

    println("--------------------------")
    println(s"Files parsed correctly ${stats.checkedFiles}")
    println(s"Files errored: ${stats.errors}")
    println(s"Time taken: ${stats.timeTaken}ms")
    println(s"Lines parsed: ~${stats.linesParsed / 1000}k")
    println(s"Parsing speed per 1k lines ===> ${timePer1KLines} ms/1klines")
    println("--------------------------")
    stats.lastError.foreach(e => throw e)
  }

  def timeIt(block: => ()): Long = {
    val t0 = System.currentTimeMillis()
    block
    val t1 = System.currentTimeMillis()
    t1 - t0
  }

  def checkFilesRecursive(parent: Path)(implicit build: CommunityBuild): TestStats = {
    if (ignoreParts.exists(p => parent.toAbsolutePath.toString.contains(p)))
      return InitTestStats
    if (Files.isDirectory(parent)) {
      import scala.collection.JavaConverters._
      Files
        .list(parent)
        .map(checkFilesRecursive)
        .iterator()
        .asScala
        .fold(InitTestStats)(merger)
    } else {
      if (parent.toAbsolutePath.toString.endsWith(".scala")) {
        checkFile(parent)
      } else InitTestStats
    }
  }

  def checkFile(file: Path)(implicit build: CommunityBuild): TestStats = {
    val fileContent = Input.File(file.toAbsolutePath)
    val lines = fileContent.chars.count(_ == '\n')
    if (excluded(file.toAbsolutePath.toString, build)) {
      try {
        val taken = timeIt {
          fileContent.parse[Source].get
        }
        println("File marked as error but parsed correctly " + file.toAbsolutePath)
        TestStats(1, 1, None, taken, lines)
      } catch {
        case e: Throwable => TestStats(1, 1, None, 0, 0)
      }
    } else {
      try {
        val taken = timeIt {
          fileContent.parse[Source].get
        }
        TestStats(1, 0, None, taken, lines)
      } catch {
        case e: Throwable =>
          println(s"Failed for file ${file.toAbsolutePath}")
          println(s"Error: " + e.getMessage())
          TestStats(1, 1, Some(e), 0, 0)
      }
    }
  }

  def excluded(path: String, build: CommunityBuild): Boolean = {
    build.excluded.exists(el => path.endsWith(el))
  }

  final def dottyExclusionList = List(
    // [scalameta] erased modifier - for now used internally, will be available in 3.1
    "library/src/scala/compiletime/package.scala",
    // most likely will become deprecated: if (cond) <ident>
    "tools/dotc/typer/Implicits.scala",
    "tools/dotc/typer/Checking.scala",
    // if then - else without outdentation before else.
    // it's unlcear what to do in this case
    // https://github.com/lampepfl/dotty/issues/10372
    "dotty/dokka/tasty/ClassLikeSupport.scala",
    // extension will become a keyword, needs fix in dotty
    "dotty/dokka/translators/ScalaSignatureProvider.scala",
    "dotty/tools/io/AbstractFile.scala",
    "dotty/tools/io/JarArchive.scala",
    "dotty/tools/io/Path.scala",
    "dotty/tools/dotc/core/StdNames.scala",
    "dotty/tools/dotc/core/Symbols.scala",
    "dotty/tools/dotc/fromtasty/Debug.scala",
    "dotty/tools/dotc/config/Settings.scala",
    "dotty/tools/dotc/parsing/Parsers.scala",
    "dotty/tools/dotc/sbt/ExtractDependencies.scala",
    "dotty/tools/dotc/core/tasty/CommentPicklingTest.scala",
    "dotty/tools/dotc/printing/PrintingTest.scala",
    "dotty/tools/dotc/transform/PatmatExhaustivityTest.scala",
    "dotty/tools/sbtplugin/DottyPlugin.scala",
    "src/scala/tasty/inspector/TastyInspector.scala"
  )

  final def munitExclusionList = List(
    // Syntax no longer valid in Scala 3
    //xml literals
    "main/scala/docs/MUnitModifier.scala",
    // old given syntax
    "main/scala-3/munit/internal/MacroCompat.scala"
  )

  final val ignoreParts = List(
    "/tests/",
    "/sbt-test/",
    "/out/",
    "/language-server/src/dotty/"
  )
}

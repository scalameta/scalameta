package scala.meta.parser.dotty

import scala.meta._

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.concurrent.duration
import scala.language.postfixOps

import munit.FunSuite
import sys.process._

class CommunityDottySuite extends FunSuite {

  override val munitTimeout = new duration.FiniteDuration(3, duration.MINUTES)

  val communityDirectory = Paths.get("community-projects")

  def fetchCommunityBuild(build: CommunityBuild): Path = {
    if (!Files.exists(communityDirectory)) Files.createDirectory(communityDirectory)

    val folderPath = communityDirectory.resolve(build.name)
    val folder = folderPath.toString
    if (!Files.exists(folderPath)) {
      val gclone = s"git clone --depth=1 --no-single-branch ${build.giturl} $folder"
      val result: Int = gclone.!
      assert(clue(result) == 0, s"Fetching community build ${build.name} failed")
    }

    locally {
      val gchangecommit =
        s"""sh -c "cd $folder && git fetch --depth=1 origin ${build.commit} && git checkout ${build.commit} " """
      val result: Int = gchangecommit.!

      assert(clue(result) == 0, s"Checking out community build ${build.name} failed")
    }

    folderPath
  }

  case class CommunityBuild(
      giturl: String,
      commit: String,
      name: String,
      excluded: List[String],
      checkedFiles: Int,
      dialect: Dialect,
      dialectAlt: Option[Dialect] = None
  )
  case class TestStats(
      checkedFiles: Int,
      errors: Int,
      lastError: Option[Throwable],
      timeTaken: Long,
      linesParsed: Int
  )

  final val InitTestStats = TestStats(0, 0, None, 0, 0)

  def merger(s1: TestStats, s2: TestStats): TestStats = TestStats(
    s1.checkedFiles + s2.checkedFiles,
    s1.errors + s2.errors,
    s1.lastError.orElse(s2.lastError),
    s1.timeTaken + s2.timeTaken,
    s1.linesParsed + s2.linesParsed
  )

  val communityBuilds = List(
    dottyBuild("3.0.2", dialects.Scala30, 886),
    dottyBuild("3.1.3", dialects.Scala31, 946),
    dottyBuild("3.2.2", dialects.Scala32, 996),
    dottyBuild("3.3.1-RC1", dialects.Scala33, 1028),
    // spark
    sparkBuild("v1.6.3", dialects.Scala213, 2144),
    sparkBuild("v2.0.2", dialects.Scala213, 2468),
    sparkBuild("v2.1.3", dialects.Scala213, 2611),
    sparkBuild("v2.2.3", dialects.Scala213, 2748),
    sparkBuild("v2.3.3", dialects.Scala213, 2968),
    sparkBuild("v2.4.8", dialects.Scala213, 3165),
    sparkBuild("v3.4.1", dialects.Scala213, 4491),
    // latest commit from 30.03.2021
    munitBuild("06346adfe3519c384201eec531762dad2f4843dc", dialects.Scala213, 102)
  )

  for (build <- communityBuilds) test(s"community-build-${build.name}-${build.commit}")(check(build))

  def check(implicit build: CommunityBuild): Unit = {
    val folder = fetchCommunityBuild(build)

    val stats = checkFilesRecursive(folder.toAbsolutePath).fold(InitTestStats)(merger)
    val timePer1KLines = Math.round(stats.timeTaken / (stats.linesParsed / 1000.0))

    println("--------------------------")
    println(build.name)
    println(s"Files parsed correctly ${stats.checkedFiles - stats.errors}")
    println(s"Files errored: ${stats.errors}")
    println(s"Time taken: ${stats.timeTaken}ms")
    if (stats.linesParsed < 1000) println(s"Lines parsed: ${stats.linesParsed}")
    else println(s"Lines parsed: ~${stats.linesParsed / 1000}k")
    println(s"Parsing speed per 1k lines ===> $timePer1KLines ms/1klines")
    println("--------------------------")
    stats.lastError.foreach(e => throw e)

    assertEquals(stats.errors, 0)
    assertEquals(stats.checkedFiles, build.checkedFiles)
  }

  def timeIt(block: => Unit): Long = {
    val t0 = System.currentTimeMillis()
    block
    val t1 = System.currentTimeMillis()
    t1 - t0
  }

  private def checkFilesRecursive(path: Path)(implicit cb: CommunityBuild): Iterator[TestStats] =
    if (ignoreParts.exists(path.endsWith)) Iterator.empty
    else {
      val ds = Files.newDirectoryStream(path)
      val (dirs, files) =
        try {
          import scala.collection.JavaConverters._
          ds.iterator().asScala.toList.partition(Files.isDirectory(_))
        } finally ds.close()
      files.iterator.flatMap { x =>
        val fileStr = x.toString
        if (fileStr.endsWith(".scala")) Some(checkAbsPath(x, fileStr)) else None
      } ++ dirs.iterator.flatMap(checkFilesRecursive)
    }

  def checkAbsPath(absPath: Path, absPathString: String)(implicit
      build: CommunityBuild
  ): TestStats = {
    val fileContent = Input.File(absPath)
    implicit val dialect: Dialect =
      if (build.dialect.allowSignificantIndentation)
        if (!absPathString.contains("/scala-2")) build.dialect
        else build.dialectAlt.getOrElse(dialects.Scala213)
      else if (!absPathString.contains("/scala-3")) build.dialect
      else build.dialectAlt.getOrElse(dialects.Scala3)
    val lines = fileContent.chars.count(_ == '\n')
    if (excluded(absPathString, build))
      try {
        val taken = timeIt(fileContent.parse[Source].get)
        println("File marked as error but parsed correctly " + absPathString)
        TestStats(1, 1, None, taken, lines)
      } catch { case e: Throwable => TestStats(1, 1, None, 0, 0) }
    else
      try {
        val taken = timeIt(fileContent.parse[Source].get)
        TestStats(1, 0, None, taken, lines)
      } catch {
        case e: Throwable =>
          println(s"Failed for file $absPathString")
          println(s"Error: " + e.getMessage)
          TestStats(1, 1, Some(e), 0, 0)
      }
  }

  def excluded(path: String, build: CommunityBuild): Boolean = build.excluded
    .exists(el => path.endsWith(el))

  private def dottyBuild(ref: String, dialect: Dialect, files: Int): CommunityBuild =
    CommunityBuild("https://github.com/lampepfl/dotty.git", ref, "dotty", Nil, files, dialect)

  private def munitBuild(ref: String, dialect: Dialect, files: Int): CommunityBuild =
    CommunityBuild("https://github.com/scalameta/munit.git", ref, "munit", Nil, files, dialect)

  private def sparkBuild(ref: String, dialect: Dialect, files: Int): CommunityBuild =
    CommunityBuild("https://github.com/apache/spark.git", ref, "spark", Nil, files, dialect)

  final val ignoreParts =
    List(".git/", "tests/", "test-resources/scripting/", "test-resources/repl/", "sbt-test/", "out/")
      .map(Paths.get(_))
}

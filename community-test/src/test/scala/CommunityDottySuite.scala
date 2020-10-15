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

    if (!Files.exists(communityDirectory.resolve(build.name))) {
      val folder = communityDirectory.resolve(build.name).toString
      val gclone = s"git clone ${build.giturl} ${folder}"
      val gchangecommit = s"""sh -c "cd ${folder} && git checkout ${build.commit} " """

      val result: Int = (gclone #&& gchangecommit) !

      assert(clue(result) == 0, s"Fetching community build ${build.name} failed")
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
      //commit hash from 7.10
      "3be62c1cbc3b00fe4411112ffdb2c4ba7da33b03",
      "dotty",
      dottyExclusionList
    ),
    CommunityBuild(
      "https://github.com/scalameta/munit.git",
      // latest commit from 27.09
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
    // 'type T' - as statement in block, rejected by tree check
    "library/src-bootstrapped/scala/quoted/util/ExprMap.scala",
    // '[t] => t => F[t]'  - polymorphic functions - not supported currently
    "library/src-bootstrapped/scala/quoted/unsafe/UnsafeExpr.scala",
    "library/src/scala/runtime/Tuple.scala",
    "library/src/scala/Tuple.scala",
    "src/main/scala/dotty/tools/benchmarks/tuples/TupleOps.scala",
    "src/main/scala/dotty/tools/benchmarks/tuples/Map.scala",
    // erased modifier - for now used internally, will be available in 3.1
    "library/src/scala/compiletime/package.scala",
    // 'val refinTest:  '
    // '  SomeType = X  '  - Type provided in newline - parser needs to handle this
    "input/src/main/scala/example/level2/Documentation.scala",
    // ident.match { ... }
    "tools/dotc/core/tasty/TreePickler.scala",
    "dotty/tools/dotc/core/Types.scala",
    "tools/dotc/core/TypeComparer.scala",
    "/tools/dotc/core/SymDenotations.scala",
    "tools/dotc/core/OrderingConstraint.scala",
    "tools/dotc/ast/tpd.scala", // comment after extension before def

    // for (a, b) <- lst yield ...
    "tools/dotc/transform/BetaReduce.scala",
    "tools/dotc/typer/Checking.scala",
    "compiler/src/dotty/tools/dotc/core/Symbols.scala", // for (tparam ^,^ bound) <- tparams.lazyZip(bounds)
    "compiler/src/dotty/tools/dotc/transform/Splicer.scala",
    // match <indent> case => match <indent> case => (match in match indented)
    "tools/dotc/semanticdb/ExtractSemanticDB.scala",
    "doc-tool/test/dotty/tools/dottydoc/GenDocs.scala", // +: ^"^-project" +: "Dotty"
    // Not fixable
    "tools/dotc/typer/Implicits.scala" // if without then but using significant identation
  )

  final def munitExclusionList = List(
    "main/scala/docs/MUnitModifier.scala" //xml literals
  )

  final val ignoreParts = List(
    "/tests/",
    "/sbt-test/",
    "/out/",
    "/language-server/src/dotty/"
  )
}

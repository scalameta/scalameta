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
      //commit hash from 5.10
      "4b8a1de304f9ec5f0b39d0e69ed9ae8a2f9fb151",
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
    "library/src-bootstrapped/scala/quoted/util/ExprMap.scala", // type T
    "library/src-bootstrapped/scala/quoted/unsafe/UnsafeExpr.scala", //  [t] => Expr[t] => Expr[T1] => Expr[t]
    "library/src/scala/runtime/Tuple.scala", // [t] => t => F[t]
    "library/src/scala/compiletime/package.scala", // erased modifier
    "library/src/scala/Tuple.scala", // [t] => t => F[t]
    "src/main/scala/dotty/tools/benchmarks/tuples/TupleOps.scala", // [A] => A => Tuple
    "src/main/scala/dotty/tools/benchmarks/tuples/Map.scala", // [T] => (x:T) => x
    "input/src/main/scala/example/level2/Documentation.scala", // val refinementTest:  // Type def in newline :/
    "/tools/dotc/core/Annotations.scala", // (Context ?=> Tree) = (using ctx) => bodyFn(using ctx)
    "/tools/dotc/core/Flags.scala", // val (^Private^ @ _, PrivateTerm @ _, PrivateType @ _) = newFlags
    "compiler/src/dotty/tools/dotc/util/LinearSet.scala", // ???
    "/compiler/src/dotty/tools/dotc/util/LinearMap.scala", // ???
    "src/dotty/tools/dotc/semanticdb/Tools.scala", // ???

    // wrong alignment, PR for dotty issued.
    "tools/dotc/quoted/PickledQuotes.scala",
    // ident.match { ... }
    "tools/dotc/core/tasty/TreePickler.scala",
    "dotty/tools/dotc/core/Types.scala",
    "tools/dotc/core/TypeComparer.scala",
    "/tools/dotc/core/SymDenotations.scala",
    "tools/dotc/core/OrderingConstraint.scala",
    "tools/dotc/util/SourceFile.scala", // catch case error => sth ^)^

    // derives keyword
    "/tools/dotc/core/Names.scala",
    "tools/dotc/semanticdb/TextDocuments.scala",
    "tools/dotc/semanticdb/TextDocument.scala",
    "tools/dotc/semanticdb/Range.scala",
    "/tools/dotc/semanticdb/SymbolOccurrence.scala",
    "tools/dotc/semanticdb/Scala3.scala",
    "tools/dotc/semanticdb/Schema.scala",
    "tools/dotc/semanticdb/SymbolInformation.scala",
    "tools/dotc/semanticdb/Language.scala",
    "tools/dotc/ast/tpd.scala", // comment after extension before def
    "tools/dotc/typer/ProtoTypes.scala", // comment after colonEOL
    "tools/dotc/ast/Desugar.scala", // if () indented block, missing then!

    // for (a, b) <- lst yield ...
    "tools/dotc/transform/BetaReduce.scala",
    "tools/dotc/typer/Checking.scala",
    "compiler/src/dotty/tools/dotc/core/Symbols.scala", // for (tparam ^,^ bound) <- tparams.lazyZip(bounds)
    "/tools/dotc/typer/Typer.scala", // case ref @ OrNull(tpnn) ^:^ TermRef
    "compiler/src/dotty/tools/dotc/transform/Splicer.scala",
    // if () block
    "compiler/src/dotty/tools/dotc/typer/Implicits.scala",
    // match <indent> case => match <indent> case => (match in match indented)
    "tools/dotc/semanticdb/ExtractSemanticDB.scala",
    "doc-tool/test/dotty/tools/dottydoc/GenDocs.scala" // +: ^"^-project" +: "Dotty"
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

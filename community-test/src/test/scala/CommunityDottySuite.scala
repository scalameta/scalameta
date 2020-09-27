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

      val result: Int = (gclone #&& gchangecommit)!

      assert(clue(result) == 0, s"Fetching community build ${build.name} failed")
    }
  }

  case class CommunityBuild(giturl: String, commit: String, name: String, excluded: List[String])
  case class TestStats(checkedFiles: Int, errors: Int, lastError: Option[Throwable])

  val communityBuilds = List(
    CommunityBuild(
      "https://github.com/lampepfl/dotty.git",
      //commit hash from 24.09 14:40
      "85d1322c4e8a7254b67dd7b88fa8fdf87b4dac72",
      "dotty",
      dottyExclusionList
    ),
    CommunityBuild(
      "https://github.com/scalameta/munit.git",
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

    println("--------------------------")
    println(s"Files parsed correctly ${stats.checkedFiles}")
    println(s"Files errored: ${stats.errors}")
    println("--------------------------")
    stats.lastError.foreach(e => throw e)
  }


  def checkFilesRecursive(parent: Path)(implicit build: CommunityBuild): TestStats = {
    def merger(s1: TestStats, s2: TestStats): TestStats =
        TestStats(s1.checkedFiles + s2.checkedFiles, s1.errors + s2.errors, s1.lastError.orElse(s2.lastError))

    if (ignoreParts.exists(p => parent.toAbsolutePath.toString.contains(p))) return TestStats(0, 0, None)
    if (Files.isDirectory(parent)) {
      import scala.collection.JavaConverters._
      Files.list(parent)
        .map(checkFilesRecursive).iterator().asScala
        .fold(TestStats(0, 0, None))(merger)
    } else {
      if (parent.toAbsolutePath.toString.endsWith(".scala")) {
        checkFile(parent)
      } else TestStats(0, 0, None)
    }
  }

  def checkFile(file: Path)(implicit build: CommunityBuild): TestStats = {
    if (excluded(file.toAbsolutePath.toString, build)) {
      try {
        Input.File(file.toAbsolutePath).parse[Source].get.structure
        println("File marked as error but parsed correctly " + file.toAbsolutePath)
        TestStats(1, 1, None)
      } catch {
        case e: Throwable => TestStats(1, 1, None)
      }
    } else {
      try {
        Input.File(file.toAbsolutePath).parse[Source].get.structure
        TestStats(1, 0, None)
      } catch {
        case e: Throwable =>
          println(s"Failed for file ${file.toAbsolutePath}")
          println(s"Error: " + e.getMessage())
          TestStats(1, 1, Some(e))
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
    "library/src/scala/tasty/Reflection.scala", // given  as ^^TypeTest ???
    "library/src/scala/Tuple.scala", // [t] => t => F[t]
    "src/main/scala/dotty/tools/benchmarks/tuples/TupleOps.scala", // [A] => A => Tuple
    "src/main/scala/dotty/tools/benchmarks/tuples/Map.scala", // [T] => (x:T) => x
    "input/src/main/scala/example/level2/Documentation.scala", // val refinementTest:   (wtf??)
    "tastydoc/src/dotty/tastydoc/comment/WikiParser.scala", // list mkString ""
    "/tools/dotc/reporting/messages.scala", // class TypeMismatch(found: Type, expected: Type, addenda: => String^*^)(using Context)
    "/tools/dotc/core/Annotations.scala", // (Context ?=> Tree) = (using ctx) => bodyFn(using ctx)
    "/tools/dotc/core/Flags.scala", // val (^Private^ @ _, PrivateTerm @ _, PrivateType @ _) = newFlags
    "/tools/dotc/core/classfile/ClassfileParser.scala", // (using ctx: Context) ^=>^ annotType.classSymbol
    "/tools/dotc/core/Symbols.scala", // extension [N <: Name](sym: Symbol { type ThisName = N })^(^using Context)
    "tools/dotc/quoted/PickledQuotes.scala", // if (arg.isTerm) (using qctx: QuoteContext) ^=>^ new .Expr(arg)
    "compiler/src/dotty/tools/dotc/util/LinearSet.scala", // ???
    "/compiler/src/dotty/tools/dotc/util/LinearMap.scala", // ???
    "src/dotty/tools/dotc/semanticdb/Tools.scala", // ???

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

    // @unchecked
    "tools/dotc/transform/ContextFunctionResults.scala",
    "tools/dotc/transform/Erasure.scala",
    // for (a, b) <- lst yield ...
    "tools/dotc/transform/BetaReduce.scala",
    "compiler/src/dotty/tools/dotc/typer/RefChecks.scala",
    "tools/dotc/typer/Checking.scala",
    "tools/dotc/typer/QuotesAndSplices.scala",
    "tools/dotc/typer/ErrorReporting.scala", // for if then yield
    "tools/dotc/ast/Desugar.scala", // for if yield
    "/tools/dotc/typer/Typer.scala", // case ref @ OrNull(tpnn) ^:^ TermRef
    "compiler/src/dotty/tools/dotc/transform/Splicer.scala",
    // if () block
    "compiler/src/dotty/tools/dotc/typer/Implicits.scala",
    // PR waits to be merged
    "compiler/src/dotty/tools/dotc/typer/Applications.scala",
    // match <indent> case => match <indent> case => (match in match indented)
    "tools/dotc/semanticdb/ExtractSemanticDB.scala",
    // test reproduces: using-lambda-method-parameter
    "tools/dotc/core/tasty/TreeUnpickler.scala"
  )

  final def munitExclusionList = List(
    "main/scala/docs/MUnitModifier.scala", //xml literals
  )

  final val ignoreParts = List(
    "/tests/",
    "/sbt-test/",
    "/out/",
    "/doc-tool/test/", // maybe later verify this?
    "/language-server/src/dotty/"
  )
}

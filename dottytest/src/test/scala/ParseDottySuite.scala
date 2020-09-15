package scala.meta.parser.dotty

import scala.meta._
import scala.meta.dialects.Dotty
import munit.FunSuite

import java.io.File

import sys.process._
import scala.language.postfixOps

class ParseDottySuite extends FunSuite {

  // val directoryName = "dotty-codebase"
  // for my local testing
  val directoryName = "/home/kpbochenek/vl/github/kris/dotty/"

  //NOTE(kbochenek): when dotty merges this switch to dotty repo/tag
  val dottyGithubTag = "master"
  val dottyRepo = "https://github.com/kpbochenek/dotty.git"

  def fetchDottyCodebase(): Unit = {
    if (!scala.reflect.io.File(directoryName).exists) {
      println("cloning dotty code to: " + ("pwd" !!))
      s"git clone --depth 1 --branch ${dottyGithubTag} ${dottyRepo} ${directoryName}" !!
    }
  }

  var checkedFiles = 0
  var errors = 0
  var error: Option[Throwable] = None

  test("parse-dotty-suite") {
    fetchDottyCodebase()

    checkFilesRecursive(new File(directoryName))

    println("--------------------------")
    println(s"Files parsed correctly ${checkedFiles}")
    println(s"Files errored: ${errors}")
    println("--------------------------")
    error.foreach(e => throw e)
  }

  def checkFilesRecursive(parent: File): Unit = {
    if (ignoreParts.exists(p => parent.getAbsolutePath().contains(p))) return
    if (parent.isDirectory()) {
      for (f <- parent.listFiles()) checkFilesRecursive(f)
    } else {
      if (parent.getAbsolutePath().endsWith(".scala")) {
        checkFile(parent)
      }
    }
  }

  def checkFile(file: File): Unit = {
    if (excluded(file.getAbsolutePath)) {
      try {
        Input.File(file.getAbsoluteFile()).parse[Source].get.structure
        println("File marked as error but parsed correctly " + file.getAbsolutePath())
      } catch {
        case e: Throwable => errors += 1
      }

    } else {
      try {
        Input.File(file.getAbsoluteFile()).parse[Source].get.structure
        checkedFiles += 1
      } catch {
        case e: Throwable =>
          println(s"Failed for file ${file.getAbsolutePath}")
          println(s"Error: " + e.getMessage())
          errors += 1
          error = Some(e)
      }
    }
  }

  def excluded(path: String): Boolean = {
    excludedList.exists(el => path.endsWith(el))
  }

  final val excludedList = List(
    "library/src-bootstrapped/scala/quoted/util/ExprMap.scala", // type T
    "library/src-bootstrapped/scala/quoted/unsafe/UnsafeExpr.scala", //  [t] => Expr[t] => Expr[T1] => Expr[t]
    "library/src/scala/runtime/Tuple.scala", // [t] => t => F[t]
    "library/src/scala/compiletime/package.scala", // erased modifier
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
    "compiler/src/dotty/tools/dotc/util/LinearSet.scala", // ???
    "/compiler/src/dotty/tools/dotc/util/LinearMap.scala", // ???
    "src/dotty/tools/dotc/semanticdb/Tools.scala", // ???

    // ident.match { ... }
    "tools/dotc/core/tasty/TreePickler.scala",
    "dotty/tools/dotc/core/Types.scala",
    "tools/dotc/core/TypeComparer.scala",
    "/tools/dotc/core/SymDenotations.scala",
    "tools/dotc/core/OrderingConstraint.scala",
    "/tools/dotc/core/Scopes.scala", // error: this expected but indent found
    "tools/dotc/util/SourceFile.scala", // error: this expected but indent found
    "/tools/vulpix/ParallelTesting.scala", // error: this expected but indent found (first stat == this OR block with this)

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
    "dotty/tools/backend/sjs/JSCodeGen.scala", // comment before indent
    "tools/dotc/ast/tpd.scala", // comment after extension before def
    "tools/dotc/typer/ProtoTypes.scala", // comment after colonEOL

    // REPRO: catch inside catch
    "dotty/tools/backend/jvm/GenBCode.scala",
    // @unchecked
    "tools/dotc/transform/ContextFunctionResults.scala",
    "tools/dotc/transform/Erasure.scala",
    // for (a, b) <- lst yield ...
    "tools/dotc/transform/BetaReduce.scala",
    "tools/dotc/typer/Checking.scala",
    "tools/dotc/typer/ErrorReporting.scala", // for if then yield
    "tools/dotc/ast/Desugar.scala", // for if yield
    "/tools/dotc/typer/Typer.scala", // case ref @ OrNull(tpnn) ^:^ TermRef

    // illegal start of statement (???)
    "tools/dotc/parsing/xml/MarkupParserCommon.scala",
    "tools/dotc/parsing/xml/SymbolicXMLBuilder.scala",
    "tools/dotc/semanticdb/ExtractSemanticDB.scala",
    // while multistat do
    "/tools/dotc/parsing/Scanners.scala",
    "tools/dotc/util/HashTable.scala",
    "tools/dotc/core/tasty/TreeUnpickler.scala",
    "tools/dotc/core/tasty/TreeBuffer.scala",
    "tools/dotc/typer/Synthesizer.scala", // ???
    "tools/dotc/typer/RefChecks.scala", // ???
    "tools/dotc/quoted/PickledQuotes.scala", // ???
    "tools/dotc/typer/QuotesAndSplices.scala", // ???
    "tools/dotc/parsing/Parsers.scala", // ???
    "compiler/src/dotty/tools/dotc/util/GenericHashMap.scala",
    "compiler/src/dotty/tools/dotc/util/HashSet.scala",
  )

  final val ignoreParts = List(
    "/tests/",
    "/sbt-test/",
    "/out/",
    "/doc-tool/test/", // maybe later verify this?
    "/language-server/src/dotty/"
  )
}

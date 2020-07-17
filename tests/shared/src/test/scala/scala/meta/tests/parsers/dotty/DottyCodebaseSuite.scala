package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import java.io.File

class DottyCodebaseSuite extends ParseSuite {

  var parsed = 0
  var skipped = 0

  val singlePath = "compiler/src/dotty/tools/dotc/ast/Desugar.scala"
  val dottyPath = "/home/kpbochenek/vl/github/kris/dotty/"

  test("parse-dotty-codebase") {
    parseDir(new File(dottyPath))

    println(s"FILES: ${parsed + skipped} / parsed: ${parsed} / skipped: ${skipped}")
  }

  test("parse-single-file".ignore) {
    val f = dottyPath + singlePath
    for (fff <- needWorkFiles) {
      try {
        val fpath = dottyPath + fff
        val content = scala.io.Source.fromFile(fpath)(scala.io.Codec.UTF8).mkString
        source(content)(dialects.Dotty)
        println(s"!!!!!!!!!!!! ${fff}")
      } catch {
        case e: Throwable => println(s"FAILED ${fff}")
      }
    }
  }

  private def parseDir(d: File): Unit = {
    for (f <- d.listFiles()) {
      if (f.isDirectory()) { parseDir(f) }
      else {
        if (f.getName().endsWith(".scala") &&
          !f.getAbsolutePath().contains("dotty/tests/") &&
          !f.getAbsolutePath().contains("streams/test") &&
          !f.getAbsolutePath().contains("sbt-dotty/sbt-test") &&
          !f.getAbsolutePath().contains("target/") &&
          !f.getAbsolutePath().contains("out/bootstrap")) {
            if (!needWorkFiles.exists(name => f.getAbsolutePath().contains(name))) {
              println(s"Testing ${f.getAbsolutePath()}")
              val content = scala.io.Source.fromFile(f)(scala.io.Codec.UTF8).mkString
              source(content)(dialects.Dotty)
              parsed += 1
            } else {
              println(s"Skipping ${f.getAbsolutePath()}")
              skipped += 1
            }
        }
      }
    }
  }

  val needWorkFiles = List(
    "bench-run/src/main/scala/dotty/tools/benchmarks/tuples/Map.scala",
    "bench-run/src/main/scala/dotty/tools/benchmarks/tuples/TupleOps.scala",
    "community-build/test/scala/dotty/communitybuild/CommunityBuildTest.scala",
    "compiler/src/dotty/tools/backend/jvm/BTypesFromSymbols.scala",
    "compiler/src/dotty/tools/dotc/Compiler.scala",
    "compiler/src/dotty/tools/dotc/ast/Desugar.scala",
    "compiler/src/dotty/tools/dotc/core/ConstraintHandling.scala",
    "compiler/src/dotty/tools/dotc/core/Decorators.scala",
    "compiler/src/dotty/tools/dotc/core/Flags.scala",
    "compiler/src/dotty/tools/dotc/core/OrderingConstraint.scala",
    "compiler/src/dotty/tools/dotc/core/Signature.scala",
    "compiler/src/dotty/tools/dotc/core/SymDenotations.scala",
    "compiler/src/dotty/tools/dotc/core/SymbolLoaders.scala",
    "compiler/src/dotty/tools/dotc/core/Symbols.scala",
    "compiler/src/dotty/tools/dotc/core/Types.scala",
    "compiler/src/dotty/tools/dotc/core/TypeComparer.scala",
    "compiler/src/dotty/tools/dotc/core/classfile/ClassfileParser.scala",
    "compiler/src/dotty/tools/dotc/core/quoted/PickledQuotes.scala",
    "compiler/src/dotty/tools/dotc/core/tasty/TreePickler.scala",
    "compiler/src/dotty/tools/dotc/core/tasty/TreeUnpickler.scala",
    "compiler/src/dotty/tools/dotc/core/unpickleScala2/Scala2Unpickler.scala",
    "compiler/src/dotty/tools/dotc/parsing/Parsers.scala",
    "compiler/src/dotty/tools/dotc/parsing/Scanners.scala",
    "compiler/src/dotty/tools/dotc/parsing/xml/MarkupParserCommon.scala",
    "compiler/src/dotty/tools/dotc/parsing/xml/SymbolicXMLBuilder.scala",
    "compiler/src/dotty/tools/dotc/printing/RefinedPrinter.scala",
    "compiler/src/dotty/tools/dotc/reporting/messages.scala",
    "compiler/src/dotty/tools/dotc/reporting/trace.scala",
    "compiler/src/dotty/tools/dotc/semanticdb/ExtractSemanticDB.scala",
    "compiler/src/dotty/tools/dotc/semanticdb/Scala3.scala",
    "compiler/src/dotty/tools/dotc/semanticdb/Tools.scala",
    "compiler/src/dotty/tools/dotc/transform/BetaReduce.scala",
    "compiler/src/dotty/tools/dotc/transform/CompleteJavaEnums.scala",
    "compiler/src/dotty/tools/dotc/transform/ContextFunctionResults.scala",
    "compiler/src/dotty/tools/dotc/transform/Erasure.scala",
    "compiler/src/dotty/tools/dotc/transform/Getters.scala",
    "compiler/src/dotty/tools/dotc/transform/Mixin.scala",
    "compiler/src/dotty/tools/dotc/transform/Splicer.scala",
    "compiler/src/dotty/tools/dotc/transform/init/Env.scala",
    "compiler/src/dotty/tools/dotc/typer/Applications.scala",
    "compiler/src/dotty/tools/dotc/typer/Checking.scala",
    "compiler/src/dotty/tools/dotc/typer/Implicits.scala",
    "compiler/src/dotty/tools/dotc/typer/ImportInfo.scala",
    "compiler/src/dotty/tools/dotc/typer/Inliner.scala",
    "compiler/src/dotty/tools/dotc/typer/Namer.scala",
    "compiler/src/dotty/tools/dotc/typer/Nullables.scala",
    "compiler/src/dotty/tools/dotc/typer/ProtoTypes.scala",
    "compiler/src/dotty/tools/dotc/typer/QuotesAndSplices.scala",
    "compiler/src/dotty/tools/dotc/typer/RefChecks.scala",
    "compiler/src/dotty/tools/dotc/typer/Synthesizer.scala",
    "compiler/src/dotty/tools/dotc/typer/Typer.scala",
    "compiler/src/dotty/tools/dotc/util/SourceFile.scala",
    "compiler/src/dotty/tools/dotc/transform/DropOuterAccessors.scala",
    "compiler/test/dotty/tools/compilerSupport.scala",
    "compiler/test/dotty/tools/dotc/semanticdb/SemanticdbTests.scala",
    "compiler/test/dotty/tools/repl/ReplTest.scala",
    "compiler/test/dotty/tools/vulpix/ParallelTesting.scala",
    "doc-tool/src/dotty/tools/dottydoc/staticsite/Template.scala",
    "doc-tool/test/dotty/tools/dottydoc/GenDocs.scala",
    "language-server/src/dotty/tools/languageserver/DottyLanguageServer.scala",
    "library/src-bootstrapped/scala/Enum.scala",
    "library/src-bootstrapped/scala/quoted/unsafe/UnsafeExpr.scala",
    "library/src-bootstrapped/scala/quoted/util/ExprMap.scala",
    "library/src-non-bootstrapped/scala/Enum.scala",
    "compiler/src/dotty/tools/dotc/transform/ElimRepeated.scala",
    "library/src/scala/Tuple.scala",
    "library/src/scala/compiletime/package.scala",
    "library/src/scala/runtime/Tuple.scala",
    "tastydoc/input/src/main/scala/example/level2/Documentation.scala",
    "tastydoc/src/dotty/tastydoc/comment/WikiParser.scala",
    "compiler/src/dotty/tools/dotc/typer/ErrorReporting.scala"
  )
}
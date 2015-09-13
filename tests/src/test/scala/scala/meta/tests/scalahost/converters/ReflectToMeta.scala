package scala.meta.tests
package scalahost
package converters

import org.scalatest._
import java.io._
import scala.compat.Platform.EOL
import scala.{meta => m}
import scala.meta.dialects.Scala211
import scala.meta.ui.Syntax
import scala.meta.XtensionInputLike
import scala.meta.internal.ast.mergeTrees
import scala.meta.internal.ast.MergeException
import scala.meta.internal.hosts.scalac.converters.ConvertException
import scala.reflect.internal.Phase
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter

class ReflectToMetaSuite extends FunSuite {
  val g: Global = {
    def fail(msg: String) = sys.error("ReflectToMeta initialization failed: $msg")
    val classpath = System.getProperty("sbt.paths.tests.classes")
    if (classpath == null) fail("-Dsbt.paths.tests.classes is not set")
    val options = "-cp " + classpath
    val args = CommandLineParser.tokenize(options)
    val emptySettings = new Settings(error => fail(s"couldn't apply settings because $error"))
    val reporter = new StoreReporter()
    val command = new CompilerCommand(args, emptySettings)
    val settings = command.settings
    val g = new Global(settings, reporter)
    val run = new g.Run
    g.phase = run.parserPhase
    g.globalPhase = run.parserPhase
    g
  }
  val currentRun = g.currentRun
  import currentRun.{parserPhase, namerPhase, typerPhase}

  class PipelineException(phase: Phase, pos: g.Position, msg: String)
  extends Exception(s"compiler pipeline failed at $phase: $msg at $pos")

  def runPipeline(code: String, phases: Phase*): g.Tree = {
    import g._
    val reporter = new StoreReporter()
    g.reporter = reporter
    val unit = new CompilationUnit(newSourceFile(code, "<ReflectToMeta>"))
    phases.foreach(phase => {
      g.phase = phase
      g.globalPhase = phase
      phase.asInstanceOf[g.GlobalPhase].apply(unit)
      val errors = reporter.infos.filter(_.severity == reporter.ERROR)
      errors.foreach(error => throw new PipelineException(phase, error.pos, error.msg))
    })
    unit.body
  }

  val proxy: m.Proxy[g.type] = m.Proxy[g.type](g)
  import proxy.conversions._

  def scheduleReflectToMetaTest(testDir: File): Unit = {
    def resource(label: String) = testDir.getAbsolutePath + File.separatorChar + label
    def slurp(label: String) = scala.io.Source.fromFile(new File(resource(label))).mkString
    def dump(label: String, content: String) = {
      val w = new BufferedWriter(new FileWriter(resource(label)))
      w.write(content)
      w.close()
    }
    def exists(label: String) = new File(resource(label)).exists
    def delete(label: String) = {
      val f = new File(resource(label))
      if (f.exists) f.delete
    }
    def diff(label: String, content: String): Unit = {
      val expected = if (label == "Original" || label == "Merged") "Original.scala" else label + ".expected"
      val actual = if (label == "Original" || label == "Merged") "Actual.scala" else label + ".actual"
      val actualResult = content
      if (!exists(expected)) dump(expected, "")
      val expectedResult = slurp(expected)
      if (actualResult != expectedResult) {
        dump(actual, actualResult)
        fail(s"$label subtest has failed, see ${resource(actual)} for details")
      }
    }

    val original = "Original.scala"
    if (exists(original)) {
      test(testDir.getName) {
        def fail(msg: String, ex: Exception) = {
          val stackTraceWriter = new StringWriter()
          ex.printStackTrace(new PrintWriter(stackTraceWriter))
          val s_stackTrace = stackTraceWriter.toString()
          super.fail(msg.capitalize + ":" + EOL + s_stackTrace)
        }

        // Subtest #1: Making sure that scala.meta parse > prettyprint is an identity function.
        val moriginalTree = new File(resource(original)).parse[m.Source]
        diff("Original", moriginalTree.show[Syntax])

        // Subtest #2: Making sure that scala.reflect parse > scala.meta convert works as expected.
        // NOTE: Prettyprint of a converted tree won't be equal to the contents Original.scala,
        // because scala.reflect's parser performs desugarings, and our converter can't undo some of them.
        val code = slurp("Original.scala")
        val gsyntacticTree = {
          try runPipeline(code, parserPhase)
          catch { case ex: PipelineException => fail("error parsing Original.scala", ex) }
        }
        val msyntacticTree = {
          try gsyntacticTree.toMeta
          catch { case ex: ConvertException => fail("error converting syntactic Original.scala", ex) }
        }
        diff("Syntactic", msyntacticTree.show[Syntax])

        // Subtest #3: Making sure that scala.reflect parse > scala.reflect typecheck > scala.meta convert works as expected.
        val gsemanticTree = {
          try runPipeline(code, parserPhase, namerPhase, typerPhase)
          catch { case ex: PipelineException => fail("error typechecking Original.scala", ex) }
        }
        val msemanticTree = {
          try gsemanticTree.toMeta
          catch { case ex: ConvertException => fail("error converting semantic Original.scala", ex) }
        }
        require(msemanticTree.isTypechecked)
        diff("Semantic", msemanticTree.show[Syntax])

        // Subtest #4: Making sure that merging the results of subtest #1 and subtest #3 works as expected.
        // NOTE: We can't merge the results of #2 and #3, because #2, having been obtained from a desugared tree,
        // is not a faithful representation of the source code (something which is expected from the 1st argument to mergeTrees).
        val mmergedTree = {
          try mergeTrees(moriginalTree.asInstanceOf[scala.meta.internal.ast.Tree], msemanticTree.asInstanceOf[scala.meta.internal.ast.Tree])
          catch { case ex: MergeException => fail("error merging syntactic and semantic Original.scala", ex) }
        }
        require(mmergedTree.isTypechecked)
        diff("Merged", mmergedTree.show[Syntax])
      }
    } else {
      ignore(testDir.getName){}
    }
  }

  val resourceDir = new File(System.getProperty("sbt.paths.tests.resources"))
  if (!resourceDir.exists) sys.error("ReflectToMeta initialization failed: -Dsbt.paths.tests.resources is not set")
  val testDirs = resourceDir.listFiles().filter(_.listFiles().nonEmpty)
  testDirs.foreach(scheduleReflectToMetaTest)
}

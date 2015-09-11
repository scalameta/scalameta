import org.scalatest._
import java.io._
import scala.compat.Platform.EOL
import scala.{meta => m}
import scala.meta.dialects.Scala211
import scala.meta.ui.Syntax
import scala.meta.internal.ast.mergeTrees
import scala.meta.internal.ast.MergeException
import scala.meta.internal.hosts.scalac.converters.ConvertException
import scala.reflect.internal.Phase
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter

class ScalaToMeta extends FunSuite {
  val g: Global = {
    def fail(msg: String) = sys.error("ScalaToMeta initialization failed: $msg")
    val classpath = System.getProperty("sbt.paths.tests.classpath")
    if (classpath == null) fail("-Dsbt.paths.tests.classpath is not set")
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
    val unit = new CompilationUnit(newSourceFile(code, "<ScalaToMeta>"))
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

  def scheduleScalaToMetaTest(testDir: File): Unit = {
    def resource(label: String) = testDir.getAbsolutePath + File.separatorChar + label
    def slurp(label: String) = scala.io.Source.fromFile(new File(resource(label))).mkString.trim
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
      val expected = label + ".expected"
      val actual = label + ".actual"
      val actualResult = content
      if (!exists(expected)) dump(expected, "")
      val expectedResult = slurp(expected)
      if (actualResult != expectedResult) {
        dump(actual, actualResult)
        fail(s"see ${resource(actual)} for details")
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
        val gsemanticTree = {
          try runPipeline(code, parserPhase, namerPhase, typerPhase)
          catch { case ex: PipelineException => fail("error typechecking Original.scala", ex) }
        }
        val msemanticTree = {
          try gsemanticTree.toMeta
          catch { case ex: ConvertException => fail("error converting semantic Original.scala", ex) }
        }
        diff("Semantic", msemanticTree.show[Syntax])
        val mmergedTree = {
          try mergeTrees(msyntacticTree.asInstanceOf[scala.meta.internal.ast.Tree], msemanticTree.asInstanceOf[scala.meta.internal.ast.Tree])
          catch { case ex: MergeException => fail("error merging syntactic and semantic Original.scala", ex) }
        }
        diff("Merged", mmergedTree.show[Syntax])
      }
    } else {
      ignore(testDir.getName){}
    }
  }

  val resourceDir = new File(System.getProperty("sbt.paths.tests.resources"))
  if (!resourceDir.exists) sys.error("ScalaToMeta initialization failed: -Dsbt.paths.tests.resources is not set")
  val testDirs = resourceDir.listFiles().filter(_.listFiles().nonEmpty)
  testDirs.foreach(scheduleScalaToMetaTest)
}

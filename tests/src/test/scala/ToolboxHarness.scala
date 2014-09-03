import java.net._
import java.io.File
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.ToolBox
import scala.meta._
import scala.meta.syntactic.parsers._
import scala.meta.syntactic.show._
import scala.meta.internal.hosts.scalacompiler.scalahost.Scalahost
import scala.meta.semantic.{Host => PalladiumHost}
import scala.meta.internal.hosts.scalacompiler.scalahost.{Host => OurHost}

trait ToolboxHarness {
  def typecheckConvertAndPrettyprint(code: String): String = {
    val pluginJar = System.getProperty("sbt.classpaths.package.plugin")
    val compilationClasspath = System.getProperty("sbt.classpaths.test.tests").split(File.pathSeparatorChar.toString).map(path => new URL("file://" + path))
    val classloader = new URLClassLoader(compilationClasspath)
    val mirror = ru.runtimeMirror(classloader)
    val tb = mirror.mkToolBox(options = "-Xplugin:" + pluginJar + " -Xplugin-require:scalahost")
    val tree = tb.parse(code)
    val result: String = null
    def cont(compilerApi: AnyRef): Unit = {
      val m_compiler = compilerApi.getClass.getDeclaredMethod("compiler")
      val compiler = m_compiler.invoke(compilerApi).asInstanceOf[scala.tools.nsc.Global]

      val m_importer = compilerApi.getClass.getDeclaredMethod("importer")
      val importer = m_importer.invoke(compilerApi).asInstanceOf[compiler.Importer { val from: ru.type }]
      val ctree: compiler.Tree = importer.importTree(tree)
      import compiler._
      import analyzer._
      val run = new Run
      phase = run.namerPhase
      globalPhase = run.namerPhase
      val namer = newNamer(rootContext(NoCompilationUnit))
      namer.enterSym(ctree)
      phase = run.typerPhase
      globalPhase = run.typerPhase
      val typer = newTyper(rootContext(NoCompilationUnit))
      val ttree = typer.typed(ctree).asInstanceOf[compiler.Tree]
      println(ttree)

      val h = Scalahost(compiler).asInstanceOf[PalladiumHost with OurHost[compiler.type]]
      val result = ttree match {
        case term: TermTree => h.toPalladium(term, classOf[Term])
        case member: MemberDef => h.toPalladium(member, classOf[Stmt.Template])
        case other => h.toPalladium(other, classOf[Stmt.TopLevel])
      }
      println(result.show[Code])
      println(result.show[Raw])
      ???
    }
    val m_withCompilerApi = tb.getClass.getDeclaredMethod("withCompilerApi")
    val o_withCompilerApi = m_withCompilerApi.invoke(tb)
    val m_apply = o_withCompilerApi.getClass.getDeclaredMethods.find(_.getName == "apply").get
    m_apply.invoke(o_withCompilerApi, cont _)
    result
  }
}

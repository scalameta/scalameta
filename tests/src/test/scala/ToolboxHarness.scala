import org.scalatest._
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

trait ToolboxHarness extends FunSuite {
  private def typecheckConvertAndPrettyprint(code: String): String = {
    val pluginJar = System.getProperty("sbt.classpaths.package.plugin")
    val compilationClasspath = System.getProperty("sbt.classpaths.test.tests").split(File.pathSeparatorChar.toString).map(path => new URL("file://" + path))
    val classloader = new URLClassLoader(compilationClasspath, getClass.getClassLoader)
    val mirror = ru.runtimeMirror(classloader)
    val tb = mirror.mkToolBox(options = "-Xplugin:" + pluginJar + " -Xplugin-require:scalahost")
    var result: String = null
    def cont(compilerApi: AnyRef): Unit = {
      val m_compiler = compilerApi.getClass.getDeclaredMethod("compiler")
      val compiler = m_compiler.invoke(compilerApi).asInstanceOf[scala.tools.nsc.Global]
      import compiler._
      import analyzer._

      val run = new Run
      phase = run.parserPhase
      globalPhase = run.parserPhase
      val unit = compiler.newCompilationUnit(code, "<memory>")
      unit.body = compiler.newUnitParser(unit).parse()
      phase = run.namerPhase
      globalPhase = run.namerPhase
      newNamer(rootContext(unit)).enterSym(unit.body)
      phase = run.phaseNamed("packageobjects")
      globalPhase = run.phaseNamed("packageobjects")
      val openPackageObjectsTraverser = new Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case ModuleDef(_, _, _) =>
            if (tree.symbol.name == nme.PACKAGEkw) {
              openPackageModule(tree.symbol, tree.symbol.owner)
            }
          case ClassDef(_, _, _, _) => () // make it fast
          case _ => super.traverse(tree)
        }
      }
      openPackageObjectsTraverser(unit.body)
      phase = run.typerPhase
      globalPhase = run.typerPhase
      unit.body = newTyper(rootContext(unit)).typed(unit.body).asInstanceOf[compiler.Tree]
      for (workItem <- unit.toCheck) workItem()

      val h = Scalahost(compiler).asInstanceOf[PalladiumHost with OurHost[compiler.type]]
      val ptree = h.toPalladium(unit.body, classOf[Aux.CompUnit])
      result = ptree.show[Code]
    }
    val m_withCompilerApi = tb.getClass.getDeclaredMethod("withCompilerApi")
    val o_withCompilerApi = m_withCompilerApi.invoke(tb)
    val m_apply = o_withCompilerApi.getClass.getDeclaredMethods.find(_.getName == "apply").get
    m_apply.invoke(o_withCompilerApi, cont _)
    result
  }

  def checkScalaToMeta(scalaCodeTemplate: String, metaCodeTemplate: String): Unit = {
    val actualResult = typecheckConvertAndPrettyprint(scalaCodeTemplate.trim.stripMargin.replace("QQQ", "\"\"\""), debug = false)
    val expectedResult = metaCodeTemplate.trim.stripMargin.replace("QQQ", "\"\"\"")
    if (actualResult != expectedResult) {
      typecheckConvertAndPrettyprint(scalaCodeTemplate.trim.stripMargin.replace("QQQ", "\"\"\""), debug = true)
      assert(actualResult === expectedResult)
    }
  }
}

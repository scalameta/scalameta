import org.scalatest._
import java.net._
import java.io.File
import scala.reflect.runtime.{ universe => ru }
import scala.tools.reflect.{ ToolBox, ToolBoxError }
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.syntactic.parsers._
import scala.meta.syntactic.show._
import scala.meta.internal.hosts.scalacompiler.scalahost.Scalahost
import scala.meta.semantic.{ Host => PalladiumHost }
import scala.meta.internal.hosts.scalacompiler.scalahost.{ Host => OurHost }
class ScalaToMeta extends FunSuite {
  def typecheckConvertAndPrettyprint(code: String, debug: Boolean): String = {
    val pluginJar = System.getProperty("sbt.paths.plugin.jar")
    val compilationClasspath = System.getProperty("sbt.paths.tests.classpath").split(File.pathSeparatorChar.toString()).map(path => new URL("file://" + path))
    val classloader = new URLClassLoader(compilationClasspath, getClass().getClassLoader())
    val mirror = ru.runtimeMirror(classloader)
    val tb = mirror.mkToolBox(options = "-cp " + System.getProperty("sbt.paths.tests.classpath") + " -Xplugin:" + pluginJar + " -Xplugin-require:scalahost")
    var result: String = null
    def cont(compilerApi: AnyRef): Unit = {
      val m_compiler = compilerApi.getClass().getDeclaredMethod("compiler")
      val compiler = m_compiler.invoke(compilerApi).asInstanceOf[scala.tools.nsc.Global]
      import compiler._
      import analyzer._
      reporter.reset()
      val m_frontEnd = tb.getClass().getDeclaredMethod("frontEnd")
      val frontEnd = m_frontEnd.invoke(tb).asInstanceOf[scala.tools.reflect.FrontEnd]
      frontEnd.reset()
      def throwIfErrors(): Unit = if (frontEnd.hasErrors) throw ToolBoxError("reflective compilation has failed:" + EOL + EOL + frontEnd.infos.map(_ => x$1.msg).mkString(EOL))
      val run = new compiler.Run
      compiler phase_= run.parserPhase
      compiler globalPhase_= run.parserPhase
      val unit = compiler.newCompilationUnit(code, "<memory>")
      unit body_= compiler.newUnitParser(unit).parse()
      throwIfErrors()
      compiler phase_= run.namerPhase
      compiler globalPhase_= run.namerPhase
      newNamer(rootContext(unit)).enterSym(unit.body)
      throwIfErrors()
      compiler phase_= run.phaseNamed("packageobjects")
      compiler globalPhase_= run.phaseNamed("packageobjects")
      val openPackageObjectsTraverser = new Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case ModuleDef(_, _, _) =>
            if (tree.symbol.name == nme.PACKAGEkw) openPackageModule(tree.symbol, tree.symbol.owner)
          case ClassDef(_, _, _, _) =>
            ()
          case _ =>
            super.traverse(tree)
        }
      }
      openPackageObjectsTraverser(unit.body)
      throwIfErrors()
      compiler phase_= run.typerPhase
      compiler globalPhase_= run.typerPhase
      val typer = newTyper(rootContext(unit))
      typer.context.setReportErrors()
      unit body_= typer.typed(unit.body).asInstanceOf[compiler.Tree]
      if (debug) println(unit.body)
      unit.toCheck.foreach(workItem => workItem())
      throwIfErrors()
      val h = Scalahost(compiler).asInstanceOf[PalladiumHostwithOurHost[compiler.type] {  }]
      val ptree = h.toPalladium(unit.body, classOf[Aux.CompUnit])
      if (debug) println(ptree.show[Code])
      if (debug) println(ptree.show[Raw])
      result = ptree.show[Code]
    }
    val m_withCompilerApi = tb.getClass().getDeclaredMethod("withCompilerApi")
    val o_withCompilerApi = m_withCompilerApi.invoke(tb)
    val m_apply = o_withCompilerApi.getClass().getDeclaredMethods().find(_ => x$2.getName() == "apply").get
    try m_apply.invoke(o_withCompilerApi, {
      compilerApi => cont(compilerApi)
    }) catch {
      case x$3: Throwable =>
        val catchExpr1 = scala.reflect.runtime.ReflectionUtils.unwrapHandler({
          case ex =>
            throw ex
        });
        if (catchExpr1.isDefinedAt(x$3)) catchExpr1(x$3) else throw x$3
    }
    result
  }
  def runScalaToMetaTest(dirPath: String): Unit = {
    def slurp(filePath: String) = scala.io.Source.fromFile(new File(filePath)).mkString.trim()
    val actualResult = typecheckConvertAndPrettyprint(slurp(dirPath + File.separatorChar + "Original.scala"), debug = false)
    val expectedResult = slurp(dirPath + File.separatorChar + "Expected.scala")
    if (actualResult != expectedResult) {
      typecheckConvertAndPrettyprint(slurp(dirPath + File.separatorChar + "Original.scala"), debug = true)
      assert(actualResult === expectedResult)
    }
  }
  val resourceDir = new File(System.getProperty("sbt.paths.tests.resources") + File.separatorChar + "ScalaToMeta")
  resourceDir.listFiles().foreach(dir => test(dir.getName())(runScalaToMetaTest(dir.getAbsolutePath())))
}
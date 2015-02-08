import org.scalatest._
import java.net._
import java.io._
import scala.reflect.runtime.{ universe => ru }
import scala.tools.reflect.{ ToolBox, ToolBoxError }
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.internal.hosts.scalac.Scalahost
import org.scalameta.reflection._
class ScalaToMeta extends FunSuite {
  def typecheckConvertAndPrettyprint(code: String, debug: Boolean): String = {
    val pluginJar = System.getProperty("sbt.paths.plugin.jar")
    val compilationClasspath = System.getProperty("sbt.paths.tests.classpath").split(File.pathSeparatorChar.toString).map(path => new URL("file://" + path))
    val classloader = new URLClassLoader(compilationClasspath, getClass.getClassLoader)
    val mirror = ru.runtimeMirror(classloader)
    val tb = mirror.mkToolBox(options = "-cp " + System.getProperty("sbt.paths.tests.classpath") + " -Xplugin:" + pluginJar + " -Xplugin-require:scalahost")
    var result: String = null
    def cont(compilerApi: AnyRef): Unit = {
      val m_compiler = compilerApi.getClass.getDeclaredMethod("compiler")
      val compiler = m_compiler.invoke(compilerApi).asInstanceOf[scala.tools.nsc.Global]
      import compiler._
      import analyzer._
      reporter.reset()
      val m_frontEnd = tb.getClass.getDeclaredMethod("frontEnd")
      val frontEnd = m_frontEnd.invoke(tb).asInstanceOf[scala.tools.reflect.FrontEnd]
      frontEnd.reset()
      def throwIfErrors(): Unit = if (frontEnd.hasErrors) throw ToolBoxError("reflective compilation has failed:" + EOL + EOL + frontEnd.infos.map(_.msg).mkString(EOL))
      val run = new compiler.Run
      phase = run.parserPhase
      globalPhase = run.parserPhase
      val unit = compiler.newCompilationUnit(code, "<memory>")
      unit.body = compiler.newUnitParser(unit).parse()
      throwIfErrors()
      phase = run.namerPhase
      globalPhase = run.namerPhase
      newNamer(rootContext(unit)).enterSym(unit.body)
      throwIfErrors()
      phase = run.phaseNamed("packageobjects")
      globalPhase = run.phaseNamed("packageobjects")
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
      phase = run.typerPhase
      globalPhase = run.typerPhase
      val typer = newTyper(rootContext(unit))
      typer.context.initRootContext()
      unit.body = typer.typed(unit.body).asInstanceOf[compiler.Tree]
      if (debug) println(unit.body)
      if (debug) println(new { val global: compiler.type = compiler } with GlobalToolkit {}.ensugar(unit.body))
      for (workItem <- unit.toCheck) workItem()
      throwIfErrors()
      implicit val c = Scalahost.mkSemanticContext[compiler.type](compiler)
      val ptree = c.toScalameta(unit.body, classOf[Source])
      if (debug) println(ptree.show[Code])
      if (debug) println(ptree.show[Raw])
      result = ptree.show[Code]
    }
    val m_withCompilerApi = tb.getClass.getDeclaredMethod("withCompilerApi")
    val o_withCompilerApi = m_withCompilerApi.invoke(tb)
    val m_apply = o_withCompilerApi.getClass.getDeclaredMethods.find(_.getName == "apply").get
    try m_apply.invoke(o_withCompilerApi, cont _) catch scala.reflect.runtime.ReflectionUtils.unwrapHandler({
      case ex =>
        throw ex
    })
    result
  }
  def runScalaToMetaTest(dirPath: String): Unit = {
    def resource(label: String) = dirPath + File.separatorChar + label + ".scala"
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
    delete("Actual")
    val actualResult = typecheckConvertAndPrettyprint(slurp("Original"), debug = false)
    if (!exists("Expected")) dump("Expected", "")
    val expectedResult = slurp("Expected")
    if (actualResult != expectedResult) {
      dump("Actual", actualResult)
      fail(s"see ${resource("Actual")} for details")
    }
  }
  val resourceDir = new File(System.getProperty("sbt.paths.tests.resources") + File.separatorChar + "ScalaToMeta")
  val testDirs = resourceDir.listFiles().filter(_.listFiles().nonEmpty).filter(!_.getName().endsWith("_disabled"))
  testDirs.foreach(testDir => test(testDir.getName)(runScalaToMetaTest(testDir.getAbsolutePath)))
}
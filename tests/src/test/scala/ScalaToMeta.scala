import org.scalatest._

class ScalaToMeta extends ToolboxHarness {
  test("definition of old Join") {
    checkScalaToMeta("""
      |import scala.reflect.macros.whitebox._
      |import scala.language.experimental.macros
      |
      |object Join {
      |  def impl(c: Context)(x: c.Tree, y: c.Tree) = {
      |    import c.universe._
      |    def fields(tree: Tree) = tree.tpe.members.collect{ case m: TermSymbol if m.isGetter => m }
      |    val xfields = fields(x).map(f => f -> q"xtemp")
      |    val yfields = fields(y).map(f => f -> q"ytemp")
      |    val getters = (xfields ++ yfields).map{ case (f, ref) => q"val ${f.name} = $ref.${f.name}" }
      |    qQQQ
      |      val xtemp = $x
      |      val ytemp = $y
      |      new { ..$getters }
      |    QQQ
      |  }
      |
      |  def apply[T, U](x: T, y: U): Any = macro impl
      |}
    """,
    """
      |import scala.reflect.macros.whitebox._
      |import scala.language.experimental.macros
      |object Join {
      |  def impl(c: Context)(x: c.Tree, y: c.Tree) = {
      |    import c.universe._
      |    def fields(tree: Tree) = tree.tpe.members.collect({
      |      case m: TermSymbol if m.isGetter =>
      |        m
      |    })
      |    val xfields = fields(x).map(f => f -> q"xtemp")
      |    val yfields = fields(y).map(f => f -> q"ytemp")
      |    val getters = (xfields ++ yfields).map({
      |      case (f, ref) =>
      |        q"val ${f.name} = $ref.${f.name}"
      |    })
      |    qQQQ
      |      val xtemp = $x
      |      val ytemp = $y
      |      new { ..$getters }
      |    QQQ
      |  }
      |  def apply[T, U](x: T, y: U): Any = macro Join.impl
      |}
    """)
  }

  test("attachment-checking plugin") {
    checkScalaToMeta("""
      |import scala.tools.nsc.{Global, Phase, SubComponent}
      |import scala.tools.nsc.plugins.{Plugin => NscPlugin, PluginComponent => NscPluginComponent}
      |import org.scalameta.reflection.Metadata
      |
      |class Plugin(val global: Global) extends NscPlugin with Metadata { self =>
      |  val name = "attatest"
      |  val description = "Tests attachments produced by scalahost"
      |  val components = List[NscPluginComponent](PluginComponent)
      |  object PluginComponent extends NscPluginComponent {
      |    val global: self.global.type = self.global
      |    import global._
      |
      |    override val runsAfter = List("typer")
      |    override val runsRightAfter = None
      |    val phaseName = "attatest"
      |    override def description = "tests attachments produced by scalahost"
      |
      |    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      |      import analyzer._
      |      override def apply(unit: CompilationUnit): Unit = {
      |        unit.body.foreach(tree => {
      |          if (hasMacroExpansionAttachment(tree)) {
      |            tree.metadata.toOption match {
      |              case None =>
      |                reporter.error(tree.pos, "macro expansion without a property bag")
      |              case Some(bag) =>
      |                if (!bag.contains("expandeeTree")) reporter.error(tree.pos, "macro expansion without expandeeTree in the property bag")
      |                if (!bag.contains("expandedTree")) reporter.error(tree.pos, "macro expansion without expandedTree in the property bag")
      |                if (!bag.contains("expansionString")) reporter.error(tree.pos, "macro expansion without expansionString in the property bag")
      |            }
      |          }
      |        })
      |      }
      |    }
      |  }
      |}
    """,
    """
      |import scala.tools.nsc.{ Global, Phase, SubComponent }
      |import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
      |import org.scalameta.reflection.Metadata
      |class Plugin(val global: Global) extends NscPlugin with Metadata { self =>
      |  val name = "attatest"
      |  val description = "Tests attachments produced by scalahost"
      |  val components = List[NscPluginComponent](PluginComponent)
      |  object PluginComponent extends NscPluginComponent {
      |    val global: self.global.type = self.global
      |    import global._
      |    override val runsAfter = List("typer")
      |    override val runsRightAfter = None
      |    val phaseName = "attatest"
      |    override def description = "tests attachments produced by scalahost"
      |    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      |      import analyzer._
      |      override def apply(unit: CompilationUnit): Unit = unit.body.foreach(tree => if (hasMacroExpansionAttachment(tree)) tree.metadata.toOption match {
      |        case None =>
      |          reporter.error(tree.pos, "macro expansion without a property bag")
      |        case Some(bag) =>
      |          if (!bag.contains("expandeeTree")) reporter.error(tree.pos, "macro expansion without expandeeTree in the property bag");
      |          if (!bag.contains("expandedTree")) reporter.error(tree.pos, "macro expansion without expandedTree in the property bag");
      |          if (!bag.contains("expansionString")) reporter.error(tree.pos, "macro expansion without expansionString in the property bag")
      |      })
      |    }
      |  }
      |}
    """)
  }

  test("old Join test") {
    checkScalaToMeta("""
      |import org.scalatest._
      |
      |class OldMacros extends FunSuite {
      |  test("old join") {
      |    import scala.language.reflectiveCalls
      |    val x = new { val x = 2 }
      |    val y = new { val y = 3 }
      |    val result = Join(x, y)
      |    assert(result.x === 2)
      |    assert(result.y === 3)
      |  }
      |}
    """,
    """
      |import org.scalatest._
      |class OldMacros extends FunSuite {
      |  test("old join") {
      |    import scala.language.reflectiveCalls
      |    val x = new { val x = 2 }
      |    val y = new { val y = 3 }
      |    val result = Join(x, y)
      |    assert(result.x === 2)
      |    assert(result.y === 3)
      |  }
      |}
    """)
  }

  test("new Join test") {
    checkScalaToMeta("""
      |import org.scalatest._
      |
      |import scala.language.reflectiveCalls
      |import scala.language.experimental.macros
      |import scala.meta._
      |import scala.meta.semantic._
      |import scala.meta.semantic.errors.throwExceptions
      |
      |class NewMacros extends FunSuite {
      |  test("new join") {
      |    def join[T, U](x: T, y: U): Any = macro {
      |      val xfields = x.tpe.vals.map(f => f -> q"xtemp")
      |      val yfields = y.tpe.vals.map(f => f -> q"ytemp")
      |      val getters = (xfields ++ yfields).map{ case (f, ref) => q"val ${f.name} = $ref.${f.name}" }
      |      c.whitebox(qQQQ
      |        val xtemp = $x
      |        val ytemp = $y
      |        new { ..$getters }
      |      QQQ)
      |    }
      |    val result = join(new { val x = 2 }, new { val y = 3 })
      |    assert(result.x === 2)
      |    assert(result.y === 3)
      |  }
      |}
    """,
    """
      |import org.scalatest._
      |import scala.language.reflectiveCalls
      |import scala.language.experimental.macros
      |import scala.meta._
      |import scala.meta.semantic._
      |import scala.meta.semantic.errors.throwExceptions
      |class NewMacros extends FunSuite {
      |  test("new join") {
      |    def join[T, U](x: T, y: U): Any = macro {
      |      val xfields = x.tpe.vals.map(f => f -> q"xtemp")
      |      val yfields = y.tpe.vals.map(f => f -> q"ytemp")
      |      val getters = (xfields ++ yfields).map({
      |        case (f, ref) =>
      |          q"val ${f.name} = $ref.${f.name}"
      |      })
      |      c.whitebox(qQQQ
      |        val xtemp = $x
      |        val ytemp = $y
      |        new { ..$getters }
      |      QQQ)
      |    }
      |    val result = join(new { val x = 2 }, new { val y = 3 })
      |    assert(result.x === 2)
      |    assert(result.y === 3)
      |  }
      |}
    """)
  }

  test("toolbox harness") {
    checkScalaToMeta("""
      |import org.scalatest._
      |import java.net._
      |import java.io.File
      |import scala.reflect.runtime.{universe => ru}
      |import scala.tools.reflect.{ToolBox, ToolBoxError}
      |import scala.compat.Platform.EOL
      |import scala.meta._
      |import scala.meta.syntactic.parsers._
      |import scala.meta.syntactic.show._
      |import scala.meta.internal.hosts.scalacompiler.scalahost.Scalahost
      |import scala.meta.semantic.{Host => PalladiumHost}
      |import scala.meta.internal.hosts.scalacompiler.scalahost.{Host => OurHost}
      |
      |trait ToolboxHarness extends FunSuite {
      |  private def typecheckConvertAndPrettyprint(code: String, debug: Boolean): String = {
      |    val pluginJar = System.getProperty("sbt.classpaths.package.plugin")
      |    val compilationClasspath = System.getProperty("sbt.classpaths.test.tests").split(File.pathSeparatorChar.toString).map(path => new URL("file://" + path))
      |    val classloader = new URLClassLoader(compilationClasspath, getClass.getClassLoader)
      |    val mirror = ru.runtimeMirror(classloader)
      |    val tb = mirror.mkToolBox(options = "-cp " + System.getProperty("sbt.classpaths.test.tests") + " -Xplugin:" + pluginJar + " -Xplugin-require:scalahost")
      |    var result: String = null
      |    def cont(compilerApi: AnyRef): Unit = {
      |      val m_compiler = compilerApi.getClass.getDeclaredMethod("compiler")
      |      val compiler = m_compiler.invoke(compilerApi).asInstanceOf[scala.tools.nsc.Global]
      |      import compiler._
      |      import analyzer._
      |
      |      reporter.reset()
      |      val m_frontEnd = tb.getClass.getDeclaredMethod("frontEnd")
      |      val frontEnd = m_frontEnd.invoke(tb).asInstanceOf[scala.tools.reflect.FrontEnd]
      |      frontEnd.reset()
      |      def throwIfErrors(): Unit = {
      |        if (frontEnd.hasErrors) throw ToolBoxError(
      |          "reflective compilation has failed:" + EOL + EOL + (frontEnd.infos map (_.msg) mkString EOL)
      |        )
      |      }
      |
      |      val run = new compiler.Run
      |      phase = run.parserPhase
      |      globalPhase = run.parserPhase
      |      val unit = compiler.newCompilationUnit(code, "<memory>")
      |      unit.body = compiler.newUnitParser(unit).parse()
      |      throwIfErrors()
      |
      |      phase = run.namerPhase
      |      globalPhase = run.namerPhase
      |      newNamer(rootContext(unit)).enterSym(unit.body)
      |      throwIfErrors()
      |
      |      phase = run.phaseNamed("packageobjects")
      |      globalPhase = run.phaseNamed("packageobjects")
      |      val openPackageObjectsTraverser = new Traverser {
      |        override def traverse(tree: Tree): Unit = tree match {
      |          case ModuleDef(_, _, _) =>
      |            if (tree.symbol.name == nme.PACKAGEkw) {
      |              openPackageModule(tree.symbol, tree.symbol.owner)
      |            }
      |          case ClassDef(_, _, _, _) => () // make it fast
      |          case _ => super.traverse(tree)
      |        }
      |      }
      |      openPackageObjectsTraverser(unit.body)
      |      throwIfErrors()
      |
      |      phase = run.typerPhase
      |      globalPhase = run.typerPhase
      |      val typer = newTyper(rootContext(unit))
      |      typer.context.setReportErrors() // need to manually set context mode, otherwise typer.silent will throw exceptions
      |      unit.body = typer.typed(unit.body).asInstanceOf[compiler.Tree]
      |      if (debug) println(unit.body)
      |      for (workItem <- unit.toCheck) workItem()
      |      throwIfErrors()
      |
      |      val h = Scalahost(compiler).asInstanceOf[PalladiumHost with OurHost[compiler.type]]
      |      val ptree = h.toPalladium(unit.body, classOf[Aux.CompUnit])
      |      if (debug) println(ptree.show[Code])
      |      if (debug) println(ptree.show[Raw])
      |      result = ptree.show[Code]
      |    }
      |    val m_withCompilerApi = tb.getClass.getDeclaredMethod("withCompilerApi")
      |    val o_withCompilerApi = m_withCompilerApi.invoke(tb)
      |    val m_apply = o_withCompilerApi.getClass.getDeclaredMethods.find(_.getName == "apply").get
      |    try m_apply.invoke(o_withCompilerApi, cont _)
      |    catch scala.reflect.runtime.ReflectionUtils.unwrapHandler({ case ex => throw ex })
      |    result
      |  }
      |}
    """,
    """
      |import org.scalatest._
      |import java.net._
      |import java.io.File
      |import scala.reflect.runtime.{ universe => ru }
      |import scala.tools.reflect.{ ToolBox, ToolBoxError }
      |import scala.compat.Platform.EOL
      |import scala.meta._
      |import scala.meta.syntactic.parsers._
      |import scala.meta.syntactic.show._
      |import scala.meta.internal.hosts.scalacompiler.scalahost.Scalahost
      |import scala.meta.semantic.{ Host => PalladiumHost }
      |import scala.meta.internal.hosts.scalacompiler.scalahost.{ Host => OurHost }
      |trait ToolboxHarness extends FunSuite {
      |  private def typecheckConvertAndPrettyprint(code: String, debug: Boolean): String = {
      |    val pluginJar = System.getProperty("sbt.classpaths.package.plugin")
      |    val compilationClasspath = System.getProperty("sbt.classpaths.test.tests").split(File.pathSeparatorChar.toString()).map(path => new URL("file://" + path))
      |    val classloader = new URLClassLoader(compilationClasspath, getClass().getClassLoader())
      |    val mirror = ru.runtimeMirror(classloader)
      |    val tb = {
      |      val qual$1 = ToolBox(mirror)
      |      val x$4 = "-cp " + System.getProperty("sbt.classpaths.test.tests") + " -Xplugin:" + pluginJar + " -Xplugin-require:scalahost"
      |      val x$5 = qual$1.mkToolBox$default$1
      |      qual$1.mkToolBox(x$5, x$4)
      |    }
      |    var result: String = null
      |    def cont(compilerApi: AnyRef): Unit = {
      |      val m_compiler = compilerApi.getClass().getDeclaredMethod("compiler")
      |      val compiler = m_compiler.invoke(compilerApi).asInstanceOf[scala.tools.nsc.Global]
      |      import compiler._
      |      import analyzer._
      |      reporter.reset()
      |      val m_frontEnd = tb.getClass().getDeclaredMethod("frontEnd")
      |      val frontEnd = m_frontEnd.invoke(tb).asInstanceOf[scala.tools.reflect.FrontEnd]
      |      frontEnd.reset()
      |      def throwIfErrors(): Unit = if (frontEnd.hasErrors) throw ToolBoxError("reflective compilation has failed:" + EOL + EOL + frontEnd.infos.map(_ => x$1.msg).mkString(EOL), ToolBoxError.apply$default$2)
      |      val run = new compiler.Run
      |      compiler phase_= run.parserPhase
      |      compiler globalPhase_= run.parserPhase
      |      val unit = compiler.newCompilationUnit(code, "<memory>")
      |      unit body_= compiler.newUnitParser(unit).parse()
      |      throwIfErrors()
      |      compiler phase_= run.namerPhase
      |      compiler globalPhase_= run.namerPhase
      |      newNamer(rootContext(unit, analyzer.rootContext$default$2, analyzer.rootContext$default$3)).enterSym(unit.body)
      |      throwIfErrors()
      |      compiler phase_= run.phaseNamed("packageobjects")
      |      compiler globalPhase_= run.phaseNamed("packageobjects")
      |      val openPackageObjectsTraverser = new Traverser {
      |        override def traverse(tree: Tree): Unit = tree match {
      |          case ModuleDef(_, _, _) =>
      |            if (tree.symbol.name == nme.PACKAGEkw) openPackageModule(tree.symbol, tree.symbol.owner)
      |          case ClassDef(_, _, _, _) =>
      |            ()
      |          case _ =>
      |            .super.traverse(tree)
      |        }
      |      }
      |      openPackageObjectsTraverser(unit.body)
      |      throwIfErrors()
      |      compiler phase_= run.typerPhase
      |      compiler globalPhase_= run.typerPhase
      |      val typer = newTyper(rootContext(unit, analyzer.rootContext$default$2, analyzer.rootContext$default$3))
      |      typer.context.setReportErrors()
      |      unit body_= typer.typed(unit.body).asInstanceOf[compiler.Tree]
      |      if (debug) println(unit.body)
      |      unit.toCheck.foreach(workItem => workItem())
      |      throwIfErrors()
      |      val h = Scalahost(compiler).asInstanceOf[PalladiumHostwithOurHost[compiler.type] {  }]
      |      val ptree = h.toPalladium(unit.body, Predef.this.classOf[scala.meta.Aux.CompUnit])
      |      if (debug) println(ptree.show[Code])
      |      if (debug) println(ptree.show[Raw])
      |      result = ptree.show[Code]
      |    }
      |    val m_withCompilerApi = tb.getClass().getDeclaredMethod("withCompilerApi")
      |    val o_withCompilerApi = m_withCompilerApi.invoke(tb)
      |    val m_apply = o_withCompilerApi.getClass().getDeclaredMethods().find(_ => x$2.getName().==("apply")).get
      |    try m_apply.invoke(o_withCompilerApi, {
      |      compilerApi => cont(compilerApi)
      |    }) catch {
      |      case x$3: Throwable =>
      |        val catchExpr1 = scala.reflect.runtime.ReflectionUtils.unwrapHandler({
      |          case ex =>
      |            throw ex
      |        });
      |        if (catchExpr1.isDefinedAt(x$3)) catchExpr1(x$3) else throw x$3
      |    }
      |    result
      |  }
      |}
    """)
  }
}

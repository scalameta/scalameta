package scala.meta.interactive

import java.io.File
import java.net.URLClassLoader
import scala.meta.internal.SemanticdbPlugin
import scala.meta.internal.semanticdb.DatabaseOps
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.reporters.StoreReporter
import scala.meta.semanticdb.Document
import org.langmeta.internal.semanticdb.schema.Database

object InteractiveSemanticdb {

  def newCompiler(): Global =
    newCompiler(thisClasspath, Nil)
  def newCompiler(scalacOptions: List[String]): Global =
    newCompiler(thisClasspath, scalacOptions)

  /** Construct new presentation compiler with given classpath and scalac flags. */
  def newCompiler(classpath: String, scalacOptions: List[String]): Global = {
    val vd = new VirtualDirectory("(memory)", None)
    val settings = new Settings
    settings.outputDirs.setSingleOutput(vd)
    settings.classpath.value = classpath
    if (classpath.isEmpty) {
      settings.usejavacp.value = true
    }
    settings.processArgumentString(
      ("-Ypresentation-any-thread" :: scalacOptions).mkString(" ")
    )
    val compiler = new Global(settings, new StoreReporter)
    new SemanticdbPlugin(compiler) // hijack reporter/analyzer
    compiler
  }

  def toDocument(compiler: Global, code: String): Document =
    toDocument(compiler, code, "interactive.scala", 10000)

  /**
    * Build semanticdb document from this snippet of code.
    *
    * @param compiler an instance of scalac interactive global.
    * @param code the code to be compiled.
    * @param filename the name of the source file.
    * @param timeout max number of milliseconds to allow the presentation compiler
    *                to typecheck this file.
    *  @throws Exception note that this method can fail in many different ways
    *                    with exceptions, including but not limited to tokenize/parse/type
    *                    errors.
    */
  def toDocument(compiler: Global, code: String, filename: String, timeout: Long): Document = {
    val unit = addCompilationUnit(compiler, code, filename)
    // reload seems to be necessary before askLoadedType.
    ask[Unit](r => compiler.askReload(unit.source :: Nil, r)).get
    val compiledTree =
      ask[compiler.Tree](r => compiler.askLoadedTyped(unit.source, r))
        .get(timeout)
    val tree = compiledTree match {
      case Some(Left(t)) => t
      case Some(Right(ex)) => throw ex
      case None => throw new IllegalArgumentException("Presentation compiler timed out")
    }
    lazy val databaseOps: DatabaseOps {
      val global: compiler.type
    } = new DatabaseOps {
      val global: compiler.type = compiler
    }
    import databaseOps._
    unit.body = tree
    val document = unit.asInstanceOf[databaseOps.global.CompilationUnit].toDocument
    Database(document :: Nil).toDb(None).documents.head
  }

  /**
    * Inserts "_CURSOR_" at given offset.
    *
    * _CURSOR_ hints to the presentation compiler that this file is being edited
    * with the cursor at that offset. This hint helps completions amongst
    * other things.
    */
  def addCursor(code: String, offset: Int): String = {
    new StringBuilder(code.length + "_CURSOR_".length)
      .append(code.substring(0, offset))
      .append("_CURSOR_")
      .append(code.substring(offset))
      .toString()
  }

  /** Create new compilation unit from given code. */
  def addCompilationUnit(
      global: Global,
      code: String,
      filename: String
  ): global.RichCompilationUnit = {
    val unit = global.newCompilationUnit(code, filename)
    val richUnit = new global.RichCompilationUnit(unit.source)
    global.unitOfFile(richUnit.source.file) = richUnit
    richUnit
  }

  private def thisClasspath: String = this.getClass.getClassLoader match {
    case url: URLClassLoader =>
      url.getURLs.map(_.toURI.getPath).mkString(File.pathSeparator)
    case els =>
      throw new IllegalStateException(s"Expected URLClassloader, got $els")
  }

  private def ask[A](f: Response[A] => Unit): Response[A] = {
    val r = new Response[A]
    f(r)
    r
  }

}

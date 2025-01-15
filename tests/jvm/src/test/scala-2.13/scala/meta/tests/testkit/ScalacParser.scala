package scala.meta.tests.testkit

import java.io.File
import java.net.URLClassLoader

import scala.collection.mutable
import scala.reflect.internal.util.CodeAction
import scala.tools.nsc.Global
import scala.tools.nsc.Settings

/**
 * Borrowed from
 * https://github.com/lihaoyi/fastparse/blob/0d67eca8f9264bfaff68e5cbb227045ceac4a15f/scalaparse/jvm/src/test/scala/scalaparse/ProjectTests.scala
 */
object ScalacParser {
  var current: ClassLoader = Thread.currentThread().getContextClassLoader
  val files: mutable.Buffer[File] = collection.mutable.Buffer.empty[java.io.File]
  val settings = new Settings()
  Seq("sun.boot.class.path", "java.class.path").foreach { prop =>
    System.getProperty(prop).split(File.pathSeparator)
      .foreach(entry => files.append(new File(entry)))
  }
  while (current != null) {
    current match {
      case t: URLClassLoader => t.getURLs.foreach(u => files.append(new File(u.toURI)))
      case _ =>
    }
    current = current.getParent
  }
  val global = new Global(settings)
  settings.usejavacp.value = true
  settings.embeddedDefaults[ScalacParser.type]
  settings.classpath.append(files.mkString(":"))

  def canParseInput(input: String) = this.synchronized {
    val run = new global.Run()
    var fail = false
    import global.syntaxAnalyzer.Offset
    val cu = new global.CompilationUnit(global.newSourceFile(input))
    val parser = new global.syntaxAnalyzer.UnitParser(cu, Nil) {
      override def newScanner() = new global.syntaxAnalyzer.UnitScanner(cu, Nil) {
        override def error(off: Offset, msg: String) = fail = true
        override def syntaxError(off: Offset, msg: String) = fail = true
        override def incompleteInputError(off: Offset, msg: String) = fail = true
      }
      override def incompleteInputError(msg: String, actions: List[CodeAction]) = fail = true
      override def syntaxError(offset: Offset, msg: String, actions: List[CodeAction]) = fail = true
    }
    parser.parse()
    !fail
  }
}

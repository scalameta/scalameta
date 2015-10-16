package scala.meta
package internal.hosts.scalac
package contexts

import java.io.File
import java.lang.management.ManagementFactory
import scala.collection.JavaConverters._
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.reflect.WrappedProperties.AccessControl._

object Compiler {
  def apply()(implicit resolver: Resolver): Global = {
    apply("")
  }

  def apply(options: String)(implicit resolver: Resolver): Global = {
    def fail(reason: String) = throw new InfrastructureException("can't initialize a semantic adapter from scratch: " + reason)
    val args = CommandLineParser.tokenize(options)
    val emptySettings = new Settings(error => fail(s"invalid compiler options: $error"))
    val reporter = new StoreReporter()
    val command = new CompilerCommand(args, emptySettings)
    val settings = command.settings
    initializeJreClasspath(settings)
    Global(settings, reporter)
  }

  private def initializeJreClasspath(settings: Settings): Unit = {
    // NOTE: The task of setting a classpath of the compiler to a blank slate is surprisingly difficult.
    // 1) Not specifying any -classpath will actually lead to scalac magically
    //    selecting either CLASSPATH or current dir as the compilation classpath.
    // 2) Without using obscure compiler options (thank god that -javabootclasspath exists),
    //    it is impossible to prevent scalac from populating the compilation classpath
    //    with the current JVM's boot classpath.
    // 3) Another, less obscure, option (-usejavacp) is required to prevent scalac
    //    from population the compilation classpath with the current JVM's classpath.
    // 4) -Dscala.usejavacp (why on Earth would we need a separate option to double -usejavacp??)
    //    can still mess things up, and we can't just temporarily set sys.props("scala.usejavacp") to false,
    //    because classpath initialization happens elsewhere, lazily. TODO: I'll get to that later.
    settings.javabootclasspath.value = computeJreClasspath()
    settings.nobootcp.value = true // NOTE: has no effect, but I'm still writing it down here for the future
    settings.usejavacp.value = false
    settings.classpath.value = ""
    // settings.Ylogcp.value = true
  }

  // TODO: I've tried to figure out how to programmatically obtain the default bootclasspath
  // (i.e. the one just with rj.jar and the like), but failed and has to resort to workarounds.
  private def computeJreClasspath(): String = {
    def parse(classpath: String): List[String] = classpath.split(File.pathSeparatorChar).toList

    def unparse(classpath: List[String]): String = classpath.mkString(File.pathSeparator)

    def subtract(classpath1: List[String], classpath2: List[String]): List[String] = {
      classpath1.filter(entry1 => !classpath2.exists(entry2 => {
        new File(entry1).getAbsolutePath == new File(entry2).getAbsolutePath
      }))
    }

    def loop(classpath: List[String], args: List[String]): List[String] = {
      args match {
        case arg :: rest if arg.startsWith("-Xbootclasspath/a:") =>
          loop(subtract(classpath, parse(arg.stripPrefix("-Xbootclasspath/a:"))), rest)
        case arg :: rest if arg.startsWith("-Xbootclasspath/p:") =>
          loop(subtract(classpath, parse(arg.stripPrefix("-Xbootclasspath/p:"))), rest)
        case arg :: rest if arg.startsWith("-Xbootclasspath") =>
          throw new InfrastructureException(s"can't initialize a semantic adapter from scratch: classpath infrastructure doesn't support $arg")
        case arg :: rest =>
          loop(classpath, rest)
        case Nil =>
          classpath
      }
    }

    def searchForBootClasspath = systemProperties find (_._1 endsWith ".boot.class.path") map (_._2) getOrElse ""
    val jvmBootClasspath = propOrElse("sun.boot.class.path", searchForBootClasspath)
    val jvmArguments = ManagementFactory.getRuntimeMXBean().getInputArguments().asScala.toList
    unparse(loop(parse(jvmBootClasspath), jvmArguments))
  }
}

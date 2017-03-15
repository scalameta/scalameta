package scala.meta.internal
package scalahost
package v1
package offline

import java.io._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.meta._
import scala.meta.semantic.v1.{Mirror => MirrorApi}
import scala.meta.semantic.v1.Database
import scala.meta.semantic.v1.Location
import scala.meta.internal.scalahost.v1.{Mirror => BaseMirror}
import scala.meta.internal.scalahost.v1.online.{Mirror => OnlineMirror}

class Mirror(classpath: String, sourcepath: String)
    extends MirrorApi
    with BaseMirror
    with PathOps {

  private def fail(what: String) =
    sys.error(
      s"$what must be non-empty. " +
        s"This may indicate that Mirror is badly configured. " +
        s"If you use sbt-scalahost, make sure your project defines " +
        s"`dependsOn(<projectname> % Scalameta)` for at least one <projectname>.")
  if (classpath == null || classpath == "") fail("classpath")
  if (sourcepath == null || sourcepath == "") fail("sourcepath")

  override def toString: String = {
    s"online mirror for $classpath and $sourcepath"
  }

  lazy val sources: Seq[Source] = {
    sourcepath
      .split(File.pathSeparator)
      .flatMap(_.files.map(file => dialect(file).parse[Source].get))
      .to[Seq]
  }

  private lazy val classpathDatabase: Database = {
    val databaseFiles = classpath.paths
      .flatMap(uri => {
        val subfiles = new File(uri).listFiles
        if (subfiles != null) subfiles.filter(_.getName == "semanticdb").toList
        else Nil
      })
      .sortBy(_.getName)
    val databases = databaseFiles.map(SerializationOps.fromFile).collect {
      case scala.util.Success(db) => db
    }
    databases.foldLeft(Database())(_ append _)
  }

  def database: Database = {
    classpathDatabase.append(onlineMirror.database)
  }

  override def typecheck(tree: Tree): Tree = {
    val tree1 = onlineMirror.typecheck(tree)
    minputMap ++= onlineMirror.minputMap
    tree1
  }

  private lazy val onlineMirror: OnlineMirror = {
    // TODO: Before the final release, change this to something more principled.
    // In the meanwhile, we can happily hack away and hope that our analyzer changes get merged.
    val pluginpath = {
      def autodetectFromClassloader: Option[String] = {
        this.getClass.getClassLoader match {
          case cl: java.net.URLClassLoader =>
            val paths = cl.getURLs.map(_.getPath)
            paths.find(p => p.contains("scalahost") && p.endsWith(".jar"))
          case _ =>
            None
        }
      }
      def autodetectFromProperties: Option[String] = {
        val customPath = Option(sys.props("scalahost.jar"))
        val sbtPath = Option(sys.props("sbt.paths.scalahost.compile.jar"))
        customPath.orElse(sbtPath)
      }
      def fail(): Nothing = {
        sys.error(
          "failed to locate scalahost.jar, specify the location manually via -Dscalahost.jar")
      }
      autodetectFromClassloader.orElse(autodetectFromProperties).getOrElse(fail)
    }
    val global: Global = {
      def fail(msg: String) = sys.error(s"mirror initialization failed: $msg")
      val options = "-Yrangepos -cp " + classpath + " -Xplugin:" + pluginpath + " -Xplugin-require:scalahost"
      val args = CommandLineParser.tokenize(options)
      val emptySettings = new Settings(error => fail(s"couldn't apply settings because $error"))
      val reporter = new StoreReporter()
      val command = new CompilerCommand(args, emptySettings)
      val settings = command.settings
      val g = new Global(settings, reporter)
      val run = new g.Run
      if (reporter.hasErrors) reporter.infos.foreach(info => fail(info.msg))
      g.phase = run.phaseNamed("patmat")
      g.globalPhase = run.phaseNamed("patmat")
      g
    }
    new OnlineMirror(global)
  }
}

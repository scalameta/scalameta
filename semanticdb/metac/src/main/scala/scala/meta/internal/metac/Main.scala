package scala.meta.internal.metac

import scala.meta.cli._
import scala.meta.internal.classpath.ClasspathUtils
import scala.meta.internal.semanticdb.scalac._
import scala.meta.metac._

import java.io._
import java.nio.channels._
import java.nio.file._

import scala.tools.nsc.{Main => ScalacMain}

class Main(settings: Settings, reporter: Reporter) {
  def process(): Boolean = {
    val pluginClass = classOf[SemanticdbPlugin]
    val pluginClasspath = Option(pluginClass.getClassLoader).fold {
      val manifestDir = Files.createTempDirectory("semanticdb-scalac_")
      val pluginConfig = "scalac-plugin.xml"
      val resourceUrl = pluginClass.getResource("/" + pluginConfig)
      val resourceChannel = Channels.newChannel(resourceUrl.openStream())
      val manifestStream = new FileOutputStream(manifestDir.resolve(pluginConfig).toFile)
      manifestStream.getChannel.transferFrom(resourceChannel, 0, Long.MaxValue)
      manifestStream.close()
      manifestDir.toString
    }(cl => ClasspathUtils.getClassPathEntries(cl).mkString(File.pathSeparator))
    val enablePluginArgs = List("-Xplugin:" + pluginClasspath, "-Xplugin-require:semanticdb")
    val enableRangeposArgs = List("-Yrangepos")
    val stopAfterPluginArgs = List("-Ystop-after:semanticdb-typer")
    val args = settings.scalacArgs ++ enablePluginArgs ++ enableRangeposArgs ++ stopAfterPluginArgs
    scala.Console
      .withOut(reporter.out)(scala.Console.withErr(reporter.err)(ScalacMain.process(args.toArray)))
    !ScalacMain.reporter.hasErrors
  }
}

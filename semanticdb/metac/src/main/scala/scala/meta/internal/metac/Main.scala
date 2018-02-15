package scala.meta.internal.metac

import java.io._
import java.net._
import java.nio.channels._
import java.nio.file._
import scala.meta.internal.semanticdb.scalac._
import scala.tools.nsc.{Main => ScalacMain}

object Main {
  def process(args: Array[String]): Int = {
    val manifestDir = Files.createTempDirectory("semanticdb-scalac_")
    val resourceUrl = classOf[SemanticdbPlugin].getResource("/scalac-plugin.xml")
    val resourceChannel = Channels.newChannel(resourceUrl.openStream())
    val manifestStream = new FileOutputStream(manifestDir.resolve("scalac-plugin.xml").toFile)
    manifestStream.getChannel().transferFrom(resourceChannel, 0, Long.MaxValue)
    manifestStream.close()
    val pluginClasspath = classOf[SemanticdbPlugin].getClassLoader match {
      case null => manifestDir.toString
      case cl: URLClassLoader => cl.getURLs.map(_.getFile).mkString(File.pathSeparator)
      case cl => sys.error(s"unsupported classloader: $cl")
    }
    val enablePluginArgs = Array("-Xplugin:" + pluginClasspath, "-Xplugin-require:semanticdb")
    val enableRangeposArgs = Array("-Yrangepos")
    val stopAfterPluginArgs = Array("-Ystop-after:semanticdb-typer")
    val scalacArgs = args ++ enablePluginArgs ++ enableRangeposArgs ++ stopAfterPluginArgs
    ScalacMain.process(scalacArgs)
    if (ScalacMain.reporter.hasErrors) 1 else 0
  }
}

package scala.meta.cli

import java.io._
import java.nio.channels._
import java.nio.file._
import scala.tools.nsc.Main

object Metac {
  def main(args: Array[String]): Unit = {
    val pluginDir = Files.createTempDirectory("semanticdb-scalac")
    val resourceUrl = classOf[scala.meta.internal.SemanticdbPlugin].getResource("/scalac-plugin.xml")
    val resourceChannel = Channels.newChannel(resourceUrl.openStream())
    val pluginStream = new FileOutputStream(pluginDir.resolve("scalac-plugin.xml").toFile)
    pluginStream.getChannel().transferFrom(resourceChannel, 0, Long.MaxValue)
    pluginStream.close()
    val semanticdbArgs = Array("-Xplugin:" + pluginDir, "-Xplugin-require:semanticdb", "-Yrangepos")
    val scalacArgs = args ++ semanticdbArgs
    Main.main(scalacArgs)
  }
}

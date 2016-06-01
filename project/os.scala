package org.scalameta
package os

import java.io._
import scala.sys.process._
import scala.compat.Platform.EOL

object shell {
  def exec(command: String): (Int, String, String) = {
    val stdoutStream = new ByteArrayOutputStream
    val stderrStream = new ByteArrayOutputStream
    val stdoutWriter = new PrintWriter(stdoutStream)
    val stderrWriter = new PrintWriter(stderrStream)
    val exitcode = command.!(ProcessLogger(stdoutWriter.println, stderrWriter.println))
    stdoutWriter.close()
    stderrWriter.close()
    (exitcode, stdoutStream.toString, stderrStream.toString)
  }
}

object secret {
  def obtain(domain: String): List[(String, String)] = {
    val credentialsFile = System.getProperty(domain + ".settings.file")
    if (credentialsFile != null) {
      println(s"Loading ${domain.capitalize} credentials from $credentialsFile")
      try {
        import scala.xml._
        val settings = XML.loadFile(credentialsFile)
        def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
        List((readServerConfig("username"), readServerConfig("password")))
      } catch {
        case ex: Exception =>
          println(s"Failed to load ${domain.capitalize} settings from $credentialsFile: $ex")
          Nil
      }
    } else {
      println(s"Loading ${domain.capitalize} credentials from environment variables")
      for {
        username <- sys.env.get(s"${domain.toUpperCase}_USERNAME").toList
        password <- sys.env.get(s"${domain.toUpperCase}_PASSWORD").toList
      } yield {
        (username, password)
      }
    }
  }
}
package java.io

import java.net.URI
import java.nio.file.NodeJSPath
import java.nio.file.Path

trait File {
  def toPath: Path
  def toURI: URI
  def getPath: String
}

case class NodeJSFile(filename: String) extends File {
  def toPath: Path = NodeJSPath(filename)
  def toURI: URI = toPath.toUri
  def getPath: String = filename
}

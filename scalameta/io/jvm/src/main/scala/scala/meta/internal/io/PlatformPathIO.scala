package scala.meta.internal.io

import scala.meta.io._

import java.io.File
import java.nio.file.Paths

object PlatformPathIO {
  def workingDirectoryString: String = sys.props("user.dir")
}

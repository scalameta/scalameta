package scala.meta.internal.io

import java.io.File
import java.nio.file.Paths
import scala.meta.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    sys.props("user.dir")
}

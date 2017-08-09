package lang.meta.internal.io

import java.io.File
import java.nio.file.Paths
import lang.meta.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    sys.props("user.dir")
}

package org.langmeta.internal.io

import java.io.File
import java.nio.file.Paths
import org.langmeta.highlevel.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    sys.props("user.dir")
}

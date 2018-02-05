package org.langmeta.internal.io

import java.io.File
import java.nio.file.Paths
import org.langmeta.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    // TODO: sys.props("user.dir") doesn't work under Scala Native,
    // so I used (a hopefully equivalent) analog.
    new File(".").getAbsolutePath
}

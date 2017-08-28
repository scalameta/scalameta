package org.langmeta.internal.io

import org.langmeta.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    JSIO.cwd()
}

package org.langmeta.internal.io

import org.langmeta.highlevel.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    JSIO.cwd()
}

package lang.meta.internal.io

import lang.meta.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    JSIO.cwd()
}

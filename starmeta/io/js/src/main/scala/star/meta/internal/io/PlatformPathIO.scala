package star.meta.internal.io

import star.meta.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    JSIO.cwd()
}

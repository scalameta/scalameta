package scala.meta.internal.io

import scala.meta.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    JSIO.cwd()
}

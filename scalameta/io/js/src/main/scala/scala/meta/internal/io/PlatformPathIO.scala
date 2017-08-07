package scala.meta.internal.io

import scala.meta.io._
import java.io.File

object PlatformPathIO {
  def workingDirectoryString: String =
    JSIO.cwd()
}

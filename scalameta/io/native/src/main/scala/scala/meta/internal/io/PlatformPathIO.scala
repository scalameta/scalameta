package scala.meta.internal.io

import scala.meta.io._
import java.io.File

object PlatformPathIO {
  def workingDirectoryString: String = {
    // NOTE: sys.props("user.dir") doesn't work under Scala Native,
    // so I used (a hopefully equivalent) analog.
    new File(".").getAbsolutePath
  }
}

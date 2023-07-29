package scala.meta.tests

import java.nio.file.{Path, Paths}
import scala.meta.io.AbsolutePath

object Utils {

  def getResourceOpt(file: String): Option[Path] =
    Option(getClass.getResource("/" + file)).map(x => Paths.get(x.toURI))

  def getAbsResourceOpt(file: String): Option[AbsolutePath] =
    getResourceOpt(file).map(AbsolutePath.apply).filter(_.isFile)

}

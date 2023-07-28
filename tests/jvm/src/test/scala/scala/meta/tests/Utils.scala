package scala.meta.tests

import java.nio.file.{Path, Paths}

object Utils {

  def getResourceOpt(file: String): Option[Path] =
    Option(getClass.getResource("/" + file)).map(x => Paths.get(x.toURI))

}

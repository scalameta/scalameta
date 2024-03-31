package scala.meta.tests

import java.nio.file.{Path, Paths}
import scala.annotation.tailrec
import scala.meta.io.AbsolutePath

object Utils {

  def getResourceOpt(file: String): Option[Path] = Option(getClass.getResource("/" + file))
    .map(x => Paths.get(x.toURI))

  def getAbsResourceOpt(file: String): Option[AbsolutePath] = getResourceOpt(file)
    .map(AbsolutePath.apply).filter(_.isFile)

  @tailrec
  def getFirstAbsResourceOpt(files: String*): Option[AbsolutePath] = files match {
    case head +: tail =>
      val res = getAbsResourceOpt(head)
      if (res.isEmpty) getFirstAbsResourceOpt(tail: _*) else res
    case _ => None
  }

}

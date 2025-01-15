package scala.meta.internal.classpath

import java.io.File
import java.net.URLClassLoader
import java.nio.file.Path
import java.nio.file.Paths

object ClasspathUtils {

  def getDefaultClassPathEntries: Seq[Path] = System.getProperty("java.class.path")
    .split(File.pathSeparator).map(x => Paths.get(x))

  def getClassPathEntriesOpt(cl: ClassLoader): Option[Seq[Path]] = cl match {
    case cl: URLClassLoader => Some(cl.getURLs.map(x => Paths.get(x.toURI)))
    case _ => None
  }

  def getClassPathEntries(cl: ClassLoader): Seq[Path] = getClassPathEntriesOpt(cl)
    .getOrElse(getDefaultClassPathEntries)

}

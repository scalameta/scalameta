package scala.meta.internal.io

import scala.meta.io.AbsolutePath

/** A single file element of a classpath.
  *
  * @param path The path to the file, either on disk or inside a jar file system.
  * @param enclosingJar The path to the jar file containing this file, if any.
  * @param enclosingManifestJar The manifest jar file linking to the enclosing jar, if any.
  */
final case class ClasspathFile(
    path: AbsolutePath,
    enclosingJar: Option[AbsolutePath],
    enclosingManifestJar: Option[ClasspathFile]
) {
  def pathOnDisk: AbsolutePath = enclosingJar.getOrElse(path)
}

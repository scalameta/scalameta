package scala.meta
package artifacts

import java.io.File
import scala.language.implicitConversions
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.collections._
import org.scalameta.data._

@data class Path(path: String) { override def toString = "Path(\"" + path + "\")" }
object Path {
  implicit def stringIsPath(s: String): Path = apply(s)
  implicit def fileIsPath(file: File): Path = apply(file)

  def apply(file: File): Path = apply(file.toString)
  def apply(path: String): Path = new Path(path)
}

@data class Multipath(paths: Path*) { override def toString = "Multipath(\"" + paths.map(_.path).mkString(File.pathSeparator) + "\")" }
object Multipath {
  implicit def stringIsMultipath(s: String): Multipath = apply(s)
  implicit def fileIsMultipath(file: File): Multipath = apply(file)
  implicit def nilIsMultipath(nil: Nil.type): Multipath = apply(Nil)
  implicit def stringsIsMultipath(ss: Seq[String])(implicit hack1: OverloadHack1): Multipath = apply(ss: _*)
  implicit def filesIsMultipath(files: Seq[File])(implicit hack2: OverloadHack2): Multipath = apply(files: _*)

  def apply(s: String): Multipath = apply(s.split(File.pathSeparatorChar): _*)
  def apply(file: File): Multipath = apply(List(Path(file)): _*)
  def apply(): Multipath = apply(List[Path](): _*)
  def apply(nil: Nil.type): Multipath = apply(List[Path](): _*)
  def apply(ss: String*)(implicit hack1: OverloadHack1): Multipath = apply(ss.map(Path.apply): _*)
  def apply(files: File*)(implicit hack2: OverloadHack2): Multipath = apply(files.map(Path.apply): _*)
  def apply(paths: Path*)(implicit hack2: OverloadHack3): Multipath = new Multipath(paths: _*)
}
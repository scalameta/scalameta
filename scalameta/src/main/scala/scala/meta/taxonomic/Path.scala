package scala.meta
package taxonomic

import java.io.File
import java.net.URI
import scala.{Seq => _}
import scala.language.implicitConversions
import scala.collection.immutable.Seq
import org.scalameta.collections._

final case class Path(uri: URI)
object Path {
  implicit def stringIsPath(s: String): Path = apply(s)
  implicit def fileIsPath(file: File): Path = apply(file)
  implicit def uriIsPath(uri: URI): Path = apply(uri)

  def apply(s: String): Path = apply(new URI(s))
  def apply(file: File): Path = apply(file.toURI)
}

final case class Multipath(paths: Seq[Path])
object Multipath {
  implicit def stringIsMultipath(s: String): Multipath = apply(s)
  implicit def fileIsMultipath(file: File): Multipath = apply(file)
  implicit def uriIsMultipath(uri: URI): Multipath = apply(uri)
  implicit def nilIsMultipath(nil: Nil.type): Multipath = apply(Nil)
  implicit def stringsIsMultipath(ss: Seq[String])(implicit hack1: OverloadHack1): Multipath = apply(ss)
  implicit def filesIsMultipath(files: Seq[File])(implicit hack2: OverloadHack2): Multipath = apply(files)
  implicit def urisIsMultipath(uris: Seq[URI])(implicit hack3: OverloadHack3): Multipath = apply(uris)

  def apply(s: String): Multipath = parse(s)
  def apply(file: File): Multipath = parse(file.getAbsolutePath)
  def apply(uri: URI): Multipath = parse(uri.toString)
  def apply(nil: Nil.type): Multipath = parse("")
  def apply(ss: Seq[String])(implicit hack1: OverloadHack1): Multipath = parse(ss.mkString(File.pathSeparator))
  def apply(files: Seq[File])(implicit hack2: OverloadHack2): Multipath = parse(files.map(_.getAbsolutePath).mkString(File.pathSeparator))
  def apply(uris: Seq[URI])(implicit hack3: OverloadHack3): Multipath = parse(uris.map(_.toString).mkString(File.pathSeparator))

  private def parse(s: String): Multipath = {
    ???
  }
}
package scala.meta
package io

import scala.language.implicitConversions

import java.net._
import java.io._
import java.util.zip._

import org.scalameta.adt._
import scala.collection.mutable
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.internal.io.PathIO.pathSeparator

import org.scalameta.logger

@root
trait Multipath {
  def shallow: Seq[AbsolutePath]
  def syntax: String = shallow match {
    case cwd :: Nil if cwd == PathIO.workingDirectory => "\".\""
    case _ => shallow.mkString(pathSeparator)
  }
  def deep: List[Fragment] = {
    var buf = mutable.LinkedHashSet[Fragment]()
    shallow.foreach { base =>
      def exploreJar(base: AbsolutePath): Unit = {
        if (scala.meta.internal.platform.isJS) {
          // Dead code eliminate
        } else {
          val stream = new FileInputStream(base.toFile)
          try {
            val zip = new ZipInputStream(stream)
            var entry = zip.getNextEntry
            while (entry != null) {
              if (!entry.getName.endsWith("/")) {
                val name = RelativePath(entry.getName.stripPrefix("/"))
                buf += new Fragment(base, name)
              }
              entry = zip.getNextEntry
            }
          } catch {
            case ex: IOException =>
            // NOTE: If we fail to read the zip file, this shouldn't crash exploration.
            // We may want to revisit this decision later.
          } finally {
            stream.close()
          }
        }
      }
      if (base.isDirectory) {
        logger.elem(base)
        FileIO
          .listAllFilesRecursively(base)
          .files
          .foreach(relpath => buf += new Fragment(base, relpath))
      } else if (base.isFile && base.toString.endsWith(".jar")) {
        exploreJar(base)
      } else {
        sys.error(s"Obtained file $base. Expected directory.")
      }
    }
    buf.toList
  }

  def relativize(uri: URI): Option[RelativePath] = {
    deep.find(_.uri == uri).map(_.name)
  }

  def find(name: RelativePath): Option[URI] = {
    deep.find(_.name == name).map(_.uri)
  }
}

@leaf
class Classpath(shallow: Seq[AbsolutePath]) extends Multipath {
  def structure: String = s"""Classpath("$syntax")"""
  override def toString: String = syntax
}
object Classpath {
  // NOTE: These methods are duplicated in Classpath and Sourcepath.
  // The @leaf annotation should synthesize at least the default apply(List[Path])
  // but it doesn't seem to do that for some reason.
  def apply(paths: Seq[AbsolutePath]): Classpath =
    new Classpath(paths)
  def apply(path: AbsolutePath): Classpath =
    new Classpath(List(path))
  // NOTE: This constructor is inherently unsafe and escapes the entire safety
  // provided by Absolute vs. Relative paths. This constructor will crash if the
  // argument is not an absolute path.
  def apply(value: String): Classpath = {
    new Classpath(value.split(pathSeparator).map(AbsolutePath.apply))
  }
}

@leaf
class Sourcepath(shallow: Seq[AbsolutePath]) extends Multipath {
  def structure: String = s"""Sourcepath("$syntax")"""
  override def toString: String = syntax
}
object Sourcepath {
  def workingDirectory = new Sourcepath(List(PathIO.workingDirectory))
  def apply(paths: Seq[AbsolutePath]): Sourcepath =
    new Sourcepath(paths)
  def apply(path: AbsolutePath): Sourcepath =
    new Sourcepath(List(path))
  def apply(value: String): Sourcepath = {
    if (value == ".") workingDirectory
    else new Sourcepath(value.split(pathSeparator).map(AbsolutePath.apply))
  }
}

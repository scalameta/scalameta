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
  def value: Seq[AbsolutePath]
  def syntax: String = value match {
    case cwd :: Nil if cwd == PathIO.workingDirectory => "\".\""
    case _ => value.mkString(pathSeparator)
  }
  def shallow: Seq[AbsolutePath] = value.flatMap(path => FileIO.listFiles(path))
  def deep: List[Fragment] = {
    var buf = mutable.LinkedHashSet[Fragment]()
    value.foreach { base =>
      def exploreJar(base: AbsolutePath): Unit = {
        if (scala.meta.internal.platform.isJS) return
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
      if (base.isDirectory) {
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
class Classpath(value: Seq[AbsolutePath]) extends Multipath {
  def structure: String = s"""Classpath("$syntax")"""
  override def toString: String = syntax
}
// NOTE: This empty companion object is necessary for some reason to avoid a
// "classnotfound exception: Classpath$". It seems to be a bug in the @leaf
// macro annotation.
object Classpath {
  def apply(value: String): Classpath = {
    new Classpath(value.split(pathSeparator).map(AbsolutePath.apply))
  }
}

@leaf
class Sourcepath(value: Seq[AbsolutePath]) extends Multipath {
  def structure: String = s"""Sourcepath("$syntax")"""
  override def toString: String = syntax
}
// NOTE: same comment as for Classpath.
object Sourcepath {
  def workingDirectory = new Sourcepath(List(PathIO.workingDirectory))
  def apply(path: AbsolutePath): Sourcepath =
    new Sourcepath(List(path))
  def apply(value: String): Sourcepath = {
    if (value == ".") workingDirectory
    else new Sourcepath(value.split(pathSeparator).map(AbsolutePath.apply))
  }
}

package scala.meta
package io

import scala.language.implicitConversions
import java.net._
import java.io._
import java.util.zip._
import org.scalameta.adt._
import scala.collection.mutable
import scala.meta.internal.io.PathIO.pathSeparator

@root trait Multipath {
  def value: String

  def shallow: List[File] = value.split(pathSeparator).map(s => new File(s)).toList
  def deep: List[Fragment] = {
    var buf = mutable.ListBuffer[Fragment]()
    shallow.distinct.foreach(base => {
      def exploreDir(base: File): Unit = {
        def loop(file: File): Unit = {
          if (file.isDirectory) {
            val files = file.listFiles
            if (files != null) files.foreach(loop)
          } else {
            val name = AbsolutePath(file).toRelative(base)
            buf += Fragment(AbsolutePath(base), name)
          }
        }
        loop(base)
      }
      def exploreJar(base: File): Unit = {
        val stream = new FileInputStream(base)
        try {
          val zip = new ZipInputStream(stream)
          var entry = zip.getNextEntry()
          while (entry != null) {
            if (!entry.getName.endsWith("/")) {
              val name = RelativePath(entry.getName.stripPrefix("/"))
              buf += Fragment(AbsolutePath(base), name)
            }
            entry = zip.getNextEntry()
          }
        } catch {
          case ex: IOException =>
            // NOTE: If we fail to read the zip file, this shouldn't crash exploration.
            // We may want to revisit this decision later.
        } finally {
          stream.close()
        }
      }
      if (base.isDirectory) exploreDir(base)
      else if (base.isFile && base.getName.endsWith(".jar")) exploreJar(base)
    })
    buf.toList
  }

  def relativize(uri: URI): Option[RelativePath] = {
    deep.find(_.uri == uri).map(_.name)
  }

  def find(name: RelativePath): Option[URI] = {
    deep.find(_.name == name).map(_.uri)
  }
}

@leaf class Classpath(value: String) extends Multipath {
  def syntax: String = value
  def structure: String = s"""Classpath("$value")"""
  override def toString: String = syntax
}

@leaf class Sourcepath(value: String) extends Multipath {
  def syntax: String = value
  def structure: String = s"""Sourcepath("$value")"""
  override def toString: String = syntax
}

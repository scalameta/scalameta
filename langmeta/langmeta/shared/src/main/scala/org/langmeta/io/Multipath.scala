package org.langmeta
package io

import java.net._
import java.io._
import java.io.File.pathSeparator
import java.nio.file.FileVisitor
import java.nio.file.Files
import java.nio.file.Path
import java.util.zip._
import scala.collection.mutable
import org.langmeta.internal.io.FileIO
import org.langmeta.internal.io.PathIO

sealed trait Multipath {
  def shallow: List[AbsolutePath]
  def syntax: String = shallow.mkString(pathSeparator)
  def deep: List[Fragment] = {
    var buf = mutable.LinkedHashSet[Fragment]()
    shallow.foreach { base =>
      def exploreJar(base: AbsolutePath): Unit = {
        if (org.langmeta.internal.platform.isJS) {
          throw new UnsupportedEncodingException("Unzipping jars is not yet supported in JS.")
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
        FileIO
          .listAllFilesRecursively(base)
          .files
          .foreach(relpath => buf += new Fragment(base, relpath))
      } else if (base.isFile) {
        if (base.toString.endsWith(".jar")) {
          exploreJar(base)
        } else {
          sys.error(s"Obtained non-jar file $base. Expected directory or *.jar file.")
        }
      } else {
        // Skip
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

  def visit(getVisitor: AbsolutePath => FileVisitor[Path]): Unit = {
    shallow.foreach { base =>
      val path = base.toNIO
      if (Files.isDirectory(path)) {
        val visitor = getVisitor(base)
        Files.walkFileTree(path, visitor)
      } else {
        PathIO.extension(path) match {
          case "jar" =>
            val root = FileIO.jarRootPath(base)
            val visitor = getVisitor(root)
            Files.walkFileTree(root.toNIO, visitor)
          case _ =>
            sys.error(s"Expected jar file, obtained $base")
        }
      }
    }
  }

}

final case class Classpath(shallow: List[AbsolutePath]) extends Multipath {
  def structure: String = s"""Classpath("$syntax")"""
  override def toString: String = syntax
}
object Classpath {
  def apply(path: AbsolutePath): Classpath =
    new Classpath(List(path))
  // NOTE: This constructor is inherently unsafe and escapes the entire safety
  // provided by Absolute vs. Relative paths. This constructor will crash if the
  // argument is not an absolute path.
  def apply(value: String): Classpath = {
    new Classpath(value.split(pathSeparator).map(AbsolutePath(_)).toList)
  }
}

final case class Sourcepath(shallow: List[AbsolutePath]) extends Multipath {
  def structure: String = s"""Sourcepath("$syntax")"""
  override def toString: String = syntax
}
object Sourcepath {
  def apply(path: AbsolutePath): Sourcepath =
    new Sourcepath(List(path))
  def apply(value: String): Sourcepath =
    new Sourcepath(value.split(pathSeparator).map(AbsolutePath(_)).toList)
}

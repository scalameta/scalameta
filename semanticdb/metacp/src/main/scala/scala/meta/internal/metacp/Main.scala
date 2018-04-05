package scala.meta.internal.metacp

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.JavaConverters._
import scala.meta.internal.index._
import scala.meta.internal.javacp._
import scala.meta.internal.scalacp._
import scala.meta.metacp._
import scala.util.control.NonFatal
import org.langmeta.internal.io._
import org.langmeta.io._
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.GenSeq

class Main(settings: Settings, reporter: Reporter) {
  def process(): Option[Classpath] = {
    val success = new AtomicBoolean(true)

    val classpath: GenSeq[AbsolutePath] =
      if (settings.par) settings.classpath.shallow.par
      else settings.classpath.shallow

    val buffer = new ConcurrentLinkedQueue[AbsolutePath]()

    classpath.foreach { entry =>
      if (entry.isDirectory) {
        val out = AbsolutePath(Files.createTempDirectory("semanticdb"))
        val res = convertClasspathEntry(entry, out)
        success.compareAndSet(true, res)
        buffer.add(out)
      } else if (entry.isFile) {
        val cacheEntry = {
          val base = settings.cacheDir
          val checksum = Checksum(entry)
          base.resolve(entry.toFile.getName.stripSuffix(".jar") + "-" + checksum + ".jar")
        }
        if (cacheEntry.toFile.exists) {
          buffer.add(cacheEntry)
        } else {
          PlatformFileIO.withJarFileSystem(cacheEntry, create = true) { out =>
            val res = convertClasspathEntry(entry, out)
            success.compareAndSet(true, res)
            buffer.add(cacheEntry)
          }
        }
      }
    }

    if (settings.scalaLibrarySynthetics) {
      val cacheEntry = settings.cacheDir.resolve("scala-library-synthetics.jar")
      if (cacheEntry.toFile.exists) {
        buffer.add(cacheEntry)
      } else {
        PlatformFileIO.withJarFileSystem(cacheEntry, create = true) { out =>
          val res = dumpScalaLibrarySynthetics(out)
          success.compareAndSet(true, res)
          buffer.add(cacheEntry)
        }
      }
    }

    if (success.get) {
      import scala.collection.JavaConverters._
      Some(Classpath(buffer.iterator().asScala.toList))
    } else {
      None
    }
  }

  private def convertClasspathEntry(in: AbsolutePath, out: AbsolutePath): Boolean = {
    var success = true
    val index = new Index
    val classpath = Classpath(in)
    classpath.visit { base =>
      new SimpleFileVisitor[Path] {
        override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(path) == "class") {
            try {
              val abspath = AbsolutePath(path)
              val node = abspath.toClassNode
              val result = {
                val attrs = if (node.attrs != null) node.attrs.asScala else Nil
                if (attrs.exists(_.`type` == "ScalaSig")) {
                  val classfile = ToplevelClassfile(base, abspath, node)
                  Scalacp.parse(classfile)
                } else if (attrs.exists(_.`type` == "Scala")) {
                  None
                } else {
                  val innerClassNode = node.innerClasses.asScala.find(_.name == node.name)
                  if (innerClassNode.isEmpty) {
                    val classfile = ToplevelClassfile(base, abspath, node)
                    Javacp.parse(classfile)
                  } else {
                    None
                  }
                }
              }
              result.foreach { infos =>
                index.append(infos.uri, infos.toplevels)
                infos.save(out)
              }
            } catch {
              case NonFatal(ex) =>
                reporter.out.println(s"error: can't convert $path")
                ex.printStackTrace(reporter.out)
                success = false
            }
          }
          FileVisitResult.CONTINUE
        }
      }
    }
    index.save(out)
    success
  }

  private def dumpScalaLibrarySynthetics(out: AbsolutePath): Boolean = {
    val index = new Index
    val synthetics = List(
      Scalalib.anyClass,
      Scalalib.anyValClass,
      Scalalib.anyRefClass,
      Scalalib.nothingClass,
      Scalalib.nullClass)
    synthetics.foreach { infos =>
      index.append(infos.uri, infos.toplevels)
      infos.save(out)
    }
    index.save(out)
    true
  }
}

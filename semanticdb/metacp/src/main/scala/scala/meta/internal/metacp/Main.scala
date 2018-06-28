package scala.meta.internal.metacp

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.jar._
import scala.collection.JavaConverters._
import scala.meta.cli._
import scala.meta.internal.index._
import scala.meta.internal.javacp._
import scala.meta.internal.scalacp._
import scala.meta.internal.io._
import scala.meta.io._
import scala.meta.metacp._
import scala.util.control.NonFatal
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.GenSeq

class Main(settings: Settings, reporter: Reporter) {

  lazy val classpathIndex = ClasspathIndex(settings.fullClasspath)

  def process(): Option[Classpath] = {
    val success = new AtomicBoolean(true)

    val classpath: GenSeq[AbsolutePath] =
      if (settings.par) settings.classpath.entries.par
      else settings.classpath.entries

    val buffer = new ConcurrentLinkedQueue[AbsolutePath]()

    def createCachedJar(cacheEntry: AbsolutePath)(f: AbsolutePath => Boolean): Unit =
      MetacpGlobalCache.computeIfAbsent(cacheEntry.toNIO) { tmp =>
        PlatformFileIO.withJarFileSystem(AbsolutePath(tmp), create = true) { out =>
          val res = f(out)
          success.compareAndSet(true, res)
          buffer.add(cacheEntry)
        }
      }

    if (!Files.exists(Files.createDirectories(settings.cacheDir.toNIO))) {
      Files.createDirectories(settings.cacheDir.toNIO)
    }

    classpath.foreach { entry =>
      if (entry.isDirectory) {
        val out = AbsolutePath(Files.createTempDirectory("semanticdb"))
        val index = new Index
        val res = convertClasspathEntry(entry, out, index)
        index.save(out)
        success.compareAndSet(true, res)
        buffer.add(out)
      } else if (entry.isFile) {
        val cacheEntry = {
          val checksum = Checksum(entry)
          settings.cacheDir.resolve(
            entry.toFile.getName.stripSuffix(".jar") + "-" + checksum + ".jar")
        }
        if (cacheEntry.toFile.exists) {
          buffer.add(cacheEntry)
        } else {
          createCachedJar(cacheEntry) { out =>
            val index = new Index
            def loop(entry: AbsolutePath): Boolean = {
              var result = convertClasspathEntry(entry, out, index)
              if (entry.isFile) {
                val jar = new JarFile(entry.toFile)
                val manifest = jar.getManifest
                if (manifest != null) {
                  val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
                  if (classpathAttr != null) {
                    classpathAttr.split(" ").foreach { classpathEntry =>
                      val path = entry.toNIO.getParent.resolve(classpathEntry)
                      result &= loop(AbsolutePath(path))
                    }
                  }
                }
              }
              result
            }
            val result = loop(entry)
            index.save(out)
            result
          }
        }
      }
    }

    if (settings.scalaLibrarySynthetics) {
      val cacheTarget = settings.cacheDir.resolve("scala-library-synthetics.jar")
      if (cacheTarget.toFile.exists) {
        buffer.add(cacheTarget)
      } else {
        createCachedJar(cacheTarget)(dumpScalaLibrarySynthetics)
      }
    }

    if (success.get) {
      import scala.collection.JavaConverters._
      Some(Classpath(buffer.iterator().asScala.toList))
    } else {
      None
    }
  }

  private def convertClasspathEntry(in: AbsolutePath, out: AbsolutePath, index: Index): Boolean = {
    var success = true
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
                  Scalacp.parse(classfile, settings, classpathIndex)
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
    success
  }

  private def dumpScalaLibrarySynthetics(out: AbsolutePath): Boolean = {
    val index = new Index
    val synthetics = List(
      Scalalib.anyClass,
      Scalalib.anyValClass,
      Scalalib.anyRefClass,
      Scalalib.nothingClass,
      Scalalib.nullClass,
      Scalalib.singletonTrait)
    synthetics.foreach { infos =>
      index.append(infos.uri, infos.toplevels)
      infos.save(out)
    }
    index.save(out)
    true
  }
}

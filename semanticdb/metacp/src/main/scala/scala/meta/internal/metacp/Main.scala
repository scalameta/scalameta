package scala.meta.internal.metacp

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.jar._
import scala.collection.JavaConverters._
import scala.meta.cli._
import scala.meta.internal.classpath._
import scala.meta.internal.scalacp._
import scala.meta.internal.io._
import scala.meta.io._
import scala.meta.metacp._
import scala.util.control.NonFatal
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.GenSeq
import scala.collection.mutable

class Main(settings: Settings, reporter: Reporter) {

  val classpathIndex = ClasspathIndex(settings.fullClasspath)
  private val missingSymbols = mutable.Set.empty[String]
  private val outRoot: AbsolutePath =
    if (PathIO.extension(settings.out.toNIO) == "jar") {
      PlatformFileIO.jarRootPath(settings.out, create = true)
    } else {
      if (!settings.out.isDirectory) Files.createDirectories(settings.out.toNIO)
      settings.out
    }

  def process(): Option[Classpath] = {
    val success = new AtomicBoolean(true)

    val classpath: GenSeq[AbsolutePath] =
      if (settings.par) settings.classpath.entries.par
      else settings.classpath.entries

    classpath.foreach { entry =>
      if (entry.isDirectory) {
        success.compareAndSet(true, convertClasspathEntry(entry, outRoot))
      } else if (entry.isFile) {
        def loop(entry: AbsolutePath): Unit = {
          success.compareAndSet(true, convertClasspathEntry(entry, outRoot))
          if (entry.isFile) {
            val jar = new JarFile(entry.toFile)
            val manifest = jar.getManifest
            if (manifest != null) {
              val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
              if (classpathAttr != null) {
                classpathAttr.split(" ").foreach { classpathEntry =>
                  val linkedPath = entry.toNIO.getParent.resolve(classpathEntry)
                  val linkedEntry = AbsolutePath(linkedPath)
                  if (linkedEntry.isFile || linkedEntry.isDirectory) {
                    loop(linkedEntry)
                  }
                }
              }
            }
          }
        }
        loop(entry)
      }
    }

    if (settings.scalaLibrarySynthetics) {
      Scalalib.synthetics.foreach { infos =>
        infos.save(outRoot)
      }
    }

    if (success.get) {
      Some(Classpath(settings.out))
    } else {
      if (missingSymbols.nonEmpty) {
        reporter.out.println(
          "NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. " +
            "The provided classpath or classpaths should include the Scala library as well as JDK jars such as rt.jar."
        )
      }
      None
    }
  }

  private def convertClasspathEntry(in: AbsolutePath, out: AbsolutePath): Boolean = {
    var success = true
    val classpath = Classpath(in)
    classpath.visit { _ =>
      new SimpleFileVisitor[Path] {
        override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(path) == "class") {
            try {
              val abspath = AbsolutePath(path)
              val node = abspath.toClassNode
              val result = ClassfileInfos.fromClassNode(node, classpathIndex)
              result.foreach { infos =>
                infos.save(out)
              }
            } catch {
              case e @ MissingSymbolException(symbol) =>
                if (!missingSymbols(symbol)) {
                  missingSymbols += symbol
                  reporter.out.println(e.getMessage)
                  success = false
                }
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
}

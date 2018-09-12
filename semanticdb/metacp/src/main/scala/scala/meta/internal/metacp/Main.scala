package scala.meta.internal.metacp

import java.io.BufferedOutputStream
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.ConcurrentHashMap
import java.util.jar._
import scala.collection.immutable
import scala.collection.JavaConverters._
import scala.meta.cli._
import scala.meta.internal.cli._
import scala.meta.internal.classpath._
import scala.meta.internal.scalacp._
import scala.meta.internal.io._
import scala.meta.io._
import scala.meta.metacp._
import scala.collection.GenSeq
import scala.collection.mutable

class Main(settings: Settings, reporter: Reporter) {

  val classpathIndex =
    ClasspathIndex(settings.classpath ++ settings.dependencyClasspath ++ detectJavacp)
  private val missingSymbols = mutable.Set.empty[String]

  def process(): Result = {
    if (settings.out.isFile) {
      throw new FileAlreadyExistsException(settings.out.toString, null, "--out must not be a file")
    } else if (!settings.out.isDirectory) {
      Files.createDirectories(settings.out.toNIO)
    }

    val classpath: GenSeq[AbsolutePath] =
      if (settings.par) settings.classpath.entries.par
      else settings.classpath.entries

    val status = new ConcurrentHashMap[AbsolutePath, Option[AbsolutePath]]()
    def processEntry(entry: AbsolutePath): OutputEntry = {
      withOutputEntry(entry) { out =>
        val isSuccess = convertClasspathEntry(entry, out.root)
        if (isSuccess) status.put(entry, Some(out.output))
        else status.put(entry, None)
        out
      }
    }

    val job = Job(classpath, if (settings.verbose) reporter.err else devnull)
    job.foreach { entry =>
      val out = processEntry(entry)
      if (entry.isFile) {
        val jar = new JarFile(entry.toFile)
        try {
          val manifest = jar.getManifest
          if (manifest != null) {
            val isSuccess = processManifest(entry, manifest, out.output)
            if (!isSuccess) status.put(entry, None)
          }
        } finally {
          jar.close()
        }
      }
    }

    val scalaLibrarySynthetics = {
      if (settings.scalaLibrarySynthetics) {
        withOutputEntry(settings.out.resolve("scala-library-synthetics.jar")) { out =>
          Scalalib.synthetics.foreach { infos =>
            infos.save(out.root)
          }
          Some(out.output)
        }
      } else {
        None
      }
    }

    if (missingSymbols.nonEmpty) {
      reporter.err.println(
        "NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. " +
          "The provided classpath or classpaths should include the Scala library as well as JDK jars such as rt.jar."
      )
    }

    reporter.out.println("{")
    reporter.out.println("  \"status\": {")
    val ins = settings.classpath.entries
    ins.zipWithIndex.foreach {
      case (in, i) =>
        val s_out = status.get(in).map(_.toString).getOrElse("")
        reporter.out.print(s"""    "${in.toNIO}": "${s_out}"""")
        if (i != ins.length - 1) reporter.out.print(",")
        reporter.out.println()
    }
    reporter.out.println("  },")
    val s_out = scalaLibrarySynthetics.map(_.toString).getOrElse("")
    reporter.out.println(s"""  "scalaLibrarySynthetics": "${s_out}"""")
    reporter.out.println("}")

    val orderedStatus = immutable.ListMap(ins.map(in => in -> status.get(in)): _*)
    Result(orderedStatus, scalaLibrarySynthetics)
  }

  private def processManifest(entry: AbsolutePath, manifest: Manifest, out: AbsolutePath): Boolean = {
    var success = true
    val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
    if (classpathAttr != null) {
      val buf = List.newBuilder[Path]
      classpathAttr.split(" ").foreach { classpathEntry =>
        val linkedPath = entry.toNIO.getParent.resolve(classpathEntry)
        val linkedEntry = AbsolutePath(linkedPath)
        if (linkedEntry.isFile || linkedEntry.isDirectory) {
          withOutputEntry(linkedEntry) { out =>
            buf += out.output.toNIO.getFileName
            success &= convertClasspathEntry(linkedEntry, out.root)
          }
        }
      }
      val convertedLinkedJars = buf.result()
      if (convertedLinkedJars.nonEmpty) {
        withJar(out.toNIO) { jos =>
          jos.putNextEntry(new JarEntry("META-INF/MANIFEST.MF"))
          val classPath = convertedLinkedJars.mkString(" ")
          val manifest =
            s"""|Manifest-Version: 1.0
                |Class-Path: $classPath
                |""".stripMargin.trim + "\n\n"
          jos.write(manifest.getBytes(StandardCharsets.UTF_8))
          jos.closeEntry()
        }
      }
    }
    success
  }

  private def withJar(path: Path)(fn: JarOutputStream => Unit): Unit = {
    val os = Files.newOutputStream(path)
    val bos = new BufferedOutputStream(os)
    val jos = new JarOutputStream(bos)
    try fn(jos)
    finally {
      jos.close()
      bos.close()
      os.close()
    }
  }

  /** An output entry that is either a directory or a jar.
    *
    * @param output the output directory or jar file on disk that is returned to the user.
    * @param root the output directory or the NIO FileSystem jar root path if output is a jar file.
    */
  private case class OutputEntry(output: AbsolutePath, root: AbsolutePath)

  private def withOutputEntry[T](entry: AbsolutePath)(f: OutputEntry => T): T = {
    val name = entry.toNIO.normalize().getFileName.toString
    if (PathIO.extension(entry.toNIO) == "jar") {
      val freeJar = jarNameAlternatives(name, 0).filter(!_.isFile).head
      PlatformFileIO.withJarFileSystem(freeJar, create = true) { jarRoot =>
        f(OutputEntry(freeJar, jarRoot))
      }
    } else {
      val freeDir = directoryNameAlternatives(name, 0).filter(!_.isDirectory).head
      Files.createDirectories(freeDir.toNIO)
      f(OutputEntry(freeDir, freeDir))
    }
  }

  private def directoryNameAlternatives(filename: String, i: Int): Stream[AbsolutePath] = {
    val name = if (i == 0) filename else filename + "-" + i
    settings.out.resolve(name) #:: directoryNameAlternatives(filename, i + 1)
  }

  private def jarNameAlternatives(filename: String, i: Int): Stream[AbsolutePath] = {
    val name = if (i == 0) filename else (filename.stripSuffix(".jar") + "-" + i) + ".jar"
    settings.out.resolve(name) #:: jarNameAlternatives(filename, i + 1)
  }

  private def convertClasspathEntry(in: AbsolutePath, out: AbsolutePath): Boolean = {
    var success = true
    val classpath = Classpath(in)
    classpath.visit { _ =>
      new SimpleFileVisitor[Path] {
        override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(path) == "class" && Files.size(path) > 0) {
            try {
              val abspath = AbsolutePath(path)
              val node = abspath.toClassNode
              val result = ClassfileInfos.fromClassNode(node, classpathIndex, settings, reporter)
              result.foreach { infos =>
                infos.save(out)
              }
            } catch {
              case e @ MissingSymbolException(symbol) =>
                if (!missingSymbols(symbol)) {
                  missingSymbols += symbol
                  reporter.err.println(s"${e.getMessage} in $in")
                  success = false
                }
              case ex: Throwable =>
                reporter.err.println(s"error: can't convert $path in $in")
                ex.printStackTrace(reporter.err)
                success = false
            }
          }
          FileVisitResult.CONTINUE
        }
      }
    }
    // NOTE: In the case when an input contains no class files,
    // we need to create an empty META-INF/semanticdb directory to distinguish
    // metacp-processed outputs from regular class directories and/or jar.
    val semanticdbRoot = out.resolve("META-INF").resolve("semanticdb")
    Files.createDirectories(semanticdbRoot.toNIO)
    success
  }

  private def detectJavacp: Classpath = {
    if (settings.usejavacp) {
      val jdk = sys.props
        .collectFirst {
          case (k, v) if k.endsWith(".boot.class.path") =>
            Classpath(v).entries.filter(_.isFile)
        }
        .getOrElse {
          throw new IllegalStateException("Unable to detect bootclasspath via --usejavacp")
        }
      val scalaLibrary = this.getClass.getClassLoader match {
        case loader: URLClassLoader =>
          loader.getURLs
            .collectFirst {
              case url if url.toString.contains("scala-library") =>
                AbsolutePath(Paths.get(url.toURI))
            }
            .getOrElse {
              throw new IllegalStateException("Unable to detect scala-library via --usejavacp")
            }
        case unexpected =>
          throw new IllegalStateException(
            s"Expected this.getClass.getClassLoader to be URLClassLoader. " +
              s"Obtained $unexpected")
      }
      Classpath(scalaLibrary :: jdk)
    } else {
      Classpath(Nil)
    }
  }
}

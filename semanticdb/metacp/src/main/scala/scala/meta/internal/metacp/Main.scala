package scala.meta.internal.metacp

import scala.meta.cli._
import scala.meta.internal.classpath._
import scala.meta.internal.cli._
import scala.meta.internal.io._
import scala.meta.internal.scalacp._
import scala.meta.io._
import scala.meta.metacp._

import java.io.BufferedOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.jar._

import scala.collection.{immutable, mutable}

class Main(settings: Settings, reporter: Reporter) {

  val classpathIndex = ClasspathIndex(
    settings.classpath ++ settings.dependencyClasspath ++ detectJavacp,
    includeJdk = settings.includeJdk,
    reporter,
  )
  private val missingSymbols = mutable.Set.empty[String]

  def process(): Result = {
    if (settings.out.isFile)
      throw new FileAlreadyExistsException(settings.out.toString, null, "--out must not be a file")
    if (!settings.out.isDirectory) Files.createDirectories(settings.out.toNIO)

    val classpath = settings.classpath.entries
    val cpargs = classpath.zip(settings.classpathArgs).zipWithIndex

    val outputs = new Array[Option[AbsolutePath]](classpath.length)

    Job(cpargs, if (settings.verbose) reporter.err else devnull).foreach { case ((entry, arg), i) =>
      val out = withOutputEntry(entry) { x =>
        outputs(i) = if (convertClasspathEntry(entry, arg, x.root)) Some(x.output) else None
        x.output
      }
      if (entry.isFile) {
        val jar = new JarFile(entry.toFile)
        try {
          val manifest = jar.getManifest
          if (manifest != null) if (!processManifest(entry, manifest, out)) outputs(i) = None
        } finally jar.close()
      }
    }

    val scalaLibrarySynthetics =
      if (settings.scalaLibrarySynthetics)
        withOutputEntry(settings.out.resolve("scala-library-synthetics.jar")) { out =>
          Scalalib.synthetics(Scalalib.isHashHashNullary(classpathIndex))
            .foreach(infos => infos.save(out.root))
          Some(out.output)
        }
      else None

    if (missingSymbols.nonEmpty) reporter.err.println(
      "NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. " +
        "The provided classpath or classpaths should include the Scala library as well as JDK jars such as rt.jar.",
    )

    reporter.out.println("{")
    reporter.out.println("  \"status\": {")
    val orderedStatus = immutable.ListMap.newBuilder[AbsolutePath, Option[AbsolutePath]]
    cpargs.foreach { case ((cp, arg), i) =>
      val output = outputs(i)
      orderedStatus += cp -> output
      val s_out = output.map(_.toString).getOrElse("")
      reporter.out.print(s"""    "$arg": "$s_out"""")
      if (i != classpath.length - 1) reporter.out.print(",")
      reporter.out.println()
    }
    reporter.out.println("  },")
    val s_out = scalaLibrarySynthetics.map(_.toString).getOrElse("")
    reporter.out.println(s"""  "scalaLibrarySynthetics": "$s_out"""")
    reporter.out.println("}")

    Result(orderedStatus.result(), scalaLibrarySynthetics)
  }

  private def processManifest(entry: AbsolutePath, manifest: Manifest, out: AbsolutePath): Boolean = {
    var success = true
    val classpathAttr = manifest.getMainAttributes.getValue("Class-Path")
    if (classpathAttr != null) {
      val buf = List.newBuilder[Path]
      classpathAttr.split(" ").foreach { classpathEntry =>
        val linkedPath = entry.toNIO.getParent.resolve(classpathEntry)
        val linkedEntry = AbsolutePath(linkedPath)
        if (linkedEntry.isFile || linkedEntry.isDirectory) withOutputEntry(linkedEntry) { out =>
          buf += out.output.toNIO.getFileName
          success &= convertClasspathEntry(linkedEntry, linkedEntry.toString, out.root)
        }
      }
      val convertedLinkedJars = buf.result()
      if (convertedLinkedJars.nonEmpty) withJar(out.toNIO) { jos =>
        jos.putNextEntry(new JarEntry("META-INF/MANIFEST.MF"))
        val classPath = convertedLinkedJars.mkString(" ")
        val manifest =
          s"""|Manifest-Version: 1.0
              |Class-Path: $classPath
              |""".stripMargin + "\n\n"
        jos.write(manifest.getBytes(StandardCharsets.UTF_8))
        jos.closeEntry()
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

  /**
   * An output entry that is either a directory or a jar.
   *
   * @param output
   *   the output directory or jar file on disk that is returned to the user.
   * @param root
   *   the output directory or the NIO FileSystem jar root path if output is a jar file.
   */
  private case class OutputEntry(output: AbsolutePath, root: AbsolutePath)

  private def withOutputEntry[T](entry: AbsolutePath)(f: OutputEntry => T): T = {
    val name = entry.toNIO.normalize().getFileName.toString
    if (PathIO.extension(entry.toNIO) == "jar") {
      val freeJar = jarNameAlternatives(name, 0).filter(!_.isFile).head
      PlatformFileIO
        .withJarFileSystem(freeJar, create = true)(jarRoot => f(OutputEntry(freeJar, jarRoot)))
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
    val name = if (i == 0) filename else filename.stripSuffix(".jar") + "-" + i + ".jar"
    settings.out.resolve(name) #:: jarNameAlternatives(filename, i + 1)
  }

  private def convertClasspathEntry(in: AbsolutePath, arg: String, out: AbsolutePath): Boolean = {
    var success = true
    val classpath = Classpath(in)
    classpath.visit { _ =>
      new SimpleFileVisitor[Path] {
        override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(path) == "class" && Files.size(path) > 0)
            try {
              val abspath = AbsolutePath(path)
              val node = abspath.toClassNode
              val result = ClassfileInfos.fromClassNode(node, classpathIndex, settings, reporter)
              result.foreach(infos => infos.save(out))
            } catch {
              case e @ MissingSymbolException(symbol) => if (!missingSymbols(symbol)) {
                  missingSymbols += symbol
                  reporter.err.println(s"${e.getMessage} in $arg")
                  success = false
                }
              case ex: Throwable =>
                reporter.err.println(s"error: can't convert $path in $arg")
                ex.printStackTrace(reporter.err)
                success = false
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

  private def detectJavacp: Classpath =
    if (settings.usejavacp) {
      val scalaLibraryPath = ClasspathUtils.getClassPathEntries(this.getClass.getClassLoader)
        .collectFirst { case x if x.toString.contains("scala-library") => x }
        .getOrElse(throw new IllegalStateException("Unable to detect scala-library via --usejavacp"))
      Classpath(scalaLibraryPath)
    } else Classpath(Nil)

}

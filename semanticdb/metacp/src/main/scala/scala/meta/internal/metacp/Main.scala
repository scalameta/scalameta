package scala.meta.internal.metacp

import scala.meta.cli._
import scala.meta.internal.classpath._
import scala.meta.internal.cli._
import scala.meta.internal.io.Implicits._
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
import scala.runtime.BooleanRef

class Main(settings: Settings, reporter: Reporter) {

  import Main._

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

    Job.foreach(cpargs, if (settings.verbose) reporter.err else devnull) { case ((entry, arg), i) =>
      val out = withOutput(entry, outputName(entry)) { x =>
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
      if (settings.scalaLibrarySynthetics) {
        val entry = settings.out.resolve("scala-library-synthetics.jar")
        withOutput(entry, outputName(entry)) { out =>
          Scalalib.synthetics(Scalalib.isHashHashNullary(classpathIndex))
            .foreach(infos => infos.save(out.root))
          Some(out.output)
        }
      } else None

    if (missingSymbols.nonEmpty) reporter.err.println(
      "NOTE. To fix 'missing symbol' errors please provide a complete --classpath or --dependency-classpath. " +
        "The provided classpath or classpaths should include the Scala library as well as JDK jars such as rt.jar.",
    )

    val nl = System.lineSeparator()
    val sb = new java.lang.StringBuilder("{").append(nl).append("  \"status\": {").append(nl)
    val orderedStatus = immutable.ListMap.newBuilder[AbsolutePath, Option[AbsolutePath]]
    cpargs.foreach { case ((cp, arg), i) =>
      val output = outputs(i)
      orderedStatus += cp -> output
      sb.append("    ").json(arg).append(": ").json(output.fold("")(_.toString))
      if (i != classpath.length - 1) sb.append(',')
      sb.append(nl)
    }
    sb.append("  },").append(nl).append("  \"scalaLibrarySynthetics\": ")
      .json(scalaLibrarySynthetics.fold("")(_.toString)).append(nl).append('}').append(nl)
    reporter.out.print(sb)

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
        if (linkedEntry.isFile || linkedEntry.isDirectory)
          withOutput(linkedEntry, outputName(linkedEntry)) { out =>
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

  private def outputName(entry: AbsolutePath): AbsolutePath = {
    val name = entry.toNIO.normalize().getFileName.toString
    val base = name.stripSuffix(JarExtension)
    val ext = if (name ne base) JarExtension else ""
    val pred: AbsolutePath => Boolean = if (name ne base) !_.isFile else !_.isDirectory
    Iterator.from(0).map(i => settings.out.resolve(if (i == 0) name else s"$base-$i$ext"))
      .filter(pred).next()
  }

  private def withOutput[T](entry: AbsolutePath, output: AbsolutePath)(f: OutputEntry => T): T =
    if (isJar(entry)) PlatformFileIO
      .withJarFileSystem(output, create = true)(jarRoot => f(OutputEntry(output, jarRoot)))
    else {
      Files.createDirectories(output.toNIO)
      f(OutputEntry(output, output))
    }

  private def saveInfoVisitor(
      arg: String,
      out: AbsolutePath,
      ok: BooleanRef,
  ): SimpleFileVisitor[Path] = new SimpleFileVisitor[Path] {
    override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
      if (PathIO.fileExtension(path) == ".class" && attrs.size() > 0)
        try ClassfileInfos
            .fromClassNode(AbsolutePath(path).toClassNode, classpathIndex, settings, reporter)
            .foreach(_.save(out))
        catch {
          case ex: MissingSymbolException => if (missingSymbols.add(ex.symbol)) {
              reporter.err.println(s"${ex.getMessage} in $arg")
              ok.elem = false
            }
          case ex: Throwable =>
            reporter.err.println(s"error: can't convert $path in $arg")
            ex.printStackTrace(reporter.err)
            ok.elem = false
        }
      FileVisitResult.CONTINUE
    }
  }

  private def convertClasspathEntry(in: AbsolutePath, arg: String, out: AbsolutePath): Boolean = {
    val ok = BooleanRef.create(true)
    Classpath(in).visit(_ => saveInfoVisitor(arg, out, ok))
    // NOTE: In the case when an input contains no class files,
    // we need to create an empty META-INF/semanticdb directory to distinguish
    // metacp-processed outputs from regular class directories and/or jar.
    val semanticdbRoot = out.resolve("META-INF").resolve("semanticdb")
    Files.createDirectories(semanticdbRoot.toNIO)
    ok.elem
  }

  private def detectJavacp: Classpath =
    if (settings.usejavacp) {
      val scalaLibraryPath = ClasspathUtils.getClassPathEntries(this.getClass.getClassLoader)
        .collectFirst { case x if x.toString.contains("scala-library") => x }
        .getOrElse(throw new IllegalStateException("Unable to detect scala-library via --usejavacp"))
      Classpath(scalaLibraryPath)
    } else Classpath(Nil)

}

object Main {

  implicit class ImplicitStringBuilder(val sb: java.lang.StringBuilder) extends AnyVal {
    // escape non-ASCII too, so that stdout in any charset keeps the string
    def json(value: String): java.lang.StringBuilder = {
      sb.append('"')
      var i = 0
      while (i < value.length) {
        value.charAt(i) match {
          case '"' => sb.append("\\\"")
          case '\\' => sb.append("\\\\")
          case '\b' => sb.append("\\b")
          case '\f' => sb.append("\\f")
          case '\n' => sb.append("\\n")
          case '\r' => sb.append("\\r")
          case '\t' => sb.append("\\t")
          case c if c < ' ' || c >= '\u0080' =>
            sb.append("\\u").append(Integer.toHexString(c | 0x10000), 1, 5)
          case c => sb.append(c)
        }
        i += 1
      }
      sb.append('"')
    }
  }

  private val JarExtension = ".jar"
  private def isJar(entry: AbsolutePath): Boolean = PathIO.fileExtension(entry.toNIO) ==
    JarExtension

}

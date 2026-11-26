package scala.meta.internal.metacp

import scala.meta.cli._
import scala.meta.internal.classpath._
import scala.meta.internal.cli._
import scala.meta.internal.io._
import scala.meta.internal.scalacp._
import scala.meta.io._
import scala.meta.metacp._

import java.io.IOException
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.Locale
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
    val classpathLen = classpath.length
    val cpargs = classpath.zip(settings.classpathArgs).zipWithIndex

    val inputs = mutable.ArrayBuffer.empty[Input]
    val indices = mutable.HashMap.empty[Path, Int]
    def visit(path: AbsolutePath, arg: String): Int = {
      val key = path.toNIO.normalize()
      indices.get(key) match {
        case Some(i) => i
        case _ =>
          val i = inputs.length
          indices(key) = i
          inputs += null // reserve slot; set below after visits
          inputs(i) = Input(path, arg, manifestLinks(path).map(visit(_, arg)))
          i
      }
    }
    val files = new Array[Int](classpathLen)
    cpargs.foreach { case ((cp, arg), i) => files(i) = visit(cp, arg) }

    val outputs = new Array[AbsolutePath](inputs.length)
    val names = mutable.HashSet.empty[String]
    val reserve =
      (x: AbsolutePath) => names.add(x.toNIO.getFileName.toString.toLowerCase(Locale.ROOT))
    def name(i: Int) = if (outputs(i) eq null) outputs(i) = outputName(inputs(i).path, reserve)
    files.foreach { i =>
      name(i)
      inputs(i).links.foreach(name)
    }
    inputs.indices.foreach(name)

    val failed = new Array[Boolean](inputs.length)
    Job.foreach(inputs.indices, if (settings.verbose) reporter.err else devnull)(i =>
      failed(i) = !processInput(inputs(i), outputs(i), outputs),
    )
    def ok(i: Int, seen: mutable.BitSet): Boolean =
      if (seen.add(i)) !failed(i) && inputs(i).links.forall(ok(_, seen)) else true

    val scalaLibrarySynthetics =
      if (settings.scalaLibrarySynthetics) {
        val entry = settings.out.resolve("scala-library-synthetics.jar")
        withOutput(entry, outputName(entry, _ => true)) { out =>
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
      val file = files(i)
      val output = if (ok(file, new mutable.BitSet)) Some(outputs(file)) else None
      orderedStatus += cp -> output
      sb.append("    ").json(arg).append(": ").json(output.fold("")(_.toString))
      if (i != classpathLen - 1) sb.append(',')
      sb.append(nl)
    }
    sb.append("  },").append(nl).append("  \"scalaLibrarySynthetics\": ")
      .json(scalaLibrarySynthetics.fold("")(_.toString)).append(nl).append('}').append(nl)
    reporter.out.print(sb)

    Result(orderedStatus.result(), scalaLibrarySynthetics)
  }

  private def processInput(
      input: Input,
      output: AbsolutePath,
      outputs: Array[AbsolutePath],
  ): Boolean = withOutput(input.path, output) { out =>
    if (input.links.nonEmpty) {
      val manifest = new Manifest
      val attributes = manifest.getMainAttributes
      attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
      val classPath = input.links.map(outputs(_).toNIO.getFileName).mkString(" ")
      attributes.put(Attributes.Name.CLASS_PATH, classPath)
      val dir = Files.createDirectories(out.root.resolve("META-INF").toNIO)
      val os = Files.newOutputStream(dir.resolve("MANIFEST.MF"))
      try manifest.write(os)
      finally os.close()
    }
    convertClasspathEntry(input.path, input.arg, out.root)
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

  private def outputName(entry: AbsolutePath, reserve: AbsolutePath => Boolean): AbsolutePath = {
    val name = entry.toNIO.normalize().getFileName.toString
    val base = name.stripSuffix(JarExtension)
    val ext = if (name ne base) JarExtension else ""
    val pred: AbsolutePath => Boolean = if (name ne base) !_.isFile else !_.isDirectory
    Iterator.from(0).map(i => settings.out.resolve(if (i == 0) name else s"$base-$i$ext"))
      .filter(x => pred(x) && reserve(x)).next()
  }

  private def withOutput[T](entry: AbsolutePath, output: AbsolutePath)(f: OutputEntry => T): T =
    if (isJar(entry)) PlatformFileIO
      .withJarFileSystem(output, create = true)(jarRoot => f(OutputEntry(output, jarRoot)))
    else {
      Files.createDirectories(output.toNIO)
      f(OutputEntry(output, output))
    }

  /** The existing files and directories that the `Class-Path` of a jar's manifest lists. */
  private def manifestLinks(path: AbsolutePath): List[AbsolutePath] = {
    val jar =
      try if (path.isFile) new JarFile(path.toFile) else null
      catch { case _: IOException => null }
    val manifest =
      if (null eq jar) null
      else
        try jar.getManifest
        finally jar.close()
    val classPath =
      if (null eq manifest) null else manifest.getMainAttributes.getValue(Attributes.Name.CLASS_PATH)
    if (null eq classPath) Nil
    else {
      val dir = path.toNIO.getParent
      val res = List.newBuilder[AbsolutePath]
      classPath.split(" ").foreach { x =>
        val absPath = AbsolutePath(dir.resolve(x))
        if (absPath.isFile || absPath.isDirectory) res += absPath
      }
      res.result()
    }
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
    val walk = (x: AbsolutePath) => Files.walkFileTree(x.toNIO, saveInfoVisitor(arg, out, ok))
    if (in.isDirectory) walk(in)
    else if (in.isFile) {
      if (!isJar(in)) sys.error(s"Expected jar file, obtained $in")
      PlatformFileIO.withJarFileSystem(in, create = false, close = true)(walk)
    }
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

  private case class Input(path: AbsolutePath, arg: String, links: List[Int])

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

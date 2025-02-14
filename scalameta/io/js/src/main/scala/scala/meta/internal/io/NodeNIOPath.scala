package scala.meta.internal.io

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util.regex.Pattern
import java.util.{Iterator => JIterator}

import scala.collection.mutable

// Rough implementation of java.nio.Path, should work similarly for the happy
// path but has undefined behavior for error handling.
case class NodeNIOPath(filename: String) extends Path {
  import NodeNIOPath._

  private lazy val normalized =
    if (JSIO.isNode) JSPath.normalize(filename)
    else separatorsPattern.matcher(filename).replaceAll(File.separator)

  private lazy val (parsed, parts): (ParsedPath, Array[String]) = parse(filename)

  override def getNameCount: Int = parts.length
  override def getName(idx: Int): Path = NodeNIOPath(
    if (idx < parts.length) parts(idx)
    else throw new IllegalArgumentException(s"Path doesn't contain part #$idx: $filename")
  )
  override def subpath(beg: Int, end: Int): Path = {
    require(
      0 <= beg && beg < end && end <= parts.length,
      s"0 <= $beg && $beg < $end && $end <= ${parts.length}"
    )
    val sb = new StringBuilder
    for (idx <- beg until end) {
      if (idx > beg) sb.append(File.separatorChar)
      sb.append(parts(idx))
    }
    NodeNIOPath(sb.toString())
  }
  override def iterator(): JIterator[Path] = new JIterator[Path] {
    private val iter = parts.iterator
    override def hasNext: Boolean = iter.hasNext
    override def next(): Path = NodeNIOPath(iter.next())
  }

  override def getRoot: Path = if (parsed.root.isEmpty) null else NodeNIOPath(parsed.root)
  override def getParent: Path = NodeNIOPath(parsed.dir)
  override def getFileName: Path = NodeNIOPath(parsed.base)

  override def isAbsolute: Boolean = parsed.root.nonEmpty
  override def toAbsolutePath: Path =
    if (isAbsolute) this else NodeNIOPath.workingDirectory.resolve(this)
  override def toRealPath(): Path = toAbsolutePath
  override def toFile: File = new File(filename)
  override def toUri: URI = toFile.toURI

  private def relativize(other: String): Path = NodeNIOPath(JSPath.relative(filename, other))
  override def relativize(other: Path): Path = relativize(other.toString)
  override def normalize(): Path = if (normalized == filename) this else NodeNIOPath(normalized)

  private def startsWith(other: NodeNIOPath): Boolean = {
    val end = other.normalized.length
    normalized.startsWith(other.normalized, 0) &&
    (end == normalized.length || normalized.charAt(end) == File.separatorChar)
  }
  override def startsWith(other: Path): Boolean = other match {
    case other: NodeNIOPath => startsWith(other)
    case _ => startsWith(other.toString)
  }
  override def startsWith(other: String): Boolean = startsWith(NodeNIOPath(other))

  private def endsWith(other: NodeNIOPath): Boolean = {
    val off = normalized.length - other.normalized.length
    off >= 0 && normalized.startsWith(other.normalized, off) &&
    (off == 0 || normalized.charAt(off - 1) == File.separatorChar)
  }
  override def endsWith(other: Path): Boolean = other match {
    case other: NodeNIOPath => endsWith(other)
    case _ => endsWith(other.toString)
  }
  override def endsWith(other: String): Boolean = endsWith(NodeNIOPath(other))

  // JSPath.resolve(relpath, relpath) produces an absolute path from cwd.
  // This method turns the generated absolute path back into a relative path.
  private def adjustResolvedPath(resolved: String): Path =
    if (isAbsolute) NodeNIOPath(resolved) else NodeNIOPath.workingDirectory.relativize(resolved)
  override def resolveSibling(other: Path): Path = resolveSibling(other.toString)
  override def resolveSibling(other: String): Path =
    adjustResolvedPath(JSPath.resolve(parsed.dir, other))
  override def resolve(other: Path): Path = resolve(other.toString)
  override def resolve(other: String): Path = adjustResolvedPath(JSPath.resolve(filename, other))

  override def toString: String = filename
}

object NodeNIOPath {
  def workingDirectory = NodeNIOPath(PlatformPathIO.workingDirectoryString)

  private val separatorPattern: Pattern = Pattern.compile("[" + Pattern.quote(File.separator) + "]+")

  private val separatorsPattern: Pattern = File.separator match {
    case "/" => Pattern.compile("/+")
    case sep => Pattern.compile("[/" + Pattern.quote(sep) + "]+")
  }

  private val normalizeForParsing: String => String = File.separatorChar match {
    case '/' => identity
    case sep => _.replace('/', sep)
  }

  private def parse(path: String) = {
    val parsed = JSPath.parse(normalizeForParsing(path))
    import parsed._
    var idx = root.length
    val parts =
      if (dir.length == idx) if (base.isEmpty && path.nonEmpty) Array.empty[String] else Array(base)
      else {
        val res = new mutable.ArrayBuilder.ofRef[String]()
        val matcher = separatorPattern.matcher(dir)
        matcher.region(idx, dir.length)
        while (matcher.find()) {
          if (idx < matcher.start()) res += dir.substring(idx, matcher.start())
          idx = matcher.end()
        }
        if (idx < dir.length) res += dir.substring(idx)
        if (base.nonEmpty) res += base
        res.result()
      }
    (parsed, parts)
  }

}

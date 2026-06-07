package scala.meta.internal.semanticdb

import org.scalameta.collections.Conversions._
import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath

import java.io.InputStream
import java.nio.file._
import java.util.jar._

import scala.util.Try

class Locator(private val fn: Locator.Visitor) extends AnyVal {
  def apply(paths: List[Path]): Unit = paths.foreach(path => apply(path))

  def apply(path: Path): Unit = Try( // if missing or inaccessible, will throw
    Files.readAttributes(path, classOf[attribute.BasicFileAttributes]),
  ).foreach { attrs =>
    if (attrs.isDirectory) visitIter( // walk all files
      FileIO.listAllFilesRecursively(AbsolutePath(path)).iterator,
    )(_.toNIO, e => Files.newInputStream(e.toNIO))
    else if (path.toString.endsWith(".jar")) {
      val jar = new JarFile(path.toFile)
      try {
        visitIter(jar.entries().toScala)(x => Paths.get(x.getName), jar.getInputStream)
        Option(jar.getManifest).flatMap(x => Option(x.getMainAttributes.getValue("Class-Path")))
          .foreach { classPath =>
            val parent = path.toAbsolutePath.getParent
            classPath.split(" ").foreach(x => apply(parent.resolve(x)))
          }
      } finally jar.close()
    } else if (Locator.isSemantic(path)) visit(path, Files.newInputStream(path))
  }

  private def visit(path: Path, stream: InputStream): Unit =
    try fn(path, () => TextDocuments.parseFrom(stream))
    finally stream.close()

  private def visitIter[A](
      iter: Iterator[A],
  )(getPath: A => Path, getStream: A => InputStream): Unit = {
    val buf = Seq.newBuilder[(A, Path)]
    iter.foreach { elem =>
      val path = getPath(elem)
      if (Locator.isSemantic(path)) buf += elem -> path
    }
    // NOTE: nio.file.Path.compareTo is file system specific,
    // and the behavior is different on windows vs. unix
    buf.result().sortBy(_._2.toString.toLowerCase).foreach { case (elem, path) =>
      visit(path, getStream(elem))
    }
  }

}

object Locator {
  type Visitor = (Path, () => TextDocuments) => Unit

  def apply(paths: List[Path])(fn: Visitor): Unit = new Locator(fn).apply(paths)

  def apply(path: Path)(fn: Visitor): Unit = new Locator(fn).apply(path)

  private def isSemantic(path: Path): Boolean = path.toString.endsWith(".semanticdb")

}

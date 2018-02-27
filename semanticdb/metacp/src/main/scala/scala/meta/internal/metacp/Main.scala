package scala.meta.internal.metacp

import java.io._
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConverters._
import scala.meta.internal.javacp._
import scala.meta.internal.scalacp._
import scala.util.control.NonFatal
import org.langmeta.internal.io._
import org.langmeta.io._

class Main(settings: Settings, out: PrintStream, err: PrintStream) {
  def process(): Int = {
    var failed = false
    val classpath = Classpath(settings.cps.mkString(File.pathSeparator))
    val index = new Index
    classpath.visit { base =>
      new SimpleFileVisitor[Path] {
        override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(path) == "class") {
            try {
              val node = path.toClassNode
              val result = {
                val attrs = if (node.attrs != null) node.attrs.asScala else Nil
                if (attrs.exists(_.`type` == "ScalaSig")) {
                  val classfile = ToplevelClassfile(base.toNIO, path, node)
                  Scalacp.parse(classfile)
                } else if (attrs.exists(_.`type` == "Scala")) {
                  None
                } else {
                  val innerClassNode = node.innerClasses.asScala.find(_.name == node.name)
                  if (innerClassNode.isEmpty) {
                    val classfile = ToplevelClassfile(base.toNIO, path, node)
                    Javacp.parse(classfile)
                  } else {
                    None
                  }
                }
              }
              result.foreach { infos =>
                index.append(infos)
                infos.save(settings)
              }
            } catch {
              case NonFatal(ex) =>
                out.println(s"error: can't convert $path")
                ex.printStackTrace(out)
                failed = true
            }
          }
          FileVisitResult.CONTINUE
        }
      }
    }
    index.save(settings)
    if (failed) 1 else 0
  }
}

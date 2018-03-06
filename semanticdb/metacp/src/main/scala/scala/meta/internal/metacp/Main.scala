package scala.meta.internal.metacp

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConverters._
import scala.meta.internal.javacp._
import scala.meta.internal.scalacp._
import scala.util.control.NonFatal
import org.langmeta.internal.io._
import org.langmeta.io._

object Main {
  def process(settings: Settings): Int = {
    var failed = false
    val index = new Index
    settings.classpath.visit { base =>
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
                index.append(infos)
                infos.save(settings)
              }
            } catch {
              case NonFatal(ex) =>
                settings.out.println(s"error: can't convert $path")
                ex.printStackTrace(settings.out)
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

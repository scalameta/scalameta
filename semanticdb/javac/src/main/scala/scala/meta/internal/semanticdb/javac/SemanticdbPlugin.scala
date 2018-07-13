package scala.meta.internal.semanticdb.javac

import com.sun.source.util._
import java.nio.file.{Files, Paths}

class SemanticdbPlugin extends Plugin {

  def getName: String = {
    "semanticdb"
  }

  def init(task: JavacTask, args: String*): Unit = {
    val (targetRoot, sourceRoot) = args match {
      case Seq(path) => (Paths.get(path), Paths.get(sys.props("user.dir")))
      case Seq(path, "--sourceroot", sourceRoot) => (Paths.get(path), Paths.get(sourceRoot))
    }

    Files.createDirectories(targetRoot)
    Files.createDirectories(sourceRoot)

    task.addTaskListener(new SemanticdbListener(targetRoot, sourceRoot, Trees.instance(task)))
  }

}

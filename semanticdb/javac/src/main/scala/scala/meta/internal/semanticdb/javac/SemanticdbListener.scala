package scala.meta.internal.semanticdb.javac

import com.sun.source.tree.{ClassTree, CompilationUnitTree}
import com.sun.source.util.{TaskEvent, TaskListener, TreePath, Trees}
import java.nio.file.{Path, Paths}
import javax.lang.model.element.TypeElement
import javax.tools.JavaFileObject
import scala.collection.mutable
import scala.collection.JavaConverters._
import scala.meta.io.AbsolutePath

class SemanticdbListener(targetRoot: Path, sourceRoot: Path, trees: Trees) extends TaskListener {

  private val toplevelsProcessed =
    mutable.Map[CompilationUnitTree, mutable.Map[TypeElement, Boolean]]()

  private def singleFileGen(sourceFile: JavaFileObject, elems: Seq[TypeElement]): Unit = {
    val sourceRelativePath = sourceRoot.relativize(Paths.get(sourceFile.toUri))
    val gen = new SemanticdbGen(sourceRelativePath, elems)
    gen.populate()
    gen.persist(targetRoot)
  }

  override def started(e: TaskEvent): Unit = {}

  override def finished(e: TaskEvent): Unit = e.getKind match {
    case TaskEvent.Kind.ENTER if e.getSourceFile.getKind == JavaFileObject.Kind.SOURCE =>
      val cu = e.getCompilationUnit
      // pull the top-level TypeElements from the compilation unit
      val elements = cu.getTypeDecls.asScala.map {
        case tree: ClassTree =>
          val elemPath = TreePath.getPath(cu, tree)
          trees.getElement(elemPath).asInstanceOf[TypeElement]
      }
      assert(
        !toplevelsProcessed.contains(cu),
        "the same compilation unit has been entered twice, something is very wrong")
      toplevelsProcessed(cu) = mutable.Map(elements.map(_ -> false): _*)
    case TaskEvent.Kind.ANALYZE =>
      val cu = e.getCompilationUnit
      val elem = e.getTypeElement
      val remainingToplevels = toplevelsProcessed(cu)
      remainingToplevels(elem) = true
      if (remainingToplevels.values.reduce(_ & _)) {
        toplevelsProcessed.remove(cu)
        singleFileGen(e.getSourceFile, remainingToplevels.keys.toSeq)
      }
    case _ => ()
  }

}

  package scala.meta.internal.semanticdb.javac

import java.nio.file.{Files, Path}

import com.sun.source.tree.CompilationUnitTree
import com.sun.source.util.Trees
import javax.lang.model.element.TypeElement

import scala.collection.mutable
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.javac.semantics._
import scala.meta.internal.io.PathIO

class SemanticdbGen(relSourcePath: Path, toplevels: Seq[TypeElement],
                    val trees: Trees,
                    val compilationUnitTree: CompilationUnitTree) extends Semantics {

  private val infos = mutable.ListBuffer[s.SymbolInformation]()
  private val occurrences = mutable.ListBuffer[s.SymbolOccurrence]()

  private lazy val relOutputPath =
    PathIO.toUnix(relSourcePath.toString + ".semanticdb")

  def populate(): Unit = {
    toplevels.foreach { toplevel =>
      toplevel.populateInfos(infos, occurrences)
    }
  }

  def persist(targetRoot: Path): Unit = {
    val outputFile = targetRoot.resolve("META-INF/semanticdb").resolve(relOutputPath)
    Files.createDirectories(outputFile.getParent)
    val outputDocuments = s.TextDocuments(
      List(
        s.TextDocument(
          schema = s.Schema.SEMANTICDB4,
          uri = PathIO.toUnix(relSourcePath.toString),
          language = s.Language.JAVA,
          symbols = infos.result(),
          occurrences = occurrences.result())))
    val os = Files.newOutputStream(outputFile)
    try outputDocuments.writeTo(os)
    finally os.close()
  }

}

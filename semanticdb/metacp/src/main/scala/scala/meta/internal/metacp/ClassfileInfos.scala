package scala.meta.internal.metacp

import scala.collection.JavaConverters._
import scala.meta.internal.classpath.ClasspathIndex
import scala.meta.internal.io.FileIO
import scala.meta.internal.javacp.Javacp
import scala.meta.internal.scalacp.Scalacp
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.AbsolutePath
import scala.tools.asm.tree.ClassNode

final case class ClassfileInfos(
    relativeUri: String,
    language: s.Language,
    infos: List[s.SymbolInformation]) {
  def toTextDocuments: s.TextDocuments = {
    val semanticdbDocument = s.TextDocument(
      schema = s.Schema.SEMANTICDB4,
      uri = relativeUri,
      language = language,
      symbols = infos
    )
    s.TextDocuments(List(semanticdbDocument))
  }
  def save(out: AbsolutePath): Unit = {
    assert(infos.nonEmpty)
    val semanticdbAbspath =
      out.resolve("META-INF").resolve("semanticdb").resolve(relativeUri + ".semanticdb")
    FileIO.write(semanticdbAbspath, toTextDocuments)
  }
}

object ClassfileInfos {
  def fromClassNode(
      node: ClassNode,
      classpathIndex: ClasspathIndex
  ): Option[ClassfileInfos] = {
    node.scalaSig match {
      case Some(scalaSig) =>
        Some(Scalacp.parse(scalaSig, classpathIndex))
      case None =>
        val attrs = if (node.attrs != null) node.attrs.asScala else Nil
        if (attrs.exists(_.`type` == "Scala")) {
          None
        } else {
          val innerClassNode = node.innerClasses.asScala.find(_.name == node.name)
          if (innerClassNode.isEmpty) {
            Some(Javacp.parse(node, classpathIndex))
          } else {
            None
          }
        }
    }
  }
}

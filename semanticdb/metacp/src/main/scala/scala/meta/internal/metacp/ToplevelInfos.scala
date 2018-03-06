package scala.meta.internal.metacp

import org.langmeta.internal.io._
import org.langmeta.io._
import scala.meta.internal.{semanticdb3 => s}

final case class ToplevelInfos(
    classfile: ToplevelClassfile,
    toplevels: List[s.SymbolInformation],
    others: List[s.SymbolInformation]) {
  def save(settings: Settings): Unit = {
    assert(toplevels.nonEmpty)
    val semanticdbRoot = settings.d.resolve("META-INF").resolve("semanticdb")
    val semanticdbRelpath = classfile.name + ".semanticdb"
    val semanticdbAbspath = semanticdbRoot.resolve(semanticdbRelpath)
    val semanticdbDocument = s.TextDocument(
      schema = s.Schema.SEMANTICDB3,
      uri = classfile.name,
      language = toplevels.head.language,
      symbols = toplevels ++ others)
    val semanticdbMessage = s.TextDocuments(List(semanticdbDocument))
    FileIO.write(semanticdbAbspath, semanticdbMessage)
  }
}

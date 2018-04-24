package scala.meta.internal.metacp

import scala.meta.internal.io._
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.io._

final case class ToplevelInfos(
    classfile: ToplevelClassfile,
    toplevels: List[s.SymbolInformation],
    others: List[s.SymbolInformation]) {
  def uri: String = classfile.uri + ".semanticdb"
  def save(out: AbsolutePath): Unit = {
    assert(toplevels.nonEmpty)
    val semanticdbAbspath = out.resolve("META-INF").resolve("semanticdb").resolve(uri)
    val semanticdbDocument = s.TextDocument(
      schema = s.Schema.SEMANTICDB3,
      uri = classfile.uri,
      language = toplevels.head.language,
      symbols = toplevels ++ others)
    val semanticdbMessage = s.TextDocuments(List(semanticdbDocument))
    FileIO.write(semanticdbAbspath, semanticdbMessage)
  }
}

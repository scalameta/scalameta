package scala.meta.internal.scalacp

import scala.meta.internal.classpath._
import scala.meta.internal.metacp._
import scala.meta.internal.{semanticdb => s}
import scala.tools.scalap.scalax.rules.scalasig._

final class Scalacp private (
    val symbolIndex: SymbolIndex
) extends AnnotationOps
    with NameOps
    with SymbolInformationOps
    with SymbolOps
    with TypeOps {
  def parse(scalaSig: ScalaSig, relativeUri: String): ClassfileInfos = {
    val sinfos = scalaSig.symbols.toList.flatMap {
      case sym: SymbolInfoSymbol => this.sinfos(sym)
      case _ => Nil
    }
    val snonlocalInfos = sinfos.filter(sinfo => !hardlinks.contains(sinfo.symbol))
    ClassfileInfos(relativeUri, s.Language.SCALA, snonlocalInfos)
  }

  private def sinfos(sym: SymbolInfoSymbol): List[s.SymbolInformation] = {
    if (sym.isSemanticdbLocal) return Nil
    if (sym.isUseless) return Nil
    val ssym = sym.ssym
    if (ssym.contains("$extension")) return Nil
    val sinfo = sym.toSymbolInformation(SymlinkChildren)
    if (sym.isUsefulField && sym.isMutable) {
      List(sinfo) ++ Synthetics.setterInfos(sinfo, SymlinkChildren)
    } else {
      List(sinfo)
    }
  }
}

object Scalacp {
  def parse(
      scalaSig: ScalaSig,
      relativeUri: String,
      classpathIndex: ClasspathIndex
  ): ClassfileInfos = {
    val symbolIndex = SymbolIndex(classpathIndex)
    val scalacp = new Scalacp(symbolIndex)
    scalacp.parse(scalaSig, relativeUri)
  }
}

package scala.meta.internal.scalacp

import scala.meta.internal.classpath._
import scala.meta.internal.metacp._
import scala.meta.internal.{semanticdb => s}
import scala.meta.cli._
import scala.meta.metacp._
import scala.tools.scalap.scalax.rules.scalasig._

final class Scalacp private (
    val symbolIndex: SymbolIndex,
    val settings: Settings,
    val reporter: Reporter
) extends AnnotationOps
    with SymbolInformationOps
    with SymbolOps
    with TypeOps {
  def parse(node: ScalaSigNode): ClassfileInfos = {
    val sinfos = node.scalaSig.symbols.toList.flatMap {
      case sym: SymbolInfoSymbol => this.sinfos(sym)
      case _ => Nil
    }
    val snonlocalInfos = sinfos.filter(sinfo => !hardlinks.contains(sinfo.symbol))
    ClassfileInfos(node.relativeUri, s.Language.SCALA, snonlocalInfos)
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
      node: ScalaSigNode,
      classpathIndex: ClasspathIndex,
      settings: Settings,
      reporter: Reporter
  ): ClassfileInfos = {
    val symbolIndex = SymbolIndex(classpathIndex)
    val scalacp = new Scalacp(symbolIndex, settings, reporter)
    scalacp.parse(node)
  }
}

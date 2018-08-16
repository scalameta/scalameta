package scala.meta.internal.scalacp

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb.Scala.{Names => n}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}

object Synthetics {
  def setterInfos(getterInfo: s.SymbolInformation, linkMode: LinkMode): List[s.SymbolInformation] = {
    val getterSym = getterInfo.symbol
    val setterSym = {
      if (getterSym.isGlobal) {
        val setterSymbolName = getterSym.desc.name + "_="
        Symbols.Global(getterSym.owner, d.Method(setterSymbolName, "()"))
      } else {
        getterSym + "+1"
      }
    }

    val paramSym = {
      if (getterSym.isGlobal) Symbols.Global(setterSym, d.Parameter("x$1"))
      else getterSym + "+2"
    }
    val paramSig = getterInfo.signature match {
      case s.MethodSignature(_, _, sret) => s.ValueSignature(sret)
      case _ => s.NoSignature
    }
    val paramInfo = s.SymbolInformation(
      symbol = paramSym,
      language = l.SCALA,
      kind = k.PARAMETER,
      properties = 0,
      displayName = "x$1",
      signature = paramSig,
      annotations = Nil,
      access = s.NoAccess)

    val setterSig = {
      val unit = s.TypeRef(s.NoType, "scala/Unit#", Nil)
      val setterParamss = {
        linkMode match {
          case SymlinkChildren => List(s.Scope(symlinks = List(paramInfo.symbol)))
          case HardlinkChildren => List(s.Scope(hardlinks = List(paramInfo)))
        }
      }
      s.MethodSignature(Some(s.Scope()), setterParamss, unit)
    }
    val setterInfo = s.SymbolInformation(
      symbol = setterSym,
      language = l.SCALA,
      kind = k.METHOD,
      properties = getterInfo.properties,
      displayName = getterInfo.displayName + "_=",
      signature = setterSig,
      annotations = getterInfo.annotations,
      access = getterInfo.access)

    linkMode match {
      case SymlinkChildren => List(paramInfo, setterInfo)
      case HardlinkChildren => List(setterInfo)
    }
  }
}

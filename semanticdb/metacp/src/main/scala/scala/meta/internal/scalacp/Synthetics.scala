package scala.meta.internal.scalacp

import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb3.{Language => l}
import scala.meta.internal.semanticdb3.Scala._
import scala.meta.internal.semanticdb3.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.semanticdb3.Type.{Tag => t}

object Synthetics {
  def setterInfos(getterInfo: s.SymbolInformation): List[s.SymbolInformation] = {
    val setterName = getterInfo.name + "_="
    val setterSym = Symbols.Global(getterInfo.symbol.owner, d.Method(setterName, "()"))

    val paramSym = Symbols.Global(setterSym, d.Parameter("x$1"))
    val paramTpe = getterInfo.tpe.flatMap(_.methodType.flatMap(_.returnType))
    val paramInfo = s.SymbolInformation(
      symbol = paramSym,
      language = l.SCALA,
      kind = k.PARAMETER,
      properties = 0,
      name = paramSym.desc.name,
      tpe = paramTpe,
      annotations = Nil,
      accessibility = Some(s.Accessibility(a.PUBLIC)))

    val setterTpe = {
      val unitTpe = s.TypeRef(None, "scala.Unit#", Nil)
      val unit = s.Type(tag = t.TYPE_REF, typeRef = Some(unitTpe))
      val setterParams = s.MethodType.ParameterList(List(paramSym))
      val setterTpe = s.MethodType(Nil, List(setterParams), Some(unit))
      s.Type(tag = t.METHOD_TYPE, methodType = Some(setterTpe))
    }
    val setterInfo = s.SymbolInformation(
      symbol = setterSym,
      language = l.SCALA,
      kind = k.METHOD,
      properties = getterInfo.properties,
      name = setterName,
      tpe = Some(setterTpe),
      annotations = getterInfo.annotations,
      accessibility = getterInfo.accessibility)

    List(paramInfo, setterInfo)
  }
}

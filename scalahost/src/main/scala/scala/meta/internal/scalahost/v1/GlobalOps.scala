package scala.meta.internal
package scalahost
package v1

import scala.reflect.internal.Flags._
import scala.{meta => m}

trait GlobalOps extends ReflectionToolkit {
  import global._

  // ============ PRETTYPRINTING ============

  def syntaxAndPos(gtree: g.Tree): String = {
    if (gtree == g.EmptyTree) "\u001b[1;31mEmptyTree\u001b[0m"
    else
      s"${gtree.toString
        .substring(0, Math.min(45, gtree.toString.length))
        .replace("\n", " ")} [${gtree.pos.start}..${gtree.pos.end})"
  }
  def syntaxAndPos(mtree: m.Tree): String = {
    s"$mtree [${mtree.pos.start.offset}..${mtree.pos.end.offset})"
  }

  // ============ SYMBOLS ============

  def wrapAlternatives(name: String, alts: Symbol*): Symbol = {
    val normalizedAlts = {
      val alts1 = alts.toList.filter(_.exists)
      val alts2 = alts1.map(alt => if (alt.isModuleClass) alt.asClass.module else alt)
      alts2.distinct
    }
    normalizedAlts match {
      case List(sym) =>
        sym
      case normalizedAlts =>
        val wrapper = NoSymbol.newTermSymbol(TermName(name))
        wrapper.setFlag(OVERLOADED)
        wrapper.setInfo(OverloadedType(NoType, normalizedAlts))
    }
  }
}

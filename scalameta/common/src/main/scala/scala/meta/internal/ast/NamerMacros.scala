package scala.meta
package internal
package ast

import scala.language.experimental.macros
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox.Context
import org.scalameta.internal.MacroHelpers

trait CommonNamerMacros extends MacroHelpers {
  import c.universe._
  import Flag._

  def mkQuasi(name: TypeName, parents: List[Tree], paramss: List[List[ValDef]], extraStubs: String*): ClassDef = {
    val qmods = Modifiers(NoFlags, TypeName("meta"), List(q"new _root_.scala.meta.internal.ast.ast"))
    val qname = TypeName("Quasi")
    val qparents = tq"$name" +: tq"_root_.scala.meta.internal.ast.Quasi" +: parents.map({
      case Ident(name) => Select(Ident(name.toTermName), TypeName("Quasi"))
      case Select(qual, name) => Select(Select(qual, name.toTermName), TypeName("Quasi"))
      case unsupported => c.abort(unsupported.pos, "implementation restriction: unsupported parent")
    })

    val qstats = ListBuffer[Tree]()
    qstats += q"""
      def pt: _root_.java.lang.Class[_] = {
        _root_.scala.meta.internal.ast.Helpers.arrayClass(_root_.scala.Predef.classOf[$name], this.rank)
      }
    """

    def stub() = {
      val unsupportedUnquotingPosition = "unsupported unquoting position"
      val unsupportedSplicingPosition = "unsupported splicing position"
      val message = q"if (this.rank == 0) $unsupportedUnquotingPosition else $unsupportedSplicingPosition"
      q"throw new _root_.scala.`package`.UnsupportedOperationException($message)"
    }
    val qstubs = (paramss.flatten.map(_.name.toString) ++ extraStubs).distinct.map(TermName.apply)
    qstubs.foreach(name => qstats += q"def $name: _root_.scala.Nothing = ${stub()}")
    val qcopyParamss = paramss.map(_.map{ case ValDef(mods, name, tpt, _) => q"val $name: $tpt = this.$name" })
    qstats += q"def copy(...$qcopyParamss): $name = ${stub()}"

    q"$qmods class $qname(rank: _root_.scala.Int, tree: _root_.scala.Any) extends ..$qparents { ..$qstats }"
  }
}

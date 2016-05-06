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

  lazy val TreeClass = tq"_root_.scala.meta.Tree"
  lazy val QuasiClass = tq"_root_.scala.meta.internal.ast.Quasi"
  lazy val ClassifierClass = tq"_root_.scala.meta.classifiers.Classifier"
  lazy val ArrayClassMethod = q"_root_.scala.meta.internal.ast.Helpers.arrayClass"
  lazy val ClassOfMethod = q"_root_.scala.Predef.classOf"
  lazy val FlagsClass = tq"_root_.scala.meta.internal.flags.Flags"
  lazy val FlagsZeroField = q"_root_.scala.meta.internal.flags.ZERO"
  lazy val TokensClass = tq"_root_.scala.meta.tokens.Tokens"
  lazy val EnvironmentClass = tq"_root_.scala.meta.internal.semantic.Environment"
  lazy val DenotationClass = tq"_root_.scala.meta.internal.semantic.Denotation"
  lazy val TypingClass = tq"_root_.scala.meta.internal.semantic.Typing"
  lazy val AstAnnotation = tq"_root_.scala.meta.internal.ast.ast"
  lazy val PositionClass = tq"_root_.scala.meta.inputs.Position"
  lazy val PositionModule = q"_root_.scala.meta.inputs.Position"
  lazy val PointClass = tq"_root_.scala.meta.inputs.Point"
  lazy val PointModule = q"_root_.scala.meta.inputs.Point"
  lazy val OriginClass = tq"_root_.scala.meta.internal.ast.Origin"
  lazy val OriginModule = q"_root_.scala.meta.internal.ast.Origin"

  def mkClassifier(name: TypeName): List[Tree] = {
    val q"..$classifierBoilerplate" = q"""
      private object sharedClassifier extends $ClassifierClass[$TreeClass, $name] {
        def apply(x: $TreeClass): Boolean = x.isInstanceOf[$name]
      }
      implicit def ClassifierClass[T <: $TreeClass]: $ClassifierClass[T, $name] = {
        sharedClassifier.asInstanceOf[$ClassifierClass[T, $name]]
      }
    """
    classifierBoilerplate
  }

  def mkQuasi(name: TypeName, parents: List[Tree], paramss: List[List[ValDef]], extraStubs: String*): ClassDef = {
    val qmods = Modifiers(NoFlags, TypeName("meta"), List(q"new $AstAnnotation"))
    val qname = TypeName("Quasi")
    val qparents = tq"$name" +: tq"$QuasiClass" +: parents.map({
      case Ident(name) => Select(Ident(name.toTermName), TypeName("Quasi"))
      case Select(qual, name) => Select(Select(qual, name.toTermName), TypeName("Quasi"))
      case unsupported => c.abort(unsupported.pos, "implementation restriction: unsupported parent")
    })

    val qstats = ListBuffer[Tree]()
    qstats += q"""
      def pt: _root_.java.lang.Class[_] = {
        $ArrayClassMethod($ClassOfMethod[$name], this.rank)
      }
    """

    def stub() = {
      val unsupportedUnquotingPosition = "unsupported unquoting position"
      val unsupportedSplicingPosition = "unsupported splicing position"
      val message = q"if (this.rank == 0) $unsupportedUnquotingPosition else $unsupportedSplicingPosition"
      q"throw new $UnsupportedOperationException($message)"
    }
    val qstubs = (paramss.flatten.map(_.name.toString) ++ extraStubs).distinct.map(TermName.apply)
    qstubs.foreach(name => qstats += q"def $name: $NothingClass = ${stub()}")
    val qcopyParamss = paramss.map(_.map{ case ValDef(mods, name, tpt, _) => q"val $name: $tpt = this.$name" })
    qstats += q"def copy(...$qcopyParamss): $name = ${stub()}"

    q"$qmods class $qname(rank: $IntClass, tree: $TreeClass) extends ..$qparents { ..$qstats }"
  }
}

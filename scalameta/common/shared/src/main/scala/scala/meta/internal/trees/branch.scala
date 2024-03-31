package scala.meta
package internal
package trees

import scala.meta.internal.trees.{Reflection => AstReflection}

import scala.annotation.StaticAnnotation
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

// @branch is a specialized version of @org.scalameta.adt.branch for scala.meta ASTs.
class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BranchNamerMacros.impl
}

class BranchNamerMacros(val c: Context) extends AstReflection with CommonNamerMacros {
  import c.universe.Flag._
  import c.universe._

  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      def isQuasi = isQuasiClass(cdef)
      val q"${mods @ Modifiers(flags, privateWithin, anns)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =
        cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" =
        mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "@branch traits cannot be sealed")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@branch traits cannot be final")
      val flags1 = flags
      mstats1 += q"$CommonTyperMacrosModule.hierarchyCheck[$name]"
      val anns1 = anns :+ q"new $AdtMetadataModule.branch" :+ q"new $AstMetadataModule.branch"
      mstats1 ++= mkClassifier(name)
      if (!isQuasi) {
        mstats1 += mkAstInfo(name)
        mstats1 += mkQuasi(name, parents, Nil, Nil, stats, "value", "name", "tpe")
      }

      val needsUnapply = !mstats.exists {
        case DefDef(_, TermName("unapply"), _, _, _, _) => true
        case _ => false
      }
      if (needsUnapply) {
        def getUnapply(unapplyParams: Seq[ValOrDefDef], annots: Tree*): Tree = {
          val successTargs = tq"(..${unapplyParams.map(p => p.tpt)})"
          val successArgs = q"(..${unapplyParams.map(p => q"x.${p.name}")})"
          q"""
              @$InlineAnnotation @..$annots final def unapply(x: $name): $OptionClass[$successTargs] =
                if (x != null) $SomeModule($successArgs) else $NoneModule
            """
        }
        val params = stats.collect {
          case x: ValDef => x
          case x: DefDef if x.tparams.isEmpty && x.vparamss.isEmpty => x
        }.filter(_.rhs eq EmptyTree)
        if (params.nonEmpty) mstats1 += getUnapply(params)
        else mstats1 += q"@$InlineAnnotation final def unapply(x: $name): $BooleanClass = true"
      }

      val cdef1 =
        q"${Modifiers(flags1, privateWithin, anns1)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      val mdef1 =
        q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
  })
}

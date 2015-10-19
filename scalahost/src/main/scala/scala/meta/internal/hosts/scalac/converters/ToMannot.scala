package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.{ast => m}
import scala.meta.internal.hosts.scalac.reflect._
import scala.meta.internal.flags._

// This module exposes a method that can convert scala.reflect annotations into equivalent scala.meta mods.
// There's not much to say about this conversion except that it's a really lossy one:
// not only we have to deal with desugared trees in annotation arguments,
// but we also have to tolerate the loss of the constructor symbol (because g.AnnotationInfos only have a type).
// See comments to ToMtree to learn more about preserving original syntax.
trait ToMannot extends ReflectToolkit with MetaToolkit {
  self: Api =>

  protected implicit class XtensionGannotToMannot(gannot: g.AnnotationInfo) {
    def toMannot: m.Mod.Annot = {
      def mannotcore(gannot: g.AnnotationInfo): m.Term = {
        val g.AnnotationInfo(gatp, gargs, gassocs) = gannot
        val gctor = gatp.decl(g.nme.CONSTRUCTOR).alternatives.head // TODO: manual overload resolution!
        val matp = gatp.toMtype
        val margs = {
          if (gassocs.nonEmpty) {
            def loop(garg: g.ClassfileAnnotArg): m.Term = garg match {
              case g.LiteralAnnotArg(gconst) =>
                gconst.toMlit
              case g.ArrayAnnotArg(gargs) =>
                val marray = g.definitions.ArrayModule.toMname(g.DefaultPrefix)
                m.Term.Apply(marray, gargs.map(loop).toList)
              case g.NestedAnnotArg(gannot: g.AnnotationInfo) =>
                mannotcore(gannot)
              case _ =>
                unreachable(debug(garg))
            }
            gassocs.map({ case (gname, garg) =>
              val gparam = gctor.paramss.flatten.find(_.name == gname).get.asTerm
              val mname = gparam.toMname(g.DefaultPrefix)
              m.Term.Arg.Named(mname, loop(garg))
            })
          } else {
            if (gannot.atp.typeSymbol == g.definitions.ThrowsClass) Nil
            else gargs.map(_.toMtree[m.Term])
          }
        }
        val msyctor = m.Ctor.Name(gatp.typeSymbolDirect.name.decoded)
        val msector = msyctor.withMattrs(gatp, gctor)
        val mctorname = matp.ctorRef(msector).require[m.Term]
        val mannot = m.Term.Apply(mctorname, margs).withMattrs(gannot.atp)
        if (margs.isEmpty) mctorname.withExpansion(mannot)
        else mannot
      }
      m.Mod.Annot(mannotcore(gannot)).forceTypechecked
    }
  }

  protected implicit class RichToMannots(gannots: Seq[g.AnnotationInfo]) {
    def toMannots: Seq[m.Mod.Annot] = {
      gannots.filter(gannot => {
        val gsym = gannot.tree.tpe.typeSymbol
        def isOldMacroSignature = gsym.fullName == "scala.reflect.macros.internal.macroImpl"
        def isNewMacroSignature = isOldMacroSignature
        !isOldMacroSignature && !isNewMacroSignature
      }).map(_.toMannot)
    }
  }
}
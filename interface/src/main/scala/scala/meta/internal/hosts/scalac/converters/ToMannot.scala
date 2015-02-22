package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.{ast => m}

// This module exposes a method that can convert scala.reflect annotations into equivalent scala.meta mods.
// There's not much to say about this conversion except that it's a really lossy one:
// not only we have to deal with desugared trees in annotation arguments,
// but we also have to tolerate the loss of the constructor symbol (because g.AnnotationInfos only have a type).
trait ToMannot extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class RichToMannot(gannot: g.AnnotationInfo) {
    def toMannot: m.Mod.Annot = {
      def mannotcore(gannot: g.AnnotationInfo): m.Term = {
        val g.AnnotationInfo(gatp, gargs, gassocs) = gannot
        val gctor = gatp.decl(g.nme.CONSTRUCTOR).alternatives.head // TODO: manual overload resolution!
        val matp = gatp.toMtype
        val margs = {
          if (gassocs.nonEmpty) {
            def loop(garg: g.ClassfileAnnotArg): m.Term = garg match {
              case g.LiteralAnnotArg(gconst) =>
                gconst.rawcvt.withOriginal(g.Literal(gconst).setType(g.ConstantType(gconst)))
              case g.ArrayAnnotArg(gargs) =>
                val marray = g.definitions.ArrayModule.rawcvt(g.Ident(g.definitions.ArrayModule))
                m.Term.Apply(marray, gargs.map(loop).toList)
              case g.NestedAnnotArg(gannot: g.AnnotationInfo) =>
                mannotcore(gannot)
              case _ =>
                unreachable(debug(garg))
            }
            gassocs.map({ case (gname, garg) =>
              val gparam = gctor.paramss.flatten.find(_.name == gname).get.asTerm
              val mname = gparam.rawcvt(g.Ident(gparam))
              m.Term.Arg.Named(mname, loop(garg))
            })
          } else {
            def loop(garg: g.Tree): m.Term = { val _ = toMtree.computeConverters; toMtree(garg, classOf[m.Term]) }
            if (gannot.atp.typeSymbol == g.definitions.ThrowsClass) Nil
            else gargs.map(loop)
          }
        }
        val mctor = m.Ctor.Name(gatp.typeSymbolDirect.name.decoded).withDenot(gatp, gctor)
        val mcore = matp.ctorRef(mctor).require[m.Term]
        if (margs.isEmpty) mcore
        else m.Term.Apply(mcore, margs).withOriginal(gannot)
      }
      m.Mod.Annot(mannotcore(gannot)).withOriginal(gannot)
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
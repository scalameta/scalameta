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
import scala.meta.internal.{ast => p}

// This module exposes a method that can convert scala.reflect annotations into equivalent scala.meta mods.
// There's not much to say about this conversion except that it's a really lossy one:
// not only we have to deal with desugared trees in annotation arguments,
// but we also have to tolerate the loss of the constructor symbol (because g.AnnotationInfos only have a type).
trait ToPannot extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class RichToPannot(gannot: g.AnnotationInfo) {
    def toPannot: p.Mod.Annot = {
      def pannotcore(gannot: g.AnnotationInfo): p.Term = {
        val g.AnnotationInfo(gatp, gargs, gassocs) = gannot
        val gctor = gatp.decl(g.nme.CONSTRUCTOR).alternatives.head // TODO: manual overload resolution!
        val patp = gatp.toPtype
        val pargs = {
          if (gassocs.nonEmpty) {
            def loop(garg: g.ClassfileAnnotArg): p.Term = garg match {
              case g.LiteralAnnotArg(const) =>
                const.rawcvt.withOriginal(g.Literal(const).setType(g.ConstantType(const)))
              case g.ArrayAnnotArg(args) =>
                val parray = g.definitions.ArrayModule.rawcvt(g.Ident(g.definitions.ArrayModule))
                p.Term.Apply(parray, args.map(loop).toList)
              case g.NestedAnnotArg(gannot: g.AnnotationInfo) =>
                pannotcore(gannot)
              case _ =>
                unreachable
            }
            gassocs.map({ case (gname, garg) =>
              val gparam = gctor.paramss.flatten.find(_.name == gname).get.asTerm
              val pname = gparam.rawcvt(g.Ident(gparam))
              p.Term.Arg.Named(pname, loop(garg))
            })
          } else {
            def loop(garg: g.Tree): p.Term = {
              // TODO: think of a better way to express this
              // and, by the way, why is an implicit context needed here at all?
              implicit val c: ScalametaSemanticContext = self.require[ScalametaSemanticContext]
              toPtree(garg, classOf[p.Term])
            }
            if (gannot.atp.typeSymbol == g.definitions.ThrowsClass) Nil
            else gargs.map(loop)
          }
        }
        val pcore = patp.ctorRef(gctor)
        if (pargs.isEmpty) pcore
        else p.Term.Apply(pcore, pargs).withOriginal(gannot)
      }
      p.Mod.Annot(pannotcore(gannot)).withOriginal(gannot)
    }
  }

  protected implicit class RichToPannots(gannots: Seq[g.AnnotationInfo]) {
    def toPannots: Seq[p.Mod.Annot] = {
      gannots.filter(gannot => {
        val gsym = gannot.tree.tpe.typeSymbol
        def isOldMacroSignature = gsym.fullName == "scala.reflect.macros.internal.macroImpl"
        def isNewMacroSignature = isOldMacroSignature
        !isOldMacroSignature && !isNewMacroSignature
      }).map(_.toPannot)
    }
  }
}
package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.convert.auto._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.{meta => papi}
import scala.meta.internal.{ast => p}
import scala.reflect.runtime.universe.{Type => Pt}
import scala.meta.internal.{hygiene => h}
import scala.meta.syntactic.parsers.SyntacticInfo.{SyntacticTermOps => _, _}

// This module exposes a method that can convert scala.reflect trees into high-fidelity scala.meta trees.
//
// It is capable of undoing a whole bunch of desugarings performed by scalac,
// but for some of these desugarings it needs additional tree attachments produced by custom analyzer
// provided by the scalahost plugin.
//
// A lot of heavylifting is performed by the `@converter` macro annotation,
// which not only simplifies the notation of the converter (by providing convenience .cvt and .cvt_! methods),
// but also generates a strongly-typed conversion harness, produces boilerplate that simplifies debugging,
// ensures that all trees are covered by the converter, that there are no conversion ambiguities and much else.
//
// Even though this macro annotation does a lot, it's forcing us into
// a lot of notational weirdness and itself has an implementation whose LOC count
// surpasses the LOC count of this file. I think that, at a point when we can afford big refactorings,
// we need to drop the annotation and rewrite everything without its help.
trait ToPtree extends GlobalToolkit with MetaToolkit {
  self: Api =>

  val TermQuote = "shadow scala.meta quasiquotes"
  import g.Quasiquote

  @converter def toPtree(in: Any, pt: Pt): Any = {
    object Helpers extends g.ReificationSupportImpl { self =>
      def pctorcall(in: g.Tree, gtpt: g.Tree, gctor: g.Symbol, gargss: Seq[Seq[g.Tree]]): p.Term = {
        val pcore = (gtpt.cvt_! : p.Type).ctorRef(gctor)
        val pargss = gargss.map(_.map(parg))
        pargss.foldLeft(pcore)((pcurr, pargs) => p.Term.Apply(pcurr, pargs)).withOriginal(in)
      }
      def pfakector(gparent: g.MemberDef): p.Ctor.Primary = {
        val ctor = gparent match { case _: g.ClassDef => gparent.symbol; case _: g.ModuleDef => gparent.symbol.moduleClass.primaryConstructor }
        val name = p.Ctor.Name(gparent.name.toString).withDenot(gparent.symbol)
        p.Ctor.Primary(Nil, name, Nil)
      }
      def pannot(gannot: g.Tree): p.Mod.Annot = {
        val q"new $gtpt(...$gargss)" = gannot
        val g.treeInfo.Applied(ctorref @ g.Select(g.New(tpt), _), _, _) = gannot
        p.Mod.Annot(pctorcall(gannot, gtpt, ctorref.symbol, gargss))
      }
      def pannots(gannots: Seq[g.Tree]): Seq[p.Mod.Annot] = {
        gannots.map(pannot)
      }
      def pmods(gmdef: g.MemberDef): Seq[p.Mod] = {
        def annotationMods(gmdef: g.MemberDef): Seq[p.Mod] = pannots(gmdef.mods.annotations)
        def accessQualifierMods(gmdef: g.MemberDef): Seq[p.Mod] = {
          val gmods = gmdef.mods
          val gsym = gmdef.symbol.getterIn(gmdef.symbol.owner).orElse(gmdef.symbol)
          val gpriv = gsym.privateWithin.orElse(gmdef.symbol.owner)
          if (gmods.hasFlag(SYNTHETIC) && gmods.hasFlag(ARTIFACT)) {
            // NOTE: some sick artifact vals produced by mkPatDef can be private to method (whatever that means)
            // so we can't invoke paccessqual on them, because that will produce crazy results
            Nil
          } else if (gmods.hasFlag(LOCAL)) {
            if (gmods.hasFlag(PROTECTED)) List(p.Mod.ProtectedThis().withDenot(gpriv))
            else if (gmods.hasFlag(PRIVATE)) List(p.Mod.PrivateThis().withDenot(gpriv))
            else unreachable
          } else if (gmods.hasAccessBoundary && gpriv != g.NoSymbol) {
            // TODO: `private[pkg] class C` doesn't have PRIVATE in its flags
            // so we need to account for that!
            if (gmods.hasFlag(PROTECTED)) List(p.Mod.ProtectedWithin(gpriv.name.toString).withDenot(gpriv))
            else List(p.Mod.PrivateWithin(gpriv.name.toString).withDenot(gpriv))
          } else {
            if (gmods.hasFlag(PROTECTED)) List(p.Mod.Protected())
            else if (gmods.hasFlag(PRIVATE)) List(p.Mod.Private())
            else Nil
          }
        }
        def otherMods(gmdef: g.MemberDef): Seq[p.Mod] = {
          val pmods = scala.collection.mutable.ListBuffer[p.Mod]()
          val gmods = gmdef.mods
          if (gmods.hasFlag(IMPLICIT)) pmods += p.Mod.Implicit()
          if (gmods.hasFlag(FINAL)) pmods += p.Mod.Final()
          if (gmods.hasFlag(SEALED)) pmods += p.Mod.Sealed()
          if (gmods.hasFlag(OVERRIDE)) pmods += p.Mod.Override()
          if (gmods.hasFlag(CASE)) pmods += p.Mod.Case()
          if (gmods.hasFlag(ABSTRACT) && gmdef.isInstanceOf[g.ClassDef] && !gmods.hasFlag(TRAIT)) pmods += p.Mod.Abstract()
          if (gmods.hasFlag(ABSOVERRIDE)) { pmods += p.Mod.Abstract(); pmods += p.Mod.Override() }
          if (gmods.hasFlag(COVARIANT) && gmdef.isInstanceOf[g.TypeDef]) pmods += p.Mod.Covariant()
          if (gmods.hasFlag(CONTRAVARIANT) && gmdef.isInstanceOf[g.TypeDef]) pmods += p.Mod.Contravariant()
          if (gmods.hasFlag(LAZY)) pmods += p.Mod.Lazy()
          pmods.toList
        }
        def valVarParamMods(gmdef: g.MemberDef): Seq[p.Mod] = {
          val pmods = scala.collection.mutable.ListBuffer[p.Mod]()
          val ggetter = gmdef.symbol.owner.filter(_.isPrimaryConstructor).map(_.owner.info.member(gmdef.name))
          val gfield = ggetter.map(_.owner.info.member(ggetter.localName))
          val isApplicable = gmdef.symbol.owner.isPrimaryConstructor && gfield != g.NoSymbol
          if (isApplicable && gfield.isMutable) pmods += p.Mod.VarParam()
          if (isApplicable && !gfield.isMutable && !gfield.owner.isCase) pmods += p.Mod.ValParam()
          pmods.toList
        }
        val result = annotationMods(gmdef) ++ accessQualifierMods(gmdef) ++ otherMods(gmdef) ++ valVarParamMods(gmdef)
        // TODO: we can't discern `class C(x: Int)` and `class C(private[this] val x: Int)`
        // so let's err on the side of the more popular option
        if (gmdef.symbol.owner.isPrimaryConstructor) result.filter(!_.isInstanceOf[p.Mod.PrivateThis]) else result
      }
      def pvparamtpe(gtpt: g.Tree): p.Type.Arg = {
        def unwrap(ptpe: p.Type): p.Type = ptpe.require[p.Type.Apply].args.head
        if (g.definitions.isRepeatedParamType(gtpt.tpe)) p.Type.Arg.Repeated(unwrap(gtpt.cvt_! : p.Type)).withOriginal(gtpt)
        else if (g.definitions.isByNameParamType(gtpt.tpe)) p.Type.Arg.ByName(unwrap(gtpt.cvt_! : p.Type)).withOriginal(gtpt)
        else (gtpt.cvt_! : p.Type)
      }
      def ptypebounds(gtpt: g.Tree): p.Type.Bounds = gtpt match {
        case g.TypeBoundsTree(glo, ghi) => p.Type.Bounds(if (glo.isEmpty) None else Some[p.Type](glo.cvt_!), if (ghi.isEmpty) None else Some[p.Type](ghi.cvt_!))
        case _ => unreachable
      }
      def parg(garg: g.Tree): p.Term.Arg = garg match {
        case g.Typed(expr, g.Ident(g.tpnme.WILDCARD_STAR)) => p.Term.Arg.Repeated(expr.cvt_!)
        case g.AssignOrNamedArg(lhs, rhs) => p.Term.Arg.Named(lhs.cvt_!, rhs.cvt_!)
        case _ => garg.cvt_! : p.Term
      }
      def pargs(gargs: List[g.Tree]): Seq[p.Term.Arg] = gargs.map(parg)
      def pargss(gargss: List[List[g.Tree]]): Seq[Seq[p.Term.Arg]] = gargss.map(pargs)
      def pstats(gparent: g.Tree, gstats: List[g.Tree]): List[p.Stat] = {
        object PatDefCore {
          def unapply(tree: g.MemberDef): Option[(g.Tree, g.Tree)] = tree match {
            // CASE #1: 0 and 2+ patterns in a val definition
            // ~$ typecheck 'locally { val List() = List() }'
            // [[syntax trees at end of typer]]// Scala source: tmp1T1gLt
            // scala.this.Predef.locally[Unit]({
            //   <synthetic> <artifact> private[this] val x$1: Unit = (immutable.this.Nil: List[Nothing] @unchecked) match {
            //     case immutable.this.List.unapplySeq[Nothing](<unapply-selector>) <unapply> () => ()
            //   };
            //   ()
            // })
            // [[syntax trees at end of typer]]// Scala source: tmpDlXMAx
            // scala.this.Predef.locally[Unit]({
            //   <synthetic> <artifact> private[this] val x$1: (Nothing, Nothing) = (immutable.this.Nil: List[Nothing] @unchecked) match {
            //     case immutable.this.List.unapplySeq[Nothing](<unapply-selector>) <unapply> ((x @ _), (y @ _)) => scala.Tuple2.apply[Nothing, Nothing](x, y)
            //   };
            //   val x: Nothing = x$1._1;
            //   val y: Nothing = x$1._2;
            //   ()
            // })
            // btw we can't trust annot.tpe.typeSymbol, because annot.tpe is bugged
            // for example, @unchecked in `List[Int] @unchecked` has type `List[Int] @unchecked` instead of the correct `unchecked`
            // see https://github.com/scala/scala/blob/73fb460c1cd20ee97556ec0867d17efaa795d129/src/compiler/scala/tools/nsc/typechecker/Typers.scala#L4048
            case in @ g.ValDef(_, _, g.EmptyTree, g.Match(g.Annotated(annot @ g.treeInfo.Applied(core, _, _), rhs), List(g.CaseDef(pat, g.EmptyTree, _))))
            if core.tpe.finalResultType.typeSymbolDirect == g.symbolOf[unchecked] && in.symbol.isSynthetic && in.symbol.isArtifact =>
              Some((pat, rhs))
            // CASE #2: 1 pattern in a val definition
            // ~$ typecheck 'locally { val List(x) = List() }'
            // [[syntax trees at end of typer]]// Scala source: tmpHlTi6k
            // scala.this.Predef.locally[Unit]({
            //   val x: Nothing = (immutable.this.Nil: List[Nothing] @unchecked) match {
            //     case immutable.this.List.unapplySeq[Nothing](<unapply-selector>) <unapply> ((x @ _)) => x
            //   };
            //   ()
            // })
            case in @ g.ValDef(_, _, g.EmptyTree, g.Match(g.Annotated(annot @ g.treeInfo.Applied(core, _, _), rhs), List(g.CaseDef(pat, g.EmptyTree, body @ g.Ident(_)))))
            if core.tpe.finalResultType.typeSymbolDirect == g.symbolOf[unchecked] =>
              val binds = pat.collect{ case x: g.Bind => x }
              if (binds.length == 1 && binds.head.symbol == body.symbol) Some((pat, rhs))
              else None
            // CASE #3: funnily enough, in a very degenerate case, we can also ends up with a def with the same structure
            // try `locally { implicit lazy val List() = List() }` if you care to know more
            case in @ g.DefDef(_, _, Nil, Nil, g.EmptyTree, g.Match(g.Annotated(annot @ g.treeInfo.Applied(core, _, _), rhs), List(g.CaseDef(pat, g.EmptyTree, _))))
            if core.tpe.finalResultType.typeSymbolDirect == g.symbolOf[unchecked] && in.symbol.isSynthetic && in.symbol.isArtifact =>
              Some((pat, rhs))
            case _ =>
              None
          }
        }
        object AnonymousClassDef {
          def unapply(tree: g.ClassDef): Option[g.Template] = if (tree.name == g.tpnme.ANON_CLASS_NAME) Some(tree.impl) else None
        }
        object RightAssociativeApplicationLhsTemporaryVal {
          def unapply(tree: g.ValDef): Option[g.Tree] = {
            if (tree.mods.isSynthetic && tree.mods.isArtifact && tree.name.startsWith(g.nme.FRESH_TERM_NAME_PREFIX)) Some(tree.rhs)
            else None
          }
        }
        object InlinableHoistedTemporaryVal {
          def unapply(tree: g.ValDef): Option[(g.Symbol, g.Tree)] = {
            if (tree.symbol.isSynthetic && tree.name.startsWith("ev$")) Some((tree.symbol, tree.rhs))
            else None
          }
        }
        val result = mutable.ListBuffer[p.Stat]()
        var i = 0
        while (i < gstats.length) {
          val gstat = gstats(i)
          i += 1
          val pstat = gstat match {
            case in @ PatDefCore(pat, rhs) =>
              if (in.symbol.isSynthetic && in.symbol.isArtifact) i += g.definitions.TupleClass.seq.indexOf(in.symbol.info.typeSymbolDirect) + 1
              val modbearer = gstats(i - 1).require[g.MemberDef] // TODO: mods for nullary patterns get irrevocably lost (`implicit val List() = List()`)
              if (!modbearer.symbol.isMutable) p.Defn.Val(pmods(modbearer), List(pat.cvt_! : p.Pat), None, rhs.cvt_!)
              else p.Defn.Var(pmods(modbearer), List(pat.cvt_! : p.Pat), None, Some[p.Term](rhs.cvt_!))
            case in @ AnonymousClassDef(templ) =>
              i += 1
              val q"new $$anon()" = gstats(i - 1)
              p.Term.New(templ.cvt_!)
            case in @ RightAssociativeApplicationLhsTemporaryVal(left) =>
              object Right {
                def unapply(gtree: g.Tree): Option[(g.Select, g.Tree, List[g.Tree])] = gtree match {
                  case g.treeInfo.Applied(op @ g.Select(qual, _), targs, List(List(leftRef @ g.Ident(_)))) if in.symbol == leftRef.symbol =>
                    val op1 = g.duplicateAndKeepPositions(op).setType(g.treeInfo.dissectApplied(gtree).callee.tpe).require[g.Select]
                    Some((op1, qual, targs))
                  case _ =>
                    None
                }
              }
              i += 1
              gstats(i - 1) match {
                // NOTE: typically, right-associative applications are desugared as follows:
                // ~$ typecheck '1 :: Nil'
                // [[syntax trees at end of typer]]// Scala source: tmpdPr5f0
                // {
                //   <synthetic> <artifact> val x$1: Int = 1;
                //   immutable.this.Nil.::[Int](x$1)
                // }
                // so we use this particular shape of a block to detect such applications
                case Right(op, qual, targs) =>
                  p.Term.ApplyInfix(left.cvt_!, op.symbol.asTerm.precvt(qual.tpe, op), targs.cvt_!, List(qual.cvt_!))
                // HOWEVER, under some conditions (but not always!)
                // applications of right-associative applications can move into the block as follows:
                // ~$ typecheck '(1 :: Nil)(0)'
                // [[syntax trees at end of typer]]// Scala source: tmp2C43uG
                // {
                //   <synthetic> <artifact> val x$1: Int = 1;
                //   immutable.this.Nil.::[Int](x$1).apply(0)
                // }
                // then we canonicalize the block by hoisting the extraneous applications and then convert it via normal means
                case gstat =>
                  def canonicalize(gparent: g.Block): g.Apply = {
                    val g.Block(List(gtempval @ RightAssociativeApplicationLhsTemporaryVal(_)), gstat) = gparent
                    def loop(gtree: g.Tree): g.Tree = gtree match {
                      case gtree @ Right(_, _, _) => g.treeCopy.Block(gparent, List(gtempval), gtree)
                      case gtree @ g.TypeApply(fn, targs) => g.treeCopy.TypeApply(gtree, loop(fn), targs)
                      case gtree @ g.Apply(fn, args) => g.treeCopy.Apply(gtree, loop(fn), args)
                      case gtree @ g.Select(qual, name) => g.treeCopy.Select(gtree, loop(qual), name)
                    }
                    loop(gstat).require[g.Apply]
                  }
                  canonicalize(gparent.require[g.Block]).cvt_! : p.Term
              }
            case in @ InlinableHoistedTemporaryVal(_, _) =>
              val inlinees = gstats.collect({ case InlinableHoistedTemporaryVal(sym, rhs) => (sym -> rhs) }).toMap
              require(i == 1 && inlinees.size == gstats.size - 1)
              i = gstats.length
              object inliner extends g.Transformer {
                override def transform(tree: g.Tree): g.Tree = tree match {
                  case tree @ g.Ident(_) if inlinees.contains(tree.symbol) => transform(inlinees(tree.symbol))
                  case _ => super.transform(tree)
                }
              }
              inliner.transform(gstats(i - 1)).copyAttrs(gparent).cvt_! : p.Term
            case in =>
              in.cvt_! : p.Stat
          }
          result += pstat.withOriginal(gstat)
        }
        result.toList
      }
      def gextractContextBounds(gtparams0: List[g.TypeDef], gimplicits0: List[g.ValDef]): (List[g.TypeDef], List[g.ValDef]) = {
        val (gbounds, gimplicits) = gimplicits0.partition(_.name.startsWith(g.nme.EVIDENCE_PARAM_PREFIX))
        val gtparams = gtparams0.map(gp => {
          val grawRelevantBounds = gbounds.filter(_.exists(_.symbol == gp.symbol))
          val (grawContextBounds, grawViewBounds) = grawRelevantBounds.partition(_.tpt.tpe.typeSymbolDirect != g.definitions.FunctionClass(1))
          val gcontextBounds = grawContextBounds.map{ case g.ValDef(_, _, g.AppliedTypeTree(gtpt, List(garg @ g.Ident(_))), _) if garg.symbol == gp.symbol => gtpt }
          val gviewBounds = grawViewBounds.map{ case g.ValDef(_, _, g.AppliedTypeTree(_, List(garg @ g.Ident(_), gtarget)), _) if garg.symbol == gp.symbol => gtarget }
          gp.appendMetadata("originalContextBounds" -> gcontextBounds, "originalViewBounds" -> gviewBounds)
        })
        (gtparams, gimplicits)
      }
    }

    import Helpers._
    val TermQuote = "shadow scala.meta quasiquotes"
    in.require[g.Tree].requireAttributed()
    in match {
      case g.EmptyTree =>
        unreachable
      case g.UnmappableTree =>
        unreachable
      case in @ g.PackageDef(pid, stats) if pt <:< typeOf[p.Stat] =>
        require(pid.name != g.nme.EMPTY_PACKAGE_NAME)
        stats match {
          case List(pkgobject: g.ModuleDef) if pkgobject.symbol.isPackageObject => pkgobject.cvt_! : p.Pkg.Object
          case _ => p.Pkg(pid.cvt_!, pstats(in, stats))
        }
      case in @ g.PackageDef(pid, stats) if pt <:< typeOf[p.Source] =>
        if (pid.name == g.nme.EMPTY_PACKAGE_NAME) p.Source(pstats(in, stats))
        else p.Source(List(in.cvt_! : p.Stat))
      case in @ g.ClassDef(_, _, tparams0, templ) =>
        require(in.symbol.isClass)
        if (in.symbol.isTrait) {
          val (tparams, implicits) = gextractContextBounds(tparams0, Nil)
          require(implicits.isEmpty) // NOTE: no context bounds for traits
          p.Defn.Trait(pmods(in), in.symbol.asClass.rawcvt(in), tparams.cvt, pfakector(in), templ.cvt)
        } else {
          val gctor = templ.body.find(_.symbol == in.symbol.primaryConstructor).get.require[g.DefDef]
          val q"$_ def $_[..$_](...$impreciseExplicitss)(implicit ..$implicits0): $_ = $_" = gctor
          // TODO: discern `class C` and `class C()`
          val explicitss = if (impreciseExplicitss.flatten.isEmpty) List() else impreciseExplicitss
          val (tparams, implicits) = gextractContextBounds(tparams0, implicits0)
          val paramss = if (implicits.nonEmpty) explicitss :+ implicits else explicitss
          val ctor = p.Ctor.Primary(pmods(gctor), p.Ctor.Name(in.name.toString).withDenot(in.symbol.primaryConstructor), paramss.cvt_!)
          p.Defn.Class(pmods(in), in.symbol.asClass.rawcvt(in), tparams.cvt, ctor, templ.cvt)
        }
      case in @ g.ModuleDef(_, _, templ) =>
        require(in.symbol.isModule && !in.symbol.isModuleClass)
        if (in.symbol.isPackageObject) p.Pkg.Object(pmods(in), in.symbol.asModule.rawcvt(in), pfakector(in), templ.cvt)
        else p.Defn.Object(pmods(in), in.symbol.asModule.rawcvt(in), pfakector(in), templ.cvt)
      case in @ g.ValDef(_, _, tpt, rhs) if pt <:< typeOf[p.Term.Param] =>
        // TODO: how do we really distinguish `case class C(val x: Int)` and `case class C(x: Int)`?
        require(in != g.noSelfType && in.symbol.isTerm)
        val pname = in.symbol.asTerm.anoncvt(in)
        val ptpe = if (tpt.nonEmpty) Some[p.Type.Arg](pvparamtpe(tpt)) else None
        val pdefault = if (rhs.nonEmpty) Some[p.Term](rhs.cvt_!) else None
        require(in.symbol.isAnonymous ==> pdefault.isEmpty)
        p.Term.Param(pmods(in), pname, ptpe, pdefault)
      case in @ g.ValDef(_, _, tpt, rhs) if pt <:< typeOf[p.Stat] =>
        // TODO: collapse desugared representations of pattern-based vals and vars
        require(in.symbol.isTerm)
        require(in.symbol.isDeferred ==> rhs.isEmpty)
        require(in.symbol.hasFlag(DEFAULTINIT) ==> rhs.isEmpty)
        val pname = p.Pat.Var.Term(in.symbol.asTerm.rawcvt(in))
        (in.symbol.isDeferred, in.symbol.isMutable || in.mods.isMutable) match {
          case (true, false) => p.Decl.Val(pmods(in), List(pname), tpt.cvt_!)
          case (true, true) => p.Decl.Var(pmods(in), List(pname), tpt.cvt_!)
          case (false, false) => p.Defn.Val(pmods(in), List(pname), if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None, rhs.cvt_!)
          case (false, true) => p.Defn.Var(pmods(in), List(pname), if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None, if (rhs.nonEmpty) Some[p.Term](rhs.cvt_!) else None)
        }
      case in @ g.DefDef(_, _, _, _, _, _) =>
        require(in.symbol.isMethod)
        val q"$_ def $_[..$tparams0](...$explicitss)(implicit ..$implicits0): $tpt = $body" = in
        val (tparams, implicits) = gextractContextBounds(tparams0, implicits0)
        val paramss = if (implicits.nonEmpty) explicitss :+ implicits else explicitss
        require(in.symbol.isDeferred ==> body.isEmpty)
        if (in.symbol.isConstructor) {
          p.Ctor.Secondary(pmods(in), p.Ctor.Name(in.name.toString).withDenot(in.symbol), paramss.cvt_!, body.cvt_!)
        } else if (in.symbol.isMacro) {
          require(tpt.nonEmpty) // TODO: support pre-2.12 macros with inferred return types
          val pbody = if (body != g.EmptyTree) (body.cvt_! : p.Term) else p.Term.Name("???").withDenot(g.definitions.Predef_???)
          p.Defn.Macro(pmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, paramss.cvt_!, tpt.cvt_!, pbody)
        } else if (in.symbol.isDeferred) {
          p.Decl.Def(pmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, paramss.cvt_!, tpt.cvt_!)
        } else {
          p.Defn.Def(pmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, paramss.cvt_!, if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None, body.cvt_!)
        }
      case in @ g.TypeDef(_, _, tparams0, tpt) if pt <:< typeOf[p.Type.Param] =>
        require(in.symbol.isType)
        val pname = in.symbol.asType.anoncvt(in)
        val tparams = tparams0.map(_.appendMetadata("originalContextBounds" -> Nil).appendMetadata("originalViewBounds" -> Nil))
        val pviewbounds = in.metadata("originalViewBounds").require[List[g.Tree]].map(_.cvt_! : p.Type)
        val pcontextbounds = in.metadata("originalContextBounds").require[List[g.Tree]].map(_.cvt_! : p.Type)
        p.Type.Param(pmods(in), pname, tparams.cvt, ptypebounds(tpt), pviewbounds, pcontextbounds)
      case in @ g.TypeDef(_, _, tparams0, tpt) if pt <:< typeOf[p.Stat] =>
        require(in.symbol.isType)
        val tparams = tparams0.map(_.appendMetadata("originalContextBounds" -> Nil).appendMetadata("originalViewBounds" -> Nil))
        if (in.symbol.isDeferred) p.Decl.Type(pmods(in), in.symbol.asType.rawcvt(in), tparams.cvt, ptypebounds(tpt))
        else p.Defn.Type(pmods(in), in.symbol.asType.rawcvt(in), tparams.cvt, tpt.cvt_!)
      case in @ g.LabelDef(name, params, rhs) =>
        require(params.isEmpty)
        rhs match {
          case q"if ($cond) { $body; $cont } else ()" if name.startsWith(g.nme.WHILE_PREFIX) => p.Term.While(cond.cvt_!, body.cvt_!)
          case q"$body; if ($cond) $cont else ()" if name.startsWith(g.nme.DO_WHILE_PREFIX) => p.Term.Do(body.cvt_!, cond.cvt_!)
          case _ => unreachable
        }
      case in @ g.Import(expr, selectors) =>
        // TODO: collapse desugared chains of imports
        // TODO: distinguish `import foo.x` from `import foo.{x => x}`
        p.Import(List(p.Import.Clause(expr.cvt_!, selectors.map({
          case g.ImportSelector(g.nme.WILDCARD, _, null, _)           => p.Import.Selector.Wildcard()
          case g.ImportSelector(name1, _, name2, _) if name1 == name2 => p.Import.Selector.Name(name1.toString)
          case g.ImportSelector(name1, _, name2, _) if name1 != name2 => p.Import.Selector.Rename(name1.toString, name2.toString)
          case g.ImportSelector(name, _, g.nme.WILDCARD, _)           => p.Import.Selector.Unimport(name.toString)
        }))))
      case in @ g.Template(_, _, _) =>
        val SyntacticTemplate(gsupersym, gparents, gself, gearlydefns, gstats) = in
        val gsupersyms = if (gparents.nonEmpty) gsupersym +: List.fill(gparents.length - 1)(g.NoSymbol) else Nil
        val pparents = gsupersyms.zip(gparents).map({ case (gsupersym, gparent) =>
          val gapplied = g.treeInfo.dissectApplied(gparent)
          val gctorsym = gsupersym.orElse(gparent.tpe.typeSymbolDirect)
          pctorcall(gparent, gapplied.callee, gctorsym, gapplied.argss)
        })
        val pself = {
          // NOTE: if we're converting a template of a CompoundTypeTree
          // then it won't have any symbol set, so our p.Name.Anonymous is going to remain denotation-less
          val pdumbselfname = if (gself.name == g.nme.WILDCARD) p.Name.Anonymous() else p.Term.Name(gself.alias)
          val gselfsym = in.symbol.owner.thisSym
          val pselfname = if (in.symbol.owner != gselfsym) pdumbselfname.withDenot(gselfsym) else pdumbselfname
          val pselftpe = if (gself.tpt.nonEmpty) Some[p.Type.Arg](pvparamtpe(gself.tpt)) else None
          p.Term.Param(Nil, pselfname, pselftpe, None).withOriginal(gself)
        }
        val hasStats = gstats.nonEmpty || in.symbol.owner.name == g.tpnme.ANON_CLASS_NAME
        p.Template(gearlydefns.cvt_!, pparents, pself, if (hasStats) Some(pstats(in, gstats)) else None)
      case in: g.Block =>
        // NOTE: if a block with non-trivial stats has been collapsed to a single stat
        // then this means that it's a desugaring of a language construct (e.g. `new C{}` or `1 :: Nil`)
        // and it shouldn't be a p.Term.Block, but rather a resulting stat instead
        val gstats = in.stats :+ in.expr
        (gstats, Helpers.pstats(in, gstats)) match {
          case ((nel @ _ :+ _) :+ gexpr, Nil :+ pstat) => pstat.require[p.Term]
          case (_, pstats) => p.Term.Block(pstats)
        }
      case in @ g.CaseDef(pat, guard, body @ q"..$stats") =>
        p.Case(pat.cvt_!, if (guard.nonEmpty) Some[p.Term](guard.cvt_!) else None, p.Term.Block(pstats(in, stats)).withOriginal(body))
      case g.Alternative(fst :: snd :: Nil) =>
        p.Pat.Alternative(fst.cvt_!, snd.cvt_!)
      case in @ g.Alternative(hd :: rest) =>
        p.Pat.Alternative(hd.cvt_!, g.Alternative(rest).setType(in.tpe).require[g.Alternative].cvt)
      case g.Ident(g.nme.WILDCARD) =>
        p.Pat.Wildcard()
      case g.Bind(g.tpnme.WILDCARD, g.EmptyTree) =>
        p.Pat.Type.Wildcard()
      case g.Star(g.Ident(g.nme.WILDCARD)) =>
        p.Pat.Arg.SeqWildcard()
      case in @ g.Bind(_, g.Ident(g.nme.WILDCARD)) =>
        // TODO: discern `case x => ...` and `case x @ _ => ...`
        require(in.symbol.isTerm)
        p.Pat.Var.Term(in.symbol.asTerm.rawcvt(in))
      case in @ g.Bind(_, g.EmptyTree) =>
        require(in.symbol.isType)
        p.Pat.Var.Type(in.symbol.asType.rawcvt(in))
      case in @ g.Bind(_, g.Typed(g.Ident(g.nme.WILDCARD), tpt)) =>
        require(in.symbol.isTerm)
        p.Pat.Typed(p.Pat.Var.Term(in.symbol.asTerm.rawcvt(in)), tpt.cvt_!)
      case in @ g.Bind(name, tree) =>
        require(in.symbol.isTerm)
        require(name == in.symbol.name)
        p.Pat.Bind(p.Pat.Var.Term(in.symbol.asTerm.rawcvt(in)), tree.cvt_!)
      case g.Function(params, body) =>
        // NOTE: need to be careful to discern `_ + 2` and `_ => 2`
        // because in both cases parameter names are `x$...`
        // also need to be careful not to mistakenly recognize `(_, y) => y` as shorthand syntax
        val isShorthand = params.nonEmpty && params.forall(param => param.symbol.isAnonymous && body.exists(_.symbol == param.symbol))
        if (isShorthand) (body.cvt_! : p.Term) else p.Term.Function(params.cvt_!, body.cvt_!)
      case in @ g.Assign(lhs, rhs) =>
        in match {
          // TODO: this is a very-very weird representation for op-assignments
          // representing `foo.x_=(value)` as an Assign to a getter selection totally okay
          // because `foo.x = value` doesn't compiler without a getter
          // representing `foo.update(args, value)` as an Assign to an Apply is moderately weird
          // because one can write `foo(args) = value` even without having foo to be applicable
          // but this encoding is so awkward that I wonder whether it's ever going to be useful
          case DesugaredOpAssign(g.Select(lhs, op), args) =>
            // NOTE: funny thing is that desugared op-assigns can't have explicitly provided type arguments
            // something like `class C { def +[T](x: T): C = ??? }; var c = new C; c +=[Int] 2` isn't going to compile
            p.Term.ApplyInfix(lhs.cvt_!, p.Term.Name(op.decodedName.toString), Nil, args.cvt_!)
          case _ =>
            p.Term.Assign(lhs.cvt_!, rhs.cvt_!)
        }
      case g.AssignOrNamedArg(lhs, rhs) =>
        // NOTE: handled in parg/pargs/pargss
        unreachable
      case g.If(cond, thenp, elsep) =>
        p.Term.If(cond.cvt_!, thenp.cvt_!, elsep.cvt_!)
      case g.Match(selector, cases) =>
        if (selector == g.EmptyTree) p.Term.PartialFunction(cases.cvt)
        else p.Term.Match(selector.cvt_!, cases.cvt)
      case g.Return(expr) =>
        p.Term.Return(expr.cvt_!)
      case g.Try(body, catches, finalizer) =>
        object CatchExpr {
          def unapply(tree: g.Tree): Option[g.Tree] = tree match {
            case g.CaseDef(pq"$scrutdef: Throwable", g.EmptyTree, g.Block(List(g.ValDef(_, tempdef, _, handler)), _))
            if scrutdef.startsWith(g.nme.FRESH_TERM_NAME_PREFIX) && tempdef.startsWith("catchExpr") => Some(handler)
            case _ => None
          }
        }
        val finallyp = if (finalizer.nonEmpty) Some[p.Term](finalizer.cvt_!) else None
        catches match {
          case CatchExpr(handler) :: Nil => p.Term.TryWithTerm(body.cvt_!, handler.cvt_!, finallyp)
          case Nil => p.Term.TryWithCases(body.cvt_!, Nil, finallyp)
          case _ => p.Term.TryWithCases(body.cvt_!, catches.cvt, finallyp)
        }
      case g.Throw(expr) =>
        p.Term.Throw(expr.cvt_!)
      case g.New(_) =>
        unreachable
      case g.Typed(expr, g.Function(Nil, g.EmptyTree)) if pt <:< typeOf[p.Term] =>
        p.Term.Eta(expr.cvt_!)
      case g.Typed(expr, g.EmptyTree) =>
        ???
      case g.Typed(expr, tpt) if pt <:< typeOf[p.Term] =>
        p.Term.Ascribe(expr.cvt_!, tpt.cvt_!)
      case g.Typed(expr, tpt) if pt <:< typeOf[p.Pat] =>
        p.Pat.Typed(expr.cvt_!, tpt.cvt_!)
      case in @ g.TypeApply(fn, targs) =>
        p.Term.ApplyType(fn.cvt_!, targs.cvt_!)
      case in @ g.Apply(fn, args) if pt <:< typeOf[p.Term] =>
        // TODO: figure out whether the programmer actually wrote the interpolator or they were explicitly using a desugaring
        // TODO: figure out whether the programmer actually wrote the infix application or they were calling a symbolic method using a dot
        // TODO: infer the difference between `new X` vs `new X()`
        // TODO: figure out whether the programmer actually used the tuple syntax or they were calling the tuple companion explicitly
        // TODO: figure out whether the programmer actually wrote `foo(...) = ...` or it was `foo.update(..., ...)`
        // TODO: figure out whether the programmer actually wrote `'foo` or it was 'Symbol("foo")'
        // TODO: support applyDynamic, but don't forget to account for .apply desugarings (see DesugaredInterpolation for an example of what I'm talking about)
        in match {
          case q"new $_(...$argss0)" =>
            val g.treeInfo.Applied(ctorref @ g.Select(g.New(tpt), _), _, _) = in
            val argss = if (argss0.isEmpty && in.symbol.info.paramss.flatten.nonEmpty) List(List()) else argss0
            val supercall = pctorcall(in, tpt, ctorref.symbol, argss)
            val self = p.Term.Param(Nil, p.Name.Anonymous().withDenot(tpt.tpe.typeSymbol), None, None).withOriginal(g.noSelfType)
            val templ = p.Template(Nil, List(supercall), self, None)
            p.Term.New(templ)
          case DesugaredSetter(lhs, rhs) =>
            p.Term.Assign(lhs.cvt_!, rhs.cvt_!)
          case DesugaredUpdate(lhs, argss, rhs) =>
            p.Term.Update(lhs.cvt_!, argss.cvt_!, rhs.cvt_!)
          case DesugaredOpAssign(g.Select(lhs, op), args) =>
            p.Term.ApplyInfix(lhs.cvt_!, p.Term.Name(op.decodedName.toString), Nil, args.cvt_!)
          case DesugaredSymbolLiteral(value) =>
            p.Lit.Symbol(value) : p.Term
          case DesugaredTuple(arg) =>
            p.Term.Tuple(args.cvt_!)
          case DesugaredInterpolation(prefix, parts, args) =>
            p.Term.Interpolate(prefix.symbol.asTerm.precvt(prefix.qualifier.tpe, prefix), parts.cvt_!, args.cvt_!)
          case DesugaredFor(enums, body, isYield) =>
            val penums = enums.map({
              case Generator(pat, rhs) => p.Enumerator.Generator(pat.cvt_!, rhs.cvt_!)
              case Val(pat, rhs) => p.Enumerator.Val(pat.cvt_!, rhs.cvt_!)
              case Guard(cond) => p.Enumerator.Guard(cond.cvt_!)
            })
            if (isYield) p.Term.ForYield(penums, body.cvt_!)
            else p.Term.For(penums, body.cvt_!)
          case q"$lhs.$op($arg)" if op.looksLikeInfix && !lhs.isInstanceOf[g.Super] =>
            p.Term.ApplyInfix(lhs.cvt_!, fn.symbol.asTerm.precvt(lhs.tpe, fn), Nil, List(parg(arg)))
          case DesugaredApply(target, args) =>
            p.Term.Apply(target.cvt_!, pargs(args))
          case _ =>
            p.Term.Apply(fn.cvt_!, pargs(args))
        }
      case in @ g.Apply(fn, args) if pt <:< typeOf[p.Pat] =>
        // TODO: infer Extract vs ExtractInfix
        // TODO: also figure out Interpolate
        // TODO: also figure out whether the user wrote p.Pat.Tuple themselves, or it was inserted by the compiler
        if (g.definitions.isTupleSymbol(in.symbol.companion)) p.Pat.Tuple(args.cvt_!)
        else {
          val (ref, targs) = in match { case q"$ref.$unapply[..$targs](..$_)" => (ref, targs); case q"$ref[..$targs](..$_)" => (ref, targs) }
          (ref, args) match {
            case (ref @ g.Ident(name), List(lhs, rhs)) if name.looksLikeInfix && targs.isEmpty => p.Pat.ExtractInfix(lhs.cvt_!, ref.cvt_!, List(rhs.cvt_!))
            case _ => p.Pat.Extract(ref.cvt_!, targs.cvt_!, args.cvt_!)
          }
        }
      case in @ g.ApplyDynamic(_, _) =>
        unreachable
      case in @ g.Super(qual @ g.This(_), mix) =>
        require(in.symbol.isClass)
        val superdumb = p.Term.Super((qual.cvt : p.Term.This).qual, if (mix != g.tpnme.EMPTY) Some(in.mix.toString) else None)
        superdumb.withDenot(qual.tpe, in.tpe.typeSymbol)
      case in @ g.This(qual) =>
        require(!in.symbol.isPackageClass)
        p.Term.This(if (qual != g.tpnme.EMPTY) Some(in.symbol.name.toString) else None).withDenot(in.tpe.prefix, in.symbol)
      case in: g.PostfixSelect =>
        unreachable
      case in @ g.Select(qual, name) =>
        // TODO: discern unary applications via !x and via explicit x.unary_!
        // TODO: also think how to detect unary applications that have implicit arguments
        require(name != g.nme.CONSTRUCTOR)
        if (name.isTermName) {
          if (name.toString.startsWith("unary_")) {
            val in1 = g.treeCopy.Select(in, qual, g.TermName(name.toString.stripPrefix("unary_")))
            p.Term.ApplyUnary(in.symbol.asTerm.precvt(qual.tpe, in1), qual.cvt_!)
          } else {
            p.Term.Select(qual.cvt_!, in.symbol.asTerm.precvt(qual.tpe, in))
          }
        } else {
          p.Type.Select(qual.cvt_!, in.symbol.asType.precvt(qual.tpe, in))
        }
      case in @ g.Ident(_) =>
        // NOTE: Ident(<term name>) with a type symbol attached to it
        // is the encoding that the ensugarer uses to denote a self reference
        // also see the ensugarer for more information
        // NOTE: primary ctor calls in secondary ctors are represented as follows:
        // Apply(Select(This(TypeName("C")), nme.CONSTRUCTOR), List(...))
        // therefore we need to detect this special select and transform it accordingly
        if (in.name == g.nme.CONSTRUCTOR) p.Ctor.Name("this").withDenot(in.symbol)
        else if (in.isTerm && in.symbol.isType) p.Term.Name(in.alias)
        else if (in.isTerm && in.symbol.isAnonymous) p.Term.Placeholder()
        else if (in.isType && in.symbol.isAnonymous) p.Type.Placeholder(ptypebounds(in.metadata("originalBounds").require[g.Tree]))
        else in.symbol.rawcvt(in)
      case g.ReferenceToBoxed(_) =>
        ???
      case in @ g.Literal(const) =>
        const.rawcvt
      case g.Parens(_) =>
        unreachable
      case g.DocDef(_, _) =>
        unreachable
      case g.Annotated(annot, arg) if pt <:< typeOf[p.Term] =>
        val (parg, pannots) = (arg.cvt_! : p.Term) match { case p.Term.Annotate(arg, annots) => (arg, annots); case arg => (arg, Nil) }
        p.Term.Annotate(parg, pannots :+ pannot(annot))
      case g.Annotated(annot, arg) if pt <:< typeOf[p.Type] =>
        val (parg, pannots) = (arg.cvt_! : p.Type) match { case p.Type.Annotate(arg, annots) => (arg, annots); case arg => (arg, Nil) }
        p.Type.Annotate(parg, pannots :+ pannot(annot))
      case g.Annotated(annot, arg) if pt <:< typeOf[p.Pat.Type] =>
        val (parg, pannots) = (arg.cvt_! : p.Pat.Type) match { case p.Pat.Type.Annotate(arg, annots) => (arg, annots); case arg => (arg, Nil) }
        p.Pat.Type.Annotate(parg, pannots :+ pannot(annot))
      case g.ArrayValue(_, _) =>
        unreachable
      case g.InjectDerivedValue(_) =>
        unreachable
      case g.SelectFromArray(_, _, _) =>
        unreachable
      case g.UnApply(_, _) =>
        unreachable
      case in @ g.TypeTree() =>
        unreachable
      case in @ g.TypeTreeWithDeferredRefCheck() =>
        unreachable
      case in @ g.SingletonTypeTree(ref) =>
        p.Type.Singleton(ref.cvt_!)
      case in @ g.CompoundTypeTree(templ) if pt <:< typeOf[p.Type] =>
        val template @ p.Template(early, parents, _, stats) = templ.cvt : p.Template
        require(template.isCompoundTypeCompatible)
        p.Type.Compound(parents.map(_.ctorTpe), stats.getOrElse(Nil))
      case in @ g.CompoundTypeTree(templ) if pt <:< typeOf[p.Pat.Type] =>
        // TODO: this doesn't handle situations like `List(42) match { case _: (List[t] { def head: Int }) => }`
        val template @ p.Template(early, parents, _, stats) = templ.cvt : p.Template
        require(template.isCompoundTypeCompatible)
        p.Pat.Type.Compound(parents.map(_.ctorTpe).map(_.patTpe), stats.getOrElse(Nil))
      case in @ g.AppliedTypeTree(tpt, args) if pt <:< typeOf[p.Type] =>
        // TODO: infer whether that was really Apply, Function or Tuple
        // TODO: precisely infer whether that was infix application or normal application
        if (g.definitions.FunctionClass.seq.contains(tpt.tpe.typeSymbolDirect)) p.Type.Function(args.init.cvt_!, args.last.cvt_!)
        else if (g.definitions.TupleClass.seq.contains(tpt.tpe.typeSymbolDirect) && args.length > 1) p.Type.Tuple(args.cvt_!)
        else in match {
          case g.AppliedTypeTree(tpt @ g.Ident(name), List(lhs, rhs)) if name.looksLikeInfix => p.Type.ApplyInfix(lhs.cvt_!, tpt.cvt_!, rhs.cvt_!)
          case _ => p.Type.Apply(tpt.cvt_!, args.cvt_!)
        }
      case in @ g.AppliedTypeTree(tpt, args) if pt <:< typeOf[p.Pat.Type] =>
        // TODO: infer whether that was really Apply, Function or Tuple
        // TODO: precisely infer whether that was infix application or normal application
        if (g.definitions.FunctionClass.seq.contains(tpt.tpe.typeSymbolDirect)) p.Pat.Type.Function(args.init.cvt_!, args.last.cvt_!)
        else if (g.definitions.TupleClass.seq.contains(tpt.tpe.typeSymbolDirect) && args.length > 1) p.Pat.Type.Tuple(args.cvt_!)
        else in match {
          case g.AppliedTypeTree(tpt @ g.Ident(name), List(lhs, rhs)) if name.looksLikeInfix => p.Pat.Type.ApplyInfix(lhs.cvt_!, tpt.cvt_!, rhs.cvt_!)
          case _ => p.Pat.Type.Apply(tpt.cvt_!, args.cvt_!)
        }
      case in @ g.ExistentialTypeTree(tpt, whereClauses) if pt <:< typeOf[p.Type] =>
        val isShorthand = whereClauses.exists(_.symbol.isAnonymous)
        if (isShorthand) (in.shorthand.cvt_! : p.Type) else p.Type.Existential(tpt.cvt_!, whereClauses.cvt_!)
      case in @ g.ExistentialTypeTree(tpt, whereClauses) if pt <:< typeOf[p.Pat.Type] =>
        val isShorthand = whereClauses.exists(_.symbol.isAnonymous)
        if (isShorthand) (in.shorthand.cvt_! : p.Pat.Type) else p.Pat.Type.Existential(tpt.cvt_!, whereClauses.cvt_!)
      case in @ g.SelectFromTypeTree(qual, name) if pt <:< typeOf[p.Type] =>
        p.Type.Project(qual.cvt_!, in.symbol.asType.precvt(qual.tpe, in))
      case in @ g.SelectFromTypeTree(qual, name) if pt <:< typeOf[p.Pat.Type] =>
        p.Pat.Type.Project(qual.cvt_!, in.symbol.asType.precvt(qual.tpe, in))
      case in @ g.TypeBoundsTree(_, _) =>
        unreachable
      // NOTE: this derivation is ambiguous because of at least g.ValDef, g.TypeDef and g.Typed
      // case _: g.Tree =>
      //   derive
    }
  }
}
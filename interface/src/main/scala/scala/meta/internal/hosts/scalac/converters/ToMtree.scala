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
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.reflect.runtime.universe.{Type => Pt}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.parsers.Helpers.{XtensionTermOps => _, _}

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
trait ToMtree extends GlobalToolkit with MetaToolkit {
  self: Api =>

  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
  import g.Quasiquote

  @converter def toMtree(in: Any, pt: Pt): Any = {
    object Helpers extends g.ReificationSupportImpl { self =>
      def mctorcall(in: g.Tree, gtpt: g.Tree, gctor: g.Symbol, gargss: Seq[Seq[g.Tree]]): m.Term = {
        val mctor = m.Ctor.Name(gtpt.tpe.typeSymbolDirect.name.decoded).withDenot(gtpt.tpe, gctor)
        val mcore = (gtpt.cvt_! : m.Type).ctorRef(mctor).require[m.Term]
        val margss = gargss.map(_.map(marg))
        margss.foldLeft(mcore)((mcurr, margs) => m.Term.Apply(mcurr, margs)).withTpe(in.tpe)
      }
      def mfakector(gparent: g.MemberDef): m.Ctor.Primary = {
        val ctor = gparent match { case _: g.ClassDef => gparent.symbol; case _: g.ModuleDef => gparent.symbol.moduleClass.primaryConstructor }
        val name = m.Ctor.Name(gparent.name.toString).withDenot(gparent.symbol)
        m.Ctor.Primary(Nil, name, Nil)
      }
      def mannot(gannot: g.Tree): m.Mod.Annot = {
        val q"new $gtpt(...$gargss)" = gannot
        val g.treeInfo.Applied(ctorref @ g.Select(g.New(tpt), _), _, _) = gannot
        m.Mod.Annot(mctorcall(gannot, gtpt, ctorref.symbol, gargss))
      }
      def mannots(gannots: Seq[g.Tree]): Seq[m.Mod.Annot] = {
        gannots.map(mannot)
      }
      def mmods(gmdef: g.MemberDef): Seq[m.Mod] = {
        def annotationMods(gmdef: g.MemberDef): Seq[m.Mod] = mannots(gmdef.mods.annotations)
        def accessQualifierMods(gmdef: g.MemberDef): Seq[m.Mod] = {
          val gmods = gmdef.mods
          val gsym = gmdef.symbol.getterIn(gmdef.symbol.owner).orElse(gmdef.symbol)
          val gpriv = gsym.privateWithin.orElse(gmdef.symbol.owner)
          if (gmods.hasFlag(SYNTHETIC) && gmods.hasFlag(ARTIFACT)) {
            // NOTE: some sick artifact vals produced by mkPatDef can be private to method (whatever that means)
            // so we can't invoke paccessqual on them, because that will produce crazy results
            Nil
          } else if (gmods.hasFlag(LOCAL)) {
            if (gmods.hasFlag(PROTECTED)) List(m.Mod.Protected(m.Term.This(m.Name.Anonymous().withDenot(gpriv))))
            else if (gmods.hasFlag(PRIVATE)) List(m.Mod.Private(m.Term.This(m.Name.Anonymous().withDenot(gpriv))))
            else unreachable(debug(gmods))
          } else if (gmods.hasAccessBoundary && gpriv != g.NoSymbol) {
            // TODO: `private[pkg] class C` doesn't have PRIVATE in its flags
            // so we need to account for that!
            if (gmods.hasFlag(PROTECTED)) List(m.Mod.Protected(gpriv.rawcvt(g.Ident(gpriv)).require[m.Name.Qualifier]))
            else List(m.Mod.Private(gpriv.rawcvt(g.Ident(gpriv)).require[m.Name.Qualifier]))
          } else {
            if (gmods.hasFlag(PROTECTED)) List(m.Mod.Protected(m.Name.Anonymous().withDenot(gsym.owner)))
            else if (gmods.hasFlag(PRIVATE)) List(m.Mod.Private(m.Name.Anonymous().withDenot(gsym.owner)))
            else Nil
          }
        }
        def otherMods(gmdef: g.MemberDef): Seq[m.Mod] = {
          val mmods = scala.collection.mutable.ListBuffer[m.Mod]()
          val gmods = gmdef.mods
          if (gmods.hasFlag(IMPLICIT)) mmods += m.Mod.Implicit()
          if (gmods.hasFlag(FINAL)) mmods += m.Mod.Final()
          if (gmods.hasFlag(SEALED)) mmods += m.Mod.Sealed()
          if (gmods.hasFlag(OVERRIDE)) mmods += m.Mod.Override()
          if (gmods.hasFlag(CASE)) mmods += m.Mod.Case()
          if (gmods.hasFlag(ABSTRACT) && gmdef.isInstanceOf[g.ClassDef] && !gmods.hasFlag(TRAIT)) mmods += m.Mod.Abstract()
          if (gmods.hasFlag(ABSOVERRIDE)) { mmods += m.Mod.Abstract(); mmods += m.Mod.Override() }
          if (gmods.hasFlag(COVARIANT) && gmdef.isInstanceOf[g.TypeDef]) mmods += m.Mod.Covariant()
          if (gmods.hasFlag(CONTRAVARIANT) && gmdef.isInstanceOf[g.TypeDef]) mmods += m.Mod.Contravariant()
          if (gmods.hasFlag(LAZY)) mmods += m.Mod.Lazy()
          mmods.toList
        }
        def valVarParamMods(gmdef: g.MemberDef): Seq[m.Mod] = {
          val mmods = scala.collection.mutable.ListBuffer[m.Mod]()
          val ggetter = gmdef.symbol.owner.filter(_.isPrimaryConstructor).map(_.owner.info.member(gmdef.name))
          val gfield = ggetter.map(_.owner.info.member(ggetter.localName))
          val isApplicable = gmdef.symbol.owner.isPrimaryConstructor && gfield != g.NoSymbol
          if (isApplicable && gfield.isMutable) mmods += m.Mod.VarParam()
          if (isApplicable && !gfield.isMutable && !gfield.owner.isCase) mmods += m.Mod.ValParam()
          mmods.toList
        }
        val result = annotationMods(gmdef) ++ accessQualifierMods(gmdef) ++ otherMods(gmdef) ++ valVarParamMods(gmdef)
        // TODO: we can't discern `class C(x: Int)` and `class C(private[this] val x: Int)`
        // so let's err on the side of the more popular option
        if (gmdef.symbol.owner.isPrimaryConstructor) result.filter({
          case m.Mod.Private(m.Term.This(_)) => false
          case _ => true
        }) else result
      }
      def mtypebounds(gtpt: g.Tree): m.Type.Bounds = gtpt match {
        case g.TypeBoundsTree(glo, ghi) => m.Type.Bounds(if (glo.isEmpty) None else Some[m.Type](glo.cvt_!), if (ghi.isEmpty) None else Some[m.Type](ghi.cvt_!))
        case _ => unreachable(debug(gtpt, g.showRaw(gtpt)))
      }
      def marg(garg: g.Tree): m.Term.Arg = garg match {
        case g.Typed(expr, g.Ident(g.tpnme.WILDCARD_STAR)) => m.Term.Arg.Repeated(expr.cvt_!)
        case g.AssignOrNamedArg(lhs, rhs) => m.Term.Arg.Named(lhs.cvt_!, rhs.cvt_!)
        case _ => garg.cvt_! : m.Term
      }
      def margs(gargs: List[g.Tree]): Seq[m.Term.Arg] = gargs.map(marg)
      def margss(gargss: List[List[g.Tree]]): Seq[Seq[m.Term.Arg]] = gargss.map(margs)
      def mstats(gparent: g.Tree, gstats: List[g.Tree]): List[m.Stat] = {
        object PatDefCore {
          object MaybeTyped {
            def unapply(tree: g.Tree): Option[(g.Tree, g.Tree)] = tree match {
              case g.Typed(tree, tpt) => Some((tree, tpt))
              case tree => Some((tree, g.EmptyTree))
            }
          }
          def unapply(tree: g.MemberDef): Option[(g.Tree, g.Tree, g.Tree)] = tree match {
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
            case in @ g.ValDef(_, _, g.EmptyTree, g.Match(MaybeTyped(g.Annotated(annot @ g.treeInfo.Applied(core, _, _), rhs), tpt), List(g.CaseDef(pat, g.EmptyTree, _))))
            if core.tpe.finalResultType.typeSymbolDirect == g.symbolOf[unchecked] && in.symbol.isSynthetic && in.symbol.isArtifact =>
              Some((pat, tpt, rhs))
            // CASE #2: 1 pattern in a val definition
            // ~$ typecheck 'locally { val List(x) = List() }'
            // [[syntax trees at end of typer]]// Scala source: tmpHlTi6k
            // scala.this.Predef.locally[Unit]({
            //   val x: Nothing = (immutable.this.Nil: List[Nothing] @unchecked) match {
            //     case immutable.this.List.unapplySeq[Nothing](<unapply-selector>) <unapply> ((x @ _)) => x
            //   };
            //   ()
            // })
            case in @ g.ValDef(_, _, g.EmptyTree, g.Match(MaybeTyped(g.Annotated(annot @ g.treeInfo.Applied(core, _, _), rhs), tpt), List(g.CaseDef(pat, g.EmptyTree, body @ g.Ident(_)))))
            if core.tpe.finalResultType.typeSymbolDirect == g.symbolOf[unchecked] =>
              val binds = pat.collect{ case x: g.Bind => x }
              if (binds.length == 1 && binds.head.symbol == body.symbol) Some((pat, tpt, rhs))
              else None
            // CASE #3: funnily enough, in a very degenerate case, we can also ends up with a def with the same structure
            // try `locally { implicit lazy val List() = List() }` if you care to know more
            case in @ g.DefDef(_, _, Nil, Nil, g.EmptyTree, g.Match(MaybeTyped(g.Annotated(annot @ g.treeInfo.Applied(core, _, _), rhs), tpt), List(g.CaseDef(pat, g.EmptyTree, _))))
            if core.tpe.finalResultType.typeSymbolDirect == g.symbolOf[unchecked] && in.symbol.isSynthetic && in.symbol.isArtifact =>
              Some((pat, tpt, rhs))
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
        val result = mutable.ListBuffer[m.Stat]()
        var i = 0
        while (i < gstats.length) {
          val gstat = gstats(i)
          i += 1
          val mstat = gstat match {
            case in @ PatDefCore(pat, tpt, rhs) =>
              if (in.symbol.isSynthetic && in.symbol.isArtifact) i += g.definitions.TupleClass.seq.indexOf(in.symbol.info.typeSymbolDirect) + 1
              val modbearer = gstats(i - 1).require[g.MemberDef] // TODO: mods for nullary patterns get irrevocably lost (`implicit val List() = List()`)
              val mtpt = if (tpt.nonEmpty) Some[m.Type](tpt.cvt_!) else None
              if (!modbearer.symbol.isMutable) m.Defn.Val(mmods(modbearer), List(pat.cvt_! : m.Pat), mtpt, rhs.cvt_!)
              else m.Defn.Var(mmods(modbearer), List(pat.cvt_! : m.Pat), mtpt, Some[m.Term](rhs.cvt_!))
            case in @ AnonymousClassDef(templ) =>
              i += 1
              val gstat @ q"new $$anon()" = gstats(i - 1)
              m.Term.New(templ.cvt_!).withTpe(gstat.tpe)
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
                case gstat @ Right(op, qual, targs) =>
                  m.Term.ApplyInfix(left.cvt_!, op.symbol.asTerm.precvt(qual.tpe, op), targs.cvt_!, List(qual.cvt_!)).withTpe(gstat.tpe)
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
                  canonicalize(gparent.require[g.Block]).cvt_! : m.Term
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
              inliner.transform(gstats(i - 1)).copyAttrs(gparent).cvt_! : m.Term
            case in @ g.PackageDef(pid, stats) =>
              stats match {
                case List(pkgobject: g.ModuleDef) if pkgobject.symbol.isPackageObject => pkgobject.cvt_! : m.Pkg.Object
                case _ => m.Pkg(pid.cvt_!, mstats(in, stats))
              }
            case in =>
              in.cvt_! : m.Stat
          }
          result += mstat
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
    val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
    in.require[g.Tree].requireAttributed()
    in match {
      case g.EmptyTree =>
        unreachable
      case g.UnmappableTree =>
        unreachable
      case in @ g.PackageDef(pid, stats) =>
        val mstats = {
          if (pid.name == g.nme.EMPTY_PACKAGE_NAME) Helpers.mstats(in, stats)
          else stats match {
            case List(pkgobject: g.ModuleDef) if pkgobject.symbol.isPackageObject => List(pkgobject.cvt_! : m.Pkg.Object)
            case _ => List(m.Pkg(pid.cvt_!, Helpers.mstats(in, stats)))
          }
        }
        m.Source(mstats)
      case in @ g.ClassDef(_, _, tparams0, templ) =>
        require(in.symbol.isClass)
        if (in.symbol.isTrait) {
          val (tparams, implicits) = gextractContextBounds(tparams0, Nil)
          require(implicits.isEmpty) // NOTE: no context bounds for traits
          m.Defn.Trait(mmods(in), in.symbol.asClass.rawcvt(in), tparams.cvt, mfakector(in), templ.cvt)
        } else {
          val gctor = templ.body.find(_.symbol == in.symbol.primaryConstructor).get.require[g.DefDef]
          val q"$_ def $_[..$_](...$impreciseExplicitss)(implicit ..$implicits0): $_ = $_" = gctor
          // TODO: discern `class C` and `class C()`
          val explicitss = if (impreciseExplicitss.flatten.isEmpty) List() else impreciseExplicitss
          val (tparams, implicits) = gextractContextBounds(tparams0, implicits0)
          val paramss = if (implicits.nonEmpty) explicitss :+ implicits else explicitss
          val ctor = m.Ctor.Primary(mmods(gctor), m.Ctor.Name(in.name.toString).withDenot(in.symbol.primaryConstructor), paramss.cvt_!)
          m.Defn.Class(mmods(in), in.symbol.asClass.rawcvt(in), tparams.cvt, ctor, templ.cvt)
        }
      case in @ g.ModuleDef(_, _, templ) =>
        require(in.symbol.isModule && !in.symbol.isModuleClass)
        if (in.symbol.isPackageObject) m.Pkg.Object(mmods(in), in.symbol.asModule.rawcvt(in), mfakector(in), templ.cvt)
        else m.Defn.Object(mmods(in), in.symbol.asModule.rawcvt(in), mfakector(in), templ.cvt)
      case in @ g.ValDef(_, _, tpt, rhs) if pt <:< typeOf[m.Term.Param] =>
        // TODO: how do we really distinguish `case class C(val x: Int)` and `case class C(x: Int)`?
        require(in != g.noSelfType && in.symbol.isTerm)
        val mname = in.symbol.asTerm.anoncvt(in).require[m.Term.Param.Name]
        val mtpe = if (tpt.nonEmpty) Some[m.Type.Arg](tpt.cvt_!) else None
        val mdefault = if (rhs.nonEmpty) Some[m.Term](rhs.cvt_!) else None
        require(in.symbol.isAnonymous ==> mdefault.isEmpty)
        m.Term.Param(mmods(in), mname, mtpe, mdefault)
      case in @ g.ValDef(_, _, tpt, rhs) if pt <:< typeOf[m.Stat] =>
        // TODO: collapse desugared representations of pattern-based vals and vars
        require(in.symbol.isTerm)
        require(in.symbol.isDeferred ==> rhs.isEmpty)
        require(in.symbol.hasFlag(DEFAULTINIT) ==> rhs.isEmpty)
        val mname = m.Pat.Var.Term(in.symbol.asTerm.rawcvt(in))
        (in.symbol.isDeferred, in.symbol.isMutable || in.mods.isMutable) match {
          case (true, false) => m.Decl.Val(mmods(in), List(mname), tpt.cvt_!)
          case (true, true) => m.Decl.Var(mmods(in), List(mname), tpt.cvt_!)
          case (false, false) => m.Defn.Val(mmods(in), List(mname), if (tpt.nonEmpty) Some[m.Type](tpt.cvt_!) else None, rhs.cvt_!)
          case (false, true) => m.Defn.Var(mmods(in), List(mname), if (tpt.nonEmpty) Some[m.Type](tpt.cvt_!) else None, if (rhs.nonEmpty) Some[m.Term](rhs.cvt_!) else None)
        }
      case in @ g.DefDef(_, _, _, _, _, _) =>
        require(in.symbol.isMethod)
        val q"$_ def $_[..$tparams0](...$explicitss)(implicit ..$implicits0): $tpt = $body" = in
        val (tparams, implicits) = gextractContextBounds(tparams0, implicits0)
        val paramss = if (implicits.nonEmpty) explicitss :+ implicits else explicitss
        require(in.symbol.isDeferred ==> body.isEmpty)
        if (in.symbol.isConstructor) {
          m.Ctor.Secondary(mmods(in), m.Ctor.Name(in.name.toString).withDenot(in.symbol), paramss.cvt_!, body.cvt_!)
        } else if (in.symbol.isMacro) {
          require(tpt.nonEmpty) // TODO: support pre-2.12 macros with inferred return types
          val mbody = if (body != g.EmptyTree) (body.cvt_! : m.Term) else m.Term.Name("???").withDenot(g.definitions.Predef_???)
          m.Defn.Macro(mmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, paramss.cvt_!, tpt.cvt_!, mbody)
        } else if (in.symbol.isDeferred) {
          m.Decl.Def(mmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, paramss.cvt_!, tpt.cvt_!)
        } else {
          m.Defn.Def(mmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, paramss.cvt_!, if (tpt.nonEmpty) Some[m.Type](tpt.cvt_!) else None, body.cvt_!)
        }
      case in @ g.TypeDef(_, _, tparams0, tpt) if pt <:< typeOf[m.Type.Param] =>
        require(in.symbol.isType)
        val mname = in.symbol.asType.anoncvt(in).require[m.Type.Param.Name]
        val tparams = tparams0.map(_.appendMetadata("originalContextBounds" -> Nil).appendMetadata("originalViewBounds" -> Nil))
        val mviewbounds = in.metadata("originalViewBounds").require[List[g.Tree]].map(_.cvt_! : m.Type)
        val mcontextbounds = in.metadata("originalContextBounds").require[List[g.Tree]].map(_.cvt_! : m.Type)
        m.Type.Param(mmods(in), mname, tparams.cvt, mtypebounds(tpt), mviewbounds, mcontextbounds)
      case in @ g.TypeDef(_, _, tparams0, tpt) if pt <:< typeOf[m.Stat] =>
        require(in.symbol.isType)
        val tparams = tparams0.map(_.appendMetadata("originalContextBounds" -> Nil).appendMetadata("originalViewBounds" -> Nil))
        if (in.symbol.isDeferred) m.Decl.Type(mmods(in), in.symbol.asType.rawcvt(in), tparams.cvt, mtypebounds(tpt))
        else m.Defn.Type(mmods(in), in.symbol.asType.rawcvt(in), tparams.cvt, tpt.cvt_!)
      case in @ g.LabelDef(name, params, rhs) =>
        require(params.isEmpty)
        rhs match {
          case q"if ($cond) { $body; $cont } else ()" if name.startsWith(g.nme.WHILE_PREFIX) => m.Term.While(cond.cvt_!, body.cvt_!)
          case q"$body; if ($cond) $cont else ()" if name.startsWith(g.nme.DO_WHILE_PREFIX) => m.Term.Do(body.cvt_!, cond.cvt_!)
          case _ => unreachable(debug(rhs, g.showRaw(rhs)))
        }
      case in @ g.Import(expr, selectors) =>
        // TODO: collapse desugared chains of imports
        // TODO: distinguish `import foo.x` from `import foo.{x => x}`
        // TODO: populate denotations
        m.Import(List(m.Import.Clause(expr.cvt_!, selectors.map({
          case g.ImportSelector(g.nme.WILDCARD, _, null, _)           => m.Import.Selector.Wildcard()
          case g.ImportSelector(name1, _, name2, _) if name1 == name2 => m.Import.Selector.Name(m.Name.Indeterminate(name1.toString))
          case g.ImportSelector(name1, _, name2, _) if name1 != name2 => m.Import.Selector.Rename(m.Name.Indeterminate(name1.toString), m.Name.Indeterminate(name2.toString))
          case g.ImportSelector(name, _, g.nme.WILDCARD, _)           => m.Import.Selector.Unimport(m.Name.Indeterminate(name.toString))
        }))))
      case in @ g.Template(_, _, _) =>
        val SyntacticTemplate(gsupersym, gparents, gself, gearlydefns, gstats) = in
        val gsupersyms = if (gparents.nonEmpty) gsupersym +: List.fill(gparents.length - 1)(g.NoSymbol) else Nil
        val mparents = gsupersyms.zip(gparents).map({ case (gsupersym, gparent) =>
          val gapplied = g.treeInfo.dissectApplied(gparent)
          val gctorsym = gsupersym.orElse(gparent.tpe.typeSymbolDirect)
          mctorcall(gparent, gapplied.callee, gctorsym, gapplied.argss)
        })
        val mself = {
          // NOTE: if we're converting a template of a CompoundTypeTree
          // then it won't have any symbol set, so our m.Name.Anonymous is going to remain denotation-less
          val mdumbselfname = if (gself.name == g.nme.WILDCARD) m.Name.Anonymous() else m.Term.Name(gself.alias)
          val gselfsym = in.symbol.owner.thisSym
          val mselfname = if (in.symbol.owner != gselfsym) mdumbselfname.withDenot(gselfsym) else mdumbselfname
          val mselftpe = if (gself.tpt.nonEmpty) Some[m.Type](gself.tpt.cvt_!) else None
          m.Term.Param(Nil, mselfname, mselftpe, None).withTpe(gself.tpt.tpe)
        }
        val hasStats = gstats.nonEmpty || in.symbol.owner.name == g.tpnme.ANON_CLASS_NAME
        m.Template(gearlydefns.cvt_!, mparents, mself, if (hasStats) Some(mstats(in, gstats)) else None)
      case in: g.Block =>
        // NOTE: if a block with non-trivial stats has been collapsed to a single stat
        // then this means that it's a desugaring of a language construct (e.g. `new C{}` or `1 :: Nil`)
        // and it shouldn't be a m.Term.Block, but rather a resulting stat instead
        val gstats = in.stats :+ in.expr
        (gstats, Helpers.mstats(in, gstats)) match {
          case ((nel @ _ :+ _) :+ gexpr, Nil :+ mstat) => mstat.require[m.Term]
          case (_, mstats) => m.Term.Block(mstats)
        }
      case in @ g.CaseDef(pat, guard, body @ q"..$stats") =>
        m.Case(pat.cvt_!, if (guard.nonEmpty) Some[m.Term](guard.cvt_!) else None, m.Term.Block(mstats(in, stats)).withTpe(body.tpe))
      case g.Alternative(fst :: snd :: Nil) =>
        m.Pat.Alternative(fst.cvt_!, snd.cvt_!)
      case in @ g.Alternative(hd :: rest) =>
        m.Pat.Alternative(hd.cvt_!, g.Alternative(rest).setType(in.tpe).require[g.Alternative].cvt)
      case g.Ident(g.nme.WILDCARD) =>
        m.Pat.Wildcard()
      case g.Bind(g.tpnme.WILDCARD, g.EmptyTree) =>
        m.Pat.Type.Wildcard()
      case g.Star(g.Ident(g.nme.WILDCARD)) =>
        m.Pat.Arg.SeqWildcard()
      case in @ g.Bind(_, g.Ident(g.nme.WILDCARD)) =>
        // TODO: discern `case x => ...` and `case x @ _ => ...`
        require(in.symbol.isTerm)
        m.Pat.Var.Term(in.symbol.asTerm.rawcvt(in))
      case in @ g.Bind(_, g.EmptyTree) =>
        require(in.symbol.isType)
        m.Pat.Var.Type(in.symbol.asType.rawcvt(in))
      case in @ g.Bind(_, g.Typed(g.Ident(g.nme.WILDCARD), tpt)) =>
        require(in.symbol.isTerm)
        m.Pat.Typed(m.Pat.Var.Term(in.symbol.asTerm.rawcvt(in)), tpt.cvt_!)
      case in @ g.Bind(name, tree) =>
        require(in.symbol.isTerm)
        require(name == in.symbol.name)
        m.Pat.Bind(m.Pat.Var.Term(in.symbol.asTerm.rawcvt(in)), tree.cvt_!)
      case g.Function(params, body) =>
        // NOTE: need to be careful to discern `_ + 2` and `_ => 2`
        // because in both cases parameter names are `x$...`
        // also need to be careful not to mistakenly recognize `(_, y) => y` as shorthand syntax
        val isShorthand = params.nonEmpty && params.forall(param => param.symbol.isAnonymous && body.exists(_.symbol == param.symbol))
        if (isShorthand) (body.cvt_! : m.Term) else m.Term.Function(params.cvt_!, body.cvt_!)
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
            m.Term.ApplyInfix(lhs.cvt_!, m.Term.Name(op.decodedName.toString), Nil, args.cvt_!)
          case _ =>
            m.Term.Assign(lhs.cvt_!, rhs.cvt_!)
        }
      case g.AssignOrNamedArg(lhs, rhs) =>
        // NOTE: handled in marg/margs/margss
        unreachable
      case g.If(cond, thenp, elsep) =>
        m.Term.If(cond.cvt_!, thenp.cvt_!, elsep.cvt_!)
      case g.Match(selector, cases) =>
        if (selector == g.EmptyTree) m.Term.PartialFunction(cases.cvt)
        else m.Term.Match(selector.cvt_!, cases.cvt)
      case g.Return(expr) =>
        m.Term.Return(expr.cvt_!)
      case g.Try(body, catches, finalizer) =>
        object CatchExpr {
          def unapply(tree: g.Tree): Option[g.Tree] = tree match {
            case g.CaseDef(pq"$scrutdef: Throwable", g.EmptyTree, g.Block(List(g.ValDef(_, tempdef, _, handler)), _))
            if scrutdef.startsWith(g.nme.FRESH_TERM_NAME_PREFIX) && tempdef.startsWith("catchExpr") => Some(handler)
            case _ => None
          }
        }
        val finallyp = if (finalizer.nonEmpty) Some[m.Term](finalizer.cvt_!) else None
        catches match {
          case CatchExpr(handler) :: Nil => m.Term.TryWithTerm(body.cvt_!, handler.cvt_!, finallyp)
          case Nil => m.Term.TryWithCases(body.cvt_!, Nil, finallyp)
          case _ => m.Term.TryWithCases(body.cvt_!, catches.cvt, finallyp)
        }
      case g.Throw(expr) =>
        m.Term.Throw(expr.cvt_!)
      case g.New(_) =>
        unreachable
      case g.Typed(expr, g.Function(Nil, g.EmptyTree)) if pt <:< typeOf[m.Term] =>
        m.Term.Eta(expr.cvt_!)
      case g.Typed(expr, g.EmptyTree) =>
        ???
      case g.Typed(expr, tpt) if pt <:< typeOf[m.Term] =>
        m.Term.Ascribe(expr.cvt_!, tpt.cvt_!)
      case g.Typed(expr, tpt) if pt <:< typeOf[m.Pat] =>
        m.Pat.Typed(expr.cvt_!, tpt.cvt_!)
      case in @ g.TypeApply(fn, targs) =>
        m.Term.ApplyType(fn.cvt_!, targs.cvt_!)
      case in @ g.Apply(fn, args) if pt <:< typeOf[m.Term] =>
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
            val supercall = mctorcall(in, tpt, ctorref.symbol, argss)
            val self = m.Term.Param(Nil, m.Name.Anonymous().withDenot(tpt.tpe.typeSymbol), None, None).withTpe(in.tpe)
            val templ = m.Template(Nil, List(supercall), self, None)
            m.Term.New(templ)
          case DesugaredSetter(lhs, rhs) =>
            m.Term.Assign(lhs.cvt_!, rhs.cvt_!)
          case DesugaredUpdate(lhs, argss, rhs) =>
            m.Term.Update(lhs.cvt_!, argss.cvt_!, rhs.cvt_!)
          case DesugaredOpAssign(g.Select(lhs, op), args) =>
            m.Term.ApplyInfix(lhs.cvt_!, m.Term.Name(op.decodedName.toString), Nil, args.cvt_!)
          case DesugaredSymbolLiteral(value) =>
            m.Lit.Symbol(value) : m.Term
          case DesugaredTuple(arg) =>
            m.Term.Tuple(args.cvt_!)
          case DesugaredInterpolation(prefix, parts, args) =>
            m.Term.Interpolate(prefix.symbol.asTerm.precvt(prefix.qualifier.tpe, prefix), parts.cvt_!, args.cvt_!)
          case DesugaredFor(enums, body, isYield) =>
            val menums = enums.map({
              case Generator(pat, rhs) => m.Enumerator.Generator(pat.cvt_!, rhs.cvt_!)
              case Val(pat, rhs) => m.Enumerator.Val(pat.cvt_!, rhs.cvt_!)
              case Guard(cond) => m.Enumerator.Guard(cond.cvt_!)
            })
            if (isYield) m.Term.ForYield(menums, body.cvt_!)
            else m.Term.For(menums, body.cvt_!)
          case q"$lhs.$op($arg)" if op.looksLikeInfix && !lhs.isInstanceOf[g.Super] =>
            m.Term.ApplyInfix(lhs.cvt_!, fn.symbol.asTerm.precvt(lhs.tpe, fn), Nil, List(marg(arg)))
          case DesugaredApply(target, args) =>
            m.Term.Apply(target.cvt_!, margs(args))
          case _ =>
            m.Term.Apply(fn.cvt_!, margs(args))
        }
      case in @ g.Apply(fn, args) if pt <:< typeOf[m.Pat] =>
        // TODO: infer Extract vs ExtractInfix
        // TODO: also figure out Interpolate
        // TODO: also figure out whether the user wrote m.Pat.Tuple themselves, or it was inserted by the compiler
        if (g.definitions.isTupleSymbol(in.symbol.companion)) m.Pat.Tuple(args.cvt_!)
        else {
          val (ref, targs) = in match { case q"$ref.$unapply[..$targs](..$_)" => (ref, targs); case q"$ref[..$targs](..$_)" => (ref, targs) }
          (ref, args) match {
            case (ref @ g.Ident(name), List(lhs, rhs)) if name.looksLikeInfix && targs.isEmpty => m.Pat.ExtractInfix(lhs.cvt_!, ref.cvt_!, List(rhs.cvt_!))
            case _ => m.Pat.Extract(ref.cvt_!, targs.cvt_!, args.cvt_!)
          }
        }
      case in @ g.ApplyDynamic(_, _) =>
        unreachable
      case in @ g.Super(qual @ g.This(_), mix) =>
        require(in.symbol.isClass)
        val mthis = (qual.cvt : m.Term.This).qual
        val msuperdumb = if (mix != g.tpnme.EMPTY) m.Name.Indeterminate(in.mix.toString) else m.Name.Anonymous()
        m.Term.Super(mthis, msuperdumb.withDenot(qual.tpe, in.tpe.typeSymbol))
      case in @ g.This(qual) =>
        require(!in.symbol.isPackageClass)
        val mqualdumb = if (qual != g.tpnme.EMPTY) m.Name.Indeterminate(g.Ident(in.symbol).alias) else m.Name.Anonymous()
        m.Term.This(mqualdumb.withDenot(in.tpe, in.symbol))
      case in: g.PostfixSelect =>
        unreachable
      case in @ g.Select(qual, name) =>
        // TODO: discern unary applications via !x and via explicit x.unary_!
        // TODO: also think how to detect unary applications that have implicit arguments
        require(name != g.nme.CONSTRUCTOR)
        if (name.isTermName) {
          if (name.toString.startsWith("unary_")) {
            val in1 = g.treeCopy.Select(in, qual, g.TermName(name.toString.stripPrefix("unary_")))
            m.Term.ApplyUnary(in.symbol.asTerm.precvt(qual.tpe, in1), qual.cvt_!)
          } else {
            if (qual.isInstanceOf[g.This] && in.symbol == g.definitions.NilModule) {
              // TODO: this is a hack to handle `gen.mkNil` emitted during parsing
              // such weird occasion can happen when we parse attributes in XML syntax
              in.symbol.asTerm.precvt(qual.tpe, g.Ident(in.symbol))
            } else if (qual.isInstanceOf[g.This] && qual.symbol.isPackageClass) {
              // TODO: not sure whether I should do this check here
              // or create a separate version of toMtree, e.g. toApproximateMtree
              in.symbol.asTerm.precvt(qual.tpe, in)
            } else {
              m.Term.Select(qual.cvt_!, in.symbol.asTerm.precvt(qual.tpe, in))
            }
          }
        } else {
          m.Type.Select(qual.cvt_!, in.symbol.asType.precvt(qual.tpe, in))
        }
      case in @ g.Ident(_) =>
        // NOTE: Ident(<term name>) with a type symbol attached to it
        // is the encoding that the ensugarer uses to denote a self reference
        // also see the ensugarer for more information
        // NOTE: primary ctor calls in secondary ctors are represented as follows:
        // Apply(Select(This(TypeName("C")), nme.CONSTRUCTOR), List(...))
        // therefore we need to detect this special select and transform it accordingly
        if (in.name == g.nme.CONSTRUCTOR) m.Ctor.Name("this").withDenot(in.symbol)
        else if (in.isTerm && in.symbol.isType) m.Term.Name(in.alias)
        else if (in.isTerm && in.symbol.isAnonymous) m.Term.Placeholder()
        else if (in.isType && in.symbol.isAnonymous) m.Type.Placeholder(mtypebounds(in.metadata("originalBounds").require[g.Tree]))
        else in.symbol.rawcvt(in)
      case g.ReferenceToBoxed(_) =>
        ???
      case in @ g.Literal(const) =>
        const.rawcvt
      case g.Parens(_) =>
        unreachable
      case g.DocDef(_, _) =>
        unreachable
      case g.Annotated(annot, arg) if pt <:< typeOf[m.Term] =>
        val (marg, mannots) = (arg.cvt_! : m.Term) match { case m.Term.Annotate(arg, annots) => (arg, annots); case arg => (arg, Nil) }
        m.Term.Annotate(marg, mannots :+ mannot(annot))
      case g.Annotated(annot, arg) if pt <:< typeOf[m.Type] =>
        val (marg, mannots) = (arg.cvt_! : m.Type) match { case m.Type.Annotate(arg, annots) => (arg, annots); case arg => (arg, Nil) }
        m.Type.Annotate(marg, mannots :+ mannot(annot))
      case g.Annotated(annot, arg) if pt <:< typeOf[m.Pat.Type] =>
        val (marg, mannots) = (arg.cvt_! : m.Pat.Type) match { case m.Pat.Type.Annotate(arg, annots) => (arg, annots); case arg => (arg, Nil) }
        m.Pat.Type.Annotate(marg, mannots :+ mannot(annot))
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
        m.Type.Singleton(ref.cvt_!)
      case in @ g.CompoundTypeTree(templ) if pt <:< typeOf[m.Type] =>
        val SyntacticTemplate(gsupersym, gparents, gself, gearlydefns, gstats) = templ
        require(gsupersym == g.NoSymbol && gself == g.noSelfType && gearlydefns.isEmpty)
        m.Type.Compound(gparents.cvt_!, gstats.cvt_!)
      case in @ g.CompoundTypeTree(templ) if pt <:< typeOf[m.Pat.Type] =>
        val SyntacticTemplate(gsupersym, gparents, gself, gearlydefns, gstats) = templ
        require(gsupersym == g.NoSymbol && gself == g.noSelfType && gearlydefns.isEmpty)
        m.Pat.Type.Compound(gparents.cvt_!, gstats.cvt_!)
      case in @ g.AppliedTypeTree(tpt, args) if pt <:< typeOf[m.Type.Arg] =>
        // TODO: infer whether that was really Apply, Function or Tuple
        // TODO: precisely infer whether that was infix application or normal application
        if (g.definitions.RepeatedParamClass == tpt.tpe.typeSymbolDirect) m.Type.Arg.Repeated(args.head.cvt_!)
        else if (g.definitions.ByNameParamClass == tpt.tpe.typeSymbolDirect) m.Type.Arg.ByName(args.head.cvt_!)
        else if (g.definitions.FunctionClass.seq.contains(tpt.tpe.typeSymbolDirect)) m.Type.Function(args.init.cvt_!, args.last.cvt_!)
        else if (g.definitions.TupleClass.seq.contains(tpt.tpe.typeSymbolDirect) && args.length > 1) m.Type.Tuple(args.cvt_!)
        else in match {
          case g.AppliedTypeTree(tpt @ g.Ident(name), List(lhs, rhs)) if name.looksLikeInfix => m.Type.ApplyInfix(lhs.cvt_!, tpt.cvt_!, rhs.cvt_!)
          case _ => m.Type.Apply(tpt.cvt_!, args.cvt_!)
        }
      case in @ g.AppliedTypeTree(tpt, args) if pt <:< typeOf[m.Pat.Type] =>
        // TODO: infer whether that was really Apply, Function or Tuple
        // TODO: precisely infer whether that was infix application or normal application
        if (g.definitions.FunctionClass.seq.contains(tpt.tpe.typeSymbolDirect)) m.Pat.Type.Function(args.init.cvt_!, args.last.cvt_!)
        else if (g.definitions.TupleClass.seq.contains(tpt.tpe.typeSymbolDirect) && args.length > 1) m.Pat.Type.Tuple(args.cvt_!)
        else in match {
          case g.AppliedTypeTree(tpt @ g.Ident(name), List(lhs, rhs)) if name.looksLikeInfix => m.Pat.Type.ApplyInfix(lhs.cvt_!, tpt.cvt_!, rhs.cvt_!)
          case _ => m.Pat.Type.Apply(tpt.cvt_!, args.cvt_!)
        }
      case in @ g.ExistentialTypeTree(tpt, whereClauses) if pt <:< typeOf[m.Type] =>
        val isShorthand = whereClauses.exists(_.symbol.isAnonymous)
        if (isShorthand) (in.shorthand.cvt_! : m.Type) else m.Type.Existential(tpt.cvt_!, whereClauses.cvt_!)
      case in @ g.ExistentialTypeTree(tpt, whereClauses) if pt <:< typeOf[m.Pat.Type] =>
        val isShorthand = whereClauses.exists(_.symbol.isAnonymous)
        if (isShorthand) (in.shorthand.cvt_! : m.Pat.Type) else m.Pat.Type.Existential(tpt.cvt_!, whereClauses.cvt_!)
      case in @ g.SelectFromTypeTree(qual, name) if pt <:< typeOf[m.Type] =>
        m.Type.Project(qual.cvt_!, in.symbol.asType.precvt(qual.tpe, in))
      case in @ g.SelectFromTypeTree(qual, name) if pt <:< typeOf[m.Pat.Type] =>
        m.Pat.Type.Project(qual.cvt_!, in.symbol.asType.precvt(qual.tpe, in))
      case in @ g.TypeBoundsTree(_, _) =>
        unreachable
      // NOTE: this derivation is ambiguous because of at least g.ValDef, g.TypeDef and g.Typed
      // case _: g.Tree =>
      //   derive
    }
  }
}
package scala.meta
package internal.hosts.scalac

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.reflect.{classTag, ClassTag}
import scala.reflect.runtime.universe.{Type => Pt, typeOf}
import scala.{meta => papi}
import scala.meta.internal.{ast => p}
import scala.meta.syntactic.parsers.SyntacticInfo._
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.hosts.scalac.{SemanticContext => ScalahostSemanticContext}
import scala.reflect.macros.contexts.{Context => ScalareflectMacroContext}
import scala.meta.macros.{Context => ScalametaMacroContext}
import scala.meta.internal.hosts.scalac.{MacroContext => ScalahostMacroContext}
import scala.meta.ui.{Exception => SemanticException, _}
import scala.reflect.runtime.{universe => ru}
import scala.reflect.internal.util.NoSourceFile
import scala.tools.reflect.{ToolBox, mkSilentFrontEnd}
import scala.compat.Platform.EOL
import org.scalameta.adt._
import org.scalameta.collections._
import org.scalameta.convert._
import org.scalameta.convert.auto._
import org.scalameta.invariants._
import org.scalameta.meta.{Toolkit => MetaToolkit, _}
import org.scalameta.reflection._
import org.scalameta.unreachable
import scala.meta.internal.{hygiene => h}
import java.util.UUID.randomUUID

class SemanticContext[G <: ScalaGlobal](val g: G) extends ScalametaSemanticContext with GlobalToolkit with MetaToolkit {
  lazy val global: g.type = g
  import g.Quasiquote
  import scala.reflect.internal.Flags._
  implicit val c: ScalametaSemanticContext = this
  def dialect: scala.meta.dialects.Scala211.type = scala.meta.dialects.Scala211

  private[meta] def tpe(term: papi.Term): papi.Type = {
    val tree = term.require[p.Term]
    val gtpeFromOriginal = tree.originalTree.map(_.tpe)
    val gtpeFromDenotation = tree.originalPre.flatMap(gpre => tree.originalSym.map(gsym => gsym.infoIn(gpre)))
    val gtpe = gtpeFromOriginal.orElse(gtpeFromDenotation) match {
      case Some(gtpe) => gtpe
      case _ => throw new SemanticException("implementation restriction: internal cache has no type associated with ${term.show[Summary]}")
    }
    toApproximateScalameta(gtpe).asInstanceOf[papi.Type]
  }
  private[meta] def defns(ref: papi.Ref): Seq[papi.Member] = {
    def tryScratchpad(pref: p.Ref): Option[Seq[papi.Member]] = {
      for { gpre <- pref.originalPre; gsyms <- pref.originalSym.map(_.alternatives) }
      yield gsyms.map(gsym => toApproximateScalameta(gpre, gsym))
    }
    def tryNative(pref: p.Ref): Seq[papi.Member] = {
      def resolveName(pname: p.Name): Seq[papi.Member] = {
        val gpre = pname.denot.prefix match { case h.Prefix.Zero => g.NoPrefix; case h.Prefix.Type(ptpe) => toScalareflect(ptpe.asInstanceOf[p.Type]) }
        val gsym = symbolTable.get(pname.denot.symbol).getOrElse(throw new SemanticException("implementation restriction: internal cache has no definition associated with ${term.show[Summary]}"))
        List(toApproximateScalameta(gpre, gsym))
      }
      pref match {
        case pname: p.Name => resolveName(pname)
        case p.Term.Select(_, pname) => defns(pname)
        case p.Type.Select(_, pname) => defns(pname)
        case p.Type.Project(_, pname) => defns(pname)
        case p.Type.Singleton(pref) => defns(pref)
        case p.Ctor.Ref.Select(_, pname) => defns(pname)
        case p.Ctor.Ref.Project(_, pname) => defns(pname)
        case p.Ctor.Ref.Function(pname) => defns(pname)
        case _: p.Import.Selector => ???
      }
    }
    tryScratchpad(ref.require[p.Ref]).getOrElse(tryNative(ref.require[p.Ref]))
  }
  private[meta] def members(tpe: papi.Type): Seq[papi.Member] = {
    val ppre = tpe.require[p.Type]
    val gpre = toScalareflect(ppre)
    gpre.members.sorted.toList.map(gsym => toApproximateScalameta(gpre, gsym))
  }

  private[meta] def isSubType(tpe1: papi.Type, tpe2: papi.Type): Boolean = toScalareflect(tpe1.require[p.Type]) <:< toScalareflect(tpe2.require[p.Type])
  private[meta] def lub(tpes: Seq[papi.Type]): papi.Type = toApproximateScalameta(g.lub(tpes.map(tpe => toScalareflect(tpe.require[p.Type])).toList)).asInstanceOf[papi.Type]
  private[meta] def glb(tpes: Seq[papi.Type]): papi.Type = toApproximateScalameta(g.glb(tpes.map(tpe => toScalareflect(tpe.require[p.Type])).toList)).asInstanceOf[papi.Type]
  private[meta] def widen(tpe: papi.Type): papi.Type = toApproximateScalameta(toScalareflect(tpe.require[p.Type]).widen).asInstanceOf[papi.Type]
  private[meta] def dealias(tpe: papi.Type): papi.Type = toApproximateScalameta(toScalareflect(tpe.require[p.Type]).dealias).asInstanceOf[papi.Type]

  private[meta] def parents(member: papi.Member): Seq[papi.Member] = ???
  private[meta] def children(member: papi.Member): Seq[papi.Member] = ???

  // NOTE: this is an exhaustive list of payloads that can be attached to scratchpads of converted trees
  private sealed trait ScratchpadDatum
  private object ScratchpadDatum {
    case class Denotation(gpre: g.Type, gsym: g.Symbol) extends ScratchpadDatum
    case class Original(goriginal: Any) extends ScratchpadDatum
  }

  private implicit class RichScratchpadTree[T <: p.Tree](ptree: T) {
    private def denot(gpre: g.Type, gsym: g.Symbol): h.Denotation = {
      require(gpre != g.NoType)
      val hpre = if (gpre != g.NoPrefix) h.Prefix.Type(toApproximateScalameta(gpre).asInstanceOf[p.Type]) else h.Prefix.Zero
      val hsym = symbolTable(gsym)
      h.Denotation.Precomputed(hpre, hsym)
    }
    def withDenot(gsym: g.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      def defaultpre(gsym: g.Symbol): g.Type = {
        if (gsym.hasFlag(EXISTENTIAL | PARAM)) g.NoPrefix
        else if (gsym.isConstructor) defaultpre(gsym.owner)
        else gsym.owner.thisType
      }
      ptree.withDenot(defaultpre(gsym), gsym)
    }
    def withDenot(gpre: g.Type, gsym: g.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      val ptree0 = ptree // NOTE: this is here only to provide an unqualified Ident for the `require` macro
      require(ptree0.isInstanceOf[p.Name] && gpre != null && ((gsym == g.NoSymbol) ==> ptree.isInstanceOf[p.Term.Super]))
      val scratchpad = ptree.scratchpad :+ ScratchpadDatum.Denotation(gpre, gsym)
      val ptree1 = ptree match {
        case ptree: p.Name.Anonymous => ptree.copy(denot = denot(gpre, gsym), sigma = h.Sigma.Naive)
        case ptree: p.Term.Name => ptree.copy(denot = denot(gpre, gsym), sigma = h.Sigma.Naive)
        case ptree: p.Type.Name => ptree.copy(denot = denot(gpre, gsym), sigma = h.Sigma.Naive)
        // TODO: some ctor refs don't have corresponding constructor symbols in Scala (namely, ones for traits and objects)
        // in these cases, our gsym is going to be a symbol of the trait or object in question
        // we need to account for that in `convert` and create a constructor symbol of our own
        case ptree: p.Ctor.Name => ptree.copy(denot = denot(gpre, gsym), sigma = h.Sigma.Naive)
        case ptree: p.Term.This => ptree.copy(denot = denot(gpre, gsym), sigma = h.Sigma.Naive)
        case ptree: p.Term.Super => ptree.copy(denot = denot(gpre, gsym), sigma = h.Sigma.Naive)
        case ptree: p.Mod.PrivateThis => ptree.copy(denot = denot(gpre, gsym), sigma = h.Sigma.Naive)
        case ptree: p.Mod.PrivateWithin => ptree.copy(denot = denot(gpre, gsym), sigma = h.Sigma.Naive)
        case ptree: p.Mod.ProtectedThis => ptree.copy(denot = denot(gpre, gsym), sigma = h.Sigma.Naive)
        case ptree: p.Mod.ProtectedWithin => ptree.copy(denot = denot(gpre, gsym), sigma = h.Sigma.Naive)
        case _ => unreachable
      }
      ptree1.withScratchpad(scratchpad).asInstanceOf[T]
    }
    def withOriginal(gtree: g.Tree): T = ptree.appendScratchpad(ScratchpadDatum.Original(gtree)).asInstanceOf[T]
    def withOriginal(gtpe: g.Type): T = ptree.appendScratchpad(ScratchpadDatum.Original(gtpe)).asInstanceOf[T]
    def withOriginal(gsym: g.Symbol): T = ptree.appendScratchpad(ScratchpadDatum.Original(gsym)).asInstanceOf[T]
    def originalTree: Option[g.Tree] = ptree.scratchpad.collect { case ScratchpadDatum.Original(goriginal: g.Tree) => goriginal }.headOption
    def originalTpe: Option[g.Type] = ptree.scratchpad.collect { case ScratchpadDatum.Original(goriginal: g.Type) => goriginal }.headOption
    def originalPre: Option[g.Type] = ptree.scratchpad.collect { case ScratchpadDatum.Denotation(gpre: g.Type, _) => gpre }.headOption
    def originalSym: Option[g.Symbol] = {
      val fromOriginal = ptree.scratchpad.collect { case ScratchpadDatum.Original(goriginal: g.Symbol) => goriginal }.headOption
      val fromDenot = ptree.scratchpad.collect { case ScratchpadDatum.Denotation(_, gsym: g.Symbol) => gsym }.headOption
      fromOriginal.orElse(fromDenot)
    }
  }

  private implicit class RichToScalametaTree(gtree: g.Tree) {
    def alias: String = gtree match {
      case _ if gtree.symbol == g.rootMirror.EmptyPackage || gtree.symbol == g.rootMirror.EmptyPackageClass => "_empty_"
      case gtree: g.ModuleDef if gtree.symbol.isPackageObject => gtree.symbol.owner.name.decodedName.toString
      case gtree: g.NameTree => gtree.name.decodedName.toString
      case g.This(name) => name.decodedName.toString
      case g.Super(_, name) => name.decodedName.toString
    }
  }
  private implicit class RichToScalametaName(gname: g.Name) {
    def looksLikeInfix = !gname.decoded.forall(c => Character.isLetter(c) || Character.isDigit(c) || c == '_') || gname == g.nme.eq || gname == g.nme.ne
  }
  private implicit class RichToScalametaSymbol(gsym: g.Symbol) {
    def isAnonymous: Boolean = {
      // NOTE: not all symbols whose names start with x$ are placeholders
      // there are also at least artifact vals created for named arguments
      val isTermPlaceholder = gsym.isTerm && gsym.isParameter && gsym.name.startsWith(g.nme.FRESH_TERM_NAME_PREFIX)
      val isTypePlaceholder = gsym.isType && gsym.isAbstract && gsym.name.startsWith("_$")
      val isAnonymousSelfName = gsym.name.startsWith(g.nme.FRESH_TERM_NAME_PREFIX) || gsym.name == g.nme.this_
      val isAnonymousSelf = gsym.isTerm && isAnonymousSelfName && gsym.owner.isClass // TODO: more precise detection, probably via attachments
      val isAnonymousTypeParameter = gsym.name == g.tpnme.WILDCARD
      isTermPlaceholder || isTypePlaceholder || isAnonymousSelf || isAnonymousTypeParameter
    }
    type pTermOrTypeName = p.Name{type ThisType >: p.Term.Name with p.Type.Name <: p.Name}
    type pTermOrTypeOrAnonymousName = p.Name{type ThisType >: p.Term.Name with p.Type.Name with p.Name.Anonymous <: p.Name}
    private def dumbcvt(in: g.Tree): p.Name = {
      if (gsym.isTerm) p.Term.Name(in.alias)
      else if (gsym.isType) p.Type.Name(in.alias)
      else unreachable
    }
    def precvt(pre: g.Type, in: g.Tree): pTermOrTypeName = dumbcvt(in).withDenot(pre, gsym).asInstanceOf[pTermOrTypeName]
    def rawcvt(in: g.Tree): pTermOrTypeName = dumbcvt(in).withDenot(gsym).asInstanceOf[pTermOrTypeName]
    def anoncvt(in: g.Tree): pTermOrTypeOrAnonymousName = (if (gsym.isAnonymous) p.Name.Anonymous() else dumbcvt(in)).withDenot(gsym).asInstanceOf[pTermOrTypeOrAnonymousName]
  }
  private implicit class RichToScalametaTermSymbol(gsym: g.TermSymbol) {
    type pTermOrAnonymousName = papi.Term.Name with p.Name{type ThisType >: p.Term.Name with p.Name.Anonymous <: p.Name}
    def precvt(pre: g.Type, in: g.Tree): p.Term.Name = (gsym: g.Symbol).precvt(pre, in).asInstanceOf[p.Term.Name]
    def rawcvt(in: g.Tree, allowNoSymbol: Boolean = false): p.Term.Name = (gsym: g.Symbol).rawcvt(in).asInstanceOf[p.Term.Name]
    def anoncvt(in: g.ValDef): pTermOrAnonymousName = (gsym: g.Symbol).anoncvt(in).asInstanceOf[pTermOrAnonymousName]
  }
  private implicit class RichToScalametaTypeSymbol(gsym: g.TypeSymbol) {
    type pTypeOrAnonymousName = papi.Type.Name with p.Name{type ThisType >: p.Type.Name with p.Name.Anonymous <: p.Name}
    def precvt(pre: g.Type, in: g.Tree): p.Type.Name = (gsym: g.Symbol).precvt(pre, in).asInstanceOf[p.Type.Name]
    def rawcvt(in: g.Tree): p.Type.Name = (gsym: g.Symbol).rawcvt(in).asInstanceOf[p.Type.Name]
    def anoncvt(in: g.TypeDef): pTypeOrAnonymousName = (gsym: g.Symbol).anoncvt(in).asInstanceOf[pTypeOrAnonymousName]
  }

  // TODO: remember positions. actually, in scalac they are almost accurate, so it would be a shame to discard them
  val hsymToNativePmemberCache = mutable.Map[h.Symbol, p.Member]()
  @converter def toScalameta(in: Any, pt: Pt): Any = {
    object Helpers extends g.ReificationSupportImpl { self =>
      def pctorcall(in: g.Tree, gtpt: g.Tree, gctor: g.Symbol, gargss: Seq[Seq[g.Tree]]): p.Term = {
        def pctorref(ptpt: p.Type): p.Term = {
          object pTypes {
            def unapply(tpes: Seq[p.Type.Arg]): Option[Seq[p.Type]] = {
              if (tpes.forall(_.isInstanceOf[p.Type])) Some(tpes.map(_.asInstanceOf[p.Type]))
              else None
            }
          }
          val result = ptpt match {
            case p.Type.Name(value) => p.Ctor.Name(value).withDenot(gctor)
            case p.Type.Select(qual, name) => p.Ctor.Ref.Select(qual, p.Ctor.Name(name.value).withDenot(qual.originalTree.requireGet.tpe, gctor))
            case p.Type.Project(qual, name) => p.Ctor.Ref.Project(qual, p.Ctor.Name(name.value).withDenot(qual.originalTree.requireGet.tpe, gctor))
            case p.Type.Function(pTypes(params), ret) => p.Term.ApplyType(p.Ctor.Ref.Function(p.Ctor.Name("=>").withDenot(gctor)), params :+ ret)
            case p.Type.Annotate(tpe, annots) => p.Term.Annotate(pctorref(tpe), annots)
            case p.Type.Apply(tpe, args) => p.Term.ApplyType(pctorref(tpe), args)
            case _ => unreachable
          }
          result.withOriginal(ptpt.originalTree.requireGet)
        }
        val pcore = pctorref(gtpt.cvt_! : p.Type)
        val pargss = gargss.map(_.map(parg))
        pargss.foldLeft(pcore)((pcurr, pargs) => p.Term.Apply(pcurr, pargs)).withOriginal(in)
      }
      def pctorcalltpe(pctorcall: p.Term): p.Type = {
        def loop(pctorref: p.Term): p.Type = {
          val result = pctorref match {
            case p.Ctor.Name(value) => p.Type.Name(value).withDenot(pctorref.originalPre.requireGet, pctorref.originalSym.requireGet)
            case p.Ctor.Ref.Select(qual, name) => p.Type.Select(qual, loop(name).require[p.Type.Name])
            case p.Ctor.Ref.Project(qual, name) => p.Type.Project(qual, loop(name).require[p.Type.Name])
            case p.Ctor.Ref.Function(_) => unreachable
            case p.Term.ApplyType(p.Ctor.Ref.Function(_), targs) => p.Type.Function(targs.init, targs.last)
            case p.Term.ApplyType(callee, targs) => p.Type.Apply(loop(callee), targs)
            case p.Term.Annotate(annottee, annots) => p.Type.Annotate(loop(annottee), annots)
            case _ => unreachable
          }
          result.withOriginal(pctorref.originalTree.requireGet)
        }
        pctorcall match {
          case p.Term.Apply(callee, _) => pctorcalltpe(callee)
          case _ => loop(pctorcall)
        }
      }
      def pctorcallargs(pctorcall: p.Term): Seq[Seq[p.Term.Arg]] = {
        pctorcall match {
          case _: p.Ctor.Ref => Nil
          case p.Term.ApplyType(callee, _) => pctorcallargs(callee)
          case p.Term.Apply(callee, args) => pctorcallargs(callee) :+ args
          case p.Term.Annotate(annottee, _) => annottee.ctorArgss
          case _ => unreachable
        }
      }
      def pfakector(gparent: g.MemberDef): p.Ctor.Primary = {
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
        def unwrap(ptpe: p.Type): p.Type = ptpe.asInstanceOf[p.Type.Apply].args.head
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
              val modbearer = gstats(i - 1).asInstanceOf[g.MemberDef] // TODO: mods for nullary patterns get irrevocably lost (`implicit val List() = List()`)
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
                    val op1 = g.duplicateAndKeepPositions(op).setType(g.treeInfo.dissectApplied(gtree).callee.tpe).asInstanceOf[g.Select]
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
                    loop(gstat).asInstanceOf[g.Apply]
                  }
                  canonicalize(gparent.asInstanceOf[g.Block]).cvt_! : p.Term
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
    in.asInstanceOf[g.Tree].requireAttributed()
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
          val gctor = templ.body.find(_.symbol == in.symbol.primaryConstructor).get.asInstanceOf[g.DefDef]
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
        (in.symbol.isDeferred, in.symbol.isMutable || in.mods.isMutable) match {
          case (true, false) => p.Decl.Val(pmods(in), List(p.Pat.Var(in.symbol.asTerm.rawcvt(in))), tpt.cvt_!)
          case (true, true) => p.Decl.Var(pmods(in), List(p.Pat.Var(in.symbol.asTerm.rawcvt(in))), tpt.cvt_!)
          case (false, false) => p.Defn.Val(pmods(in), List(p.Pat.Var(in.symbol.asTerm.rawcvt(in))), if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None, rhs.cvt_!)
          case (false, true) => p.Defn.Var(pmods(in), List(p.Pat.Var(in.symbol.asTerm.rawcvt(in))), if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None, if (rhs.nonEmpty) Some[p.Term](rhs.cvt_!) else None)
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
        val pcontextbounds = in.metadata("originalContextBounds").asInstanceOf[List[g.Tree]].map(_.cvt_! : p.Type)
        val pviewbounds = in.metadata("originalViewBounds").asInstanceOf[List[g.Tree]].map(_.cvt_! : p.Type)
        p.Type.Param(pmods(in), pname, tparams.cvt, pcontextbounds, pviewbounds, ptypebounds(tpt))
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
          val pselfname = if (in.symbol.owner != g.NoSymbol) pdumbselfname.withDenot(in.symbol.owner) else pdumbselfname
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
          case ((nel @ _ :+ _) :+ gexpr, Nil :+ pstat) => pstat.asInstanceOf[p.Term]
          case (_, pstats) => p.Term.Block(pstats)
        }
      case in @ g.CaseDef(pat, guard, body @ q"..$stats") =>
        p.Case(pat.cvt_!, if (guard.nonEmpty) Some[p.Term](guard.cvt_!) else None, p.Term.Block(pstats(in, stats)).withOriginal(body))
      case g.Alternative(fst :: snd :: Nil) =>
        p.Pat.Alternative(fst.cvt_!, snd.cvt_!)
      case in @ g.Alternative(hd :: rest) =>
        p.Pat.Alternative(hd.cvt_!, g.Alternative(rest).setType(in.tpe).asInstanceOf[g.Alternative].cvt)
      case g.Ident(g.nme.WILDCARD) =>
        p.Pat.Wildcard()
      case g.Bind(g.tpnme.WILDCARD, g.EmptyTree) =>
        // TODO: we can't do p.Pat.Wildcard(), because we expect a meta.Type here
        // NOTE: it looks like bounds in type wildcards aren't allowed by Scala's typechecker
        // so we just insert empty type bounds here
        p.Type.Placeholder(p.Type.Bounds(None, None))
      case g.Star(g.Ident(g.nme.WILDCARD)) =>
        p.Pat.Arg.SeqWildcard()
      case in @ g.Bind(_, g.Ident(g.nme.WILDCARD)) =>
        // TODO: discern `case x => ...` and `case x @ _ => ...`
        require(in.symbol.isTerm)
        p.Pat.Var(in.symbol.asTerm.rawcvt(in))
      case in @ g.Bind(_, g.EmptyTree) =>
        require(in.symbol.isType)
        in.symbol.asType.rawcvt(in)
      case in @ g.Bind(_, g.Typed(g.Ident(g.nme.WILDCARD), tpt)) =>
        require(in.symbol.isTerm)
        p.Pat.Typed(p.Pat.Var(in.symbol.asTerm.rawcvt(in)), tpt.cvt_!)
      case in @ g.Bind(name, tree) =>
        require(in.symbol.isTerm)
        require(name == in.symbol.name)
        p.Pat.Bind(p.Pat.Var(in.symbol.asTerm.rawcvt(in)), tree.cvt_!)
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
        else if (in.isType && in.symbol.isAnonymous) p.Type.Placeholder(ptypebounds(in.metadata("originalBounds").asInstanceOf[g.Tree]))
        else in.symbol.rawcvt(in)
      case g.ReferenceToBoxed(_) =>
        ???
      case in @ g.Literal(g.Constant(value)) =>
        value match {
          case null => p.Lit.Null()
          case () => p.Lit.Unit()
          case v: Boolean => p.Lit.Bool(v)
          case v: Byte => ???
          case v: Short => ???
          case v: Int => p.Lit.Int(v)
          case v: Long => p.Lit.Long(v)
          case v: Float => p.Lit.Float(v)
          case v: Double => p.Lit.Double(v)
          case v: String => p.Lit.String(v)
          case v: Char => p.Lit.Char(v)
          case _ => unreachable
        }
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
      case in @ g.CompoundTypeTree(templ) =>
        val template @ p.Template(early, parents, _, stats) = templ.cvt : p.Template
        require(template.isCompoundTypeCompatible)
        p.Type.Compound(parents.map(pctorcalltpe), stats.getOrElse(Nil))
      case in @ g.AppliedTypeTree(tpt, args) =>
        // TODO: infer whether that was really Apply, Function or Tuple
        // TODO: precisely infer whether that was infix application or normal application
        if (g.definitions.FunctionClass.seq.contains(tpt.tpe.typeSymbolDirect)) p.Type.Function(args.init.cvt_!, args.last.cvt_!)
        else if (g.definitions.TupleClass.seq.contains(tpt.tpe.typeSymbolDirect) && args.length > 1) p.Type.Tuple(args.cvt_!)
        else in match {
          case g.AppliedTypeTree(tpt @ g.Ident(name), List(lhs, rhs)) if name.looksLikeInfix => p.Type.ApplyInfix(lhs.cvt_!, tpt.cvt_!, rhs.cvt_!)
          case _ => p.Type.Apply(tpt.cvt_!, args.cvt_!)
        }
      case in @ g.ExistentialTypeTree(tpt, whereClauses) =>
        val isShorthand = whereClauses.exists(_.symbol.isAnonymous)
        object rememberBounds extends g.Transformer {
          override def transform(tree: g.Tree): g.Tree = tree match {
            case tree @ g.Ident(_) =>
              whereClauses.find(_.symbol == tree.symbol) match {
                case Some(g.TypeDef(_, _, _, tpt)) => tree.appendMetadata("originalBounds" -> tpt)
                case _ => super.transform(tree)
              }
            case _ =>
              super.transform(tree)
          }
        }
        if (isShorthand) (rememberBounds.transform(tpt).cvt_! : p.Type) else p.Type.Existential(tpt.cvt_!, whereClauses.cvt_!)
      case in @ g.SelectFromTypeTree(qual, name) =>
        p.Type.Project(qual.cvt_!, in.symbol.asType.precvt(qual.tpe, in))
      case in @ g.TypeBoundsTree(_, _) =>
        unreachable
      // NOTE: this derivation is ambiguous because of at least g.ValDef, g.TypeDef and g.Typed
      // case _: g.Tree =>
      //   derive
    }
  }

  // TODO: strangely enough, these caches don't improve performance in a measurable fashion
  private val tpeCache = TwoWayCache[g.Type, p.Type.Arg]()
  private val gsymToPmemberCache = mutable.Map[(g.Type, g.Symbol), p.Member]()
  object toApproximateScalameta {
    private def pannot(gannot: g.AnnotationInfo): p.Mod.Annot = {
      val g.AnnotationInfo(gatp, gargs, gassocs) = gannot
      p.Mod.Annot(???)
    }
    private def pannots(gannots: List[g.AnnotationInfo]): Seq[p.Mod.Annot] = {
      gannots.filter(gannot => {
        val gsym = gannot.tree.tpe.typeSymbol
        def isOldMacroSignature = gsym.fullName == "scala.reflect.macros.internal.macroImpl"
        def isNewMacroSignature = isOldMacroSignature
        !isOldMacroSignature && !isNewMacroSignature
      }).map(pannot)
    }
    private def pmods(gsym0: g.Symbol): Seq[p.Mod] = {
      // TODO: bring this up to sync with toScalameta.pmods
      val gsym = gsym0.getterIn(gsym0.owner).orElse(gsym0)
      val pmods = scala.collection.mutable.ListBuffer[p.Mod]()
      pmods ++= pannots(gsym0.annotations)
      if (gsym.isImplicit) pmods += p.Mod.Implicit()
      if (gsym.isFinal) pmods += p.Mod.Final()
      if (gsym.isSealed) pmods += p.Mod.Sealed()
      if (gsym.isOverride) pmods += p.Mod.Override()
      if (gsym.isCase) pmods += p.Mod.Case()
      if (gsym.isAbstract && !gsym.isParameter && !gsym.isTrait) pmods += p.Mod.Abstract()
      if (gsym.isAbstractOverride) { pmods += p.Mod.Abstract(); pmods += p.Mod.Override() }
      if (gsym.isCovariant) pmods += p.Mod.Covariant()
      if (gsym.isContravariant) pmods += p.Mod.Contravariant()
      if (gsym.isLazy) pmods += p.Mod.Lazy()
      pmods.toList
    }
    private def pvparamtpe(gtpe: g.Type): p.Type.Arg = {
      def unwrap(ptpe: p.Type): p.Type = ptpe.asInstanceOf[p.Type.Apply].args.head
      if (g.definitions.isRepeatedParamType(gtpe)) p.Type.Arg.Repeated(unwrap(apply(gtpe).asInstanceOf[p.Type]))
      else if (g.definitions.isByNameParamType(gtpe)) p.Type.Arg.ByName(unwrap(apply(gtpe).asInstanceOf[p.Type]))
      else apply(gtpe)
    }
    private def ptypebounds(gtpe: g.Type): p.Type.Bounds = gtpe match {
      case g.TypeBounds(glo, ghi) =>
        val plo = if (glo =:= g.typeOf[Nothing]) None else Some(apply(glo).asInstanceOf[p.Type])
        val phi = if (ghi =:= g.typeOf[Any]) None else Some(apply(ghi).asInstanceOf[p.Type])
        p.Type.Bounds(plo, phi)
      case _ =>
        unreachable
    }
    private def pvparam(gsym: g.Symbol): p.Term.Param = {
      require(gsym.isTerm)
      // TODO: discern inferred and explicitly specified vparamtpe
      // TODO: somehow figure out the default argument from a parameter symbol if it is specified
      p.Term.Param(pmods(gsym), gsym.asTerm.anoncvt(g.internal.valDef(gsym)), Some(pvparamtpe(gsym.info.depoly)), None).withOriginal(gsym)
    }
    private def pvparams(gsyms: List[g.Symbol]): Seq[p.Term.Param] = gsyms.map(pvparam)
    private def pvparamss(gsymss: List[List[g.Symbol]]): Seq[Seq[p.Term.Param]] = gsymss.map(pvparams)
    private def ptparam(gsym: g.Symbol): p.Type.Param = {
      // TODO: undo desugarings of context and view bounds
      require(gsym.isType)
      p.Type.Param(pmods(gsym), gsym.asType.anoncvt(g.internal.typeDef(gsym)), ptparams(gsym.typeParams), Nil, Nil, ptypebounds(gsym.info.depoly)).withOriginal(gsym)
    }
    private def ptparams(gsyms: List[g.Symbol]): Seq[p.Type.Param] = gsyms.map(ptparam)
    def apply(gtpe: g.Type): p.Type.Arg = tpeCache.getOrElseUpdate(gtpe, {
      def loop(gtpe: g.Type): p.Type.Arg = {
        val in = gtpe
        val result = gtpe match {
          case g.NoPrefix =>
            unreachable
          case g.NoType =>
            unreachable
          case in @ g.SuperType(thistpe, supertpe) =>
            require(thistpe.isInstanceOf[g.ThisType] && thistpe.typeSymbol.isType && supertpe.typeSymbol.isType)
            val superdumb = p.Term.Super(Some(thistpe.typeSymbol.name.toString), Some(supertpe.typeSymbol.name.toString)).withOriginal(in)
            val superpre = thistpe
            val supersym = if (supertpe.isInstanceOf[g.RefinedType]) g.NoSymbol else supertpe.typeSymbol
            p.Type.Singleton(superdumb.withDenot(thistpe, supersym))
          case in @ g.ThisType(sym) =>
            require(sym.isClass)
            if (sym.isModuleClass) p.Type.Singleton(sym.module.asTerm.rawcvt(g.Ident(sym.module)).withOriginal(in))
            else p.Type.Singleton(p.Term.This(Some(sym.name.toString)).withDenot(sym).withOriginal(in))
          case in @ g.SingleType(pre, sym) =>
            require(sym.isTerm)
            val ref = (pre match {
              case g.NoPrefix =>
                sym.asTerm.precvt(pre, g.Ident(sym))
              case pre if pre.typeSymbol.isStaticOwner =>
                sym.asTerm.precvt(pre, g.Ident(sym))
              case pre: g.SingletonType =>
                val p.Type.Singleton(preref) = loop(pre)
                p.Term.Select(preref, sym.asTerm.precvt(pre, g.Ident(sym)).withOriginal(in))
              case pre @ g.TypeRef(g.NoPrefix, quant, Nil) if quant.hasFlag(DEFERRED | EXISTENTIAL) =>
                require(quant.name.toString.endsWith(".type"))
                val preref = p.Term.Name(g.Ident(g.TermName(quant.name.toString.stripSuffix(".type"))).alias).withDenot(quant).withOriginal(quant)
                p.Term.Select(preref, sym.asTerm.precvt(pre, g.Ident(sym)).withOriginal(in))
              case pre: g.TypeRef =>
                // TODO: wow, so much for the hypothesis that all post-typer types are representable with syntax
                // here's one for you: take a look at `context.unit.synthetics.get` in Typers.scala
                // the prefix of the selection is typed as `Typers.this.global.CompilationUnit#synthetics.type`
                // from what I can see, we should represent this type as an existential, i.e.
                // `_1.synthetics.type forSome { val _1: Typers.this.global.CompilationUnit }`
                // however that representation would require non-trivial effort to pull off
                // (because we'll have to carry around that p.Type.Existential and unwrap it when anyone wants to use it)
                // therefore for now I'm just putting a stub here
                sym.asTerm.precvt(pre, g.Ident(sym))
              case _ =>
                sys.error(s"unsupported type $in, prefix = ${pre.getClass}, structure = ${g.showRaw(in, printIds = true, printTypes = true)}")
            }).withOriginal(in)
            // NOTE: we can't just emit p.Type.Singleton(p.Term.Name(...).withDenot(pre, sym))
            // because in some situations (when the prefix is not stable) that will be a lie
            // because naked names are supposed to be usable without a prefix
            p.Type.Singleton(ref)
          case in @ g.TypeRef(pre, sym, args) =>
            require(sym.isType)
            val ref = ({
              if (sym.isModuleClass) {
                apply(g.SingleType(pre, sym.module)).asInstanceOf[p.Type]
              } else {
                pre match {
                  case g.NoPrefix =>
                    sym.asType.precvt(pre, g.Ident(sym))
                  case pre if pre.typeSymbol.isStaticOwner =>
                    sym.asType.precvt(pre, g.Ident(sym))
                  case pre: g.SingletonType =>
                    val p.Type.Singleton(preref) = loop(pre)
                    p.Type.Select(preref, sym.asType.precvt(pre, g.Ident(sym)).withOriginal(in))
                  case _ =>
                    p.Type.Project(loop(pre).asInstanceOf[p.Type], sym.asType.precvt(pre, g.Ident(sym)).withOriginal(in))
                }
              }
            }).withOriginal(in)
            if (args.isEmpty) ref
            else p.Type.Apply(ref, args.map(arg => loop(arg).asInstanceOf[p.Type]))
          case g.RefinedType(parents, decls) =>
            // TODO: detect `val x, y: Int`
            // NOTE: due to the way we represent vals and vars, multiple g.Symbols can correspond to a single p.Member
            // therefore the logic of conversion between g.RefinedType and p.Type.Compound is so complicated:
            // 1) It needs to ensugar getters and setters to vals and vars
            // 2) It can't use standard h.Symbol creation facilities, because symCache only works for 1-to-1 correspondence
            @root trait Stat
            object Stat {
              @leaf class Getter(gget: g.Symbol) extends Stat
              @leaf class Setter(gget: g.Symbol, gset: g.Symbol) extends Stat
              @leaf class Other(gsym: g.Symbol) extends Stat
            }
            val gstats = decls.sorted.toList
            val stats = mutable.ListBuffer[Stat]()
            var i = 0
            while (i < gstats.length) {
              val gsym = gstats(i)
              if (gsym.isGetter) {
                val isSetter = (i + 1) < gstats.length && gstats(i + 1).isSetter
                if (isSetter) { stats += Stat.Setter(gstats(i), gstats(i + 1)); i += 1 }
                else stats += Stat.Getter(gsym)
              } else if (gsym.isSetter) {
                unreachable
              } else {
                stats += Stat.Other(gsym)
              }
              i += 1
            }
            val pstats = stats.toList.map({
              case Stat.Getter(gget) =>
                val hsymbol = symbolTable.getOrElseUpdateGetter(gget, h.Symbol.Local(randomUUID().toString))
                val hdenot = h.Denotation.Precomputed(h.Prefix.Zero, hsymbol) // TODO: actually, prefix here is not zero
                val pname = p.Term.Name(gget.name.toString, hdenot, h.Sigma.Naive)
                p.Decl.Val(pmods(gget), List(p.Pat.Var(pname)), apply(gget.info.resultType.depoly).asInstanceOf[p.Type])
              case Stat.Setter(gget, gset) =>
                val hsymbol = {
                  val result = symbolTable.getOrElseUpdateGetter(gget, h.Symbol.Local(randomUUID().toString))
                  symbolTable.getOrElseUpdateSetter(gset, result)
                }
                val hdenot = h.Denotation.Precomputed(h.Prefix.Zero, hsymbol) // TODO: actually, prefix here is not zero
                val pname = p.Term.Name(gget.name.toString, hdenot, h.Sigma.Naive)
                p.Decl.Var(pmods(gget), List(p.Pat.Var(pname)), apply(gget.info.resultType.depoly).asInstanceOf[p.Type])
              case Stat.Other(gsym) =>
                apply(g.NoPrefix, gsym).asInstanceOf[p.Stat]
            })
            p.Type.Compound(parents.map(parent => loop(parent).asInstanceOf[p.Type]), pstats)
          case g.ExistentialType(quants, underlying) =>
            // TODO: infer type placeholders where they were specified explicitly
            require(quants.forall(quant => quant.isType && quant.hasFlag(DEFERRED | EXISTENTIAL)))
            val pquants = quants.map(quant => {
              val name = quant.name.toString
              if (name.endsWith(".type")) {
                val pname = p.Term.Name(g.Ident(g.TermName(name.stripSuffix(".type"))).alias).withDenot(quant).withOriginal(quant)
                val g.TypeBounds(_, hi @ g.RefinedType(List(tpe, singleton), g.Scope())) = quant.info
                require(singleton == g.definitions.SingletonClass.tpe)
                val ptpe = apply(tpe).asInstanceOf[p.Type]
                p.Decl.Val(Nil, List(p.Pat.Var(pname).withOriginal(quant)), ptpe).withOriginal(quant)
              } else {
                val pname = quant.asType.rawcvt(g.Ident(quant)).withOriginal(quant)
                p.Decl.Type(Nil, pname, ptparams(quant.typeParams), ptypebounds(quant.info.depoly)).withOriginal(quant)
              }
            })
            p.Type.Existential(loop(underlying).asInstanceOf[p.Type], pquants)
          case g.AnnotatedType(annots, underlying) =>
            p.Type.Annotate(loop(underlying).asInstanceOf[p.Type], pannots(annots))
          case g.ConstantType(const) =>
            const.value match {
              case null => p.Lit.Null()
              case () => p.Lit.Unit()
              case v: Boolean => p.Lit.Bool(v)
              case v: Byte => ???
              case v: Short => ???
              case v: Int => p.Lit.Int(v)
              case v: Long => p.Lit.Long(v)
              case v: Float => p.Lit.Float(v)
              case v: Double => p.Lit.Double(v)
              case v: String => p.Lit.String(v)
              case v: Char => p.Lit.Char(v)
              case v: g.Type => unreachable
              case v: g.Symbol => unreachable
            }
          case _ =>
            sys.error(s"unsupported type $in, designation = ${in.getClass}, structure = ${g.showRaw(in, printIds = true, printTypes = true)}")
        }
        result.withOriginal(in)
      }
      loop(gtpe)
    })
    def apply(gpre: g.Type, gsym: g.Symbol): p.Member = gsymToPmemberCache.getOrElseUpdate((gpre, gsym), {
      def approximateSymbol(gsym: g.Symbol): p.Member = {
        def dummyTemplate = p.Template(Nil, Nil, p.Term.Param(Nil, p.Name.Anonymous(), None, None), None)
        def dummyCtor = p.Ctor.Primary(Nil, p.Ctor.Name("this"), Nil)
        def dummyBody = p.Term.Name("???")
        val result = gsym match {
          case PkgSymbol(gsym) => p.Pkg(gsym.rawcvt(g.Ident(gsym)), Nil)
          case ClassSymbol(gsym) => p.Defn.Class(pmods(gsym), gsym.rawcvt(g.Ident(gsym)), ptparams(gsym.typeParams), dummyCtor, dummyTemplate)
          case TraitSymbol(gsym) => p.Defn.Trait(pmods(gsym), gsym.rawcvt(g.Ident(gsym)), ptparams(gsym.typeParams), dummyCtor, dummyTemplate)
          case PkgObjectSymbol(gsym) => p.Pkg.Object(Nil, gsym.rawcvt(g.Ident(gsym)), dummyCtor, dummyTemplate)
          case ObjectSymbol(gsym) => p.Defn.Object(pmods(gsym), gsym.rawcvt(g.Ident(gsym)), dummyCtor, dummyTemplate)
          case AbstractValSymbol(gsym) => p.Decl.Val(pmods(gsym), List(p.Pat.Var(gsym.rawcvt(g.ValDef(gsym)))), apply(gsym.info.depoly).asInstanceOf[p.Type]).pats.head.require[p.Pat.Var].name
          case ValSymbol(gsym) => p.Defn.Val(pmods(gsym), List(p.Pat.Var(gsym.rawcvt(g.ValDef(gsym)))), Some(apply(gsym.info.depoly).asInstanceOf[p.Type]), dummyBody).pats.head.require[p.Pat.Var].name
          case AbstractVarSymbol(gsym) => p.Decl.Var(pmods(gsym), List(p.Pat.Var(gsym.rawcvt(g.ValDef(gsym)))), apply(gsym.info.depoly).asInstanceOf[p.Type]).pats.head.require[p.Pat.Var].name
          case VarSymbol(gsym) => p.Defn.Var(pmods(gsym), List(p.Pat.Var(gsym.rawcvt(g.ValDef(gsym)))), Some(apply(gsym.info.depoly).asInstanceOf[p.Type]), Some(dummyBody)).pats.head.require[p.Pat.Var].name
          case AbstractDefSymbol(gsym) => p.Decl.Def(pmods(gsym), gsym.rawcvt(g.DefDef(gsym, g.EmptyTree)), ptparams(gsym.typeParams), pvparamss(gsym.paramss), apply(gsym.info.finalResultType).asInstanceOf[p.Type])
          case DefSymbol(gsym) => p.Defn.Def(pmods(gsym), gsym.rawcvt(g.DefDef(gsym, g.EmptyTree)), ptparams(gsym.typeParams), pvparamss(gsym.paramss), Some(apply(gsym.info.finalResultType).asInstanceOf[p.Type]), dummyBody)
          case MacroSymbol(gsym) => p.Defn.Macro(pmods(gsym), gsym.rawcvt(g.DefDef(gsym, g.EmptyTree)), ptparams(gsym.typeParams), pvparamss(gsym.paramss), apply(gsym.info.finalResultType).asInstanceOf[p.Type], dummyBody)
          case AbstractTypeSymbol(gsym) => p.Decl.Type(pmods(gsym), gsym.rawcvt(g.TypeDef(gsym)), ptparams(gsym.typeParams), ptypebounds(gsym.info.depoly))
          case AliasTypeSymbol(gsym) => p.Defn.Type(pmods(gsym), gsym.rawcvt(g.TypeDef(gsym, g.TypeTree(gsym.info))), ptparams(gsym.typeParams), apply(gsym.info.depoly).asInstanceOf[p.Type])
          case _ => sys.error(s"unsupported symbol $gsym, designation = ${gsym.getClass}, flags = ${gsym.flags}")
        }
        result.withOriginal(gsym)
      }
      def applyPrefix(gpre: g.Type, pmem: p.Member): p.Member = {
        if (gpre == g.NoPrefix) pmem
        else {
          // TODO: implement me! it might not be that hard, to be honest
          // 1) Replace Type.Name(tparam) in pmem and its denotations with values obtained from gpre
          // 2) Replace Term.This(pmem.owner) in pmem and its denotations with apply(gpre)
          pmem
        }
      }
      val maybeNativePmember = symbolTable.get(gsym).flatMap(hsym => hsymToNativePmemberCache.get(hsym))
      val pmember = maybeNativePmember.getOrElse(approximateSymbol(gsym))
      applyPrefix(gpre, pmember)
    })
  }

  object toScalareflect {
    private def gannotinfo(pannot: p.Mod.Annot): g.AnnotationInfo = {
      val gtpe = apply(pannot.tree.ctorTpe)
      val gargss = pannot.tree.ctorArgss.map(_.map(apply))
      if (gargss.length > 1) throw new SemanticException("implementation restriction: annotations with multiple argument lists are not supported by scalac")
      if (gtpe <:< g.definitions.StaticAnnotationClass.tpe) {
        g.AnnotationInfo(gtpe, gargss.head.toList, Nil)
      } else {
        require(gtpe <:< g.definitions.ClassfileAnnotationClass.tpe)
        ???
      }
    }
    private def gannotinfos(pannots: Seq[p.Mod.Annot]): List[g.AnnotationInfo] = {
      pannots.map(gannotinfo).toList
    }
    private def gtypeBounds(pbounds: p.Type.Bounds): g.TypeBounds = {
      val glo = pbounds.lo.map(apply).getOrElse(g.definitions.NothingClass.tpe)
      val ghi = pbounds.hi.map(apply).getOrElse(g.definitions.AnyClass.tpe)
      g.TypeBounds(glo, ghi)
    }
    private implicit class RichScalareflectSymbol(gsym: g.Symbol) {
      private def mimicMods(pmods: Seq[p.Mod], ptree: p.Tree): Unit = {
        pmods.foreach({
          case pmod: p.Mod.Annot => gsym.withAnnotations(List(gannotinfo(pmod)))
          case pmod: p.Mod.Private => gsym.setFlag(PRIVATE)
          case pmod: p.Mod.PrivateThis => gsym.setFlag(LOCAL)
          case pmod: p.Mod.PrivateWithin => ???
          case pmod: p.Mod.Protected => gsym.setFlag(PROTECTED)
          case pmod: p.Mod.ProtectedThis => gsym.setFlag(LOCAL)
          case pmod: p.Mod.ProtectedWithin => ???
          case pmod: p.Mod.Implicit => gsym.setFlag(IMPLICIT)
          case pmod: p.Mod.Final => gsym.setFlag(FINAL)
          case pmod: p.Mod.Sealed => gsym.setFlag(SEALED)
          case pmod: p.Mod.Override => gsym.setFlag(OVERRIDE)
          case pmod: p.Mod.Case => gsym.setFlag(CASE)
          case pmod: p.Mod.Abstract => gsym.setFlag(ABSTRACT)
          case pmod: p.Mod.Covariant => gsym.setFlag(COVARIANT)
          case pmod: p.Mod.Contravariant => gsym.setFlag(CONTRAVARIANT)
          case pmod: p.Mod.Lazy => gsym.setFlag(LAZY)
          case pmod: p.Mod.ValParam => // do nothing
          case pmod: p.Mod.VarParam => // do nothing
        })
        // TODO: INTERFACE, MUTABLE, STATIC, PRESUPER, INCONSTRUCTOR, STABLE, *ACCESSOR, EXISTENTIAL
        ptree match { case p.Term.Param(_, _, Some(tpe), _) if apply(tpe).typeSymbol == g.definitions.ByNameParamClass => gsym.setFlag(BYNAMEPARAM); case _ => }
        if (gsym.hasFlag(ABSTRACT) && gsym.hasFlag(OVERRIDE)) { gsym.resetFlag(ABSTRACT | OVERRIDE); gsym.setFlag(ABSOVERRIDE) }
        if (ptree.isInstanceOf[p.Defn.Trait]) gsym.setFlag(TRAIT)
        ptree match { case ptree: p.Term.Param if ptree.default.nonEmpty => gsym.setFlag(DEFAULTPARAM); case _ => }
        ptree match { case ptree: p.Defn.Var if ptree.rhs.isEmpty => gsym.setFlag(DEFAULTINIT); case _ => }
      }
      private def gtparams(ptparams: Seq[p.Type.Param]): List[g.Symbol] = {
        ptparams.map(ptparam => {
          val htparam = ptparam.name.asInstanceOf[p.Name].denot.symbol
          val gtparam = symbolTable.getOrElseUpdate(htparam, gsym.newTypeSymbol(g.TypeName(ptparam.name.toString), newFlags = PARAM | DEFERRED))
          gtparam.mimic(ptparam)
        }).toList
      }
      private def gparams(pparams: Seq[p.Term.Param]): List[g.Symbol] = {
        pparams.map(pparam => {
          val hparam = pparam.name.asInstanceOf[p.Name].denot.symbol
          val gparam = symbolTable.getOrElseUpdate(hparam, gsym.newTermSymbol(g.TermName(pparam.name.toString), newFlags = PARAM))
          gparam.mimic(pparam)
        }).toList
      }
      private def mimicInfo(ptree: p.Tree): Unit = {
        ptree match {
          case p.Decl.Val(_, _, ptpe) if gsym.isGetter =>
            gsym.setInfo(g.NullaryMethodType(apply(ptpe)))
          case p.Decl.Var(_, _, ptpe) if gsym.isGetter =>
            gsym.setInfo(g.NullaryMethodType(apply(ptpe)))
          case p.Decl.Var(_, _, ptpe) if gsym.isSetter =>
            val gparams = List(gsym.newTermSymbol(g.TermName("x$1"), newFlags = PARAM).setInfo(apply(ptpe)))
            val gret = g.definitions.UnitClass.tpe
            gsym.setInfo(g.MethodType(gparams, gret))
          case p.Decl.Def(_, _, ptparams, pparamss, ptpe) =>
            val gprecachedTparams = gtparams(ptparams)
            val gprecachedParamss = pparamss.map(gparams)
            val gmethodType = gprecachedParamss.foldLeft(apply(ptpe))((curr, gparams) => g.MethodType(gparams, curr))
            gsym.setInfo(g.genPolyType(gprecachedTparams, gmethodType))
          case p.Decl.Type(_, _, ptparams, ptypeBounds) =>
            gsym.setInfo(g.genPolyType(gtparams(ptparams), gtypeBounds(ptypeBounds)))
          case p.Defn.Type(_, _, ptparams, ptpe) =>
            gsym.setInfo(g.genPolyType(gtparams(ptparams), apply(ptpe)))
          case p.Type.Param(_, _, ptparams, pcontextBounds, pviewBounds, ptypeBounds) =>
            require(pcontextBounds.isEmpty && pviewBounds.isEmpty)
            gsym.setInfo(g.genPolyType(gtparams(ptparams), gtypeBounds(ptypeBounds)))
          case p.Term.Param(_, _, pdecltpe, pdefault) =>
            require(pdecltpe.nonEmpty && pdefault.isEmpty)
            gsym.setInfo(apply(pdecltpe.get))
          case _ =>
            ???
        }
      }
      def mimic(ptree: p.Tree): g.Symbol = {
        if (!gsym.hasRawInfo) {
          import scala.language.reflectiveCalls
          mimicMods(ptree.asInstanceOf[{ def mods: Seq[p.Mod] }].mods, ptree)
          mimicInfo(ptree)
        }
        gsym
      }
    }
    private def gowner(ptree: p.Tree): g.Symbol = {
      // TODO: we probably need something other than NoSymbol for RefinedType.decls and ExistentialType.quants
      // I always had no idea about how this works in scala. I guess, it's time for find out :)
      g.NoSymbol
    }
    private def gprefix(hprefix: h.Prefix): g.Type = {
      hprefix match {
        case h.Prefix.Zero => g.NoPrefix
        case h.Prefix.Type(ptpe) => apply(ptpe.asInstanceOf[p.Type])
      }
    }
    def apply(ptree: p.Tree): g.Tree = {
      ???
    }
    def apply(ptpe: p.Type.Arg): g.Type = tpeCache.getOrElseUpdate(ptpe, {
      def loop(ptpe: p.Type.Arg): g.Type = ptpe match {
        case pname: p.Type.Name =>
          g.TypeRef(gprefix(pname.denot.prefix), symbolTable(pname.denot.symbol), Nil)
        case p.Type.Select(pqual, pname) =>
          g.TypeRef(loop(p.Type.Singleton(pqual)), symbolTable(pname.denot.symbol), Nil)
        case p.Type.Project(pqual, pname) =>
          g.TypeRef(loop(pqual), symbolTable(pname.denot.symbol), Nil)
        case p.Type.Singleton(pref) =>
          def singleType(pname: p.Term.Name): g.Type = {
            val gsym = symbolTable(pname.denot.symbol)
            if (gsym.isModuleClass) g.ThisType(gsym)
            else g.SingleType(gprefix(pname.denot.prefix), gsym)
          }
          def superType(psuper: p.Term.Super): g.Type = {
            val gpre = gprefix(psuper.denot.prefix)
            val gmixsym = psuper.denot.symbol match {
              case h.Symbol.Zero => g.intersectionType(gpre.typeSymbol.info.parents)
              case hsym => gpre.typeSymbol.info.baseType(symbolTable(hsym))
            }
            g.SuperType(gpre, gmixsym)
          }
          pref match {
            case pname: p.Term.Name => singleType(pname)
            case p.Term.Select(_, pname) => singleType(pname)
            case pref: p.Term.This => g.ThisType(symbolTable(pref.denot.symbol))
            case pref: p.Term.Super => superType(pref)
          }
        case p.Type.Apply(ptpe, pargs) =>
          g.appliedType(loop(ptpe), pargs.map(loop).toList)
        case p.Type.ApplyInfix(plhs, pop, prhs) =>
          g.appliedType(loop(pop), List(loop(plhs), loop(prhs)))
        case p.Type.Function(pparams, pres) =>
          g.appliedType(g.definitions.FunctionClass(pparams.length + 1), (pparams :+ pres).map(loop).toList)
        case p.Type.Tuple(pelements) =>
          g.appliedType(g.definitions.TupleClass(pelements.length), pelements.map(loop).toList)
        case p.Type.Compound(ptpes, prefinement) =>
          // NOTE: `C { val x: Int; var y: Long; def z: Float; type T <: Double; type U = String }` is represented as:
          // RefinedType(List(TypeRef(ThisType(Test#7837), Test.C#7939, List())), Scope(TermName("x")#16204, TermName("y")#16205, TermName("y_$eq")#16206, TermName("z")#16207, TypeName("T")#16208, TypeName("U")#16209))
          // value x, flags = 138412112 (DEFERRED | METHOD | STABLE | ACCESSOR), info = => Int
          // method y, flags = 134217808 (DEFERRED | METHOD | ACCESSOR), info = => Long
          // method y_=, flags = 134217808 (DEFERRED | METHOD | ACCESSOR), info = (x$1: Long)Unit
          // method z, flags = 80 (DEFERRED | METHOD), info = => Float
          // type T, flags = 16 (DEFERRED), info =  <: Double
          // type U, flags = 0, info = String
          // NOTE: due to the way we represent vals and vars, a single p.Member can give rise to multiple g.Symbols
          // therefore the logic of conversion between p.Type.Compound and g.RefinedType is so complicated:
          // 1) It needs to desugar vals and vars into getters and setters
          // 2) It can't use standard g.Symbol creation facilities, because symCache only works for 1-to-1 correspondence
          val gscope = g.newScope
          def namesPrefine(prefine: p.Stat) = prefine match {
            case p.Decl.Val(_, vars, _) => vars.map(_.name)
            case p.Decl.Var(_, vars, _) => vars.map(_.name)
            case p.Decl.Def(_, name, _, _, _) => List(name)
            case p.Decl.Type(_, name, _, _) => List(name)
            case p.Defn.Type(_, name, _, _) => List(name)
            case _ => unreachable
          }
          val pexplodedRefinement = prefinement.flatMap(prefine => namesPrefine(prefine).map(pname => prefine -> pname))
          val refinement = pexplodedRefinement.flatMap({ case (prefine, pname) =>
            def mkTerm(name: String, flags: Long) = gowner(prefine).newTermSymbol(g.TermName(pname.toString), newFlags = flags)
            def mkType(name: String, flags: Long) = gowner(prefine).newTypeSymbol(g.TypeName(pname.toString), newFlags = flags)
            val hrefine = pname.denot.symbol
            val grefines = prefine match {
              case _: p.Decl.Val =>
                val ggetter = symbolTable.getOrElseUpdateGetter(hrefine, mkTerm(pname.toString, DEFERRED | METHOD | STABLE | ACCESSOR))
                List(ggetter)
              case _: p.Decl.Var =>
                val ggetter = symbolTable.getOrElseUpdateGetter(hrefine, mkTerm(pname.toString, DEFERRED | METHOD | ACCESSOR))
                val gsetter = symbolTable.getOrElseUpdateGetter(hrefine, mkTerm(pname.toString + "_=", DEFERRED | METHOD | ACCESSOR))
                List(ggetter, gsetter)
              case _: p.Decl.Def =>
                List(symbolTable.getOrElseUpdate(hrefine, mkTerm(pname.toString, METHOD)))
              case _: p.Decl.Type =>
                List(symbolTable.getOrElseUpdate(hrefine, mkType(pname.toString, DEFERRED)))
              case _: p.Defn.Type =>
                List(symbolTable.getOrElseUpdate(hrefine, mkType(pname.toString, 0)))
              case _ =>
                unreachable
            }
            grefines.map(grefine => prefine -> gscope.enter(grefine))
          })
          refinement.foreach({ case (prefine, grefine) => grefine.mimic(prefine) })
          g.refinedType(ptpes.map(loop).toList, gowner(ptpe), gscope, g.NoPosition)
        case p.Type.Existential(ptpe, pquants) =>
          // NOTE: `C forSome { val x: Int; type T <: AnyRef }` is represented as:
          // ExistentialType(List(TypeName("x.type")#16433, TypeName("T")#16434), TypeRef(ThisType(Test#7837), Test.C#7939, List()))
          // type x.type, flags = 34359738384 (DEFERRED | EXISTENTIAL), info =  <: Int with Singleton
          // type T, flags = 34359738384 (DEFERRED | EXISTENTIAL), info =  <: AnyRef
          def namesPquant(pquant: p.Stat) = pquant match {
            case p.Decl.Val(_, pats, _) => pats.map(_.name)
            case p.Decl.Type(_, name, _, _) => List(name)
            case _ => unreachable
          }
          def createGquant(pname: p.Name, pquant: p.Stat): g.Symbol = pquant match {
            case p.Decl.Val(mods, _, _) => gowner(pquant).newTypeSymbol(g.TypeName(pname.toString + ".type"), newFlags = DEFERRED | EXISTENTIAL)
            case p.Decl.Type(mods, _, _, _) => gowner(pquant).newTypeSymbol(g.TypeName(pname.toString), newFlags = DEFERRED | EXISTENTIAL)
            case _ => unreachable
          }
          val pexplodedQuants = pquants.flatMap(pquant => namesPquant(pquant).map(pname => pquant -> pname))
          val quants = pexplodedQuants.map({ case (pquant, pname) =>
            val gquant = symbolTable.getOrElseUpdate(pname.denot.symbol, createGquant(pname, pquant))
            pquant -> gquant
          })
          quants.foreach{
            case (pquant: p.Decl.Val, gquant) =>
              // NOTE: this can't use mimic because of the need to model the weird desugaring scheme employed by scalac
              val upperBound = g.intersectionType(List(loop(pquant.decltpe), g.definitions.SingletonClass.tpe), gowner(pquant))
              gquant.setInfo(g.TypeBounds.upper(upperBound))
            case (pquant: p.Decl.Type, gquant) =>
              gquant.mimic(pquant)
            case _ =>
              unreachable
          }
          g.ExistentialType(quants.map(_._2).toList, loop(ptpe))
        case p.Type.Annotate(ptpe, pannots) =>
          g.AnnotatedType(gannotinfos(pannots), loop(ptpe))
        case p.Type.Placeholder(pbounds) =>
          ???
        case p.Type.Arg.ByName(ptpe) =>
          g.appliedType(g.definitions.ByNameParamClass, List(loop(ptpe)))
        case p.Type.Arg.Repeated(ptpe) =>
          g.appliedType(g.definitions.RepeatedParamClass, List(loop(ptpe)))
        case plit: p.Lit =>
          plit match {
            case p.Lit.Bool(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Int(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Long(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Float(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Double(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Char(value) => g.ConstantType(g.Constant(value))
            case p.Lit.String(value) => g.ConstantType(g.Constant(value))
            case p.Lit.Symbol(value) => unreachable
            case p.Lit.Null() => g.ConstantType(g.Constant(null))
            case p.Lit.Unit() => g.ConstantType(g.Constant(()))
          }
      }
      ptpe.requireAttributed()
      loop(ptpe)
    })
  }

  private object symbolTable {
    private val symCache = TwoWayCache[g.Symbol, h.Symbol]()
    private val getterCache = TwoWayCache[g.Symbol, h.Symbol]()
    private val setterCache = TwoWayCache[g.Symbol, h.Symbol]()

    // TODO: `apply` is somewhat copy/pasted from core/quasiquotes/Macros.scala
    // however, there's no way for us to share those implementations until we bootstrap
    def apply(gsym: g.Symbol): h.Symbol = symCache.getOrElseUpdate(gsym, {
      def isGlobal(gsym: g.Symbol): Boolean = {
        def definitelyLocal = gsym == g.NoSymbol || gsym.name.toString.startsWith("<local ") || (gsym.owner.isMethod && !gsym.isParameter)
        def isParentGlobal = gsym.hasPackageFlag || isGlobal(gsym.owner)
        !definitelyLocal && isParentGlobal
      }
      def signature(gsym: g.Symbol): h.Signature = {
        if (gsym.isMethod && !gsym.asMethod.isGetter) h.Signature.Method(gsym.jvmsig)
        else if (gsym.isTerm || (gsym.hasFlag(DEFERRED | EXISTENTIAL) && gsym.name.toString.endsWith(".type"))) h.Signature.Term
        else if (gsym.isType) h.Signature.Type
        else unreachable
      }
      // TODO: implement this
      // // NOTE: so far, our current model is to ignore the existence of getters and setters
      // // therefore, we should make sure that they don't end up here
      // require(!gsym.isGetter && !gsym.isSetter)
      if (gsym.isModuleClass || gsym.isPackageClass) apply(gsym.asClass.module)
      else if (gsym == g.NoSymbol) h.Symbol.Zero
      else if (gsym == g.rootMirror.RootPackage) h.Symbol.Root
      else if (gsym == g.rootMirror.EmptyPackage) h.Symbol.Empty
      else if (isGlobal(gsym)) h.Symbol.Global(apply(gsym.owner), gsym.name.decodedName.toString, signature(gsym))
      else h.Symbol.Local(randomUUID().toString)
    })

    def get(gsym: g.Symbol): Option[h.Symbol] = symCache.get(gsym)
    def getOrElseUpdate(gsym: g.Symbol, hsym: => h.Symbol): h.Symbol = symCache.getOrElseUpdate(gsym, hsym)
    def getOrElseUpdateGetter(gsym: g.Symbol, hsym: => h.Symbol): h.Symbol = getterCache.getOrElseUpdate(gsym, hsym)
    def getOrElseUpdateSetter(gsym: g.Symbol, hsym: => h.Symbol): h.Symbol = setterCache.getOrElseUpdate(gsym, hsym)

    def apply(hsym: h.Symbol): g.Symbol = symCache.getOrElseUpdate(hsym, {
      def resolve(gsym: g.Symbol, name: String, hsig: h.Signature): g.Symbol = hsig match {
        case h.Signature.Type => gsym.info.decl(g.TypeName(name)).asType
        case h.Signature.Term => gsym.info.decl(g.TermName(name)).suchThat(galt => galt.isGetter || !galt.isMethod)
        case h.Signature.Method(jvmsig) => gsym.info.decl(g.TermName(name)).suchThat(galt => galt.isMethod && galt.jvmsig == jvmsig)
      }
      hsym match {
        case h.Symbol.Zero => g.NoSymbol
        case h.Symbol.Root => g.rootMirror.RootPackage
        case h.Symbol.Empty => g.rootMirror.EmptyPackage
        case h.Symbol.Global(howner, name, hsig) => resolve(apply(howner), name, hsig)
        case h.Symbol.Local(id) => throw new SemanticException(s"implementation restriction: internal cache has no symbol associated with $hsym")
      }
    })

    def get(hsym: h.Symbol): Option[g.Symbol] = symCache.get(hsym)
    def getOrElseUpdate(hsym: h.Symbol, gsym: => g.Symbol): g.Symbol = symCache.getOrElseUpdate(hsym, gsym)
    def getOrElseUpdateGetter(hsym: h.Symbol, gsym: => g.Symbol): g.Symbol = getterCache.getOrElseUpdate(hsym, gsym)
    def getOrElseUpdateSetter(hsym: h.Symbol, gsym: => g.Symbol): g.Symbol = setterCache.getOrElseUpdate(hsym, gsym)
  }
}

class MacroContext[G <: ScalaGlobal](val scalareflectMacroContext: ScalareflectMacroContext)
extends ScalahostSemanticContext[G](scalareflectMacroContext.universe.asInstanceOf[G]) with ScalametaMacroContext {
  private[meta] def warning(msg: String): Unit = ???
  private[meta] def error(msg: String): Unit = ???
  private[meta] def abort(msg: String): Nothing = ???
  private[meta] def resources: Map[String, Array[Byte]] = ???
}

class ToolboxContext(tb: ToolBox[ru.type]) extends ScalahostSemanticContext(tb.global) {
  def define(rutree: ru.ImplDef): p.Tree = {
    val gtree = g.mkImporter(ru).importTree(rutree).asInstanceOf[g.ImplDef]
    val gtypedtree = {
      import g._
      import analyzer._
      val pkg = PackageDef(Ident(nme.EMPTY_PACKAGE_NAME), List(gtree))
      val run = new Run
      reporter.reset()
      phase = run.namerPhase
      globalPhase = run.namerPhase
      val namer = newNamer(rootContext(NoCompilationUnit))
      namer.enterSym(pkg)
      phase = run.typerPhase
      globalPhase = run.typerPhase
      val typer = newTyper(rootContext(NoCompilationUnit))
      val typedpkg = typer.typed(pkg).asInstanceOf[Tree]
      if (tb.frontEnd.hasErrors) sys.error("reflective compilation has failed:" + EOL + EOL + (tb.frontEnd.infos map (_.msg) mkString EOL))
      typedpkg.asInstanceOf[PackageDef].stats.head
    }
    toScalameta(gtypedtree, classOf[papi.Stat])
  }
}

object Scalahost {
  def mkSemanticContext[G <: ScalaGlobal](g: G): ScalametaSemanticContext with ScalahostSemanticContext[G] = {
    new ScalahostSemanticContext[G](g)
  }
  def mkMacroContext[G <: ScalaGlobal](rc: ScalareflectMacroContext): ScalametaMacroContext with ScalahostMacroContext[G] = {
    new ScalahostMacroContext[G](rc)
  }
  def mkToolboxContext(mirror: ru.Mirror, options: String = ""): ToolboxContext = {
    var toolboxOptions = options
    if (!toolboxOptions.contains("-Xplugin-require:scalahost")) {
      val scalahostJar = {
        try getClass.getProtectionDomain().getCodeSource().getLocation().getFile()
        catch { case ex: Throwable => throw new scala.Exception("failed to auto-load the scalahost plugin", ex) }
      }
      val scalahostOptions = " -Xplugin:" + scalahostJar + " -Xplugin-require:scalahost"
      toolboxOptions += scalahostOptions
    }
    new ToolboxContext(mirror.mkToolBox(mkSilentFrontEnd(), toolboxOptions))
  }
}

package scala.meta
package internal.hosts.scalacompiler
package scalahost

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.reflect.{classTag, ClassTag}
import scala.reflect.runtime.universe.{Type => Pt, typeOf}
import scala.{meta => p}
import scala.meta.semantic._
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Host => PalladiumHost}
import org.scalameta.convert._
import org.scalameta.convert.auto._
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.reflection._

class Host[G <: ScalaGlobal](val g: G) extends PalladiumHost with GlobalToolkit {
  lazy val global: g.type = g
  import g.Quasiquote
  implicit val palladiumHost: PalladiumHost = this

  def defns(ref: Ref): Seq[Tree] = ???
  def attrs(tree: Tree): Seq[Attr] = ???
  def owner(tree: Tree): Scope = ???
  def members(scope: Scope): Seq[Tree] = ???
  def members(scope: Scope, name: Name): Seq[Tree] = ???
  def <:<(tpe1: Type, tpe2: Type): Boolean = ???
  def lub(tpes: Seq[Type]): Type = ???
  def glb(tpes: Seq[Type]): Type = ???
  def superclasses(member: Member.Template): Seq[Member.Template] = ???
  def subclasses(member: Member.Template): Seq[Member.Template] = ???
  def overridden(member: Member): Seq[Member] = ???
  def overriding(member: Member): Seq[Member] = ???
  def erasure(tpe: Type): Type = ???

  // NOTE: we only handle trees, not types or symbols
  // NOTE: can't convert symbols, because that's quite unsafe: a lot of symbols don't make sense without prefixes
  // NOTE: can't convert types, because they don't have originals, so any such conversion will be an approximation
  // NOTE: careful use of NameTree.name, because it can lie (renaming imports) and it doesn't have enough semantic information (unlike the underlying symbol)
  // TODO: remember positions. actually, in scalac they are almost accurate, so it would be a shame to discard them
  @converter def toPalladium(in: Any, pt: Pt): Any = {
    object Helpers extends g.ReificationSupportImpl { self =>
      implicit class RichHelperTree(gtree: g.Tree) {
        def alias: String = gtree match {
          case gtree: g.NameTree => gtree.name.decodedName.toString
          case g.This(name) => name.decodedName.toString
          case g.Super(_, name) => name.decodedName.toString
        }
        def isBackquoted: Boolean = gtree match {
          // TODO: infer isBackquoted
          // TODO: iirc according to Denys, info in BackquotedIdentifierAttachment might be incomplete
          case gtree: g.Ident => gtree.isBackquoted || scala.meta.syntactic.parsers.keywords.contains(gtree.name.toString)
          case gtree: g.Select => scala.meta.syntactic.parsers.keywords.contains(gtree.name.toString)
          case _ => false
        }
      }
      implicit class RichHelperName(gname: g.Name) {
        def looksLikeInfix = !gname.decoded.forall(c => Character.isLetter(c) || Character.isDigit(c) || c == '_') || gname == g.nme.eq || gname == g.nme.ne
      }
      implicit class RichHelperSymbol(gsym: g.Symbol) {
        def isAnonymous: Boolean = {
          // NOTE: not all symbols whose names start with x$ are placeholders
          // there are also at least artifact vals created for named arguments
          val isTermPlaceholder = gsym.isTerm && gsym.isParameter && gsym.name.startsWith(g.nme.FRESH_TERM_NAME_PREFIX)
          val isAnonymousSelfName = gsym.name.startsWith(g.nme.FRESH_TERM_NAME_PREFIX) || gsym.name == g.nme.this_
          val isAnonymousSelf = gsym.isTerm && isAnonymousSelfName && gsym.owner.isClass // TODO: more precise detection, probably via attachments
          val isAnonymousTypeParameter = gsym.name == g.tpnme.WILDCARD
          isTermPlaceholder || isAnonymousSelf || isAnonymousTypeParameter
        }
        def precvt(pre: g.Type, in: g.Tree): p.Name = {
          gsym.rawcvt(in).appendScratchpad(pre)
        }
        def rawcvt(in: g.Tree): p.Name = {
          if (gsym.isTerm) p.Term.Name(in.alias, in.isBackquoted).appendScratchpad(gsym)
          else if (gsym.isType) p.Type.Name(in.alias, in.isBackquoted).appendScratchpad(gsym)
          else unreachable
        }
        def qualcvt(in: g.Tree): p.Qual.Name = {
          require(gsym != g.NoSymbol)
          val gsyms = {
            if (gsym.isModuleClass) List(gsym.sourceModule.asModule, g.NoSymbol)
            else List(g.NoSymbol, gsym.asClass)
          }
          p.Qual.Name(in.alias, in.isBackquoted).appendScratchpad(gsyms)
        }
      }
      implicit class RichHelperSymbols(gsyms: List[g.Symbol]) {
        def importcvt(in: g.Tree): p.Import.Name = {
          val List(gterm, gtype) = gsyms
          require(gterm != g.NoSymbol || gtype != g.NoSymbol)
          require(gterm != g.NoSymbol ==> gterm.isTerm)
          require(gtype != g.NoSymbol ==> gtype.isType)
          p.Import.Name(in.alias, in.isBackquoted).appendScratchpad(gsyms)
        }
      }
      implicit class RichHelperTermSymbol(gsym: g.TermSymbol) {
        def precvt(pre: g.Type, in: g.Tree): p.Term.Name = (gsym: g.Symbol).precvt(pre, in).asInstanceOf[p.Term.Name]
        def rawcvt(in: g.Tree, allowNoSymbol: Boolean = false): p.Term.Name = (gsym: g.Symbol).rawcvt(in).asInstanceOf[p.Term.Name]
      }
      implicit class RichHelperTypeSymbol(gsym: g.TypeSymbol) {
        def precvt(pre: g.Type, in: g.Tree): p.Type.Name = (gsym: g.Symbol).precvt(pre, in).asInstanceOf[p.Type.Name]
        def rawcvt(in: g.Tree): p.Type.Name = (gsym: g.Symbol).rawcvt(in).asInstanceOf[p.Type.Name]
      }
      def pannot(gannot: g.Tree): p.Mod.Annot = {
        val q"new $gtpt(...$gargss)" = gannot
        p.Mod.Annot(gtpt.cvt_!, pargss(gargss))
      }
      def pmods(gmdef: g.MemberDef): Seq[p.Mod] = {
        // TODO: since we have mods correctly ensugared here, I think, we could try to avoid using gsym here
        // however, everything works fine at the moment, so I'll denote this refactoring as future work
        def paccessqual(gsym: g.Symbol): Option[p.Mod.AccessQualifier] = {
          if (gsym.isPrivateThis || gsym.isProtectedThis) {
            // TODO: does NoSymbol here actually mean gsym.owner?
            val gpriv = gsym.privateWithin.orElse(gsym.owner)
            require(gpriv.isClass)
            Some(p.Term.This(None).appendScratchpad(gpriv))
          } else if (gsym.privateWithin == g.NoSymbol || gsym.privateWithin == null) None
          else Some(gsym.privateWithin.qualcvt(g.Ident(gsym.privateWithin))) // TODO: this loses information is gsym.privateWithin was brought into scope with a renaming import
        }
        val gsym = gmdef.symbol.getterIn(gmdef.symbol.owner).orElse(gmdef.symbol)
        val pmods = scala.collection.mutable.ListBuffer[p.Mod]()
        pmods ++= gmdef.mods.annotations.map(pannot)
         // NOTE: some sick artifact vals produced by mkPatDef can be private to method, so we can't invoke paccessqual on them
        if (gsym.isPrivate && !gsym.isSynthetic && !gsym.isArtifact) pmods += p.Mod.Private(paccessqual(gsym))
        if (gsym.isProtected) pmods += p.Mod.Protected(paccessqual(gsym))
        if (gsym.isImplicit && !(gsym.isParameter && gsym.owner.isMethod)) pmods += p.Mod.Implicit()
        if (gsym.isFinal) pmods += p.Mod.Final()
        if (gsym.isSealed) pmods += p.Mod.Sealed()
        if (gsym.isOverride) pmods += p.Mod.Override()
        if (gsym.isCase) pmods += p.Mod.Case()
        if (gsym.isAbstract && gsym.isClass && !gsym.isTrait) pmods += p.Mod.Abstract()
        if (gsym.isAbstractOverride) { pmods += p.Mod.Abstract(); pmods += p.Mod.Override() }
        if (gsym.isCovariant) pmods += p.Mod.Covariant()
        if (gsym.isContravariant) pmods += p.Mod.Contravariant()
        if (gsym.isLazy) pmods += p.Mod.Lazy()
        // TODO: how do we distinguish `case class C(val x: Int)` and `case class C(x: Int)`?
        val gparamaccessor = gsym.owner.filter(_.isPrimaryConstructor).map(_.owner.info.member(gsym.name))
        val gaccessed = gparamaccessor.map(_.owner.info.member(gparamaccessor.localName))
        if (gaccessed != g.NoSymbol && gaccessed.isMutable) pmods += p.Mod.VarParam()
        if (gaccessed != g.NoSymbol && !gaccessed.owner.isCase && !gaccessed.isMutable) pmods += p.Mod.ValParam()
        if (gsym.isPackageObject) pmods += p.Mod.Package()
        pmods.toList
      }
      def pvparamtpe(gtpt: g.Tree): p.Param.Type = {
        def unwrap(ptpe: p.Type): p.Type = ptpe.asInstanceOf[p.Type.Apply].args.head
        if (g.definitions.isRepeatedParamType(gtpt.tpe)) p.Param.Type.Repeated(unwrap(gtpt.cvt_! : p.Type))
        else if (g.definitions.isByNameParamType(gtpt.tpe)) p.Param.Type.ByName(unwrap(gtpt.cvt_! : p.Type))
        else (gtpt.cvt_! : p.Type)
      }
      def ptparambounds(gtpt: g.Tree): p.Aux.TypeBounds = gtpt match {
        case g.TypeBoundsTree(glo, ghi) =>
          (glo.isEmpty, ghi.isEmpty) match {
            case (false, false) => p.Aux.TypeBounds(lo = glo.cvt_! : p.Type, hi = ghi.cvt_! : p.Type)
            case (true, false) => p.Aux.TypeBounds(hi = ghi.cvt_! : p.Type)
            case (false, true) => p.Aux.TypeBounds(lo = glo.cvt_! : p.Type)
            case (true, true) => p.Aux.TypeBounds()
          }
        case _ =>
          unreachable
      }
      def parg(garg: g.Tree): p.Arg = garg match {
        case g.Typed(expr, g.Ident(g.tpnme.WILDCARD_STAR)) => p.Arg.Repeated(expr.cvt_!)
        case g.AssignOrNamedArg(lhs, rhs) => p.Arg.Named(lhs.cvt_!, rhs.cvt_!)
        case _ => garg.cvt_! : p.Term
      }
      def pargs(gargs: List[g.Tree]): Seq[p.Arg] = gargs.map(parg)
      def pargss(gargss: List[List[g.Tree]]): Seq[Seq[p.Arg]] = gargss.map(pargs)
      def pstats[T <: p.Stmt : ClassTag](gparent: g.Tree, gstats: List[g.Tree]): List[T] = {
        object PatDefCore {
          def unapply(tree: g.MemberDef): Option[(g.Tree, g.Tree)] = tree match {
            // NOTE: we can't trust annot.tpe.typeSymbol, because annot.tpe is bugged
            // for example, @unchecked in `List[Int] @unchecked` has type `List[Int] @unchecked` instead of the correct `unchecked`
            // see https://github.com/scala/scala/blob/73fb460c1cd20ee97556ec0867d17efaa795d129/src/compiler/scala/tools/nsc/typechecker/Typers.scala#L4048
            case in @ g.ValDef(_, _, tpt, g.Match(g.Annotated(annot @ g.treeInfo.Applied(core, _, _), rhs), List(g.CaseDef(pat, g.EmptyTree, _))))
            if core.tpe.finalResultType.typeSymbol == g.symbolOf[unchecked] =>
              require(tpt == g.EmptyTree && rhs.nonEmpty)
              Some((pat, rhs))
            // NOTE: funnily enough, in a very degenerate case, we can also ends up with a def with the same structure
            // try `locally { implicit lazy val List() = List() }` if you care to know more
            case in @ g.DefDef(_, _, Nil, Nil, tpt, g.Match(g.Annotated(annot @ g.treeInfo.Applied(core, _, _), rhs), List(g.CaseDef(pat, g.EmptyTree, _))))
            if core.tpe.finalResultType.typeSymbol == g.symbolOf[unchecked] =>
              require(tpt == g.EmptyTree && rhs.nonEmpty)
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
        val result = mutable.ListBuffer[p.Stmt]()
        var i = 0
        while (i < gstats.length) {
          val gstat = gstats(i)
          i += 1
          val pstat = gstat match {
            case in @ PatDefCore(pat, rhs) =>
              if (in.symbol.isSynthetic && in.symbol.isArtifact) i += g.definitions.TupleClass.seq.indexOf(in.symbol.info.typeSymbol) + 1
              val modbearer = gstats(i - 1).asInstanceOf[g.MemberDef] // TODO: mods for nullary patterns get irrevocably lost (`implicit val List() = List()`)
              if (!modbearer.symbol.isMutable) p.Defn.Val(pmods(modbearer), List(pat.cvt_! : p.Pat), None, rhs.cvt_!)
              else p.Defn.Var(pmods(modbearer), List(pat.cvt_! : p.Pat), None, Some[p.Term](rhs.cvt_!))
            case in @ AnonymousClassDef(templ) =>
              i += 1
              val q"new $$anon()" = gstats(i - 1)
              p.Term.New(templ.cvt_!)
            case in @ RightAssociativeApplicationLhsTemporaryVal(left) =>
              i += 1
              val g.treeInfo.Applied(op @ g.Select(right, _), targs, List(List(g.Ident(_)))) = gstats(i - 1)
              p.Term.ApplyInfix(left.cvt_!, op.symbol.asTerm.precvt(right.tpe, op), targs.cvt_!, List(right.cvt_!))
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
              in.cvt_! : p.Stmt
          }
          result += pstat
        }
        require(result.forall(pstat => classTag[T].runtimeClass.isAssignableFrom(pstat.getClass)))
        result.toList.asInstanceOf[List[T]]
      }
    }

    import Helpers._
    val TermQuote = "denied" // TODO: find a better approach
    in.asInstanceOf[g.Tree].ensureAttributed()
    in match {
      case g.EmptyTree =>
        unreachable
      case g.UnmappableTree =>
        unreachable
      case in @ g.PackageDef(pid, stats) if pt <:< typeOf[p.Pkg] =>
        require(pid.name != g.nme.EMPTY_PACKAGE_NAME)
        val hasBraces = stats.collect{ case pdef: g.PackageDef => pdef }.length > 1
        p.Pkg(pid.cvt_!, pstats[p.Stmt.TopLevel](in, stats), hasBraces = hasBraces) // TODO: infer hasBraces
      case in @ g.PackageDef(pid, stats) if pt <:< typeOf[p.Aux.CompUnit] =>
        val hasBraces = stats.collect{ case pdef: g.PackageDef => pdef }.length > 1
        if (pid.name == g.nme.EMPTY_PACKAGE_NAME) p.Aux.CompUnit(pstats[p.Stmt.TopLevel](in, stats))
        else p.Aux.CompUnit(List(p.Pkg(pid.cvt_!, pstats[p.Stmt.TopLevel](in, stats), hasBraces = hasBraces))) // TODO: infer hasBraces
      case in @ g.ClassDef(_, _, tparams, templ) =>
        require(in.symbol.isClass)
        if (in.symbol.isTrait) {
          p.Defn.Trait(pmods(in), in.symbol.asClass.rawcvt(in), tparams.cvt, templ.cvt)
        } else {
          val gctor = templ.body.find(_.symbol == in.symbol.primaryConstructor).get.asInstanceOf[g.DefDef]
          val q"$_ def $_[..$_](...$impreciseExplicitss)(implicit ..$implicits): $_ = $_" = gctor
          // TODO: discern `class C` and `class C()`
          val explicitss = if (impreciseExplicitss.flatten.isEmpty) List() else impreciseExplicitss
          val ctor = p.Ctor.Primary(pmods(gctor), explicitss.cvt_!, implicits.cvt_!).appendScratchpad(in.symbol.primaryConstructor)
          p.Defn.Class(pmods(in), in.symbol.asClass.rawcvt(in), tparams.cvt, ctor, templ.cvt)
        }
      case in @ g.ModuleDef(_, _, templ) =>
        require(in.symbol.isModule && !in.symbol.isModuleClass)
        p.Defn.Object(pmods(in), in.symbol.asModule.rawcvt(in), templ.cvt)
      case in @ g.ValDef(_, _, tpt, rhs) if pt <:< typeOf[p.Param] =>
        require(in.symbol.isTerm)
        val ptpe = if (tpt.nonEmpty) Some[p.Param.Type](pvparamtpe(tpt)) else None
        val pdefault = if (rhs.nonEmpty) Some[p.Term](rhs.cvt_!) else None
        require(in.symbol.isAnonymous ==> pdefault.isEmpty)
        if (in.symbol.isAnonymous) p.Param.Anonymous(pmods(in), ptpe)
        else p.Param.Named(pmods(in), in.symbol.asTerm.rawcvt(in), ptpe, pdefault)
      case in @ g.ValDef(_, _, tpt, rhs) if pt <:< typeOf[p.Aux.Self] =>
        require(rhs.isEmpty)
        if (in == g.noSelfType) p.Aux.Self(None, None, hasThis = false)
        else {
          require(in.symbol.isTerm)
          val pname = if (!in.symbol.isAnonymous) Some(in.symbol.asTerm.rawcvt(in)) else None
          val ptpe = if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None
          p.Aux.Self(pname, ptpe, hasThis = false) // TODO: figure out hasThis
        }
      case in @ g.ValDef(_, _, tpt, rhs) if pt <:< typeOf[p.Stmt] =>
        // TODO: collapse desugared representations of pattern-based vals and vars
        require(in.symbol.isTerm)
        require(in.symbol.isDeferred ==> rhs.isEmpty)
        require(in.symbol.hasFlag(g.Flag.DEFAULTINIT) ==> rhs.isEmpty)
        (in.symbol.isDeferred, in.symbol.isMutable) match {
          case (true, false) => p.Decl.Val(pmods(in), List(in.symbol.asTerm.rawcvt(in)), tpt.cvt_!)
          case (true, true) => p.Decl.Var(pmods(in), List(in.symbol.asTerm.rawcvt(in)), tpt.cvt_!)
          case (false, false) => p.Defn.Val(pmods(in), List(in.symbol.asTerm.rawcvt(in)), if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None, rhs.cvt_!)
          case (false, true) => p.Defn.Var(pmods(in), List(in.symbol.asTerm.rawcvt(in)), if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None, if (rhs.nonEmpty) Some[p.Term](rhs.cvt_!) else None)
        }
      case in @ g.DefDef(_, _, _, _, _, _) =>
        // TODO: figure out procedures
        require(in.symbol.isMethod)
        val q"$_ def $_[..$tparams](...$explicitss)(implicit ..$implicits): $tpt = $body" = in
        require(in.symbol.isDeferred ==> body.isEmpty)
        if (in.symbol.isConstructor) {
          require(!in.symbol.isPrimaryConstructor)
          val q"{ $_(...$argss); ..$stats; () }" = body
          p.Ctor.Secondary(pmods(in), explicitss.cvt_!, implicits.cvt_!, pargss(argss), pstats[p.Stmt.Block](in, stats))
        } else if (in.symbol.isMacro) {
          require(tpt.nonEmpty) // TODO: support pre-2.12 macros with inferred return types
          p.Defn.Macro(pmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, explicitss.cvt_!, implicits.cvt_!, tpt.cvt_!, body.cvt_!)
        } else if (in.symbol.isDeferred) {
          p.Decl.Def(pmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, explicitss.cvt_!, implicits.cvt_!, tpt.cvt_!) // TODO: infer procedures
        } else {
          p.Defn.Def(pmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, explicitss.cvt_!, implicits.cvt_!, if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None, body.cvt_!)
        }
      case in @ g.TypeDef(_, _, tparams, tpt) if pt <:< typeOf[p.TypeParam] =>
        // TODO: undo desugarings of context and view bounds
        require(in.symbol.isType)
        if (in.symbol.isAnonymous) p.TypeParam.Anonymous(pmods(in), tparams.cvt, Nil, Nil, ptparambounds(tpt))
        else p.TypeParam.Named(pmods(in), in.symbol.asType.rawcvt(in), tparams.cvt, Nil, Nil, ptparambounds(tpt))
      case in @ g.TypeDef(_, _, tparams, tpt) if pt <:< typeOf[p.Member] =>
        require(in.symbol.isType)
        if (in.symbol.isDeferred) p.Decl.Type(pmods(in), in.symbol.asType.rawcvt(in), tparams.cvt, ptparambounds(tpt))
        else p.Defn.Type(pmods(in), in.symbol.asType.rawcvt(in), tparams.cvt, tpt.cvt_!)
      case in @ g.LabelDef(name, params, rhs) =>
        require(params.isEmpty)
        rhs match {
          case q"if ($cond) { $body; $cont } else ()" if name.startsWith(g.nme.WHILE_PREFIX) => p.Term.While(cond.cvt_!, body.cvt_!)
          case q"$body; if ($cond) $cont else ()" if name.startsWith(g.nme.DO_WHILE_PREFIX) => p.Term.Do(body.cvt_!, cond.cvt_!)
          case _ => unreachable
        }
      case g.Import(expr, selectors) =>
        // TODO: collapse desugared chains of imports
        // TODO: distinguish `import foo.x` from `import foo.{x => x}`
        p.Import(List(p.Import.Clause(expr.cvt_!, selectors.map(selector => {
          def resolveImport(source: String, alias: String): p.Import.Name = {
            val imported = g.TermName(source).bothNames.map(source => expr.tpe.nonLocalMember(source))
            imported.importcvt(g.Ident(g.TermName(alias)))
          }
          selector match {
            case g.ImportSelector(g.nme.WILDCARD, _, null, _) =>
              p.Import.Wildcard()
            case g.ImportSelector(name1, _, name2, _) if name1 == name2 =>
              resolveImport(name1.toString, name1.toString)
            case g.ImportSelector(name1, _, name2, _) if name1 != name2 =>
              p.Import.Rename(resolveImport(name1.toString, name1.toString), resolveImport(name1.toString, name2.toString))
            case g.ImportSelector(name, _, g.nme.WILDCARD, _) =>
              p.Import.Unimport(resolveImport(name.toString, name.toString))
          }
        }))))
      case in @ g.Template(_, _, _) =>
        // TODO: really infer hasStats
        // TODO: we should be able to write Template instantiations without an `if` by having something like `hasStats` as an optional synthetic parameter
        val SyntacticTemplate(gparents, gself, gearlydefns, gstats) = in
        val pparents = gparents.map(gparent => { val applied = g.treeInfo.dissectApplied(gparent); p.Aux.Parent(applied.callee.cvt_!, pargss(applied.argss)) })
        if (gstats.isEmpty) p.Aux.Template(gearlydefns.cvt_!, pparents, gself.cvt)
        else p.Aux.Template(gearlydefns.cvt_!, pparents, gself.cvt, pstats[p.Stmt.Template](in, gstats))
      case in: g.Block =>
        // NOTE: if a block with non-trivial stats has been collapsed to a single stat
        // then this means that it's a desugaring of a language construct (e.g. `new C{}` or `1 :: Nil`)
        // and it shouldn't be a p.Term.Block, but rather a resulting stat instead
        val gstats = in.stats :+ in.expr
        (gstats, Helpers.pstats[p.Stmt.Block](in, gstats)) match {
          case ((nel @ _ :+ _) :+ gexpr, Nil :+ pstat) => pstat.asInstanceOf[p.Term]
          case (_, pstats) => p.Term.Block(pstats)
        }
      case in @ g.CaseDef(pat, guard, q"..$stats") =>
        p.Aux.Case(pat.cvt_!, if (guard.nonEmpty) Some[p.Term](guard.cvt_!) else None, pstats[p.Stmt.Block](in, stats))
      case g.Alternative(fst :: snd :: Nil) =>
        p.Pat.Alternative(fst.cvt_!, snd.cvt_!)
      case in @ g.Alternative(hd :: rest) =>
        p.Pat.Alternative(hd.cvt_!, g.Alternative(rest).setType(in.tpe).asInstanceOf[g.Alternative].cvt)
      case g.Ident(g.nme.WILDCARD) =>
        p.Pat.Wildcard()
      case g.Star(g.Ident(g.nme.WILDCARD)) =>
        p.Pat.SeqWildcard()
      case in @ g.Bind(_, g.Ident(g.nme.WILDCARD)) =>
        // TODO: discern `case x => ...` and `case x @ _ => ...`
        require(in.symbol.isTerm)
        in.symbol.asTerm.rawcvt(in)
      case in @ g.Bind(_, g.EmptyTree) =>
        require(in.symbol.isType)
        in.symbol.asType.rawcvt(in)
      case in @ g.Bind(_, g.Typed(g.Ident(g.nme.WILDCARD), tpt)) =>
        require(in.symbol.isTerm)
        p.Pat.Typed(in.symbol.asTerm.rawcvt(in), tpt.cvt_!)
      case in @ g.Bind(name, tree) =>
        require(in.symbol.isTerm)
        require(name == in.symbol.name)
        p.Pat.Bind(in.symbol.asTerm.rawcvt(in), tree.cvt_!)
      case g.Function(params, body) =>
        // TODO: need to be careful to discern `_ + 2` and `_ => 2`
        // because in both cases parameter names are `x$...`
        val isShorthand = params.exists(_.symbol.isAnonymous) && body.exists(tree => params.exists(_.symbol == tree.symbol))
        if (isShorthand) (body.cvt_! : p.Term) else p.Term.Function(params.cvt, body.cvt_!)
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
            p.Term.ApplyInfix(lhs.cvt_!, p.Term.Name(op.decodedName.toString, isBackquoted = false), Nil, args.cvt_!)
          case _ =>
            p.Term.Assign(lhs.cvt_!, rhs.cvt_!)
        }
      case g.AssignOrNamedArg(lhs, rhs) =>
        // NOTE: handled in parg/pargs/pargss
        unreachable
      case g.If(cond, thenp, g.Literal(g.Constant(()))) =>
        // TODO: figure out hasElse with definitive precision
        p.Term.If(cond.cvt_!, thenp.cvt_!)
      case g.If(cond, thenp, elsep) =>
        p.Term.If(cond.cvt_!, thenp.cvt_!, elsep.cvt_!)
      case g.Match(selector, cases) =>
        // TODO: it's cute that Term.Cases is a Term, but what tpe shall we return for it? :)
        if (selector == g.EmptyTree) p.Term.Cases(cases.cvt)
        else p.Term.Match(selector.cvt_!, p.Term.Cases(cases.cvt))
      case g.Return(expr) =>
        // TODO: figure out hasExpr
        p.Term.Return(expr.cvt_!)
      case g.Try(body, catches, finalizer) =>
        object CatchExpr {
          def unapply(tree: g.Tree): Option[g.Tree] = tree match {
            case g.CaseDef(pq"$scrutdef: Throwable", g.EmptyTree, g.Block(List(g.ValDef(_, tempdef, _, handler)), _))
            if scrutdef.startsWith(g.nme.FRESH_TERM_NAME_PREFIX) && tempdef.startsWith("catchExpr") => Some(handler)
            case _ => None
          }
        }
        val catchp = catches match { case CatchExpr(handler) :: Nil => Some[p.Term](handler.cvt_!); case Nil => None; case _ => Some(p.Term.Cases(catches.cvt)) }
        val finallyp = if (finalizer.nonEmpty) Some[p.Term](finalizer.cvt_!) else None
        p.Term.Try(body.cvt_!, catchp, finallyp)
      case g.Throw(expr) =>
        p.Term.Throw(expr.cvt_!)
      case g.New(_) =>
        unreachable
      case g.Typed(expr, g.Function(Nil, g.EmptyTree)) if pt <:< typeOf[p.Term] =>
        p.Term.Eta(expr.cvt_!)
      case g.Typed(expr, tpt) if pt <:< typeOf[p.Term] =>
        p.Term.Ascribe(expr.cvt_!, tpt.cvt_!)
      case g.Typed(expr, tpt) if pt <:< typeOf[p.Pat] =>
        p.Pat.Typed(expr.cvt_!, tpt.cvt_!)
      case in @ g.TypeApply(fn, targs) =>
        p.Term.ApplyType(fn.cvt_!, targs.cvt_!)
      case in @ g.Apply(fn, args) if pt <:< typeOf[p.Term] =>
        // TODO: undo the for desugaring
        // TODO: figure out whether the programmer actually wrote the interpolator or they were explicitly using a desugaring
        // TODO: figure out whether the programmer actually wrote the infix application or they were calling a symbolic method using a dot
        // TODO: infer the difference between `new X` vs `new X()`
        // TODO: figure out whether the programmer actually used the tuple syntax or they were calling the tuple companion explicitly
        // TODO: figure out whether the programmer actually wrote `foo(...) = ...` or it was `foo.update(..., ...)`
        // TODO: figure out whether the programmer actually wrote `'foo` or it was 'Symbol("foo")'
        in match {
          case q"new $_(...$argss0)" =>
            val g.treeInfo.Applied(g.Select(g.New(tpt), _), _, _) = in
            val argss = if (argss0.isEmpty && in.symbol.info.paramss.flatten.nonEmpty) List(List()) else argss0
            val supercall = p.Aux.Parent(tpt.cvt_!, pargss(argss)).appendScratchpad(in)
            val self = p.Aux.Self(None, None).appendScratchpad(tpt)
            val templ = p.Aux.Template(Nil, List(supercall), self).appendScratchpad(in)
            p.Term.New(templ)
          case q"$stringContext(..$parts).$prefix(..$args)" if stringContext.symbol == g.definitions.StringContextClass.companion =>
            p.Term.Interpolate((fn.cvt_! : p.Term.Select).selector, parts.cvt_!, args.cvt_!)
          case DesugaredSetter(lhs, rhs) =>
            p.Term.Assign(lhs.cvt_!, rhs.cvt_!)
          case DesugaredUpdate(core @ g.Apply(lhs, args), rhs) =>
            p.Term.Update(p.Term.Apply(lhs.cvt_!, args.cvt_!).appendScratchpad(core), rhs.cvt_!)
          case DesugaredOpAssign(g.Select(lhs, op), args) =>
            p.Term.ApplyInfix(lhs.cvt_!, p.Term.Name(op.decodedName.toString, isBackquoted = false), Nil, args.cvt_!)
          case DesugaredSymbolLiteral(value) =>
            p.Lit.Symbol(value) : p.Term
          case q"$lhs.$op($arg)" if op.looksLikeInfix =>
            p.Term.ApplyInfix(lhs.cvt_!, (fn.cvt_! : p.Term.Select).selector, Nil, List(parg(arg)))
          case _ if g.definitions.TupleClass.seq.contains(in.symbol.companion) =>
            p.Term.Tuple(args.cvt_!)
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
        p.Qual.Super((qual.cvt : p.Term.This).qual, if (mix != g.tpnme.EMPTY) Some(in.symbol.asClass.rawcvt(in)) else None)
      case in @ g.This(qual) =>
        require(!in.symbol.isPackageClass)
        p.Term.This(if (qual != g.tpnme.EMPTY) Some(in.symbol.qualcvt(in)) else None)
      case in: g.PostfixSelect =>
        unreachable
      case in @ g.Select(qual, name) =>
        // TODO: discern unary applications via !x and via explicit x.unary_!
        // TODO: also think how to detect unary applications that have implicit arguments
        if (name.isTermName) {
          val pname = in.symbol.asTerm.precvt(qual.tpe, in)
          if (pname.value.startsWith("unary_")) p.Term.ApplyUnary(pname.copy(value = pname.value.stripPrefix("unary_")).appendScratchpad(pname.scratchpad), qual.cvt_!)
          else p.Term.Select(qual.cvt_!, pname, isPostfix = false) // TODO: figure out isPostfix
        } else {
          p.Type.Select(qual.cvt_!, in.symbol.asType.precvt(qual.tpe, in))
        }
      case in @ g.Ident(_) =>
        // NOTE: Ident(<term name>) with a type symbol attached to it
        // is the encoding that the ensugarer uses to denote a self reference
        // also see the ensugarer for more information
        if (in.isTerm && in.symbol.isType) p.Term.Name(in.alias, in.isBackquoted)
        else if (in.isTerm && in.symbol.isAnonymous) p.Term.Placeholder()
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
        val p.Aux.Template(early, parents, self, stats) = templ.cvt
        require(early.isEmpty && parents.forall(_.argss.isEmpty) && self.name.isEmpty && self.decltpe.isEmpty && stats.forall(_.isInstanceOf[p.Stmt.Refine]))
        p.Type.Compound(parents.map(_.tpe), stats.asInstanceOf[Seq[p.Stmt.Refine]])
      case in @ g.AppliedTypeTree(tpt, args) =>
        // TODO: infer whether that was really Apply, Function or Tuple
        if (g.definitions.FunctionClass.seq.contains(tpt.tpe.typeSymbol)) p.Type.Function(args.init.cvt_!, args.last.cvt_!)
        else if (g.definitions.TupleClass.seq.contains(tpt.tpe.typeSymbol)) p.Type.Tuple(args.cvt_!)
        else p.Type.Apply(tpt.cvt_!, args.cvt_!)
      case in @ g.ExistentialTypeTree(tpt, whereClauses) =>
        p.Type.Existential(tpt.cvt_!, whereClauses.cvt_!)
      case in @ g.SelectFromTypeTree(qual, name) =>
        p.Type.Project(qual.cvt_!, in.symbol.asType.precvt(qual.tpe, in))
      case in @ g.TypeBoundsTree(_, _) =>
        unreachable
      // NOTE: this derivation is ambiguous because of at least g.ValDef, g.TypeDef and g.Typed
      // case _: g.Tree =>
      //   derive
    }
  }
}

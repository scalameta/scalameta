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

class Host[G <: ScalaGlobal](val g: G) extends PalladiumHost with Metadata with Ensugar {
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
  // NOTE: can't use MemberDef.mods, because they get their annotations erased and moved to Symbol.annotations during typechecking
  // NOTE: can't convert symbols, because that's quite unsafe: a lot of symbols don't make sense without prefixes
  // NOTE: careful use of NameTree.name, because it can lie (renaming imports) and it doesn't have enough semantic information (unlike the underlying symbol)
  // TODO: remember positions. actually, in scalac they are almost accurate, so it would be a shame to discard them
  @converter def toPalladium(in: Any, pt: Pt): Any = {
    object Helpers extends g.ReificationSupportImpl { self =>
      // implicit class RichPalladiumTree[T <: p.Tree](val ptree: T) {
      //   def appendScratchpad(x: Any): ptree.ThisType = ptree.mapScratchpad(_.map({
      //     case xs: List[_] => xs :+ x
      //     case y => sys.error(s"unexpected scratchpad $y for $ptree")
      //   }).getOrElse(Nil))
      // }
      object SyntacticTemplate {
        def unapply(templ: g.Template): Option[(List[g.Tree], List[g.Tree], g.ValDef, List[g.Tree])] = {
          self.UnMkTemplate.unapply(templ).map { case (parents, self, ctorMods, pvparamss, earlydefns, stats) => (earlydefns, parents, self, stats) }
        }
      }
      implicit class RichHelperType(tpe: g.Type) {
        def depoly: g.Type = tpe match {
          case g.PolyType(_, tpe) => tpe.depoly
          case _ => tpe
        }
      }
      def alias(in: g.Tree): String = in match {
        case in: g.NameTree => in.name.decodedName.toString
        case g.This(name) => name.decodedName.toString
      }
      def isBackquoted(in: g.Tree): Boolean = in match {
        // TODO: infer isBackquoted
        // TODO: iirc according to Denys, info in BackquotedIdentifierAttachment might be incomplete
        case in: g.Ident => in.isBackquoted || scala.meta.syntactic.parsers.keywords.contains(in.name.toString)
        case in: g.Select => scala.meta.syntactic.parsers.keywords.contains(in.name.toString)
        case _ => false
      }
      implicit class RichHelperSymbol(gsym: g.Symbol) {
        type pTermOrTypeName = p.Name{type ThisType >: p.Term.Name with p.Type.Name <: p.Name}
        def precvt(pre: g.Type, in: g.Tree): pTermOrTypeName = {
          gsym.rawcvt(in).appendScratchpad(pre).asInstanceOf[pTermOrTypeName]
        }
        def rawcvt(in: g.Tree): pTermOrTypeName = {
          (if (gsym.isTerm) p.Term.Name(alias(in), isBackquoted(in)).appendScratchpad(gsym)
          else if (gsym.isType) p.Type.Name(alias(in), isBackquoted(in)).appendScratchpad(gsym)
          else unreachable).asInstanceOf[pTermOrTypeName]
        }
        def qualcvt(in: g.Tree): p.Qual.Name = {
          require(gsym != g.NoSymbol)
          val gsyms = {
            if (gsym.isModuleClass) List(gsym.sourceModule.asModule, g.NoSymbol)
            else List(g.NoSymbol, gsym.asClass)
          }
          p.Qual.Name(alias(in), isBackquoted(in)).appendScratchpad(gsyms)
        }
      }
      implicit class RichHelperSymbols(gsyms: List[g.Symbol]) {
        def importcvt(in: g.Tree): p.Import.Name = {
          val List(gterm, gtype) = gsyms
          require(gterm != g.NoSymbol || gtype != g.NoSymbol)
          require(gterm != g.NoSymbol ==> gterm.isTerm)
          require(gtype != g.NoSymbol ==> gtype.isType)
          p.Import.Name(alias(in), isBackquoted(in)).appendScratchpad(gsyms)
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
      private def paccessqual(gsym: g.Symbol): Option[p.Mod.AccessQualifier] = {
        if (gsym.isPrivateThis || gsym.isProtectedThis) {
          // TODO: does NoSymbol here actually mean gsym.owner?
          val gpriv = gsym.privateWithin.orElse(gsym.owner)
          require(gpriv.isClass)
          Some(p.Term.This(None).appendScratchpad(gpriv))
        } else if (gsym.privateWithin == g.NoSymbol || gsym.privateWithin == null) None
        else Some(gsym.privateWithin.qualcvt(g.Ident(gsym.privateWithin))) // TODO: this loses information is gsym.privateWithin was brought into scope with a renaming import
      }
      def pmods(gmdef: g.MemberDef): Seq[p.Mod] = {
        val gsym = gmdef.symbol.getterIn(gmdef.symbol.owner).orElse(gmdef.symbol)
        val pmods = scala.collection.mutable.ListBuffer[p.Mod]()
        pmods ++= gmdef.mods.annotations.map{ case q"new $gtpt(...$gargss)" => p.Mod.Annot(gtpt.cvt_!, gargss.cvt_!) }
        if (gsym.isPrivate) pmods += p.Mod.Private(paccessqual(gsym))
        if (gsym.isProtected) pmods += p.Mod.Protected(paccessqual(gsym))
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
        // TODO: how do we distinguish `case class C(val x: Int)` and `case class C(x: Int)`?
        val gparamaccessor = gsym.owner.filter(_.isPrimaryConstructor).map(_.owner.info.member(gsym.name))
        val gaccessed = gparamaccessor.map(_.owner.info.member(gparamaccessor.localName))
        if (gaccessed != g.NoSymbol && gaccessed.isMutable) pmods += p.Mod.VarParam()
        if (gaccessed != g.NoSymbol && !gaccessed.owner.isCase) pmods += p.Mod.ValParam()
        if (gsym.isPackageObject) pmods += p.Mod.Package()
        pmods.toList
      }
      private def pclassof(gtype: g.Type): p.Term.ApplyType = {
        val mothershipCore = g.gen.mkAttributedRef(g.currentRun.runDefinitions.Predef_classOf).asInstanceOf[g.Select]
        val scratchpad = g.TypeApply(mothershipCore, List(g.TypeTree(gtype))).setType(g.appliedType(mothershipCore.tpe, gtype))
        // TODO: track the original TypTree here, so that we don't have to approximate with ptpe
        p.Term.ApplyType(mothershipCore.cvt_!, List(ptpe(gtype))).appendScratchpad(scratchpad)
      }
      type pScalaConst = p.Term{type ThisType >: p.Lit.Null with p.Lit.Unit with p.Lit.Bool with p.Lit.Int with p.Lit.Long with p.Lit.Float with p.Lit.Double with p.Lit.String with p.Lit.Char with p.Term.ApplyType <: p.Term}
      def pconst(gconst: g.Constant): pScalaConst = (gconst.value match {
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
        case v: g.Type => pclassof(v)
        case v: g.Symbol => ??? // TODO: this is a super-crazy corner case that only appears in arguments of java annotations that refer to java enums
      }).asInstanceOf[pScalaConst]
      def unattributedNodes(in: Any): List[(g.Tree, List[g.Tree])] = in match {
        case gtree: g.Tree =>
          val offenders = mutable.ListBuffer[(g.Tree, List[g.Tree])]()
          object traverser extends g.Traverser {
            private var path = List[g.Tree]()
            private def drilldown[T](tree: g.Tree)(op: g.Tree => T): T = try { path ::= tree; op(tree) } finally { path = path.tail }
            private def check(tree: g.Tree, skipSymbol: Boolean = false, skipType: Boolean = false): Unit = {
              val ok = {
                !tree.isErroneous &&
                (tree.canHaveAttrs ==> (skipType || (tree.tpe != null))) &&
                ((tree.canHaveAttrs && tree.hasSymbolField) ==> (skipSymbol || (tree.symbol != null && tree.symbol != g.NoSymbol)))
              }
              if (ok) super.traverse(tree)
              else { offenders += ((tree, path)) }
            }
            override def traverse(tree: g.Tree): Unit = drilldown(tree) {
              // imports and selectors of imports aren't attributed by the typechecker
              case g.Import(qual, selectors) => check(qual)
              // patmat wildcards used in binds and typeds don't have symbols
              case g.Ident(g.nme.WILDCARD) => check(tree, skipSymbol = true)
              // skip verification of Literal(Constant(0))
              // https://groups.google.com/forum/#!topic/scala-internals/gppfuY7-tdA
              case g.Literal(g.Constant(0)) =>
              // literals don't have symbols
              case g.Literal(const) => check(tree, skipSymbol = true)
              // template symbols aren't set when typechecking compound type trees, and they are dummies anyway, so it doesn't really matter
              // https://groups.google.com/forum/#!topic/scala-internals/6ebL7waMav8
              case g.Template(parents, self, stats) => check(tree, skipSymbol = true)
              case _ => check(tree)
            }
          }
          traverser.traverse(gtree)
          offenders.toList
        case _ => Nil
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
      // TODO: now that we plan to restore TypeTrees to their originals, the facility of converting Types is no longer necessary
      // therefore I'm demoting the corresponding plan of the uber-patternmatch to a dedicated method
      // let's see how far we can get with that
      type pScalaType = p.Type{ type ThisType >: p.Type.Name with p.Type.Select with p.Type.Project with p.Type.Singleton with p.Type.Apply with p.Type.Compound with p.Type.Existential with p.Type.Annotate with p.Lit.Bool with p.Lit.Int with p.Lit.Long with p.Lit.Float with p.Lit.Double with p.Lit.String with p.Lit.Char <: p.Type }
      type pScalaLitType = p.Lit{type ThisType >: p.Lit.Bool with p.Lit.Int with p.Lit.Long with p.Lit.Float with p.Lit.Double with p.Lit.String with p.Lit.Char <: p.Lit}
      def ptpe(gtpe: g.Type): pScalaType = {
        def loop(gtpe: g.Type): p.Type = {
          object ValSymbol { def unapply(gsym: g.Symbol): Option[g.TermSymbol] = if (gsym.isTerm && !gsym.isMethod && !gsym.isModule && !gsym.isMutable) Some(gsym.asTerm) else None }
          object VarSymbol { def unapply(gsym: g.Symbol): Option[g.TermSymbol] = if (gsym.isTerm && !gsym.isMethod && !gsym.isModule && gsym.isMutable) Some(gsym.asTerm) else None }
          object DefSymbol { def unapply(gsym: g.Symbol): Option[g.TermSymbol] = if (gsym.isMethod) Some(gsym.asTerm) else None }
          object AbstractTypeSymbol { def unapply(gsym: g.Symbol): Option[g.TypeSymbol] = if (gsym.isType && gsym.isAbstractType) Some(gsym.asType) else None }
          object AliasTypeSymbol { def unapply(gsym: g.Symbol): Option[g.TypeSymbol] = if (gsym.isType && gsym.isAliasType) Some(gsym.asType) else None }
          def pann(gann: g.AnnotationInfo): p.Mod.Annot = {
            val g.AnnotationInfo(gatp, gargs, gassocs) = gann
            // TODO: need an original for AnnotationInfo.atp
            p.Mod.Annot(ptpe(gatp), List(gargs.map(garg => ensugar(garg)).cvt_!))
          }
          def panns(ganns: List[g.AnnotationInfo]): Seq[p.Mod.Annot] = {
            ganns.filter(gann => {
              val sym = gann.tree.tpe.typeSymbol
              def isOldMacroSignature = sym.fullName == "scala.reflect.macros.internal.macroImpl"
              def isNewMacroSignature = isOldMacroSignature
              !isOldMacroSignature && !isNewMacroSignature
            }).map(pann)
          }
          def pmods(gsym0: g.Symbol): Seq[p.Mod] = {
            val gsym = gsym0.getterIn(gsym0.owner).orElse(gsym0)
            val pmods = scala.collection.mutable.ListBuffer[p.Mod]()
            pmods ++= panns(gsym0.annotations)
            if (gsym.isPrivate) pmods += p.Mod.Private(paccessqual(gsym))
            if (gsym.isProtected) pmods += p.Mod.Protected(paccessqual(gsym))
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
            // TODO: how do we distinguish `case class C(val x: Int)` and `case class C(x: Int)`?
            val gparamaccessor = gsym.owner.filter(_.isPrimaryConstructor).map(_.owner.info.member(gsym.name))
            val gaccessed = gparamaccessor.map(_.owner.info.member(gparamaccessor.localName))
            if (gaccessed != g.NoSymbol && gaccessed.isMutable) pmods += p.Mod.VarParam()
            if (gaccessed != g.NoSymbol && !gaccessed.owner.isCase) pmods += p.Mod.ValParam()
            if (gsym.isPackageObject) pmods += p.Mod.Package()
            pmods.toList
          }
          def pbounds(gtpe: g.Type): p.Aux.TypeBounds = gtpe match {
            case g.TypeBounds(glo, ghi) =>
              (glo =:= g.typeOf[Nothing], ghi =:= g.typeOf[Any]) match {
                case (false, false) => p.Aux.TypeBounds(lo = loop(glo), hi = loop(ghi))
                case (true, false) => p.Aux.TypeBounds(hi = loop(ghi))
                case (false, true) => p.Aux.TypeBounds(lo = loop(glo))
                case (true, true) => p.Aux.TypeBounds()
              }
            case _ =>
              unreachable
          }
          def ptparam(gsym: g.Symbol): p.TypeParam = {
            // TODO: undo desugarings of context and view bounds
            require(gsym.isType)
            val isAnonymous = gsym.name == g.tpnme.WILDCARD
            if (isAnonymous) p.TypeParam.Anonymous(pmods(gsym), ptparams(gsym.typeParams), Nil, Nil, pbounds(gsym.info.depoly))
            else p.TypeParam.Named(pmods(gsym), gsym.asType.rawcvt(g.Ident(gsym)), ptparams(gsym.typeParams), Nil, Nil, pbounds(gsym.info.depoly))
          }
          def ptparams(gsyms: List[g.Symbol]): Seq[p.TypeParam] = gsyms.map(ptparam)
          def pvparamtpe(gtpe: g.Type): p.Param.Type = {
            def unwrap(ptpe: p.Type): p.Type = ptpe.asInstanceOf[p.Type.Apply].args.head
            if (g.definitions.isRepeatedParamType(gtpe)) p.Param.Type.Repeated(unwrap(loop(gtpe)))
            else if (g.definitions.isByNameParamType(gtpe)) p.Param.Type.ByName(unwrap(loop(gtpe)))
            else loop(gtpe)
          }
          def pvparam(gsym: g.Symbol): p.Param.Named = {
            require(gsym.isTerm)
            // TODO: discern inferred and explicitly specified vparamtpe
            // TODO: somehow figure out the default argument from a parameter symbol if it is specified
            p.Param.Named(pmods(gsym), gsym.asTerm.rawcvt(g.Ident(gsym)), Some(pvparamtpe(gsym.info.depoly)), None)
          }
          def pvparams(gsyms: List[g.Symbol]): Seq[p.Param.Named] = gsyms.map(pvparam)
          def pvparamss(gsymss: List[List[g.Symbol]]): Seq[Seq[p.Param.Named]] = gsymss.map(pvparams)
          def pexplicitss(gsym: g.Symbol): Seq[Seq[p.Param.Named]] = if (pimplicits(gsym).nonEmpty) pvparamss(gsym.info.paramss).dropRight(1) else pvparamss(gsym.info.paramss)
          def pimplicits(gsym: g.Symbol): Seq[p.Param.Named] = pvparams(gsym.info.paramss.flatten.filter(_.isImplicit))
          gtpe match {
            case g.NoPrefix =>
              unreachable
            case g.NoType =>
              unreachable
            case in @ g.ThisType(sym) =>
              p.Type.Singleton({
                if (sym.isPackageClass) {
                  def moduleRef(gsym: g.Symbol): g.Tree = {
                    def loop(gsym: g.Symbol): g.Tree = {
                      if (gsym.isPackageClass) loop(gsym.asClass.module)
                      else if (gsym.isClass) g.This(gsym).setType(gsym.tpe)
                      else {
                        // TODO: figure out what to do about _root_ and _empty_
                        val isIdent = gsym.owner == g.NoSymbol || gsym.owner == g.rootMirror.RootClass || gsym.owner == g.rootMirror.EmptyPackageClass
                        if (isIdent) g.Ident(gsym).setType(gsym.tpe)
                        else g.Select(loop(gsym.owner), gsym).setType(gsym.tpe)
                      }
                    }
                    loop(gsym)
                  }
                  val isIdent = sym.owner == g.NoSymbol || sym.owner == g.rootMirror.RootClass || sym.owner == g.rootMirror.EmptyPackageClass
                  if (isIdent) moduleRef(sym).asInstanceOf[g.Ident].cvt_! : p.Term.Name
                  else moduleRef(sym).asInstanceOf[g.Select].cvt_! : p.Term.Select
                } else {
                  // TODO: infer whether thistpe originally corresponded to Some or None
                  p.Term.This(None)
                }
              }.appendScratchpad(sym))
            case g.SuperType(thistpe, supertpe) =>
              // TODO: infer whether supertpe originally corresponded to Some or None
              val p.Type.Singleton(p.Term.This(pthis)) = loop(thistpe)
              require(supertpe.typeSymbol.isType)
              val supersym = supertpe.typeSymbol.asType
              p.Type.Singleton(p.Qual.Super(pthis, Some(supersym.rawcvt(g.Ident(supersym)).appendScratchpad(in))))
            case g.SingleType(pre, sym) =>
              // TODO: this loses information if sym was brought into scope with a renaming import
              require(sym.isTerm)
              val ref = (pre match {
                case g.NoPrefix =>
                  sym.asTerm.rawcvt(g.Ident(sym))
                case _: g.SingletonType =>
                  // TODO: what do we do if sym is a package object? do we skip it altogether or do we still emit an explicit reference to it?
                  val p.Type.Singleton(preref) = loop(pre)
                  p.Term.Select(preref, sym.asTerm.precvt(pre, g.Ident(sym)), isPostfix = false) // TODO: figure out isPostfix
                case _ =>
                  unreachable
              }).appendScratchpad(in)
              p.Type.Singleton(ref)
            case g.ConstantType(const) =>
              pconst(const) match {
                case lit: p.Lit => lit.asInstanceOf[pScalaLitType]
                // TODO: can Literal(Constant(_: Type)) or Literal(Constant(_: Symbol)) ever end up in patterns?
                case _ => unreachable
              }
            case g.TypeRef(pre, sym, args) =>
              // TODO: this loses information if sym was brought into scope with a renaming import
              require(sym.isType)
              val ref = pre match {
                case g.NoPrefix =>
                  sym.asType.rawcvt(g.Ident(sym))
                case _: g.SingletonType =>
                  val p.Type.Singleton(preref) = loop(pre)
                  p.Type.Select(preref, sym.asType.precvt(pre, g.Ident(sym))).appendScratchpad(in)
                case _ =>
                  p.Type.Project(loop(pre), sym.asType.precvt(pre, g.Ident(sym))).appendScratchpad(in)
              }
              // TODO: infer whether that was Apply, Function or Tuple
              // TODO: discern Apply and ApplyInfix
              if (args.isEmpty) ref
              else p.Type.Apply(ref, args.map(loop))
            case g.RefinedType(parents, decls) =>
              val pstmts: Seq[p.Stmt.Refine] = decls.sorted.toList.map({
                case ValSymbol(sym) => p.Decl.Val(pmods(sym), List(sym.rawcvt(g.ValDef(sym))), loop(sym.info.depoly))
                case VarSymbol(sym) if !sym.isMethod && !sym.isModule && sym.isMutable => p.Decl.Var(pmods(sym), List(sym.rawcvt(g.ValDef(sym))), loop(sym.info.depoly))
                // TODO: infer the difference between Defs and Procedures
                case DefSymbol(sym) => p.Decl.Def(pmods(sym), sym.rawcvt(g.DefDef(sym, g.EmptyTree)), ptparams(sym.typeParams), pexplicitss(sym), pimplicits(sym), loop(sym.info.finalResultType))
                case AbstractTypeSymbol(sym) => p.Decl.Type(pmods(sym), sym.rawcvt(g.TypeDef(sym)), ptparams(sym.typeParams), pbounds(sym.info.depoly))
                case AliasTypeSymbol(sym) => p.Defn.Type(pmods(sym), sym.rawcvt(g.TypeDef(sym, g.TypeTree(sym.info))), ptparams(sym.typeParams), loop(sym.info.depoly))
              })
              p.Type.Compound(parents.map(loop), pstmts) // TODO: infer hasRefinement
            case g.ExistentialType(quantified, underlying) =>
              // TODO: infer type placeholders where they were specified explicitly
              val pstmts: Seq[p.Stmt.Existential] = quantified.map({
                case ValSymbol(sym) => p.Decl.Val(pmods(sym), List(sym.rawcvt(g.ValDef(sym))), loop(sym.info.depoly))
                case AbstractTypeSymbol(sym) => p.Decl.Type(pmods(sym), sym.rawcvt(g.TypeDef(sym)), ptparams(sym.typeParams), pbounds(sym.info.depoly))
              })
              p.Type.Existential(loop(underlying), pstmts)
            case g.AnnotatedType(anns, underlying) =>
              p.Type.Annotate(loop(underlying), panns(anns))
            case g.TypeBounds(lo, hi) =>
              unreachable
            // NOTE: these types have no equivalent in Palladium
            // e.g. q"List.apply".tpe is not some mysterious MethodType, but is an error, because List.apply is actually not a well-formed term
            // if one wants to analyze q"List.apply" in e.g. q"List.apply(2)", then it's possible to do Term.defn
            case g.ClassInfoType(_, _, _) =>
              unreachable
            case g.MethodType(_, _) =>
              unreachable
            case g.NullaryMethodType(_) =>
              unreachable
            case g.PolyType(_, _) =>
              unreachable
            // NOTE: the types below are only used internally by the typechecker
            // as far as I understand, it's impossible for them to linger past typer
            case g.WildcardType =>
              unreachable
            case g.BoundedWildcardType(_) =>
              unreachable
            case in: g.UniqueErasedValueType =>
              unreachable
            case in: g.RepeatedType =>
              unreachable
            case in: g.NamedType =>
              unreachable
            case in: g.AppliedTypeVar =>
              unreachable
            case in: g.HKTypeVar =>
              unreachable
            case in: g.AntiPolyType =>
              unreachable
            case in: g.ImportType =>
              unreachable
            case in: g.OverloadedType =>
              unreachable
            case g.ErrorType =>
              unreachable
          }
        }
        loop(gtpe).asInstanceOf[pScalaType]
      }
    }
    import Helpers._
    val offenders = unattributedNodes(in)
    if (offenders.nonEmpty) {
      val grouped = offenders.groupBy(_._1.productPrefix).toList.sortBy(_._1)
      def commaCommaAnd[T](list: List[T]): String = list.init.mkString(", ") + (if (list.length == 1) "" else " and ") + list.last
      // The problem is caused by 6 Idents that are either unattributed or erroneous:
      val offenderSummary = commaCommaAnd(grouped.map{ case (k, v) => s"${v.length} $k${if (v.length == 1) "" else "s"}" })
      // 1: _ (... CaseDef > Bind > UnApply > Typed > Ident)
      // 2: _ (...  DefDef > Match > CaseDef > Bind > Ident)
      // ...
      val offenderPrintout = grouped.flatMap(_._2).map({ case (tree, path) =>
        var prefix = tree.toString.replace("\n", " ")
        if (prefix.length > 60) prefix = prefix.take(60) + "..."
        val suffix = path.scanLeft("")((suffix, tree) => {
          if (suffix.length == 0) tree.productPrefix
          else tree.productPrefix + " > " + suffix
        }) match {
          case _ :+ last if last.length <= 40 => last
          case list => "... " + list.dropWhile(_.length <= 40).head.takeRight(40)
        }
        s"$prefix ($suffix)"
      }).zipWithIndex.map{ case (s, i) => s"${i + 1}: $s" }.mkString("\n")
      sys.error(s"""
        |Input Scala tree is not fully attributed and can't be converted to a Palladium tree.
        |The problem is caused by $offenderSummary that ${if (offenders.length == 1) "is" else "are"} either unattributed or erroneous:
        |$offenderPrintout
        |The input tree that has caused problems to the converter is printed out below:
        |(Note that showRaw output at the end of the printout is supposed to contain ids and types)
        |$in
        |${g.showRaw(in, printIds = true, printTypes = true)}
      """.stripMargin)
    }
    val TermQuote = "denied" // TODO: find a better approach
    in match {
      case g.EmptyTree =>
        unreachable
      case g.UnmappableTree =>
        unreachable
      case g.PackageDef(pid, stats) if pt <:< typeOf[p.Pkg] =>
        require(pid.name != g.nme.EMPTY)
        p.Pkg(pid.cvt_!, stats.cvt_!, hasBraces = true) // TODO: infer hasBraces
      case g.PackageDef(pid, stats) if pt <:< typeOf[p.Aux.CompUnit] =>
        if (pid.name != g.nme.EMPTY) p.Aux.CompUnit(stats.cvt_!)
        else p.Aux.CompUnit(List(p.Pkg(pid.cvt_!, stats.cvt_!, hasBraces = true))) // TODO: infer hasBraces
      case in @ g.ClassDef(_, _, tparams, templ) =>
        require(in.symbol.isClass)
        in match {
          case q"$_ class $_[..$_] $_(...$_)(implicit ..$_) extends { ..$_ } with ..$_ { $_ => ..$_ }" =>
            val gctor = templ.body.find(_.symbol == in.symbol.primaryConstructor).get.asInstanceOf[g.DefDef]
            val q"$_ def $_[..$_](...$impreciseExplicitss)(implicit ..$implicits): $_ = $_" = gctor
            // TODO: discern `class C` and `class C()`
            // TODO: recover named/default parameters
            val explicitss = if (impreciseExplicitss.flatten.isEmpty) List() else impreciseExplicitss
            val ctor = p.Ctor.Primary(pmods(gctor), explicitss.cvt_!, implicits.cvt_!).appendScratchpad(in.symbol.primaryConstructor)
            p.Defn.Class(pmods(in), in.symbol.asClass.rawcvt(in), tparams.cvt, ctor, templ.cvt)
          case q"$_ trait $_[..$_] extends { ..$_ } with ..$_ { $_ => ..$_ }" =>
            p.Defn.Trait(pmods(in), in.symbol.asClass.rawcvt(in), tparams.cvt, templ.cvt)
        }
      case in @ g.ModuleDef(_, _, templ) =>
        require(in.symbol.isModule && !in.symbol.isModuleClass)
        p.Defn.Object(pmods(in), in.symbol.asModule.rawcvt(in), templ.cvt)
      case in @ g.ValDef(_, _, tpt, rhs) if pt <:< typeOf[p.Param] =>
        require(in.symbol.isTerm)
        val isAnonymous = in.symbol.name.toString.startsWith("x$")
        val ptpe = if (tpt.nonEmpty) Some[p.Param.Type](pvparamtpe(tpt)) else None
        val pdefault = if (rhs.nonEmpty) Some[p.Term](rhs.cvt_!) else None
        require(isAnonymous ==> pdefault.isEmpty)
        if (isAnonymous) p.Param.Anonymous(pmods(in), ptpe)
        else p.Param.Named(pmods(in), in.symbol.asTerm.rawcvt(in), ptpe, pdefault)
      case in @ g.ValDef(_, _, tpt, rhs) if pt <:< typeOf[p.Aux.Self] =>
        require(rhs.isEmpty)
        if (in == g.noSelfType) p.Aux.Self(None, None, hasThis = false)
        else {
          require(in.symbol.isTerm)
          val pname = if (in.symbol.name.toString != "x$1") Some(in.symbol.asTerm.rawcvt(in)) else None
          val ptpe = if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None
          p.Aux.Self(pname, ptpe, hasThis = false) // TODO: figure out hasThis
        }
      case in @ g.ValDef(_, _, tpt, rhs) if pt <:< typeOf[p.Member.ValOrVar] =>
        // TODO: collapse desugared representations of pattern-based vals and vars
        // TODO: figure out whether a var def has an explicitly written underscore as its body or not
        require(in.symbol.isTerm)
        require(in.symbol.isDeferred ==> rhs.isEmpty)
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
          // TODO: recover named/default parameters
          p.Ctor.Secondary(pmods(in), explicitss.cvt_!, implicits.cvt_!, argss.cvt_!, stats.cvt_!)
        } else if (in.symbol.isMacro) {
          require(tpt.nonEmpty) // TODO: support pre-2.12 macros with inferred return types
          val macroSigs = in.symbol.annotations.filter(_.tree.tpe.typeSymbol.fullName == "scala.reflect.macros.internal.macroImpl")
          def mkMacroDefn(gbody: g.Tree) =
            p.Defn.Macro(pmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, explicitss.cvt_!, implicits.cvt_!, tpt.cvt_!, gbody.cvt_!)
          def parseSig(gsig: g.Annotation) = {
            val q"new $_[..$_]($_(..$args)[..$targs])" = ensugar(gsig.tree)
            val metadata = args.collect{
              case g.Assign(g.Literal(g.Constant(s: String)), g.Literal(g.Constant(v))) => s -> v
              case g.Assign(g.Literal(g.Constant(s: String)), tree) => s -> tree
            }.toMap
            metadata + ("targs" -> targs)
          }
          macroSigs match {
            case legacySig :: palladiumSig :: Nil =>
              // TODO: figure out the protocol of communicating whether the macro is blackbox or whitebox
              // this information can be datamined from palladiumSig, but so far it's unclear where to put it
              val metaprogram = parseSig(palladiumSig)("implDdef").asInstanceOf[g.DefDef].rhs
              mkMacroDefn(metaprogram)
            case legacySig :: Nil =>
              // TODO: obtain the impl ref exactly how it was written by the programmer
              val legacy = parseSig(legacySig)
              val className = legacy("className").asInstanceOf[String]
              val methodName = legacy("methodName").asInstanceOf[String]
              val isBundle = legacy("isBundle").asInstanceOf[Boolean]
              val targs = legacy("targs").asInstanceOf[List[g.Tree]]
              require(className.endsWith("$") ==> !isBundle)
              val containerSym = if (isBundle) g.rootMirror.staticClass(className) else g.rootMirror.staticModule(className.stripSuffix("$"))
              val container = g.Ident(containerSym).setType(if (isBundle) containerSym.asType.toType else containerSym.info)
              val methodSym = containerSym.info.member(g.TermName(methodName))
              var implRef: g.Tree = g.Select(container, methodSym).setType(methodSym.info)
              if (targs.nonEmpty) implRef = g.TypeApply(implRef, targs).setType(g.appliedType(methodSym.info, targs.map(_.tpe)))
              mkMacroDefn(implRef)
            case _ => unreachable
          }
        } else if (in.symbol.isDeferred) p.Decl.Def(pmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, explicitss.cvt_!, implicits.cvt_!, tpt.cvt_!) // TODO: infer procedures
        else p.Defn.Def(pmods(in), in.symbol.asMethod.rawcvt(in), tparams.cvt, explicitss.cvt_!, implicits.cvt_!, if (tpt.nonEmpty) Some[p.Type](tpt.cvt_!) else None, body.cvt_!)
      case in @ g.TypeDef(_, _, tparams, tpt) if pt <:< typeOf[p.TypeParam] =>
        // TODO: undo desugarings of context and view bounds
        require(in.symbol.isType)
        val isAnonymous = in.symbol.name == g.tpnme.WILDCARD
        if (isAnonymous) p.TypeParam.Anonymous(pmods(in), tparams.cvt, Nil, Nil, ptparambounds(tpt))
        else p.TypeParam.Named(pmods(in), in.symbol.asType.rawcvt(in), tparams.cvt, Nil, Nil, ptparambounds(tpt))
      case in @ g.TypeDef(_, _, tparams, tpt) if pt <:< typeOf[p.Member] =>
        require(in.symbol.isType)
        if (in.symbol.isDeferred) p.Decl.Type(pmods(in), in.symbol.asType.rawcvt(in), tparams.cvt, ptparambounds(tpt))
        else p.Defn.Type(pmods(in), in.symbol.asType.rawcvt(in), tparams.cvt, tpt.cvt_!)
      case g.LabelDef(_, _, _) =>
        // TODO: preprocess the input so that we don't have LabelDefs
        ???
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
      case in @ g.Template(_, _, rawstats) =>
        // NOTE: SyntacticTemplate (based on UnMkTemplate, the basis of SyntacticClassDef and friends)
        // returns incorrect parents if input is typechecked, so we have to work around
        val SyntacticTemplate(gearlydefns, _, gself, gstats) = in
        val pparents = {
          // TODO: discern `... extends C()` and `... extends C`
          // TODO: recover names and defaults
          val gparents = in.metadata("originalParents").asInstanceOf[List[g.Tree]]
          gparents match {
            case Nil => Nil
            case gfirstparent +: gotherparents =>
              var (gsupersymbol, gargss) = g.treeInfo.firstConstructor(rawstats) match {
                case g.DefDef(_, name, _, _, _, rawinit) if name == g.nme.CONSTRUCTOR && !rawinit.exists(_ == g.pendingSuperCall) =>
                  rawinit.collect { case g.treeInfo.Applied(core @ g.Select(g.Super(_, _), _), _, argss) => (core.symbol, argss) }.head
                case _ =>
                  (g.NoSymbol, g.analyzer.superArgs(gfirstparent).getOrElse(Nil))
              }
              if (gargss == List(Nil)) gargss = Nil
              val pfirstparent = p.Aux.Parent(gfirstparent.cvt_!, gargss.cvt_!).appendScratchpad(gsupersymbol).appendScratchpad(gfirstparent)
              val potherparents = gotherparents.map(gp => p.Aux.Parent(gp.cvt_!, Nil))
              pfirstparent +: potherparents
          }
        }
        // TODO: really infer hasStats
        // TODO: we should be able to write this without an `if` by having something like `hasStats` as an optional synthetic parameter
        if (gstats.isEmpty) p.Aux.Template(gearlydefns.cvt_!, pparents, gself.cvt)
        else p.Aux.Template(gearlydefns.cvt_!, pparents, gself.cvt, gstats.cvt_!)
      case g.Block((gcdef @ g.ClassDef(_, g.TypeName("$anon"), _, _)) :: Nil, q"new $$anon()") =>
        val pcdef: p.Defn.Class = gcdef.cvt_!
        p.Term.New(pcdef.templ)
      case g.Block(stats, expr) =>
        p.Term.Block((stats :+ expr).cvt_!)
      case g.CaseDef(pat, guard, body) =>
        val q"..$stats" = body
        p.Aux.Case(pat.cvt_!, if (guard.nonEmpty) Some[p.Term](guard.cvt_!) else None, stats.cvt_!)
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
      case in @ g.Bind(_, tree @ g.UnApply(q"$ref.$unapply[..$targs](..$_)", g.Typed(g.Ident(g.nme.WILDCARD), tpt) :: Nil)) =>
        require(unapply == g.TermName("unapply") || unapply == g.TermName("unapplySeq"))
        // TODO: figure out whether a classtag-style extractor was written explicitly by the programmer
        if (tree.fun.symbol.owner == g.definitions.ClassTagClass) p.Pat.Typed(in.symbol.asTerm.rawcvt(in), tpt.cvt_!)
        else p.Pat.Bind(in.symbol.asTerm.rawcvt(in), tree.cvt)
      case in @ g.Bind(name, tree) =>
        require(in.symbol.isTerm)
        require(name == in.symbol.name)
        p.Pat.Bind(in.symbol.asTerm.rawcvt(in), tree.cvt_!)
      case in @ g.Apply(tpt @ g.TypeTree(), args) =>
        // TypeTree[1]().setOriginal(Select[2](Ident[3](scala#26), scala.Tuple2#1688))
        // [1] MethodType(List(TermName("_1")#30490, TermName("_2")#30491), TypeRef(ThisType(scala#27), scala.Tuple2#1687, List(TypeRef(SingleType(SingleType(NoPrefix, TermName("c")#15795), TermName("universe")#15857), TypeName("TermSymbol")#9456, List()), TypeRef(SingleType(SingleType(NoPrefix, TermName("c")#15795), TermName("universe")#15857), TypeName("Ident")#10233, List()))))
        // [2] SingleType(SingleType(ThisType(<root>#2), scala#26), scala.Tuple2#1688)
        // [3] SingleType(ThisType(<root>#2), scala#26)
        require(tpt.tpe.isInstanceOf[g.MethodType])
        require(tpt.original != null)
        val (companion, targs) = tpt.original match {
          case g.AppliedTypeTree(companion, targs) => (companion, targs)
          case companion => (companion, Nil)
        }
        require(companion.symbol.isModule)
        // TODO: infer whether it was an application or a Tuple
        if (g.definitions.isTupleSymbol(companion.symbol.companion)) p.Pat.Tuple(args.cvt_!)
        else p.Pat.Extract(companion.cvt_!, targs.cvt_!, args.cvt_!)
      case in @ g.UnApply(q"$ref.$unapply[..$targs](..$_)", args) =>
        // TODO: infer Extract vs ExtractInfix
        // TODO: also figure out Interpolate
        // TODO: figure out whether targs were explicitly specified or not
        require(unapply == g.TermName("unapply") || unapply == g.TermName("unapplySeq"))
        type pScalaExtract = p.Pat{ type ThisType >: p.Pat.Extract with p.Pat.Typed <: p.Pat }
        // TODO: change this to be an ascription of pScalaExtract instead of essentially a no-op asInstanceOf
        // TODO: figure out whether a classtag-style extractor was written explicitly by the programmer
        if (in.fun.symbol.owner == g.definitions.ClassTagClass) (args.head.cvt_! : p.Pat).asInstanceOf[pScalaExtract]
        else p.Pat.Extract(ref.cvt_!, targs.cvt_!, args.cvt_!)
      case g.Function(params, body) =>
        // TODO: recover eta-expansions that typer desugars to lambdas
        // TODO: recover shorthand function syntax
        (params, body) match {
          case (x0def :: Nil, g.Match(x0ref @ g.Ident(_), cases)) if x0def.symbol == x0ref.symbol && x0def.name.toString.startsWith("x0$") => p.Term.Cases(cases.cvt)
          case _ => p.Term.Function(params.cvt, body.cvt_!)
        }
      case g.Assign(lhs, rhs) =>
        p.Term.Assign(lhs.cvt_!, rhs.cvt_!)
      case g.AssignOrNamedArg(lhs, rhs) =>
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
        // TODO: undo desugarings of `try foo catch bar`
        val catchp = if (catches.nonEmpty) Some(p.Term.Cases(catches.cvt)) else None
        val finallyp = if (finalizer.nonEmpty) Some[p.Term](finalizer.cvt_!) else None
        p.Term.Try(body.cvt_!, catchp, finallyp)
      case g.Throw(expr) =>
        p.Term.Throw(expr.cvt_!)
      case g.New(_) =>
        unreachable
      case g.Typed(expr, tpt) if pt <:< typeOf[p.Term] =>
        // TODO: infer the difference between Ascribe and Annotate
        p.Term.Ascribe(expr.cvt_!, tpt.cvt_!)
      case g.Typed(expr, tpt) if pt <:< typeOf[p.Pat] =>
        p.Pat.Typed(expr.cvt_!, tpt.cvt_!)
      case in @ g.TypeApply(fn, targs) =>
        type pScalaFn = p.Term{ type ThisType >: p.Term.Name with p.Term.Select with p.Term.ApplyUnary <: p.Term }
        val pfn = (fn match {
          case fn @ g.Ident(_) => fn.symbol.asTerm.rawcvt(fn)
          case fn @ g.Select(_, _) => (fn.cvt_! : p.Term)
          case _ => unreachable
        }).asInstanceOf[pScalaFn]
        p.Term.ApplyType(pfn, targs.cvt_!)
      case in @ g.Apply(g.Select(g.New(_), g.nme.CONSTRUCTOR), _) =>
        // TODO: infer the difference between `new X` vs `new X()`
        // TODO: strip off inferred type and value arguments (but be careful to not remove explicitly provided arguments!)
        // TODO: recover names and defaults (https://github.com/scala/scala/pull/3753/files#diff-269d2d5528eed96b476aded2ea039444R617)
        // TODO: figure out whether type arguments were inferred or not
        val q"new $tpt(...$argss)" = in
        val supercall = p.Aux.Parent(tpt.cvt_!, argss.cvt_!).appendScratchpad(in)
        val self = p.Aux.Self(None, None).appendScratchpad(tpt)
        val templ = p.Aux.Template(Nil, List(supercall), self).appendScratchpad(in)
        p.Term.New(templ)
      case in @ g.Apply(_, _) =>
        // TODO: infer the difference between Apply and Update
        // TODO: infer whether it was an application or a Tuple
        // TODO: recover names and defaults (https://github.com/scala/scala/pull/3753/files#diff-269d2d5528eed96b476aded2ea039444R617)
        // TODO: strip off inferred type arguments in loopParent
        // TODO: undo the for desugaring
        // TODO: undo the Lit.Symbol desugaring
        // TODO: undo the interpolation desugaring
        // TODO: figure out whether the programmer actually wrote `foo(...)` or it was `foo.apply(...)`
        // TODO: figure out whether the programmer actually wrote the interpolator or they were explicitly using a desugaring
        // TODO: figure out whether the programmer actually wrote the infix application or they were calling a symbolic method using a dot
        type pScalaApply = p.Term{ type ThisType >: p.Term.Name with p.Term.Select with p.Term.Apply with p.Term.ApplyInfix with p.Term.ApplyType with p.Term.Interpolate <: p.Term }
        def loop(in: g.Tree): pScalaApply = {
          val prelimResult = in match {
            case g.Apply(fn, args) => p.Term.Apply(loop(fn), args.cvt_!)
            case g.TypeApply(g.Select(qual, _), targs) if in.symbol.name == g.TermName("apply") => g.treeCopy.TypeApply(in, qual, targs).cvt
            case g.Select(qual, _) if in.symbol.name == g.TermName("apply") => (qual.cvt_! : p.Term)
            case in: g.TypeApply => in.cvt
            case in: g.Select => in.cvt
            case in: g.Ident => in.symbol.asTerm.rawcvt(in)
          }
          val result = prelimResult match {
            case Term.Apply(Term.Select(Term.Apply(stringContext, parts), prefix), args) =>
              val isInterpolation = stringContext.scratchpad.collect{ case tree: g.Tree if tree.symbol == g.symbolOf[StringContext.type].companion.companion => tree }.nonEmpty
              if (isInterpolation) {
                require(parts.forall(_.isInstanceOf[p.Lit.String]))
                require(args.forall(_.isInstanceOf[p.Term]))
                p.Term.Interpolate(prefix, parts.asInstanceOf[Seq[p.Lit.String]], args.asInstanceOf[Seq[p.Term]])
              } else prelimResult
            case Term.Apply(Term.Select(lhs: p.Term, op), args @ List(arg)) if !op.value.forall(c => Character.isLetter(c)) =>
              Term.ApplyInfix(lhs, op, Nil, args)
            case _ =>
              prelimResult
          }
          result.appendScratchpad(in).asInstanceOf[pScalaApply]
        }
        loop(in)
      case in @ g.ApplyDynamic(_, _) =>
        unreachable
      case in @ g.Super(qual @ g.This(_), mix) =>
        require(in.symbol.isClass)
        val orig = in.metadata.get("originalThis").map(_.asInstanceOf[g.This])
        val pthis = orig.map(orig => qual.symbol.qualcvt(orig))
        val psuper = if (mix != g.tpnme.EMPTY) Some(in.symbol.asClass.rawcvt(in)) else None
        p.Qual.Super(pthis, psuper)
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
        // TODO: Ident(<term name>) with a type symbol attached to it
        // is the encoding that the ensugarer uses to denote a self reference
        // also see the ensugarer for more information
        if (in.isTerm && in.symbol.isType) p.Term.Name(alias(in), isBackquoted(in))
        else in.symbol.rawcvt(in)
      case g.ReferenceToBoxed(_) =>
        ???
      case g.Literal(const) =>
        pconst(const)
      case g.Parens(_) =>
        unreachable
      case g.DocDef(_, _) =>
        unreachable
      case g.Annotated(_, _) =>
        unreachable
      case g.ArrayValue(_, _) =>
        unreachable
      case g.InjectDerivedValue(_) =>
        unreachable
      case g.SelectFromArray(_, _, _) =>
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
        // TODO: infer whether that was Apply, Function or Tuple
        p.Type.Apply(tpt.cvt_!, args.cvt_!)
      case _: g.TypTree =>
        // TODO: also account for Idents, which can very well refer to types while not being TypTrees themselves
        ???
      // NOTE: this derivation is ambiguous because of at least g.ValDef, g.TypeDef and g.Typed
      // case _: g.Tree =>
      //   derive
    }
  }
}

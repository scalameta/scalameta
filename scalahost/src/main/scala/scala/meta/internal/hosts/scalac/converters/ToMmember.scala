package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.collections._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.hosts.scalac.reflect._
import scala.meta.internal.flags._

// This module exposes a method that can convert scala.reflect symbols into equivalent scala.meta members.
// There are some peculiarities that you'll need to know about it:
//
// 1) The conversion always requires a prefix (i.e. a scala.reflect type), because
// our members track prefixes to avoid accidental mishaps and the inconvenience of typeSignatureIn/asSeenFrom.
// Consequently, the output m.Member might change its structural parts based on the prefix,
// e.g. `t"List".defs("head")` will look like `def head: A = ???`,
// while `t"List[Int]".defs("head")` will look like `def head: Int = ???`.
//
// 2) The conversion actually works not with g.Symbol, but with l.Symbol (aka logical symbol).
// That's because scala.reflect symbols and scala.meta members don't have a one-to-one correspondence
// (e.g. field + getter + setter collapse into a single m.Defn.Var and certain g.Symbols, e.g. $isInstanceOf,
// don't even have a representation in the scala.meta world).
//
// 3) The conversion not only supports lookup within scala.meta ASTs (i.e. AST persistence),
// but it can also operate in legacy mode, where it rebuilds scala.meta-compliant metadata from g.Symbols.
//
// 4) See comments to ToMtree to learn more about how this conversion preserves the original syntax of those types.
trait ToMmember extends ReflectToolkit with MetaToolkit {
  self: Api =>

  protected implicit class XtensionLsymbolToMmember(lsym: l.Symbol) {
    private def mmods(gpre: g.Type, lsym: l.Symbol): Seq[m.Mod] = {
      def annotationMods(lsym: l.Symbol): Seq[m.Mod] = {
        // TODO: collect annotations scattered over synthetic members
        lsym.gsymbol.annotations.toMannots
      }
      def accessQualifierMods(lsym: l.Symbol): Seq[m.Mod] = {
        val gsym = lsym.gsymbol
        val gpriv = gsym.privateWithin.orElse(gsym.owner)
        if (gsym.hasFlag(LOCAL)) {
          val maccessBoundary = m.Term.This(m.Name.Anonymous().withMattrs(gpre, gpriv))
          if (gsym.hasFlag(PROTECTED)) List(m.Mod.Protected(maccessBoundary))
          else if (gsym.hasFlag(PRIVATE)) List(m.Mod.Private(maccessBoundary))
          else unreachable(debug(gsym, gsym.flags, gsym.getClass, gsym.owner))
        } else if (gsym.hasAccessBoundary && gpriv != g.NoSymbol) {
          // TODO: `private[pkg] class C` doesn't have PRIVATE in its flags
          // so we need to account for that!
          // TODO: we probably need to account for gpre instead of blindly using g.DefaultPrefix
          val maccessBoundary = gpriv.toMname(g.DefaultPrefix).require[m.Name.Qualifier]
          if (gsym.hasFlag(PROTECTED)) List(m.Mod.Protected(maccessBoundary))
          else List(m.Mod.Private(maccessBoundary))
        } else {
          val maccessBoundary = m.Name.Anonymous().withMattrs(gpre, gsym.owner)
          if (gsym.hasFlag(PROTECTED)) List(m.Mod.Protected(maccessBoundary))
          else if (gsym.hasFlag(PRIVATE)) List(m.Mod.Private(maccessBoundary))
          else Nil
        }
      }
      def otherMods(lsym: l.Symbol): Seq[m.Mod] = {
        val gsym = lsym.gsymbol
        val mmods = scala.collection.mutable.ListBuffer[m.Mod]()
        if (gsym.isImplicit) mmods += m.Mod.Implicit()
        if (gsym.isFinal) mmods += m.Mod.Final()
        if (gsym.isSealed) mmods += m.Mod.Sealed()
        if (gsym.isOverride) mmods += m.Mod.Override()
        if (gsym.isCase) mmods += m.Mod.Case()
        if (gsym.isAbstract && lsym.isInstanceOf[l.Clazz]) mmods += m.Mod.Abstract()
        if (gsym.isAbstractOverride) { mmods += m.Mod.Abstract(); mmods += m.Mod.Override() }
        if (gsym.isCovariant) mmods += m.Mod.Covariant()
        if (gsym.isContravariant) mmods += m.Mod.Contravariant()
        if (gsym.isLazy) mmods += m.Mod.Lazy()
        mmods.toList
      }
      def valVarParamMods(lsym: l.Symbol): Seq[m.Mod] = {
        val mmods = scala.collection.mutable.ListBuffer[m.Mod]()
        val ggetter = lsym.gsymbol.owner.filter(_.isPrimaryConstructor).map(_.owner.info.member(lsym.gsymbol.name))
        val gfield = ggetter.map(_.owner.info.member(ggetter.localName))
        val isApplicable = lsym.gsymbol.owner.isPrimaryConstructor && gfield != g.NoSymbol
        if (isApplicable && gfield.isMutable) mmods += m.Mod.VarParam()
        if (isApplicable && !gfield.isMutable && !gfield.owner.isCase) mmods += m.Mod.ValParam()
        mmods.toList
      }
      val result = annotationMods(lsym) ++ accessQualifierMods(lsym) ++ otherMods(lsym) ++ valVarParamMods(lsym)
      // TODO: we can't discern `class C(x: Int)` and `class C(private[this] val x: Int)`
      // so let's err on the side of the more popular option
      if (lsym.gsymbol.owner.isPrimaryConstructor) result.filter({
        case m.Mod.Private(m.Term.This(_)) => false
        case _ => true
      }) else result
    }
    def toMmember(gpre: g.Type): m.Member = ldenotToMmemberIndex.getOrElseUpdate(l.Denotation(gpre, lsym), {
      def approximateSymbol(lsym: l.Symbol): m.Member = {
        // NOTE: we don't need to clear the LOCAL_SUFFIX_STRING from the name of `lsym.gsymbol`
        // because it's always guaranteed not to end with LOCAL_SUFFIX_STRING
        // see LogicalSymbols.scala for more information
        lazy val gsym = lsym.gsymbol
        lazy val ginfo = gsym.moduleClass.orElse(gsym).infoIn(gpre)
        lazy val gtparams = ginfo.typeParams
        lazy val gvparamss = ginfo.paramss
        lazy val gtpe = {
          // NOTE: strips off only those vparams and tparams that are part of the definition
          // we don't want to, for example, damage type lambdas
          def loop(gtpe: g.Type): g.Type = gtpe match {
            case g.NullaryMethodType(gret) =>
              loop(gret)
            case g.MethodType(gvparams, gret) =>
              if (gvparams.forall(gsym => gvparamss.flatten.exists(_ == gsym))) loop(gret)
              else gtpe
            case g.PolyType(gtparams, gret) =>
              if (gtparams.forall(gsym => gtparams.exists(_ == gsym))) loop(gret)
              else gret
            case g.ExistentialType(quants, gtpe) =>
              // NOTE: apparently, sometimes we can get stuff like `ExistentialType(..., NullaryMethodType(...))`
              // see Enumeration.vmap for an example
              g.ExistentialType(quants, loop(gtpe))
            case _ =>
              gtpe
          }
          loop(ginfo)
        }
        lazy val mmods = {
          val mffi: List[m.Mod.Ffi] = {
            def ffiScalaIntrinsic(gmeth: g.Symbol) = {
              val className = gmeth.owner.tpe.jvmsig
              val methodName = gmeth.name.encoded
              val methodSig = gmeth.tpe.jvmsig
              List(m.Mod.Ffi(s"scalaIntrinsic($className, $methodName, $methodSig)"))
            }
            def ffiScalaReflectMacro(gmacro: g.Symbol) = {
              List(m.Mod.Ffi("scalaReflectMacro"))
            }
            def ffiJvmField(gfield: g.Symbol) = {
              val className = gfield.owner.tpe.jvmsig
              val fieldName = gfield.name.encoded
              val fieldSig = gfield.tpe.jvmsig
              List(m.Mod.Ffi(s"jvmField($className, $fieldName, $fieldSig)"))
            }
            def ffiJvmMethod(gmeth: g.Symbol) = {
              val className = gmeth.owner.tpe.jvmsig
              val methodName = gmeth.name.encoded
              val methodSig = gmeth.tpe.jvmsig
              List(m.Mod.Ffi(s"jvmMethod($className, $methodName, $methodSig)"))
            }
            def ffiJvmErasure(gtpe: g.Type) = {
              val className = gtpe.jvmsig
              List(m.Mod.Ffi(s"jvmErasure($className)"))
            }
            def ffiJvmPackage(gsym: g.Symbol) = {
              val packageName = gsym.fullName
              List(m.Mod.Ffi(s"jvmPackage($packageName)"))
            }
            lsym match {
              case l.AbstractVal(gget) =>
                ffiJvmMethod(gget)
              case l.AbstractVar(gget, gset) =>
                ffiJvmMethod(gget)
              case l.AbstractDef(gsym) =>
                if (gsym.isIntrinsic) ffiScalaIntrinsic(gsym)
                else ffiJvmMethod(gsym)
              case l.AbstractType(gsym) =>
                ffiJvmErasure(gsym.tpe)
              case l.Val(gfield, gget) =>
                if (gget == g.NoSymbol) ffiJvmField(gfield)
                else ffiJvmMethod(gget)
              case l.Var(gfield, gget, _) =>
                if (gget == g.NoSymbol) ffiJvmField(gfield)
                else ffiJvmMethod(gget)
              case l.Def(gsym) =>
                if (gsym.isIntrinsic) ffiScalaIntrinsic(gsym)
                else ffiJvmMethod(gsym)
              case l.Macro(gsym) =>
                gsym.macroBody match {
                  case MacroBody.None => unreachable(debug(gsym, gsym.flags, gsym.getClass, gsym.owner))
                  case MacroBody.FastTrack(_) => ffiScalaReflectMacro(gsym)
                  case MacroBody.Reflect(_) => ffiScalaReflectMacro(gsym)
                  case MacroBody.Meta(body) => Nil
                }
              case l.Type(gsym) =>
                ffiJvmErasure(gsym.tpe)
              case l.Clazz(gsym) =>
                ffiJvmErasure(gsym.tpe)
              case l.Trait(gsym) =>
                ffiJvmErasure(gsym.tpe)
              case l.Object(gmodule, gmoduleClass) =>
                ffiJvmErasure(gmodule.tpe)
              case l.Package(gmodule, gmoduleClass) =>
                ffiJvmPackage(gmodule)
              case l.PackageObject(gmodule, gmoduleClass) =>
                ffiJvmErasure(gmodule.tpe)
              case l.PrimaryCtor(gsym) =>
                ffiJvmMethod(gsym)
              case l.SecondaryCtor(gsym) =>
                ffiJvmMethod(gsym)
              case l.TermParameter(gsym) if gsym.hasFlag(DEFAULTPARAM) =>
                val paramPos = gsym.owner.paramss.flatten.indexWhere(_.name == gsym.name)
                require(paramPos != -1)
                val gdefaultGetterName = gsym.owner.name + "$default$" + (paramPos + 1)
                var gdefaultGetterOwner = if (!gsym.owner.isConstructor) gsym.owner.owner else gsym.owner.owner.companion
                val gdefaultGetter = gdefaultGetterOwner.info.decl(g.TermName(gdefaultGetterName).encodedName)
                require(gdefaultGetter != g.NoSymbol && debug(gsym, gdefaultGetterOwner, gdefaultGetterName))
                ffiJvmMethod(gdefaultGetter)
              // TODO: should we also generate ffi information for type parameters? what should it look like?
              case _ =>
                Nil
            }
          }
          mffi ++ this.mmods(gpre, lsym)
        }
        lazy val mname = {
          def ctorName(gsym: g.Symbol) = {
            m.Ctor.Name(gsym.owner.displayName).withMattrs(gpre, gsym)
          }
          def paramName(gsym: g.Symbol) = {
            if (gsym.name.isAnonymous) m.Name.Anonymous().withMattrs(g.DefaultPrefix, gsym)
            else gsym.toMname(g.DefaultPrefix)
          }
          lsym match {
            case l.AbstractVal(gsym) if gsym.hasFlag(EXISTENTIAL) =>
              val name = g.Ident(gsym.name.toString.stripSuffix(g.nme.SINGLETON_SUFFIX)).displayName
              m.Term.Name(name).withMattrs(gpre, gsym)
            case l.AbstractVal(gsym) =>
              gsym.toMname(gpre)
            case l.PackageObject(gmodule, gmoduleClass) =>
              m.Term.Name(gmodule.owner.displayName).withMattrs(gpre, gmodule)
            case l.PrimaryCtor(gsym) =>
              ctorName(gsym)
            case l.SecondaryCtor(gsym) =>
              ctorName(gsym)
            case l.TermParameter(gsym) =>
              paramName(gsym)
            case l.TypeParameter(gsym) =>
              paramName(gsym)
            case _ =>
              gsym.toMname(gpre)
          }
        }
        lazy val mtparams = gtparams.map(gtparam => l.TypeParameter(gtparam).toMmember(g.NoPrefix).require[m.Type.Param])
        lazy val mvparamss = gvparamss.map(_.map(gvparam => l.TermParameter(gvparam).toMmember(g.NoPrefix).require[m.Term.Param]))
        lazy val mtpe = lsym match {
          case l.AbstractVal(gsym) if gsym.hasFlag(EXISTENTIAL) =>
            val g.TypeBounds(_, g.RefinedType(List(gtpe, singleton), g.Scope())) = gsym.info
            require(singleton =:= g.definitions.SingletonClass.tpe)
            gtpe.toMtype
          case l.Val(_, _) =>
            gtpe.normalizeVararg.toMtype
          case l.Var(_, _, _) =>
            gtpe.normalizeVararg.toMtype
          case _ =>
            gtpe.toMtype
        }
        lazy val mtpearg = gtpe.toMtypeArg
        lazy val mtpebounds = gtpe match {
          case gtpe @ g.TypeBounds(glo, ghi) =>
            val mlo = if (glo =:= g.typeOf[Nothing]) None else Some(glo.toMtype)
            val mhi = if (ghi =:= g.typeOf[Any]) None else Some(ghi.toMtype)
            m.Type.Bounds(mlo, mhi)
        }
        lazy val mbody: m.Term = {
          val munknownTerm = m.Term.Name("???").withMattrs(g.DefaultPrefix, g.definitions.Predef_???)
          lsym match {
            case l.Macro(gsym) =>
              gsym.macroBody match {
                case MacroBody.Meta(body) => body.toMtree[m.Term]
                case _ => munknownTerm
              }
            case l.SecondaryCtor(gsym) =>
              val gctor = gsym.owner.primaryConstructor
              val mctorref = m.Ctor.Name(gsym.owner.displayName).withMattrs(gpre, gctor)
              m.Term.Apply(mctorref, List(munknownTerm))
            case _ =>
              munknownTerm
          }
        }
        lazy val mmaybeBody = if (gsym.hasFlag(DEFAULTINIT)) None else Some(mbody)
        lazy val mfakector = self.mfakector(gtpe)
        lazy val mctor = {
          if (lsym.isInstanceOf[l.Clazz] || lsym.isInstanceOf[l.Object]) {
            val gctorsym = lsym.gsymbol.moduleClass.orElse(lsym.gsymbol).primaryConstructor
            if (gctorsym != g.NoSymbol) {
              val gctorinfo = gctorsym.infoIn(gpre)
              val mctorname = m.Ctor.Name(gsym.displayName).withMattrs(gpre, gctorsym)
              var mctorparamss = {
                if (lsym.isInstanceOf[l.Clazz]) gctorinfo.paramss.map(_.map(gvparam => l.TermParameter(gvparam).toMmember(g.NoPrefix).require[m.Term.Param]))
                else Nil // NOTE: synthetic constructors for modules have a fake List(List()) parameter list
              }
              if (mctorparamss.length == 1 && mctorparamss.flatten.length == 0) mctorparamss = Nil
              m.Ctor.Primary(this.mmods(gpre, l.PrimaryCtor(gctorsym)), mctorname, mctorparamss)
            } else {
              mfakector
            }
          } else {
            mfakector
          }
        }
        lazy val mtemplate = {
          def isEarly(mstat: m.Stat) = {
            def mfield(mstat: m.Stat): Option[m.Name] = mstat match {
              case m.Defn.Val(_, List(m.Pat.Var.Term(name)), _, _) => Some(mname)
              case m.Defn.Var(_, List(m.Pat.Var.Term(name)), _, _) => Some(mname)
              case _ => None
            }
            val sfield = mfield(mstat).map(_.denot.requireSymbol).getOrElse(s.Symbol.Zero)
            symbolTable.convert(sfield) match {
              case l.Val(gfield, _) => gfield.hasFlag(PRESUPER)
              case l.Var(gfield, _, _) => gfield.hasFlag(PRESUPER)
              case _ => false
            }
          }
          val mearly = LazySeq(mstats.filter(mstat => isEarly(mstat)))
          val mlate = LazySeq(mstats.filter(mstat => !isEarly(mstat)))
          val gparents = ginfo match {
            case gtpe: g.ClassInfoType => gtpe.realParents
            case g.PolyType(_, gtpe: g.ClassInfoType) => gtpe.realParents
          }
          val mparents = gparents.map(gparent => {
            val mtpe = gparent.toMtype
            var gctor = gparent.typeSymbol.primaryConstructor.orElse(gparent.typeSymbol)
            if (gctor.name == g.nme.MIXIN_CONSTRUCTOR) gctor = gparent.typeSymbol
            val mctor = m.Ctor.Name(gparent.typeSymbolDirect.name.decoded).withMattrs(gparent, gctor)
            mtpe.ctorRef(mctor).require[m.Ctor.Call]
          })
          // TODO: apply gpre to mselftpe
          val mselftpe = if (gsym.thisSym != gsym) Some(gsym.thisSym.tpe.toMtype) else None
          val mself = m.Term.Param(Nil, m.Name.Anonymous(), mselftpe, None)
          m.Template(mearly, mparents, mself, Some(mlate))
        }
        lazy val mstats = LazySeq({
          val gstatowner = gsym match { case gclass: g.ClassSymbol => gclass; case gmodule: g.ModuleSymbol => gmodule.moduleClass.asClass }
          val gstatpre = gstatowner.toTypeIn(gpre)
          val ldecls = gstatpre.decls.toLogical
          val lcensoredDecls = ldecls.filter(!_.isInstanceOf[l.PrimaryCtor])
          lcensoredDecls.map(_.toMmember(gstatpre)).map(_.stat)
        })
        lazy val mmaybeDefault = if (gsym.hasFlag(DEFAULTPARAM)) Some(mbody) else None
        lazy val mviewbounds = {
          val gevidences = gsym.owner.paramss.flatten.filter(_.name.startsWith(g.nme.EVIDENCE_PARAM_PREFIX))
          val gviewBounds = gevidences.map(gev => gev.tpe.typeArgs match {
            // TODO: hygiene!
            case List(gfrom, gto) if gfrom.typeSymbol.name == gsym.name => gto.typeSymbol
            case _ => g.NoSymbol
          }).filter(_ != g.NoSymbol)
          gviewBounds.map(gbound => gbound.asType.toMname(g.DefaultPrefix))
        }
        lazy val mcontextbounds = {
          val gevidences = gsym.owner.paramss.flatten.filter(_.name.startsWith(g.nme.EVIDENCE_PARAM_PREFIX))
          val gcontextBounds = gevidences.map(gev => gev.tpe.typeArgs match {
            // TODO: hygiene!
            case List(gtarg) if gtarg.typeSymbol.name == gsym.name => gev.tpe.typeSymbol
            case _ => g.NoSymbol
          }).filter(_ != g.NoSymbol)
          gcontextBounds.map(gbound => gbound.asType.toMname(g.DefaultPrefix))
        }
        lsym match {
          case l.Zero => unreachable(debug(lsym.gsymbol, lsym.gsymbol.flags, lsym.gsymbol.getClass, lsym.gsymbol.owner))
          case _: l.AbstractVal => m.Decl.Val(mmods, List(m.Pat.Var.Term(mname.require[m.Term.Name])), mtpe).member
          case _: l.AbstractVar => m.Decl.Var(mmods, List(m.Pat.Var.Term(mname.require[m.Term.Name])), mtpe).member
          case _: l.AbstractDef if lsym.gsymbol.isIntrinsic => m.Defn.Def(mmods, mname.require[m.Term.Name], mtparams, mvparamss, Some(mtpe), mbody)
          case _: l.AbstractDef => m.Decl.Def(mmods, mname.require[m.Term.Name], mtparams, mvparamss, mtpe)
          case _: l.AbstractType => m.Decl.Type(mmods, mname.require[m.Type.Name], mtparams, mtpebounds)
          case _: l.Val => m.Defn.Val(mmods, List(m.Pat.Var.Term(mname.require[m.Term.Name])), Some(mtpe), mbody).member
          case _: l.Var => m.Defn.Var(mmods, List(m.Pat.Var.Term(mname.require[m.Term.Name])), Some(mtpe), mmaybeBody).member
          case _: l.Def => m.Defn.Def(mmods, mname.require[m.Term.Name], mtparams, mvparamss, Some(mtpe), mbody)
          case _: l.Macro => m.Defn.Macro(mmods, mname.require[m.Term.Name], mtparams, mvparamss, mtpe, mbody)
          case _: l.Type => m.Defn.Type(mmods, mname.require[m.Type.Name], mtparams, mtpe)
          case _: l.Clazz => m.Defn.Class(mmods, mname.require[m.Type.Name], mtparams, mctor, mtemplate)
          case _: l.Trait => m.Defn.Trait(mmods, mname.require[m.Type.Name], mtparams, mctor, mtemplate)
          case _: l.Object => m.Defn.Object(mmods, mname.require[m.Term.Name], mctor, mtemplate)
          case _: l.Package => m.Pkg(mname.require[m.Term.Name], mstats)
          case _: l.PackageObject => m.Pkg.Object(mmods, mname.require[m.Term.Name], mctor, mtemplate)
          case _: l.PrimaryCtor => m.Ctor.Primary(mmods, mname.require[m.Ctor.Name], mvparamss)
          case _: l.SecondaryCtor => m.Ctor.Secondary(mmods, mname.require[m.Ctor.Name], mvparamss, mbody)
          case _: l.TermBind => m.Pat.Var.Term(mname.require[m.Term.Name])
          case _: l.TypeBind => m.Pat.Var.Type(mname.require[m.Type.Name])
          case _: l.TermParameter => m.Term.Param(mmods, mname.require[m.Term.Param.Name], Some(mtpearg), mmaybeDefault).withAttrs(mtpearg)
          case _: l.TypeParameter => m.Type.Param(mmods, mname.require[m.Type.Param.Name], mtparams, mtpebounds, mviewbounds, mcontextbounds)
          case _ => throw new ConvertException(lsym, s"unsupported symbol $lsym, designation = ${gsym.getClass}, flags = ${gsym.flags}")
        }
      }
      def applyPrefix(gpre: g.Type, mmem: m.Member): m.Member = {
        if (gpre == g.NoPrefix) mmem
        else {
          // TODO: implement me! it might not be that hard, to be honest
          // 1) Replace Type.Name(tparam) in mmem and its denotations with values obtained from gpre
          // 2) Replace Term.This(mmem.owner) in mmem and its denotations with apply(gpre)
          mmem
        }
      }
      val ssym = symbolTable.convert(lsym)
      val maybeCachedMmember = ssymToMmemberIndex.get(ssym)
      val maybePrefixedCachedMmember = maybeCachedMmember.map(applyPrefix(gpre, _))
      maybePrefixedCachedMmember.getOrElse(approximateSymbol(lsym)).forceTypechecked
    })
  }

  protected implicit class XtensionLdenotationToMmember(ldenot: l.Denotation) {
    def toMmember: m.Member = ldenot.sym.toMmember(ldenot.pre)
  }
}
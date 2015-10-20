package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.{ListBuffer, ListMap}
import org.scalameta.unreachable
import org.scalameta.ast.{Reflection => AstReflection}
import scala.compat.Platform.EOL

class ast extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AstMacros.impl
}

class AstMacros(val c: Context) extends AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror
  import c.universe._
  import Flag._
  val AdtInternal = q"_root_.org.scalameta.adt.Internal"
  val AstInternal = q"_root_.org.scalameta.ast.internal"
  val InternalSemantic = q"_root_.scala.meta.internal.semantic"
  val FlagsPackage = q"_root_.scala.meta.internal.flags.`package`"
  val Tokens = q"_root_.scala.meta.tokens"
  val Prettyprinters = q"_root_.scala.meta.internal.prettyprinters"
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      def fullName = c.internal.enclosingOwner.fullName.toString + "." + cdef.name.toString
      def is(abbrev: String) = fullName == "scala.meta.internal.ast." + abbrev
      def isQuasi = cdef.name.toString == "Quasi"
      def isName = is("Name.Anonymous") || is("Name.Indeterminate") || is("Term.Name") || is("Type.Name") || is("Ctor.Ref.Name")
      def isLit = !isQuasi && fullName.startsWith("scala.meta.internal.ast.Lit")
      def isCtorRef = !isQuasi && fullName.startsWith("scala.meta.internal.ast.Ctor.Ref")
      def isCtorCall = !isQuasi && fullName.startsWith("scala.meta.internal.ast.Ctor.Call")
      def looksLikeTermButNotTerm = is("Term.Param") || is("Term.Arg.Named") || is("Term.Arg.Repeated")
      def isTerm = !isQuasi && (fullName.startsWith("scala.meta.internal.ast.Term") || isLit || isCtorRef || isCtorCall) && !looksLikeTermButNotTerm
      def isTermParam = is("Term.Param")
      def hasTokens = true
      def hasEnv = isName || isTerm
      def hasDenot = isName
      def hasTyping = isTerm || isTermParam
      def hasExpansion = isTerm
      val q"$imods class $iname[..$tparams] $ctorMods(...$rawparamss) extends { ..$earlydefns } with ..$iparents { $aself => ..$astats }" = cdef
      // TODO: For stack traces, we'd like to have short class names, because stack traces print full names anyway.
      // However debugging macro expansion errors is much-much easier with full names for Api and Impl classes
      // because the typechecker only uses short names in error messages.
      // E.g. compare:
      //  class Impl needs to be abstract, since method withDenot in trait Name
      //  of type (denot: scala.meta.internal.semantic.Denotation)Impl.this.ThisType is not defined
      // and:
      //  class NameAnonymousImpl needs to be abstract, since method withDenot in trait Name
      //  of type (denot: scala.meta.internal.semantic.Denotation)NameAnonymousImpl.this.ThisType is not defined
      // val descriptivePrefix = fullName.stripPrefix("scala.meta.internal.ast.").stripPrefix("scala.meta.").replace(".", "")
      // val aname = TypeName(descriptivePrefix.replace(".", "") + "Api")
      // val name = TypeName(descriptivePrefix.replace(".", "") + "Impl")
      val aname = TypeName("Api")
      val name = TypeName("Impl")
      val qname = TypeName("Quasi")
      val qmname = TermName("Quasi")
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val bparams1 = ListBuffer[ValDef]() // boilerplate params
      val paramss1 = ListBuffer[List[ValDef]]() // payload params
      val iself = noSelfType
      val self = aself
      val istats1 = ListBuffer[Tree]()
      val astats1 = ListBuffer[Tree]()
      val stats1 = ListBuffer[Tree]()
      val qstats1 = ListBuffer[Tree]()
      val ianns1 = ListBuffer[Tree]() ++ imods.annotations
      def imods1 = imods.mapAnnotations(_ => ianns1.toList)
      def cmods1 = Modifiers(FINAL, mname.toTypeName, List(q"new _root_.scala.SerialVersionUID(1L)"))
      def qmods1 = Modifiers(NoFlags, TypeName("meta"), List(q"new _root_.org.scalameta.ast.ast"))
      val iparents1 = ListBuffer[Tree]() ++ iparents
      def aparents1 = List(tq"$iname")
      def parents1 = List(tq"$aname")
      def qparents1 = tq"$iname" +: tq"_root_.scala.meta.internal.ast.Quasi" +: iparents1.filter(_.toString != "_root_.scala.Product").map({
        case Ident(name) => Select(Ident(name.toTermName), TypeName("Quasi"))
        case Select(qual, name) => Select(Select(qual, name.toTermName), TypeName("Quasi"))
        case unsupported => c.abort(unsupported.pos, "implementation restriction: unsupported parent")
      })
      val mstats1 = ListBuffer[Tree]() ++ mstats
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      def unprivatize(mods: Modifiers) = Modifiers((mods.flags.asInstanceOf[Long] & (~scala.reflect.internal.Flags.PRIVATE) & (~scala.reflect.internal.Flags.LOCAL)).asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
      def unoverride(mods: Modifiers) = Modifiers((mods.flags.asInstanceOf[Long] & (~scala.reflect.internal.Flags.OVERRIDE)).asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
      def undefault(mods: Modifiers) = Modifiers((mods.flags.asInstanceOf[Long] & (~scala.reflect.internal.Flags.DEFAULTPARAM)).asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
      def varify(mods: Modifiers) = Modifiers(mods.flags | MUTABLE, mods.privateWithin, mods.annotations)
      def finalize(mods: Modifiers) = Modifiers(mods.flags | FINAL, mods.privateWithin, mods.annotations)

      // step 1: validate the shape of the class
      if (imods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @ast classes")
      if (imods.hasFlag(FINAL)) c.abort(cdef.pos, "final is redundant for @ast classes")
      if (imods.hasFlag(CASE)) c.abort(cdef.pos, "case is redundant for @ast classes")
      if (imods.hasFlag(ABSTRACT)) c.abort(cdef.pos, "@ast classes cannot be abstract")
      if (ctorMods.flags != NoFlags) c.abort(cdef.pos, "@ast classes must define a public primary constructor")
      if (rawparamss.length == 0) c.abort(cdef.pos, "@leaf classes must define a non-empty parameter list")

      // step 2: validate the body of the class
      val (adefns, arest1) = astats.partition(_.isDef)
      val (aimports, arest2) = arest1.partition(_ match { case _: Import => true; case _ => false })
      astats1 ++= adefns
      astats1 ++= aimports
      val (requires, illegal) = arest2.partition(_ match { case q"require($what)" => true; case _ => false })
      illegal.foreach(stmt => c.abort(stmt.pos, "only invariants and definitions are allowed in @ast classes"))

      // step 3: calculate the parameters of the class
      val paramss = rawparamss

      // step 4: create internal bookkeeping parameters
      locally {
        bparams1 += q"protected val privateFlags: $FlagsPackage.Flags"
      }
      bparams1 += q"@_root_.scala.transient protected val privatePrototype: $iname"
      locally {
        bparams1 += q"protected val privateParent: _root_.scala.meta.Tree"
        stats1 += q"""
          def parent: _root_.scala.Option[_root_.scala.meta.Tree] = {
            if (privateParent != null) _root_.scala.Some(privateParent)
            else _root_.scala.None
          }
        """
        stats1 += q"def children: Seq[_root_.scala.meta.Tree] = $AstInternal.children[ThisType, _root_.scala.meta.Tree]"
      }
      if (hasTokens) {
        bparams1 += q"@_root_.scala.transient protected var privateTokens: $Tokens.Tokens"
        astats1 += q"def tokens: $Tokens.Tokens"
        stats1 += q"""
          def tokens: $Tokens.Tokens = {
            privateTokens = privateTokens match {
              case null => $Prettyprinters.inferTokens(this, None)
              case _root_.scala.meta.internal.tokens.TransformedTokens(proto) => $Prettyprinters.inferTokens(this, Some(proto))
              case other => other
            }
            privateTokens
          }
        """
      } else {
        // NOTE: never happens
        // stats1 += q"protected def privateTokens: $InternalSemantic.Denotation = null"
        ???
      }
      if (hasEnv) {
        bparams1 += q"protected val privateEnv: $InternalSemantic.Environment"
        astats1 += q"private[meta] def env: $InternalSemantic.Environment"
        stats1 += q"""
          private[meta] def env: $InternalSemantic.Environment = {
            if (privateEnv != null) privateEnv
            else $InternalSemantic.Environment.Zero
          }
        """
      } else {
        if (!isQuasi) {
          stats1 += q"protected def privateEnv: $InternalSemantic.Environment = null"
        } else {
          // NOTE: generated elsewhere, grep for `quasigetter`
        }
      }
      if (hasDenot) {
        bparams1 += q"protected val privateDenot: $InternalSemantic.Denotation"
        astats1 += q"private[meta] def denot: $InternalSemantic.Denotation"
        stats1 += q"""
          private[meta] def denot: $InternalSemantic.Denotation = {
            if (privateDenot != null) privateDenot
            else $InternalSemantic.Denotation.Zero
          }
        """
      } else {
        if (!isQuasi) {
          stats1 += q"protected def privateDenot: $InternalSemantic.Denotation = null"
        } else {
          // NOTE: generated elsewhere, grep for `quasigetter`
        }
      }
      if (hasTyping) {
        bparams1 += q"protected val privateTyping: $InternalSemantic.Typing"
        astats1 += q"private[meta] def typing: $InternalSemantic.Typing"
        stats1 += q"""
          private[meta] def typing: $InternalSemantic.Typing = {
            if (privateTyping != null) privateTyping
            else $InternalSemantic.Typing.Zero
          }
        """
      } else {
        if (!isQuasi) {
          stats1 += q"protected def privateTyping: $InternalSemantic.Typing = null"
        } else {
          // NOTE: generated elsewhere, grep for `quasigetter`
        }
      }
      if (hasExpansion) {
        bparams1 += q"protected val privateExpansion: $InternalSemantic.Expansion"
        astats1 += q"private[meta] def expansion: $InternalSemantic.Expansion"
        stats1 += q"""
          private[meta] def expansion: $InternalSemantic.Expansion = {
            if (privateExpansion != null) privateExpansion
            else $InternalSemantic.Expansion.Zero
          }
        """
      } else {
        if (!isQuasi) {
          stats1 += q"protected def privateExpansion: $InternalSemantic.Expansion = null"
        } else {
          // NOTE: generated elsewhere, grep for `quasigetter`
        }
      }

      // step 5: turn all parameters into vars, create getters and setters
      def internalize(name: TermName) = TermName("_" + name.toString)
      val fieldParamss = paramss
      paramss1 ++= fieldParamss.map(_.map{ case p @ q"$mods val $name: $tpt = $default" => q"${undefault(unoverride(unprivatize(varify(mods))))} val ${internalize(p.name)}: $tpt" })
      istats1 ++= fieldParamss.flatten.map(p => {
        var getterAnns = List(q"new $AstInternal.astField")
        if (p.mods.annotations.exists(_.toString.contains("auxiliary"))) getterAnns :+= q"new $AstInternal.auxiliary"
        val getterMods = Modifiers(DEFERRED, typeNames.EMPTY, getterAnns)
        q"$getterMods def ${p.name}: ${p.tpt}"
      })
      stats1 ++= fieldParamss.flatten.map(p => {
        val pinternal = internalize(p.name)
        val pmods = if (p.mods.hasFlag(OVERRIDE)) Modifiers(OVERRIDE) else NoMods
        q"""
          $pmods def ${p.name}: ${p.tpt} = {
            $AstInternal.loadField(this.$pinternal)
            this.$pinternal
          }
        """
      })
      def quasigetter(mods: Modifiers, name: String) = {
        val unsupportedUnquotingPosition = "unsupported unquoting position"
        val unsupportedSplicingPosition = "unsupported splicing position"
        val message = q"if (this.rank == 0) $unsupportedUnquotingPosition else $unsupportedSplicingPosition"
        val impl = q"throw new _root_.scala.`package`.UnsupportedOperationException($message)"
        val Modifiers(flags, privateWithin, anns) = mods
        var flags1 = flags
        if (!isTermParam) flags1 |= OVERRIDE // NOTE: crazy, I know...
        val mods1 = Modifiers(flags1, privateWithin, anns)
        q"$mods1 def ${TermName(name)}: _root_.scala.Nothing = $impl"
      }
      def quasisetter(mods: Modifiers, name: String, params: ValDef*) = {
        val DefDef(mods1, termName, tparams, _, tpt, rhs) = quasigetter(mods, name)
        DefDef(mods1, termName, tparams, List(params.toList), tpt, rhs)
      }
      // NOTE: we don't create quasigetters for tokens, because those are used for prettyprinting
      // here we have a weird mix of metalevels, because unlike denot, typing and others
      // `Quasi.tokens` may reasonably mean two different things:
      // 1) tokens of a tree that a quasi stands for, 2) quasi's own tokens
      if (hasEnv) qstats1 += quasigetter(PrivateMeta, "env")
      qstats1 += q"override protected def privateEnv: $InternalSemantic.Environment = null"
      if (hasDenot) qstats1 += quasigetter(PrivateMeta, "denot")
      qstats1 += q"override protected def privateDenot: $InternalSemantic.Denotation = null"
      if (hasTyping) qstats1 += quasigetter(PrivateMeta, "typing")
      qstats1 += q"override protected def privateTyping: $InternalSemantic.Typing = null"
      if (hasExpansion) qstats1 += quasigetter(PrivateMeta, "expansion")
      qstats1 += q"override protected def privateExpansion: $InternalSemantic.Expansion = null"
      qstats1 ++= fieldParamss.flatten.map(p => quasigetter(NoMods, p.name.toString))
      if (is("Name.Anonymous")) qstats1 += quasigetter(NoMods, "value")

      // step 6: create the privateCopy method
      // The purpose of this method is to provide extremely cheap cloning
      // in the case when a tree changes its parent (because that happens often in our framework,
      // e.g. when we create a quasiquote and then insert it into a bigger quasiquote,
      // or when we parse something and build the trees from the ground up).
      // In such a situation, we copy all private state verbatim (tokens, denotations, etc)
      // and create lazy initializers that will take care of recursively copying the children.
      // Compare this with the `copy` method (described below), which additionally flushes the private state.
      // This method is private[meta] because the state that it's managing is not supposed to be touched
      // by the users of the framework.
      val privateCopyInternals = ListBuffer[Tree]()
      privateCopyInternals += q"flags"
      privateCopyInternals += q"prototype.asInstanceOf[ThisType]"
      privateCopyInternals += q"parent"
      if (hasTokens) privateCopyInternals += q"tokens"
      if (hasEnv) privateCopyInternals += q"env"
      if (hasDenot) privateCopyInternals += q"denot"
      if (hasTyping) privateCopyInternals += q"typing"
      if (hasExpansion) privateCopyInternals += q"expansion"
      val privateCopyInitss = paramss.map(_.map(p => q"$AstInternal.initField(this.${internalize(p.name)})"))
      val privateCopyBody = q"new $name(..$privateCopyInternals)(...$privateCopyInitss)"
      stats1 += q"""
        private[meta] def privateCopy(
            flags: $FlagsPackage.Flags = privateFlags,
            prototype: _root_.scala.meta.Tree = this,
            parent: _root_.scala.meta.Tree = privateParent,
            tokens: $Tokens.Tokens = privateTokens,
            env: $InternalSemantic.Environment = privateEnv,
            denot: $InternalSemantic.Denotation = privateDenot,
            typing: $InternalSemantic.Typing = privateTyping,
            expansion: $InternalSemantic.Expansion = privateExpansion): ThisType = {
          $privateCopyBody
        }
      """

      // step 7: create the copy method
      // The purpose of this method is to provide a facility to change small parts of the tree
      // without modifying the other parts, much like the standard case class copy works.
      // In such a situation, the tree is going to be recreated
      val fieldDefaultss = fieldParamss.map(_.map(p => q"this.${p.name}"))
      val copyParamss = fieldParamss.zip(fieldDefaultss).map{ case (f, d) => f.zip(d).map { case (p, default) => q"val ${p.name}: ${p.tpt} = $default" } }
      val copyArgss = fieldParamss.map(_.map(p => q"${p.name}"))
      val copyCore = q"$mname.apply(...$copyArgss)"
      val copyBody = q"$copyCore.withTokens(tokens = _root_.scala.meta.internal.tokens.TransformedTokens(this))"
      // TODO: would be useful to turn copy into a macro, so that its calls are guaranteed to be inlined
      astats1 += q"def copy(...$copyParamss): $iname = $copyBody"

      // step 8: create the withXXX methods
      // TODO: We should maybe consistently reintroduce the withXXX family of methods for all kinds of fields.
      locally {
        val attributeValidator = {
          var attrs = ListMap[Tree, Tree]()
          if (hasDenot) attrs(q"this.denot") = q"$InternalSemantic.Denotation.Zero"
          if (hasTyping) attrs(q"this.typing") = q"$InternalSemantic.Typing.Zero"
          if (hasExpansion) attrs(q"this.expansion") = q"$InternalSemantic.Expansion.Zero"
          if (attrs.nonEmpty) {
            val checks = attrs.map({ case (k, v) =>
              val enablesTypechecked = q"(flags & $FlagsPackage.TYPECHECKED) == $FlagsPackage.TYPECHECKED"
              val attrEmpty = q"$k == $v"
              var attrPrinter = q"$Prettyprinters.Attributes.attributesTree[_root_.scala.meta.Tree]"
              attrPrinter = q"$attrPrinter($Prettyprinters.Attributes.Recursion.Deep, $Prettyprinters.Attributes.Force.Never)"
              val attrShow = q"$attrPrinter.apply(this).toString"
              val message = q"${"failed to enable TYPECHECKED for "} + $attrShow"
              q"if ($enablesTypechecked && $attrEmpty) throw new _root_.scala.`package`.UnsupportedOperationException($message)"
            })
            q"..$checks"
          } else {
            EmptyTree
          }
        }
        astats1 += q"""
          protected def privateWithFlags(flags: $FlagsPackage.Flags): $iname = {
            $attributeValidator
            this.privateCopy(flags = flags)
          }
        """
      }
      if (hasTokens) {
        astats1 += q"""
          def withTokens(tokens: $Tokens.Tokens): $iname = {
            this.privateCopy(tokens = tokens)
          }
        """
        // NOTE: Much like we don't create a quasigetter for tokens,
        // we don't create a quasisetter for withTokens.
      }
      if (hasEnv) {
        // NOTE: We shouldn't clean up denots when setting env,
        // because that would destroy links between defs and refs,
        // which may irreversibly hamper subsequent retypechecks.
        val paramEnv = q"val env: $InternalSemantic.Environment"
        // NOTE: No state validation here, because withEnv can be called on U, PA and FA.
        // Hygiene (embodied by envs in scala.meta) applies to unattributed and attributed trees alike.
        astats1 += q"""
          private[meta] def withEnv($paramEnv): $iname = {
            this.privateCopy(
              flags = this.privateFlags & ~$FlagsPackage.TYPECHECKED,
              env = env,
              denot = this.privateDenot,
              typing = $InternalSemantic.Typing.Zero,
              expansion = $InternalSemantic.Expansion.Zero
            )
          }
        """
        qstats1 += quasisetter(PrivateMeta, "withEnv", paramEnv)
      }
      if (hasDenot || hasTyping) {
        val paramDenot = q"val denot: $InternalSemantic.Denotation"
        val paramTypingLike = q"val typingLike: $InternalSemantic.TypingLike"
        val dortMods = if (!isTermParam) PrivateMeta(OVERRIDE) else PrivateMeta
        val dandtMods = PrivateMeta
        val termOrCtorName = q"this.isInstanceOf[_root_.scala.meta.Term.Name] || this.isInstanceOf[_root_.scala.meta.Ctor.Name]"
        val termOrCtorNameCheck = q"""if ($termOrCtorName) throw new UnsupportedOperationException("need to simultaneously set both denotation and typing for a " + this.productPrefix)"""
        val stateMessage = "can only call withAttrs on unattributed trees; if necessary, call .copy() to unattribute and then do .withAttrs(...)"
        var attrPrinter = q"$Prettyprinters.Attributes.attributesTree[_root_.scala.meta.Tree]"
        attrPrinter = q"$attrPrinter($Prettyprinters.Attributes.Recursion.Deep, $Prettyprinters.Attributes.Force.Never)"
        val stateDetails = q"$attrPrinter.apply(this).toString"
        val stateCheck = q"if (!isUnattributed) throw new UnsupportedOperationException($stateMessage + $EOL + $stateDetails)"
        val withAttrsD = q"""
          $dortMods def withAttrs($paramDenot): $iname = {
            $termOrCtorNameCheck
            $stateCheck
            this.privateCopy(
              flags = this.privateFlags & ~$FlagsPackage.TYPECHECKED,
              env = $InternalSemantic.Environment.Zero,
              denot = denot,
              typing = this.privateTyping,
              expansion = $InternalSemantic.Expansion.Identity
            )
          }
        """
        val withAttrsT = q"""
          $dortMods def withAttrs($paramTypingLike): $iname = {
            $termOrCtorNameCheck
            $stateCheck
            val typing = typingLike.typing
            this.privateCopy(
              flags = this.privateFlags & ~$FlagsPackage.TYPECHECKED,
              env = $InternalSemantic.Environment.Zero,
              denot = this.privateDenot,
              typing = typing,
              expansion = $InternalSemantic.Expansion.Identity
            )
          }
        """
        val withAttrsDT = q"""
          $dandtMods def withAttrs($paramDenot, $paramTypingLike): $iname = {
            $stateCheck
            val typing = typingLike.typing
            this.privateCopy(
              flags = this.privateFlags & ~$FlagsPackage.TYPECHECKED,
              env = $InternalSemantic.Environment.Zero,
              denot = denot,
              typing = typing,
              expansion = $InternalSemantic.Expansion.Identity
            )
          }
        """
        if (hasDenot) {
          astats1 += withAttrsD
          qstats1 += quasisetter(PrivateMeta, "withAttrs", paramDenot)
        }
        if (hasTyping) {
          astats1 += withAttrsT
          qstats1 += quasisetter(PrivateMeta, "withAttrs", paramTypingLike)
        }
        if (hasDenot && hasTyping) {
          istats1 += withAttrsDT
          qstats1 += quasisetter(PrivateMeta, "withAttrs", paramDenot, paramTypingLike)
        }
      }
      if (hasExpansion) {
        val paramExpansionLike = q"val expansionLike: $InternalSemantic.ExpansionLike"
        val commonMessage = "can only call withExpansion on partially attributed trees"
        val unattributedMessage = commonMessage + ", call .withAttrs first and only then .withExpansion(...)"
        val fullyAttributedMessage = commonMessage + ", if necessary, call .copy() to unattribute, then .withAttrs(...) and only then .withExpansion(...)"
        val stateMessage = q"if (isUnattributed) $unattributedMessage else $fullyAttributedMessage"
        var attrPrinter = q"$Prettyprinters.Attributes.attributesTree[_root_.scala.meta.Tree]"
        attrPrinter = q"$attrPrinter($Prettyprinters.Attributes.Recursion.Deep, $Prettyprinters.Attributes.Force.Never)"
        val stateDetails = q"$attrPrinter.apply(this).toString"
        val stateCheck = q"if (!isPartiallyAttributed) throw new UnsupportedOperationException($stateMessage + $EOL + $stateDetails)"
        astats1 += q"""
          private[meta] def withExpansion($paramExpansionLike): $iname = {
            $stateCheck
            val expansion = expansionLike.expansion
            this.privateCopy(
              flags = this.privateFlags & ~$FlagsPackage.TYPECHECKED,
              env = this.privateEnv,
              denot = this.privateDenot,
              typing = this.privateTyping,
              expansion = expansion
            )
          }
        """
        qstats1 += quasisetter(PrivateMeta, "withExpansion", paramExpansionLike)
      }

      // step 9: generate boilerplate required by the @ast infrastructure
      istats1 += q"override type ThisType <: $iname"
      astats1 += q"override type ThisType = $iname"
      astats1 += q"override def privateTag: _root_.scala.Int = $mname.privateTag"
      mstats1 += q"def privateTag: _root_.scala.Int = $AdtInternal.calculateTag[$iname]"
      // TODO: remove leafClass and leafCompanion from here
      ianns1 += q"new $AstInternal.astClass"
      ianns1 += q"new $AdtInternal.leafClass"
      manns1 += q"new $AstInternal.astCompanion"
      manns1 += q"new $AdtInternal.leafCompanion"

      // step 10: implement Product
      val productParamss = rawparamss.map(_.map(_.duplicate))
      iparents1 += tq"_root_.scala.Product"
      astats1 += q"override def productPrefix: _root_.scala.Predef.String = $AstInternal.productPrefix[ThisType]"
      astats1 += q"override def productArity: _root_.scala.Int = ${productParamss.head.length}"
      val pelClauses = ListBuffer[Tree]()
      pelClauses ++= 0.to(productParamss.head.length - 1).map(i => cq"$i => this.${productParamss.head(i).name}")
      pelClauses += cq"_ => throw new _root_.scala.IndexOutOfBoundsException(n.toString)"
      astats1 += q"override def productElement(n: _root_.scala.Int): Any = n match { case ..$pelClauses }"
      astats1 += q"override def productIterator: _root_.scala.Iterator[_root_.scala.Any] = _root_.scala.runtime.ScalaRunTime.typedProductIterator(this)"

      // step 11: generate serialization logic
      val fieldInits = fieldParamss.flatten.map(p => q"$AstInternal.loadField(this.${internalize(p.name)})")
      stats1 += q"protected def writeReplace(): _root_.scala.AnyRef = { ..$fieldInits; this }"

      // step 12: generate Companion.apply
      val applyParamss = paramss.map(_.map(_.duplicate))
      val internalParamss = paramss.map(_.map(p => q"@..${p.mods.annotations} val ${p.name}: ${p.tpt}"))
      val internalBody = ListBuffer[Tree]()
      val internalLocalss = paramss.map(_.map(p => (p.name, internalize(p.name))))
      internalBody += q"$AstInternal.hierarchyCheck[$iname]"
      // internalBody += q"$AdtInternal.immutabilityCheck[$iname]"
      internalBody ++= internalLocalss.flatten.map{ case (local, internal) => q"$AdtInternal.nullCheck($local)" }
      internalBody ++= internalLocalss.flatten.map{ case (local, internal) => q"$AdtInternal.emptyCheck($local)" }
      internalBody ++= aimports
      internalBody ++= requires.map(require => {
        var hasErrors = false
        object errorChecker extends Traverser {
          override def traverse(tree: Tree): Unit = tree match {
            case _: This => hasErrors = true; c.error(tree.pos, "cannot refer to this in @ast requires")
            case _ => super.traverse(tree)
          }
        }
        errorChecker.traverse(require)
        if (hasErrors) q"()"
        else require
      })
      var internalInitCount = 2 // privatePrototype, privateParent
      if (hasTokens) internalInitCount += 1
      if (hasEnv) internalInitCount += 1
      if (hasDenot) internalInitCount += 1
      if (hasTyping) internalInitCount += 1
      if (hasExpansion) internalInitCount += 1
      val internalInitss = 1.to(internalInitCount).map(_ => q"null")
      val paramInitss = internalLocalss.map(_.map{ case (local, internal) => q"$AstInternal.initParam($local)" })
      internalBody += q"val node = new $name($FlagsPackage.ZERO, ..$internalInitss)(...$paramInitss)"
      internalBody ++= internalLocalss.flatten.flatMap{ case (local, internal) =>
        val (validators, assignee) = {
          // TODO: this is totally ugly. we need to come up with a way to express this in a sane way.
          val validateLocal = TermName("validate" + local.toString.capitalize)
          if (is("Pkg") && local.toString == "stats") {
            val validators = List(q"def $validateLocal(stat: Stat) = { require(stat.isTopLevelStat); stat }")
            (validators, q"$local.map($validateLocal)")
          } else if ((is("Defn.Trait") || is("Defn.Object") || is("Pkg.Object")) && local.toString == "templ") {
            val validators = List(q"def $validateLocal(stats: Seq[Stat]) = stats.map(stat => { require(!stat.isInstanceOf[Ctor]); stat })")
            (validators, q"{ if (!$local.isInstanceOf[impl.Quasi]) $local.stats.map($validateLocal); $local }")
          } else if (is("Template") && local.toString == "early") {
            val validators = List(q"def $validateLocal(stat: Stat) = { require(stat.isEarlyStat && parents.nonEmpty); stat }")
            (validators, q"$local.map($validateLocal)")
          } else if (is("Template") && local.toString == "stats") {
            val validators = List(q"def $validateLocal(stats: Seq[Stat]) = stats.map(stat => { require(stat.isTemplateStat); stat })")
            (validators, q"$local.map($validateLocal)")
          } else {
            (Nil, q"$local")
          }
        }
        validators :+ q"$AstInternal.storeField(node.$internal, $assignee)"
      }
      internalBody += q"node"
      val internalArgss = paramss.map(_.map(p => q"${p.name}"))
      mstats1 += q"""
        def apply(...$applyParamss): $iname = {
          def internal(...$internalParamss): $iname = {
            ..$internalBody
          }
          internal(...$internalArgss)
        }
      """

      // step 13: generate Companion.unapply
      val unapplyParamss = rawparamss.map(_.map(_.duplicate))
      val unapplyParams = unapplyParamss.head
      val needsUnapply = !mstats.exists(stat => stat match { case DefDef(_, TermName("unapply"), _, _, _, _) => true; case _ => false })
      if (needsUnapply) {
        if (unapplyParams.length != 0) {
          // TODO: re-enable name-based pattern matching once https://issues.scala-lang.org/browse/SI-9029 is fixed
          // astats1 += q"@_root_.scala.inline final def isDefined = !isEmpty"
          // astats1 += q"@_root_.scala.inline final def isEmpty = false"
          // val getBody = if (unapplyParams.length == 1) q"this.${unapplyParams.head.name}" else q"this"
          // astats1 += q"@_root_.scala.inline final def get = $getBody"
          // unapplyParams.zipWithIndex.foreach({ case (p, i) => astats1 += q"@_root_.scala.inline final def ${TermName("_" + (i + 1))} = this.${p.name}" })
          // mstats1 += q"@_root_.scala.inline final def unapply(x: $name): $name = x"
          val successTargs = tq"(..${unapplyParams.map(p => p.tpt)})"
          val successArgs = q"(..${unapplyParams.map(p => q"x.${p.name}")})"
          mstats1 += q"@_root_.scala.inline final def unapply(x: $iname): Option[$successTargs] = if (x == null) _root_.scala.None else _root_.scala.Some($successArgs)"
        } else {
          mstats1 += q"@_root_.scala.inline final def unapply(x: $iname): Boolean = true"
        }
      }

      // step 14: finish codegen for Quasi
      qstats1 += q"def pt: _root_.java.lang.Class[_] = _root_.org.scalameta.runtime.arrayClass(_root_.scala.Predef.classOf[$iname], this.rank)"
      if (is("Pkg")) {
        qstats1 += quasigetter(NoMods, "name")
      }
      if (isQuasi) {
        stats1 += q"""
          def become[T <: _root_.scala.meta.internal.ast.Quasi](implicit ev: _root_.org.scalameta.ast.AstMetadata[T]): T = {
            this match {
              case $mname(0, tree) =>
                ev.quasi(0, tree).withTokens(this.tokens).asInstanceOf[T]
              case $mname(1, nested @ $mname(0, tree)) =>
                ev.quasi(1, nested.become[T]).withTokens(this.tokens).asInstanceOf[T]
              case _ =>
                throw new Exception("complex ellipses are not supported yet")
            }
          }
        """
      }
      if (is("Term.Block.Quasi") || is("Type.Bounds.Quasi") || is("Ctor.Primary.Quasi")) {
        // NOTE: before you remove one or all of these throws, you must know what's going on in the "allFields.unquote" test
        stats1 += q"""throw new _root_.scala.NotImplementedError("implementation restriction: dangerous quasi, see the sources of @ast for more information")"""
      }

      mstats1 += q"import _root_.scala.language.experimental.{macros => prettyPlease}"
      mstats1 += q"import _root_.scala.language.implicitConversions"
      mstats1 += q"implicit def interfaceToApi(interface: $iname): $aname = macro $AstInternal.Macros.interfaceToApi[$iname, $aname]"
      mstats1 += q"trait $aname[..$tparams] extends ..$aparents1 { $aself => ..$astats1 }"
      mstats1 += q"$cmods1 class $name[..$tparams] $ctorMods(...${bparams1 +: paramss1}) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
      if (!isQuasi) mstats1 += q"$qmods1 class $qname(rank: _root_.scala.Int, tree: _root_.scala.Any) extends ..$qparents1 { ..$qstats1 }"
      val cdef1 = q"$imods1 trait $iname extends ..$iparents1 { $iself => ..$istats1 }"
      val mdef1 = q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      if (c.compilerSettings.contains("-Xprint:typer")) { println(cdef1); println(mdef1) }
      List(cdef1, mdef1)
    }
    val expanded = annottees match {
      case (cdef: ClassDef) :: (mdef: ModuleDef) :: rest => transform(cdef, mdef) ++ rest
      case (cdef: ClassDef) :: rest => transform(cdef, q"object ${cdef.name.toTermName}") ++ rest
      case annottee :: rest => c.abort(annottee.pos, "only classes can be @ast")
    }
    q"{ ..$expanded; () }"
  }
}
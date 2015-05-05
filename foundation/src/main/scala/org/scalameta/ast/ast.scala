package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import org.scalameta.unreachable

class ast extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AstMacros.impl
}

class AstMacros(val c: Context) {
  import c.universe._
  import Flag._
  val AdtInternal = q"_root_.org.scalameta.adt.Internal"
  val AstInternal = q"_root_.org.scalameta.ast.internal"
  val HygieneInternal = q"_root_.scala.meta.internal.hygiene"
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      def is(abbrev: String) = c.internal.enclosingOwner.fullName.toString + "." + cdef.name.toString == "scala.meta.internal.ast." + abbrev
      def isQuasi = cdef.name.toString == "Quasi"
      val q"$imods class $iname[..$tparams] $ctorMods(...$rawparamss) extends { ..$earlydefns } with ..$iparents { $aself => ..$astats }" = cdef
      val aname = TypeName("Api")
      val name = TypeName("Impl")
      val qname = TypeName("Quasi")
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
      val denotParam = q"@$AstInternal.auxiliary val denot: $HygieneInternal.Denotation = $HygieneInternal.Denotation.Zero"
      val sigmaParam = q"@$AstInternal.auxiliary val sigma: $HygieneInternal.Sigma = $HygieneInternal.Sigma.Zero"
      val paramss = {
        if (is("Name.Anonymous") || is("Name.Indeterminate") ||
            is("Term.Name") || is("Type.Name") || is("Ctor.Ref.Name")) {
          (rawparamss.head ++ List(denotParam, sigmaParam)) +: rawparamss.tail
        } else {
          rawparamss
        }
      }

      // step 4: create boilerplate bookkeeping parameters
      val scratchpadType = tq"_root_.scala.collection.immutable.Seq[Any]"
      bparams1 += q"protected val internalPrototype: $iname"
      bparams1 += q"protected val internalParent: _root_.scala.meta.Tree"
      bparams1 += q"protected val internalScratchpad: $scratchpadType"
      bparams1 += q"protected var internalOrigin: _root_.scala.meta.Origin"
      def internalize(name: TermName) = TermName("_" + name.toString)
      val internalCopyInitss = paramss.map(_.map(p => q"$AstInternal.initField(this.${internalize(p.name)})"))
      val internalCopyBody = q"new $name(prototype.asInstanceOf[ThisType], parent, scratchpad, origin)(...$internalCopyInitss)"
      stats1 += q"private[meta] def internalCopy(prototype: _root_.scala.meta.Tree = this, parent: _root_.scala.meta.Tree = internalParent, scratchpad: $scratchpadType = internalScratchpad, origin: _root_.scala.meta.Origin = internalOrigin): ThisType = $internalCopyBody"
      stats1 += q"def parent: _root_.scala.Option[_root_.scala.meta.Tree] = if (internalParent != null) _root_.scala.Some(internalParent) else _root_.scala.None"
      stats1 += q"def origin: _root_.scala.meta.Origin = { if (internalOrigin == null) internalOrigin = _root_.scala.meta.Origin.Synthetic(this); internalOrigin }"

      // step 5: turn all parameters into vars, create getters and setters
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
      def quasigetter(name: String) = {
        val unsupportedUnquotingPosition = "unsupported unquoting position"
        val unsupportedSplicingPosition = "unsupported splicing position"
        val message = q"if (this.rank == 0) $unsupportedUnquotingPosition else $unsupportedSplicingPosition"
        val impl = q"throw new _root_.scala.`package`.UnsupportedOperationException($message)"
        q"override def ${TermName(name)}: _root_.scala.Nothing = $impl"
      }
      qstats1 ++= fieldParamss.flatten.map(p => quasigetter(p.name.toString))
      if (is("Name.Anonymous")) qstats1 += quasigetter("value")
      val fieldDefaultss = fieldParamss.map(_.map(p => {
        if (p.name == denotParam.name) denotParam.rhs
        else if (p.name == sigmaParam.name) sigmaParam.rhs
        else q"this.${p.name}"
      }))
      val copyFieldParamss = fieldParamss.zip(fieldDefaultss).map{ case (f, d) => f.zip(d).map { case (p, default) => q"val ${p.name}: ${p.tpt} = $default" } }
      val copyFieldArgss = fieldParamss.map(_.map(p => q"${p.name}"))
      val copyParamss = copyFieldParamss.init :+ (copyFieldParamss.last :+ q"val origin: _root_.scala.meta.Origin = null")
      val copyArgss = copyFieldArgss.init :+ (copyFieldArgss.last :+ q"origin")
      // TODO: would be useful to turn copy into a macro, so that its calls are guaranteed to be inlined
      astats1 += q"def copy(...$copyParamss): ThisType = $mname.apply(...$copyArgss)"

      // step 6: generate boilerplate required by the @ast infrastructure
      istats1 += q"override type ThisType <: $iname"
      astats1 += q"override type ThisType = $iname"
      astats1 += q"override def internalTag: _root_.scala.Int = $mname.internalTag"
      mstats1 += q"def internalTag: _root_.scala.Int = $AdtInternal.calculateTag[$iname]"
      // TODO: remove leafClass and leafCompanion from here
      ianns1 += q"new $AstInternal.astClass"
      ianns1 += q"new $AdtInternal.leafClass"
      manns1 += q"new $AstInternal.astCompanion"
      manns1 += q"new $AdtInternal.leafCompanion"

      // step 7: implement Product
      val productParamss = rawparamss.map(_.map(_.duplicate))
      iparents1 += tq"_root_.scala.Product"
      astats1 += q"override def productPrefix: _root_.scala.Predef.String = $AstInternal.productPrefix[ThisType]"
      astats1 += q"override def productArity: _root_.scala.Int = ${productParamss.head.length}"
      val pelClauses = ListBuffer[Tree]()
      pelClauses ++= 0.to(productParamss.head.length - 1).map(i => cq"$i => this.${productParamss.head(i).name}")
      pelClauses += cq"_ => throw new _root_.scala.IndexOutOfBoundsException(n.toString)"
      astats1 += q"override def productElement(n: _root_.scala.Int): Any = n match { case ..$pelClauses }"
      astats1 += q"override def productIterator: _root_.scala.Iterator[_root_.scala.Any] = _root_.scala.runtime.ScalaRunTime.typedProductIterator(this)"

      // step 8: generate Companion.apply
      val applyFieldParamss = paramss.map(_.map(_.duplicate))
      val applyParamss = applyFieldParamss.init :+ (applyFieldParamss.last :+ q"val origin: _root_.scala.meta.Origin = null")
      val internalFieldParamss = paramss.map(_.map(p => q"@..${p.mods.annotations} val ${p.name}: ${p.tpt}"))
      val internalParamss = internalFieldParamss.init :+ (internalFieldParamss.last :+ q"val origin: _root_.scala.meta.Origin")
      val internalBody = ListBuffer[Tree]()
      val internalLocalss = paramss.map(_.map(p => (p.name, internalize(p.name))))
      internalBody += q"$AstInternal.hierarchyCheck[$iname]"
      // internalBody += q"$AdtInternal.immutabilityCheck[$iname]"
      internalBody ++= internalLocalss.flatten.map{ case (local, internal) => q"$AdtInternal.nullCheck($local)" }
      internalBody ++= internalLocalss.flatten.map{ case (local, internal) => q"$AdtInternal.emptyCheck($local)" }
      internalBody ++= aimports
      internalBody ++= requires
      val paramInitss = internalLocalss.map(_.map{ case (local, internal) => q"$AstInternal.initParam($local)" })
      internalBody += q"val node = new $name(null, null, _root_.scala.collection.immutable.Nil, origin)(...$paramInitss)"
      internalBody ++= internalLocalss.flatten.flatMap{ case (local, internal) =>
        val (validators, assignee) = {
          // TODO: this is totally ugly. we need to come up with a way to express this in a sane way.
          val validateLocal = TermName("validate" + local.toString.capitalize)
          if (is("Pkg") && local.toString == "stats") {
            val validators = List(q"def $validateLocal(stat: Stat) = { require(stat.isTopLevelStat); stat }")
            (validators, q"$local.map($validateLocal)")
          } else if ((is("Defn.Trait") || is("Defn.Object") || is("Pkg.Object")) && local.toString == "templ") {
            val validators = List(q"def $validateLocal(stats: Seq[Stat]) = stats.map(stat => { require(!stat.isInstanceOf[Ctor]); stat })")
            (validators, q"$local.copy(stats = $local.stats.map($validateLocal), origin = $local.origin)")
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
      val internalFieldArgss = paramss.map(_.map(p => q"${p.name}"))
      val internalArgss = internalFieldArgss.init :+ (internalFieldArgss.last :+ q"origin")
      mstats1 += q"""
        def apply(...$applyParamss): $iname = {
          def internal(...$internalParamss): $iname = {
            ..$internalBody
          }
          internal(...$internalArgss)
        }
      """

      // step 9: generate Companion.unapply
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
          val successTargs = tq"(..${unapplyParamss.head.map(p => p.tpt)})"
          val successArgs = q"(..${unapplyParamss.head.map(p => q"x.${p.name}")})"
          mstats1 += q"@_root_.scala.inline final def unapply(x: $iname): Option[$successTargs] = if (x == null) _root_.scala.None else _root_.scala.Some($successArgs)"
        } else {
          mstats1 += q"@_root_.scala.inline final def unapply(x: $iname): Boolean = true"
        }
      }

      // step 10: finish codegen for Quasi
      qstats1 += q"def pt: _root_.java.lang.Class[_] = _root_.org.scalameta.runtime.arrayClass(_root_.scala.Predef.classOf[$iname], this.rank)"
      if (is("Term.Block.Quasi") || is("Type.Bounds.Quasi") || is("Ctor.Primary.Quasi") ||
          is("Import.Clause.Quasi") || is("Lit.String.Quasi") || is("Mod.Annot.Quasi")) {
        // NOTE: before you remove one or all of these throws, you must know what's going on in the "allFields.unquote" test
        stats1 += q"""throw new _root_.scala.NotImplementedError("implementation restriction: dangerous quasi, see the sources of @ast for more information")"""
      }

      mstats1 += q"import _root_.scala.language.experimental.{macros => prettyPlease}"
      mstats1 += q"import _root_.scala.language.implicitConversions"
      mstats1 += q"implicit def interfaceToApi(interface: $iname): $aname = macro $AstInternal.Macros.interfaceToApi[$iname, $aname]"
      mstats1 += q"trait $aname[..$tparams] extends ..$aparents1 { $aself => ..$astats1 }"
      mstats1 += q"private[${mname.toTypeName}] final class $name[..$tparams] $ctorMods(...${bparams1 +: paramss1}) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
      if (!isQuasi) mstats1 += q"$qmods1 class $qname(tree: _root_.scala.Any, rank: _root_.scala.Int) extends ..$qparents1 { ..$qstats1 }"
      val cdef1 = q"$imods1 trait $iname extends ..$iparents1 { $iself => ..$istats1 }"
      val mdef1 = q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
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
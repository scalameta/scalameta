package org.scalareflect.annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer

class ast extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AstMacros.impl
}

class AstMacros(val c: Context) {
  import c.universe._
  import Flag._
  val AdtInternal = q"_root_.org.scalareflect.adt.Internal"
  val AstInternal = q"_root_.org.scalareflect.annotations.internal.ast"
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val params1 = ListBuffer[ValDef]()
      val stats1 = ListBuffer[Tree]()
      val anns1 = ListBuffer[Tree]() ++ mods.annotations
      def mods1 = mods.mapAnnotations(_ => anns1.toList)
      val parents1 = ListBuffer[Tree]() ++ parents
      val mstats1 = ListBuffer[Tree]() ++ mstats
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      def privatize(mods: Modifiers) = Modifiers(((mods.flags | PRIVATE).asInstanceOf[Long] & (~scala.reflect.internal.Flags.LOCAL)).asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
      def unoverride(mods: Modifiers) = Modifiers((mods.flags.asInstanceOf[Long] & (~scala.reflect.internal.Flags.OVERRIDE)).asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
      def undefault(mods: Modifiers) = Modifiers((mods.flags.asInstanceOf[Long] & (~scala.reflect.internal.Flags.DEFAULTPARAM)).asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
      def varify(mods: Modifiers) = Modifiers(mods.flags | MUTABLE, mods.privateWithin, mods.annotations)
      def finalize(mods: Modifiers) = Modifiers(mods.flags | FINAL, mods.privateWithin, mods.annotations)

      // step 1: validate the shape of the class
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @ast classes")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "final is redundant for @ast classes")
      if (mods.hasFlag(CASE)) c.abort(cdef.pos, "case is redundant for @ast classes")
      if (mods.hasFlag(ABSTRACT)) c.abort(cdef.pos, "@ast classes cannot be abstract")
      if (ctorMods.flags != NoFlags) c.abort(cdef.pos, "@ast classes must define a public primary constructor")
      if (paramss.length != 1) c.abort(cdef.pos, "@ast classes must define exactly one parameter list")

      // step 2: validate the body of the class
      val (defns, rest) = stats.partition(_.isDef)
      stats1 ++= defns
      val (requires, illegal) = rest.partition(_ match { case q"require($what)" => true; case _ => false })
      illegal.foreach(stmt => c.abort(stmt.pos, "only invariants and definitions are allowed in @ast classes"))

      // step 3: turn all parameters into private internal vars, create getters and setters
      val params :: Nil = paramss
      def internalize(p: ValDef) = TermName("internal" + p.name.toString.capitalize)
      params1 ++= params.map { case p @ q"$mods val $name: $tpt = $default" => q"${undefault(unoverride(privatize(varify(mods))))} val ${internalize(p)}: $tpt" }
      stats1 += q"import scala.language.experimental.macros"
      stats1 ++= params.flatMap{p =>
        val pstats = ListBuffer[Tree]()
        val pinternal = internalize(p)
        val pmods = if (p.mods.hasFlag(OVERRIDE)) Modifiers(OVERRIDE) else NoMods
        pstats += q"""
          $pmods def ${p.name}: ${p.tpt} = {
            $AstInternal.loadField(this.$pinternal)
            this.$pinternal
          }
        """
        pstats += q"def ${TermName(p.name + "_=")}(x: Tree): Unit = macro $AstInternal.AstHelperMacros.payloadIsImmutable"
        val withName = TermName("with" + p.name.toString.capitalize)
        pstats += q"def $withName(${p.name}: ${p.tpt})(implicit origin: _root_.scala.reflect.core.Origin): ThisType = this.copy(${p.name} = ${p.name})"
        val mapName = TermName("map" + p.name.toString.capitalize)
        pstats += q"def $mapName(f: ${p.tpt} => ${p.tpt})(implicit origin: _root_.scala.reflect.core.Origin): ThisType = this.copy(${p.name} = f(this.${p.name}))"
        pstats.toList
      }
      val copyParams = params.map(p => q"val ${p.name}: ${p.tpt} = this.${p.name}")
      val copyArgs = params.map(p => q"${p.name}")
      stats1 += q"def copy(..$copyParams)(implicit origin: _root_.scala.reflect.core.Origin): ThisType = $mname.apply(..$copyArgs)(_root_.scala.reflect.core.Origin.Transform(this, this.origin))"

      // step 4: generate boilerplate parameters
      params1 += q"private val prototype: $name"
      params1 += q"val parent: Tree"
      val fieldInits = params.map(p => q"$AstInternal.initField(this.${internalize(p)})")
      stats1 += q"private[reflect] def internalWithParent(parent: Tree): ThisType = new ThisType(..$fieldInits, this, parent, scratchpad, origin)"
      params1 += q"private[reflect] val scratchpad: Any"
      stats1 += q"private[reflect] def withScratchpad(scratchpad: Any): ThisType = new ThisType(..$fieldInits, this, parent, scratchpad, origin)"
      stats1 += q"private[reflect] def mapScratchpad(f: Any => Any): ThisType = new ThisType(..$fieldInits, this, parent, f(scratchpad), origin)"
      params1 += q"val origin: _root_.scala.reflect.core.Origin"
      stats1 += q"def withOrigin(origin: Origin): ThisType = new ThisType(..$fieldInits, this, parent, scratchpad, origin)"
      stats1 += q"def mapOrigin(f: Origin => Origin): ThisType = new ThisType(..$fieldInits, this, parent, scratchpad, f(origin))"

      // step 5: generate boilerplate required by the @adt infrastructure
      stats1 += q"override type ThisType = $name"
      stats1 += q"private[reflect] def tag: _root_.scala.Int = $AdtInternal.calculateTag[ThisType]"
      stats1 += q"$AdtInternal.hierarchyCheck[ThisType]"
      // stats1 += q"$AdtInternal.immutabilityCheck[ThisType]"
      anns1 += q"new $AdtInternal.leaf"
      manns1 += q"new $AdtInternal.leaf"

      // step 6: implement Product
      parents1 += tq"_root_.scala.Product"
      stats1 += q"override def productPrefix: _root_.scala.Predef.String = $AstInternal.productPrefix[ThisType]"
      stats1 += q"override def productArity: _root_.scala.Int = ${params.length}"
      val pelClauses = ListBuffer[Tree]()
      pelClauses ++= 0.to(params.length - 1).map(i => cq"$i => this.${params(i).name}")
      pelClauses += cq"_ => throw new _root_.scala.IndexOutOfBoundsException(n.toString)"
      stats1 += q"override def productElement(n: _root_.scala.Int): Any = n match { case ..$pelClauses }"
      stats1 += q"override def productIterator: _root_.scala.Iterator[_root_.scala.Any] = _root_.scala.runtime.ScalaRunTime.typedProductIterator(this)"

      // step 7: implement equality
      stats1 += q"override def canEqual(that: _root_.scala.Any): _root_.scala.Boolean = that.isInstanceOf[ThisType]"
      stats1 += q"override def equals(that: _root_.scala.Any): _root_.scala.Boolean = (this eq that.asInstanceOf[AnyRef]) || (semanticallyEqual(that) && structurallyEqual(that))"
      stats1 += q"private def semanticallyEqual(that: _root_.scala.Any): _root_.scala.Boolean = false"
      val equalityClauses = params.map(p => q"this.${p.name} == that.${p.name}")
      val equalityCondition = equalityClauses.foldLeft(q"true": Tree)((acc, clause) => q"$clause && $acc")
      stats1 += q"""
        private def structurallyEqual(that: _root_.scala.Any): _root_.scala.Boolean = that match {
          case that: ThisType => ..$equalityCondition
          case _ => false
        }
      """
      stats1 += q"override def hashCode: _root_.scala.Int = semanticHashCode ^ structuralHashCode"
      stats1 += q"private def semanticHashCode: _root_.scala.Int = _root_.java.lang.System.identityHashCode(this)"
      stats1 += q"private def structuralHashCode: _root_.scala.Int = _root_.scala.runtime.ScalaRunTime._hashCode(this)"

      // step 8: generate Companion.apply
      val applyParams = params.map(p => q"@..${mods.annotations} val ${p.name}: ${p.tpt} = ${p.rhs}")
      val applyBody = ListBuffer[Tree]()
      applyBody ++= params.map(p => q"$AdtInternal.nullCheck(${p.name})")
      applyBody ++= params.map(p => q"$AdtInternal.emptyCheck(${p.name})")
      applyBody ++= requires
      val paramInits = params.map(p => q"$AstInternal.initParam(${p.name})")
      applyBody += q"val node = new $name(..$paramInits, prototype = null, parent = _root_.scala.reflect.core.root, scratchpad = null, origin = origin)"
      applyBody ++= params.map(p => q"$AstInternal.storeField(node.${internalize(p)}, ${p.name})")
      applyBody += q"node"
      mstats1 += q"def apply(..$applyParams)(implicit origin: _root_.scala.reflect.core.Origin): $name = { ..$applyBody }"

      // step 9: generate Companion.unapply
      // TODO: migrate to name-based pattern matching
      val needsUnapply = !mstats.exists(stat => stat match { case DefDef(_, TermName("unapply"), _, _, _, _) => true; case _ => false })
      if (needsUnapply) {
        if (params.isEmpty) mstats1 += q"def unapply(x: $name): Boolean = true"
        else {
          val successTargs = tq"(..${params.map(p => p.tpt)})"
          val successArgs = q"(..${params.map(p => q"x.${p.name}")})"
          mstats1 += q"def unapply(x: $name): Option[$successTargs] = if (x == null) _root_.scala.None else _root_.scala.Some($successArgs)"
        }
      }

      val cdef1 = q"${finalize(mods1)} class $name[..$tparams] ${privatize(ctorMods)}(..$params1) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
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
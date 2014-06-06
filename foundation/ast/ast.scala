package org.scalareflect.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import org.scalareflect.unreachable

class ast extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AstMacros.impl
}

class AstMacros(val c: Context) {
  import c.universe._
  import Flag._
  val AdtInternal = q"_root_.org.scalareflect.adt.Internal"
  val AstInternal = q"_root_.org.scalareflect.ast.internal"
  def impl(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$rawparamss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val bparams1 = ListBuffer[ValDef]() // boilerplate params
      val paramss1 = ListBuffer[List[ValDef]]() // payload params
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
      if (rawparamss.length == 0) c.abort(cdef.pos, "@leaf classes must define a non-empty parameter list")

      // step 2: validate the body of the class
      val (defns, rest) = stats.partition(_.isDef)
      stats1 ++= defns
      val (requires, illegal) = rest.partition(_ match { case q"require($what)" => true; case _ => false })
      illegal.foreach(stmt => c.abort(stmt.pos, "only invariants and definitions are allowed in @ast classes"))

      // step 3: figure out relevant parameters, generate companion parameters for those with defaults
      def isTrivia(p: ValDef) = {
        // TODO: obviously, the toString check is very lame, but I'm having problems with c.typecheck here
        def isTriviaType(tpt: Tree) = tpt.toString == "trivia" || tpt.toString.endsWith(".trivia")
        p.mods.annotations.exists{ case q"new $tpt(...$argss)" => isTriviaType(tpt) }
      }
      val nontriviaParamss = rawparamss.map(_.filter(p => !isTrivia(p)))
      val nontriviaDefaultss = nontriviaParamss.map(_.filter(_.rhs.nonEmpty))
      def hasify(name: TermName) = TermName("has" + name.toString.capitalize)
      def uncapitalize(s: String) = if (s.length == 0) "" else { val chars = s.toCharArray; chars(0) = chars(0).toLower; new String(chars) }
      def unhasify(name: TermName) = TermName(uncapitalize(name.toString.stripPrefix("has")))
      val companionsForDefaultss = nontriviaDefaultss.map(_.map{
        case p @ q"$mods val $name: $tpt = $default" =>
          val Modifiers(flags, privateWithin, anns) = undefault(unoverride(mods))
          val anns1 = anns :+ q"new _root_.org.scalareflect.annotations.trivia"
          q"${Modifiers(flags, privateWithin, anns1)} val ${hasify(p.name)}: _root_.scala.Boolean"
      })
      def isVanilla(p: ValDef) = !isNontriviaDefault(p) && !isNontriviaCompanion(p)
      def isNontriviaDefault(p: ValDef) = nontriviaDefaultss.exists(_.exists(_.name == p.name))
      def isNontriviaCompanion(p: ValDef) = companionsForDefaultss.exists(_.exists(_.name == p.name))
      val paramss = rawparamss.zip(companionsForDefaultss).map{ case (raws, companions) => raws ++ companions }

      // step 4: create boilerplate bookkeeping parameters
      val scratchpadsType = tq"_root_.scala.collection.immutable.Map[_root_.scala.reflect.semantic.HostContext, _root_.scala.collection.immutable.Seq[Any]]"
      bparams1 += q"protected val internalPrototype: $name"
      bparams1 += q"protected val internalParent: _root_.scala.reflect.core.Tree"
      bparams1 += q"protected val internalScratchpads: $scratchpadsType"
      bparams1 += q"val origin: _root_.scala.reflect.core.Origin"
      def internalize(name: TermName) = TermName("_" + name.toString)
      val internalCopyInitss = paramss.map(_.map(p => q"$AstInternal.initField(this.${internalize(p.name)})"))
      val internalCopyBody = q"new ThisType(prototype.asInstanceOf[ThisType], parent, internalScratchpads, origin)(...$internalCopyInitss)"
      stats1 += q"private[core] def internalCopy(prototype: Tree = internalPrototype, parent: Tree = internalParent, scratchpads: $scratchpadsType = internalScratchpads, origin: _root_.scala.reflect.core.Origin = origin): ThisType = $internalCopyBody"

      // step 5: turn all parameters into private internal vars, create getters and setters
      paramss1 ++= paramss.map(_.map{ case p @ q"$mods val $name: $tpt = $default" => q"${undefault(unoverride(privatize(varify(mods))))} val ${internalize(p.name)}: $tpt" })
      stats1 ++= paramss.zipWithIndex.flatMap { case (params, i) =>
        params.flatMap { p =>
          val pstats = ListBuffer[Tree]()
          val pinternal = internalize(p.name)
          val pmods = if (p.mods.hasFlag(OVERRIDE)) Modifiers(OVERRIDE) else NoMods
          pstats += q"""
            $pmods def ${p.name}: ${p.tpt} = {
              $AstInternal.loadField(this.$pinternal)
              this.$pinternal
            }
          """
          def generateCow(cowName: TermName, cowParams: List[ValDef], action: Tree => Tree): Tree = {
            val preArgss = paramss.take(i).map(_.map(p => q"this.${p.name}"))
            val cowArgs = List(AssignOrNamedArg(q"${p.name}", action(q"this.${p.name}")))
            val postArgss = paramss.drop(i + 1).map(_.map(p => q"this.${p.name}"))
            val cowArgss = preArgss ++ List(cowArgs) ++ postArgss
            q"def $cowName(..$cowParams)(implicit origin: _root_.scala.reflect.core.Origin): ThisType = this.copy(...$cowArgss)"
          }
          if (isVanilla(p)) {
            pstats += generateCow(TermName("with" + p.name.toString.capitalize), List(q"val ${p.name}: ${p.tpt}"), pref => pref)
            pstats += generateCow(TermName("map" + p.name.toString.capitalize), List(q"val f: ${p.tpt} => ${p.tpt}"), pref => q"f($pref)")
          } else if (isNontriviaDefault(p)) {
            pstats += generateCow(TermName("with" + p.name.toString.capitalize), List(q"val ${p.name}: ${p.tpt}"), pref => pref)
            pstats += generateCow(TermName("without" + p.name.toString.capitalize), Nil, pref => q"null")
            pstats += generateCow(TermName("map" + p.name.toString.capitalize), List(q"val f: ${p.tpt} => ${p.tpt}"), pref => q"f($pref)")
          } else if (isNontriviaCompanion(p)) {
            // NOTE: generate no setters for non-trivia companion parameters
          } else {
            unreachable
          }
          pstats.toList
        }
      }
      val copyParamss = rawparamss.map(_.map(p => q"val ${p.name}: ${p.tpt} = this.${p.name}"))
      val copyArgss = rawparamss.map(_.map(p => q"${p.name}"))
      // TODO: would be useful to turn copy, mapXXX and withXXX into macros, so that their calls are guaranteed to be inlined
      stats1 += q"def copy(...$copyParamss)(implicit origin: _root_.scala.reflect.core.Origin): ThisType = $mname.apply(...$copyArgss)(_root_.scala.reflect.core.Origin.Transform(this, this.origin))"

      // step 7: generate boilerplate required by the @adt infrastructure
      stats1 += q"override type ThisType = $name"
      stats1 += q"def $$tag: _root_.scala.Int = $AdtInternal.calculateTag[ThisType]"
      anns1 += q"new $AdtInternal.leaf"
      manns1 += q"new $AdtInternal.leaf"

      // step 8: implement Product
      val productParamss = nontriviaParamss
      parents1 += tq"_root_.scala.Product"
      stats1 += q"override def productPrefix: _root_.scala.Predef.String = $AstInternal.productPrefix[ThisType]"
      stats1 += q"override def productArity: _root_.scala.Int = ${productParamss.head.length}"
      val pelClauses = ListBuffer[Tree]()
      pelClauses ++= 0.to(productParamss.head.length - 1).map(i => cq"$i => this.${productParamss.head(i).name}")
      pelClauses += cq"_ => throw new _root_.scala.IndexOutOfBoundsException(n.toString)"
      stats1 += q"override def productElement(n: _root_.scala.Int): Any = n match { case ..$pelClauses }"
      stats1 += q"override def productIterator: _root_.scala.Iterator[_root_.scala.Any] = _root_.scala.runtime.ScalaRunTime.typedProductIterator(this)"

      // step 9: implement equality
      stats1 += q"override def canEqual(that: _root_.scala.Any): _root_.scala.Boolean = that.isInstanceOf[ThisType]"
      stats1 += q"override def equals(that: _root_.scala.Any): _root_.scala.Boolean = this eq that.asInstanceOf[AnyRef]"
      stats1 += q"override def hashCode: _root_.scala.Int = _root_.java.lang.System.identityHashCode(this)"

      // step 10: generate Companion.apply
      // TODO: using nulls as marker default values is dubious. we need to look into other ways of implementing this!!
      val applyParamss = rawparamss.map(_.map(p => q"@..${p.mods.annotations} val ${p.name}: ${p.tpt} = ${if (isNontriviaDefault(p)) q"null" else p.rhs}"))
      val internalParamss = paramss.map(_.map(p => q"@..${p.mods.annotations} val ${p.name}: ${p.tpt}"))
      val internalBody = ListBuffer[Tree]()
      val internalLocalss = paramss.map(_.map(p => (p.name, internalize(p.name))))
      internalBody += q"$AdtInternal.hierarchyCheck[$name]"
      // internalBody += q"$AdtInternal.immutabilityCheck[$name]"
      internalBody ++= internalLocalss.flatten.map{ case (local, internal) => q"$AdtInternal.nullCheck($local)" }
      internalBody ++= internalLocalss.flatten.map{ case (local, internal) => q"$AdtInternal.emptyCheck($local)" }
      internalBody ++= requires
      val paramInitss = internalLocalss.map(_.map{ case (local, internal) => q"$AstInternal.initParam($local)" })
      internalBody += q"val node = new $name(null, null, _root_.scala.collection.immutable.Map(), origin)(...$paramInitss)"
      internalBody ++= internalLocalss.flatten.map{ case (local, internal) => q"$AstInternal.storeField(node.$internal, $local)" }
      internalBody += q"node"
      val internalArgss = paramss.map(_.map(p => {
        if (isVanilla(p)) q"${p.name}"
        else if (isNontriviaDefault(p)) q"if (${p.name} != null) ${p.name} else ${p.rhs}"
        else if (isNontriviaCompanion(p)) q"${unhasify(p.name)} != null"
        else unreachable
      }))
      mstats1 += q"""
        def apply(...$applyParamss)(implicit origin: _root_.scala.reflect.core.Origin): $name = {
          def internal(...$internalParamss): $name = {
            ..$internalBody
          }
          internal(...$internalArgss)
        }
      """

      // step 11: generate Companion.unapply
      // TODO: migrate to name-based pattern matching
      val unapplyParamss = nontriviaParamss
      val needsUnapply = !mstats.exists(stat => stat match { case DefDef(_, TermName("unapply"), _, _, _, _) => true; case _ => false })
      if (needsUnapply) {
        if (paramss.head.isEmpty) mstats1 += q"def unapply(x: $name): Boolean = true"
        else {
          val successTargs = tq"(..${unapplyParamss.head.map(p => p.tpt)})"
          val successArgs = q"(..${unapplyParamss.head.map(p => q"x.${p.name}")})"
          mstats1 += q"def unapply(x: $name): Option[$successTargs] = if (x == null) _root_.scala.None else _root_.scala.Some($successArgs)"
        }
      }

      // TODO: swap bparams1 and paramss1
      val cdef1 = q"${finalize(mods1)} class $name[..$tparams] ${privatize(ctorMods)}(...${bparams1 +: paramss1}) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
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
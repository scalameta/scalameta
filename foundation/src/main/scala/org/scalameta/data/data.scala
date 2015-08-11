package org.scalameta.data

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer

class data extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DataMacros.data
}

class DataMacros(val c: Context) {
  import c.universe._
  import definitions._
  import Flag._
  val Public = q"_root_.org.scalameta.data"
  val Internal = q"_root_.org.scalameta.adt.Internal"

  def data(annottees: Tree*): Tree = {
    def transformDataClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val parents1 = ListBuffer[Tree]() ++ parents
      val stats1 = ListBuffer[Tree]() ++ stats
      val anns1 = ListBuffer[Tree]() ++ mods.annotations
      def mods1 = mods.mapAnnotations(_ => anns1.toList)
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      val mstats1 = ListBuffer[Tree]() ++ mstats
      def unprivatize(mods: Modifiers) = Modifiers((mods.flags.asInstanceOf[Long] & (~scala.reflect.internal.Flags.LOCAL) & (~scala.reflect.internal.Flags.PRIVATE)).asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
      def privatize(mods: Modifiers) = Modifiers((mods.flags.asInstanceOf[Long] | scala.reflect.internal.Flags.LOCAL & scala.reflect.internal.Flags.PRIVATE).asInstanceOf[FlagSet], mods.privateWithin, mods.annotations)
      def byNeed(mods: Modifiers) = Modifiers(mods.flags, mods.privateWithin, mods.annotations :+ q"new $Internal.byNeedField")
      def finalize(mods: Modifiers) = Modifiers(mods.flags | FINAL, mods.privateWithin, mods.annotations)
      def varify(mods: Modifiers) = Modifiers(mods.flags | MUTABLE, mods.privateWithin, mods.annotations)
      def needs(name: Name) = {
        val q"new $_(...$argss).macroTransform(..$_)" = c.macroApplication
        val banIndicator = argss.flatten.find {
          case AssignOrNamedArg(Ident(TermName(param)), Literal(Constant(false))) => param == name.toString
          case _ => false
        }
        val ban = banIndicator.map(_ => true).getOrElse(false)
        val presenceIndicator = stats.collectFirst { case mdef: MemberDef if mdef.name == name => mdef }
        val present = presenceIndicator.map(_ => true).getOrElse(false)
        !ban && !present
      }

      object VanillaParam {
        def unapply(tree: ValDef): Option[(Modifiers, TermName, Tree, Tree)] = tree match {
          case ByNeedParam(_, _, _, _) => None
          case VarargParam(_, _, _, _) => None
          case _ => Some((tree.mods, tree.name, tree.tpt, tree.rhs))
        }
      }
      object VarargParam {
        def unapply(tree: ValDef): Option[(Modifiers, TermName, Tree, Tree)] = tree.tpt match {
          case Vararg(_) => Some((tree.mods, tree.name, tree.tpt, tree.rhs))
          case _ => None
        }
      }
      object ByNeedParam {
        def unapply(tree: ValDef): Option[(Modifiers, TermName, Tree, Tree)] = tree.tpt match {
          case Annotated(Apply(Select(New(Ident(TypeName("byNeed"))), termNames.CONSTRUCTOR), List()), tpt) =>
            if (Vararg.unapply(tree.tpt).nonEmpty) c.abort(cdef.pos, "vararg parameters cannot be by-need")
            else Some((tree.mods, tree.name, tpt, tree.rhs))
          case _ =>
            None
        }
      }
      def unByNeed(tree: ValDef): ValDef = tree match {
        case ByNeedParam(mods, name, tpt, default) => ValDef(mods, name, tpt, default)
        case _ => tree.duplicate
      }
      def byNameTpt(tpt: Tree): Tree = {
        val DefDef(_, _, _, List(List(ValDef(_, _, byNameTpt, _))), _, _) = q"def dummy(dummy: => $tpt) = ???"
        byNameTpt
      }
      object Vararg {
        def unapply(tpt: Tree): Option[Tree] = tpt match {
          case Annotated(_, arg) =>
            unapply(arg)
          case AppliedTypeTree(Select(Select(Ident(root), scala), repeated), List(arg))
          if root == termNames.ROOTPKG && scala == TermName("scala") && repeated == definitions.RepeatedParamClass.name.decodedName =>
            Some(arg)
          case _ =>
            None
        }
      }
      val isVararg = paramss.flatten.lastOption.flatMap(p => Vararg.unapply(p.tpt)).nonEmpty

      // step 1: validate the shape of the class
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @data classes")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "final is redundant for @data classes")
      if (mods.hasFlag(CASE)) c.abort(cdef.pos, "case is redundant for @data classes")
      if (mods.hasFlag(ABSTRACT)) c.abort(cdef.pos, "@data classes cannot be abstract")
      if (ctorMods.flags != NoFlags) c.abort(cdef.pos, "@data classes must define a public primary constructor")
      if (paramss.length == 0) c.abort(cdef.pos, "@data classes must define a non-empty parameter list")

      // step 2: create parameters, generate getters and validation checks
      val paramss1 = paramss.map(_.map{
        case VanillaParam(mods, name, tpt, default) =>
          stats1 += q"$Internal.nullCheck(this.$name)"
          stats1 += q"$Internal.emptyCheck(this.$name)"
          q"${unprivatize(mods)} val $name: $tpt = $default"
        case VarargParam(mods, name, tpt, default) =>
          stats1 += q"$Internal.nullCheck(this.$name)"
          stats1 += q"$Internal.emptyCheck(this.$name)"
          q"${unprivatize(mods)} val $name: $tpt = $default"
        case ByNeedParam(mods, name, tpt, default) =>
          val flagName = TermName(name + "Flag")
          val valueName = TermName(name + "Value")
          val storageName = TermName(name + "Storage")
          val getterName = name
          val paramName = TermName("_" + name)
          val paramTpt = tq"_root_.scala.Function0[$tpt]"
          stats1 += q"@$Internal.byNeedField private[this] var $flagName: _root_.scala.Boolean = false"
          stats1 += q"@$Internal.byNeedField private[this] var $storageName: $tpt = _"
          stats1 += q"""
            def $getterName = {
              if (!this.$flagName) {
                val $valueName = this.$paramName()
                $Internal.nullCheck($valueName)
                $Internal.emptyCheck($valueName)
                this.$paramName = null
                this.$storageName = $valueName
                this.$flagName = true
              }
              this.$storageName
            }
          """
          q"${varify(byNeed(privatize(mods)))} val $paramName: $paramTpt = $default"
      })

      // step 3: implement Object
      if (needs(TermName("toString"))) {
        stats1 += q"override def toString: _root_.scala.Predef.String = _root_.scala.runtime.ScalaRunTime._toString(this)"
      }
      if (needs(TermName("hashCode"))) {
        stats1 += q"override def hashCode: _root_.scala.Int = _root_.scala.runtime.ScalaRunTime._hashCode(this)"
      }
      if (needs(TermName("equals"))) {
        stats1 += q"override def canEqual(other: _root_.scala.Any): _root_.scala.Boolean = other.isInstanceOf[$name]"
        stats1 += q"""
          override def equals(other: _root_.scala.Any): _root_.scala.Boolean = (
            this.canEqual(other) && _root_.scala.runtime.ScalaRunTime._equals(this, other)
          )
        """
      }

      // step 4: implement Product
      parents1 += tq"_root_.scala.Product"
      if (needs(TermName("product"))) {
        val productParamss = paramss.map(_.map(_.duplicate))
        stats1 += q"override def productPrefix: _root_.scala.Predef.String = ${name.toString}"
        stats1 += q"override def productArity: _root_.scala.Int = ${paramss.head.length}"
        val pelClauses = ListBuffer[Tree]()
        pelClauses ++= 0.to(productParamss.head.length - 1).map(i => cq"$i => this.${productParamss.head(i).name}")
        pelClauses += cq"_ => throw new _root_.scala.IndexOutOfBoundsException(n.toString)"
        stats1 += q"override def productElement(n: _root_.scala.Int): Any = n match { case ..$pelClauses }"
        stats1 += q"override def productIterator: _root_.scala.Iterator[_root_.scala.Any] = _root_.scala.runtime.ScalaRunTime.typedProductIterator(this)"
      }

      // step 5: generate copy
      if (needs(TermName("copy")) && !isVararg) {
        val copyParamss = paramss.map(_.map({
          case VanillaParam(mods, name, tpt, default) => q"$mods val $name: $tpt = this.$name"
          case VarargParam(mods, name, tpt, default) => q"$mods val $name: $tpt = this.$name"
          // TODO: This doesn't compile, producing nonsensical errors
          // about incompatibility between the default value and the type of the parameter
          // e.g. "expected: => T, actual: T"
          // Therefore, I'm making the parameter of copy eager, even though I'd like it to be lazy.
          // case ByNeedParam(mods, name, tpt, default) => q"$mods val $name: ${byNameTpt(tpt)} = this.$name"
          case ByNeedParam(mods, name, tpt, default) => q"$mods val $name: $tpt = this.$name"
        }))
        val copyArgss = paramss.map(_.map({
          case VanillaParam(mods, name, tpt, default) => q"$name"
          case VarargParam(mods, name, tpt, default) => q"$name: _*"
          case ByNeedParam(mods, name, tpt, default) => q"(() => $name)"
        }))
        stats1 += q"def copy(...$copyParamss): $name = new $name(...$copyArgss)"
      }

      // step 6: generate Companion.apply
      if (needs(TermName("apply"))) {
        val applyParamss = paramss.map(_.map({
          case VanillaParam(mods, name, tpt, default) => q"$mods val $name: $tpt = $default"
          case VarargParam(mods, name, tpt, default) => q"$mods val $name: $tpt = $default"
          case ByNeedParam(mods, name, tpt, default) => q"$mods val $name: ${byNameTpt(tpt)} = $default"
        }))
        val applyArgss = paramss.map(_.map({
          case VanillaParam(mods, name, tpt, default) => q"$name"
          case VarargParam(mods, name, tpt, default) => q"$name: _*"
          case ByNeedParam(mods, name, tpt, default) => q"(() => $name)"
        }))
        mstats1 += q"def apply(...$applyParamss): $name = new $name(...$applyArgss)"
      }

      // step 7: generate Companion.unapply
      // TODO: go for name-based pattern matching once blocking bugs (e.g. SI-9029) are fixed
      val unapplyName = if (isVararg) TermName("unapplySeq") else TermName("unapply")
      if (needs(unapplyName)) {
        val unapplyParamss = paramss.map(_.map(unByNeed))
        val unapplyParams = unapplyParamss.head
        if (unapplyParams.length != 0) {
          val successTargs = unapplyParams.map({
            case VanillaParam(mods, name, tpt, default) => tpt
            case VarargParam(mods, name, Vararg(tpt), default) => tq"_root_.scala.Seq[$tpt]"
            case ByNeedParam(mods, name, tpt, default) => tpt
          })
          val successTpe = tq"(..$successTargs)"
          val successArgs = q"(..${unapplyParams.map(p => q"x.${p.name}")})"
          mstats1 += q"""
            def $unapplyName(x: $name): Option[$successTpe] = {
              if (x == null) _root_.scala.None
              else _root_.scala.Some($successArgs)
            }
          """
        } else {
          mstats1 += q"def $unapplyName(x: $name): Boolean = true"
        }
      }

      val cdef1 = q"${finalize(mods1)} class $name[..$tparams] $ctorMods(...$paramss1) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
      val mdef1 = q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }

    def transformDataModule(mdef: ModuleDef): ModuleDef = {
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      def casify(mods: Modifiers) = Modifiers(mods.flags | CASE, mods.privateWithin, mods.annotations)

      // step 1: validate the shape of the module
      if (mmods.hasFlag(FINAL)) c.abort(mdef.pos, "final is redundant for @leaf classes")
      if (mmods.hasFlag(CASE)) c.abort(mdef.pos, "case is redundant for @leaf classes")

      // TODO: later on we could migrate data modules to hand-rolled codegen, much like data classes.
      // However, for now it's quite fine to implement them as case objects.

      q"${casify(mmods)} object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }"
    }

    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef @ ModuleDef(_, _, _)) :: rest if !(mods hasFlag TRAIT) => transformDataClass(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, name, _, _)) :: rest if !mods.hasFlag(TRAIT) => transformDataClass(cdef, q"object ${name.toTermName}") ++ rest
      case (mdef @ ModuleDef(_, _, _)) :: rest => transformDataModule(mdef) +: rest
      case annottee :: rest => c.abort(annottee.pos, "only classes and objects can be @data")
    }
    q"{ ..$expanded; () }"
  }
}


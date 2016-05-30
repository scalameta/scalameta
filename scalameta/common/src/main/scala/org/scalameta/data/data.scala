package org.scalameta.data

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import org.scalameta.internal.MacroHelpers

// Virtually the same as the `case' modifier:
// can be used on both classes and objects with very similar effect.
//
// The main different is customizability - we can stuff any extensions we want into @data.
// Currently it's just two things:
//    1) Support for lazy parameters (implemented via a @byNeed marker annotation),
//       as in e.g.: `@leaf class Nonrecursive(tpe: Type.Arg @byNeed) extends Typing`.
//       NOTE: @byNeed isn't defined anywhere - it's just a syntactic marker.
//    2) Support for on-demand member generation via named parameters to @data,
//       as in e.g. `@data(toString = false) class ...`.
//
// Performance of pattern matching may be subpar until SI-9029 is fixed,
// as well as some minor semantic details may differ.
class data extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DataMacros.data
}

class none extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DataMacros.none
}

class DataMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  import Flag._

  def data(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val parents1 = ListBuffer[Tree]() ++ parents
      val stats1 = ListBuffer[Tree]() ++ stats
      val anns1 = ListBuffer[Tree]() ++ mods.annotations
      def mods1 = mods.mapAnnotations(_ => anns1.toList)
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      val mstats1 = ListBuffer[Tree]() ++ mstats

      implicit class XtensionDataModifiers(mods: Modifiers) {
        def mkByneed = Modifiers(mods.flags, mods.privateWithin, mods.annotations :+ q"new $AdtMetadataModule.byNeedField")
      }
      def needs(name: Name, companion: Boolean, duplicate: Boolean) = {
        val q"new $_(...$argss).macroTransform(..$_)" = c.macroApplication
        val banIndicator = argss.flatten.find {
          case AssignOrNamedArg(Ident(TermName(param)), Literal(Constant(false))) => param == name.toString
          case _ => false
        }
        val ban = banIndicator.map(_ => true).getOrElse(false)
        val presenceIndicator = {
          val where = if (companion) mstats else stats
          where.collectFirst { case mdef: MemberDef if mdef.name == name => mdef }
        }
        val present = presenceIndicator.map(_ => true).getOrElse(false)
        !ban && (duplicate || !present)
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
      val itparams = tparams.map{ case q"$mods type $name[..$tparams] >: $low <: $high" => q"${mods.unVariant} type $name[..$tparams] >: $low <: $high" }
      val tparamrefs = tparams.map(tparam => Ident(tparam.name))

      // step 1: validate the shape of the class
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @data classes")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "final is redundant for @data classes")
      if (mods.hasFlag(CASE)) c.abort(cdef.pos, "case is redundant for @data classes")
      if (mods.hasFlag(ABSTRACT)) c.abort(cdef.pos, "@data classes cannot be abstract")
      if (paramss.length == 0) c.abort(cdef.pos, "@data classes must define a non-empty parameter list")

      // step 2: create parameters, generate getters and validation checks
      val paramss1 = paramss.map(_.map{
        case VanillaParam(mods, name, tpt, default) =>
          stats1 += q"$DataTyperMacrosModule.nullCheck(this.$name)"
          stats1 += q"$DataTyperMacrosModule.emptyCheck(this.$name)"
          q"${mods.unPrivate} val $name: $tpt = $default"
        case VarargParam(mods, name, tpt, default) =>
          stats1 += q"$DataTyperMacrosModule.nullCheck(this.$name)"
          stats1 += q"$DataTyperMacrosModule.emptyCheck(this.$name)"
          q"${mods.unPrivate} val $name: $tpt = $default"
        case ByNeedParam(mods, name, tpt, default) =>
          val flagName = TermName(name + "Flag")
          val statusName = TermName("is" + name.toString.capitalize + "Loaded")
          val valueName = TermName(name + "Value")
          val storageName = TermName(name + "Storage")
          val getterName = name
          val paramName = TermName("_" + name)
          val paramTpt = tq"_root_.scala.Function0[$tpt]"
          stats1 += q"@$AdtMetadataModule.byNeedField private[this] var $flagName: _root_.scala.Boolean = false"
          stats1 += q"def $statusName: _root_.scala.Boolean = this.$flagName"
          stats1 += q"@$AdtMetadataModule.byNeedField private[this] var $storageName: $tpt = _"
          stats1 += q"""
            def $getterName = {
              if (!this.$flagName) {
                val $valueName = this.$paramName()
                $DataTyperMacrosModule.nullCheck($valueName)
                $DataTyperMacrosModule.emptyCheck($valueName)
                this.$paramName = null
                this.$storageName = $valueName
                this.$flagName = true
              }
              this.$storageName
            }
          """
          q"${mods.mkPrivate.mkByneed.mkMutable} val $paramName: $paramTpt = $default"
      })

      // step 3: implement Object
      if (needs(TermName("toString"), companion = false, duplicate = false)) {
        stats1 += q"override def toString: _root_.scala.Predef.String = _root_.scala.runtime.ScalaRunTime._toString(this)"
      }
      if (needs(TermName("hashCode"), companion = false, duplicate = false)) {
        stats1 += q"override def hashCode: _root_.scala.Int = _root_.scala.runtime.ScalaRunTime._hashCode(this)"
      }
      if (needs(TermName("equals"), companion = false, duplicate = false)) {
        stats1 += q"override def canEqual(other: _root_.scala.Any): _root_.scala.Boolean = other.isInstanceOf[$name[..$tparamrefs]]"
        stats1 += q"""
          override def equals(other: _root_.scala.Any): _root_.scala.Boolean = (
            this.canEqual(other) && _root_.scala.runtime.ScalaRunTime._equals(this, other)
          )
        """
      }

      // step 4: implement Product
      parents1 += tq"_root_.scala.Product"
      if (needs(TermName("product"), companion = false, duplicate = false)) {
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
      if (needs(TermName("copy"), companion = false, duplicate = false) && !isVararg) {
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
        stats1 += q"def copy[..$itparams](...$copyParamss): $name[..$tparamrefs] = new $name[..$tparamrefs](...$copyArgss)"
      }

      // step 6: generate Companion.apply
      // TODO: try change this to duplicate=true and see what happens
      if (needs(TermName("apply"), companion = true, duplicate = false)) {
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
        mstats1 += q"def apply[..$itparams](...$applyParamss): $name[..$tparamrefs] = new $name[..$tparamrefs](...$applyArgss)"
      }

      // step 7: generate Companion.unapply
      // TODO: go for name-based pattern matching once blocking bugs (e.g. SI-9029) are fixed
      val unapplyName = if (isVararg) TermName("unapplySeq") else TermName("unapply")
      if (needs(TermName("unapply"), companion = true, duplicate = false) &&
          needs(TermName("unapplySeq"), companion = true, duplicate = false)) {
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
            def $unapplyName[..$itparams](x: $name[..$tparamrefs]): Option[$successTpe] = {
              if (x == null) _root_.scala.None
              else _root_.scala.Some($successArgs)
            }
          """
        } else {
          mstats1 += q"def $unapplyName(x: $name): Boolean = true"
        }
      }

      val cdef1 = q"${mods1.mkFinal} class $name[..$tparams] $ctorMods(...$paramss1) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
      val mdef1 = q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }

    override def transformModule(mdef: ModuleDef): ModuleDef = {
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef

      // step 1: validate the shape of the module
      if (mmods.hasFlag(FINAL)) c.abort(mdef.pos, "final is redundant for @data objects")
      if (mmods.hasFlag(CASE)) c.abort(mdef.pos, "case is redundant for @data objects")

      // TODO: later on we could migrate data modules to hand-rolled codegen, much like data classes.
      // However, for now it's quite fine to implement them as case objects.

      q"${mmods.mkCase} object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }"
    }
  })

  def none(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformModule(mdef: ModuleDef): ModuleDef = {
      val q"new $_(...$argss).macroTransform(..$_)" = c.macroApplication
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      val mstats1 = ListBuffer[Tree]() ++ mstats

      manns1 += q"new $DataAnnotation"
      mstats1 += q"override def isEmpty: $BooleanClass = true"

      q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
    }
  })
}


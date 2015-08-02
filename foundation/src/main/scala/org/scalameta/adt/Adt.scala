package org.scalameta.adt

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import org.scalameta.invariants.nonEmpty
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import org.scalameta.adt.{Reflection => AdtReflection}

class root extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.root
}

class monadicRoot extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.monadicRoot
}

class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.branch
}

class leaf extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.leaf
}

class noneLeaf extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.noneLeaf
}

class someLeaf extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AdtMacros.someLeaf
}

class AdtMacros(val c: Context) {
  import c.universe._
  import definitions._
  import Flag._
  val Public = q"_root_.org.scalameta.adt"
  val Internal = q"_root_.org.scalameta.adt.Internal"

  def root(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      val ModuleDef(mmods, mname, Template(mparents, mself, mstats)) = mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @root traits")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@root traits cannot be final")
      val flags1 = flags | SEALED
      val needsThisType = stats.collect{ case TypeDef(_, TypeName("ThisType"), _, _) => () }.isEmpty
      if (needsThisType) stats1 += q"type ThisType <: $name"
      stats1 += q"def internalTag: _root_.scala.Int"
      mstats1 += q"$Internal.hierarchyCheck[$name]"
      val anns1 = anns :+ q"new $Internal.root"
      val parents1 = parents :+ tq"$Internal.Adt" :+ tq"_root_.scala.Product" :+ tq"_root_.scala.Serializable"

      val cdef1 = ClassDef(Modifiers(flags1, privateWithin, anns1), name, tparams, Template(parents1, self, stats1.toList))
      val mdef1 = ModuleDef(mmods, mname, Template(mparents, mself, mstats1.toList))
      List(cdef1, mdef1)
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef: ModuleDef) :: rest if mods.hasFlag(TRAIT) => transform(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef, q"object ${cdef.name.toTermName}") ++ rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @root")
    }
    q"{ ..$expanded; () }"
  }

  def monadicRoot(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      val ModuleDef(mmods, mname, Template(mparents, mself, mstats)) = mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      val anns1 = anns :+ q"new $Public.root" :+ q"new $Internal.monadicRoot"
      stats1 += q"def map(fn: this.ContentType => this.ContentType): $name"
      stats1 += q"def flatMap(fn: this.ContentType => $name): $name"

      val contentTypes = mstats.collect {
        case q"${Modifiers(_, _, anns)} class $_[..$_] $_($_: $tpt) extends { ..$_ } with ..$_ { $_ => ..$_ }"
        if anns.exists({ case q"new someLeaf" => true; case _ => false }) =>
          tpt
      }
      var contentType = contentTypes match {
        case Nil => c.abort(cdef.pos, s"no @someLeaf classes found in $name's companion object")
        case List(contentType) => contentType
        case _ => c.abort(cdef.pos, s"multiple @someLeaf classes found in $name's companion object")
      }
      contentType = contentType match {
        case Annotated(_, contentType) => contentType
        case contentType => contentType
      }
      stats1 += q"type ContentType = $mname.ContentType"
      mstats1 += q"type ContentType = $contentType"

      val cdef1 = ClassDef(Modifiers(flags, privateWithin, anns1), name, tparams, Template(parents, self, stats1.toList))
      val mdef1 = ModuleDef(mmods, mname, Template(mparents, mself, mstats1.toList))
      List(cdef1, mdef1)
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef: ModuleDef) :: rest if mods.hasFlag(TRAIT) => transform(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef, q"object ${cdef.name.toTermName}") ++ rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @root")
    }
    q"{ ..$expanded; () }"
  }

  def branch(annottees: Tree*): Tree = {
    def transform(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      val ModuleDef(mmods, mname, Template(mparents, mself, mstats)) = mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @branch traits")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@branch traits cannot be final")
      val flags1 = flags | SEALED
      stats1 += q"type ThisType <: $name"
      mstats1 += q"$Internal.hierarchyCheck[$name]"
      val anns1 = anns :+ q"new $Internal.branch"

      val cdef1 = ClassDef(Modifiers(flags1, privateWithin, anns1), name, tparams, Template(parents, self, stats1.toList))
      val mdef1 = ModuleDef(mmods, mname, Template(mparents, mself, mstats1.toList))
      List(cdef1, mdef1)
    }
    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef: ModuleDef) :: rest if mods.hasFlag(TRAIT) => transform(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, _, _, _)) :: rest if mods.hasFlag(TRAIT) => transform(cdef, q"object ${cdef.name.toTermName}") ++ rest
      case annottee :: rest => c.abort(annottee.pos, "only traits can be @branch")
    }
    q"{ ..$expanded; () }"
  }

  def leaf(annottees: Tree*): Tree = {
    def transformLeafClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
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
      def delay(mods: Modifiers) = Modifiers(mods.flags, mods.privateWithin, mods.annotations :+ q"new $Internal.delayedField")
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
          case DelayedParam(_, _, _, _) => None
          case _ => Some((tree.mods, tree.name, tree.tpt, tree.rhs))
        }
      }
      object DelayedParam {
        def unapply(tree: ValDef): Option[(Modifiers, TermName, Tree, Tree)] = tree.tpt match {
          case Annotated(Apply(Select(New(Ident(TypeName("delayed"))), termNames.CONSTRUCTOR), List()), tpt) =>
            Some((tree.mods, tree.name, tpt, tree.rhs))
          case _ =>
            None
        }
      }
      def undelay(tree: ValDef): ValDef = tree match {
        case DelayedParam(mods, name, tpt, default) => ValDef(mods, name, tpt, default)
        case _ => tree.duplicate
      }
      def bynameTpt(tpt: Tree): Tree = {
        val DefDef(_, _, _, List(List(ValDef(_, _, bynameTpt, _))), _, _) = q"def dummy(dummy: => $tpt) = ???"
        bynameTpt
      }

      // step 1: validate the shape of the class
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @leaf classes")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "final is redundant for @leaf classes")
      if (mods.hasFlag(CASE)) c.abort(cdef.pos, "case is redundant for @leaf classes")
      if (mods.hasFlag(ABSTRACT)) c.abort(cdef.pos, "@leaf classes cannot be abstract")
      if (ctorMods.flags != NoFlags) c.abort(cdef.pos, "@leaf classes must define a public primary constructor")
      if (paramss.length == 0) c.abort(cdef.pos, "@leaf classes must define a non-empty parameter list")

      // step 2: create parameters, generate getters and validation checks
      val paramss1 = paramss.map(_.map{
        case VanillaParam(mods, name, tpt, default) =>
          stats1 += q"$Internal.nullCheck(this.$name)"
          stats1 += q"$Internal.emptyCheck(this.$name)"
          q"${unprivatize(mods)} val $name: $tpt = $default"
        case DelayedParam(mods, name, tpt, default) =>
          val flagName = TermName(name + "Flag")
          val valueName = TermName(name + "Value")
          val storageName = TermName(name + "Storage")
          val getterName = name
          val paramName = TermName("_" + name)
          val paramTpt = tq"_root_.scala.Function0[$tpt]"
          stats1 += q"@$Internal.delayedField private[this] var $flagName: _root_.scala.Boolean = false"
          stats1 += q"@$Internal.delayedField private[this] var $storageName: $tpt = _"
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
          q"${varify(delay(privatize(mods)))} val $paramName: $paramTpt = $default"
      })

      // step 3: generate boilerplate required by the @adt infrastructure
      stats1 += q"override type ThisType = $name"
      stats1 += q"override def internalTag: _root_.scala.Int = $mname.internalTag"
      mstats1 += q"def internalTag: _root_.scala.Int = $Internal.calculateTag[$name]"
      mstats1 += q"$Internal.hierarchyCheck[$name]"
      mstats1 += q"$Internal.immutabilityCheck[$name]"
      anns1 += q"new $Internal.leafClass"
      manns1 += q"new $Internal.leafCompanion"
      parents1 += tq"_root_.scala.Product"

      // step 4: implement Object
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

      // step 5: implement Product
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

      // step 6: generate copy
      if (needs(TermName("copy"))) {
        val copyParamss = paramss.map(_.map({
          case VanillaParam(mods, name, tpt, default) => q"$mods val $name: $tpt = this.$name"
          // TODO: This doesn't compile, producing nonsensical errors
          // about incompatibility between the default value and the type of the parameter
          // e.g. "expected: => T, actual: T"
          // Therefore, I'm making the parameter of copy eager, even though I'd like it to be lazy.
          // case DelayedParam(mods, name, tpt, default) => q"$mods val $name: ${bynameTpt(tpt)} = this.$name"
          case DelayedParam(mods, name, tpt, default) => q"$mods val $name: $tpt = this.$name"
        }))
        val copyArgss = paramss.map(_.map({
          case VanillaParam(mods, name, tpt, default) => q"$name"
          case DelayedParam(mods, name, tpt, default) => q"(() => $name)"
        }))
        stats1 += q"def copy(...$copyParamss): $name = new $name(...$copyArgss)"
      }

      // step 7: generate Companion.apply
      if (needs(TermName("apply"))) {
        val applyParamss = paramss.map(_.map({
          case VanillaParam(mods, name, tpt, default) => q"$mods val $name: $tpt = $default"
          case DelayedParam(mods, name, tpt, default) => q"$mods val $name: ${bynameTpt(tpt)} = $default"
        }))
        val applyArgss = paramss.map(_.map({
          case VanillaParam(mods, name, tpt, default) => q"$name"
          case DelayedParam(mods, name, tpt, default) => q"(() => $name)"
        }))
        mstats1 += q"def apply(...$applyParamss): $name = new $name(...$applyArgss)"
      }

      // step 8: generate Companion.unapply
      // TODO: go for name-based pattern matching once blocking bugs (e.g. SI-9029) are fixed
      if (needs(TermName("unapply"))) {
        val unapplyParamss = paramss.map(_.map(undelay))
        val unapplyParams = unapplyParamss.head
        if (unapplyParams.length != 0) {
          val successTargs = tq"(..${unapplyParams.map(p => p.tpt)})"
          val successArgs = q"(..${unapplyParams.map(p => q"x.${p.name}")})"
          mstats1 += q"def unapply(x: $name): Option[$successTargs] = if (x == null) _root_.scala.None else _root_.scala.Some($successArgs)"
        } else {
          mstats1 += q"def unapply(x: $name): Boolean = true"
        }
      }

      val cdef1 = q"${finalize(mods1)} class $name[..$tparams] $ctorMods(...$paramss1) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
      val mdef1 = q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }

    def transformLeafModule(mdef: ModuleDef): ModuleDef = {
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      val mparents1 = ListBuffer[Tree]() ++ mparents
      val mstats1 = ListBuffer[Tree]() ++ mstats
      def casify(mods: Modifiers) = Modifiers(mods.flags | CASE, mods.privateWithin, mods.annotations)

      // step 1: validate the shape of the module
      if (mmods.hasFlag(FINAL)) c.abort(mdef.pos, "final is redundant for @leaf classes")
      if (mmods.hasFlag(CASE)) c.abort(mdef.pos, "case is redundant for @leaf classes")

      // step 2: generate boilerplate required by the @adt infrastructure
      mstats1 += q"override type ThisType = $mname.type"
      mstats1 += q"override def internalTag: _root_.scala.Int = $Internal.calculateTag[ThisType]"
      mstats1 += q"$Internal.hierarchyCheck[ThisType]"
      mstats1 += q"$Internal.immutabilityCheck[ThisType]"
      manns1 += q"new $Internal.leafClass"
      mparents1 += tq"_root_.scala.Product"

      q"${casify(mmods1)} object $mname extends { ..$mearlydefns } with ..$mparents1 { $mself => ..$mstats1 }"
    }

    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef @ ModuleDef(_, _, _)) :: rest if !(mods hasFlag TRAIT) => transformLeafClass(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, name, _, _)) :: rest if !mods.hasFlag(TRAIT) => transformLeafClass(cdef, q"object ${name.toTermName}") ++ rest
      case (mdef @ ModuleDef(_, _, _)) :: rest => transformLeafModule(mdef) +: rest
      case annottee :: rest => c.abort(annottee.pos, "only classes and objects can be @leaf")
    }
    q"{ ..$expanded; () }"
  }

  def noneLeaf(annottees: Tree*): Tree = {
    def transformLeafClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val rname = TypeName(c.internal.enclosingOwner.name.toString)
      val rmname = rname.toTermName
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val anns1 = ListBuffer[Tree]() ++ mods.annotations
      def mods1 = mods.mapAnnotations(_ => anns1.toList)
      val stats1 = ListBuffer[Tree]() ++ stats

      if (paramss.flatten.nonEmpty) c.abort(cdef.pos, "noneLeafs can't have parameters")
      anns1 += q"new $Public.leaf"
      anns1 += q"new $Internal.noneLeaf"
      stats1 += q"override def map(fn: this.ContentType => this.ContentType): $rname = $mname()"
      stats1 += q"override def flatMap(fn: this.ContentType => $rname): $rname = $mname()"

      val cdef1 = q"$mods1 class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      List(cdef1, mdef)
    }

    def transformLeafModule(mdef: ModuleDef): ModuleDef = {
      val rname = TypeName(c.internal.enclosingOwner.name.toString)
      val rmname = rname.toTermName
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      val mstats1 = ListBuffer[Tree]() ++ mstats

      manns1 += q"new $Public.leaf"
      manns1 += q"new $Internal.noneLeaf"
      mstats1 += q"override def map(fn: this.ContentType => this.ContentType): $rname = $mname"
      mstats1 += q"override def flatMap(fn: this.ContentType => $rname): $rname = $mname"

      q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
    }

    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef @ ModuleDef(_, _, _)) :: rest if !(mods hasFlag TRAIT) => transformLeafClass(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, name, _, _)) :: rest if !mods.hasFlag(TRAIT) => transformLeafClass(cdef, q"object ${name.toTermName}") ++ rest
      case (mdef @ ModuleDef(_, _, _)) :: rest => transformLeafModule(mdef) +: rest
      case annottee :: rest => c.abort(annottee.pos, "only classes and objects can be @leaf")
    }
    q"{ ..$expanded; () }"
  }

  def someLeaf(annottees: Tree*): Tree = {
    def transformLeafClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val rname = TypeName(c.internal.enclosingOwner.name.toString)
      val rmname = rname.toTermName
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val anns1 = ListBuffer[Tree]() ++ mods.annotations
      def mods1 = mods.mapAnnotations(_ => anns1.toList)
      val stats1 = ListBuffer[Tree]() ++ stats

      if (paramss.flatten.length != 1) c.abort(cdef.pos, "someLeafs must have exactly one parameter")
      val param = paramss.flatten.head
      anns1 += q"new $Public.leaf"
      anns1 += q"new $Internal.someLeaf"
      stats1 += q"override def map(fn: this.ContentType => this.ContentType): $rname = $mname(fn(this.${param.name}))"
      stats1 += q"override def flatMap(fn: this.ContentType => $rname): $rname = fn(this.${param.name})"

      val cdef1 = q"$mods1 class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      List(cdef1, mdef)
    }

    val expanded = annottees match {
      case (cdef @ ClassDef(mods, _, _, _)) :: (mdef @ ModuleDef(_, _, _)) :: rest if !(mods hasFlag TRAIT) => transformLeafClass(cdef, mdef) ++ rest
      case (cdef @ ClassDef(mods, name, _, _)) :: rest if !mods.hasFlag(TRAIT) => transformLeafClass(cdef, q"object ${name.toTermName}") ++ rest
      case (mdef @ ModuleDef(_, _, _)) :: rest => c.abort(mdef.pos, "someLeafs must have parameters")
      case annottee :: rest => c.abort(annottee.pos, "only classes and objects can be @leaf")
    }
    q"{ ..$expanded; () }"
  }
}

object Internal {
  trait Adt
  class root extends StaticAnnotation
  class monadicRoot extends StaticAnnotation
  class branch extends StaticAnnotation
  class leafClass extends StaticAnnotation
  class leafCompanion extends StaticAnnotation
  class noneLeaf extends StaticAnnotation
  class someLeaf extends StaticAnnotation
  class delayedField extends StaticAnnotation
  case class TagAttachment(counter: Int)
  def calculateTag[T]: Int = macro AdtHelperMacros.calculateTag[T]
  def nullCheck[T](x: T): Unit = macro AdtHelperMacros.nullCheck
  def emptyCheck[T](x: T): Unit = macro AdtHelperMacros.emptyCheck
  def hierarchyCheck[T]: Unit = macro AdtHelperMacros.hierarchyCheck[T]
  def immutabilityCheck[T]: Unit = macro AdtHelperMacros.immutabilityCheck[T]
}

class AdtHelperMacros(val c: Context) extends AdtReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._
  import definitions._
  import c.internal._
  import decorators._
  import Internal.TagAttachment

  implicit class XtensionSymbol(sym: Symbol) {
    def nonEmpty = {
      val tptAnns = sym.info match {
        case AnnotatedType(anns, _) => anns
        case _ => Nil
      }
      def hasNonEmpty(anns: List[Annotation]) = anns.exists(_.tree.tpe =:= typeOf[nonEmpty])
      hasNonEmpty(sym.annotations) || hasNonEmpty(tptAnns)
    }
  }

  def calculateTag[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    val sym = T.tpe.typeSymbol.asClass
    val tag = sym.attachments.get[TagAttachment].map(_.counter).getOrElse {
      val root = sym.asAdt.root.sym
      val att = root.attachments.get[TagAttachment].map(att => att.copy(counter = att.counter + 1)).getOrElse(new TagAttachment(1))
      root.updateAttachment(att)
      sym.updateAttachment(att)
      att.counter
    }
    q"$tag"
  }

  def nullCheck(x: c.Tree): c.Tree = {
    if (x.tpe.baseClasses.contains(ObjectClass)) q"_root_.org.scalameta.invariants.require($x != null)"
    else q"()"
  }

  def emptyCheck(x: c.Tree): c.Tree = {
    val emptyCheckRequested =
      try x.symbol.asTerm.accessed.nonEmpty
      catch { case _: AssertionError => x.symbol.nonEmpty }
    if (emptyCheckRequested) q"_root_.org.scalameta.invariants.require($x != null && $x.nonEmpty)"
    else q"()"
  }

  def hierarchyCheck[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    val sym = T.tpe.typeSymbol.asClass
    val designation = if (sym.isRoot) "root" else if (sym.isBranch) "branch" else if (sym.isLeaf) "leaf" else ???
    val roots = sym.baseClasses.filter(_.isRoot)
    if (roots.length == 0 && sym.isLeaf) c.abort(c.enclosingPosition, s"rootless leaf is disallowed")
    else if (roots.length > 1) c.abort(c.enclosingPosition, s"multiple roots for a $designation: " + (roots.map(_.fullName).init.mkString(", ")) + " and " + roots.last.fullName)
    val root = roots.headOption.getOrElse(NoSymbol)
    sym.baseClasses.map(_.asClass).foreach{bsym =>
      val exempt =
        bsym.isModuleClass ||
        bsym == ObjectClass ||
        bsym == AnyClass ||
        bsym == symbolOf[scala.Serializable] ||
        bsym == symbolOf[java.io.Serializable] ||
        bsym == symbolOf[scala.Product] ||
        bsym == symbolOf[scala.Equals] ||
        root.info.baseClasses.contains(bsym)
      if (!exempt && !bsym.isRoot && !bsym.isBranch && !bsym.isLeaf) c.abort(c.enclosingPosition, s"outsider parent of a $designation: ${bsym.fullName}")
      if (!exempt && !bsym.isSealed && !bsym.isFinal) c.abort(c.enclosingPosition, s"unsealed parent of a $designation: ${bsym.fullName}")
    }
    q"()"
  }

  def immutabilityCheck[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    def check(sym: Symbol): Unit = {
      var vars = sym.info.members.collect { case x: TermSymbol if !x.isMethod && x.isVar => x }
      vars = vars.filter(!_.annotations.exists(_.tree.tpe =:= typeOf[Internal.delayedField]))
      vars.foreach(v => c.abort(c.enclosingPosition, "leafs can't have mutable state: " + v.owner.fullName + "." + v.name))
      val vals = sym.info.members.collect { case x: TermSymbol if !x.isMethod && !x.isVar => x }
      vals.foreach(v => ()) // TODO: deep immutability check
    }
    check(T.tpe.typeSymbol.asClass)
    q"()"
  }
}

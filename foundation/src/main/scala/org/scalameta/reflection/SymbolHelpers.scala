package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.reflect.internal.Flags
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.adt._

trait SymbolHelpers {
  self: GlobalToolkit =>

  import global.{require => _, _}
  import definitions._
  lazy val runDefinitions = currentRun.runDefinitions

  sealed trait MacroBody { def tree: Tree }
  object MacroBody {
    case object None extends MacroBody { def tree = EmptyTree }
    case class FastTrack(sym: Symbol) extends MacroBody { def tree = EmptyTree }
    case class Reflect(tree: Tree) extends MacroBody
    case class Meta(tree: Tree) extends MacroBody
  }

  implicit class RichHelperSymbol(sym: Symbol) {
    def isAnonymous: Boolean = {
      // NOTE: not all symbols whose names start with x$ are placeholders
      // there are also at least artifact vals created for named arguments
      val isTermPlaceholder = sym.isTerm && sym.isParameter && sym.name.startsWith(nme.FRESH_TERM_NAME_PREFIX)
      val isTypePlaceholder = sym.isType && sym.isAbstract && sym.name.startsWith("_$")
      val isAnonymousSelfName = sym.name.startsWith(nme.FRESH_TERM_NAME_PREFIX) || sym.name == nme.this_
      val isAnonymousSelf = sym.isTerm && isAnonymousSelfName && sym.owner.isClass // TODO: more precise detection, probably via attachments
      val isAnonymousTypeParameter = sym.name == tpnme.WILDCARD
      isTermPlaceholder || isTypePlaceholder || isAnonymousSelf || isAnonymousTypeParameter
    }

    def macroBody: MacroBody = {
      def macroSigs(sym: Symbol) = {
        if (sym.isMethod) sym.annotations.filter(_.tree.tpe.typeSymbol.fullName == "scala.reflect.macros.internal.macroImpl")
        else Nil
      }
      def parseMacroSig(sig: AnnotationInfo) = {
        val q"new $_[..$_]($_(..$args)[..$targs])" = sig.tree
        val metadata = args.collect{
          case Assign(Literal(Constant(s: String)), Literal(Constant(v))) => s -> v
          case Assign(Literal(Constant(s: String)), tree) => s -> tree
        }.toMap
        metadata + ("targs" -> targs)
      }
      if (g.analyzer.fastTrack.contains(sym)) MacroBody.FastTrack(sym)
      else macroSigs(sym) match {
        case legacySig :: scalametaSig :: Nil =>
          MacroBody.Meta(parseMacroSig(scalametaSig)("implDdef").require[DefDef].rhs)
        case legacySig :: Nil =>
          // TODO: obtain the impl ref exactly how it was written by the programmer
          val legacy = parseMacroSig(legacySig)
          val className = legacy("className").require[String]
          val methodName = legacy("methodName").require[String]
          val isBundle = legacy("isBundle").require[Boolean]
          val targs = legacy("targs").require[List[Tree]]
          require(className.endsWith("$") ==> !isBundle)
          val containerSym = if (isBundle) rootMirror.staticClass(className) else rootMirror.staticModule(className.stripSuffix("$"))
          val container = Ident(containerSym).setType(if (isBundle) containerSym.asType.toType else containerSym.info)
          val methodSym = containerSym.info.member(TermName(methodName))
          var implRef: Tree = Select(container, methodSym).setType(methodSym.info)
          if (targs.nonEmpty) implRef = TypeApply(implRef, targs).setType(appliedType(methodSym.info, targs.map(_.tpe)))
          MacroBody.Reflect(implRef)
        case _ :: _ =>
          unreachable(debug(macroSigs(sym)))
        case _ =>
          MacroBody.None
      }
    }

    def isIntrinsic: Boolean = {
      // NOTE: copy/paste from JavaMirrors
      def isGetClass(sym: Symbol) = (sym.name string_== "getClass") && sym.paramss.flatten.isEmpty
      def isStringConcat(sym: Symbol) = sym == String_+ || (sym.owner.isPrimitiveValueClass && sym.isMethod && sym.asMethod.returnType =:= StringClass.toType)
      val bytecodelessMethodOwners = Set[Symbol](AnyClass, AnyValClass, AnyRefClass, ObjectClass, ArrayClass) ++ ScalaPrimitiveValueClasses
      val bytecodefulObjectMethods = Set[Symbol](Object_clone, Object_equals, Object_finalize, Object_hashCode, Object_toString,
                                                 Object_notify, Object_notifyAll) ++ ObjectClass.info.member(nme.wait_).asTerm.alternatives.map(_.asMethod)
      if (isGetClass(sym) || isStringConcat(sym) || sym.owner.isPrimitiveValueClass || sym == runDefinitions.Predef_classOf || sym.isMacro) return true
      bytecodelessMethodOwners(sym.owner) && !bytecodefulObjectMethods(sym)
    }
  }

  implicit class RichHelperLogicalSymbol(lsym: l.Symbol) {
    def parents: List[l.Symbol] = {
      ???
    }

    def children: List[l.Symbol] = {
      def overriders = {
        // TODO: look up children of the owner and then search in their decls
        ???
      }
      def subclasses = {
        // TODO: the idea is to support all kinds of subclasses here, not only for sealed gsyms
        // I know how to do that, but there's a long way to go
        val unordered = lsym.symbol.initialize.knownDirectSubclasses
        unordered.toList.sortBy(_.name.decoded).map(_.toLogical)
      }
      lsym match {
        case l.AbstractVal(gget) => overriders
        case l.AbstractVar(gget, gset) => overriders
        case l.AbstractDef(gsym) => overriders
        case l.AbstractType(gsym) =>overriders
        case l.Val(gfield, gget) => overriders
        case l.Var(gfield, gget, gset) => overriders
        case l.Def(gsym) => overriders
        case l.Macro(gsym) => overriders
        case l.Type(gsym) => overriders
        case l.Clazz(gsym) => subclasses
        case l.Trait(gsym) => subclasses
        case l.Object(gmodule, gmoduleclass) => Nil
        case l.Package(gmodule, gmoduleclass) => Nil
        case l.PackageObject(gmodule, gmoduleclass) => Nil
        case l.PrimaryCtor(gsym) => Nil
        case l.SecondaryCtor(gsym) => Nil
        case l.TermBind(gsym) => Nil
        case l.TypeBind(gsym) => Nil
        case l.TermParameter(gsym) => Nil
        case l.TypeParameter(gsym) => Nil
        case _ => unreachable(debug(lsym))
      }
    }
  }
}

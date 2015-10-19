package scala.meta.internal.hosts.scalac
package reflect

import scala.tools.nsc.Global
import scala.reflect.internal.Flags
import scala.reflect.internal.Flags._
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.adt._

trait SymbolHelpers {
  self: ReflectToolkit =>

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

  implicit class RichHelperClassName(className: String) {
    def toSymbol: Symbol = {
      // TODO: again, I wish I could use pre-existing scala.reflect functionality for this...
      // TODO: duplication wrt ReificationMacros
      def loop(owner: Symbol, parts: List[String]): Symbol = parts match {
        case part :: Nil => if (className.endsWith("$")) owner.info.decl(TermName(part)) else owner.info.decl(TypeName(part))
        case part :: rest => loop(owner.info.decl(TermName(part)), rest)
        case Nil => unreachable(debug(className))
      }
      val parts = scala.reflect.NameTransformer.decode(className).stripSuffix("$").split(Array('.', '$')).toList
      loop(rootMirror.RootPackage, parts).orElse(loop(rootMirror.EmptyPackage, parts))
    }
  }

  implicit class RichHelperSymbol(sym: Symbol) {
    def isAnonymous: Boolean = sym.name.isAnonymous

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
          val containerSym = className.toSymbol
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

    def prefix: Type = {
      if (sym.hasFlag(EXISTENTIAL | PARAM)) NoPrefix
      else if (sym.isLocalToBlock) NoPrefix
      else if (sym.isConstructor) sym.owner.prefix
      else sym.owner.thisType
    }
  }
}

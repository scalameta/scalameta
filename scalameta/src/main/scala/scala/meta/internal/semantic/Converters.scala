package scala.meta
package internal
package semantic

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.reflect.api.Universe
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}

// NOTE: This functionality is here not by design, but rather by necessity:
// 1) To provide an easy notation to create denotations for mergeTrees
// 2) To power our eager "hygiene" in the current prototype of quasiquotes
//
// This is the little brother of scalahost's Tree.withDenot, doing exactly the same,
// but bailing on local symbols and supporting only a subset of prefixes.
// In principle, I would be in favor of merging these two guys, but that'll require
// moving a significant chunk of scalahost into scalameta as well as perpetuating the adhocness in scalameta.
//
// Luckily, both reasons for existence of this trait will go away in the future:
// 1) Once we modularize scalameta, mergeTrees and quasiquotes will likely end up in different modules
//    so we'll be able to quasiquotes in mergeTrees, which will solve the problem completely.
// 2) Once a better mechanism for hygiene, based on runtime resolution of names, shows up,
//    we will no longer need to compute denotations from scala.reflect artifacts.

trait Converters {
  val u: Universe
  val mirror: u.Mirror

  // NOTE: need a blanket import for class tags to avoid patmat warnings
  import u.{Type => _, Symbol => _, definitions => _, _}

  def denot(sym: u.Symbol): s.Denotation = {
    denot(u.NoType, sym)
  }

  def denot(pre: u.Type, sym: u.Symbol): s.Denotation = {
    def isGlobal(sym: u.Symbol): Boolean = {
      def definitelyLocal = sym == u.NoSymbol || sym.name.toString.startsWith("<local ") || (sym.owner.isMethod && !sym.isParameter)
      def isParentGlobal = sym.isPackage || sym.isPackageClass || isGlobal(sym.owner)
      !definitelyLocal && isParentGlobal
    }
    def signature(sym: u.Symbol): s.Signature = {
      if (sym.isMethod && !sym.asMethod.isGetter) {
        val jvmSignature = {
          // NOTE: unfortunately, this simple-looking facility generates side effects that corrupt the state of the compiler
          // in particular, mixin composition stops working correctly, at least for `object Main extends App`
          // val g = c.universe.asInstanceOf[scala.tools.nsc.Global]
          // g.exitingDelambdafy(new g.genASM.JPlainBuilder(null, false).descriptor(gsym))
          def jvmSignature(tpe: u.Type): String = {
            val u.TypeRef(_, sym, args) = tpe
            require(args.nonEmpty ==> (sym == u.definitions.ArrayClass))
            if (sym == u.definitions.UnitClass || sym == mirror.staticClass("scala.runtime.BoxedUnit")) "V"
            else if (sym == u.definitions.BooleanClass) "Z"
            else if (sym == u.definitions.CharClass) "C"
            else if (sym == u.definitions.ByteClass) "B"
            else if (sym == u.definitions.ShortClass) "S"
            else if (sym == u.definitions.IntClass) "I"
            else if (sym == u.definitions.FloatClass) "F"
            else if (sym == u.definitions.LongClass) "J"
            else if (sym == u.definitions.DoubleClass) "D"
            else if (sym == u.definitions.ArrayClass) "[" + jvmSignature(args.head)
            else "L" + sym.fullName.replace(".", "/") + ";"
          }
          val u.MethodType(params, ret) = sym.info.erasure
          val jvmRet = if (!sym.isConstructor) ret else u.definitions.UnitClass.toType
          s"(" + params.map(param => jvmSignature(param.info)).mkString("") + ")" + jvmSignature(jvmRet)
        }
        s.Signature.Method(jvmSignature)
      }
      else if (sym.isTerm) s.Signature.Term
      else if (sym.isType) s.Signature.Type
      else unreachable(debug(sym, sym.asInstanceOf[scala.reflect.internal.Symbols#Symbol].flags, sym.getClass, sym.owner))
    }
    def convertPrefix(pre: u.Type): s.Prefix = {
      def singletonType(pre: u.Type, sym: u.Symbol): m.Type = {
        val name = {
          if (sym == mirror.RootClass || sym == mirror.RootPackage) "_root_"
          else if (sym == mirror.EmptyPackageClass || sym == mirror.EmptyPackage) "_empty_"
          else sym.name.toString
        }
        m.Type.Singleton(m.Term.Name(name).withDenot(denot(pre, sym)))
      }
      val pre1 = pre.orElse(sym.prefix)
      pre1 match {
        case u.NoPrefix => s.Prefix.Zero
        case u.ThisType(sym) => s.Prefix.Type(singletonType(u.NoType, sym))
        case u.SingleType(pre, sym) => s.Prefix.Type(singletonType(pre, sym))
        case u.TypeRef(pre, sym, Nil) if sym.isModule || sym.isModuleClass => s.Prefix.Type(singletonType(pre, sym))
        case _ => sys.error(s"unsupported type ${pre1}, designation = ${pre1.getClass}, structure = ${u.showRaw(pre1, printIds = true, printTypes = true)}")
      }
    }
    def convertSymbol(sym: u.Symbol): s.Symbol = {
      if (sym.isModuleClass) convertSymbol(sym.asClass.module)
      else if (sym == mirror.RootPackage) s.Symbol.RootPackage
      else if (sym == mirror.EmptyPackage) s.Symbol.EmptyPackage
      else s.Symbol.Global(convertSymbol(sym.owner), sym.name.decodedName.toString, signature(sym))
    }
    require(isGlobal(sym) && debug(pre, sym))
    s.Denotation.Single(convertPrefix(pre), convertSymbol(sym))
  }

  implicit class XtensionConvertersSymbol(sym: u.Symbol) {
    def prefix: u.Type = {
      if (sym.isType && sym.asType.isExistential && sym.asType.isParameter) u.NoPrefix
      else if (sym.isConstructor) sym.owner.prefix
      else sym.owner.asInstanceOf[scala.reflect.internal.Symbols#Symbol].thisType.asInstanceOf[u.Type]
    }
  }

  implicit class XtensionConvertersType(tpe: u.Type) {
    def prefix: u.Type = {
      tpe.asInstanceOf[scala.reflect.internal.Types#Type].prefix.asInstanceOf[u.Type]
    }
  }
}

object RuntimeConverters extends {
  val u: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  val mirror: u.Mirror = u.runtimeMirror(classOf[scala.meta.Tree].getClassLoader)
} with Converters {
  def typeOf[T: u.TypeTag]: u.Type = u.typeOf[T]
  def symbolOf[T: u.TypeTag]: u.TypeSymbol = u.symbolOf[T]
  def TermName(name: String): u.TermName = u.TermName(name)
  def TypeName(name: String): u.TypeName = u.TypeName(name)
}

package org.scalameta.reflection

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.reflect.internal.Flags._

trait LogicalSymbols {
  self: GlobalToolkit =>

  import global.AnyNameOps

  implicit class RichLogicalSymbol(gsym: global.Symbol) {
    def logical: l.Symbol = {
      val gsym0 = gsym
      val results = logicalSymbols(List(gsym0))
      require(gsym0 != null && results != null && results.length == 1)
      results.head
    }
  }

  implicit class RichLogicalScope(gscope: global.Scope) {
    def logical: Seq[l.Symbol] = gscope match {
      case gscope: global.MemberScope => gscope.sorted.toList.logical
      case _ => gscope.toList.logical
    }
  }

  implicit class RichLogicalSymbols(gsyms: Seq[global.Symbol]) {
    def logical: Seq[l.Symbol] = logicalSymbols(gsyms)
  }

  sealed trait LogicalSymbol extends Product {
    def name: String
    def symbol: global.Symbol
    def symbols: Seq[global.Symbol] = this.productIterator.collect({case sym: global.Symbol => sym}).filter(_ != global.NoSymbol).toList
    def gsymbol: global.Symbol = symbol
    def gsymbols: Seq[global.Symbol] = symbols
    def isComplete: Boolean = !isIncomplete
    def isIncomplete: Boolean = symbol == global.NoSymbol
  }

  lazy val l: LogicalSymbol.type = LogicalSymbol
  object LogicalSymbol {
    type Symbol = LogicalSymbol

    // not representable with syntax
    // is here simply to provide an encoding for compiler's NoSymbol
    case object None extends LogicalSymbol {
      def name = global.nme.NO_NAME.toString
      def symbol = global.NoSymbol
      override def isComplete = true
    }

    // > val x: Int
    // value x, 'x', class MethodSymbol, flags = 138412112 (DEFERRED | METHOD | STABLE | ACCESSOR)
    // > type T = X { val x: Int }
    // value x, 'x', class MethodSymbol, flags = 138412112 (DEFERRED | METHOD | STABLE | ACCESSOR)
    // > type T = X forSome { val x: Int }
    // type x.type, 'x.type', class AbstractTypeSymbol, flags = 34359738384 (DEFERRED | EXISTENTIAL)
    case class AbstractVal(getter: global.Symbol) extends LogicalSymbol {
      def name = getter.name.toString
      def symbol = getter
    }

    // > var x: Int
    // method x, 'x', class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    // method x_=, 'x_$eq', class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    // > type T = X { var x: Int }
    // method x, 'x', class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    // method x_=, 'x_$eq', class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    case class AbstractVar(getter: global.Symbol, setter: global.Symbol) extends LogicalSymbol {
      def name = getter.orElse(setter).name.getterName.toString
      def symbol = getter
    }

    // > def x: Int
    // method x, 'x', class MethodSymbol, flags = 80 (DEFERRED | METHOD)
    // > type T = X { def x: Int }
    // method x, 'x', class MethodSymbol, flags = 80 (DEFERRED | METHOD)
    case class AbstractDef(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > type T
    // type T, 'T', class AbstractTypeSymbol, flags = 16 (DEFERRED)
    // > type T = X { type T <: Int }
    // type T, 'T', class AbstractTypeSymbol, flags = 16 (DEFERRED)
    // > type T = X forSome { type T <: Int }
    // type T, 'T', class AbstractTypeSymbol, flags = 34359738384 (DEFERRED | EXISTENTIAL)
    case class AbstractType(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > val x = 2
    // value x, 'x', class MethodSymbol, flags = 138412096 (METHOD | STABLE | ACCESSOR) + maybe PARAMACCESSOR
    // value x, 'x ', class TermSymbol, flags = 17592186568708 (PRIVATE | LOCAL | TRIEDCOOKING) + maybe PARAMACCESSOR
    // > private[this] val x = 2
    // value x, 'x', class TermSymbol, flags = 524292 (PRIVATE | LOCAL) + maybe PARAMACCESSOR
    // > locally { val x = 2 }
    // value x, 'x', class TermSymbol, flags = 0
    case class Val(field: global.Symbol, getter: global.Symbol) extends LogicalSymbol {
      def name = field.orElse(getter).name.getterName.toString
      def symbol = getter.orElse(field)
      override def isComplete = symbol != global.NoSymbol
    }

    // > var x = 2
    // method x, 'x', class MethodSymbol, flags = 134217792 (METHOD | ACCESSOR) + maybe PARAMACCESSOR + maybe DEFAULTINIT
    // method x_=, 'x_$eq', class MethodSymbol, flags = 134217792 (METHOD | ACCESSOR) + maybe PARAMACCESSOR + maybe DEFAULTINIT
    // variable x, 'x ', class TermSymbol, flags = 17592186572804 (PRIVATE | MUTABLE | LOCAL | TRIEDCOOKING) + maybe PARAMACCESSOR
    // > private[this] var x = 2
    // variable x, 'x', class TermSymbol, flags = 528388 (PRIVATE | MUTABLE | LOCAL) + maybe PARAMACCESSOR + maybe DEFAULTINIT
    // > locally { var x = 2 }
    // variable x, 'x', class TermSymbol, flags = 4096 (MUTABLE)
    case class Var(field: global.Symbol, getter: global.Symbol, setter: global.Symbol) extends LogicalSymbol {
      def name = field.orElse(getter).orElse(setter).getterName.toString
      def symbol = getter.orElse(field)
      override def isComplete = symbol != global.NoSymbol
    }

    // > def x = 2
    // method x, 'x', class MethodSymbol, flags = 64 (METHOD)
    case class Def(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > def x: Int = macro ???
    // macro method x, 'x', class MethodSymbol, flags = 32832 (METHOD | MACRO)
    case class Macro(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > type T = Int
    // type T, 'T', class AliasTypeSymbol, flags = 0 ()
    // > type T = X { type T = Int }
    // type T, 'T', class AliasTypeSymbol, flags = 0 ()
    case class Type(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > class C
    // class C, 'C', class ClassSymbol, flags = 0 ()
    // NOTE: [warn] LogicalSymbols.scala:108:
    // Class org.scalameta.reflection.LogicalSymbols$LogicalSymbol$Class
    // differs only in case from org.scalameta.reflection.LogicalSymbols$LogicalSymbol$class.
    // Such classes will overwrite one another on case-insensitive filesystems.
    case class Clazz(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > trait T
    // trait T, 'T', class ClassSymbol, flags = 33554568 (ABSTRACT | INTERFACE | TRAIT)
    // > trait T { def x = 2 }
    // trait T, 'T', class ClassSymbol, flags = 33554440 (ABSTRACT | TRAIT)
    case class Trait(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > object M
    // object M, 'M', class ModuleSymbol, flags = 256 (MODULE)
    case class Object(module: global.Symbol, moduleClass: global.Symbol) extends LogicalSymbol {
      def name = module.name.toString
      def symbol = module
    }

    // > package scala
    // package scala, 'scala', class ModuleSymbol, flags = 17592187109664 (FINAL | MODULE | PACKAGE | JAVA | TRIEDCOOKING)
    case class Package(module: global.Symbol, moduleClass: global.Symbol) extends LogicalSymbol {
      def name = module.name.toString
      def symbol = module
    }

    // > package object scala
    // package object scala, 'package', class ModuleSymbol, flags = 256 (MODULE)
    case class PackageObject(module: global.Symbol, moduleClass: global.Symbol) extends LogicalSymbol {
      def name = module.name.toString
      def symbol = module
    }

    // > class C(x: Int)
    // constructor C, '<init>', class MethodSymbol, flags = 64 (METHOD)
    case class PrimaryCtor(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > def this() = this(2)
    // constructor C, '<init>', class MethodSymbol, flags = 64 (METHOD)
    case class SecondaryCtor(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > Nil match { case x => ??? }
    // value x, 'x', class TermSymbol, flags = 0
    // NOTE: this symbol can't be distinguished from a local val
    // so I've patched the analyzer to attach metadata that would allow that
    case class TermBind(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > Nil match { case _: List[t] => ??? }
    // type t, 't', class AbstractTypeSymbol, flags = 16 (DEFERRED)
    // NOTE: this symbol can't be distinguished from a local type
    // so I've patched the analyzer to attach metadata that would allow that
    case class TypeBind(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > class C(x: Int)
    // value x, 'x', class TermSymbol, flags = 537395204 (PRIVATE | LOCAL | PARAMACCESSOR)
    // constructor C, class MethodSymbol, flags = 64 (METHOD)
    // value x, 'x', class TermSymbol, flags = 8192 (PARAM)
    // > def foo(x: Int) = ???
    // value x, 'x', class TermSymbol, flags = 8192 (PARAM)
    case class TermParameter(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > class C[T]
    // type T, 'T', class AbstractTypeSymbol, flags = 8208 (DEFERRED | PARAM)
    // > def foo[T] = ???
    // type T, 'T', class TypeSkolem, flags = 8208 (DEFERRED | PARAM)
    case class TypeParameter(symbol: global.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }
  }

  private def logicalSymbols(gsyms: Seq[global.Symbol]): Seq[l.Symbol] = {
    val rawResult = mutable.ListBuffer[l.Symbol]()
    val iterator = gsyms.iterator
    while (iterator.hasNext) {
      val gsym = {
        var result = iterator.next()
        result = result.deSkolemize
        if (result.isModuleClass) result = result.module
        result
      }
      val lsym = {
        if (gsym == global.NoSymbol) {
          l.None
        } else if (gsym.isTerm && !gsym.isMethod && !gsym.isModule) {
          if (gsym.hasFlag(PARAM)) l.TermParameter(gsym)
          else {
            if (gsym.hasFlag(MUTABLE)) l.Var(gsym, gsym.getter, gsym.setter)
            else {
              if (gsym.metadata.get("isPatternVariable").map(_.asInstanceOf[Boolean]).getOrElse(false)) l.TermBind(gsym)
              else l.Val(gsym, gsym.getter)
            }
          }
        } else if (gsym.isMethod) {
          require(gsym.hasFlag(METHOD))
          if (gsym.hasFlag(ACCESSOR)) {
            if (gsym.hasFlag(STABLE)) {
              if (gsym.hasFlag(DEFERRED)) l.AbstractVal(gsym)
              else l.Val(gsym.accessed, gsym)
            } else {
              if (!gsym.name.endsWith(global.nme.SETTER_SUFFIX)) {
                if (gsym.hasFlag(DEFERRED)) l.AbstractVar(gsym, gsym.setter)
                else l.Var(gsym.accessed, gsym, gsym.setter)
              } else {
                if (gsym.hasFlag(DEFERRED)) l.AbstractVar(gsym.getter, gsym)
                else l.Var(gsym.accessed, gsym.getter, gsym)
              }
            }
          } else {
            if (gsym.hasFlag(MACRO)) l.Macro(gsym)
            else if (gsym.isPrimaryConstructor) l.PrimaryCtor(gsym)
            else if (gsym.isConstructor) l.SecondaryCtor(gsym)
            else if (gsym.hasFlag(DEFERRED)) l.AbstractDef(gsym)
            else l.Def(gsym)
          }
        } else if (gsym.isModule) {
          require(gsym.hasFlag(MODULE))
          if (gsym.hasFlag(PACKAGE)) {
            l.Package(gsym, gsym.moduleClass)
          } else {
            if (gsym.name == global.nme.PACKAGE) l.PackageObject(gsym, gsym.moduleClass)
            else l.Object(gsym, gsym.moduleClass)
          }
        } else if (gsym.isType && !gsym.isClass) {
          if (gsym.hasFlag(PARAM)) {
            l.TypeParameter(gsym)
          } else if (gsym.hasFlag(DEFERRED)) {
            if (gsym.hasFlag(EXISTENTIAL)) {
              if (gsym.name.endsWith(global.nme.SINGLETON_SUFFIX)) l.AbstractVal(gsym)
              else l.AbstractType(gsym)
            } else {
              if (gsym.metadata.get("isPatternVariable").map(_.asInstanceOf[Boolean]).getOrElse(false)) l.TypeBind(gsym)
              else l.AbstractType(gsym)
            }
          } else {
            l.Type(gsym)
          }
        } else if (gsym.isClass) {
          if (gsym.hasFlag(TRAIT)) l.Trait(gsym)
          else l.Clazz(gsym)
        } else {
          sys.error(s"unsupported symbol ${gsym}, designation = ${gsym.getClass}, info = ${global.showRaw(gsym.info, printIds = true, printTypes = true)}")
        }
      }
      rawResult += lsym
    }
    val result = rawResult.toVector.distinct
    if (result.exists(_.isIncomplete)) {
      // this situation can probably occur when we are converting a scope with a getter and a setter
      // and their owners are dummy symbols that don't track their children
      val completeResult = mutable.ListBuffer[l.Symbol]()
      var i = 0
      while (i < result.length) {
        def merge(lsym1: LogicalSymbol, lsym2: LogicalSymbol) = (lsym1, lsym2) match {
          case (l.AbstractVar(g1, s1), l.AbstractVar(g2, s2)) => l.AbstractVar(g1.orElse(g2), s1.orElse(s2))
          case (l.Val(f1, g1), l.Val(f2, g2)) => l.Val(f1.orElse(f2), g1.orElse(g2))
          case (l.Var(f1, g1, s1), l.Var(f2, g2, s2)) => l.Var(f1.orElse(f2), g1.orElse(g2), s1.orElse(s2))
          case _ => unreachable
        }
        val partial = result(i)
        val found = completeResult.indexWhere(accum => accum.productPrefix == partial.productPrefix && accum.name == partial.name)
        if (found != -1) completeResult(found) = merge(completeResult(found), partial)
        else completeResult += result(i)
        i += 1
      }
      require(completeResult.forall(_.isComplete))
      completeResult.toList
    } else {
      result
    }
  }
}
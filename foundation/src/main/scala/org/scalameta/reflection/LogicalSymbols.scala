package org.scalameta.reflection

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.reflect.internal.Flags._

trait LogicalSymbols {
  self: GlobalToolkit =>

  implicit class RichLogicalSymbol(gsym: global.Symbol) {
    def logical: l.Symbol = logicalSymbols(List(gsym)).head
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
    def symbol: global.Symbol
    def symbols: Seq[global.Symbol] = this.productIterator.collect({case sym: global.Symbol => sym}).filter(_ != global.NoSymbol).toList
    def gsymbol: global.Symbol = symbol
    def gsymbols: Seq[global.Symbol] = symbols
  }

  lazy val l: LogicalSymbol.type = LogicalSymbol
  object LogicalSymbol {
    type Symbol = LogicalSymbol

    // not representable with syntax
    // is here simply to provide an encoding for compiler's NoSymbol
    case object None extends LogicalSymbol { def symbol = global.NoSymbol }

    // > val x: Int
    // value x, class MethodSymbol, flags = 138412112 (DEFERRED | METHOD | STABLE | ACCESSOR)
    // > type T = X { val x: Int }
    // value x, class MethodSymbol, flags = 138412112 (DEFERRED | METHOD | STABLE | ACCESSOR)
    // > type T = X forSome { val x: Int }
    // type x.type, class AbstractTypeSymbol, flags = 34359738384 (DEFERRED | EXISTENTIAL)
    case class AbstractVal(getter: global.Symbol) extends LogicalSymbol { def symbol = getter }

    // > var x: Int
    // method x, class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    // method x_=, class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    // > type T = X { var x: Int }
    // method x, class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    // method x_=, class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    case class AbstractVar(getter: global.Symbol, setter: global.Symbol) extends LogicalSymbol { def symbol = getter }

    // > def x: Int
    // method x, class MethodSymbol, flags = 80 (DEFERRED | METHOD)
    // > type T = X { def x: Int }
    // method x, class MethodSymbol, flags = 80 (DEFERRED | METHOD)
    case class AbstractDef(symbol: global.Symbol) extends LogicalSymbol

    // > type T
    // type T, class AbstractTypeSymbol, flags = 16 (DEFERRED)
    // > type T = X { type T <: Int }
    // type T, class AbstractTypeSymbol, flags = 16 (DEFERRED)
    // > type T = X forSome { type T <: Int }
    // type T, class AbstractTypeSymbol, flags = 34359738384 (DEFERRED | EXISTENTIAL)
    case class AbstractType(symbol: global.Symbol) extends LogicalSymbol

    // > val x = 2
    // value x, class MethodSymbol, flags = 138412096 (METHOD | STABLE | ACCESSOR) + maybe PARAMACCESSOR
    // value x, class TermSymbol, flags = 17592186568708 (PRIVATE | LOCAL | TRIEDCOOKING) + maybe PARAMACCESSOR
    // > private[this] val x = 2
    // value x, class TermSymbol, flags = 524292 (PRIVATE | LOCAL) + maybe PARAMACCESSOR
    // > locally { val x = 2 }
    // value x, class TermSymbol, flags = 0
    case class Val(field: global.Symbol, getter: global.Symbol) extends LogicalSymbol { def symbol = getter.orElse(field) }

    // > var x = 2
    // method x, class MethodSymbol, flags = 134217792 (METHOD | ACCESSOR) + maybe PARAMACCESSOR
    // method x_=, class MethodSymbol, flags = 134217792 (METHOD | ACCESSOR) + maybe PARAMACCESSOR
    // variable x, class TermSymbol, flags = 17592186572804 (PRIVATE | MUTABLE | LOCAL | TRIEDCOOKING) + maybe PARAMACCESSOR
    // > private[this] var x = 2
    // variable x, class TermSymbol, flags = 528388 (PRIVATE | MUTABLE | LOCAL) + maybe PARAMACCESSOR
    // > locally { var x = 2 }
    // variable x, class TermSymbol, flags = 4096 (MUTABLE)
    case class Var(field: global.Symbol, getter: global.Symbol, setter: global.Symbol) extends LogicalSymbol { def symbol = getter.orElse(field) }

    // > def x = 2
    // method x, class MethodSymbol, flags = 64 (METHOD)
    case class Def(symbol: global.Symbol) extends LogicalSymbol

    // > def x: Int = macro ???
    // macro method x, class MethodSymbol, flags = 32832 (METHOD | MACRO)
    case class Macro(symbol: global.Symbol) extends LogicalSymbol

    // > type T = Int
    // type T, class AliasTypeSymbol, flags = 0 ()
    // > type T = X { type T = Int }
    // type T, class AliasTypeSymbol, flags = 0 ()
    case class Type(symbol: global.Symbol) extends LogicalSymbol

    // > class C
    // class C, class ClassSymbol, flags = 0 ()
    // NOTE: [warn] LogicalSymbols.scala:108:
    // Class org.scalameta.reflection.LogicalSymbols$LogicalSymbol$Class
    // differs only in case from org.scalameta.reflection.LogicalSymbols$LogicalSymbol$class.
    // Such classes will overwrite one another on case-insensitive filesystems.
    case class Clazz(symbol: global.Symbol) extends LogicalSymbol

    // > trait T
    // trait T, class ClassSymbol, flags = 33554568 (ABSTRACT | INTERFACE | TRAIT)
    // > trait T { def x = 2 }
    // trait T, class ClassSymbol, flags = 33554440 (ABSTRACT | TRAIT)
    case class Trait(symbol: global.Symbol) extends LogicalSymbol

    // > object M
    // object M, class ModuleSymbol, flags = 256 (MODULE)
    case class Object(module: global.Symbol, moduleClass: global.Symbol) extends LogicalSymbol { def symbol = module }

    // > package scala
    // package scala, class ModuleSymbol, flags = 17592187109664 (FINAL | MODULE | PACKAGE | JAVA | TRIEDCOOKING)
    case class Package(module: global.Symbol, moduleClass: global.Symbol) extends LogicalSymbol { def symbol = module }

    // > package object scala
    // package object scala, class ModuleSymbol, flags = 256 (MODULE)
    case class PackageObject(module: global.Symbol, moduleClass: global.Symbol) extends LogicalSymbol { def symbol = module }

    // > class C(x: Int)
    // constructor C, class MethodSymbol, flags = 64 (METHOD)
    case class PrimaryCtor(symbol: global.Symbol) extends LogicalSymbol

    // > def this() = this(2)
    // constructor C, class MethodSymbol, flags = 64 (METHOD)
    case class SecondaryCtor(symbol: global.Symbol) extends LogicalSymbol

    // > class C(x: Int)
    // value x, class TermSymbol, flags = 537395204 (PRIVATE | LOCAL | PARAMACCESSOR)
    // constructor C, class MethodSymbol, flags = 64 (METHOD)
    // value x, class TermSymbol, flags = 8192 (PARAM)
    // > def foo(x: Int) = ???
    // value x, class TermSymbol, flags = 8192 (PARAM)
    case class TermParameter(symbol: global.Symbol) extends LogicalSymbol

    // > class C[T]
    // type T, class AbstractTypeSymbol, flags = 8208 (DEFERRED | PARAM)
    // > def foo[T] = ???
    // type T, class TypeSkolem, flags = 8208 (DEFERRED | PARAM)
    case class TypeParameter(symbol: global.Symbol) extends LogicalSymbol
  }

  private def logicalSymbols(gsyms: Seq[global.Symbol]): Seq[l.Symbol] = {
    val result = mutable.ListBuffer[l.Symbol]()
    val iterator = gsyms.iterator
    while (iterator.hasNext) {
      val gsym = {
        var result = iterator.next()
        result = result.deSkolemize
        if (result.isModuleClass) result = result.module
        result
      }
      val lsym = {
        if (gsym.isTerm && !gsym.isMethod && !gsym.isModule) {
          if (gsym.hasFlag(PARAM)) l.TermParameter(gsym)
          else {
            require(gsym.hasFlag(PRIVATE | LOCAL))
            if (gsym.hasFlag(MUTABLE)) l.Var(gsym, global.NoSymbol, global.NoSymbol)
            else l.Val(gsym, global.NoSymbol)
          }
        } else if (gsym.isMethod) {
          require(gsym.hasFlag(METHOD))
          if (gsym.hasFlag(ACCESSOR)) {
            if (gsym.hasFlag(STABLE)) {
              if (gsym.hasFlag(DEFERRED)) l.AbstractVal(gsym)
              else l.Val(global.NoSymbol, gsym)
            } else {
              if (!gsym.name.endsWith(global.nme.SETTER_SUFFIX)) {
                if (gsym.hasFlag(DEFERRED)) l.AbstractVar(gsym, global.NoSymbol)
                else l.Var(global.NoSymbol, gsym, global.NoSymbol)
              } else {
                if (gsym.hasFlag(DEFERRED)) l.AbstractVar(global.NoSymbol, gsym)
                else l.Var(global.NoSymbol, global.NoSymbol, gsym)
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
              l.AbstractType(gsym)
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
      ???
    }
    result.toList
  }
}
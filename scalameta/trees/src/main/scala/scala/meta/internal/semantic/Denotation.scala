package scala.meta
package internal
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._

// In our sketch, symbols are split into global and local. Global symbols can be observed from multiple
// compilation units, so we need a scheme to make observers arrive at the same representation for them.
// Towards that end, we represent public symbols with their full paths and erased signatures.
// For local symbols, things are much simpler - we require unique ids, whose generation is feasible,
// because everyone who can create those symbols is localized to the same compilation unit.

@root trait Signature
object Signature {
  @leaf object Type extends Signature
  @leaf object Term extends Signature
  @leaf class Method(jvmSignature: String) extends Signature
  @leaf object TypeParameter extends Signature
  @leaf object TermParameter extends Signature
  @leaf object Self extends Signature
}

@root trait Symbol
object Symbol {
  @leaf object Zero extends Symbol
  @leaf object RootPackage extends Symbol
  @leaf object EmptyPackage extends Symbol
  @leaf class Global(owner: Symbol, name: String, signature: Signature) extends Symbol
  @leaf class Local(id: String) extends Symbol
}

// upd. It should've been obvious from the very beginning, but symbols alone don't cut it.
// Even though symbols and symbol-based sigmas can preserve enough information to ensure hygiene,
// that information isn't useful in itself - only a full-fledged typechecker can make sense of it
// (in the paper, such a typechecker includes a symbol table and a list of prefixes for all symbols).
// However, when comparing trees, we don't have a typechecker, so we need to figure out what else is necessary.
// Another essential thing apart from symbols is prefixes. Let's hope that this is it.

@monadicRoot trait Prefix
object Prefix {
  @noneLeaf object Zero extends Prefix
  @someLeaf class Type(tpe: scala.meta.Type) extends Prefix {
    require(tpe.isTypechecked && debug(tpe.show[Attributes]))
    override def canEqual(other: Any): Boolean = other.isInstanceOf[Type]
    override def equals(that: Any): Boolean = that match {
      case that: Type => equality.Semantic.equals(this.tpe, that.tpe)
      case _ => false
    }
    override def hashCode: Int = equality.Semantic.hashCode(tpe)
  }
}

// upd. The bunch of information that should be enough for hygienic comparison?
// We define it here and call it denotation. Hopefully, prefix and symbol are all that it takes.
// Also, hopefully, it's only necessary to define denotations for all names in a tree
// to guarantee hygienic comparisons and name lookups.

@root trait Denotation {
  def prefix: Prefix
  def symbols: List[Symbol]
  def map[Result: DenotationMapResult](fn: (Prefix, Symbol) => Result): Denotation
  def flatMap(fn: (Prefix, Symbol) => Denotation): Denotation
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Denotation]
  override def equals(that: Any): Boolean = that match {
    case that: Denotation => equality.Semantic.equals(this.prefix, that.prefix) && this.symbols == that.symbols
    case _ => false
  }
  override def hashCode: Int = equality.Semantic.hashCode(prefix) * 37 + symbols.hashCode
}
object Denotation {
  @leaf object Zero extends Denotation {
    def prefix = Prefix.Zero
    def symbols = Nil
    def map[Result: DenotationMapResult](fn: (Prefix, Symbol) => Result) = Zero
    def flatMap(fn: (Prefix, Symbol) => Denotation) = Zero
    override def equals(that: Any): Boolean = super.equals(that)
    override def hashCode: Int = super.hashCode
  }
  @branch trait CommonMonadicOps extends Denotation {
    private def merge(extracts: Seq[(Option[Prefix], Option[Seq[Symbol]])]): Denotation = {
      val prefixes = extracts.foldLeft(List[Prefix]())((acc, curr) => acc ++ curr._1)
      val prefix = prefixes.filter(_ != this.prefix) match {
        case Nil => this.prefix
        case _ => sys.error("internal error: don't know how to merge prefixes: $prefixes")
      }
      var symbols = extracts.foldLeft(List[Symbol]())((acc, curr) => acc ++ curr._2.getOrElse(Nil))
      if (symbols.isEmpty) symbols = this.symbols
      symbols match {
        case Nil => unreachable
        case Seq(symbol) => Single(prefix, symbol)
        case symbols => Multi(prefix, symbols)
      }
    }
    def map[Result: DenotationMapResult](fn: (Prefix, Symbol) => Result) = {
      val results = this.symbols.map(symbol => fn(this.prefix, symbol))
      merge(results.map(implicitly[DenotationMapResult[Result]].extract))
    }
    def flatMap(fn: (Prefix, Symbol) => Denotation) = {
      var results = this.symbols.map(symbol => fn(this.prefix, symbol))
      results = results.filter(_ != Denotation.Zero)
      merge(results.map(result => (Some(result.prefix), Some(result.symbols))))
    }
  }
  @leaf class Single(prefix: Prefix, symbol: Symbol) extends Denotation with CommonMonadicOps {
    def symbols = List(symbol)
    override def equals(that: Any): Boolean = super.equals(that)
    override def hashCode: Int = super.hashCode
  }
  @leaf class Multi(prefix: Prefix, symbols: List[Symbol]) extends Denotation with CommonMonadicOps {
    require(symbols.length > 1)
    override def equals(that: Any): Boolean = super.equals(that)
    override def hashCode: Int = super.hashCode
  }
}

// TODO: This unrelated code is here because of the limitation of knownDirectSubclasses.
// We would like move it to scala/meta/internal/quasiquotes/ast/ReificationMacros.scala where it belongs,
// but then we have problems with compilation order.
trait DenotationLiftables extends adt.Liftables {
  implicit def liftableSubTree[T <: Tree]: u.Liftable[T]
  lazy implicit val liftableDenotation: u.Liftable[Denotation] = materializeAdt[Denotation]
}

// TODO: I wish there was a leaner way to achieve static safety for advanced type signatures.
trait DenotationMapResult[T] {
  def extract(x: T): (Option[Prefix], Option[Seq[Symbol]])
}
object DenotationMapResult {
  implicit object PrefixIsResult extends DenotationMapResult[Prefix] {
    def extract(x: Prefix) = (Some(x), None)
  }
  implicit object SymbolIsResult extends DenotationMapResult[Symbol] {
    def extract(x: Symbol) = (None, Some(List(x)))
  }
  implicit def SymbolsIsResult[T <: Seq[Symbol]] = new DenotationMapResult[T] {
    def extract(x: T) = (None, Some(x))
  }
  implicit object PrefixSymbolIsResult extends DenotationMapResult[(Prefix, Symbol)] {
    def extract(x: (Prefix, Symbol)) = (Some(x._1), Some(List(x._2)))
  }
  implicit def PrefixSymbolsIsResult[T <: Seq[Symbol]] = new DenotationMapResult[(Prefix, T)] {
    def extract(x: (Prefix, T)) = (Some(x._1), Some(x._2))
  }
}

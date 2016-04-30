package scala.meta
package internal
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.meta.common._
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._

// In our sketch, symbols are split into global and local. Global symbols can be observed from multiple
// compilation units, so we need a scheme to make observers arrive at the same representation for them.
// Towards that end, we represent public symbols with their full paths and erased signatures.
// For local symbols, things are much simpler - we require unique ids, whose generation is feasible,
// because everyone who can create those symbols is localized to the same compilation unit.

@root trait ScalaSig
object ScalaSig {
  @leaf class Type(name: String) extends ScalaSig
  @leaf class Term(name: String) extends ScalaSig
  @leaf class Method(name: String, jvmSignature: String) extends ScalaSig
  @leaf class TypeParameter(name: String) extends ScalaSig
  @leaf class TermParameter(name: String) extends ScalaSig
  @leaf class Self(name: String) extends ScalaSig
}

@root trait BinarySig extends Optional
object BinarySig {
  @none object None extends BinarySig
  @leaf class Intrinsic(className: String, methodName: String, signature: String) extends BinarySig
  @leaf class JvmField(className: String, fieldName: String, signature: String) extends BinarySig
  @leaf class JvmMethod(className: String, fieldName: String, signature: String) extends BinarySig
  @leaf class JvmErasure(className: String) extends BinarySig
  @leaf class JvmPackage(packageName: String) extends BinarySig
}

@root trait Symbol extends Optional
object Symbol {
  @none object None extends Symbol
  @leaf object RootPackage extends Symbol
  @leaf object EmptyPackage extends Symbol
  @leaf class Global(owner: Symbol, scalaSig: ScalaSig, binarySig: BinarySig) extends Symbol
  @leaf class Local(id: String) extends Symbol // TODO: also keep signatures?
}

// upd. It should've been obvious from the very beginning, but symbols alone don't cut it.
// Even though symbols and symbol-based sigmas can preserve enough information to ensure hygiene,
// that information isn't useful in itself - only a full-fledged typechecker can make sense of it
// (in the paper, such a typechecker includes a symbol table and a list of prefixes for all symbols).
// However, when comparing trees, we don't have a typechecker, so we need to figure out what else is necessary.
// Another essential thing apart from symbols is prefixes. Let's hope that this is it.

@root trait Prefix extends Optional
object Prefix {
  @none object None extends Prefix
  @leaf class Type(tpe: scala.meta.Type) extends Prefix {
    override def canEqual(other: Any): Boolean = other.isInstanceOf[Type]
    override def equals(that: Any): Boolean = that match {
      case that: Type => equality.Semantic.equals(this.tpe, that.tpe)
      case _ => false
    }
    override def hashCode: Int = equality.Semantic.hashCode(tpe)
  }
  object Type {
    def apply(tpe: scala.meta.Type): Prefix = new Type(tpe.setTypechecked)
  }
}

// upd. The bunch of information that should be enough for hygienic comparison?
// We define it here and call it denotation. Hopefully, prefix and symbol are all that it takes.
// Also, hopefully, it's only necessary to define denotations for all names in a tree
// to guarantee hygienic comparisons and name lookups.

@root trait Denotation extends Optional {
  def prefix: Prefix
  def symbols: List[Symbol]
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Denotation]
  override def equals(that: Any): Boolean = that match {
    case that: Denotation => equality.Semantic.equals(this.prefix, that.prefix) && this.symbols == that.symbols
    case _ => false
  }
  override def hashCode: Int = equality.Semantic.hashCode(prefix) * 37 + symbols.hashCode
}
object Denotation {
  @none object None extends Denotation {
    def prefix = Prefix.None
    def symbols = Nil
    override def equals(that: Any): Boolean = super.equals(that)
    override def hashCode: Int = super.hashCode
  }
  @leaf class Single(prefix: Prefix, symbol: Symbol) extends Denotation {
    def symbols = List(symbol)
    override def equals(that: Any): Boolean = super.equals(that)
    override def hashCode: Int = super.hashCode
  }
  @leaf class Multi(prefix: Prefix, symbols: List[Symbol]) extends Denotation {
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

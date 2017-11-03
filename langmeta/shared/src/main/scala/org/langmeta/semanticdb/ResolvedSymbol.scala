package org.langmeta
package semanticdb

import scala.runtime.AbstractFunction2

final case class ResolvedSymbol2(symbol: Symbol, denotation: Denotation, members: List[Signature]) {
  def input: Input = Input.Denotation(denotation.signature, symbol)
  def structure = s"""ResolvedSymbol(${symbol.structure}, ${denotation.structure})"""
}

final class ResolvedSymbol(
    val symbol: Symbol,
    val denotation: Denotation,
    val members: List[Signature])
    extends Product
    with Serializable
{
  def this(symbol: Symbol, denotation: Denotation) = this(symbol, denotation, Nil)
  def input: Input = Input.Denotation(denotation.signature, symbol)
  def syntax: String = {
    val memberSyntax = if (members.isEmpty) "" else s".{+${members.length}}"
    s"${symbol.syntax} => ${denotation.syntax}$memberSyntax"
  }
  def structure: String = {
    val memberSyntax = if (members.isEmpty) "" else s", {+${members.length}}"
    s"""ResolvedSymbol(${symbol.structure}, ${denotation.structure}$memberSyntax)"""
  }
  // We must implement the boilerplate below in order to keep binary compatibility
  // with previous releases.
  def copy(symbol: Symbol = this.symbol, denotation: Denotation = this.denotation): ResolvedSymbol =
    new ResolvedSymbol(symbol, denotation, members)
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ResolvedSymbol =>
      that.symbol.equals(that.symbol) &&
        that.denotation.equals(that.denotation) &&
        that.members.equals(that.members)
    case _ => false
  }
  override def hashCode(): Int = {
    var acc = -889275714
    acc = runtime.Statics.mix(acc, runtime.Statics.anyHash(symbol))
    acc = runtime.Statics.mix(acc, runtime.Statics.anyHash(denotation))
    acc = runtime.Statics.mix(acc, runtime.Statics.anyHash(members))
    acc
  }
  override def toString: String = syntax
  override def productPrefix: String = "ResolvedSymbol"
  override def productElement(n: Int): Any = n match {
    case 0 => symbol
    case 1 => denotation
    case 2 => members
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }
  override def productArity = 3
  override def canEqual(that: Any): Boolean = that.isInstanceOf[ResolvedSymbol]
}

object ResolvedSymbol extends AbstractFunction2[Symbol, Denotation, ResolvedSymbol] {
  def apply(symbol: Symbol, denotation: Denotation, members: List[Signature]): ResolvedSymbol =
    new ResolvedSymbol(symbol, denotation, members)
  def apply(symbol: Symbol, denotation: Denotation): ResolvedSymbol =
    ResolvedSymbol(symbol, denotation, Nil)
  def unapply(arg: ResolvedSymbol): Option[(Symbol, Denotation)] =
    Some(arg.symbol -> arg.denotation)
}

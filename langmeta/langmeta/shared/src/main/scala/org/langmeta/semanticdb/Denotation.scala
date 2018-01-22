package org.langmeta
package semanticdb

import scala.annotation.switch
import scala.runtime.AbstractFunction4

final class Denotation(
    val flags: Long,
    val name: String,
    val signature: String,
    val names: List[ResolvedName],
    val members: List[Signature],
    val overrides: Option[Symbol]
) extends HasFlags
    with Product
    with Serializable {
  def this(flags: Long, name: String, signature: String, names: List[ResolvedName]) =
      this(flags, name, signature, names, Nil, None)

  def this(flags: Long, name: String, signature: String, names: List[ResolvedName], members: List[Signature]) =
      this(flags, name, signature, names, members, None)

  def syntax: String = {
    val s_members_and_overrides =
      if (members.isEmpty && overrides.isEmpty) ""
      else {
        val membersPart =
          if (members.isEmpty) ""
          else s"+${members.length} members"

        val overridePart =
          overrides match {
            case Some(symbol) => s"override $symbol"
            case _ => ""
          }

        val join = List(membersPart, overridePart).filterNot(_.isEmpty).mkString(" ")

        s".{$join}"
      }

    val s_info = if (signature != "") ": " + signature else ""
    val s_names = ResolvedName.syntax(names)
    // TODO(olafur) use more advances escaping.
    val s_name = if (name.contains(" ")) s"`$name`" else name
    s"$flagSyntax $s_name" + s_info + s_names + s_members_and_overrides
  }
  def structure = s"""Denotation($flagStructure, "$name", "$signature")"""

  // Expanded case class boilerplate
  @deprecated("Use new Denotation(...) instead.", "2.1.0")
  def copy(
      flags: Long = flags,
      name: String = name,
      signature: String = signature,
      names: List[ResolvedName] = names): Denotation =
    new Denotation(flags, name, signature, names)
  override def toString: String = syntax
  override def hashCode(): Int = {
    var acc: Int = -889275714
    acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.longHash(flags))
    acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.anyHash(name))
    acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.anyHash(signature))
    acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.anyHash(names))
    acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.anyHash(members))
    acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.anyHash(overrides))
    scala.runtime.Statics.finalizeHash(acc, 4)
  }
  override def equals(x$1: scala.Any): Boolean = this.eq(x$1.asInstanceOf[Object]) || {
    x$1 match {
      case d: Denotation =>
        flags == d.flags &&
          name == d.name &&
          signature == d.signature &&
          names == d.names
    }
  }
  override def productElement(x$1: Int): Any = (x$1: @switch) match {
    case 0 => flags
    case 1 => name
    case 2 => signature
    case 3 => names
    case 4 => members
    case 5 => overrides
    case _ => throw new IndexOutOfBoundsException(x$1.toString)
  }
  override def productArity = 4
  override def canEqual(that: Any): Boolean = that.isInstanceOf[Denotation]
}
object Denotation extends AbstractFunction4[Long, String, String, List[ResolvedName], Denotation] {
  def apply(flags: Long, name: String, signature: String, names: List[ResolvedName]): Denotation =
    new Denotation(flags, name, signature, names)

  def apply(flags: Long,
            name: String,
            signature: String,
            names: List[ResolvedName],
            members: List[Signature],
            overrides: Option[Symbol]): Denotation =
    new Denotation(flags, name, signature, names, members, overrides)

  def apply(flags: Long,
            name: String,
            signature: String,
            names: List[ResolvedName],
            members: List[Signature]): Denotation =
    new Denotation(flags, name, signature, names, members)
  @deprecated("Use `case d: Denotation` instead.", "2.1.0")
  def unapply(x$0: Denotation): Option[(Long, String, String, List[ResolvedName])] =
    if (x$0.==(null))
      scala.None
    else
      Some((x$0.flags, x$0.name, x$0.signature, x$0.names))
}

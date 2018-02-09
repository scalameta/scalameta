package org.langmeta
package semanticdb

import scala.annotation.switch
import scala.runtime.AbstractFunction4
import scala.compat.Platform.EOL
import scala.meta.internal.{semanticdb3 => s}

final class Denotation(
    val flags: Long,
    val name: String,
    val signature: String,
    val names: List[ResolvedName],
    val members: List[Signature],
    val overrides: List[Symbol],
    val tpe: Option[s.Type],
    val annotations: List[Symbol]
) extends HasFlags
    with Product
    with Serializable {
  def this(flags: Long, name: String, signature: String, names: List[ResolvedName]) =
      this(flags, name, signature, names, Nil, Nil, None, Nil)

  def this(flags: Long, name: String, signature: String, names: List[ResolvedName], 
           members: List[Signature]) =
      this(flags, name, signature, names, members, Nil, None, Nil)

  def this(flags: Long, name: String, signature: String, names: List[ResolvedName],
           members: List[Signature], overrides: List[Symbol]) =
      this(flags, name, signature, names, members, overrides, None, Nil)

  def this(flags: Long, name: String, signature: String, names: List[ResolvedName],
           members: List[Signature], overrides: List[Symbol], tpe: Option[s.Type]) =
    this(flags, name, signature, names, members, overrides, tpe, Nil)

  def syntax: String = {
    val s_overrides = if (overrides.isEmpty) "" else s"${EOL}  override ${overrides.head}"
    val s_annotations =
      if (annotations.isEmpty) ""
      else annotations.map(annot =>s"  @$annot").mkString(EOL, EOL, "")

    val s_members = if (members.isEmpty) "" else s".{+${members.length} members}"
    val s_info = if (signature != "") ": " + signature else ""
    val s_names = ResolvedName.syntax(names)
    // TODO(olafur) use more advances escaping.
    val s_name = if (name.contains(" ")) s"`$name`" else name
    // TODO: implement Denotation.tpe prettyprinting.

    s"$flagSyntax $s_name" + s_info + s_members +
      s_overrides +
      s_annotations +
      s_names
  }
  def structure = s"""Denotation($flagStructure, "$name", "$signature")"""

  private val specialized = Symbol("_root_.scala.specialized#")
  def isSpecialized: Boolean = annotations.exists(_ == specialized)

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
    acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.anyHash(tpe))
    acc = scala.runtime.Statics.mix(acc, scala.runtime.Statics.anyHash(annotations))
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
            members: List[Signature]): Denotation =
    new Denotation(flags, name, signature, names, members)

  def apply(flags: Long,
            name: String,
            signature: String,
            names: List[ResolvedName],
            members: List[Signature],
            overrides: List[Symbol]): Denotation =
    new Denotation(flags, name, signature, names, members, overrides)

  def apply(flags: Long,
            name: String,
            signature: String,
            names: List[ResolvedName],
            members: List[Signature],
            overrides: List[Symbol],
            tpe: Option[s.Type]): Denotation =
    new Denotation(flags, name, signature, names, members, overrides, tpe)

  def apply(flags: Long,
            name: String,
            signature: String,
            names: List[ResolvedName],
            members: List[Signature],
            overrides: List[Symbol],
            tpe: Option[s.Type],
            annotations: List[Symbol]): Denotation =
    new Denotation(flags, name, signature, names, members, overrides, tpe, annotations)

  @deprecated("Use `case d: Denotation` instead.", "2.1.0")
  def unapply(x$0: Denotation): Option[(Long, String, String, List[ResolvedName])] =
    if (x$0.==(null))
      scala.None
    else
      Some((x$0.flags, x$0.name, x$0.signature, x$0.names))
}

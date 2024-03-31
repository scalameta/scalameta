package org.scalameta
package internal

import scala.annotation.tailrec

trait MacroHelpers extends DebugFinder with MacroCompat with FreeLocalFinder with ImplTransformers {
  import scala.reflect.internal.Flags._

  import c.universe._
  import c.universe.definitions._

  implicit class XtensionModifiers(mods: Modifiers) {
    def transformFlags(fn: Long => Long): Modifiers = {
      val flags1 = fn(mods.flags.asInstanceOf[Long]).asInstanceOf[FlagSet]
      Modifiers(flags1, mods.privateWithin, mods.annotations)
    }
    def mkPrivate = mods.transformFlags(_ | LOCAL | PRIVATE)
    def unPrivate = mods.transformFlags(_ & ~LOCAL & ~PRIVATE)
    def mkFinal = mods.transformFlags(_ | FINAL)
    def mkMutable = mods.transformFlags(_ | MUTABLE)
    def unMutable = mods.transformFlags(_ & ~MUTABLE)
    def mkCase = mods.transformFlags(_ | CASE)
    def unVariant = mods.transformFlags(_ & ~COVARIANT & ~CONTRAVARIANT)
    def unOverride = mods.transformFlags(_ & ~OVERRIDE)
    def unDefault = mods.transformFlags(_ & ~DEFAULTPARAM)
  }

  implicit class XtensionSymbol(sym: Symbol) {
    def nonEmpty = {
      val tptAnns = sym.info match {
        case AnnotatedType(anns, _) => anns
        case _ => Nil
      }
      def hasNonEmpty(anns: List[Annotation]) = anns
        .exists(_.tree.tpe =:= typeOf[org.scalameta.invariants.nonEmpty])
      hasNonEmpty(sym.annotations) || hasNonEmpty(tptAnns)
    }
  }

  lazy val InvariantFailedRaiseMethod =
    q"${hygienicRef(org.scalameta.invariants.InvariantFailedException)}.raise"
  lazy val InvariantsRequireMethod = q"${hygienicRef(org.scalameta.invariants.`package`)}.require"
  lazy val UnreachableErrorModule = hygienicRef(org.scalameta.UnreachableError)
  lazy val DataAnnotation = tq"_root_.org.scalameta.data.data"
  lazy val DataTyperMacrosModule = hygienicRef(org.scalameta.data.DataTyperMacros)
  lazy val AdtPackage = q"_root_.org.scalameta.adt"
  lazy val AdtMetadataModule = hygienicRef(org.scalameta.adt.Metadata)
  lazy val AdtTyperMacrosModule = hygienicRef(org.scalameta.adt.AdtTyperMacros)
  lazy val AstMetadataModule = hygienicRef(scala.meta.internal.trees.Metadata)
  lazy val CommonTyperMacrosModule = hygienicRef(scala.meta.internal.trees.CommonTyperMacros)
  lazy val CommonTyperMacrosBundle = hygienicRef[scala.meta.internal.trees.CommonTyperMacrosBundle]
  lazy val AstInfoClass = tq"_root_.scala.meta.internal.trees.AstInfo"
  lazy val QuasiClass = tq"_root_.scala.meta.internal.trees.Quasi"
  lazy val TokenMetadataModule = hygienicRef(scala.meta.internal.tokens.Metadata)
  lazy val BooleanClass = hygienicRef[scala.Boolean]
  lazy val IntClass = hygienicRef[scala.Int]
  lazy val AnyClass = hygienicRef[scala.Any]
  lazy val AnyRefClass = hygienicRef[scala.AnyRef]
  lazy val NothingClass = tq"_root_.scala.Nothing"
  lazy val OptionClass = tq"_root_.scala.Option"
  lazy val SomeClass = tq"_root_.scala.Some"
  lazy val SomeModule = hygienicRef(Some)
  lazy val NoneModule = hygienicRef(scala.None)
  def SerialVersionUIDAnnotation(uid: Long) = q"new ${hygienicRef[SerialVersionUID]}($uid)"
  def TransientAnnotation = q"new ${hygienicRef[transient]}"
  def InlineAnnotation = q"new ${hygienicRef[inline]}"
  lazy val ProductClass = hygienicRef[Product]
  lazy val SerializableClass = hygienicRef[Serializable]
  lazy val StringClass = hygienicRef[String]
  lazy val ScalaRunTimeModule = hygienicRef(scala.runtime.ScalaRunTime)
  lazy val UnsupportedOperationException = hygienicRef[UnsupportedOperationException]
  lazy val IndexOutOfBoundsException = hygienicRef[IndexOutOfBoundsException]
  lazy val IteratorClass = tq"_root_.scala.collection.Iterator"
  lazy val ListClass = tq"_root_.scala.List"
  lazy val ListModule = q"_root_.scala.List"
  lazy val SeqClass = tq"_root_.scala.Seq"
  lazy val SeqModule = q"_root_.scala.Seq"
  lazy val ListBufferModule = hygienicRef(scala.collection.mutable.ListBuffer)
  lazy val UnitClass = hygienicRef[scala.Unit]
  lazy val ClassClass = tq"_root_.java.lang.Class"
  lazy val ClassTagClass = tq"_root_.scala.reflect.ClassTag"
  lazy val ImplicitlyMethod = q"${hygienicRef(scala.Predef)}.implicitly"

  def hygienicRef(sym: Symbol): Tree = {
    @tailrec
    def loop(parts: List[String], res: Tree): Tree = {
      val part :: rest = parts
      if (rest.nonEmpty) loop(rest, q"$res.${TermName(part)}")
      else if (sym.isTerm || sym.isModuleClass) q"$res.${TermName(part)}"
      else tq"$res.${TypeName(part)}"
    }
    loop(sym.fullName.split("\\.").toList, q"${TermName("_root_")}")
  }

  def hygienicRef[T: TypeTag]: Tree = hygienicRef(symbolOf[T])
  def hygienicRef[T <: Singleton: TypeTag](x: T): Tree = hygienicRef(symbolOf[T])

  def typeRef(cdef: ClassDef, requireHk: Boolean, requireWildcards: Boolean): Tree = {
    if (requireWildcards && requireHk) sys.error("invalid combination of arguments")
    val ClassDef(_, name, tparams, _) = cdef
    if (requireHk || tparams.isEmpty) tq"$name"
    else if (requireWildcards) {
      val quantrefs = tparams.map(_ => c.freshName(TypeName("_")))
      val quantdefs = quantrefs.map(name => q"type $name")
      tq"$name[..$quantrefs] forSome { ..$quantdefs }"
    } else tq"$name[..${tparams.map(_.name)}]"
  }

  object AnyTpe {
    def unapply(tpe: Type): Boolean = tpe =:= definitions.AnyTpe
  }

  object PrimitiveTpe {
    @tailrec
    def unapply(tpe: Type): Boolean = tpe =:= typeOf[String] || tpe =:= typeOf[scala.Symbol] ||
      ScalaPrimitiveValueClasses.contains(tpe.typeSymbol) ||
      tpe.typeSymbol == definitions.ClassClass ||
      tpe.typeSymbol == definitions.OptionClass && PrimitiveTpe.unapply(tpe.typeArgs.head)
  }

  object TreeTpe {
    def unapply(tpe: Type): Boolean = tpe <:< c.mirror.staticClass("scala.meta.Tree").asType.toType
  }

  object OptionTreeTpe {
    def unapply(tpe: Type): Option[Type] =
      if (tpe.typeSymbol == c.mirror.staticClass("scala.Option")) tpe.typeArgs match {
        case (tpe @ TreeTpe()) :: Nil => Some(tpe)
        case _ => None
      }
      else None
  }

  object ListTreeTpe {
    def unapply(tpe: Type): Option[Type] =
      if (isListSymbol(tpe.typeSymbol)) tpe.typeArgs match {
        case (tpe @ TreeTpe()) :: Nil => Some(tpe)
        case _ => None
      }
      else None
  }

  object OptionListTreeTpe {
    def unapply(tpe: Type): Option[Type] =
      if (tpe.typeSymbol == c.mirror.staticClass("scala.Option")) tpe.typeArgs match {
        case ListTreeTpe(tpe) :: Nil => Some(tpe)
        case _ => None
      }
      else None
  }

  object ListListTreeTpe {
    def unapply(tpe: Type): Option[Type] =
      if (isListSymbol(tpe.typeSymbol)) tpe.typeArgs match {
        case ListTreeTpe(tpe) :: Nil => Some(tpe)
        case _ => None
      }
      else None
  }

  private def isListSymbol(sym: Symbol): Boolean =
    sym == c.mirror.staticClass("scala.collection.immutable.List") || {
      val typeSeq = typeOf[Seq[_]]
      sym == typeSeq.typeSymbol || typeSeq.baseClasses.contains(sym)
    }

}

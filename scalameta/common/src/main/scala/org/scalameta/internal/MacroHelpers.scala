package org.scalameta
package internal

import scala.reflect.macros.blackbox.Context

trait MacroHelpers extends DebugFinder
                      with FreeLocalFinder
                      with ImplTransformers {
  import c.universe._
  import definitions._
  import scala.reflect.internal.Flags._

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
      def hasNonEmpty(anns: List[Annotation]) = anns.exists(_.tree.tpe =:= typeOf[org.scalameta.invariants.nonEmpty])
      hasNonEmpty(sym.annotations) || hasNonEmpty(tptAnns)
    }
  }

  lazy val InvariantFailedRaiseMethod = q"${hygienicRef(org.scalameta.invariants.InvariantFailedException)}.raise"
  lazy val InvariantsRequireMethod = q"${hygienicRef(org.scalameta.invariants.`package`)}.require"
  lazy val UnreachableErrorModule = hygienicRef(org.scalameta.UnreachableError)
  lazy val DataAnnotation = tq"_root_.org.scalameta.data.data"
  lazy val DataTyperMacrosModule = hygienicRef(org.scalameta.data.DataTyperMacros)
  lazy val AdtPackage = q"_root_.org.scalameta.adt"
  lazy val AdtMetadataModule = hygienicRef(org.scalameta.adt.Metadata)
  lazy val AdtTyperMacrosModule = hygienicRef(org.scalameta.adt.AdtTyperMacros)
  lazy val AstMetadataModule = hygienicRef(scala.meta.internal.ast.Metadata)
  lazy val CommonTyperMacrosModule = hygienicRef(scala.meta.internal.ast.CommonTyperMacros)
  lazy val CommonTyperMacrosBundle = hygienicRef[scala.meta.internal.ast.CommonTyperMacrosBundle]
  lazy val AstInfoClass = tq"_root_.scala.meta.internal.ast.AstInfo"
  lazy val TokenMetadataModule = hygienicRef(scala.meta.internal.tokens.Metadata)
  lazy val BooleanClass = hygienicRef[scala.Boolean]
  lazy val IntClass = hygienicRef[scala.Int]
  lazy val AnyClass = hygienicRef[scala.Any]
  lazy val AnyRefClass = hygienicRef[scala.AnyRef]
  lazy val NothingClass = tq"_root_.scala.Nothing"
  lazy val OptionClass = hygienicRef[scala.Option[_]]
  lazy val SomeClass = hygienicRef[scala.Some[_]]
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
  lazy val IteratorClass = hygienicRef[Iterator[_]]
  lazy val SeqClass = hygienicRef[scala.collection.immutable.Seq[_]]
  lazy val ListBufferModule = hygienicRef(scala.collection.mutable.ListBuffer)
  lazy val UnitClass = hygienicRef[scala.Unit]
  lazy val ClassClass = hygienicRef[java.lang.Class[_]]
  lazy val ClassTagClass = hygienicRef[scala.reflect.ClassTag[_]]
  lazy val ImplicitlyMethod = q"${hygienicRef(scala.Predef)}.implicitly"

  private def fqRef(fqName: String, isTerm: Boolean): Tree = {
    def loop(parts: List[String]): Tree = parts match {
      case Nil :+ part => q"${TermName(part)}"
      case rest :+ part => q"${loop(rest)}.${TermName(part)}"
    }
    val prefix :+ last = fqName.split("\\.").toList
    if (isTerm) q"${loop(prefix)}.${TermName(last)}" else tq"${loop(prefix)}.${TypeName(last)}"
  }
  def hygienicRef(sym: Symbol): Tree = fqRef("_root_." + sym.fullName, isTerm = sym.isTerm || sym.isModuleClass)
  def hygienicRef[T: TypeTag]: Tree = hygienicRef(symbolOf[T])
  def hygienicRef[T <: Singleton : TypeTag](x: T): Tree = hygienicRef(symbolOf[T])

  def typeRef(cdef: ClassDef, requireHk: Boolean, requireWildcards: Boolean): Tree = {
    if (requireWildcards && requireHk) sys.error("invalid combination of arguments")
    val ClassDef(_, name, tparams, _) = cdef
    if (requireHk || tparams.isEmpty) {
      tq"$name"
    } else {
      if (requireWildcards) {
        val quantrefs = tparams.map(_ => c.freshName(TypeName("_")))
        val quantdefs = quantrefs.map(name => q"type $name")
        tq"$name[..$quantrefs] forSome { ..$quantdefs }"
      } else {
        tq"$name[..${tparams.map(_.name)}]"
      }
    }
  }

  object AnyTpe {
    def unapply(tpe: Type): Option[Type] = {
      if (tpe =:= definitions.AnyTpe) Some(tpe)
      else None
    }
  }

  object PrimitiveTpe {
    def unapply(tpe: Type): Option[Type] = {
      if (tpe =:= typeOf[String] ||
          tpe =:= typeOf[scala.Symbol] ||
          ScalaPrimitiveValueClasses.contains(tpe.typeSymbol)) Some(tpe)
      else if (tpe.typeSymbol == symbolOf[Option[_]] && PrimitiveTpe.unapply(tpe.typeArgs.head).nonEmpty) Some(tpe)
      else if (tpe.typeSymbol == symbolOf[Class[_]]) Some(tpe)
      else None
    }
  }

  object TreeTpe {
    def unapply(tpe: Type): Option[Type] = {
      if (tpe <:< c.mirror.staticClass("scala.meta.Tree").asType.toType) Some(tpe)
      else None
    }
  }

  object OptionTreeTpe {
    def unapply(tpe: Type): Option[Type] = {
      if (tpe.typeSymbol == c.mirror.staticClass("scala.Option")) {
        tpe.typeArgs match {
          case TreeTpe(tpe) :: Nil => Some(tpe)
          case _ => None
        }
      } else None
    }
  }

  object SeqTreeTpe {
    def unapply(tpe: Type): Option[Type] = {
      if (tpe.typeSymbol == c.mirror.staticClass("scala.collection.immutable.Seq")) {
        tpe.typeArgs match {
          case TreeTpe(tpe) :: Nil => Some(tpe)
          case _ => None
        }
      } else None
    }
  }

  object OptionSeqTreeTpe {
    def unapply(tpe: Type): Option[Type] = {
      if (tpe.typeSymbol == c.mirror.staticClass("scala.Option")) {
        tpe.typeArgs match {
          case SeqTreeTpe(tpe) :: Nil => Some(tpe)
          case _ => None
        }
      } else None
    }
  }

  object SeqSeqTreeTpe {
    def unapply(tpe: Type): Option[Type] = {
      if (tpe.typeSymbol == c.mirror.staticClass("scala.collection.immutable.Seq")) {
        tpe.typeArgs match {
          case SeqTreeTpe(tpe) :: Nil => Some(tpe)
          case _ => None
        }
      } else None
    }
  }
}
package serializer.reflect.separate

import scala.reflect.macros.blackbox._
import scala.language.experimental.macros

package adt {
  trait Adt[T]
  object Adt {
    implicit def materialize[T]: Adt[T] = macro impl[T]
    def impl[T: c.WeakTypeTag](c: Context) = {
      import c.universe._
      val T = c.weakTypeOf[T]
      val adt = q"_root_.serializer.reflect.separate.adt"
      def validateLeaf(leaf: ClassSymbol) = {
        if (!leaf.isFinal && !leaf.isModuleClass) c.abort(c.enclosingPosition, s"not `final`: ${leaf.fullName}")
        if (!leaf.isCaseClass) c.abort(c.enclosingPosition, s"not `case`: ${leaf.fullName}")
        if (!leaf.typeParams.isEmpty) c.abort(c.enclosingPosition, s"not monomorphic: ${leaf.fullName}")
      }
      val sym = T.typeSymbol.asClass
      if (sym.isTrait) {
        if (!sym.isSealed) c.abort(c.enclosingPosition, s"not `sealed`: ${sym.fullName}")
        sym.knownDirectSubclasses.foreach(leaf => validateLeaf(leaf.asClass))
      } else {
        validateLeaf(sym)
      }
      q"new $adt.Adt[$T]{}"
    }
  }
}

package serialization {
  trait Serializer[T] {
    def serialize(x: T): String
  }

  object Serializer {
    implicit def materialize[T: adt.Adt]: Serializer[T] = macro impl[T]
    implicit def intSerializer: Serializer[Int] = new Serializer[Int] { def serialize(x: Int) = x.toString }
    implicit def stringSerializer: Serializer[String] = new Serializer[String] { def serialize(x: String) = x }

    def impl[T: c.WeakTypeTag](c: Context)(adtEvidence: c.Tree) = {
      import c.universe._
      val T = c.weakTypeOf[T]
      val serialization = q"_root_.serializer.reflect.separate.serialization"
      val sym = T.typeSymbol.asClass
      val objName = c.freshName(TermName(sym.name + "Serializer"))
      val paramName = c.freshName(TermName("x"))
      def serializerFor(sym: ClassSymbol, tagged: Boolean) = {
        def tagFor(sym: ClassSymbol) = {
          val family = weakTypeOf[T].typeSymbol.asClass.knownDirectSubclasses
          family.toList.sortBy(_.name.toString).indexOf(sym).toString
        }
        val fieldNames = sym.primaryConstructor.asMethod.paramLists.head.map(_.name.toTermName)
        var fieldJson = fieldNames.map(fieldName => q"""${"\"" + fieldName.toString + "\": "} + $serialization.serialize($paramName.${fieldName})""")
        if (tagged) fieldJson = fieldJson :+ q"${"$tag: " + tagFor(sym)}"
        val unwrappedResult = fieldJson.foldLeft(None: Option[Tree])((acc, curr) => acc.map(acc => q"$acc + ${", "} + $curr").orElse(Some(curr)))
        q""" "{" + ${unwrappedResult.getOrElse(q"")} + "}" """
      }
      val body = {
        if (sym.isTrait) {
          val leafs = sym.knownDirectSubclasses.map(_.asClass)
          val clauses = leafs.map(leaf => cq"$paramName: ${leaf.toType} => ${serializerFor(leaf, tagged = true)}")
          q"$paramName match { case ..$clauses }"
        } else {
          serializerFor(sym, tagged = false)
        }
      }
      q"""
        implicit object $objName extends $serialization.Serializer[$T] {
          def serialize($paramName: $T): String = $body
        }
        $objName
      """
    }
  }

  object serialize {
    def apply[T](x: T)(implicit ev: Serializer[T]) = ev.serialize(x)
  }
}

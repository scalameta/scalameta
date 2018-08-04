package scala.meta.internal.semanticdb.javac.semantics

import javax.lang.model.`type`._
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._

trait TypeMirrors {

  val ObjectType = s.TypeRef(symbol = "java/lang/Object#")

  implicit class TypeMirrorOps(mirror: TypeMirror) {
    def tpe: s.Type = mirror match {
      case mirror: NoType if mirror.getKind == TypeKind.VOID =>
        s.TypeRef(symbol = "scala/Unit#")
      case mirror: DeclaredType =>
        val prefix = {
          val enclosing = mirror.getEnclosingType
          if (enclosing.getKind == TypeKind.NONE) s.NoType
          else enclosing.tpe
        }
        def prefixIsTrivial(tpe: s.Type): Boolean = tpe match {
          case s.NoType => true
          case s.TypeRef(_, _, args) if args.nonEmpty => false
          case s.TypeRef(prefix, _, _) => prefixIsTrivial(prefix)
        }
        val args = mirror.getTypeArguments.asScala.map(_.tpe)
        s.TypeRef(
          prefix = if (prefixIsTrivial(prefix)) s.NoType else prefix,
          symbol = mirror.asElement().sym,
          typeArguments = args
        )
      case mirror: ArrayType =>
        s.TypeRef(
          symbol = "scala/Array#",
          typeArguments = Seq(mirror.getComponentType.tpe)
        )
      case mirror: PrimitiveType =>
        val sym = mirror.getKind match {
          case TypeKind.BYTE => "scala/Byte#"
          case TypeKind.SHORT => "scala/Short#"
          case TypeKind.INT => "scala/Int#"
          case TypeKind.LONG => "scala/Long#"
          case TypeKind.CHAR => "scala/Char#"
          case TypeKind.FLOAT => "scala/Float#"
          case TypeKind.DOUBLE => "scala/Double#"
          case TypeKind.BOOLEAN => "scala/Boolean#"
          case _ => sys.error(mirror.toString)
        }
        s.TypeRef(symbol = sym)
      case mirror: TypeVariable =>
        s.TypeRef(
          symbol = mirror.asElement().sym
        )
      case mirror: WildcardType =>
        // FIXME: https://github.com/scalameta/scalameta/issues/1703
        s.TypeRef(
          symbol = "local_wildcard"
        )
    }
  }

}

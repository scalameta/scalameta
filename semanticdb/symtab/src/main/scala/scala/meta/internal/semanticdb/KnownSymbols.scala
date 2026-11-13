package scala.meta.internal.semanticdb

import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d}

object KnownSymbols {

  private[internal] val Any = "scala/Any#"
  private[internal] val AnyRef = "scala/AnyRef#"
  private[internal] val Unit = "scala/Unit#"
  private[internal] val Boolean = "scala/Boolean#"
  private[internal] val Int = "scala/Int#"
  private[internal] val Array = "scala/Array#"
  private[internal] val Seq = "scala/package.Seq#"
  private[internal] val Function0 = "scala/Function0#"
  private[internal] val Annotation = "scala/annotation/Annotation#"
  private[internal] val ClassfileAnnotation = "scala/annotation/ClassfileAnnotation#"
  private[internal] val Strictfp = "scala/annotation/strictfp#"
  private[internal] val MacroImpl = "scala/reflect/macros/internal/macroImpl#"
  private[internal] val Object = "java/lang/Object#"
  private[internal] val String = "java/lang/String#"
  private[internal] val Class = "java/lang/Class#"

  private def withTParam(ref: String, name: String) = ref.global(d.TypeParameter(name))

  private val AsInstanceOfDesc = d.Method("asInstanceOf", "()")
  private val AnyAsInstanceOf = Any.global(AsInstanceOfDesc)

  private val SynchronizedDesc = d.Method("synchronized", "()")
  private val AnyRefSynchronized = AnyRef.global(SynchronizedDesc)
  private val ObjectSynchronized = Object.global(SynchronizedDesc)

  private val Predef = "scala/Predef."
  private[internal] val PredefClassOf = Predef.global(d.Method("classOf", "()"))

  private[internal] def anyAsInstanceOfTParam(tp: String): String = withTParam(AnyAsInstanceOf, tp)
  private[internal] def anyRefSynchronizedTParam(tp: String): String =
    withTParam(AnyRefSynchronized, tp)
  private[internal] def objectSynchronizedTParam(tp: String): String =
    withTParam(ObjectSynchronized, tp)

}

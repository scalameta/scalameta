package types

import scala.language.existentials
import scala.language.higherKinds

class ann[T](x: T) extends scala.annotation.StaticAnnotation
class ann1 extends scala.annotation.StaticAnnotation
class ann2 extends scala.annotation.StaticAnnotation

class B

class C

class P {
  class C
  class X
  val x = new X
}

class T {
  class C
  class X
  val x = new X
}

object Test {
  class M {
    def m: Int = ???
  }

  class N {
    def n: Int = ???
  }

  class C extends M {
    val p = new P
    val x = p.x

    val typeRef1: C = ???
    val typeRef2: p.C = ???
    val typeRef3: T#C = ???

    val singleType1: x.type = ???
    val singleType2: p.x.type = ???

    val thisType1: this.type = ???
    val thisType2: C.this.type = ???

    val superType1 = super.m
    val superType2 = super[M].m
    val superType3 = C.super[M].m

    val compoundType1: { def k: Int } = ???
    val compoundType2: M with N = ???
    val compoundType3: M with N { def k: Int } = ???

    val annType1: T @ann(42) = ???
    val annType2: T @ann1 @ann2 = ???

    val existentialType1: T forSome { type T } = ???

    def typeLambda1[M[_]] = ???
    typeLambda1[({ type λ[T] = List[T] })#λ]

    object ClassInfoType1
    class ClassInfoType2 extends B { def x = 42 }
    trait ClassInfoType3[T]

    object MethodType {
      def x1: Int = ???
      def x2: Int = ???
      def m3: Int = ???
      def m4(): Int = ???
      def m5(x: Int): Int = ???
      def m6[T](x: T): T = ???
    }

    object ByNameType {
      def m1(x: => Int): Int = ???
    }

    object RepeatedType {
      def m1(x: Int*): Int = ???
    }

    object TypeType {
      type T1
      def m2[T2 >: C <: C] = ???
      def m3[M3[_]] = ???
      type T4 = C
      type T5[U] = U
    }
  }
}

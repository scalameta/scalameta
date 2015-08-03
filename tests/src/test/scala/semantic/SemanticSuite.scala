import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.internal.{ast => impl}
import scala.meta.dialects.Scala211

class SemanticDummy {
  private[this] val x = 2
  val y = 3
  def foo(w: Int = 4) = w
}

class SemanticSuite extends FunSuite {
  private val classpathOptions = s"-cp ${sys.props("sbt.paths.tests.classpath")}"
  private val pluginOptions = s"-Xplugin:${sys.props("sbt.paths.plugin.jar")} -Xplugin-require:scalahost"
  private val options = classpathOptions + " " + pluginOptions
  implicit val c = Scalahost.mkStandaloneContext(options)

  test("subtyping") {
    assert(t"List[Int]" <:< t"List[Any]")
  }

  // FIXME: uncomment this once https://github.com/scalameta/scalameta/issues/166 is fixed
  // test("q\"List(1, 2, 3)\".tpe") {
  //   intercept[SemanticException] {
  //     val expectedFail = "implementation restriction: internal cache has no type associated with List(1, 2, 3)"
  //     try q"List(1, 2, 3)".tpe
  //     catch { case ex: SemanticException => assert(ex.message.trim.startsWith(expectedFail)); throw ex }
  //   }
  //   val classDef = c.define("class C1 { def foo = List(1, 2, 3) }")
  //   classDef match {
  //     case impl.Source(List(impl.Defn.Class(_, _, _, _, impl.Template(_, _, _, Some(List(impl.Defn.Def(_, _, _, _, _, body))))))) =>
  //       assert(body.show[Code] == "List(1, 2, 3)")
  //       assert(body.show[Semantics] === """
  //         |Term.Apply(Term.Name("List")[1], List(Lit.Int(1), Lit.Int(2), Lit.Int(3)))
  //         |[1] Type.Singleton(Term.Name("immutable")[2])::scala.collection.immutable.List
  //         |[2] Type.Singleton(Term.Name("collection")[3])::scala.collection.immutable
  //         |[3] Type.Singleton(Term.Name("scala")[4])::scala.collection
  //         |[4] Type.Singleton(Term.Name("_root_")[5])::scala
  //         |[5] 0::_root_
  //       """.trim.stripMargin)
  //       assert(body.tpe.show[Code] == "List[Int]")
  //       assert(body.tpe.show[Semantics] == """
  //         |Type.Apply(Type.Name("List")[1], List(Type.Name("Int")[2]))
  //         |[1] Type.Singleton(Term.Name("immutable")[4])::scala.collection.immutable#List
  //         |[2] Type.Singleton(Term.Name("scala")[3])::scala#Int
  //         |[3] Type.Singleton(Term.Name("_root_")[5])::scala
  //         |[4] Type.Singleton(Term.Name("collection")[6])::scala.collection.immutable
  //         |[5] 0::_root_
  //         |[6] Type.Singleton(Term.Name("scala")[3])::scala.collection
  //       """.trim.stripMargin)
  //   }
  // }

  // FIXME: uncomment this once https://github.com/scalameta/scalameta/issues/166 is fixed
  // test("q\"x => x + x\".tpe") {
  //   val result = c.define("class C2 { def x = List(1).map(x => x + x) }")
  //   val impl.Source(List(impl.Defn.Class(_, _, _, _, impl.Template(_, _, _, Some(List(impl.Defn.Def(_, _, _, _, _, body))))))) = result
  //   val impl.Term.Apply(_, List(impl.Term.Function(List(x), _))) = body
  //   assert(x.tpe == t"Int")
  // }

  // FIXME: uncomment this once https://github.com/scalameta/scalameta/issues/166 is fixed
  // test("q\"1 +: List(2, 3)\".tpe") {
  //   val classDef = c.define("class C3 { def foo = 1 +: List(2, 3) }")
  //   classDef match {
  //     case impl.Source(List(impl.Defn.Class(_, _, _, _, impl.Template(_, _, _, Some(List(impl.Defn.Def(_, _, _, _, _, body))))))) =>
  //       assert(body.show[Code] == "1 +: List(2, 3)")
  //       assert(body.tpe.show[Code] == "List[Int]")
  //   }
  // }

  // FIXME: uncomment this once https://github.com/scalameta/scalameta/issues/171 is fixed
  // test("t\"List\".defn") {
  //   assert(t"List".defn.show[Code] == """@ffi("jvmErasure(Lscala/collection/immutable/List;)") type List[+A] = List[A]""")
  //   assert(t"List".defn.show[Semantics] == """
  //     |Defn.Type(List(Mod.Ffi("jvmErasure(Lscala/collection/immutable/List;)")), Type.Name("List")[1], List(Type.Param(List(Mod.Covariant()), Type.Name("A")[2], Nil, Type.Bounds(None, None), Nil, Nil)), Type.Apply(Type.Name("List")[3], List(Type.Name("A")[2])))
  //     |[1] Type.Singleton(Term.Name("package")[4])::scala.package#List
  //     |[2] 0::scala.package#List#A
  //     |[3] Type.Singleton(Term.Name("immutable")[5])::scala.collection.immutable#List
  //     |[4] Type.Singleton(Term.Name("scala")[6])::scala.package
  //     |[5] Type.Singleton(Term.Name("collection")[7])::scala.collection.immutable
  //     |[6] Type.Singleton(Term.Name("_root_")[8])::scala
  //     |[7] Type.Singleton(Term.Name("scala")[6])::scala.collection
  //     |[8] 0::_root_
  //   """.trim.stripMargin)
  // }

  test("t\"List[Int]\".dealias") {
    assert(t"List[Int]".dealias.show[Syntax] == "List[Int]")
    assert(t"List[Int]".dealias.show[Semantics] == """
      |Type.Apply(Type.Name("List")[1], List(Type.Name("Int")[2]))
      |[1] Type.Singleton(Term.Name("immutable")[3]{1})::scala.collection.immutable#List
      |[2] Type.Singleton(Term.Name("scala")[4])::scala#Int
      |[3] Type.Singleton(Term.Name("collection")[5]{2})::scala.collection.immutable
      |[4] Type.Singleton(Term.Name("_root_")[6])::scala
      |[5] Type.Singleton(Term.Name("scala")[4]{3})::scala.collection
      |[6] 0::_root_
      |{1} Type.Singleton(Term.Name("immutable")[3]{1})
      |{2} Type.Singleton(Term.Name("collection")[5]{2})
      |{3} Type.Singleton(Term.Name("scala")[4]{3})
    """.trim.stripMargin)
  }

  test("t\"List[_]\".dealias") {
    intercept[SemanticException] {
      val expectedFail = "Input scala.meta tree is not fully attributed and can't be converted to a scala.reflect artifact."
      try t"List[_]".dealias
      catch { case ex: SemanticException => assert(ex.message.trim.startsWith(expectedFail)); throw ex }
    }
  }

  // FIXME: uncomment this once https://github.com/scalameta/scalameta/issues/171 is fixed
  // test("t\"List\".members") {
  //   assert(t"List".members.map(_.show[Code]).mkString(EOL) === """
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, $lessinit$greater, ()Lscala/collection/immutable/List;)") ()
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, companion, ()Lscala/collection/generic/GenericCompanion;)") override def companion: GenericCompanion[List] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, $colon$colon, (Ljava/lang/Object;)Lscala/collection/immutable/List;)") def ::[B >: A](x: B): List[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, $colon$colon$colon, (Lscala/collection/immutable/List;)Lscala/collection/immutable/List;)") def :::[B >: A](prefix: List[B]): List[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, reverse_$colon$colon$colon, (Lscala/collection/immutable/List;)Lscala/collection/immutable/List;)") def reverse_:::[B >: A](prefix: List[B]): List[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, mapConserve, (Lscala/Function1;)Lscala/collection/immutable/List;)") @inline final def mapConserve[B >: A <: AnyRef](f: A => B): List[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, $plus$plus, (Lscala/collection/GenTraversableOnce;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, $plus$colon, (Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, toList, ()Lscala/collection/immutable/List;)") override def toList: List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, take, (I)Lscala/collection/immutable/List;)") override def take(n: Int): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, drop, (I)Lscala/collection/immutable/List;)") override def drop(n: Int): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, slice, (II)Lscala/collection/immutable/List;)") override def slice(from: Int, until: Int): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, takeRight, (I)Lscala/collection/immutable/List;)") override def takeRight(n: Int): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, splitAt, (I)Lscala/Tuple2;)") override def splitAt(n: Int): (List[A], List[A]) = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, map, (Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") @noinline final override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, collect, (Lscala/PartialFunction;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") @noinline final override def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, flatMap, (Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") @noinline final override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, takeWhile, (Lscala/Function1;)Lscala/collection/immutable/List;)") @inline final override def takeWhile(p: A => Boolean): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, dropWhile, (Lscala/Function1;)Lscala/collection/immutable/List;)") @inline final override def dropWhile(p: A => Boolean): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, span, (Lscala/Function1;)Lscala/Tuple2;)") @inline final override def span(p: A => Boolean): (List[A], List[A]) = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, foreach, (Lscala/Function1;)V)") @inline final override def foreach[U](f: A => U): Unit = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, reverse, ()Lscala/collection/immutable/List;)") override def reverse: List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, foldRight, (Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;)") override def foldRight[B](z: B)(op: (A, B) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, stringPrefix, ()Ljava/lang/String;)") override def stringPrefix: String = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, toStream, ()Lscala/collection/immutable/Stream;)") override def toStream: Stream[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, writeReplace, ()Ljava/lang/Object;)") protected final def writeReplace(): AnyRef = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, length, ()I)") def length: Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, apply, (I)Ljava/lang/Object;)") def apply(n: Int): A = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, forall, (Lscala/Function1;)Z)") override def forall(p: A => Boolean): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, exists, (Lscala/Function1;)Z)") override def exists(p: A => Boolean): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, contains, (Ljava/lang/Object;)Z)") override def contains[A1 >: A](elem: A1): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, find, (Lscala/Function1;)Lscala/Option;)") override def find(p: A => Boolean): Option[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, foldLeft, (Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;)") override def foldLeft[B](z: B)(f: (B, A) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, reduceLeft, (Lscala/Function2;)Ljava/lang/Object;)") override def reduceLeft[B >: A](f: (B, A) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, reduceRight, (Lscala/Function2;)Ljava/lang/Object;)") override def reduceRight[B >: A](op: (A, B) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, last, ()Ljava/lang/Object;)") override def last: A = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, dropRight, (I)Lscala/collection/LinearSeqOptimized;)") override def dropRight(n: Int): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, sameElements, (Lscala/collection/GenIterable;)Z)") override def sameElements[B >: A](that: GenIterable[B]): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, lengthCompare, (I)I)") override def lengthCompare(len: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, isDefinedAt, (I)Z)") override def isDefinedAt(x: Int): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, segmentLength, (Lscala/Function1;I)I)") override def segmentLength(p: A => Boolean, from: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, indexWhere, (Lscala/Function1;I)I)") override def indexWhere(p: A => Boolean, from: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, lastIndexWhere, (Lscala/Function1;I)I)") override def lastIndexWhere(p: A => Boolean, end: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/Product;, productElement, (I)Ljava/lang/Object;)") def productElement(n: Int): Any
  //     |@ffi("jvmMethod(Lscala/Product;, productArity, ()I)") def productArity: Int
  //     |@ffi("jvmMethod(Lscala/Product;, productIterator, ()Lscala/collection/Iterator;)") def productIterator: Iterator[Any] = ???
  //     |@ffi("jvmMethod(Lscala/Product;, productPrefix, ()Ljava/lang/String;)") def productPrefix: String = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/LinearSeq;, seq, ()Lscala/collection/immutable/LinearSeq;)") override def seq: LinearSeq[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqLike;, thisCollection, ()Lscala/collection/LinearSeq;)") protected[this] override def thisCollection: LinearSeq[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqLike;, toCollection, (Lscala/collection/LinearSeqLike;)Lscala/collection/LinearSeq;)") protected[this] override def toCollection(repr: List[A]): LinearSeq[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqLike;, hashCode, ()I)") override def hashCode(): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqLike;, iterator, ()Lscala/collection/Iterator;)") override def iterator: Iterator[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqLike;, corresponds, (Lscala/collection/GenSeq;Lscala/Function2;)Z)") @tailrec final override def corresponds[B](that: GenSeq[B])(p: (A, B) => Boolean): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/Seq;, toSeq, ()Lscala/collection/immutable/Seq;)") override def toSeq: Seq[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/Seq;, parCombiner, ()Lscala/collection/parallel/Combiner;)") protected[this] override def parCombiner: Combiner[A, ParSeq[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, isEmpty, ()Z)") override def isEmpty: Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, size, ()I)") override def size: Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, permutations, ()Lscala/collection/Iterator;)") def permutations: Iterator[List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, combinations, (I)Lscala/collection/Iterator;)") def combinations(n: Int): Iterator[List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, reverseMap, (Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, reverseIterator, ()Lscala/collection/Iterator;)") def reverseIterator: Iterator[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, startsWith, (Lscala/collection/GenSeq;I)Z)") def startsWith[B](that: GenSeq[B], offset: Int): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, endsWith, (Lscala/collection/GenSeq;)Z)") def endsWith[B](that: GenSeq[B]): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, indexOfSlice, (Lscala/collection/GenSeq;)I)") def indexOfSlice[B >: A](that: GenSeq[B]): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, indexOfSlice, (Lscala/collection/GenSeq;I)I)") def indexOfSlice[B >: A](that: GenSeq[B], from: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, lastIndexOfSlice, (Lscala/collection/GenSeq;)I)") def lastIndexOfSlice[B >: A](that: GenSeq[B]): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, lastIndexOfSlice, (Lscala/collection/GenSeq;I)I)") def lastIndexOfSlice[B >: A](that: GenSeq[B], end: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, containsSlice, (Lscala/collection/GenSeq;)Z)") def containsSlice[B](that: GenSeq[B]): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, union, (Lscala/collection/GenSeq;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") override def union[B >: A, That](that: GenSeq[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, diff, (Lscala/collection/GenSeq;)Ljava/lang/Object;)") def diff[B >: A](that: GenSeq[B]): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, intersect, (Lscala/collection/GenSeq;)Ljava/lang/Object;)") def intersect[B >: A](that: GenSeq[B]): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, distinct, ()Ljava/lang/Object;)") def distinct: List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, patch, (ILscala/collection/GenSeq;ILscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, updated, (ILjava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, $colon$plus, (Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, padTo, (ILjava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, sortWith, (Lscala/Function2;)Ljava/lang/Object;)") def sortWith(lt: (A, A) => Boolean): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, sortBy, (Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;)") def sortBy[B](f: A => B)(implicit ord: Ordering[B]): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, sorted, (Lscala/math/Ordering;)Ljava/lang/Object;)") def sorted[B >: A](implicit ord: Ordering[B]): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, indices, ()Lscala/collection/immutable/Range;)") def indices: Range = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, view, ()Lscala/collection/SeqView;)") override def view: AnyRef with SeqView[A, List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, view, (II)Lscala/collection/SeqView;)") override def view(from: Int, until: Int): SeqView[A, List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, toString, ()Ljava/lang/String;)") override def toString(): String = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, prefixLength, (Lscala/Function1;)I)") def prefixLength(p: A => Boolean): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, indexWhere, (Lscala/Function1;)I)") def indexWhere(p: A => Boolean): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, indexOf, (Ljava/lang/Object;)I)") def indexOf[B >: A](elem: B): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, indexOf, (Ljava/lang/Object;I)I)") def indexOf[B >: A](elem: B, from: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, lastIndexOf, (Ljava/lang/Object;)I)") def lastIndexOf[B >: A](elem: B): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, lastIndexOf, (Ljava/lang/Object;I)I)") def lastIndexOf[B >: A](elem: B, end: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, lastIndexWhere, (Lscala/Function1;)I)") def lastIndexWhere(p: A => Boolean): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, startsWith, (Lscala/collection/GenSeq;)Z)") def startsWith[B](that: GenSeq[B]): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, equals, (Ljava/lang/Object;)Z)") override def equals(that: Any): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/PartialFunction;, orElse, (Lscala/PartialFunction;)Lscala/PartialFunction;)") def orElse[A1 <: Int, B1 >: A](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = ???
  //     |@ffi("jvmMethod(Lscala/PartialFunction;, andThen, (Lscala/Function1;)Lscala/PartialFunction;)") override def andThen[C](k: A => C): PartialFunction[Int, C] = ???
  //     |@ffi("jvmMethod(Lscala/PartialFunction;, lift, ()Lscala/Function1;)") def lift: Int => Option[A] = ???
  //     |@ffi("jvmMethod(Lscala/PartialFunction;, applyOrElse, (Ljava/lang/Object;Lscala/Function1;)Ljava/lang/Object;)") def applyOrElse[A1 <: Int, B1 >: A](x: A1, default: A1 => B1): B1 = ???
  //     |@ffi("jvmMethod(Lscala/PartialFunction;, runWith, (Lscala/Function1;)Lscala/Function1;)") def runWith[U](action: A => U): Int => Boolean = ???
  //     |@ffi("jvmMethod(Lscala/Function1;, compose, (Lscala/Function1;)Lscala/Function1;)") @unspecialized def compose[A](g: A => Int): A => A = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, toIterable, ()Lscala/collection/Iterable;)") override def toIterable: Iterable[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, toIterator, ()Lscala/collection/Iterator;)") @deprecatedOverriding("toIterator should stay consistent with iterator for all Iterables: override iterator instead.", "2.11.0") override def toIterator: Iterator[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, head, ()Ljava/lang/Object;)") override def head: A = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, grouped, (I)Lscala/collection/Iterator;)") def grouped(size: Int): Iterator[List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, sliding, (I)Lscala/collection/Iterator;)") def sliding(size: Int): Iterator[List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, sliding, (II)Lscala/collection/Iterator;)") def sliding(size: Int, step: Int): Iterator[List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, copyToArray, (Ljava/lang/Object;II)V)") override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, zip, (Lscala/collection/GenIterable;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[List[A], (A1, B), That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, zipAll, (Lscala/collection/GenIterable;Ljava/lang/Object;Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def zipAll[B, A1 >: A, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[List[A], (A1, B), That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, zipWithIndex, (Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[List[A], (A1, Int), That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, canEqual, (Ljava/lang/Object;)Z)") override def canEqual(that: Any): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, newBuilder, ()Lscala/collection/mutable/Builder;)") protected[this] def newBuilder: Builder[A, List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, genericBuilder, ()Lscala/collection/mutable/Builder;)") def genericBuilder[B]: Builder[B, List[B]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, unzip, (Lscala/Function1;)Lscala/Tuple2;)") def unzip[A1, A2](implicit asPair: A => (A1, A2)): (List[A1], List[A2]) = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, unzip3, (Lscala/Function1;)Lscala/Tuple3;)") def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (List[A1], List[A2], List[A3]) = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, flatten, (Lscala/Function1;)Lscala/collection/GenTraversable;)") def flatten[B](implicit asTraversable: A => GenTraversableOnce[B]): List[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, transpose, (Lscala/Function1;)Lscala/collection/GenTraversable;)") @migration("`transpose` throws an `IllegalArgumentException` if collections are not uniformly sized.", "2.9.0") def transpose[B](implicit asTraversable: A => GenTraversableOnce[B]): List[List[B]] = ???
  //     |@ffi("jvmErasure(Ljava/lang/Object;)") protected[this] type Self = List[A]
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, repr, ()Ljava/lang/Object;)") def repr: List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, isTraversableAgain, ()Z)") final def isTraversableAgain: Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, hasDefiniteSize, ()Z)") def hasDefiniteSize: Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, $plus$plus$colon, (Lscala/collection/TraversableOnce;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def ++:[B >: A, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, $plus$plus$colon, (Lscala/collection/Traversable;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def ++:[B >: A, That](that: Traversable[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, filter, (Lscala/Function1;)Ljava/lang/Object;)") def filter(p: A => Boolean): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, filterNot, (Lscala/Function1;)Ljava/lang/Object;)") def filterNot(p: A => Boolean): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, partition, (Lscala/Function1;)Lscala/Tuple2;)") def partition(p: A => Boolean): (List[A], List[A]) = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, groupBy, (Lscala/Function1;)Lscala/collection/immutable/Map;)") def groupBy[K](f: A => K): Map[K, List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, scan, (Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, scanLeft, (Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, scanRight, (Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") @migration("The behavior of `scanRight` has changed. The previous behavior can be reproduced with scanRight.reverse.", "2.9.0") def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, headOption, ()Lscala/Option;)") def headOption: Option[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, tail, ()Ljava/lang/Object;)") override def tail: List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, lastOption, ()Lscala/Option;)") def lastOption: Option[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, init, ()Ljava/lang/Object;)") def init: List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, sliceWithKnownDelta, (III)Ljava/lang/Object;)") private[scala] def sliceWithKnownDelta(from: Int, until: Int, delta: Int): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, sliceWithKnownBound, (II)Ljava/lang/Object;)") private[scala] def sliceWithKnownBound(from: Int, until: Int): List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, tails, ()Lscala/collection/Iterator;)") def tails: Iterator[List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, inits, ()Lscala/collection/Iterator;)") def inits: Iterator[List[A]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, toTraversable, ()Lscala/collection/Traversable;)") @deprecatedOverriding("Enforce contract of toTraversable that if it is Traversable it returns itself.", "2.11.0") def toTraversable: Traversable[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, to, (Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") override def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, withFilter, (Lscala/Function1;)Lscala/collection/generic/FilterMonadic;)") def withFilter(p: A => Boolean): FilterMonadic[A, List[A]] = ???
  //     |@ffi("jvmErasure(Lscala/collection/TraversableLike$WithFilter;)") class WithFilter(p: A => Boolean) extends AnyRef with FilterMonadic[A, Repr] {
  //     |  @ffi("jvmField(Lscala/collection/TraversableLike$WithFilter;, p, Lscala/Function1;)") private[this] val p: A => Boolean = ???
  //     |  @ffi("jvmMethod(Lscala/collection/TraversableLike$WithFilter;, map, (Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |  @ffi("jvmMethod(Lscala/collection/TraversableLike$WithFilter;, flatMap, (Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
  //     |  @ffi("jvmMethod(Lscala/collection/TraversableLike$WithFilter;, foreach, (Lscala/Function1;)V)") def foreach[U](f: A => U): Unit = ???
  //     |  @ffi("jvmMethod(Lscala/collection/TraversableLike$WithFilter;, withFilter, (Lscala/Function1;)Lscala/collection/TraversableLike$WithFilter;)") def withFilter(q: A => Boolean): List#WithFilter = ???
  //     |}
  //     |@ffi("jvmMethod(Lscala/collection/Parallelizable;, par, ()Lscala/collection/Parallel;)") def par: ParSeq[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, reversed, ()Lscala/collection/immutable/List;)") protected[this] def reversed: List[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, nonEmpty, ()Z)") def nonEmpty: Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, count, (Lscala/Function1;)I)") def count(p: A => Boolean): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, collectFirst, (Lscala/PartialFunction;)Lscala/Option;)") def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, $div$colon, (Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;)") def /:[B](z: B)(op: (B, A) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, $colon$bslash, (Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;)") def :\[B](z: B)(op: (A, B) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, reduceLeftOption, (Lscala/Function2;)Lscala/Option;)") def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, reduceRightOption, (Lscala/Function2;)Lscala/Option;)") def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, reduce, (Lscala/Function2;)Ljava/lang/Object;)") def reduce[A1 >: A](op: (A1, A1) => A1): A1 = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, reduceOption, (Lscala/Function2;)Lscala/Option;)") def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, fold, (Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;)") def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, aggregate, (Lscala/Function0;Lscala/Function2;Lscala/Function2;)Ljava/lang/Object;)") def aggregate[B](z: => B)(seqop: (B, A) => B, combop: (B, B) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, sum, (Lscala/math/Numeric;)Ljava/lang/Object;)") def sum[B >: A](implicit num: Numeric[B]): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, product, (Lscala/math/Numeric;)Ljava/lang/Object;)") def product[B >: A](implicit num: Numeric[B]): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, min, (Lscala/math/Ordering;)Ljava/lang/Object;)") def min[B >: A](implicit cmp: Ordering[B]): A = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, max, (Lscala/math/Ordering;)Ljava/lang/Object;)") def max[B >: A](implicit cmp: Ordering[B]): A = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, maxBy, (Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;)") def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, minBy, (Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;)") def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, copyToBuffer, (Lscala/collection/mutable/Buffer;)V)") def copyToBuffer[B >: A](dest: Buffer[B]): Unit = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, copyToArray, (Ljava/lang/Object;I)V)") def copyToArray[B >: A](xs: Array[B], start: Int): Unit = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, copyToArray, (Ljava/lang/Object;)V)") def copyToArray[B >: A](xs: Array[B]): Unit = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toArray, (Lscala/reflect/ClassTag;)Ljava/lang/Object;)") def toArray[B >: A: ClassTag](implicit evidence$1: ClassTag[B]): Array[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toIndexedSeq, ()Lscala/collection/immutable/IndexedSeq;)") def toIndexedSeq: IndexedSeq[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toBuffer, ()Lscala/collection/mutable/Buffer;)") def toBuffer[B >: A]: Buffer[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toSet, ()Lscala/collection/immutable/Set;)") def toSet[B >: A]: Set[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toVector, ()Lscala/collection/immutable/Vector;)") def toVector: Vector[A] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toMap, (Lscala/Predef$$less$colon$less;)Lscala/collection/immutable/Map;)") def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, mkString, (Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;)") def mkString(start: String, sep: String, end: String): String = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, mkString, (Ljava/lang/String;)Ljava/lang/String;)") def mkString(sep: String): String = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, mkString, ()Ljava/lang/String;)") def mkString: String = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, addString, (Lscala/collection/mutable/StringBuilder;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Lscala/collection/mutable/StringBuilder;)") def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, addString, (Lscala/collection/mutable/StringBuilder;Ljava/lang/String;)Lscala/collection/mutable/StringBuilder;)") def addString(b: StringBuilder, sep: String): StringBuilder = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, addString, (Lscala/collection/mutable/StringBuilder;)Lscala/collection/mutable/StringBuilder;)") def addString(b: StringBuilder): StringBuilder = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, getClass, ()Ljava/lang/Class;)") final def getClass(): Class[?0] forSome { @ffi("jvmErasure(Ljava/lang/Object;)") type ?0 } = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, clone, ()Ljava/lang/Object;)") @throws[CloneNotSupportedException] protected[lang] def clone(): Object = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, notify, ()V)") final def notify(): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, notifyAll, ()V)") final def notifyAll(): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, wait, (J)V)") @throws[InterruptedException] final def wait(x$1: Long): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, wait, (JI)V)") @throws[InterruptedException] final def wait(x$1: Long, x$2: Int): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, wait, ()V)") @throws[InterruptedException] final def wait(): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, finalize, ()V)") @throws[Throwable] protected[lang] def finalize(): Unit = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, eq, (Ljava/lang/Object;)Z)") final def eq(x$1: AnyRef): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, ne, (Ljava/lang/Object;)Z)") final def ne(x$1: AnyRef): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, $eq$eq, (Ljava/lang/Object;)Z)") final def ==(x$1: Any): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, $bang$eq, (Ljava/lang/Object;)Z)") final def !=(x$1: Any): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, $hash$hash, ()I)") final def ##(): Int = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, synchronized, (Ljava/lang/Object;)Ljava/lang/Object;)") final def synchronized[T0](x$1: T0): T0 = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, isInstanceOf, ()Z)") final def isInstanceOf[T0]: Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, asInstanceOf, ()Ljava/lang/Object;)") final def asInstanceOf[T0]: T0 = ???
  //   """.trim.stripMargin)
  // }

  // FIXME: uncomment this once https://github.com/scalameta/scalameta/issues/171 is fixed
  // test("t\"List[Int]\".members") {
  //   assert(t"List[Int]".members.map(_.show[Code]).mkString(EOL) === """
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, $lessinit$greater, ()Lscala/collection/immutable/List;)") ()
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, companion, ()Lscala/collection/generic/GenericCompanion;)") override def companion: GenericCompanion[List] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, $colon$colon, (Ljava/lang/Object;)Lscala/collection/immutable/List;)") def ::[B >: Int](x: B): List[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, $colon$colon$colon, (Lscala/collection/immutable/List;)Lscala/collection/immutable/List;)") def :::[B >: Int](prefix: List[B]): List[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, reverse_$colon$colon$colon, (Lscala/collection/immutable/List;)Lscala/collection/immutable/List;)") def reverse_:::[B >: Int](prefix: List[B]): List[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, mapConserve, (Lscala/Function1;)Lscala/collection/immutable/List;)") @inline final def mapConserve[B >: Int <: AnyRef](f: Int => B): List[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, $plus$plus, (Lscala/collection/GenTraversableOnce;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") override def ++[B >: Int, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, $plus$colon, (Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") override def +:[B >: Int, That](elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, toList, ()Lscala/collection/immutable/List;)") override def toList: List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, take, (I)Lscala/collection/immutable/List;)") override def take(n: Int): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, drop, (I)Lscala/collection/immutable/List;)") override def drop(n: Int): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, slice, (II)Lscala/collection/immutable/List;)") override def slice(from: Int, until: Int): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, takeRight, (I)Lscala/collection/immutable/List;)") override def takeRight(n: Int): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, splitAt, (I)Lscala/Tuple2;)") override def splitAt(n: Int): (List[Int], List[Int]) = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, map, (Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") @noinline final override def map[B, That](f: Int => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, collect, (Lscala/PartialFunction;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") @noinline final override def collect[B, That](pf: PartialFunction[Int, B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, flatMap, (Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") @noinline final override def flatMap[B, That](f: Int => GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, takeWhile, (Lscala/Function1;)Lscala/collection/immutable/List;)") @inline final override def takeWhile(p: Int => Boolean): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, dropWhile, (Lscala/Function1;)Lscala/collection/immutable/List;)") @inline final override def dropWhile(p: Int => Boolean): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, span, (Lscala/Function1;)Lscala/Tuple2;)") @inline final override def span(p: Int => Boolean): (List[Int], List[Int]) = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, foreach, (Lscala/Function1;)V)") @inline final override def foreach[U](f: Int => U): Unit = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, reverse, ()Lscala/collection/immutable/List;)") override def reverse: List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, foldRight, (Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;)") override def foldRight[B](z: B)(op: (Int, B) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, stringPrefix, ()Ljava/lang/String;)") override def stringPrefix: String = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, toStream, ()Lscala/collection/immutable/Stream;)") override def toStream: Stream[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/List;, writeReplace, ()Ljava/lang/Object;)") protected final def writeReplace(): AnyRef = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, length, ()I)") def length: Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, apply, (I)Ljava/lang/Object;)") def apply(n: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, forall, (Lscala/Function1;)Z)") override def forall(p: Int => Boolean): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, exists, (Lscala/Function1;)Z)") override def exists(p: Int => Boolean): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, contains, (Ljava/lang/Object;)Z)") override def contains[A1 >: Int](elem: A1): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, find, (Lscala/Function1;)Lscala/Option;)") override def find(p: Int => Boolean): Option[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, foldLeft, (Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;)") override def foldLeft[B](z: B)(f: (B, Int) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, reduceLeft, (Lscala/Function2;)Ljava/lang/Object;)") override def reduceLeft[B >: Int](f: (B, Int) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, reduceRight, (Lscala/Function2;)Ljava/lang/Object;)") override def reduceRight[B >: Int](op: (Int, B) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, last, ()Ljava/lang/Object;)") override def last: Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, dropRight, (I)Lscala/collection/LinearSeqOptimized;)") override def dropRight(n: Int): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, sameElements, (Lscala/collection/GenIterable;)Z)") override def sameElements[B >: Int](that: GenIterable[B]): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, lengthCompare, (I)I)") override def lengthCompare(len: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, isDefinedAt, (I)Z)") override def isDefinedAt(x: Int): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, segmentLength, (Lscala/Function1;I)I)") override def segmentLength(p: Int => Boolean, from: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, indexWhere, (Lscala/Function1;I)I)") override def indexWhere(p: Int => Boolean, from: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqOptimized;, lastIndexWhere, (Lscala/Function1;I)I)") override def lastIndexWhere(p: Int => Boolean, end: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/Product;, productElement, (I)Ljava/lang/Object;)") def productElement(n: Int): Any
  //     |@ffi("jvmMethod(Lscala/Product;, productArity, ()I)") def productArity: Int
  //     |@ffi("jvmMethod(Lscala/Product;, productIterator, ()Lscala/collection/Iterator;)") def productIterator: Iterator[Any] = ???
  //     |@ffi("jvmMethod(Lscala/Product;, productPrefix, ()Ljava/lang/String;)") def productPrefix: String = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/LinearSeq;, seq, ()Lscala/collection/immutable/LinearSeq;)") override def seq: LinearSeq[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqLike;, thisCollection, ()Lscala/collection/LinearSeq;)") protected[this] override def thisCollection: LinearSeq[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqLike;, toCollection, (Lscala/collection/LinearSeqLike;)Lscala/collection/LinearSeq;)") protected[this] override def toCollection(repr: List[Int]): LinearSeq[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqLike;, hashCode, ()I)") override def hashCode(): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqLike;, iterator, ()Lscala/collection/Iterator;)") override def iterator: Iterator[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/LinearSeqLike;, corresponds, (Lscala/collection/GenSeq;Lscala/Function2;)Z)") @tailrec final override def corresponds[B](that: GenSeq[B])(p: (Int, B) => Boolean): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/Seq;, toSeq, ()Lscala/collection/immutable/Seq;)") override def toSeq: Seq[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/immutable/Seq;, parCombiner, ()Lscala/collection/parallel/Combiner;)") protected[this] override def parCombiner: Combiner[Int, ParSeq[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, isEmpty, ()Z)") override def isEmpty: Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, size, ()I)") override def size: Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, permutations, ()Lscala/collection/Iterator;)") def permutations: Iterator[List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, combinations, (I)Lscala/collection/Iterator;)") def combinations(n: Int): Iterator[List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, reverseMap, (Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def reverseMap[B, That](f: Int => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, reverseIterator, ()Lscala/collection/Iterator;)") def reverseIterator: Iterator[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, startsWith, (Lscala/collection/GenSeq;I)Z)") def startsWith[B](that: GenSeq[B], offset: Int): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, endsWith, (Lscala/collection/GenSeq;)Z)") def endsWith[B](that: GenSeq[B]): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, indexOfSlice, (Lscala/collection/GenSeq;)I)") def indexOfSlice[B >: Int](that: GenSeq[B]): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, indexOfSlice, (Lscala/collection/GenSeq;I)I)") def indexOfSlice[B >: Int](that: GenSeq[B], from: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, lastIndexOfSlice, (Lscala/collection/GenSeq;)I)") def lastIndexOfSlice[B >: Int](that: GenSeq[B]): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, lastIndexOfSlice, (Lscala/collection/GenSeq;I)I)") def lastIndexOfSlice[B >: Int](that: GenSeq[B], end: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, containsSlice, (Lscala/collection/GenSeq;)Z)") def containsSlice[B](that: GenSeq[B]): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, union, (Lscala/collection/GenSeq;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") override def union[B >: Int, That](that: GenSeq[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, diff, (Lscala/collection/GenSeq;)Ljava/lang/Object;)") def diff[B >: Int](that: GenSeq[B]): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, intersect, (Lscala/collection/GenSeq;)Ljava/lang/Object;)") def intersect[B >: Int](that: GenSeq[B]): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, distinct, ()Ljava/lang/Object;)") def distinct: List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, patch, (ILscala/collection/GenSeq;ILscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def patch[B >: Int, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, updated, (ILjava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def updated[B >: Int, That](index: Int, elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, $colon$plus, (Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def :+[B >: Int, That](elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, padTo, (ILjava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def padTo[B >: Int, That](len: Int, elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, sortWith, (Lscala/Function2;)Ljava/lang/Object;)") def sortWith(lt: (Int, Int) => Boolean): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, sortBy, (Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;)") def sortBy[B](f: Int => B)(implicit ord: Ordering[B]): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, sorted, (Lscala/math/Ordering;)Ljava/lang/Object;)") def sorted[B >: Int](implicit ord: Ordering[B]): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, indices, ()Lscala/collection/immutable/Range;)") def indices: Range = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, view, ()Lscala/collection/SeqView;)") override def view: AnyRef with SeqView[Int, List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, view, (II)Lscala/collection/SeqView;)") override def view(from: Int, until: Int): SeqView[Int, List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/SeqLike;, toString, ()Ljava/lang/String;)") override def toString(): String = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, prefixLength, (Lscala/Function1;)I)") def prefixLength(p: Int => Boolean): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, indexWhere, (Lscala/Function1;)I)") def indexWhere(p: Int => Boolean): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, indexOf, (Ljava/lang/Object;)I)") def indexOf[B >: Int](elem: B): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, indexOf, (Ljava/lang/Object;I)I)") def indexOf[B >: Int](elem: B, from: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, lastIndexOf, (Ljava/lang/Object;)I)") def lastIndexOf[B >: Int](elem: B): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, lastIndexOf, (Ljava/lang/Object;I)I)") def lastIndexOf[B >: Int](elem: B, end: Int): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, lastIndexWhere, (Lscala/Function1;)I)") def lastIndexWhere(p: Int => Boolean): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, startsWith, (Lscala/collection/GenSeq;)Z)") def startsWith[B](that: GenSeq[B]): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/GenSeqLike;, equals, (Ljava/lang/Object;)Z)") override def equals(that: Any): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/PartialFunction;, orElse, (Lscala/PartialFunction;)Lscala/PartialFunction;)") def orElse[A1 <: Int, B1 >: Int](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = ???
  //     |@ffi("jvmMethod(Lscala/PartialFunction;, andThen, (Lscala/Function1;)Lscala/PartialFunction;)") override def andThen[C](k: Int => C): PartialFunction[Int, C] = ???
  //     |@ffi("jvmMethod(Lscala/PartialFunction;, lift, ()Lscala/Function1;)") def lift: Int => Option[Int] = ???
  //     |@ffi("jvmMethod(Lscala/PartialFunction;, applyOrElse, (Ljava/lang/Object;Lscala/Function1;)Ljava/lang/Object;)") def applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: A1 => B1): B1 = ???
  //     |@ffi("jvmMethod(Lscala/PartialFunction;, runWith, (Lscala/Function1;)Lscala/Function1;)") def runWith[U](action: Int => U): Int => Boolean = ???
  //     |@ffi("jvmMethod(Lscala/Function1;, compose, (Lscala/Function1;)Lscala/Function1;)") @unspecialized def compose[A](g: A => Int): A => Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, toIterable, ()Lscala/collection/Iterable;)") override def toIterable: Iterable[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, toIterator, ()Lscala/collection/Iterator;)") @deprecatedOverriding("toIterator should stay consistent with iterator for all Iterables: override iterator instead.", "2.11.0") override def toIterator: Iterator[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, head, ()Ljava/lang/Object;)") override def head: Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, grouped, (I)Lscala/collection/Iterator;)") def grouped(size: Int): Iterator[List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, sliding, (I)Lscala/collection/Iterator;)") def sliding(size: Int): Iterator[List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, sliding, (II)Lscala/collection/Iterator;)") def sliding(size: Int, step: Int): Iterator[List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, copyToArray, (Ljava/lang/Object;II)V)") override def copyToArray[B >: Int](xs: Array[B], start: Int, len: Int): Unit = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, zip, (Lscala/collection/GenIterable;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def zip[A1 >: Int, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[List[Int], (A1, B), That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, zipAll, (Lscala/collection/GenIterable;Ljava/lang/Object;Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def zipAll[B, A1 >: Int, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[List[Int], (A1, B), That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, zipWithIndex, (Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def zipWithIndex[A1 >: Int, That](implicit bf: CanBuildFrom[List[Int], (A1, Int), That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/IterableLike;, canEqual, (Ljava/lang/Object;)Z)") override def canEqual(that: Any): Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, newBuilder, ()Lscala/collection/mutable/Builder;)") protected[this] def newBuilder: Builder[Int, List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, genericBuilder, ()Lscala/collection/mutable/Builder;)") def genericBuilder[B]: Builder[B, List[B]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, unzip, (Lscala/Function1;)Lscala/Tuple2;)") def unzip[A1, A2](implicit asPair: Int => (A1, A2)): (List[A1], List[A2]) = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, unzip3, (Lscala/Function1;)Lscala/Tuple3;)") def unzip3[A1, A2, A3](implicit asTriple: Int => (A1, A2, A3)): (List[A1], List[A2], List[A3]) = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, flatten, (Lscala/Function1;)Lscala/collection/GenTraversable;)") def flatten[B](implicit asTraversable: Int => GenTraversableOnce[B]): List[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/generic/GenericTraversableTemplate;, transpose, (Lscala/Function1;)Lscala/collection/GenTraversable;)") @migration("`transpose` throws an `IllegalArgumentException` if collections are not uniformly sized.", "2.9.0") def transpose[B](implicit asTraversable: Int => GenTraversableOnce[B]): List[List[B]] = ???
  //     |@ffi("jvmErasure(Ljava/lang/Object;)") protected[this] type Self = List[Int]
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, repr, ()Ljava/lang/Object;)") def repr: List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, isTraversableAgain, ()Z)") final def isTraversableAgain: Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, hasDefiniteSize, ()Z)") def hasDefiniteSize: Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, $plus$plus$colon, (Lscala/collection/TraversableOnce;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def ++:[B >: Int, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, $plus$plus$colon, (Lscala/collection/Traversable;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def ++:[B >: Int, That](that: Traversable[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, filter, (Lscala/Function1;)Ljava/lang/Object;)") def filter(p: Int => Boolean): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, filterNot, (Lscala/Function1;)Ljava/lang/Object;)") def filterNot(p: Int => Boolean): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, partition, (Lscala/Function1;)Lscala/Tuple2;)") def partition(p: Int => Boolean): (List[Int], List[Int]) = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, groupBy, (Lscala/Function1;)Lscala/collection/immutable/Map;)") def groupBy[K](f: Int => K): Map[K, List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, scan, (Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def scan[B >: Int, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, scanLeft, (Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def scanLeft[B, That](z: B)(op: (B, Int) => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, scanRight, (Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") @migration("The behavior of `scanRight` has changed. The previous behavior can be reproduced with scanRight.reverse.", "2.9.0") def scanRight[B, That](z: B)(op: (Int, B) => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, headOption, ()Lscala/Option;)") def headOption: Option[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, tail, ()Ljava/lang/Object;)") override def tail: List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, lastOption, ()Lscala/Option;)") def lastOption: Option[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, init, ()Ljava/lang/Object;)") def init: List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, sliceWithKnownDelta, (III)Ljava/lang/Object;)") private[scala] def sliceWithKnownDelta(from: Int, until: Int, delta: Int): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, sliceWithKnownBound, (II)Ljava/lang/Object;)") private[scala] def sliceWithKnownBound(from: Int, until: Int): List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, tails, ()Lscala/collection/Iterator;)") def tails: Iterator[List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, inits, ()Lscala/collection/Iterator;)") def inits: Iterator[List[Int]] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, toTraversable, ()Lscala/collection/Traversable;)") @deprecatedOverriding("Enforce contract of toTraversable that if it is Traversable it returns itself.", "2.11.0") def toTraversable: Traversable[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, to, (Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") override def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, Int, Col[Int]]): Col[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableLike;, withFilter, (Lscala/Function1;)Lscala/collection/generic/FilterMonadic;)") def withFilter(p: Int => Boolean): FilterMonadic[Int, List[Int]] = ???
  //     |@ffi("jvmErasure(Lscala/collection/TraversableLike$WithFilter;)") class WithFilter(p: A => Boolean) extends AnyRef with FilterMonadic[A, Repr] {
  //     |  @ffi("jvmField(Lscala/collection/TraversableLike$WithFilter;, p, Lscala/Function1;)") private[this] val p: Int => Boolean = ???
  //     |  @ffi("jvmMethod(Lscala/collection/TraversableLike$WithFilter;, map, (Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def map[B, That](f: Int => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |  @ffi("jvmMethod(Lscala/collection/TraversableLike$WithFilter;, flatMap, (Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;)") def flatMap[B, That](f: Int => GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
  //     |  @ffi("jvmMethod(Lscala/collection/TraversableLike$WithFilter;, foreach, (Lscala/Function1;)V)") def foreach[U](f: Int => U): Unit = ???
  //     |  @ffi("jvmMethod(Lscala/collection/TraversableLike$WithFilter;, withFilter, (Lscala/Function1;)Lscala/collection/TraversableLike$WithFilter;)") def withFilter(q: Int => Boolean): List[Int]#WithFilter = ???
  //     |}
  //     |@ffi("jvmMethod(Lscala/collection/Parallelizable;, par, ()Lscala/collection/Parallel;)") def par: ParSeq[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, reversed, ()Lscala/collection/immutable/List;)") protected[this] def reversed: List[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, nonEmpty, ()Z)") def nonEmpty: Boolean = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, count, (Lscala/Function1;)I)") def count(p: Int => Boolean): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, collectFirst, (Lscala/PartialFunction;)Lscala/Option;)") def collectFirst[B](pf: PartialFunction[Int, B]): Option[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, $div$colon, (Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;)") def /:[B](z: B)(op: (B, Int) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, $colon$bslash, (Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;)") def :\[B](z: B)(op: (Int, B) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, reduceLeftOption, (Lscala/Function2;)Lscala/Option;)") def reduceLeftOption[B >: Int](op: (B, Int) => B): Option[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, reduceRightOption, (Lscala/Function2;)Lscala/Option;)") def reduceRightOption[B >: Int](op: (Int, B) => B): Option[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, reduce, (Lscala/Function2;)Ljava/lang/Object;)") def reduce[A1 >: Int](op: (A1, A1) => A1): A1 = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, reduceOption, (Lscala/Function2;)Lscala/Option;)") def reduceOption[A1 >: Int](op: (A1, A1) => A1): Option[A1] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, fold, (Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;)") def fold[A1 >: Int](z: A1)(op: (A1, A1) => A1): A1 = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, aggregate, (Lscala/Function0;Lscala/Function2;Lscala/Function2;)Ljava/lang/Object;)") def aggregate[B](z: => B)(seqop: (B, Int) => B, combop: (B, B) => B): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, sum, (Lscala/math/Numeric;)Ljava/lang/Object;)") def sum[B >: Int](implicit num: Numeric[B]): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, product, (Lscala/math/Numeric;)Ljava/lang/Object;)") def product[B >: Int](implicit num: Numeric[B]): B = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, min, (Lscala/math/Ordering;)Ljava/lang/Object;)") def min[B >: Int](implicit cmp: Ordering[B]): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, max, (Lscala/math/Ordering;)Ljava/lang/Object;)") def max[B >: Int](implicit cmp: Ordering[B]): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, maxBy, (Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;)") def maxBy[B](f: Int => B)(implicit cmp: Ordering[B]): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, minBy, (Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;)") def minBy[B](f: Int => B)(implicit cmp: Ordering[B]): Int = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, copyToBuffer, (Lscala/collection/mutable/Buffer;)V)") def copyToBuffer[B >: Int](dest: Buffer[B]): Unit = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, copyToArray, (Ljava/lang/Object;I)V)") def copyToArray[B >: Int](xs: Array[B], start: Int): Unit = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, copyToArray, (Ljava/lang/Object;)V)") def copyToArray[B >: Int](xs: Array[B]): Unit = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toArray, (Lscala/reflect/ClassTag;)Ljava/lang/Object;)") def toArray[B >: Int: ClassTag](implicit evidence$1: ClassTag[B]): Array[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toIndexedSeq, ()Lscala/collection/immutable/IndexedSeq;)") def toIndexedSeq: IndexedSeq[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toBuffer, ()Lscala/collection/mutable/Buffer;)") def toBuffer[B >: Int]: Buffer[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toSet, ()Lscala/collection/immutable/Set;)") def toSet[B >: Int]: Set[B] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toVector, ()Lscala/collection/immutable/Vector;)") def toVector: Vector[Int] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, toMap, (Lscala/Predef$$less$colon$less;)Lscala/collection/immutable/Map;)") def toMap[T, U](implicit ev: Int <:< (T, U)): Map[T, U] = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, mkString, (Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;)") def mkString(start: String, sep: String, end: String): String = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, mkString, (Ljava/lang/String;)Ljava/lang/String;)") def mkString(sep: String): String = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, mkString, ()Ljava/lang/String;)") def mkString: String = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, addString, (Lscala/collection/mutable/StringBuilder;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Lscala/collection/mutable/StringBuilder;)") def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, addString, (Lscala/collection/mutable/StringBuilder;Ljava/lang/String;)Lscala/collection/mutable/StringBuilder;)") def addString(b: StringBuilder, sep: String): StringBuilder = ???
  //     |@ffi("jvmMethod(Lscala/collection/TraversableOnce;, addString, (Lscala/collection/mutable/StringBuilder;)Lscala/collection/mutable/StringBuilder;)") def addString(b: StringBuilder): StringBuilder = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, getClass, ()Ljava/lang/Class;)") final def getClass(): Class[?0] forSome { @ffi("jvmErasure(Ljava/lang/Object;)") type ?0 } = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, clone, ()Ljava/lang/Object;)") @throws[CloneNotSupportedException] protected[lang] def clone(): Object = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, notify, ()V)") final def notify(): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, notifyAll, ()V)") final def notifyAll(): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, wait, (J)V)") @throws[InterruptedException] final def wait(x$1: Long): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, wait, (JI)V)") @throws[InterruptedException] final def wait(x$1: Long, x$2: Int): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, wait, ()V)") @throws[InterruptedException] final def wait(): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, finalize, ()V)") @throws[Throwable] protected[lang] def finalize(): Unit = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, eq, (Ljava/lang/Object;)Z)") final def eq(x$1: AnyRef): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, ne, (Ljava/lang/Object;)Z)") final def ne(x$1: AnyRef): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, $eq$eq, (Ljava/lang/Object;)Z)") final def ==(x$1: Any): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, $bang$eq, (Ljava/lang/Object;)Z)") final def !=(x$1: Any): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, $hash$hash, ()I)") final def ##(): Int = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, synchronized, (Ljava/lang/Object;)Ljava/lang/Object;)") final def synchronized[T0](x$1: T0): T0 = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, isInstanceOf, ()Z)") final def isInstanceOf[T0]: Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, asInstanceOf, ()Ljava/lang/Object;)") final def asInstanceOf[T0]: T0 = ???
  //   """.trim.stripMargin)
  // }

  // FIXME: uncomment this once https://github.com/scalameta/scalameta/issues/171 is fixed
  // test("t\"scala.compat.Platform.type\".members") {
  //   assert(t"scala.compat.Platform.type".members.map(_.show[Code]).mkString(EOL) === """
  //     |@ffi("jvmMethod(Lscala/compat/Platform$;, $lessinit$greater, ()Lscala/compat/Platform$;)") ()
  //     |@ffi("jvmErasure(Ljava/lang/StackOverflowError;)") type StackOverflowError = StackOverflowError
  //     |@ffi("jvmErasure(Ljava/util/ConcurrentModificationException;)") type ConcurrentModificationException = ConcurrentModificationException
  //     |@ffi("jvmMethod(Lscala/compat/Platform$;, arraycopy, (Ljava/lang/Object;ILjava/lang/Object;II)V)") @inline def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit = ???
  //     |@ffi("jvmMethod(Lscala/compat/Platform$;, createArray, (Ljava/lang/Class;I)Ljava/lang/Object;)") @inline def createArray(elemClass: Class[_$1] forSome { @ffi("jvmErasure(Ljava/lang/Object;)") type _$1 }, length: Int): AnyRef = ???
  //     |@ffi("jvmMethod(Lscala/compat/Platform$;, arrayclear, ([I)V)") @inline def arrayclear(arr: Array[Int]): Unit = ???
  //     |@ffi("jvmMethod(Lscala/compat/Platform$;, getClassForName, (Ljava/lang/String;)Ljava/lang/Class;)") @inline def getClassForName(name: String): Class[_$2] forSome { @ffi("jvmErasure(Ljava/lang/Object;)") type _$2 } = ???
  //     |EOL
  //     |@ffi("jvmMethod(Lscala/compat/Platform$;, currentTime, ()J)") @inline def currentTime: Long = ???
  //     |@ffi("jvmMethod(Lscala/compat/Platform$;, collectGarbage, ()V)") @inline def collectGarbage(): Unit = ???
  //     |@ffi("jvmMethod(Lscala/compat/Platform$;, defaultCharsetName, ()Ljava/lang/String;)") @inline def defaultCharsetName: String = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, getClass, ()Ljava/lang/Class;)") final def getClass(): Class[?0] forSome { @ffi("jvmErasure(Ljava/lang/Object;)") type ?0 } = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, hashCode, ()I)") def hashCode(): Int = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, equals, (Ljava/lang/Object;)Z)") def equals(x$1: Any): Boolean = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, clone, ()Ljava/lang/Object;)") @throws[CloneNotSupportedException] protected[lang] def clone(): Object = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, toString, ()Ljava/lang/String;)") def toString(): String = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, notify, ()V)") final def notify(): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, notifyAll, ()V)") final def notifyAll(): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, wait, (J)V)") @throws[InterruptedException] final def wait(x$1: Long): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, wait, (JI)V)") @throws[InterruptedException] final def wait(x$1: Long, x$2: Int): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, wait, ()V)") @throws[InterruptedException] final def wait(): Unit = ???
  //     |@ffi("jvmMethod(Ljava/lang/Object;, finalize, ()V)") @throws[Throwable] protected[lang] def finalize(): Unit = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, eq, (Ljava/lang/Object;)Z)") final def eq(x$1: AnyRef): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, ne, (Ljava/lang/Object;)Z)") final def ne(x$1: AnyRef): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, $eq$eq, (Ljava/lang/Object;)Z)") final def ==(x$1: Any): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, $bang$eq, (Ljava/lang/Object;)Z)") final def !=(x$1: Any): Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, $hash$hash, ()I)") final def ##(): Int = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, synchronized, (Ljava/lang/Object;)Ljava/lang/Object;)") final def synchronized[T0](x$1: T0): T0 = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, isInstanceOf, ()Z)") final def isInstanceOf[T0]: Boolean = ???
  //     |@ffi("scalaIntrinsic(Ljava/lang/Object;, asInstanceOf, ()Ljava/lang/Object;)") final def asInstanceOf[T0]: T0 = ???
  //   """.trim.stripMargin)
  // }

  test("t\"scala.type\".members") {
    intercept[SemanticException] {
      val expectedFail = "Input scala.meta tree is not fully attributed and can't be converted to a scala.reflect artifact."
      try t"scala.type".members
      catch { case ex: SemanticException => assert(ex.message.trim.startsWith(expectedFail)); throw ex }
    }
  }

  // TODO: reenable when https://github.com/scalameta/scalameta/issues/171 is fixed
  // test("t\"scala.collection.immutable.List\".defn") {
  //   assert(t"scala.collection.immutable.List".defn.toString === """
  //     |@SerialVersionUID(value = -6084104484083858598L) sealed abstract class List[+A] extends AbstractSeq[A] with LinearSeq[A] with Product with GenericTraversableTemplate[A, List] with LinearSeqOptimized[A, List[A]] with Serializable { ... }
  //   """.trim.stripMargin)
  // }

  // FIXME: uncomment this once https://github.com/scalameta/scalameta/issues/166 is fixed
  // test("t\"SemanticDummy\".defn") {
  //   assert(t"SemanticDummy".defn.show[Code] === """
  //     |@ffi("jvmErasure(LSemanticDummy;)") class SemanticDummy extends AnyRef {
  //     |  @ffi("jvmField(LSemanticDummy;, x, I)") private[this] val x: Int = ???
  //     |  @ffi("jvmMethod(LSemanticDummy;, y, ()I)") val y: Int = ???
  //     |  @ffi("jvmMethod(LSemanticDummy;, foo, (I)I)") def foo(@ffi("jvmMethod(LSemanticDummy;, foo$default$1, ()I)") w: Int = ???): Int = ???
  //     |}
  //   """.trim.stripMargin)
  // }

  // FIXME: uncomment this once https://github.com/scalameta/scalameta/issues/166 is fixed
  // test("t\"Int\".defn") {
  //   assert(t"Int".defn.show[Code] === """
  //     |@ffi("jvmErasure(I)") final abstract class Int extends AnyVal {
  //     |  @ffi("scalaIntrinsic(I, toByte, ()B)") def toByte: Byte = ???
  //     |  @ffi("scalaIntrinsic(I, toShort, ()S)") def toShort: Short = ???
  //     |  @ffi("scalaIntrinsic(I, toChar, ()C)") def toChar: Char = ???
  //     |  @ffi("scalaIntrinsic(I, toInt, ()I)") def toInt: Int = ???
  //     |  @ffi("scalaIntrinsic(I, toLong, ()J)") def toLong: Long = ???
  //     |  @ffi("scalaIntrinsic(I, toFloat, ()F)") def toFloat: Float = ???
  //     |  @ffi("scalaIntrinsic(I, toDouble, ()D)") def toDouble: Double = ???
  //     |  @ffi("scalaIntrinsic(I, unary_$tilde, ()I)") def unary_~ : Int = ???
  //     |  @ffi("scalaIntrinsic(I, unary_$plus, ()I)") def unary_+ : Int = ???
  //     |  @ffi("scalaIntrinsic(I, unary_$minus, ()I)") def unary_- : Int = ???
  //     |  @ffi("scalaIntrinsic(I, $plus, (Ljava/lang/String;)Ljava/lang/String;)") def +(x: String): String = ???
  //     |  @ffi("scalaIntrinsic(I, $less$less, (I)I)") def <<(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $less$less, (J)I)") def <<(x: Long): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$greater$greater, (I)I)") def >>>(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$greater$greater, (J)I)") def >>>(x: Long): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$greater, (I)I)") def >>(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$greater, (J)I)") def >>(x: Long): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $eq$eq, (B)Z)") def ==(x: Byte): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $eq$eq, (S)Z)") def ==(x: Short): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $eq$eq, (C)Z)") def ==(x: Char): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $eq$eq, (I)Z)") def ==(x: Int): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $eq$eq, (J)Z)") def ==(x: Long): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $eq$eq, (F)Z)") def ==(x: Float): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $eq$eq, (D)Z)") def ==(x: Double): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $bang$eq, (B)Z)") def !=(x: Byte): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $bang$eq, (S)Z)") def !=(x: Short): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $bang$eq, (C)Z)") def !=(x: Char): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $bang$eq, (I)Z)") def !=(x: Int): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $bang$eq, (J)Z)") def !=(x: Long): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $bang$eq, (F)Z)") def !=(x: Float): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $bang$eq, (D)Z)") def !=(x: Double): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less, (B)Z)") def <(x: Byte): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less, (S)Z)") def <(x: Short): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less, (C)Z)") def <(x: Char): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less, (I)Z)") def <(x: Int): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less, (J)Z)") def <(x: Long): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less, (F)Z)") def <(x: Float): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less, (D)Z)") def <(x: Double): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less$eq, (B)Z)") def <=(x: Byte): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less$eq, (S)Z)") def <=(x: Short): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less$eq, (C)Z)") def <=(x: Char): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less$eq, (I)Z)") def <=(x: Int): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less$eq, (J)Z)") def <=(x: Long): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less$eq, (F)Z)") def <=(x: Float): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $less$eq, (D)Z)") def <=(x: Double): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater, (B)Z)") def >(x: Byte): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater, (S)Z)") def >(x: Short): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater, (C)Z)") def >(x: Char): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater, (I)Z)") def >(x: Int): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater, (J)Z)") def >(x: Long): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater, (F)Z)") def >(x: Float): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater, (D)Z)") def >(x: Double): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$eq, (B)Z)") def >=(x: Byte): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$eq, (S)Z)") def >=(x: Short): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$eq, (C)Z)") def >=(x: Char): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$eq, (I)Z)") def >=(x: Int): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$eq, (J)Z)") def >=(x: Long): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$eq, (F)Z)") def >=(x: Float): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $greater$eq, (D)Z)") def >=(x: Double): Boolean = ???
  //     |  @ffi("scalaIntrinsic(I, $bar, (B)I)") def |(x: Byte): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $bar, (S)I)") def |(x: Short): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $bar, (C)I)") def |(x: Char): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $bar, (I)I)") def |(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $bar, (J)J)") def |(x: Long): Long = ???
  //     |  @ffi("scalaIntrinsic(I, $amp, (B)I)") def &(x: Byte): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $amp, (S)I)") def &(x: Short): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $amp, (C)I)") def &(x: Char): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $amp, (I)I)") def &(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $amp, (J)J)") def &(x: Long): Long = ???
  //     |  @ffi("scalaIntrinsic(I, $up, (B)I)") def ^(x: Byte): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $up, (S)I)") def ^(x: Short): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $up, (C)I)") def ^(x: Char): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $up, (I)I)") def ^(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $up, (J)J)") def ^(x: Long): Long = ???
  //     |  @ffi("scalaIntrinsic(I, $plus, (B)I)") def +(x: Byte): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $plus, (S)I)") def +(x: Short): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $plus, (C)I)") def +(x: Char): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $plus, (I)I)") def +(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $plus, (J)J)") def +(x: Long): Long = ???
  //     |  @ffi("scalaIntrinsic(I, $plus, (F)F)") def +(x: Float): Float = ???
  //     |  @ffi("scalaIntrinsic(I, $plus, (D)D)") def +(x: Double): Double = ???
  //     |  @ffi("scalaIntrinsic(I, $minus, (B)I)") def -(x: Byte): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $minus, (S)I)") def -(x: Short): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $minus, (C)I)") def -(x: Char): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $minus, (I)I)") def -(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $minus, (J)J)") def -(x: Long): Long = ???
  //     |  @ffi("scalaIntrinsic(I, $minus, (F)F)") def -(x: Float): Float = ???
  //     |  @ffi("scalaIntrinsic(I, $minus, (D)D)") def -(x: Double): Double = ???
  //     |  @ffi("scalaIntrinsic(I, $times, (B)I)") def *(x: Byte): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $times, (S)I)") def *(x: Short): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $times, (C)I)") def *(x: Char): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $times, (I)I)") def *(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $times, (J)J)") def *(x: Long): Long = ???
  //     |  @ffi("scalaIntrinsic(I, $times, (F)F)") def *(x: Float): Float = ???
  //     |  @ffi("scalaIntrinsic(I, $times, (D)D)") def *(x: Double): Double = ???
  //     |  @ffi("scalaIntrinsic(I, $div, (B)I)") def /(x: Byte): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $div, (S)I)") def /(x: Short): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $div, (C)I)") def /(x: Char): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $div, (I)I)") def /(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $div, (J)J)") def /(x: Long): Long = ???
  //     |  @ffi("scalaIntrinsic(I, $div, (F)F)") def /(x: Float): Float = ???
  //     |  @ffi("scalaIntrinsic(I, $div, (D)D)") def /(x: Double): Double = ???
  //     |  @ffi("scalaIntrinsic(I, $percent, (B)I)") def %(x: Byte): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $percent, (S)I)") def %(x: Short): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $percent, (C)I)") def %(x: Char): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $percent, (I)I)") def %(x: Int): Int = ???
  //     |  @ffi("scalaIntrinsic(I, $percent, (J)J)") def %(x: Long): Long = ???
  //     |  @ffi("scalaIntrinsic(I, $percent, (F)F)") def %(x: Float): Float = ???
  //     |  @ffi("scalaIntrinsic(I, $percent, (D)D)") def %(x: Double): Double = ???
  //     |  @ffi("scalaIntrinsic(I, getClass, ()Ljava/lang/Class;)") override def getClass(): Class[Int] = ???
  //     |}
  //   """.trim.stripMargin)
  // }

  // TODO: reenable when https://github.com/scalameta/scalameta/issues/171 is fixed
  // test("q\"scala\".defn.members") {
  //   assert(q"scala".defn.members.sortBy(mem => mem.name.toString + mem.internalTag).mkString(EOL) === """
  //     |#::
  //     |+:
  //     |:+
  //     |::
  //     |type ::[A] = ::[A]
  //     |class <byname>[+T0] extends AnyRef with Any { ... }
  //     |class <repeated...>[+T0] extends Array[T0] { ... }
  //     |class <repeated>[+T0] extends Seq[T0] { ... }
  //     |type AbstractMethodError = AbstractMethodError
  //     |abstract class Any { ... }
  //     |AnyRef
  //     |type AnyRef = Object
  //     |abstract class AnyVal extends Any { ... }
  //     |private[scala] trait AnyValCompanion extends Specializable { ... }
  //     |trait App extends DelayedInit { ... }
  //     |final class Array[T](_length: Int) extends AnyRef with Serializable with Cloneable { ... }
  //     |object Array extends FallbackArrayBuilding with Serializable { ... }
  //     |type ArrayIndexOutOfBoundsException = ArrayIndexOutOfBoundsException
  //     |BigDecimal
  //     |type BigDecimal = BigDecimal
  //     |BigInt
  //     |type BigInt = BigInt
  //     |final abstract class Boolean extends AnyVal { ... }
  //     |object Boolean extends AnyValCompanion { ... }
  //     |type BufferedIterator[+A] = BufferedIterator[A]
  //     |final abstract class Byte extends AnyVal { ... }
  //     |object Byte extends AnyValCompanion { ... }
  //     |final abstract class Char extends AnyVal { ... }
  //     |object Char extends AnyValCompanion { ... }
  //     |type ClassCastException = ClassCastException
  //     |trait Cloneable extends Cloneable { ... }
  //     |object Console extends DeprecatedConsole with AnsiColor { ... }
  //     |@deprecated(QQQDelayedInit semantics can be surprising. Support for `App` will continue.
  //     |See the release notes for more details: https://github.com/scala/scala/releases/tag/v2.11.0-RC1QQQ, "2.11.0") @deprecated("see corresponding Javadoc for more information.", "") trait DelayedInit extends AnyRef { ... }
  //     |private[scala] abstract class DeprecatedConsole extends AnyRef { _: Console.type => ... }
  //     |private[scala] trait DeprecatedPredef extends AnyRef { _: Predef.type => ... }
  //     |final abstract class Double extends AnyVal { ... }
  //     |object Double extends AnyValCompanion { ... }
  //     |trait Dynamic extends Any { ... }
  //     |Either
  //     |type Either[+A, +B] = Either[A, B]
  //     |@SerialVersionUID(value = 8476000850333817230L) abstract class Enumeration(initial: Int) extends AnyRef with Serializable { ... }
  //     |trait Equals extends Any { ... }
  //     |Equiv
  //     |type Equiv[T] = Equiv[T]
  //     |type Error = Error
  //     |type Exception = Exception
  //     |class FallbackArrayBuilding extends AnyRef { ... }
  //     |final abstract class Float extends AnyVal { ... }
  //     |object Float extends AnyValCompanion { ... }
  //     |Fractional
  //     |type Fractional[T] = Fractional[T]
  //     |trait Function0[@specialized(Specializable.Primitives) +R] extends AnyRef { _: () => R => ... }
  //     |trait Function10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R => ... }
  //     |trait Function11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R => ... }
  //     |trait Function12[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R => ... }
  //     |@implicitNotFound("No implicit view available from ${T1} => ${R}.") trait Function1[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) -T1, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends AnyRef { _: T1 => R => ... }
  //     |trait Function13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R => ... }
  //     |trait Function14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R => ... }
  //     |trait Function15[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R => ... }
  //     |trait Function16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R => ... }
  //     |trait Function17[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R => ... }
  //     |trait Function18[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R => ... }
  //     |trait Function19[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R => ... }
  //     |trait Function20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R => ... }
  //     |trait Function21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R => ... }
  //     |trait Function22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R => ... }
  //     |trait Function2[@specialized(scala.Int, scala.Long, scala.Double) -T1, @specialized(scala.Int, scala.Long, scala.Double) -T2, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends AnyRef { _: (T1, T2) => R => ... }
  //     |object Function extends AnyRef { ... }
  //     |trait Function3[-T1, -T2, -T3, +R] extends AnyRef { _: (T1, T2, T3) => R => ... }
  //     |trait Function4[-T1, -T2, -T3, -T4, +R] extends AnyRef { _: (T1, T2, T3, T4) => R => ... }
  //     |trait Function5[-T1, -T2, -T3, -T4, -T5, +R] extends AnyRef { _: (T1, T2, T3, T4, T5) => R => ... }
  //     |trait Function6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6) => R => ... }
  //     |trait Function7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7) => R => ... }
  //     |trait Function8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8) => R => ... }
  //     |trait Function9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R => ... }
  //     |type IllegalArgumentException = IllegalArgumentException
  //     |trait Immutable extends AnyRef { ... }
  //     |type IndexOutOfBoundsException = IndexOutOfBoundsException
  //     |IndexedSeq
  //     |type IndexedSeq[+A] = IndexedSeq[A]
  //     |final abstract class Int extends AnyVal { ... }
  //     |object Int extends AnyValCompanion { ... }
  //     |Integral
  //     |type Integral[T] = Integral[T]
  //     |type InterruptedException = InterruptedException
  //     |Iterable
  //     |type Iterable[+A] = Iterable[A]
  //     |Iterator
  //     |type Iterator[+A] = Iterator[A]
  //     |Left
  //     |type Left[+A, +B] = Left[A, B]
  //     |List
  //     |type List[+A] = List[A]
  //     |final abstract class Long extends AnyVal { ... }
  //     |object Long extends AnyValCompanion { ... }
  //     |private[scala] abstract class LowPriorityImplicits extends AnyRef { ... }
  //     |final class MatchError(obj: Any) extends RuntimeException { ... }
  //     |trait Mutable extends AnyRef { ... }
  //     |Nil
  //     |type NoSuchElementException = NoSuchElementException
  //     |@SerialVersionUID(value = 5066590221178148012L) case object None extends Option[Nothing] with Product with Serializable { ... }
  //     |final class NotImplementedError(msg: String) extends Error { ... }
  //     |@deprecated("This trait will be removed", "2.11.0") @deprecated("see corresponding Javadoc for more information.", "") trait NotNull extends Any { ... }
  //     |final abstract class Nothing extends Any { ... }
  //     |final abstract class Null extends AnyRef { ... }
  //     |type NullPointerException = NullPointerException
  //     |type NumberFormatException = NumberFormatException
  //     |Numeric
  //     |type Numeric[T] = Numeric[T]
  //     |@SerialVersionUID(value = -114498752079829388L) sealed abstract class Option[+A] extends AnyRef with Product with Serializable { _: Option[A] => ... }
  //     |object Option extends AnyRef with Serializable { ... }
  //     |Ordered
  //     |type Ordered[T] = Ordered[T]
  //     |Ordering
  //     |type Ordering[T] = Ordering[T]
  //     |trait PartialFunction[-A, +B] extends (A => B) { _: PartialFunction[A, B] => ... }
  //     |object PartialFunction extends AnyRef { ... }
  //     |type PartialOrdering[T] = PartialOrdering[T]
  //     |type PartiallyOrdered[T] = PartiallyOrdered[T]
  //     |object Predef extends LowPriorityImplicits with DeprecatedPredef { ... }
  //     |trait Product10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] extends Product { ... }
  //     |object Product10 extends AnyRef { ... }
  //     |trait Product11[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11] extends Product { ... }
  //     |object Product11 extends AnyRef { ... }
  //     |trait Product12[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12] extends Product { ... }
  //     |object Product12 extends AnyRef { ... }
  //     |trait Product1[@specialized(Int, Long, Double) +T1] extends Product { ... }
  //     |object Product1 extends AnyRef { ... }
  //     |trait Product13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13] extends Product { ... }
  //     |object Product13 extends AnyRef { ... }
  //     |trait Product14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14] extends Product { ... }
  //     |object Product14 extends AnyRef { ... }
  //     |trait Product15[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15] extends Product { ... }
  //     |object Product15 extends AnyRef { ... }
  //     |trait Product16[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16] extends Product { ... }
  //     |object Product16 extends AnyRef { ... }
  //     |trait Product17[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17] extends Product { ... }
  //     |object Product17 extends AnyRef { ... }
  //     |trait Product18[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18] extends Product { ... }
  //     |object Product18 extends AnyRef { ... }
  //     |trait Product19[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19] extends Product { ... }
  //     |object Product19 extends AnyRef { ... }
  //     |trait Product20[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20] extends Product { ... }
  //     |object Product20 extends AnyRef { ... }
  //     |trait Product21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21] extends Product { ... }
  //     |object Product21 extends AnyRef { ... }
  //     |trait Product22[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21, +T22] extends Product { ... }
  //     |object Product22 extends AnyRef { ... }
  //     |trait Product2[@specialized(Int, Long, Double) +T1, @specialized(Int, Long, Double) +T2] extends Product { ... }
  //     |object Product2 extends AnyRef { ... }
  //     |trait Product extends Equals { ... }
  //     |trait Product3[+T1, +T2, +T3] extends Product { ... }
  //     |object Product3 extends AnyRef { ... }
  //     |trait Product4[+T1, +T2, +T3, +T4] extends Product { ... }
  //     |object Product4 extends AnyRef { ... }
  //     |trait Product5[+T1, +T2, +T3, +T4, +T5] extends Product { ... }
  //     |object Product5 extends AnyRef { ... }
  //     |trait Product6[+T1, +T2, +T3, +T4, +T5, +T6] extends Product { ... }
  //     |object Product6 extends AnyRef { ... }
  //     |trait Product7[+T1, +T2, +T3, +T4, +T5, +T6, +T7] extends Product { ... }
  //     |object Product7 extends AnyRef { ... }
  //     |trait Product8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8] extends Product { ... }
  //     |object Product8 extends AnyRef { ... }
  //     |trait Product9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9] extends Product { ... }
  //     |object Product9 extends AnyRef { ... }
  //     |trait Proxy extends Any { ... }
  //     |object Proxy extends AnyRef { ... }
  //     |Range
  //     |type Range = Range
  //     |@deprecated("This class will be removed", "2.11.0") @deprecated("see corresponding Javadoc for more information.", "") abstract class Responder[+A] extends AnyRef with Serializable { ... }
  //     |@deprecated("This object will be removed", "2.11.0") object Responder extends AnyRef with Serializable { ... }
  //     |Right
  //     |type Right[+A, +B] = Right[A, B]
  //     |type RuntimeException = RuntimeException
  //     |case class ScalaReflectionException(msg: String) extends Exception with Product with Serializable { ... }
  //     |object ScalaReflectionException extends AbstractFunction1[String, ScalaReflectionException] with Serializable { ... }
  //     |Seq
  //     |type Seq[+A] = Seq[A]
  //     |class SerialVersionUID(value: Long) extends ClassfileAnnotation { ... }
  //     |trait Serializable extends Serializable { ... }
  //     |final abstract class Short extends AnyVal { ... }
  //     |object Short extends AnyValCompanion { ... }
  //     |final trait Singleton extends Any { ... }
  //     |@SerialVersionUID(value = 1234815782226070388L) final case class Some[+A](x: A) extends Option[A] with Product with Serializable { ... }
  //     |object Some extends AnyRef with Serializable { ... }
  //     |trait Specializable extends AnyRef { ... }
  //     |object Specializable extends AnyRef { ... }
  //     |Stream
  //     |type Stream[+A] = Stream[A]
  //     |StringBuilder
  //     |type StringBuilder = StringBuilder
  //     |case class StringContext(parts: String*) extends AnyRef with Product with Serializable { ... }
  //     |object StringContext extends AnyRef with Serializable { ... }
  //     |type StringIndexOutOfBoundsException = StringIndexOutOfBoundsException
  //     |final class Symbol private (val name: String) extends AnyRef with Serializable { ... }
  //     |object Symbol extends UniquenessCache[String, Symbol] with Serializable { ... }
  //     |type Throwable = Throwable
  //     |Traversable
  //     |type Traversable[+A] = Traversable[A]
  //     |type TraversableOnce[+A] = TraversableOnce[A]
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10) extends AnyRef with Product10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] with Product with Serializable { ... }
  //     |object Tuple10 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple11[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11) extends AnyRef with Product11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] with Product with Serializable { ... }
  //     |object Tuple11 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple12[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12) extends AnyRef with Product12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] with Product with Serializable { ... }
  //     |object Tuple12 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple1[@specialized(Int, Long, Double) +T1](_1: T1) extends AnyRef with Product1[T1] with Product with Serializable { ... }
  //     |object Tuple1 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13) extends AnyRef with Product13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] with Product with Serializable { ... }
  //     |object Tuple13 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14) extends AnyRef with Product14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] with Product with Serializable { ... }
  //     |object Tuple14 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple15[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15) extends AnyRef with Product15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] with Product with Serializable { ... }
  //     |object Tuple15 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple16[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16) extends AnyRef with Product16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] with Product with Serializable { ... }
  //     |object Tuple16 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple17[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17) extends AnyRef with Product17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] with Product with Serializable { ... }
  //     |object Tuple17 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple18[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18) extends AnyRef with Product18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] with Product with Serializable { ... }
  //     |object Tuple18 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple19[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19) extends AnyRef with Product19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] with Product with Serializable { ... }
  //     |object Tuple19 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple20[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20) extends AnyRef with Product20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] with Product with Serializable { ... }
  //     |object Tuple20 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20, _21: T21) extends AnyRef with Product21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] with Product with Serializable { ... }
  //     |object Tuple21 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple22[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21, +T22](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20, _21: T21, _22: T22) extends AnyRef with Product22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] with Product with Serializable { ... }
  //     |object Tuple22 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple2[@specialized(Int, Long, Double, Char, Boolean) +T1, @specialized(Int, Long, Double, Char, Boolean) +T2](_1: T1, _2: T2) extends AnyRef with Product2[T1, T2] with Product with Serializable { ... }
  //     |object Tuple2 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple3[+T1, +T2, +T3](_1: T1, _2: T2, _3: T3) extends AnyRef with Product3[T1, T2, T3] with Product with Serializable { ... }
  //     |object Tuple3 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple4[+T1, +T2, +T3, +T4](_1: T1, _2: T2, _3: T3, _4: T4) extends AnyRef with Product4[T1, T2, T3, T4] with Product with Serializable { ... }
  //     |object Tuple4 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple5[+T1, +T2, +T3, +T4, +T5](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5) extends AnyRef with Product5[T1, T2, T3, T4, T5] with Product with Serializable { ... }
  //     |object Tuple5 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple6[+T1, +T2, +T3, +T4, +T5, +T6](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6) extends AnyRef with Product6[T1, T2, T3, T4, T5, T6] with Product with Serializable { ... }
  //     |object Tuple6 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple7[+T1, +T2, +T3, +T4, +T5, +T6, +T7](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7) extends AnyRef with Product7[T1, T2, T3, T4, T5, T6, T7] with Product with Serializable { ... }
  //     |object Tuple7 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8) extends AnyRef with Product8[T1, T2, T3, T4, T5, T6, T7, T8] with Product with Serializable { ... }
  //     |object Tuple8 extends AnyRef with Serializable { ... }
  //     |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9) extends AnyRef with Product9[T1, T2, T3, T4, T5, T6, T7, T8, T9] with Product with Serializable { ... }
  //     |object Tuple9 extends AnyRef with Serializable { ... }
  //     |final class UninitializedError extends RuntimeException { ... }
  //     |final case class UninitializedFieldError(msg: String) extends RuntimeException with Product with Serializable { ... }
  //     |object UninitializedFieldError extends AbstractFunction1[String, UninitializedFieldError] with Serializable { ... }
  //     |private[scala] abstract class UniquenessCache[K, V >: Null] extends AnyRef { ... }
  //     |final abstract class Unit extends AnyVal { ... }
  //     |object Unit extends AnyValCompanion { ... }
  //     |type UnsupportedOperationException = UnsupportedOperationException
  //     |Vector
  //     |type Vector[+A] = Vector[A]
  //     |package annotation { ... }
  //     |package beans { ... }
  //     |package collection { ... }
  //     |package compat { ... }
  //     |package concurrent { ... }
  //     |@getter @setter @beanGetter @beanSetter class deprecated(message: String = ???, since: String = ???) extends StaticAnnotation { ... }
  //     |object deprecated extends AnyRef { ... }
  //     |private[scala] class deprecatedInheritance(message: String = ???, since: String = ???) extends StaticAnnotation { ... }
  //     |private[scala] object deprecatedInheritance extends AnyRef { ... }
  //     |@param class deprecatedName(name: Symbol) extends StaticAnnotation { ... }
  //     |private[scala] class deprecatedOverriding(message: String = ???, since: String = ???) extends StaticAnnotation { ... }
  //     |private[scala] object deprecatedOverriding extends AnyRef { ... }
  //     |class inline extends StaticAnnotation { ... }
  //     |package io { ... }
  //     |object language extends AnyRef { ... }
  //     |object languageFeature extends AnyRef { ... }
  //     |package math { ... }
  //     |package meta { ... }
  //     |class native extends StaticAnnotation { ... }
  //     |class noinline extends StaticAnnotation { ... }
  //     |package org { ... }
  //     |package ref { ... }
  //     |package reflect { ... }
  //     |class remote extends StaticAnnotation { ... }
  //     |package runtime { ... }
  //     |package object scala extends AnyRef { ... }
  //     |class specialized(group: SpecializedGroup) extends StaticAnnotation { ... }
  //     |package sys { ... }
  //     |package text { ... }
  //     |class throws[T <: Throwable](cause: String = ???) extends StaticAnnotation { ... }
  //     |object throws extends AnyRef { ... }
  //     |package tools { ... }
  //     |@field class transient extends StaticAnnotation { ... }
  //     |class unchecked extends Annotation { ... }
  //     |package util { ... }
  //     |@field class volatile extends StaticAnnotation { ... }
  //     |package xml { ... }
  //   """.trim.stripMargin.replace("QQQ", "\"\"\""))
  // }
}
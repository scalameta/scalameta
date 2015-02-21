import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.{Exception => SemanticException}
import scala.meta.internal.{ast => impl}

class SemanticDummy {
  private[this] val x = 2
  val y = 3
  def foo(w: Int = 4) = w
}

class SemanticSuite extends FunSuite {
  import scala.meta.internal.hosts.scalac.Scalahost
  private val classpathOptions = s"-cp ${sys.props("sbt.paths.tests.classpath")}"
  private val pluginOptions = s"-Xplugin:${sys.props("sbt.paths.plugin.jar")} -Xplugin-require:scalahost"
  private val options = classpathOptions + " " + pluginOptions
  implicit val c = Scalahost.mkStandaloneContext(options)

  test("subtyping") {
    assert(t"List[Int]" <:< t"List[Any]")
  }

  test("q\"List(1, 2, 3)\".tpe") {
    intercept[SemanticException] {
      val expectedFail = "implementation restriction: internal cache has no type associated with List(1, 2, 3)"
      try q"List(1, 2, 3)".tpe
      catch { case ex: SemanticException => assert(ex.msg.trim.startsWith(expectedFail)); throw ex }
    }
    val classDef = c.define("class C { def foo = List(1, 2, 3) }")
    classDef match {
      case impl.Defn.Class(_, _, _, _, impl.Template(_, _, _, Some(List(impl.Defn.Def(_, _, _, _, _, body))))) =>
        assert(body.show[Code] == "List(1, 2, 3)")
        assert(body.show[Semantics] === """
          |Term.Apply(Term.Name("List")[1], List(Lit.Int(1), Lit.Int(2), Lit.Int(3)))
          |[1] Type.Singleton(Term.Name("immutable")[2])::scala.collection.immutable.List
          |[2] Type.Singleton(Term.Name("collection")[3])::scala.collection.immutable
          |[3] Type.Singleton(Term.Name("scala")[4])::scala.collection
          |[4] Type.Singleton(Term.Name("_root_")[5])::scala
          |[5] 0::_root_
        """.trim.stripMargin)
        assert(body.tpe.show[Code] == "List[Int]")
        assert(body.tpe.show[Semantics] == """
          |Type.Apply(Type.Name("List")[1], List(Type.Name("Int")[2]))
          |[1] Type.Singleton(Term.Name("immutable")[4])::scala.collection.immutable#List
          |[2] Type.Singleton(Term.Name("scala")[3])::scala#Int
          |[3] Type.Singleton(Term.Name("_root_")[6])::scala
          |[4] Type.Singleton(Term.Name("collection")[5])::scala.collection.immutable
          |[5] Type.Singleton(Term.Name("scala")[3])::scala.collection
          |[6] 0::_root_
        """.trim.stripMargin)
    }
  }

  test("t\"List\".defn") {
    assert(t"List".defn.show[Code] == "type List[+A] = List[A]")
    assert(t"List".defn.show[Semantics] == """
      |Defn.Type(Nil, Type.Name("List")[1], List(Type.Param(List(Mod.Covariant()), Type.Name("A")[2], Nil, Type.Bounds(None, None), Nil, Nil)), Type.Apply(Type.Name("List")[3], List(Type.Name("A")[2])))
      |[1] Type.Singleton(Term.Name("package")[5])::scala.package#List
      |[2] 0::scala.package#List#A
      |[3] Type.Singleton(Term.Name("immutable")[4])::scala.collection.immutable#List
      |[4] Type.Singleton(Term.Name("collection")[7])::scala.collection.immutable
      |[5] Type.Singleton(Term.Name("scala")[6])::scala.package
      |[6] Type.Singleton(Term.Name("_root_")[8])::scala
      |[7] Type.Singleton(Term.Name("scala")[6])::scala.collection
      |[8] 0::_root_
    """.trim.stripMargin)
  }

  test("t\"List[Int]\".dealias") {
    assert(t"List[Int]".dealias.show[Code] == "List[Int]")
    assert(t"List[Int]".dealias.show[Semantics] == """
      |Type.Apply(Type.Name("List")[1], List(Type.Name("Int")[2]))
      |[1] Type.Singleton(Term.Name("immutable")[4])::scala.collection.immutable#List
      |[2] Type.Singleton(Term.Name("scala")[3])::scala#Int
      |[3] Type.Singleton(Term.Name("_root_")[6])::scala
      |[4] Type.Singleton(Term.Name("collection")[5])::scala.collection.immutable
      |[5] Type.Singleton(Term.Name("scala")[3])::scala.collection
      |[6] 0::_root_
    """.trim.stripMargin)
  }

  test("t\"List[_]\".dealias") {
    intercept[SemanticException] {
      val expectedFail = "Input scala.meta tree is not fully attributed and can't be converted to a scala.reflect artifact."
      try t"List[_]".dealias
      catch { case ex: SemanticException => assert(ex.msg.trim.startsWith(expectedFail)); throw ex }
    }
  }

  test("t\"List\".members") {
    assert(t"List".members.mkString(EOL) === """
      |def this()
      |override def companion: GenericCompanion[List] = jvmMethod("List.companion()Lscala/collection/generic/GenericCompanion;").invoke(this)
      |def ::[B >: A](x: B): List[B] = jvmMethod("List.$colon$colon(Ljava/lang/Object;)Lscala/collection/immutable/List;").invoke(this, x)
      |def :::[B >: A](prefix: List[B]): List[B] = jvmMethod("List.$colon$colon$colon(Lscala/collection/immutable/List;)Lscala/collection/immutable/List;").invoke(this, prefix)
      |def reverse_:::[B >: A](prefix: List[B]): List[B] = jvmMethod("List.reverse_$colon$colon$colon(Lscala/collection/immutable/List;)Lscala/collection/immutable/List;").invoke(this, prefix)
      |@inline final def mapConserve[B >: A <: AnyRef](f: A => B): List[B] = jvmMethod("List.mapConserve(Lscala/Function1;)Lscala/collection/immutable/List;").invoke(this, f)
      |override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("List.$plus$plus(Lscala/collection/GenTraversableOnce;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, bf)
      |override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("List.$plus$colon(Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, elem, bf)
      |override def toList: List[A] = jvmMethod("List.toList()Lscala/collection/immutable/List;").invoke(this)
      |override def take(n: Int): List[A] = jvmMethod("List.take(I)Lscala/collection/immutable/List;").invoke(this, n)
      |override def drop(n: Int): List[A] = jvmMethod("List.drop(I)Lscala/collection/immutable/List;").invoke(this, n)
      |override def slice(from: Int, until: Int): List[A] = jvmMethod("List.slice(II)Lscala/collection/immutable/List;").invoke(this, from, until)
      |override def takeRight(n: Int): List[A] = jvmMethod("List.takeRight(I)Lscala/collection/immutable/List;").invoke(this, n)
      |override def splitAt(n: Int): (List[A], List[A]) = jvmMethod("List.splitAt(I)Lscala/Tuple2;").invoke(this, n)
      |@noinline final override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("List.map(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, f, bf)
      |@noinline final override def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("List.collect(Lscala/PartialFunction;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, pf, bf)
      |@noinline final override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("List.flatMap(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, f, bf)
      |@inline final override def takeWhile(p: A => Boolean): List[A] = jvmMethod("List.takeWhile(Lscala/Function1;)Lscala/collection/immutable/List;").invoke(this, p)
      |@inline final override def dropWhile(p: A => Boolean): List[A] = jvmMethod("List.dropWhile(Lscala/Function1;)Lscala/collection/immutable/List;").invoke(this, p)
      |@inline final override def span(p: A => Boolean): (List[A], List[A]) = jvmMethod("List.span(Lscala/Function1;)Lscala/Tuple2;").invoke(this, p)
      |@inline final override def foreach[U](f: A => U): Unit = jvmMethod("List.foreach(Lscala/Function1;)V").invoke(this, f)
      |override def reverse: List[A] = jvmMethod("List.reverse()Lscala/collection/immutable/List;").invoke(this)
      |override def foldRight[B](z: B)(op: (A, B) => B): B = jvmMethod("List.foldRight(Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, op)
      |override def stringPrefix: String = jvmMethod("List.stringPrefix()Ljava/lang/String;").invoke(this)
      |override def toStream: Stream[A] = jvmMethod("List.toStream()Lscala/collection/immutable/Stream;").invoke(this)
      |protected final def writeReplace(): AnyRef = jvmMethod("List.writeReplace()Ljava/lang/Object;").invoke(this)
      |def length: Int = jvmMethod("scala.collection.LinearSeqOptimized.length()I").invoke(this)
      |def apply(n: Int): A = jvmMethod("scala.collection.LinearSeqOptimized.apply(I)Ljava/lang/Object;").invoke(this, n)
      |override def forall(p: A => Boolean): Boolean = jvmMethod("scala.collection.LinearSeqOptimized.forall(Lscala/Function1;)Z").invoke(this, p)
      |override def exists(p: A => Boolean): Boolean = jvmMethod("scala.collection.LinearSeqOptimized.exists(Lscala/Function1;)Z").invoke(this, p)
      |override def contains[A1 >: A](elem: A1): Boolean = jvmMethod("scala.collection.LinearSeqOptimized.contains(Ljava/lang/Object;)Z").invoke(this, elem)
      |override def find(p: A => Boolean): Option[A] = jvmMethod("scala.collection.LinearSeqOptimized.find(Lscala/Function1;)Lscala/Option;").invoke(this, p)
      |override def foldLeft[B](z: B)(f: (B, A) => B): B = jvmMethod("scala.collection.LinearSeqOptimized.foldLeft(Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, f)
      |override def reduceLeft[B >: A](f: (B, A) => B): B = jvmMethod("scala.collection.LinearSeqOptimized.reduceLeft(Lscala/Function2;)Ljava/lang/Object;").invoke(this, f)
      |override def reduceRight[B >: A](op: (A, B) => B): B = jvmMethod("scala.collection.LinearSeqOptimized.reduceRight(Lscala/Function2;)Ljava/lang/Object;").invoke(this, op)
      |override def last: A = jvmMethod("scala.collection.LinearSeqOptimized.last()Ljava/lang/Object;").invoke(this)
      |override def dropRight(n: Int): List[A] = jvmMethod("scala.collection.LinearSeqOptimized.dropRight(I)Lscala/collection/LinearSeqOptimized;").invoke(this, n)
      |override def sameElements[B >: A](that: GenIterable[B]): Boolean = jvmMethod("scala.collection.LinearSeqOptimized.sameElements(Lscala/collection/GenIterable;)Z").invoke(this, that)
      |override def lengthCompare(len: Int): Int = jvmMethod("scala.collection.LinearSeqOptimized.lengthCompare(I)I").invoke(this, len)
      |override def isDefinedAt(x: Int): Boolean = jvmMethod("scala.collection.LinearSeqOptimized.isDefinedAt(I)Z").invoke(this, x)
      |override def segmentLength(p: A => Boolean, from: Int): Int = jvmMethod("scala.collection.LinearSeqOptimized.segmentLength(Lscala/Function1;I)I").invoke(this, p, from)
      |override def indexWhere(p: A => Boolean, from: Int): Int = jvmMethod("scala.collection.LinearSeqOptimized.indexWhere(Lscala/Function1;I)I").invoke(this, p, from)
      |override def lastIndexWhere(p: A => Boolean, end: Int): Int = jvmMethod("scala.collection.LinearSeqOptimized.lastIndexWhere(Lscala/Function1;I)I").invoke(this, p, end)
      |def productElement(n: Int): Any
      |def productArity: Int
      |def productIterator: Iterator[Any] = jvmMethod("Product.productIterator()Lscala/collection/Iterator;").invoke(this)
      |def productPrefix: String = jvmMethod("Product.productPrefix()Ljava/lang/String;").invoke(this)
      |override def seq: LinearSeq[A] = jvmMethod("scala.collection.immutable.LinearSeq.seq()Lscala/collection/immutable/LinearSeq;").invoke(this)
      |protected[this] override def thisCollection: LinearSeq[A] = jvmMethod("scala.collection.LinearSeqLike.thisCollection()Lscala/collection/LinearSeq;").invoke(this)
      |protected[this] override def toCollection(repr: List[A]): LinearSeq[A] = jvmMethod("scala.collection.LinearSeqLike.toCollection(Lscala/collection/LinearSeqLike;)Lscala/collection/LinearSeq;").invoke(this, repr)
      |override def hashCode(): Int = jvmMethod("scala.collection.LinearSeqLike.hashCode()I").invoke(this)
      |override def iterator: Iterator[A] = jvmMethod("scala.collection.LinearSeqLike.iterator()Lscala/collection/Iterator;").invoke(this)
      |@tailrec final override def corresponds[B](that: GenSeq[B])(p: (A, B) => Boolean): Boolean = jvmMethod("scala.collection.LinearSeqLike.corresponds(Lscala/collection/GenSeq;Lscala/Function2;)Z").invoke(this, that, p)
      |override def toSeq: Seq[A] = jvmMethod("scala.collection.immutable.Seq.toSeq()Lscala/collection/immutable/Seq;").invoke(this)
      |protected[this] override def parCombiner: Combiner[A, ParSeq[A]] = jvmMethod("scala.collection.immutable.Seq.parCombiner()Lscala/collection/parallel/Combiner;").invoke(this)
      |override def isEmpty: Boolean = jvmMethod("scala.collection.SeqLike.isEmpty()Z").invoke(this)
      |override def size: Int = jvmMethod("scala.collection.SeqLike.size()I").invoke(this)
      |def permutations: Iterator[List[A]] = jvmMethod("scala.collection.SeqLike.permutations()Lscala/collection/Iterator;").invoke(this)
      |def combinations(n: Int): Iterator[List[A]] = jvmMethod("scala.collection.SeqLike.combinations(I)Lscala/collection/Iterator;").invoke(this, n)
      |def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.SeqLike.reverseMap(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, f, bf)
      |def reverseIterator: Iterator[A] = jvmMethod("scala.collection.SeqLike.reverseIterator()Lscala/collection/Iterator;").invoke(this)
      |def startsWith[B](that: GenSeq[B], offset: Int): Boolean = jvmMethod("scala.collection.SeqLike.startsWith(Lscala/collection/GenSeq;I)Z").invoke(this, that, offset)
      |def endsWith[B](that: GenSeq[B]): Boolean = jvmMethod("scala.collection.SeqLike.endsWith(Lscala/collection/GenSeq;)Z").invoke(this, that)
      |def indexOfSlice[B >: A](that: GenSeq[B]): Int = jvmMethod("scala.collection.SeqLike.indexOfSlice(Lscala/collection/GenSeq;)I").invoke(this, that)
      |def indexOfSlice[B >: A](that: GenSeq[B], from: Int): Int = jvmMethod("scala.collection.SeqLike.indexOfSlice(Lscala/collection/GenSeq;I)I").invoke(this, that, from)
      |def lastIndexOfSlice[B >: A](that: GenSeq[B]): Int = jvmMethod("scala.collection.SeqLike.lastIndexOfSlice(Lscala/collection/GenSeq;)I").invoke(this, that)
      |def lastIndexOfSlice[B >: A](that: GenSeq[B], end: Int): Int = jvmMethod("scala.collection.SeqLike.lastIndexOfSlice(Lscala/collection/GenSeq;I)I").invoke(this, that, end)
      |def containsSlice[B](that: GenSeq[B]): Boolean = jvmMethod("scala.collection.SeqLike.containsSlice(Lscala/collection/GenSeq;)Z").invoke(this, that)
      |override def union[B >: A, That](that: GenSeq[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.SeqLike.union(Lscala/collection/GenSeq;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, bf)
      |def diff[B >: A](that: GenSeq[B]): List[A] = jvmMethod("scala.collection.SeqLike.diff(Lscala/collection/GenSeq;)Ljava/lang/Object;").invoke(this, that)
      |def intersect[B >: A](that: GenSeq[B]): List[A] = jvmMethod("scala.collection.SeqLike.intersect(Lscala/collection/GenSeq;)Ljava/lang/Object;").invoke(this, that)
      |def distinct: List[A] = jvmMethod("scala.collection.SeqLike.distinct()Ljava/lang/Object;").invoke(this)
      |def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.SeqLike.patch(ILscala/collection/GenSeq;ILscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, from, patch, replaced, bf)
      |def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.SeqLike.updated(ILjava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, index, elem, bf)
      |def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.SeqLike.$colon$plus(Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, elem, bf)
      |def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.SeqLike.padTo(ILjava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, len, elem, bf)
      |def sortWith(lt: (A, A) => Boolean): List[A] = jvmMethod("scala.collection.SeqLike.sortWith(Lscala/Function2;)Ljava/lang/Object;").invoke(this, lt)
      |def sortBy[B](f: A => B)(implicit ord: Ordering[B]): List[A] = jvmMethod("scala.collection.SeqLike.sortBy(Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, f, ord)
      |def sorted[B >: A](implicit ord: Ordering[B]): List[A] = jvmMethod("scala.collection.SeqLike.sorted(Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, ord)
      |def indices: Range = jvmMethod("scala.collection.SeqLike.indices()Lscala/collection/immutable/Range;").invoke(this)
      |override def view: AnyRef with SeqView[A, List[A]] = jvmMethod("scala.collection.SeqLike.view()Lscala/collection/SeqView;").invoke(this)
      |override def view(from: Int, until: Int): SeqView[A, List[A]] = jvmMethod("scala.collection.SeqLike.view(II)Lscala/collection/SeqView;").invoke(this, from, until)
      |override def toString(): String = jvmMethod("scala.collection.SeqLike.toString()Ljava/lang/String;").invoke(this)
      |def prefixLength(p: A => Boolean): Int = jvmMethod("scala.collection.GenSeqLike.prefixLength(Lscala/Function1;)I").invoke(this, p)
      |def indexWhere(p: A => Boolean): Int = jvmMethod("scala.collection.GenSeqLike.indexWhere(Lscala/Function1;)I").invoke(this, p)
      |def indexOf[B >: A](elem: B): Int = jvmMethod("scala.collection.GenSeqLike.indexOf(Ljava/lang/Object;)I").invoke(this, elem)
      |def indexOf[B >: A](elem: B, from: Int): Int = jvmMethod("scala.collection.GenSeqLike.indexOf(Ljava/lang/Object;I)I").invoke(this, elem, from)
      |def lastIndexOf[B >: A](elem: B): Int = jvmMethod("scala.collection.GenSeqLike.lastIndexOf(Ljava/lang/Object;)I").invoke(this, elem)
      |def lastIndexOf[B >: A](elem: B, end: Int): Int = jvmMethod("scala.collection.GenSeqLike.lastIndexOf(Ljava/lang/Object;I)I").invoke(this, elem, end)
      |def lastIndexWhere(p: A => Boolean): Int = jvmMethod("scala.collection.GenSeqLike.lastIndexWhere(Lscala/Function1;)I").invoke(this, p)
      |def startsWith[B](that: GenSeq[B]): Boolean = jvmMethod("scala.collection.GenSeqLike.startsWith(Lscala/collection/GenSeq;)Z").invoke(this, that)
      |override def equals(that: Any): Boolean = jvmMethod("scala.collection.GenSeqLike.equals(Ljava/lang/Object;)Z").invoke(this, that)
      |def orElse[A1 <: Int, B1 >: A](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = jvmMethod("PartialFunction.orElse(Lscala/PartialFunction;)Lscala/PartialFunction;").invoke(this, that)
      |override def andThen[C](k: A => C): PartialFunction[Int, C] = jvmMethod("PartialFunction.andThen(Lscala/Function1;)Lscala/PartialFunction;").invoke(this, k)
      |def lift: Int => Option[A] = jvmMethod("PartialFunction.lift()Lscala/Function1;").invoke(this)
      |def applyOrElse[A1 <: Int, B1 >: A](x: A1, default: A1 => B1): B1 = jvmMethod("PartialFunction.applyOrElse(Ljava/lang/Object;Lscala/Function1;)Ljava/lang/Object;").invoke(this, x, default)
      |def runWith[U](action: A => U): Int => Boolean = jvmMethod("PartialFunction.runWith(Lscala/Function1;)Lscala/Function1;").invoke(this, action)
      |@unspecialized def compose[A](g: A => Int): A => A = jvmMethod("Function1.compose(Lscala/Function1;)Lscala/Function1;").invoke(this, g)
      |override def toIterable: Iterable[A] = jvmMethod("scala.collection.IterableLike.toIterable()Lscala/collection/Iterable;").invoke(this)
      |@deprecatedOverriding("toIterator should stay consistent with iterator for all Iterables: override iterator instead.", "2.11.0") override def toIterator: Iterator[A] = jvmMethod("scala.collection.IterableLike.toIterator()Lscala/collection/Iterator;").invoke(this)
      |override def head: A = jvmMethod("scala.collection.IterableLike.head()Ljava/lang/Object;").invoke(this)
      |def grouped(size: Int): Iterator[List[A]] = jvmMethod("scala.collection.IterableLike.grouped(I)Lscala/collection/Iterator;").invoke(this, size)
      |def sliding(size: Int): Iterator[List[A]] = jvmMethod("scala.collection.IterableLike.sliding(I)Lscala/collection/Iterator;").invoke(this, size)
      |def sliding(size: Int, step: Int): Iterator[List[A]] = jvmMethod("scala.collection.IterableLike.sliding(II)Lscala/collection/Iterator;").invoke(this, size, step)
      |override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = jvmMethod("scala.collection.IterableLike.copyToArray(Ljava/lang/Object;II)V").invoke(this, xs, start, len)
      |def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[List[A], (A1, B), That]): That = jvmMethod("scala.collection.IterableLike.zip(Lscala/collection/GenIterable;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, bf)
      |def zipAll[B, A1 >: A, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[List[A], (A1, B), That]): That = jvmMethod("scala.collection.IterableLike.zipAll(Lscala/collection/GenIterable;Ljava/lang/Object;Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, thisElem, thatElem, bf)
      |def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[List[A], (A1, Int), That]): That = jvmMethod("scala.collection.IterableLike.zipWithIndex(Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, bf)
      |override def canEqual(that: Any): Boolean = jvmMethod("scala.collection.IterableLike.canEqual(Ljava/lang/Object;)Z").invoke(this, that)
      |protected[this] def newBuilder: Builder[A, List[A]] = jvmMethod("scala.collection.generic.GenericTraversableTemplate.newBuilder()Lscala/collection/mutable/Builder;").invoke(this)
      |def genericBuilder[B]: Builder[B, List[B]] = jvmMethod("scala.collection.generic.GenericTraversableTemplate.genericBuilder()Lscala/collection/mutable/Builder;").invoke(this)
      |def unzip[A1, A2](implicit asPair: A => (A1, A2)): (List[A1], List[A2]) = jvmMethod("scala.collection.generic.GenericTraversableTemplate.unzip(Lscala/Function1;)Lscala/Tuple2;").invoke(this, asPair)
      |def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (List[A1], List[A2], List[A3]) = jvmMethod("scala.collection.generic.GenericTraversableTemplate.unzip3(Lscala/Function1;)Lscala/Tuple3;").invoke(this, asTriple)
      |def flatten[B](implicit asTraversable: A => GenTraversableOnce[B]): List[B] = jvmMethod("scala.collection.generic.GenericTraversableTemplate.flatten(Lscala/Function1;)Lscala/collection/GenTraversable;").invoke(this, asTraversable)
      |@migration("`transpose` throws an `IllegalArgumentException` if collections are not uniformly sized.", "2.9.0") def transpose[B](implicit asTraversable: A => GenTraversableOnce[B]): List[List[B]] = jvmMethod("scala.collection.generic.GenericTraversableTemplate.transpose(Lscala/Function1;)Lscala/collection/GenTraversable;").invoke(this, asTraversable)
      |protected[this] type Self = List[A]
      |def repr: List[A] = jvmMethod("scala.collection.TraversableLike.repr()Ljava/lang/Object;").invoke(this)
      |final def isTraversableAgain: Boolean = jvmMethod("scala.collection.TraversableLike.isTraversableAgain()Z").invoke(this)
      |def hasDefiniteSize: Boolean = jvmMethod("scala.collection.TraversableLike.hasDefiniteSize()Z").invoke(this)
      |def ++:[B >: A, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.TraversableLike.$plus$plus$colon(Lscala/collection/TraversableOnce;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, bf)
      |def ++:[B >: A, That](that: Traversable[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.TraversableLike.$plus$plus$colon(Lscala/collection/Traversable;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, bf)
      |def filter(p: A => Boolean): List[A] = jvmMethod("scala.collection.TraversableLike.filter(Lscala/Function1;)Ljava/lang/Object;").invoke(this, p)
      |def filterNot(p: A => Boolean): List[A] = jvmMethod("scala.collection.TraversableLike.filterNot(Lscala/Function1;)Ljava/lang/Object;").invoke(this, p)
      |def partition(p: A => Boolean): (List[A], List[A]) = jvmMethod("scala.collection.TraversableLike.partition(Lscala/Function1;)Lscala/Tuple2;").invoke(this, p)
      |def groupBy[K](f: A => K): Map[K, List[A]] = jvmMethod("scala.collection.TraversableLike.groupBy(Lscala/Function1;)Lscala/collection/immutable/Map;").invoke(this, f)
      |def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.TraversableLike.scan(Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, z, op, cbf)
      |def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.TraversableLike.scanLeft(Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, z, op, bf)
      |@migration("The behavior of `scanRight` has changed. The previous behavior can be reproduced with scanRight.reverse.", "2.9.0") def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = jvmMethod("scala.collection.TraversableLike.scanRight(Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, z, op, bf)
      |def headOption: Option[A] = jvmMethod("scala.collection.TraversableLike.headOption()Lscala/Option;").invoke(this)
      |override def tail: List[A] = jvmMethod("scala.collection.TraversableLike.tail()Ljava/lang/Object;").invoke(this)
      |def lastOption: Option[A] = jvmMethod("scala.collection.TraversableLike.lastOption()Lscala/Option;").invoke(this)
      |def init: List[A] = jvmMethod("scala.collection.TraversableLike.init()Ljava/lang/Object;").invoke(this)
      |private[scala] def sliceWithKnownDelta(from: Int, until: Int, delta: Int): List[A] = jvmMethod("scala.collection.TraversableLike.sliceWithKnownDelta(III)Ljava/lang/Object;").invoke(this, from, until, delta)
      |private[scala] def sliceWithKnownBound(from: Int, until: Int): List[A] = jvmMethod("scala.collection.TraversableLike.sliceWithKnownBound(II)Ljava/lang/Object;").invoke(this, from, until)
      |def tails: Iterator[List[A]] = jvmMethod("scala.collection.TraversableLike.tails()Lscala/collection/Iterator;").invoke(this)
      |def inits: Iterator[List[A]] = jvmMethod("scala.collection.TraversableLike.inits()Lscala/collection/Iterator;").invoke(this)
      |@deprecatedOverriding("Enforce contract of toTraversable that if it is Traversable it returns itself.", "2.11.0") def toTraversable: Traversable[A] = jvmMethod("scala.collection.TraversableLike.toTraversable()Lscala/collection/Traversable;").invoke(this)
      |override def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A] = jvmMethod("scala.collection.TraversableLike.to(Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, cbf)
      |def withFilter(p: A => Boolean): FilterMonadic[A, List[A]] = jvmMethod("scala.collection.TraversableLike.withFilter(Lscala/Function1;)Lscala/collection/generic/FilterMonadic;").invoke(this, p)
      |class WithFilter(p: A => Boolean) extends AnyRef with FilterMonadic[A, Repr] { ... }
      |def par: ParSeq[A] = jvmMethod("scala.collection.Parallelizable.par()Lscala/collection/Parallel;").invoke(this)
      |protected[this] def reversed: List[A] = jvmMethod("scala.collection.TraversableOnce.reversed()Lscala/collection/immutable/List;").invoke(this)
      |def nonEmpty: Boolean = jvmMethod("scala.collection.TraversableOnce.nonEmpty()Z").invoke(this)
      |def count(p: A => Boolean): Int = jvmMethod("scala.collection.TraversableOnce.count(Lscala/Function1;)I").invoke(this, p)
      |def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = jvmMethod("scala.collection.TraversableOnce.collectFirst(Lscala/PartialFunction;)Lscala/Option;").invoke(this, pf)
      |def /:[B](z: B)(op: (B, A) => B): B = jvmMethod("scala.collection.TraversableOnce.$div$colon(Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, op)
      |def :\[B](z: B)(op: (A, B) => B): B = jvmMethod("scala.collection.TraversableOnce.$colon$bslash(Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, op)
      |def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = jvmMethod("scala.collection.TraversableOnce.reduceLeftOption(Lscala/Function2;)Lscala/Option;").invoke(this, op)
      |def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = jvmMethod("scala.collection.TraversableOnce.reduceRightOption(Lscala/Function2;)Lscala/Option;").invoke(this, op)
      |def reduce[A1 >: A](op: (A1, A1) => A1): A1 = jvmMethod("scala.collection.TraversableOnce.reduce(Lscala/Function2;)Ljava/lang/Object;").invoke(this, op)
      |def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1] = jvmMethod("scala.collection.TraversableOnce.reduceOption(Lscala/Function2;)Lscala/Option;").invoke(this, op)
      |def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = jvmMethod("scala.collection.TraversableOnce.fold(Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, op)
      |def aggregate[B](z: => B)(seqop: (B, A) => B, combop: (B, B) => B): B = jvmMethod("scala.collection.TraversableOnce.aggregate(Lscala/Function0;Lscala/Function2;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, seqop, combop)
      |def sum[B >: A](implicit num: Numeric[B]): B = jvmMethod("scala.collection.TraversableOnce.sum(Lscala/math/Numeric;)Ljava/lang/Object;").invoke(this, num)
      |def product[B >: A](implicit num: Numeric[B]): B = jvmMethod("scala.collection.TraversableOnce.product(Lscala/math/Numeric;)Ljava/lang/Object;").invoke(this, num)
      |def min[B >: A](implicit cmp: Ordering[B]): A = jvmMethod("scala.collection.TraversableOnce.min(Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, cmp)
      |def max[B >: A](implicit cmp: Ordering[B]): A = jvmMethod("scala.collection.TraversableOnce.max(Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, cmp)
      |def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = jvmMethod("scala.collection.TraversableOnce.maxBy(Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, f, cmp)
      |def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = jvmMethod("scala.collection.TraversableOnce.minBy(Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, f, cmp)
      |def copyToBuffer[B >: A](dest: Buffer[B]): Unit = jvmMethod("scala.collection.TraversableOnce.copyToBuffer(Lscala/collection/mutable/Buffer;)V").invoke(this, dest)
      |def copyToArray[B >: A](xs: Array[B], start: Int): Unit = jvmMethod("scala.collection.TraversableOnce.copyToArray(Ljava/lang/Object;I)V").invoke(this, xs, start)
      |def copyToArray[B >: A](xs: Array[B]): Unit = jvmMethod("scala.collection.TraversableOnce.copyToArray(Ljava/lang/Object;)V").invoke(this, xs)
      |def toArray[B >: A: ClassTag](implicit evidence$1: ClassTag[B]): Array[B] = jvmMethod("scala.collection.TraversableOnce.toArray(Lscala/reflect/ClassTag;)Ljava/lang/Object;").invoke(this, evidence$1)
      |def toIndexedSeq: IndexedSeq[A] = jvmMethod("scala.collection.TraversableOnce.toIndexedSeq()Lscala/collection/immutable/IndexedSeq;").invoke(this)
      |def toBuffer[B >: A]: Buffer[B] = jvmMethod("scala.collection.TraversableOnce.toBuffer()Lscala/collection/mutable/Buffer;").invoke(this)
      |def toSet[B >: A]: Set[B] = jvmMethod("scala.collection.TraversableOnce.toSet()Lscala/collection/immutable/Set;").invoke(this)
      |def toVector: Vector[A] = jvmMethod("scala.collection.TraversableOnce.toVector()Lscala/collection/immutable/Vector;").invoke(this)
      |def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U] = jvmMethod("scala.collection.TraversableOnce.toMap(Lscala/Predef/$less$colon$less;)Lscala/collection/immutable/Map;").invoke(this, ev)
      |def mkString(start: String, sep: String, end: String): String = jvmMethod("scala.collection.TraversableOnce.mkString(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;").invoke(this, start, sep, end)
      |def mkString(sep: String): String = jvmMethod("scala.collection.TraversableOnce.mkString(Ljava/lang/String;)Ljava/lang/String;").invoke(this, sep)
      |def mkString: String = jvmMethod("scala.collection.TraversableOnce.mkString()Ljava/lang/String;").invoke(this)
      |def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = jvmMethod("scala.collection.TraversableOnce.addString(Lscala/collection/mutable/StringBuilder;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Lscala/collection/mutable/StringBuilder;").invoke(this, b, start, sep, end)
      |def addString(b: StringBuilder, sep: String): StringBuilder = jvmMethod("scala.collection.TraversableOnce.addString(Lscala/collection/mutable/StringBuilder;Ljava/lang/String;)Lscala/collection/mutable/StringBuilder;").invoke(this, b, sep)
      |def addString(b: StringBuilder): StringBuilder = jvmMethod("scala.collection.TraversableOnce.addString(Lscala/collection/mutable/StringBuilder;)Lscala/collection/mutable/StringBuilder;").invoke(this, b)
      |final def getClass(): Class[?0] forSome { type ?0 } = intrinsic("Object.getClass()Ljava/lang/Class;", this)
      |@throws[CloneNotSupportedException] protected[lang] def clone(): Object = jvmMethod("Object.clone()Ljava/lang/Object;").invoke(this)
      |final def notify(): Unit = jvmMethod("Object.notify()V").invoke(this)
      |final def notifyAll(): Unit = jvmMethod("Object.notifyAll()V").invoke(this)
      |@throws[InterruptedException] final def wait(x$1: Long): Unit = jvmMethod("Object.wait(J)V").invoke(this, x$1)
      |@throws[InterruptedException] final def wait(x$1: Long, x$2: Int): Unit = jvmMethod("Object.wait(JI)V").invoke(this, x$1, x$2)
      |@throws[InterruptedException] final def wait(): Unit = jvmMethod("Object.wait()V").invoke(this)
      |@throws[Throwable] protected[lang] def finalize(): Unit = jvmMethod("Object.finalize()V").invoke(this)
      |final def eq(x$1: AnyRef): Boolean = intrinsic("Object.eq(Ljava/lang/Object;)Z", this, x$1)
      |final def ne(x$1: AnyRef): Boolean = intrinsic("Object.ne(Ljava/lang/Object;)Z", this, x$1)
      |final def ==(x$1: Any): Boolean = intrinsic("Object.$eq$eq(Ljava/lang/Object;)Z", this, x$1)
      |final def !=(x$1: Any): Boolean = intrinsic("Object.$bang$eq(Ljava/lang/Object;)Z", this, x$1)
      |final def ##(): Int = intrinsic("Object.$hash$hash()I", this)
      |final def synchronized[T0](x$1: T0): T0 = intrinsic("Object.synchronized(Ljava/lang/Object;)Ljava/lang/Object;", this, x$1)
      |final def isInstanceOf[T0]: Boolean = intrinsic("Object.isInstanceOf()Z", this)
      |final def asInstanceOf[T0]: T0 = intrinsic("Object.asInstanceOf()Ljava/lang/Object;", this)
    """.trim.stripMargin)
  }

  test("t\"List[Int]\".members") {
    assert(t"List[Int]".members.mkString(EOL) === """
      |def this()
      |override def companion: GenericCompanion[List] = jvmMethod("List.companion()Lscala/collection/generic/GenericCompanion;").invoke(this)
      |def ::[B >: Int](x: B): List[B] = jvmMethod("List.$colon$colon(Ljava/lang/Object;)Lscala/collection/immutable/List;").invoke(this, x)
      |def :::[B >: Int](prefix: List[B]): List[B] = jvmMethod("List.$colon$colon$colon(Lscala/collection/immutable/List;)Lscala/collection/immutable/List;").invoke(this, prefix)
      |def reverse_:::[B >: Int](prefix: List[B]): List[B] = jvmMethod("List.reverse_$colon$colon$colon(Lscala/collection/immutable/List;)Lscala/collection/immutable/List;").invoke(this, prefix)
      |@inline final def mapConserve[B >: Int <: AnyRef](f: Int => B): List[B] = jvmMethod("List.mapConserve(Lscala/Function1;)Lscala/collection/immutable/List;").invoke(this, f)
      |override def ++[B >: Int, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("List.$plus$plus(Lscala/collection/GenTraversableOnce;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, bf)
      |override def +:[B >: Int, That](elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("List.$plus$colon(Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, elem, bf)
      |override def toList: List[Int] = jvmMethod("List.toList()Lscala/collection/immutable/List;").invoke(this)
      |override def take(n: Int): List[Int] = jvmMethod("List.take(I)Lscala/collection/immutable/List;").invoke(this, n)
      |override def drop(n: Int): List[Int] = jvmMethod("List.drop(I)Lscala/collection/immutable/List;").invoke(this, n)
      |override def slice(from: Int, until: Int): List[Int] = jvmMethod("List.slice(II)Lscala/collection/immutable/List;").invoke(this, from, until)
      |override def takeRight(n: Int): List[Int] = jvmMethod("List.takeRight(I)Lscala/collection/immutable/List;").invoke(this, n)
      |override def splitAt(n: Int): (List[Int], List[Int]) = jvmMethod("List.splitAt(I)Lscala/Tuple2;").invoke(this, n)
      |@noinline final override def map[B, That](f: Int => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("List.map(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, f, bf)
      |@noinline final override def collect[B, That](pf: PartialFunction[Int, B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("List.collect(Lscala/PartialFunction;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, pf, bf)
      |@noinline final override def flatMap[B, That](f: Int => GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("List.flatMap(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, f, bf)
      |@inline final override def takeWhile(p: Int => Boolean): List[Int] = jvmMethod("List.takeWhile(Lscala/Function1;)Lscala/collection/immutable/List;").invoke(this, p)
      |@inline final override def dropWhile(p: Int => Boolean): List[Int] = jvmMethod("List.dropWhile(Lscala/Function1;)Lscala/collection/immutable/List;").invoke(this, p)
      |@inline final override def span(p: Int => Boolean): (List[Int], List[Int]) = jvmMethod("List.span(Lscala/Function1;)Lscala/Tuple2;").invoke(this, p)
      |@inline final override def foreach[U](f: Int => U): Unit = jvmMethod("List.foreach(Lscala/Function1;)V").invoke(this, f)
      |override def reverse: List[Int] = jvmMethod("List.reverse()Lscala/collection/immutable/List;").invoke(this)
      |override def foldRight[B](z: B)(op: (Int, B) => B): B = jvmMethod("List.foldRight(Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, op)
      |override def stringPrefix: String = jvmMethod("List.stringPrefix()Ljava/lang/String;").invoke(this)
      |override def toStream: Stream[Int] = jvmMethod("List.toStream()Lscala/collection/immutable/Stream;").invoke(this)
      |protected final def writeReplace(): AnyRef = jvmMethod("List.writeReplace()Ljava/lang/Object;").invoke(this)
      |def length: Int = jvmMethod("scala.collection.LinearSeqOptimized.length()I").invoke(this)
      |def apply(n: Int): Int = jvmMethod("scala.collection.LinearSeqOptimized.apply(I)Ljava/lang/Object;").invoke(this, n)
      |override def forall(p: Int => Boolean): Boolean = jvmMethod("scala.collection.LinearSeqOptimized.forall(Lscala/Function1;)Z").invoke(this, p)
      |override def exists(p: Int => Boolean): Boolean = jvmMethod("scala.collection.LinearSeqOptimized.exists(Lscala/Function1;)Z").invoke(this, p)
      |override def contains[A1 >: Int](elem: A1): Boolean = jvmMethod("scala.collection.LinearSeqOptimized.contains(Ljava/lang/Object;)Z").invoke(this, elem)
      |override def find(p: Int => Boolean): Option[Int] = jvmMethod("scala.collection.LinearSeqOptimized.find(Lscala/Function1;)Lscala/Option;").invoke(this, p)
      |override def foldLeft[B](z: B)(f: (B, Int) => B): B = jvmMethod("scala.collection.LinearSeqOptimized.foldLeft(Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, f)
      |override def reduceLeft[B >: Int](f: (B, Int) => B): B = jvmMethod("scala.collection.LinearSeqOptimized.reduceLeft(Lscala/Function2;)Ljava/lang/Object;").invoke(this, f)
      |override def reduceRight[B >: Int](op: (Int, B) => B): B = jvmMethod("scala.collection.LinearSeqOptimized.reduceRight(Lscala/Function2;)Ljava/lang/Object;").invoke(this, op)
      |override def last: Int = jvmMethod("scala.collection.LinearSeqOptimized.last()Ljava/lang/Object;").invoke(this)
      |override def dropRight(n: Int): List[Int] = jvmMethod("scala.collection.LinearSeqOptimized.dropRight(I)Lscala/collection/LinearSeqOptimized;").invoke(this, n)
      |override def sameElements[B >: Int](that: GenIterable[B]): Boolean = jvmMethod("scala.collection.LinearSeqOptimized.sameElements(Lscala/collection/GenIterable;)Z").invoke(this, that)
      |override def lengthCompare(len: Int): Int = jvmMethod("scala.collection.LinearSeqOptimized.lengthCompare(I)I").invoke(this, len)
      |override def isDefinedAt(x: Int): Boolean = jvmMethod("scala.collection.LinearSeqOptimized.isDefinedAt(I)Z").invoke(this, x)
      |override def segmentLength(p: Int => Boolean, from: Int): Int = jvmMethod("scala.collection.LinearSeqOptimized.segmentLength(Lscala/Function1;I)I").invoke(this, p, from)
      |override def indexWhere(p: Int => Boolean, from: Int): Int = jvmMethod("scala.collection.LinearSeqOptimized.indexWhere(Lscala/Function1;I)I").invoke(this, p, from)
      |override def lastIndexWhere(p: Int => Boolean, end: Int): Int = jvmMethod("scala.collection.LinearSeqOptimized.lastIndexWhere(Lscala/Function1;I)I").invoke(this, p, end)
      |def productElement(n: Int): Any
      |def productArity: Int
      |def productIterator: Iterator[Any] = jvmMethod("Product.productIterator()Lscala/collection/Iterator;").invoke(this)
      |def productPrefix: String = jvmMethod("Product.productPrefix()Ljava/lang/String;").invoke(this)
      |override def seq: LinearSeq[Int] = jvmMethod("scala.collection.immutable.LinearSeq.seq()Lscala/collection/immutable/LinearSeq;").invoke(this)
      |protected[this] override def thisCollection: LinearSeq[Int] = jvmMethod("scala.collection.LinearSeqLike.thisCollection()Lscala/collection/LinearSeq;").invoke(this)
      |protected[this] override def toCollection(repr: List[Int]): LinearSeq[Int] = jvmMethod("scala.collection.LinearSeqLike.toCollection(Lscala/collection/LinearSeqLike;)Lscala/collection/LinearSeq;").invoke(this, repr)
      |override def hashCode(): Int = jvmMethod("scala.collection.LinearSeqLike.hashCode()I").invoke(this)
      |override def iterator: Iterator[Int] = jvmMethod("scala.collection.LinearSeqLike.iterator()Lscala/collection/Iterator;").invoke(this)
      |@tailrec final override def corresponds[B](that: GenSeq[B])(p: (Int, B) => Boolean): Boolean = jvmMethod("scala.collection.LinearSeqLike.corresponds(Lscala/collection/GenSeq;Lscala/Function2;)Z").invoke(this, that, p)
      |override def toSeq: Seq[Int] = jvmMethod("scala.collection.immutable.Seq.toSeq()Lscala/collection/immutable/Seq;").invoke(this)
      |protected[this] override def parCombiner: Combiner[Int, ParSeq[Int]] = jvmMethod("scala.collection.immutable.Seq.parCombiner()Lscala/collection/parallel/Combiner;").invoke(this)
      |override def isEmpty: Boolean = jvmMethod("scala.collection.SeqLike.isEmpty()Z").invoke(this)
      |override def size: Int = jvmMethod("scala.collection.SeqLike.size()I").invoke(this)
      |def permutations: Iterator[List[Int]] = jvmMethod("scala.collection.SeqLike.permutations()Lscala/collection/Iterator;").invoke(this)
      |def combinations(n: Int): Iterator[List[Int]] = jvmMethod("scala.collection.SeqLike.combinations(I)Lscala/collection/Iterator;").invoke(this, n)
      |def reverseMap[B, That](f: Int => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.SeqLike.reverseMap(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, f, bf)
      |def reverseIterator: Iterator[Int] = jvmMethod("scala.collection.SeqLike.reverseIterator()Lscala/collection/Iterator;").invoke(this)
      |def startsWith[B](that: GenSeq[B], offset: Int): Boolean = jvmMethod("scala.collection.SeqLike.startsWith(Lscala/collection/GenSeq;I)Z").invoke(this, that, offset)
      |def endsWith[B](that: GenSeq[B]): Boolean = jvmMethod("scala.collection.SeqLike.endsWith(Lscala/collection/GenSeq;)Z").invoke(this, that)
      |def indexOfSlice[B >: Int](that: GenSeq[B]): Int = jvmMethod("scala.collection.SeqLike.indexOfSlice(Lscala/collection/GenSeq;)I").invoke(this, that)
      |def indexOfSlice[B >: Int](that: GenSeq[B], from: Int): Int = jvmMethod("scala.collection.SeqLike.indexOfSlice(Lscala/collection/GenSeq;I)I").invoke(this, that, from)
      |def lastIndexOfSlice[B >: Int](that: GenSeq[B]): Int = jvmMethod("scala.collection.SeqLike.lastIndexOfSlice(Lscala/collection/GenSeq;)I").invoke(this, that)
      |def lastIndexOfSlice[B >: Int](that: GenSeq[B], end: Int): Int = jvmMethod("scala.collection.SeqLike.lastIndexOfSlice(Lscala/collection/GenSeq;I)I").invoke(this, that, end)
      |def containsSlice[B](that: GenSeq[B]): Boolean = jvmMethod("scala.collection.SeqLike.containsSlice(Lscala/collection/GenSeq;)Z").invoke(this, that)
      |override def union[B >: Int, That](that: GenSeq[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.SeqLike.union(Lscala/collection/GenSeq;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, bf)
      |def diff[B >: Int](that: GenSeq[B]): List[Int] = jvmMethod("scala.collection.SeqLike.diff(Lscala/collection/GenSeq;)Ljava/lang/Object;").invoke(this, that)
      |def intersect[B >: Int](that: GenSeq[B]): List[Int] = jvmMethod("scala.collection.SeqLike.intersect(Lscala/collection/GenSeq;)Ljava/lang/Object;").invoke(this, that)
      |def distinct: List[Int] = jvmMethod("scala.collection.SeqLike.distinct()Ljava/lang/Object;").invoke(this)
      |def patch[B >: Int, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.SeqLike.patch(ILscala/collection/GenSeq;ILscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, from, patch, replaced, bf)
      |def updated[B >: Int, That](index: Int, elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.SeqLike.updated(ILjava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, index, elem, bf)
      |def :+[B >: Int, That](elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.SeqLike.$colon$plus(Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, elem, bf)
      |def padTo[B >: Int, That](len: Int, elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.SeqLike.padTo(ILjava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, len, elem, bf)
      |def sortWith(lt: (Int, Int) => Boolean): List[Int] = jvmMethod("scala.collection.SeqLike.sortWith(Lscala/Function2;)Ljava/lang/Object;").invoke(this, lt)
      |def sortBy[B](f: Int => B)(implicit ord: Ordering[B]): List[Int] = jvmMethod("scala.collection.SeqLike.sortBy(Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, f, ord)
      |def sorted[B >: Int](implicit ord: Ordering[B]): List[Int] = jvmMethod("scala.collection.SeqLike.sorted(Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, ord)
      |def indices: Range = jvmMethod("scala.collection.SeqLike.indices()Lscala/collection/immutable/Range;").invoke(this)
      |override def view: AnyRef with SeqView[Int, List[Int]] = jvmMethod("scala.collection.SeqLike.view()Lscala/collection/SeqView;").invoke(this)
      |override def view(from: Int, until: Int): SeqView[Int, List[Int]] = jvmMethod("scala.collection.SeqLike.view(II)Lscala/collection/SeqView;").invoke(this, from, until)
      |override def toString(): String = jvmMethod("scala.collection.SeqLike.toString()Ljava/lang/String;").invoke(this)
      |def prefixLength(p: Int => Boolean): Int = jvmMethod("scala.collection.GenSeqLike.prefixLength(Lscala/Function1;)I").invoke(this, p)
      |def indexWhere(p: Int => Boolean): Int = jvmMethod("scala.collection.GenSeqLike.indexWhere(Lscala/Function1;)I").invoke(this, p)
      |def indexOf[B >: Int](elem: B): Int = jvmMethod("scala.collection.GenSeqLike.indexOf(Ljava/lang/Object;)I").invoke(this, elem)
      |def indexOf[B >: Int](elem: B, from: Int): Int = jvmMethod("scala.collection.GenSeqLike.indexOf(Ljava/lang/Object;I)I").invoke(this, elem, from)
      |def lastIndexOf[B >: Int](elem: B): Int = jvmMethod("scala.collection.GenSeqLike.lastIndexOf(Ljava/lang/Object;)I").invoke(this, elem)
      |def lastIndexOf[B >: Int](elem: B, end: Int): Int = jvmMethod("scala.collection.GenSeqLike.lastIndexOf(Ljava/lang/Object;I)I").invoke(this, elem, end)
      |def lastIndexWhere(p: Int => Boolean): Int = jvmMethod("scala.collection.GenSeqLike.lastIndexWhere(Lscala/Function1;)I").invoke(this, p)
      |def startsWith[B](that: GenSeq[B]): Boolean = jvmMethod("scala.collection.GenSeqLike.startsWith(Lscala/collection/GenSeq;)Z").invoke(this, that)
      |override def equals(that: Any): Boolean = jvmMethod("scala.collection.GenSeqLike.equals(Ljava/lang/Object;)Z").invoke(this, that)
      |def orElse[A1 <: Int, B1 >: Int](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = jvmMethod("PartialFunction.orElse(Lscala/PartialFunction;)Lscala/PartialFunction;").invoke(this, that)
      |override def andThen[C](k: Int => C): PartialFunction[Int, C] = jvmMethod("PartialFunction.andThen(Lscala/Function1;)Lscala/PartialFunction;").invoke(this, k)
      |def lift: Int => Option[Int] = jvmMethod("PartialFunction.lift()Lscala/Function1;").invoke(this)
      |def applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: A1 => B1): B1 = jvmMethod("PartialFunction.applyOrElse(Ljava/lang/Object;Lscala/Function1;)Ljava/lang/Object;").invoke(this, x, default)
      |def runWith[U](action: Int => U): Int => Boolean = jvmMethod("PartialFunction.runWith(Lscala/Function1;)Lscala/Function1;").invoke(this, action)
      |@unspecialized def compose[A](g: A => Int): A => Int = jvmMethod("Function1.compose(Lscala/Function1;)Lscala/Function1;").invoke(this, g)
      |override def toIterable: Iterable[Int] = jvmMethod("scala.collection.IterableLike.toIterable()Lscala/collection/Iterable;").invoke(this)
      |@deprecatedOverriding("toIterator should stay consistent with iterator for all Iterables: override iterator instead.", "2.11.0") override def toIterator: Iterator[Int] = jvmMethod("scala.collection.IterableLike.toIterator()Lscala/collection/Iterator;").invoke(this)
      |override def head: Int = jvmMethod("scala.collection.IterableLike.head()Ljava/lang/Object;").invoke(this)
      |def grouped(size: Int): Iterator[List[Int]] = jvmMethod("scala.collection.IterableLike.grouped(I)Lscala/collection/Iterator;").invoke(this, size)
      |def sliding(size: Int): Iterator[List[Int]] = jvmMethod("scala.collection.IterableLike.sliding(I)Lscala/collection/Iterator;").invoke(this, size)
      |def sliding(size: Int, step: Int): Iterator[List[Int]] = jvmMethod("scala.collection.IterableLike.sliding(II)Lscala/collection/Iterator;").invoke(this, size, step)
      |override def copyToArray[B >: Int](xs: Array[B], start: Int, len: Int): Unit = jvmMethod("scala.collection.IterableLike.copyToArray(Ljava/lang/Object;II)V").invoke(this, xs, start, len)
      |def zip[A1 >: Int, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[List[Int], (A1, B), That]): That = jvmMethod("scala.collection.IterableLike.zip(Lscala/collection/GenIterable;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, bf)
      |def zipAll[B, A1 >: Int, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[List[Int], (A1, B), That]): That = jvmMethod("scala.collection.IterableLike.zipAll(Lscala/collection/GenIterable;Ljava/lang/Object;Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, thisElem, thatElem, bf)
      |def zipWithIndex[A1 >: Int, That](implicit bf: CanBuildFrom[List[Int], (A1, Int), That]): That = jvmMethod("scala.collection.IterableLike.zipWithIndex(Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, bf)
      |override def canEqual(that: Any): Boolean = jvmMethod("scala.collection.IterableLike.canEqual(Ljava/lang/Object;)Z").invoke(this, that)
      |protected[this] def newBuilder: Builder[Int, List[Int]] = jvmMethod("scala.collection.generic.GenericTraversableTemplate.newBuilder()Lscala/collection/mutable/Builder;").invoke(this)
      |def genericBuilder[B]: Builder[B, List[B]] = jvmMethod("scala.collection.generic.GenericTraversableTemplate.genericBuilder()Lscala/collection/mutable/Builder;").invoke(this)
      |def unzip[A1, A2](implicit asPair: Int => (A1, A2)): (List[A1], List[A2]) = jvmMethod("scala.collection.generic.GenericTraversableTemplate.unzip(Lscala/Function1;)Lscala/Tuple2;").invoke(this, asPair)
      |def unzip3[A1, A2, A3](implicit asTriple: Int => (A1, A2, A3)): (List[A1], List[A2], List[A3]) = jvmMethod("scala.collection.generic.GenericTraversableTemplate.unzip3(Lscala/Function1;)Lscala/Tuple3;").invoke(this, asTriple)
      |def flatten[B](implicit asTraversable: Int => GenTraversableOnce[B]): List[B] = jvmMethod("scala.collection.generic.GenericTraversableTemplate.flatten(Lscala/Function1;)Lscala/collection/GenTraversable;").invoke(this, asTraversable)
      |@migration("`transpose` throws an `IllegalArgumentException` if collections are not uniformly sized.", "2.9.0") def transpose[B](implicit asTraversable: Int => GenTraversableOnce[B]): List[List[B]] = jvmMethod("scala.collection.generic.GenericTraversableTemplate.transpose(Lscala/Function1;)Lscala/collection/GenTraversable;").invoke(this, asTraversable)
      |protected[this] type Self = List[Int]
      |def repr: List[Int] = jvmMethod("scala.collection.TraversableLike.repr()Ljava/lang/Object;").invoke(this)
      |final def isTraversableAgain: Boolean = jvmMethod("scala.collection.TraversableLike.isTraversableAgain()Z").invoke(this)
      |def hasDefiniteSize: Boolean = jvmMethod("scala.collection.TraversableLike.hasDefiniteSize()Z").invoke(this)
      |def ++:[B >: Int, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.TraversableLike.$plus$plus$colon(Lscala/collection/TraversableOnce;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, bf)
      |def ++:[B >: Int, That](that: Traversable[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.TraversableLike.$plus$plus$colon(Lscala/collection/Traversable;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, that, bf)
      |def filter(p: Int => Boolean): List[Int] = jvmMethod("scala.collection.TraversableLike.filter(Lscala/Function1;)Ljava/lang/Object;").invoke(this, p)
      |def filterNot(p: Int => Boolean): List[Int] = jvmMethod("scala.collection.TraversableLike.filterNot(Lscala/Function1;)Ljava/lang/Object;").invoke(this, p)
      |def partition(p: Int => Boolean): (List[Int], List[Int]) = jvmMethod("scala.collection.TraversableLike.partition(Lscala/Function1;)Lscala/Tuple2;").invoke(this, p)
      |def groupBy[K](f: Int => K): Map[K, List[Int]] = jvmMethod("scala.collection.TraversableLike.groupBy(Lscala/Function1;)Lscala/collection/immutable/Map;").invoke(this, f)
      |def scan[B >: Int, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.TraversableLike.scan(Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, z, op, cbf)
      |def scanLeft[B, That](z: B)(op: (B, Int) => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.TraversableLike.scanLeft(Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, z, op, bf)
      |@migration("The behavior of `scanRight` has changed. The previous behavior can be reproduced with scanRight.reverse.", "2.9.0") def scanRight[B, That](z: B)(op: (Int, B) => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = jvmMethod("scala.collection.TraversableLike.scanRight(Ljava/lang/Object;Lscala/Function2;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, z, op, bf)
      |def headOption: Option[Int] = jvmMethod("scala.collection.TraversableLike.headOption()Lscala/Option;").invoke(this)
      |override def tail: List[Int] = jvmMethod("scala.collection.TraversableLike.tail()Ljava/lang/Object;").invoke(this)
      |def lastOption: Option[Int] = jvmMethod("scala.collection.TraversableLike.lastOption()Lscala/Option;").invoke(this)
      |def init: List[Int] = jvmMethod("scala.collection.TraversableLike.init()Ljava/lang/Object;").invoke(this)
      |private[scala] def sliceWithKnownDelta(from: Int, until: Int, delta: Int): List[Int] = jvmMethod("scala.collection.TraversableLike.sliceWithKnownDelta(III)Ljava/lang/Object;").invoke(this, from, until, delta)
      |private[scala] def sliceWithKnownBound(from: Int, until: Int): List[Int] = jvmMethod("scala.collection.TraversableLike.sliceWithKnownBound(II)Ljava/lang/Object;").invoke(this, from, until)
      |def tails: Iterator[List[Int]] = jvmMethod("scala.collection.TraversableLike.tails()Lscala/collection/Iterator;").invoke(this)
      |def inits: Iterator[List[Int]] = jvmMethod("scala.collection.TraversableLike.inits()Lscala/collection/Iterator;").invoke(this)
      |@deprecatedOverriding("Enforce contract of toTraversable that if it is Traversable it returns itself.", "2.11.0") def toTraversable: Traversable[Int] = jvmMethod("scala.collection.TraversableLike.toTraversable()Lscala/collection/Traversable;").invoke(this)
      |override def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, Int, Col[Int]]): Col[Int] = jvmMethod("scala.collection.TraversableLike.to(Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;").invoke(this, cbf)
      |def withFilter(p: Int => Boolean): FilterMonadic[Int, List[Int]] = jvmMethod("scala.collection.TraversableLike.withFilter(Lscala/Function1;)Lscala/collection/generic/FilterMonadic;").invoke(this, p)
      |class WithFilter(p: A => Boolean) extends AnyRef with FilterMonadic[A, Repr] { ... }
      |def par: ParSeq[Int] = jvmMethod("scala.collection.Parallelizable.par()Lscala/collection/Parallel;").invoke(this)
      |protected[this] def reversed: List[Int] = jvmMethod("scala.collection.TraversableOnce.reversed()Lscala/collection/immutable/List;").invoke(this)
      |def nonEmpty: Boolean = jvmMethod("scala.collection.TraversableOnce.nonEmpty()Z").invoke(this)
      |def count(p: Int => Boolean): Int = jvmMethod("scala.collection.TraversableOnce.count(Lscala/Function1;)I").invoke(this, p)
      |def collectFirst[B](pf: PartialFunction[Int, B]): Option[B] = jvmMethod("scala.collection.TraversableOnce.collectFirst(Lscala/PartialFunction;)Lscala/Option;").invoke(this, pf)
      |def /:[B](z: B)(op: (B, Int) => B): B = jvmMethod("scala.collection.TraversableOnce.$div$colon(Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, op)
      |def :\[B](z: B)(op: (Int, B) => B): B = jvmMethod("scala.collection.TraversableOnce.$colon$bslash(Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, op)
      |def reduceLeftOption[B >: Int](op: (B, Int) => B): Option[B] = jvmMethod("scala.collection.TraversableOnce.reduceLeftOption(Lscala/Function2;)Lscala/Option;").invoke(this, op)
      |def reduceRightOption[B >: Int](op: (Int, B) => B): Option[B] = jvmMethod("scala.collection.TraversableOnce.reduceRightOption(Lscala/Function2;)Lscala/Option;").invoke(this, op)
      |def reduce[A1 >: Int](op: (A1, A1) => A1): A1 = jvmMethod("scala.collection.TraversableOnce.reduce(Lscala/Function2;)Ljava/lang/Object;").invoke(this, op)
      |def reduceOption[A1 >: Int](op: (A1, A1) => A1): Option[A1] = jvmMethod("scala.collection.TraversableOnce.reduceOption(Lscala/Function2;)Lscala/Option;").invoke(this, op)
      |def fold[A1 >: Int](z: A1)(op: (A1, A1) => A1): A1 = jvmMethod("scala.collection.TraversableOnce.fold(Ljava/lang/Object;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, op)
      |def aggregate[B](z: => B)(seqop: (B, Int) => B, combop: (B, B) => B): B = jvmMethod("scala.collection.TraversableOnce.aggregate(Lscala/Function0;Lscala/Function2;Lscala/Function2;)Ljava/lang/Object;").invoke(this, z, seqop, combop)
      |def sum[B >: Int](implicit num: Numeric[B]): B = jvmMethod("scala.collection.TraversableOnce.sum(Lscala/math/Numeric;)Ljava/lang/Object;").invoke(this, num)
      |def product[B >: Int](implicit num: Numeric[B]): B = jvmMethod("scala.collection.TraversableOnce.product(Lscala/math/Numeric;)Ljava/lang/Object;").invoke(this, num)
      |def min[B >: Int](implicit cmp: Ordering[B]): Int = jvmMethod("scala.collection.TraversableOnce.min(Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, cmp)
      |def max[B >: Int](implicit cmp: Ordering[B]): Int = jvmMethod("scala.collection.TraversableOnce.max(Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, cmp)
      |def maxBy[B](f: Int => B)(implicit cmp: Ordering[B]): Int = jvmMethod("scala.collection.TraversableOnce.maxBy(Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, f, cmp)
      |def minBy[B](f: Int => B)(implicit cmp: Ordering[B]): Int = jvmMethod("scala.collection.TraversableOnce.minBy(Lscala/Function1;Lscala/math/Ordering;)Ljava/lang/Object;").invoke(this, f, cmp)
      |def copyToBuffer[B >: Int](dest: Buffer[B]): Unit = jvmMethod("scala.collection.TraversableOnce.copyToBuffer(Lscala/collection/mutable/Buffer;)V").invoke(this, dest)
      |def copyToArray[B >: Int](xs: Array[B], start: Int): Unit = jvmMethod("scala.collection.TraversableOnce.copyToArray(Ljava/lang/Object;I)V").invoke(this, xs, start)
      |def copyToArray[B >: Int](xs: Array[B]): Unit = jvmMethod("scala.collection.TraversableOnce.copyToArray(Ljava/lang/Object;)V").invoke(this, xs)
      |def toArray[B >: Int: ClassTag](implicit evidence$1: ClassTag[B]): Array[B] = jvmMethod("scala.collection.TraversableOnce.toArray(Lscala/reflect/ClassTag;)Ljava/lang/Object;").invoke(this, evidence$1)
      |def toIndexedSeq: IndexedSeq[Int] = jvmMethod("scala.collection.TraversableOnce.toIndexedSeq()Lscala/collection/immutable/IndexedSeq;").invoke(this)
      |def toBuffer[B >: Int]: Buffer[B] = jvmMethod("scala.collection.TraversableOnce.toBuffer()Lscala/collection/mutable/Buffer;").invoke(this)
      |def toSet[B >: Int]: Set[B] = jvmMethod("scala.collection.TraversableOnce.toSet()Lscala/collection/immutable/Set;").invoke(this)
      |def toVector: Vector[Int] = jvmMethod("scala.collection.TraversableOnce.toVector()Lscala/collection/immutable/Vector;").invoke(this)
      |def toMap[T, U](implicit ev: Int <:< (T, U)): Map[T, U] = jvmMethod("scala.collection.TraversableOnce.toMap(Lscala/Predef/$less$colon$less;)Lscala/collection/immutable/Map;").invoke(this, ev)
      |def mkString(start: String, sep: String, end: String): String = jvmMethod("scala.collection.TraversableOnce.mkString(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;").invoke(this, start, sep, end)
      |def mkString(sep: String): String = jvmMethod("scala.collection.TraversableOnce.mkString(Ljava/lang/String;)Ljava/lang/String;").invoke(this, sep)
      |def mkString: String = jvmMethod("scala.collection.TraversableOnce.mkString()Ljava/lang/String;").invoke(this)
      |def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = jvmMethod("scala.collection.TraversableOnce.addString(Lscala/collection/mutable/StringBuilder;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Lscala/collection/mutable/StringBuilder;").invoke(this, b, start, sep, end)
      |def addString(b: StringBuilder, sep: String): StringBuilder = jvmMethod("scala.collection.TraversableOnce.addString(Lscala/collection/mutable/StringBuilder;Ljava/lang/String;)Lscala/collection/mutable/StringBuilder;").invoke(this, b, sep)
      |def addString(b: StringBuilder): StringBuilder = jvmMethod("scala.collection.TraversableOnce.addString(Lscala/collection/mutable/StringBuilder;)Lscala/collection/mutable/StringBuilder;").invoke(this, b)
      |final def getClass(): Class[?0] forSome { type ?0 } = intrinsic("Object.getClass()Ljava/lang/Class;", this)
      |@throws[CloneNotSupportedException] protected[lang] def clone(): Object = jvmMethod("Object.clone()Ljava/lang/Object;").invoke(this)
      |final def notify(): Unit = jvmMethod("Object.notify()V").invoke(this)
      |final def notifyAll(): Unit = jvmMethod("Object.notifyAll()V").invoke(this)
      |@throws[InterruptedException] final def wait(x$1: Long): Unit = jvmMethod("Object.wait(J)V").invoke(this, x$1)
      |@throws[InterruptedException] final def wait(x$1: Long, x$2: Int): Unit = jvmMethod("Object.wait(JI)V").invoke(this, x$1, x$2)
      |@throws[InterruptedException] final def wait(): Unit = jvmMethod("Object.wait()V").invoke(this)
      |@throws[Throwable] protected[lang] def finalize(): Unit = jvmMethod("Object.finalize()V").invoke(this)
      |final def eq(x$1: AnyRef): Boolean = intrinsic("Object.eq(Ljava/lang/Object;)Z", this, x$1)
      |final def ne(x$1: AnyRef): Boolean = intrinsic("Object.ne(Ljava/lang/Object;)Z", this, x$1)
      |final def ==(x$1: Any): Boolean = intrinsic("Object.$eq$eq(Ljava/lang/Object;)Z", this, x$1)
      |final def !=(x$1: Any): Boolean = intrinsic("Object.$bang$eq(Ljava/lang/Object;)Z", this, x$1)
      |final def ##(): Int = intrinsic("Object.$hash$hash()I", this)
      |final def synchronized[T0](x$1: T0): T0 = intrinsic("Object.synchronized(Ljava/lang/Object;)Ljava/lang/Object;", this, x$1)
      |final def isInstanceOf[T0]: Boolean = intrinsic("Object.isInstanceOf()Z", this)
      |final def asInstanceOf[T0]: T0 = intrinsic("Object.asInstanceOf()Ljava/lang/Object;", this)
    """.trim.stripMargin)
  }

  test("t\"scala.compat.Platform.type\".members") {
    assert(t"scala.compat.Platform.type".members.mkString(EOL) === """
      |def this()
      |type StackOverflowError = StackOverflowError
      |type ConcurrentModificationException = ConcurrentModificationException
      |@inline def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit = jvmMethod("scala.compat.Platform.type.arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V").invoke(this, src, srcPos, dest, destPos, length)
      |@inline def createArray(elemClass: Class[_$1] forSome { type _$1 }, length: Int): AnyRef = jvmMethod("scala.compat.Platform.type.createArray(Ljava/lang/Class;I)Ljava/lang/Object;").invoke(this, elemClass, length)
      |@inline def arrayclear(arr: Array[Int]): Unit = jvmMethod("scala.compat.Platform.type.arrayclear([I)V").invoke(this, arr)
      |@inline def getClassForName(name: String): Class[_$2] forSome { type _$2 } = jvmMethod("scala.compat.Platform.type.getClassForName(Ljava/lang/String;)Ljava/lang/Class;").invoke(this, name)
      |EOL
      |@inline def currentTime: Long = jvmMethod("scala.compat.Platform.type.currentTime()J").invoke(this)
      |@inline def collectGarbage(): Unit = jvmMethod("scala.compat.Platform.type.collectGarbage()V").invoke(this)
      |@inline def defaultCharsetName: String = jvmMethod("scala.compat.Platform.type.defaultCharsetName()Ljava/lang/String;").invoke(this)
      |final def getClass(): Class[?0] forSome { type ?0 } = intrinsic("Object.getClass()Ljava/lang/Class;", this)
      |def hashCode(): Int = jvmMethod("Object.hashCode()I").invoke(this)
      |def equals(x$1: Any): Boolean = jvmMethod("Object.equals(Ljava/lang/Object;)Z").invoke(this, x$1)
      |@throws[CloneNotSupportedException] protected[lang] def clone(): Object = jvmMethod("Object.clone()Ljava/lang/Object;").invoke(this)
      |def toString(): String = jvmMethod("Object.toString()Ljava/lang/String;").invoke(this)
      |final def notify(): Unit = jvmMethod("Object.notify()V").invoke(this)
      |final def notifyAll(): Unit = jvmMethod("Object.notifyAll()V").invoke(this)
      |@throws[InterruptedException] final def wait(x$1: Long): Unit = jvmMethod("Object.wait(J)V").invoke(this, x$1)
      |@throws[InterruptedException] final def wait(x$1: Long, x$2: Int): Unit = jvmMethod("Object.wait(JI)V").invoke(this, x$1, x$2)
      |@throws[InterruptedException] final def wait(): Unit = jvmMethod("Object.wait()V").invoke(this)
      |@throws[Throwable] protected[lang] def finalize(): Unit = jvmMethod("Object.finalize()V").invoke(this)
      |final def eq(x$1: AnyRef): Boolean = intrinsic("Object.eq(Ljava/lang/Object;)Z", this, x$1)
      |final def ne(x$1: AnyRef): Boolean = intrinsic("Object.ne(Ljava/lang/Object;)Z", this, x$1)
      |final def ==(x$1: Any): Boolean = intrinsic("Object.$eq$eq(Ljava/lang/Object;)Z", this, x$1)
      |final def !=(x$1: Any): Boolean = intrinsic("Object.$bang$eq(Ljava/lang/Object;)Z", this, x$1)
      |final def ##(): Int = intrinsic("Object.$hash$hash()I", this)
      |final def synchronized[T0](x$1: T0): T0 = intrinsic("Object.synchronized(Ljava/lang/Object;)Ljava/lang/Object;", this, x$1)
      |final def isInstanceOf[T0]: Boolean = intrinsic("Object.isInstanceOf()Z", this)
      |final def asInstanceOf[T0]: T0 = intrinsic("Object.asInstanceOf()Ljava/lang/Object;", this)
    """.trim.stripMargin)
  }

  test("t\"scala.type\".members") {
    intercept[SemanticException] {
      val expectedFail = "Input scala.meta tree is not fully attributed and can't be converted to a scala.reflect artifact."
      try t"scala.type".members
      catch { case ex: SemanticException => assert(ex.msg.trim.startsWith(expectedFail)); throw ex }
    }
  }

  test("t\"scala.collection.immutable.List\".defn") {
    assert(t"scala.collection.immutable.List".defn.toString === """
      |@SerialVersionUID(value = -6084104484083858598L) sealed abstract class List[+A]() extends AbstractSeq[A] with LinearSeq[A] with Product with GenericTraversableTemplate[A, List] with LinearSeqOptimized[A, List[A]] with Serializable { ... }
    """.trim.stripMargin)
  }

  test("t\"SemanticDummy\".defn") {
    assert(t"SemanticDummy".defn.show[Code] === """
      |class SemanticDummy() extends AnyRef {
      |  private[this] val x: Int = jvmField("SemanticDummy.x:I").get(this)
      |  val y: Int = jvmMethod("SemanticDummy.y()I").invoke(this)
      |  def foo(w: Int = jvmMethod("SemanticDummy.foo$default$1()I").invoke(this)): Int = jvmMethod("SemanticDummy.foo(I)I").invoke(this, w)
      |}
    """.trim.stripMargin)
  }

  test("t\"Int\".defn") {
    assert(t"Int".defn.show[Code] === """
      |final abstract class Int() extends AnyVal {
      |  def toByte: Byte = intrinsic("Int.toByte()B", this)
      |  def toShort: Short = intrinsic("Int.toShort()S", this)
      |  def toChar: Char = intrinsic("Int.toChar()C", this)
      |  def toInt: Int = intrinsic("Int.toInt()I", this)
      |  def toLong: Long = intrinsic("Int.toLong()J", this)
      |  def toFloat: Float = intrinsic("Int.toFloat()F", this)
      |  def toDouble: Double = intrinsic("Int.toDouble()D", this)
      |  def unary_~ : Int = intrinsic("Int.unary_$tilde()I", this)
      |  def unary_+ : Int = intrinsic("Int.unary_$plus()I", this)
      |  def unary_- : Int = intrinsic("Int.unary_$minus()I", this)
      |  def +(x: String): String = intrinsic("Int.$plus(Ljava/lang/String;)Ljava/lang/String;", this, x)
      |  def <<(x: Int): Int = intrinsic("Int.$less$less(I)I", this, x)
      |  def <<(x: Long): Int = intrinsic("Int.$less$less(J)I", this, x)
      |  def >>>(x: Int): Int = intrinsic("Int.$greater$greater$greater(I)I", this, x)
      |  def >>>(x: Long): Int = intrinsic("Int.$greater$greater$greater(J)I", this, x)
      |  def >>(x: Int): Int = intrinsic("Int.$greater$greater(I)I", this, x)
      |  def >>(x: Long): Int = intrinsic("Int.$greater$greater(J)I", this, x)
      |  def ==(x: Byte): Boolean = intrinsic("Int.$eq$eq(B)Z", this, x)
      |  def ==(x: Short): Boolean = intrinsic("Int.$eq$eq(S)Z", this, x)
      |  def ==(x: Char): Boolean = intrinsic("Int.$eq$eq(C)Z", this, x)
      |  def ==(x: Int): Boolean = intrinsic("Int.$eq$eq(I)Z", this, x)
      |  def ==(x: Long): Boolean = intrinsic("Int.$eq$eq(J)Z", this, x)
      |  def ==(x: Float): Boolean = intrinsic("Int.$eq$eq(F)Z", this, x)
      |  def ==(x: Double): Boolean = intrinsic("Int.$eq$eq(D)Z", this, x)
      |  def !=(x: Byte): Boolean = intrinsic("Int.$bang$eq(B)Z", this, x)
      |  def !=(x: Short): Boolean = intrinsic("Int.$bang$eq(S)Z", this, x)
      |  def !=(x: Char): Boolean = intrinsic("Int.$bang$eq(C)Z", this, x)
      |  def !=(x: Int): Boolean = intrinsic("Int.$bang$eq(I)Z", this, x)
      |  def !=(x: Long): Boolean = intrinsic("Int.$bang$eq(J)Z", this, x)
      |  def !=(x: Float): Boolean = intrinsic("Int.$bang$eq(F)Z", this, x)
      |  def !=(x: Double): Boolean = intrinsic("Int.$bang$eq(D)Z", this, x)
      |  def <(x: Byte): Boolean = intrinsic("Int.$less(B)Z", this, x)
      |  def <(x: Short): Boolean = intrinsic("Int.$less(S)Z", this, x)
      |  def <(x: Char): Boolean = intrinsic("Int.$less(C)Z", this, x)
      |  def <(x: Int): Boolean = intrinsic("Int.$less(I)Z", this, x)
      |  def <(x: Long): Boolean = intrinsic("Int.$less(J)Z", this, x)
      |  def <(x: Float): Boolean = intrinsic("Int.$less(F)Z", this, x)
      |  def <(x: Double): Boolean = intrinsic("Int.$less(D)Z", this, x)
      |  def <=(x: Byte): Boolean = intrinsic("Int.$less$eq(B)Z", this, x)
      |  def <=(x: Short): Boolean = intrinsic("Int.$less$eq(S)Z", this, x)
      |  def <=(x: Char): Boolean = intrinsic("Int.$less$eq(C)Z", this, x)
      |  def <=(x: Int): Boolean = intrinsic("Int.$less$eq(I)Z", this, x)
      |  def <=(x: Long): Boolean = intrinsic("Int.$less$eq(J)Z", this, x)
      |  def <=(x: Float): Boolean = intrinsic("Int.$less$eq(F)Z", this, x)
      |  def <=(x: Double): Boolean = intrinsic("Int.$less$eq(D)Z", this, x)
      |  def >(x: Byte): Boolean = intrinsic("Int.$greater(B)Z", this, x)
      |  def >(x: Short): Boolean = intrinsic("Int.$greater(S)Z", this, x)
      |  def >(x: Char): Boolean = intrinsic("Int.$greater(C)Z", this, x)
      |  def >(x: Int): Boolean = intrinsic("Int.$greater(I)Z", this, x)
      |  def >(x: Long): Boolean = intrinsic("Int.$greater(J)Z", this, x)
      |  def >(x: Float): Boolean = intrinsic("Int.$greater(F)Z", this, x)
      |  def >(x: Double): Boolean = intrinsic("Int.$greater(D)Z", this, x)
      |  def >=(x: Byte): Boolean = intrinsic("Int.$greater$eq(B)Z", this, x)
      |  def >=(x: Short): Boolean = intrinsic("Int.$greater$eq(S)Z", this, x)
      |  def >=(x: Char): Boolean = intrinsic("Int.$greater$eq(C)Z", this, x)
      |  def >=(x: Int): Boolean = intrinsic("Int.$greater$eq(I)Z", this, x)
      |  def >=(x: Long): Boolean = intrinsic("Int.$greater$eq(J)Z", this, x)
      |  def >=(x: Float): Boolean = intrinsic("Int.$greater$eq(F)Z", this, x)
      |  def >=(x: Double): Boolean = intrinsic("Int.$greater$eq(D)Z", this, x)
      |  def |(x: Byte): Int = intrinsic("Int.$bar(B)I", this, x)
      |  def |(x: Short): Int = intrinsic("Int.$bar(S)I", this, x)
      |  def |(x: Char): Int = intrinsic("Int.$bar(C)I", this, x)
      |  def |(x: Int): Int = intrinsic("Int.$bar(I)I", this, x)
      |  def |(x: Long): Long = intrinsic("Int.$bar(J)J", this, x)
      |  def &(x: Byte): Int = intrinsic("Int.$amp(B)I", this, x)
      |  def &(x: Short): Int = intrinsic("Int.$amp(S)I", this, x)
      |  def &(x: Char): Int = intrinsic("Int.$amp(C)I", this, x)
      |  def &(x: Int): Int = intrinsic("Int.$amp(I)I", this, x)
      |  def &(x: Long): Long = intrinsic("Int.$amp(J)J", this, x)
      |  def ^(x: Byte): Int = intrinsic("Int.$up(B)I", this, x)
      |  def ^(x: Short): Int = intrinsic("Int.$up(S)I", this, x)
      |  def ^(x: Char): Int = intrinsic("Int.$up(C)I", this, x)
      |  def ^(x: Int): Int = intrinsic("Int.$up(I)I", this, x)
      |  def ^(x: Long): Long = intrinsic("Int.$up(J)J", this, x)
      |  def +(x: Byte): Int = intrinsic("Int.$plus(B)I", this, x)
      |  def +(x: Short): Int = intrinsic("Int.$plus(S)I", this, x)
      |  def +(x: Char): Int = intrinsic("Int.$plus(C)I", this, x)
      |  def +(x: Int): Int = intrinsic("Int.$plus(I)I", this, x)
      |  def +(x: Long): Long = intrinsic("Int.$plus(J)J", this, x)
      |  def +(x: Float): Float = intrinsic("Int.$plus(F)F", this, x)
      |  def +(x: Double): Double = intrinsic("Int.$plus(D)D", this, x)
      |  def -(x: Byte): Int = intrinsic("Int.$minus(B)I", this, x)
      |  def -(x: Short): Int = intrinsic("Int.$minus(S)I", this, x)
      |  def -(x: Char): Int = intrinsic("Int.$minus(C)I", this, x)
      |  def -(x: Int): Int = intrinsic("Int.$minus(I)I", this, x)
      |  def -(x: Long): Long = intrinsic("Int.$minus(J)J", this, x)
      |  def -(x: Float): Float = intrinsic("Int.$minus(F)F", this, x)
      |  def -(x: Double): Double = intrinsic("Int.$minus(D)D", this, x)
      |  def *(x: Byte): Int = intrinsic("Int.$times(B)I", this, x)
      |  def *(x: Short): Int = intrinsic("Int.$times(S)I", this, x)
      |  def *(x: Char): Int = intrinsic("Int.$times(C)I", this, x)
      |  def *(x: Int): Int = intrinsic("Int.$times(I)I", this, x)
      |  def *(x: Long): Long = intrinsic("Int.$times(J)J", this, x)
      |  def *(x: Float): Float = intrinsic("Int.$times(F)F", this, x)
      |  def *(x: Double): Double = intrinsic("Int.$times(D)D", this, x)
      |  def /(x: Byte): Int = intrinsic("Int.$div(B)I", this, x)
      |  def /(x: Short): Int = intrinsic("Int.$div(S)I", this, x)
      |  def /(x: Char): Int = intrinsic("Int.$div(C)I", this, x)
      |  def /(x: Int): Int = intrinsic("Int.$div(I)I", this, x)
      |  def /(x: Long): Long = intrinsic("Int.$div(J)J", this, x)
      |  def /(x: Float): Float = intrinsic("Int.$div(F)F", this, x)
      |  def /(x: Double): Double = intrinsic("Int.$div(D)D", this, x)
      |  def %(x: Byte): Int = intrinsic("Int.$percent(B)I", this, x)
      |  def %(x: Short): Int = intrinsic("Int.$percent(S)I", this, x)
      |  def %(x: Char): Int = intrinsic("Int.$percent(C)I", this, x)
      |  def %(x: Int): Int = intrinsic("Int.$percent(I)I", this, x)
      |  def %(x: Long): Long = intrinsic("Int.$percent(J)J", this, x)
      |  def %(x: Float): Float = intrinsic("Int.$percent(F)F", this, x)
      |  def %(x: Double): Double = intrinsic("Int.$percent(D)D", this, x)
      |  override def getClass(): Class[Int] = intrinsic("Int.getClass()Ljava/lang/Class;", this)
      |}
    """.trim.stripMargin)
  }

  test("q\"scala\".defn.members") {
    assert(q"scala".defn.members.sortBy(mem => mem.name.toString + mem.internalTag).mkString(EOL) === """
      |#::
      |+:
      |:+
      |::
      |type ::[A] = ::[A]
      |class <byname>[+T0] extends AnyRef with Any { ... }
      |class <repeated...>[+T0] extends AnyRef with Array[T0] { ... }
      |class <repeated>[+T0] extends AnyRef with Seq[T0] { ... }
      |type AbstractMethodError = AbstractMethodError
      |abstract class Any { ... }
      |AnyRef
      |type AnyRef = Object
      |abstract class AnyVal() extends Any { ... }
      |private[scala] trait AnyValCompanion extends AnyRef with Specializable { ... }
      |trait App extends AnyRef with DelayedInit { ... }
      |final class Array[T](_length: Int) extends AnyRef with Serializable with Cloneable { ... }
      |object Array extends FallbackArrayBuilding with Serializable { ... }
      |type ArrayIndexOutOfBoundsException = ArrayIndexOutOfBoundsException
      |BigDecimal
      |type BigDecimal = BigDecimal
      |BigInt
      |type BigInt = BigInt
      |final abstract class Boolean() extends AnyVal { ... }
      |object Boolean extends AnyRef with AnyValCompanion { ... }
      |type BufferedIterator[+A] = BufferedIterator[A]
      |final abstract class Byte() extends AnyVal { ... }
      |object Byte extends AnyRef with AnyValCompanion { ... }
      |final abstract class Char() extends AnyVal { ... }
      |object Char extends AnyRef with AnyValCompanion { ... }
      |type ClassCastException = ClassCastException
      |trait Cloneable extends Object with Cloneable { ... }
      |object Console extends DeprecatedConsole with AnsiColor { ... }
      |@deprecated(QQQDelayedInit semantics can be surprising. Support for `App` will continue.
      |See the release notes for more details: https://github.com/scala/scala/releases/tag/v2.11.0-RC1QQQ, "2.11.0") @deprecated("see corresponding Javadoc for more information.", "") trait DelayedInit extends AnyRef { ... }
      |private[scala] abstract class DeprecatedConsole() extends AnyRef { _: Console.type => ... }
      |private[scala] trait DeprecatedPredef extends AnyRef { _: Predef.type => ... }
      |final abstract class Double() extends AnyVal { ... }
      |object Double extends AnyRef with AnyValCompanion { ... }
      |trait Dynamic extends Any { ... }
      |Either
      |type Either[+A, +B] = Either[A, B]
      |@SerialVersionUID(value = 8476000850333817230L) abstract class Enumeration(initial: Int) extends AnyRef with Serializable { ... }
      |trait Equals extends Any { ... }
      |Equiv
      |type Equiv[T] = Equiv[T]
      |type Error = Error
      |type Exception = Exception
      |class FallbackArrayBuilding() extends AnyRef { ... }
      |final abstract class Float() extends AnyVal { ... }
      |object Float extends AnyRef with AnyValCompanion { ... }
      |Fractional
      |type Fractional[T] = Fractional[T]
      |trait Function0[@specialized(Specializable.Primitives) +R] extends AnyRef { _: () => R => ... }
      |trait Function10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R => ... }
      |trait Function11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R => ... }
      |trait Function12[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R => ... }
      |trait Function13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R => ... }
      |trait Function14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R => ... }
      |trait Function15[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R => ... }
      |trait Function16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R => ... }
      |trait Function17[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R => ... }
      |trait Function18[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R => ... }
      |@implicitNotFound("No implicit view available from ${T1} => ${R}.") trait Function1[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) -T1, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends AnyRef { _: T1 => R => ... }
      |trait Function19[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R => ... }
      |trait Function20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R => ... }
      |trait Function21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R => ... }
      |trait Function22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R => ... }
      |trait Function2[@specialized(scala.Int, scala.Long, scala.Double) -T1, @specialized(scala.Int, scala.Long, scala.Double) -T2, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends AnyRef { _: (T1, T2) => R => ... }
      |trait Function3[-T1, -T2, -T3, +R] extends AnyRef { _: (T1, T2, T3) => R => ... }
      |trait Function4[-T1, -T2, -T3, -T4, +R] extends AnyRef { _: (T1, T2, T3, T4) => R => ... }
      |trait Function5[-T1, -T2, -T3, -T4, -T5, +R] extends AnyRef { _: (T1, T2, T3, T4, T5) => R => ... }
      |trait Function6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6) => R => ... }
      |trait Function7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7) => R => ... }
      |trait Function8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8) => R => ... }
      |object Function extends AnyRef { ... }
      |trait Function9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] extends AnyRef { _: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R => ... }
      |type IllegalArgumentException = IllegalArgumentException
      |trait Immutable extends AnyRef { ... }
      |type IndexOutOfBoundsException = IndexOutOfBoundsException
      |IndexedSeq
      |type IndexedSeq[+A] = IndexedSeq[A]
      |final abstract class Int() extends AnyVal { ... }
      |object Int extends AnyRef with AnyValCompanion { ... }
      |Integral
      |type Integral[T] = Integral[T]
      |type InterruptedException = InterruptedException
      |Iterable
      |type Iterable[+A] = Iterable[A]
      |Iterator
      |type Iterator[+A] = Iterator[A]
      |Left
      |type Left[+A, +B] = Left[A, B]
      |List
      |type List[+A] = List[A]
      |final abstract class Long() extends AnyVal { ... }
      |object Long extends AnyRef with AnyValCompanion { ... }
      |private[scala] abstract class LowPriorityImplicits() extends AnyRef { ... }
      |final class MatchError(obj: Any) extends RuntimeException { ... }
      |trait Mutable extends AnyRef { ... }
      |Nil
      |type NoSuchElementException = NoSuchElementException
      |@SerialVersionUID(value = 5066590221178148012L) case object None extends Option[Nothing] with Product with Serializable { ... }
      |final class NotImplementedError(msg: String) extends Error { ... }
      |@deprecated("This trait will be removed", "2.11.0") @deprecated("see corresponding Javadoc for more information.", "") trait NotNull extends Any { ... }
      |final abstract class Nothing extends Any { ... }
      |final abstract class Null extends AnyRef { ... }
      |type NullPointerException = NullPointerException
      |type NumberFormatException = NumberFormatException
      |Numeric
      |type Numeric[T] = Numeric[T]
      |@SerialVersionUID(value = -114498752079829388L) sealed abstract class Option[+A]() extends AnyRef with Product with Serializable { _: Option[A] => ... }
      |object Option extends AnyRef with Serializable { ... }
      |Ordered
      |type Ordered[T] = Ordered[T]
      |Ordering
      |type Ordering[T] = Ordering[T]
      |trait PartialFunction[-A, +B] extends AnyRef with (A => B) { _: PartialFunction[A, B] => ... }
      |object PartialFunction extends AnyRef { ... }
      |type PartialOrdering[T] = PartialOrdering[T]
      |type PartiallyOrdered[T] = PartiallyOrdered[T]
      |object Predef extends LowPriorityImplicits with DeprecatedPredef { ... }
      |trait Product10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] extends Any with Product { ... }
      |object Product10 extends AnyRef { ... }
      |trait Product11[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11] extends Any with Product { ... }
      |object Product11 extends AnyRef { ... }
      |trait Product12[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12] extends Any with Product { ... }
      |object Product12 extends AnyRef { ... }
      |trait Product13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13] extends Any with Product { ... }
      |object Product13 extends AnyRef { ... }
      |trait Product14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14] extends Any with Product { ... }
      |object Product14 extends AnyRef { ... }
      |trait Product15[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15] extends Any with Product { ... }
      |object Product15 extends AnyRef { ... }
      |trait Product16[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16] extends Any with Product { ... }
      |object Product16 extends AnyRef { ... }
      |trait Product17[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17] extends Any with Product { ... }
      |object Product17 extends AnyRef { ... }
      |trait Product18[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18] extends Any with Product { ... }
      |object Product18 extends AnyRef { ... }
      |trait Product1[@specialized(Int, Long, Double) +T1] extends Any with Product { ... }
      |object Product1 extends AnyRef { ... }
      |trait Product19[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19] extends Any with Product { ... }
      |object Product19 extends AnyRef { ... }
      |trait Product20[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20] extends Any with Product { ... }
      |object Product20 extends AnyRef { ... }
      |trait Product21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21] extends Any with Product { ... }
      |object Product21 extends AnyRef { ... }
      |trait Product22[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21, +T22] extends Any with Product { ... }
      |object Product22 extends AnyRef { ... }
      |trait Product2[@specialized(Int, Long, Double) +T1, @specialized(Int, Long, Double) +T2] extends Any with Product { ... }
      |object Product2 extends AnyRef { ... }
      |trait Product3[+T1, +T2, +T3] extends Any with Product { ... }
      |object Product3 extends AnyRef { ... }
      |trait Product4[+T1, +T2, +T3, +T4] extends Any with Product { ... }
      |object Product4 extends AnyRef { ... }
      |trait Product5[+T1, +T2, +T3, +T4, +T5] extends Any with Product { ... }
      |object Product5 extends AnyRef { ... }
      |trait Product6[+T1, +T2, +T3, +T4, +T5, +T6] extends Any with Product { ... }
      |object Product6 extends AnyRef { ... }
      |trait Product7[+T1, +T2, +T3, +T4, +T5, +T6, +T7] extends Any with Product { ... }
      |object Product7 extends AnyRef { ... }
      |trait Product8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8] extends Any with Product { ... }
      |object Product8 extends AnyRef { ... }
      |trait Product extends Any with Equals { ... }
      |trait Product9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9] extends Any with Product { ... }
      |object Product9 extends AnyRef { ... }
      |trait Proxy extends Any { ... }
      |object Proxy extends AnyRef { ... }
      |Range
      |type Range = Range
      |@deprecated("This class will be removed", "2.11.0") @deprecated("see corresponding Javadoc for more information.", "") abstract class Responder[+A]() extends AnyRef with Serializable { ... }
      |@deprecated("This object will be removed", "2.11.0") object Responder extends AnyRef with Serializable { ... }
      |Right
      |type Right[+A, +B] = Right[A, B]
      |type RuntimeException = RuntimeException
      |case class ScalaReflectionException(msg: String) extends Exception with Product with Serializable { ... }
      |object ScalaReflectionException extends AbstractFunction1[String, ScalaReflectionException] with Serializable { ... }
      |Seq
      |type Seq[+A] = Seq[A]
      |class SerialVersionUID(value: Long) extends Annotation with ClassfileAnnotation { ... }
      |trait Serializable extends Any with Serializable { ... }
      |final abstract class Short() extends AnyVal { ... }
      |object Short extends AnyRef with AnyValCompanion { ... }
      |final trait Singleton extends Any { ... }
      |@SerialVersionUID(value = 1234815782226070388L) final case class Some[+A](x: A) extends Option[A] with Product with Serializable { ... }
      |object Some extends AnyRef with Serializable { ... }
      |trait Specializable extends AnyRef { ... }
      |object Specializable extends AnyRef { ... }
      |Stream
      |type Stream[+A] = Stream[A]
      |StringBuilder
      |type StringBuilder = StringBuilder
      |case class StringContext(parts: String*) extends AnyRef with Product with Serializable { ... }
      |object StringContext extends AnyRef with Serializable { ... }
      |type StringIndexOutOfBoundsException = StringIndexOutOfBoundsException
      |final class Symbol private (val name: String) extends AnyRef with Serializable { ... }
      |object Symbol extends UniquenessCache[String, Symbol] with Serializable { ... }
      |type Throwable = Throwable
      |Traversable
      |type Traversable[+A] = Traversable[A]
      |type TraversableOnce[+A] = TraversableOnce[A]
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10) extends AnyRef with Product10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] with Product with Serializable { ... }
      |object Tuple10 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple11[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11) extends AnyRef with Product11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] with Product with Serializable { ... }
      |object Tuple11 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple12[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12) extends AnyRef with Product12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] with Product with Serializable { ... }
      |object Tuple12 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13) extends AnyRef with Product13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] with Product with Serializable { ... }
      |object Tuple13 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14) extends AnyRef with Product14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] with Product with Serializable { ... }
      |object Tuple14 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple15[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15) extends AnyRef with Product15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] with Product with Serializable { ... }
      |object Tuple15 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple16[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16) extends AnyRef with Product16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] with Product with Serializable { ... }
      |object Tuple16 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple17[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17) extends AnyRef with Product17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] with Product with Serializable { ... }
      |object Tuple17 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple18[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18) extends AnyRef with Product18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] with Product with Serializable { ... }
      |object Tuple18 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple1[@specialized(Int, Long, Double) +T1](_1: T1) extends AnyRef with Product1[T1] with Product with Serializable { ... }
      |object Tuple1 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple19[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19) extends AnyRef with Product19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] with Product with Serializable { ... }
      |object Tuple19 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple20[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20) extends AnyRef with Product20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] with Product with Serializable { ... }
      |object Tuple20 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20, _21: T21) extends AnyRef with Product21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] with Product with Serializable { ... }
      |object Tuple21 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple22[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21, +T22](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20, _21: T21, _22: T22) extends AnyRef with Product22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] with Product with Serializable { ... }
      |object Tuple22 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple2[@specialized(Int, Long, Double, Char, Boolean) +T1, @specialized(Int, Long, Double, Char, Boolean) +T2](_1: T1, _2: T2) extends AnyRef with Product2[T1, T2] with Product with Serializable { ... }
      |object Tuple2 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple3[+T1, +T2, +T3](_1: T1, _2: T2, _3: T3) extends AnyRef with Product3[T1, T2, T3] with Product with Serializable { ... }
      |object Tuple3 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple4[+T1, +T2, +T3, +T4](_1: T1, _2: T2, _3: T3, _4: T4) extends AnyRef with Product4[T1, T2, T3, T4] with Product with Serializable { ... }
      |object Tuple4 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple5[+T1, +T2, +T3, +T4, +T5](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5) extends AnyRef with Product5[T1, T2, T3, T4, T5] with Product with Serializable { ... }
      |object Tuple5 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple6[+T1, +T2, +T3, +T4, +T5, +T6](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6) extends AnyRef with Product6[T1, T2, T3, T4, T5, T6] with Product with Serializable { ... }
      |object Tuple6 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple7[+T1, +T2, +T3, +T4, +T5, +T6, +T7](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7) extends AnyRef with Product7[T1, T2, T3, T4, T5, T6, T7] with Product with Serializable { ... }
      |object Tuple7 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8) extends AnyRef with Product8[T1, T2, T3, T4, T5, T6, T7, T8] with Product with Serializable { ... }
      |object Tuple8 extends AnyRef with Serializable { ... }
      |@deprecatedInheritance("Tuples will be made final in a future version.", "2.11.0") case class Tuple9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9) extends AnyRef with Product9[T1, T2, T3, T4, T5, T6, T7, T8, T9] with Product with Serializable { ... }
      |object Tuple9 extends AnyRef with Serializable { ... }
      |final class UninitializedError() extends RuntimeException { ... }
      |final case class UninitializedFieldError(msg: String) extends RuntimeException with Product with Serializable { ... }
      |object UninitializedFieldError extends AbstractFunction1[String, UninitializedFieldError] with Serializable { ... }
      |private[scala] abstract class UniquenessCache[K, V >: Null]() extends AnyRef { ... }
      |final abstract class Unit() extends AnyVal { ... }
      |object Unit extends AnyRef with AnyValCompanion { ... }
      |type UnsupportedOperationException = UnsupportedOperationException
      |Vector
      |type Vector[+A] = Vector[A]
      |package object `package` extends AnyRef { ... }
      |package annotation { ... }
      |package beans { ... }
      |package collection { ... }
      |package compat { ... }
      |package concurrent { ... }
      |@getter @setter @beanGetter @beanSetter class deprecated(message: String = jvmMethod("deprecated.type.$lessinit$greater$default$1()Ljava/lang/String;").invoke(this), since: String = jvmMethod("deprecated.type.$lessinit$greater$default$2()Ljava/lang/String;").invoke(this)) extends Annotation with StaticAnnotation { ... }
      |object deprecated extends AnyRef { ... }
      |private[scala] class deprecatedInheritance(message: String = jvmMethod("deprecatedInheritance.type.$lessinit$greater$default$1()Ljava/lang/String;").invoke(this), since: String = jvmMethod("deprecatedInheritance.type.$lessinit$greater$default$2()Ljava/lang/String;").invoke(this)) extends Annotation with StaticAnnotation { ... }
      |private[scala] object deprecatedInheritance extends AnyRef { ... }
      |@param class deprecatedName(name: Symbol) extends Annotation with StaticAnnotation { ... }
      |private[scala] class deprecatedOverriding(message: String = jvmMethod("deprecatedOverriding.type.$lessinit$greater$default$1()Ljava/lang/String;").invoke(this), since: String = jvmMethod("deprecatedOverriding.type.$lessinit$greater$default$2()Ljava/lang/String;").invoke(this)) extends Annotation with StaticAnnotation { ... }
      |private[scala] object deprecatedOverriding extends AnyRef { ... }
      |class inline() extends Annotation with StaticAnnotation { ... }
      |package io { ... }
      |object language extends AnyRef { ... }
      |object languageFeature extends AnyRef { ... }
      |package math { ... }
      |package meta { ... }
      |class native() extends Annotation with StaticAnnotation { ... }
      |class noinline() extends Annotation with StaticAnnotation { ... }
      |package org { ... }
      |package ref { ... }
      |package reflect { ... }
      |class remote() extends Annotation with StaticAnnotation { ... }
      |package runtime { ... }
      |class specialized(group: SpecializedGroup) extends Annotation with StaticAnnotation { ... }
      |package sys { ... }
      |package text { ... }
      |class throws[T <: Throwable](cause: String = jvmMethod("throws.type.$lessinit$greater$default$1()Ljava/lang/String;").invoke(this)) extends Annotation with StaticAnnotation { ... }
      |object throws extends AnyRef { ... }
      |package tools { ... }
      |@field class transient() extends Annotation with StaticAnnotation { ... }
      |class unchecked() extends Annotation { ... }
      |package util { ... }
      |@field class volatile() extends Annotation with StaticAnnotation { ... }
      |package xml { ... }
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }
}
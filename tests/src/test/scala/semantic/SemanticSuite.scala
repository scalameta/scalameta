import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.{Exception => SemanticException}
import scala.meta.internal.{ast => impl}

class SemanticSuite extends FunSuite {
  import scala.meta.internal.hosts.scalac.Scalahost
  private val classpathOptions = s"-cp ${sys.props("sbt.paths.scala-library.jar")}"
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
      |[1] Type.Singleton(Term.Name("package")[4])::scala.package#List
      |[2] 0::scala.package#List#A
      |[3] Type.Singleton(Term.Name("immutable")[5])::scala.collection.immutable#List
      |[4] Type.Singleton(Term.Name("scala")[7])::scala.package
      |[5] Type.Singleton(Term.Name("collection")[6])::scala.collection.immutable
      |[6] Type.Singleton(Term.Name("scala")[7])::scala.collection
      |[7] Type.Singleton(Term.Name("_root_")[8])::scala
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
      |override def companion: GenericCompanion[List] = ???
      |def ::[B >: A](x: B): List[B] = ???
      |def :::[B >: A](prefix: List[B]): List[B] = ???
      |def reverse_:::[B >: A](prefix: List[B]): List[B] = ???
      |@inline final def mapConserve[B >: A <: AnyRef](f: A => B): List[B] = ???
      |override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |override def toList: List[A] = ???
      |override def take(n: Int): List[A] = ???
      |override def drop(n: Int): List[A] = ???
      |override def slice(from: Int, until: Int): List[A] = ???
      |override def takeRight(n: Int): List[A] = ???
      |override def splitAt(n: Int): (List[A], List[A]) = ???
      |@noinline final override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |@noinline final override def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |@noinline final override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |@inline final override def takeWhile(p: A => Boolean): List[A] = ???
      |@inline final override def dropWhile(p: A => Boolean): List[A] = ???
      |@inline final override def span(p: A => Boolean): (List[A], List[A]) = ???
      |@inline final override def foreach[U](f: A => U): Unit = ???
      |override def reverse: List[A] = ???
      |override def foldRight[B](z: B)(op: (A, B) => B): B = ???
      |override def stringPrefix: String = ???
      |override def toStream: Stream[A] = ???
      |protected final def writeReplace(): AnyRef = ???
      |def length: Int = ???
      |def apply(n: Int): A = ???
      |override def forall(p: A => Boolean): Boolean = ???
      |override def exists(p: A => Boolean): Boolean = ???
      |override def contains[A1 >: A](elem: A1): Boolean = ???
      |override def find(p: A => Boolean): Option[A] = ???
      |override def foldLeft[B](z: B)(f: (B, A) => B): B = ???
      |override def reduceLeft[B >: A](f: (B, A) => B): B = ???
      |override def reduceRight[B >: A](op: (A, B) => B): B = ???
      |override def last: A = ???
      |override def dropRight(n: Int): List[A] = ???
      |override def sameElements[B >: A](that: GenIterable[B]): Boolean = ???
      |override def lengthCompare(len: Int): Int = ???
      |override def isDefinedAt(x: Int): Boolean = ???
      |override def segmentLength(p: A => Boolean, from: Int): Int = ???
      |override def indexWhere(p: A => Boolean, from: Int): Int = ???
      |override def lastIndexWhere(p: A => Boolean, end: Int): Int = ???
      |def productElement(n: Int): Any
      |def productArity: Int
      |def productIterator: Iterator[Any] = ???
      |def productPrefix: String = ???
      |override def seq: LinearSeq[A] = ???
      |protected[this] override def thisCollection: LinearSeq[A] = ???
      |protected[this] override def toCollection(repr: List[A]): LinearSeq[A] = ???
      |override def hashCode(): Int = ???
      |override def iterator: Iterator[A] = ???
      |@tailrec final override def corresponds[B](that: GenSeq[B])(p: (A, B) => Boolean): Boolean = ???
      |override def toSeq: Seq[A] = ???
      |protected[this] override def parCombiner: Combiner[A, ParSeq[A]] = ???
      |override def isEmpty: Boolean = ???
      |override def size: Int = ???
      |def permutations: Iterator[List[A]] = ???
      |def combinations(n: Int): Iterator[List[A]] = ???
      |def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |def reverseIterator: Iterator[A] = ???
      |def startsWith[B](that: GenSeq[B], offset: Int): Boolean = ???
      |def endsWith[B](that: GenSeq[B]): Boolean = ???
      |def indexOfSlice[B >: A](that: GenSeq[B]): Int = ???
      |def indexOfSlice[B >: A](that: GenSeq[B], from: Int): Int = ???
      |def lastIndexOfSlice[B >: A](that: GenSeq[B]): Int = ???
      |def lastIndexOfSlice[B >: A](that: GenSeq[B], end: Int): Int = ???
      |def containsSlice[B](that: GenSeq[B]): Boolean = ???
      |override def union[B >: A, That](that: GenSeq[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |def diff[B >: A](that: GenSeq[B]): List[A] = ???
      |def intersect[B >: A](that: GenSeq[B]): List[A] = ???
      |def distinct: List[A] = ???
      |def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |def sortWith(lt: (A, A) => Boolean): List[A] = ???
      |def sortBy[B](f: A => B)(implicit ord: Ordering[B]): List[A] = ???
      |def sorted[B >: A](implicit ord: Ordering[B]): List[A] = ???
      |def indices: Range = ???
      |override def view: AnyRef with SeqView[A, List[A]] = ???
      |override def view(from: Int, until: Int): SeqView[A, List[A]] = ???
      |override def toString(): String = ???
      |def prefixLength(p: A => Boolean): Int = ???
      |def indexWhere(p: A => Boolean): Int = ???
      |def indexOf[B >: A](elem: B): Int = ???
      |def indexOf[B >: A](elem: B, from: Int): Int = ???
      |def lastIndexOf[B >: A](elem: B): Int = ???
      |def lastIndexOf[B >: A](elem: B, end: Int): Int = ???
      |def lastIndexWhere(p: A => Boolean): Int = ???
      |def startsWith[B](that: GenSeq[B]): Boolean = ???
      |override def equals(that: Any): Boolean = ???
      |def orElse[A1 <: Int, B1 >: A](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = ???
      |override def andThen[C](k: A => C): PartialFunction[Int, C] = ???
      |def lift: Int => Option[A] = ???
      |def applyOrElse[A1 <: Int, B1 >: A](x: A1, default: A1 => B1): B1 = ???
      |def runWith[U](action: A => U): Int => Boolean = ???
      |@unspecialized def compose[A](g: A => Int): A => A = ???
      |override def toIterable: Iterable[A] = ???
      |@deprecatedOverriding("toIterator should stay consistent with iterator for all Iterables: override iterator instead.", "2.11.0") override def toIterator: Iterator[A] = ???
      |override def head: A = ???
      |def grouped(size: Int): Iterator[List[A]] = ???
      |def sliding(size: Int): Iterator[List[A]] = ???
      |def sliding(size: Int, step: Int): Iterator[List[A]] = ???
      |override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = ???
      |def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[List[A], (A1, B), That]): That = ???
      |def zipAll[B, A1 >: A, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[List[A], (A1, B), That]): That = ???
      |def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[List[A], (A1, Int), That]): That = ???
      |override def canEqual(that: Any): Boolean = ???
      |protected[this] def newBuilder: Builder[A, List[A]] = ???
      |def genericBuilder[B]: Builder[B, List[B]] = ???
      |def unzip[A1, A2](implicit asPair: A => (A1, A2)): (List[A1], List[A2]) = ???
      |def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (List[A1], List[A2], List[A3]) = ???
      |def flatten[B](implicit asTraversable: A => GenTraversableOnce[B]): List[B] = ???
      |@migration("`transpose` throws an `IllegalArgumentException` if collections are not uniformly sized.", "2.9.0") def transpose[B](implicit asTraversable: A => GenTraversableOnce[B]): List[List[B]] = ???
      |protected[this] type Self = List[A]
      |def repr: List[A] = ???
      |final def isTraversableAgain: Boolean = ???
      |def hasDefiniteSize: Boolean = ???
      |def ++:[B >: A, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |def ++:[B >: A, That](that: Traversable[B])(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |def filter(p: A => Boolean): List[A] = ???
      |def filterNot(p: A => Boolean): List[A] = ???
      |def partition(p: A => Boolean): (List[A], List[A]) = ???
      |def groupBy[K](f: A => K): Map[K, List[A]] = ???
      |def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[List[A], B, That]): That = ???
      |def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |@migration("The behavior of `scanRight` has changed. The previous behavior can be reproduced with scanRight.reverse.", "2.9.0") def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[List[A], B, That]): That = ???
      |def headOption: Option[A] = ???
      |override def tail: List[A] = ???
      |def lastOption: Option[A] = ???
      |def init: List[A] = ???
      |private[scala] def sliceWithKnownDelta(from: Int, until: Int, delta: Int): List[A] = ???
      |private[scala] def sliceWithKnownBound(from: Int, until: Int): List[A] = ???
      |def tails: Iterator[List[A]] = ???
      |def inits: Iterator[List[A]] = ???
      |@deprecatedOverriding("Enforce contract of toTraversable that if it is Traversable it returns itself.", "2.11.0") def toTraversable: Traversable[A] = ???
      |override def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A]]): Col[A] = ???
      |def withFilter(p: A => Boolean): FilterMonadic[A, List[A]] = ???
      |class WithFilter(p: A => Boolean) extends AnyRef with FilterMonadic[A, Repr] { ... }
      |def par: ParSeq[A] = ???
      |protected[this] def reversed: List[A] = ???
      |def nonEmpty: Boolean = ???
      |def count(p: A => Boolean): Int = ???
      |def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = ???
      |def /:[B](z: B)(op: (B, A) => B): B = ???
      |def :\[B](z: B)(op: (A, B) => B): B = ???
      |def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = ???
      |def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = ???
      |def reduce[A1 >: A](op: (A1, A1) => A1): A1 = ???
      |def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1] = ???
      |def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = ???
      |def aggregate[B](z: => B)(seqop: (B, A) => B, combop: (B, B) => B): B = ???
      |def sum[B >: A](implicit num: Numeric[B]): B = ???
      |def product[B >: A](implicit num: Numeric[B]): B = ???
      |def min[B >: A](implicit cmp: Ordering[B]): A = ???
      |def max[B >: A](implicit cmp: Ordering[B]): A = ???
      |def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = ???
      |def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = ???
      |def copyToBuffer[B >: A](dest: Buffer[B]): Unit = ???
      |def copyToArray[B >: A](xs: Array[B], start: Int): Unit = ???
      |def copyToArray[B >: A](xs: Array[B]): Unit = ???
      |def toArray[B >: A: ClassTag](implicit evidence$1: ClassTag[B]): Array[B] = ???
      |def toIndexedSeq: IndexedSeq[A] = ???
      |def toBuffer[B >: A]: Buffer[B] = ???
      |def toSet[B >: A]: Set[B] = ???
      |def toVector: Vector[A] = ???
      |def toMap[T, U](implicit ev: A <:< (T, U)): Map[T, U] = ???
      |def mkString(start: String, sep: String, end: String): String = ???
      |def mkString(sep: String): String = ???
      |def mkString: String = ???
      |def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = ???
      |def addString(b: StringBuilder, sep: String): StringBuilder = ???
      |def addString(b: StringBuilder): StringBuilder = ???
      |final def getClass(): Class[?0] forSome { type ?0 } = ???
      |@throws[CloneNotSupportedException] protected[lang] def clone(): Object = ???
      |final def notify(): Unit = ???
      |final def notifyAll(): Unit = ???
      |@throws[InterruptedException] final def wait(_: Long): Unit = ???
      |@throws[InterruptedException] final def wait(_: Long, _: Int): Unit = ???
      |@throws[InterruptedException] final def wait(): Unit = ???
      |@throws[Throwable] protected[lang] def finalize(): Unit = ???
      |final def eq(_: AnyRef): Boolean = ???
      |final def ne(_: AnyRef): Boolean = ???
      |final def ==(_: Any): Boolean = ???
      |final def !=(_: Any): Boolean = ???
      |final def ##(): Int = ???
      |final def synchronized[T0](_: T0): T0 = ???
      |final def isInstanceOf[T0]: Boolean = ???
      |final def asInstanceOf[T0]: T0 = ???
    """.trim.stripMargin)
  }

  test("t\"List[Int]\".members") {
    assert(t"List[Int]".members.mkString(EOL) === """
      |def this()
      |override def companion: GenericCompanion[List] = ???
      |def ::[B >: Int](x: B): List[B] = ???
      |def :::[B >: Int](prefix: List[B]): List[B] = ???
      |def reverse_:::[B >: Int](prefix: List[B]): List[B] = ???
      |@inline final def mapConserve[B >: Int <: AnyRef](f: Int => B): List[B] = ???
      |override def ++[B >: Int, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |override def +:[B >: Int, That](elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |override def toList: List[Int] = ???
      |override def take(n: Int): List[Int] = ???
      |override def drop(n: Int): List[Int] = ???
      |override def slice(from: Int, until: Int): List[Int] = ???
      |override def takeRight(n: Int): List[Int] = ???
      |override def splitAt(n: Int): (List[Int], List[Int]) = ???
      |@noinline final override def map[B, That](f: Int => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |@noinline final override def collect[B, That](pf: PartialFunction[Int, B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |@noinline final override def flatMap[B, That](f: Int => GenTraversableOnce[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |@inline final override def takeWhile(p: Int => Boolean): List[Int] = ???
      |@inline final override def dropWhile(p: Int => Boolean): List[Int] = ???
      |@inline final override def span(p: Int => Boolean): (List[Int], List[Int]) = ???
      |@inline final override def foreach[U](f: Int => U): Unit = ???
      |override def reverse: List[Int] = ???
      |override def foldRight[B](z: B)(op: (Int, B) => B): B = ???
      |override def stringPrefix: String = ???
      |override def toStream: Stream[Int] = ???
      |protected final def writeReplace(): AnyRef = ???
      |def length: Int = ???
      |def apply(n: Int): Int = ???
      |override def forall(p: Int => Boolean): Boolean = ???
      |override def exists(p: Int => Boolean): Boolean = ???
      |override def contains[A1 >: Int](elem: A1): Boolean = ???
      |override def find(p: Int => Boolean): Option[Int] = ???
      |override def foldLeft[B](z: B)(f: (B, Int) => B): B = ???
      |override def reduceLeft[B >: Int](f: (B, Int) => B): B = ???
      |override def reduceRight[B >: Int](op: (Int, B) => B): B = ???
      |override def last: Int = ???
      |override def dropRight(n: Int): List[Int] = ???
      |override def sameElements[B >: Int](that: GenIterable[B]): Boolean = ???
      |override def lengthCompare(len: Int): Int = ???
      |override def isDefinedAt(x: Int): Boolean = ???
      |override def segmentLength(p: Int => Boolean, from: Int): Int = ???
      |override def indexWhere(p: Int => Boolean, from: Int): Int = ???
      |override def lastIndexWhere(p: Int => Boolean, end: Int): Int = ???
      |def productElement(n: Int): Any
      |def productArity: Int
      |def productIterator: Iterator[Any] = ???
      |def productPrefix: String = ???
      |override def seq: LinearSeq[Int] = ???
      |protected[this] override def thisCollection: LinearSeq[Int] = ???
      |protected[this] override def toCollection(repr: List[Int]): LinearSeq[Int] = ???
      |override def hashCode(): Int = ???
      |override def iterator: Iterator[Int] = ???
      |@tailrec final override def corresponds[B](that: GenSeq[B])(p: (Int, B) => Boolean): Boolean = ???
      |override def toSeq: Seq[Int] = ???
      |protected[this] override def parCombiner: Combiner[Int, ParSeq[Int]] = ???
      |override def isEmpty: Boolean = ???
      |override def size: Int = ???
      |def permutations: Iterator[List[Int]] = ???
      |def combinations(n: Int): Iterator[List[Int]] = ???
      |def reverseMap[B, That](f: Int => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |def reverseIterator: Iterator[Int] = ???
      |def startsWith[B](that: GenSeq[B], offset: Int): Boolean = ???
      |def endsWith[B](that: GenSeq[B]): Boolean = ???
      |def indexOfSlice[B >: Int](that: GenSeq[B]): Int = ???
      |def indexOfSlice[B >: Int](that: GenSeq[B], from: Int): Int = ???
      |def lastIndexOfSlice[B >: Int](that: GenSeq[B]): Int = ???
      |def lastIndexOfSlice[B >: Int](that: GenSeq[B], end: Int): Int = ???
      |def containsSlice[B](that: GenSeq[B]): Boolean = ???
      |override def union[B >: Int, That](that: GenSeq[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |def diff[B >: Int](that: GenSeq[B]): List[Int] = ???
      |def intersect[B >: Int](that: GenSeq[B]): List[Int] = ???
      |def distinct: List[Int] = ???
      |def patch[B >: Int, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |def updated[B >: Int, That](index: Int, elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |def :+[B >: Int, That](elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |def padTo[B >: Int, That](len: Int, elem: B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |def sortWith(lt: (Int, Int) => Boolean): List[Int] = ???
      |def sortBy[B](f: Int => B)(implicit ord: Ordering[B]): List[Int] = ???
      |def sorted[B >: Int](implicit ord: Ordering[B]): List[Int] = ???
      |def indices: Range = ???
      |override def view: AnyRef with SeqView[Int, List[Int]] = ???
      |override def view(from: Int, until: Int): SeqView[Int, List[Int]] = ???
      |override def toString(): String = ???
      |def prefixLength(p: Int => Boolean): Int = ???
      |def indexWhere(p: Int => Boolean): Int = ???
      |def indexOf[B >: Int](elem: B): Int = ???
      |def indexOf[B >: Int](elem: B, from: Int): Int = ???
      |def lastIndexOf[B >: Int](elem: B): Int = ???
      |def lastIndexOf[B >: Int](elem: B, end: Int): Int = ???
      |def lastIndexWhere(p: Int => Boolean): Int = ???
      |def startsWith[B](that: GenSeq[B]): Boolean = ???
      |override def equals(that: Any): Boolean = ???
      |def orElse[A1 <: Int, B1 >: Int](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = ???
      |override def andThen[C](k: Int => C): PartialFunction[Int, C] = ???
      |def lift: Int => Option[Int] = ???
      |def applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: A1 => B1): B1 = ???
      |def runWith[U](action: Int => U): Int => Boolean = ???
      |@unspecialized def compose[A](g: A => Int): A => Int = ???
      |override def toIterable: Iterable[Int] = ???
      |@deprecatedOverriding("toIterator should stay consistent with iterator for all Iterables: override iterator instead.", "2.11.0") override def toIterator: Iterator[Int] = ???
      |override def head: Int = ???
      |def grouped(size: Int): Iterator[List[Int]] = ???
      |def sliding(size: Int): Iterator[List[Int]] = ???
      |def sliding(size: Int, step: Int): Iterator[List[Int]] = ???
      |override def copyToArray[B >: Int](xs: Array[B], start: Int, len: Int): Unit = ???
      |def zip[A1 >: Int, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[List[Int], (A1, B), That]): That = ???
      |def zipAll[B, A1 >: Int, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[List[Int], (A1, B), That]): That = ???
      |def zipWithIndex[A1 >: Int, That](implicit bf: CanBuildFrom[List[Int], (A1, Int), That]): That = ???
      |override def canEqual(that: Any): Boolean = ???
      |protected[this] def newBuilder: Builder[Int, List[Int]] = ???
      |def genericBuilder[B]: Builder[B, List[B]] = ???
      |def unzip[A1, A2](implicit asPair: Int => (A1, A2)): (List[A1], List[A2]) = ???
      |def unzip3[A1, A2, A3](implicit asTriple: Int => (A1, A2, A3)): (List[A1], List[A2], List[A3]) = ???
      |def flatten[B](implicit asTraversable: Int => GenTraversableOnce[B]): List[B] = ???
      |@migration("`transpose` throws an `IllegalArgumentException` if collections are not uniformly sized.", "2.9.0") def transpose[B](implicit asTraversable: Int => GenTraversableOnce[B]): List[List[B]] = ???
      |protected[this] type Self = List[Int]
      |def repr: List[Int] = ???
      |final def isTraversableAgain: Boolean = ???
      |def hasDefiniteSize: Boolean = ???
      |def ++:[B >: Int, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |def ++:[B >: Int, That](that: Traversable[B])(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |def filter(p: Int => Boolean): List[Int] = ???
      |def filterNot(p: Int => Boolean): List[Int] = ???
      |def partition(p: Int => Boolean): (List[Int], List[Int]) = ???
      |def groupBy[K](f: Int => K): Map[K, List[Int]] = ???
      |def scan[B >: Int, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[List[Int], B, That]): That = ???
      |def scanLeft[B, That](z: B)(op: (B, Int) => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |@migration("The behavior of `scanRight` has changed. The previous behavior can be reproduced with scanRight.reverse.", "2.9.0") def scanRight[B, That](z: B)(op: (Int, B) => B)(implicit bf: CanBuildFrom[List[Int], B, That]): That = ???
      |def headOption: Option[Int] = ???
      |override def tail: List[Int] = ???
      |def lastOption: Option[Int] = ???
      |def init: List[Int] = ???
      |private[scala] def sliceWithKnownDelta(from: Int, until: Int, delta: Int): List[Int] = ???
      |private[scala] def sliceWithKnownBound(from: Int, until: Int): List[Int] = ???
      |def tails: Iterator[List[Int]] = ???
      |def inits: Iterator[List[Int]] = ???
      |@deprecatedOverriding("Enforce contract of toTraversable that if it is Traversable it returns itself.", "2.11.0") def toTraversable: Traversable[Int] = ???
      |override def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, Int, Col[Int]]): Col[Int] = ???
      |def withFilter(p: Int => Boolean): FilterMonadic[Int, List[Int]] = ???
      |class WithFilter(p: A => Boolean) extends AnyRef with FilterMonadic[A, Repr] { ... }
      |def par: ParSeq[Int] = ???
      |protected[this] def reversed: List[Int] = ???
      |def nonEmpty: Boolean = ???
      |def count(p: Int => Boolean): Int = ???
      |def collectFirst[B](pf: PartialFunction[Int, B]): Option[B] = ???
      |def /:[B](z: B)(op: (B, Int) => B): B = ???
      |def :\[B](z: B)(op: (Int, B) => B): B = ???
      |def reduceLeftOption[B >: Int](op: (B, Int) => B): Option[B] = ???
      |def reduceRightOption[B >: Int](op: (Int, B) => B): Option[B] = ???
      |def reduce[A1 >: Int](op: (A1, A1) => A1): A1 = ???
      |def reduceOption[A1 >: Int](op: (A1, A1) => A1): Option[A1] = ???
      |def fold[A1 >: Int](z: A1)(op: (A1, A1) => A1): A1 = ???
      |def aggregate[B](z: => B)(seqop: (B, Int) => B, combop: (B, B) => B): B = ???
      |def sum[B >: Int](implicit num: Numeric[B]): B = ???
      |def product[B >: Int](implicit num: Numeric[B]): B = ???
      |def min[B >: Int](implicit cmp: Ordering[B]): Int = ???
      |def max[B >: Int](implicit cmp: Ordering[B]): Int = ???
      |def maxBy[B](f: Int => B)(implicit cmp: Ordering[B]): Int = ???
      |def minBy[B](f: Int => B)(implicit cmp: Ordering[B]): Int = ???
      |def copyToBuffer[B >: Int](dest: Buffer[B]): Unit = ???
      |def copyToArray[B >: Int](xs: Array[B], start: Int): Unit = ???
      |def copyToArray[B >: Int](xs: Array[B]): Unit = ???
      |def toArray[B >: Int: ClassTag](implicit evidence$1: ClassTag[B]): Array[B] = ???
      |def toIndexedSeq: IndexedSeq[Int] = ???
      |def toBuffer[B >: Int]: Buffer[B] = ???
      |def toSet[B >: Int]: Set[B] = ???
      |def toVector: Vector[Int] = ???
      |def toMap[T, U](implicit ev: Int <:< (T, U)): Map[T, U] = ???
      |def mkString(start: String, sep: String, end: String): String = ???
      |def mkString(sep: String): String = ???
      |def mkString: String = ???
      |def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = ???
      |def addString(b: StringBuilder, sep: String): StringBuilder = ???
      |def addString(b: StringBuilder): StringBuilder = ???
      |final def getClass(): Class[?0] forSome { type ?0 } = ???
      |@throws[CloneNotSupportedException] protected[lang] def clone(): Object = ???
      |final def notify(): Unit = ???
      |final def notifyAll(): Unit = ???
      |@throws[InterruptedException] final def wait(_: Long): Unit = ???
      |@throws[InterruptedException] final def wait(_: Long, _: Int): Unit = ???
      |@throws[InterruptedException] final def wait(): Unit = ???
      |@throws[Throwable] protected[lang] def finalize(): Unit = ???
      |final def eq(_: AnyRef): Boolean = ???
      |final def ne(_: AnyRef): Boolean = ???
      |final def ==(_: Any): Boolean = ???
      |final def !=(_: Any): Boolean = ???
      |final def ##(): Int = ???
      |final def synchronized[T0](_: T0): T0 = ???
      |final def isInstanceOf[T0]: Boolean = ???
      |final def asInstanceOf[T0]: T0 = ???
    """.trim.stripMargin)
  }

  test("t\"scala.compat.Platform.type\".members") {
    assert(t"scala.compat.Platform.type".members.mkString(EOL) === """
      |def this()
      |type StackOverflowError = StackOverflowError
      |type ConcurrentModificationException = ConcurrentModificationException
      |@inline def arraycopy(src: AnyRef, srcPos: Int, dest: AnyRef, destPos: Int, length: Int): Unit = ???
      |@inline def createArray(elemClass: Class[_$1] forSome { type _$1 }, length: Int): AnyRef = ???
      |@inline def arrayclear(arr: Array[Int]): Unit = ???
      |@inline def getClassForName(name: String): Class[_$2] forSome { type _$2 } = ???
      |EOL
      |@inline def currentTime: Long = ???
      |@inline def collectGarbage(): Unit = ???
      |@inline def defaultCharsetName: String = ???
      |final def getClass(): Class[?0] forSome { type ?0 } = ???
      |def hashCode(): Int = ???
      |def equals(_: Any): Boolean = ???
      |@throws[CloneNotSupportedException] protected[lang] def clone(): Object = ???
      |def toString(): String = ???
      |final def notify(): Unit = ???
      |final def notifyAll(): Unit = ???
      |@throws[InterruptedException] final def wait(_: Long): Unit = ???
      |@throws[InterruptedException] final def wait(_: Long, _: Int): Unit = ???
      |@throws[InterruptedException] final def wait(): Unit = ???
      |@throws[Throwable] protected[lang] def finalize(): Unit = ???
      |final def eq(_: AnyRef): Boolean = ???
      |final def ne(_: AnyRef): Boolean = ???
      |final def ==(_: Any): Boolean = ???
      |final def !=(_: Any): Boolean = ???
      |final def ##(): Int = ???
      |final def synchronized[T0](_: T0): T0 = ???
      |final def isInstanceOf[T0]: Boolean = ???
      |final def asInstanceOf[T0]: T0 = ???
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
      |@SerialVersionUID(value = -6084104484083858598l) sealed abstract class List[+A]() extends AbstractSeq[A] with LinearSeq[A] with Product with GenericTraversableTemplate[A, List] with LinearSeqOptimized[A, List[A]] with Serializable { ... }
    """.trim.stripMargin)
  }
}
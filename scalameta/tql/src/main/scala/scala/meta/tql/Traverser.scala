package scala.meta
package tql

trait Traverser[T] {

  type MatchResult[A] = Option[(T, A)]

  /**
   * A Matcher is a function or 'combinator' which takes a T and return an Option of tuple of
   * - a transformed T (or the same)
   * - a result of type A s.t Exists Monoid[A] so that results can be combined during the traversal
   * A transformation/traversal has succeeded if the result of the application on the Matcher is not a None
   * */
  abstract class Matcher[+A] extends (T => MatchResult[A]) {

    /**
     * a andThen b
     * b is 'executed' only if a succeeded
     * We discard the result of a
     * */
    def andThen[B](m: => Matcher[B]) = Matcher[B] { tree =>
      for {
        (t, v) <- this(tree)
        t2 <- m(t)
      } yield t2
    }

    /**
     * Alias for andThen
     * */
    def ~>[B](m: =>  Matcher[B]) = andThen(m)

    /**
     * a andThenLeft b
     * b is 'executed' only if a succeeded
     * We discard the result of b and only keep the result of a
     * */
    def andThenLeft[B](m: => Matcher[B]) = Matcher[A] { tree =>
     for {
        (t, v) <- this(tree)
        (t2, v2) <- m(t)
     } yield (t2, v)
    }

    /**
     * Alias for andThenLeft
     * */
    def <~[B : Monoid](m: =>  Matcher[B]) = andThenLeft(m)

    /**
     * Combine the result of two TreeMappers in a tuple2.
     * The order is important, as the the second transformation is applied on the
     * result of the first one.
     * */
    def tupledWith[B : Monoid, C >: A : Monoid](m: => Matcher[B]) = Matcher[(C, B)] { tree =>
      this(tree).map {
        case u @ (a1, a2) => m(a1)
          .map {case (b1, b2) => (b1, (a2, b2))}
          .getOrElse((u._1, (u._2, implicitly[Monoid[B]].zero)))
      } orElse(m(tree).map (u => (u._1, (implicitly[Monoid[C]].zero, u._2))))
    }

    /**
     * Alias for tupledWith
     * */
    def ~[B : Monoid, C >: A : Monoid](m: => Matcher[B]) = tupledWith[B, C](m)

    /**
     * a tupledResultsWith b
     * Same as 'tupledWith' but discard the transformed trees of a and b
     * b operates on the origin tree, not the one transformed by a
     */
    def tupledResultsWith[B : Monoid, C >: A : Monoid](m: => Matcher[B]) = Matcher[(C, B)] { tree =>
      this(tree).map {
        case u @ (_, a2) => m(tree)
          .map {case (_, b2) => (tree, (a2, b2))}
          .getOrElse((u._1, (u._2, implicitly[Monoid[B]].zero)))
      } orElse(m(tree).map (u => (u._1, (implicitly[Monoid[C]].zero, u._2))))
    }

    /**
     * a aggregate b
     * Apply a and apply b on the result of the application of a. This means:
     *  - b is applied on the transformed T from a
     *  - the results of a and b are composed
     *
     * If a fails then b is applied
     * If b fails then only the result of a is used
     * */
    def aggregate[B >: A : Monoid](m: => Matcher[B]) = Matcher[B] { tree =>
      this(tree) map {
        case t @ (a1, a2)  => m(a1)
          .map{case (b1, b2) => (b1, implicitly[Monoid[B]].append(a2, b2))}
          .getOrElse(t)
      } orElse(m(tree))
    }

    /**
     * Alias for aggregate
     * */
    def +[B >: A : Monoid](m: => Matcher[B]) = aggregate(m)

    /**
     * a aggregateResults b
     * Same as 'aggregate' but discard the transformed trees of a and b
     * b operates on the origin tree, not the one transformed by a
     */
    def aggregateResults[B >: A : Monoid](m: => Matcher[B]) = Matcher[B] { tree =>
      this(tree) map {
        case t @ (_, a2) => m(tree)
          .map{case (_, b2) => (tree, implicitly[Monoid[B]].append(a2, b2))}
          .getOrElse(t)
      } orElse(m(tree))
    }

    /**
     * Alias for aggregateResults
     * */
    def +> [B >: A : Monoid](m: => Matcher[B]) = aggregateResults[B](m)

    /**
     * Transform the result of the Matcher, will probably change
     * */
    def map[B](f: A => B) = Matcher[B] { tree =>
      for {
        (v, t) <- this(tree)
      } yield ((v, f(t)))
    }

    /**
     * a orElse b
     * Apply b only if a failed
     * */
    def orElse[B >: A : Monoid](m: => Matcher[B]) = Matcher[B] { tree  =>
      this(tree) orElse m(tree)
    }

    /**
     * Alias for orElse
     * */
    def |[B >: A : Monoid](m: => Matcher[B]) = orElse(m)

    /**
     * a orThen b
     * Apply b only if a failed but return a result of type b whaterver happens
     * */
    def orThen[B : Monoid](m: => Matcher[B]) = Matcher[B] { tree  =>
      this(tree)
        .map (_ => (tree, implicitly[Monoid[B]].zero))
        .orElse(m(tree))
    }

    def either[B : Monoid, C >: A : Monoid](m2: Matcher[B]): Matcher[(C, B)] =
      map(x => (x, implicitly[Monoid[B]].zero)).orElse(m2.map(x => (implicitly[Monoid[C]].zero, x)))

    /**
     * Alias for orThen
     * */
    def |>[B : Monoid](m: => Matcher[B]) = orThen(m)

    /**
     * a feed {resa => b}
     * combinator b can use the result (resa) of a.
     * */
    def feed[B : Monoid](m: => A => Matcher[B]) = Matcher[B] {tree =>for {
        (t, v) <- this(tree)
        t2     <- m(v)(t)
      } yield t2
    }
  }

  def Matcher[A](f: T => MatchResult[A]): Matcher[A] = new Matcher[A] {
    override def apply(tree: T): MatchResult[A] = f(tree)
  }

  def traverse[A : Monoid](tree: T, f: Matcher[A]): MatchResult[A]
}
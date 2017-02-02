package scala.meta.contrib.implicits

import scala.collection.immutable.Seq
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.meta._
import scala.meta.contrib.Equal

/**
  * Typeclass to deal with scalameta trees which have an expected set of statements.
  *
  * For trees that have multiple stat lists (such as Defn.Class) the main stats are used.
  *
  * This allows you to do things like add a statement to the
  * body of a class without digging through the low level meta api
  *
  * TODO: Consider using a meta variant of simulacrum to
  * generate the boilerplate once one is avaliable
  */
trait Statements[A] {
  def getStats(a: A): Seq[Stat]
  def withStats(a: A, newStats: Seq[Stat]): A
}

trait StatementsTypeClass {

  implicit def statementsTypeClass[A](implicit ev: Statements[A]): Statements[A] = ev

  implicit class XtensionStatements[A](a: A)(implicit ev: Statements[A]) {
    def getStats: Seq[Stat] =
      ev.getStats(a)

    def withStats(newStats: Seq[Stat]): A =
      ev.withStats(a, newStats)

    def updateStats(f: Seq[Stat] => Seq[Stat]): A =
      withStats(f(getStats))

    def types(): Seq[Defn.Type] =
      getStats.collect { case t: Defn.Type => t}

    def defs(): Seq[Defn.Def] =
      getStats.collect { case d: Defn.Def => d}

    def vals(): Seq[Defn.Val] =
      getStats.collect { case v: Defn.Val => v}

    def vars(): Seq[Defn.Var] =
      getStats.collect { case v: Defn.Var => v}

    def traits(): Seq[Defn.Trait] =
      getStats.collect { case t: Defn.Trait => t}

    def objects(): Seq[Defn.Object] =
      getStats.collect { case o: Defn.Object => o}

    def classes(): Seq[Defn.Class] =
      getStats.collect { case c: Defn.Class => c}
  }
}
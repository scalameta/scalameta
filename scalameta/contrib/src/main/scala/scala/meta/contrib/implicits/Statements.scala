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
  def replaceStats(a: A, newStats: Seq[Stat]): A
}

object Statements {

  trait Ops[A] {
    val self: A
    val typeClassInstance: Statements[A]

    def getStats: Seq[Stat] =
      typeClassInstance.getStats(self)

    def replaceStats(newStats: Seq[Stat]): A =
      typeClassInstance.replaceStats(self, newStats)

// DAVETODO: Get F param working (Aka Structurally/Syntactically)
//    Is this going to be useful?
//    def replaceStatWith[F[_ <: Tree]](stat: Stat, replacement: Stat)(
//        implicit conv: Tree => F[Tree], eq: Equal[F[Tree]]): A = {
//
//      val newStats: Seq[Stat] = getStats.collect {
//        case candidate if eq.equal(candidate, stat) => replacement
//        case other => other
//      }
//
//      replaceStats(newStats)
//    }
//    def removeStat[F[_]](stat: Stat)(implicit eq: Equal[F[A]]): A =
//      removeStats[F](stat :: Nil)
//
//    def removeStats[F[_]](stats: Seq[Stat])(
//      implicit conv: Tree => F[Tree], eq: Equal[F[Tree]]): A = {
//
//      val filtered: Seq[Stat] = getStats.filter(stat => stats.exists(eq.equals(_, stat)))
//      replaceStats(filtered)
//    }


    def appendStat(stat: Stat): A =
      replaceStats(getStats :+ stat)

    def appendStats(stats: Seq[Stat]): A =
      replaceStats(getStats ++ stats)

    def prependStat(stat: Stat): A =
      replaceStats(stat +: getStats)

    def prependStats(stats: Seq[Stat]): A =
      replaceStats(stats ++ getStats)

    def deleteAllStats(): A =
      replaceStats(Nil)

    def typeDefs(): Seq[Defn.Type] =
      getStats.collect { case t: Defn.Type => t}

    def defDefs(): Seq[Defn.Def] =
      getStats.collect { case d: Defn.Def => d}

    def valDefs(): Seq[Defn.Val] =
      getStats.collect { case v: Defn.Val => v}

    def varDefs(): Seq[Defn.Var] =
      getStats.collect { case v: Defn.Var => v}

    def traitDefs(): Seq[Defn.Trait] =
      getStats.collect { case t: Defn.Trait => t}

    def objectDefs(): Seq[Defn.Object] =
      getStats.collect { case o: Defn.Object => o}

    def classDefs(): Seq[Defn.Class] =
      getStats.collect { case c: Defn.Class => c}
  }

  trait ToStatementOps {
    implicit def toStatementsOps[A](target: A)(implicit tc: Statements[A]): Ops[A] = new Ops[A] {
      override val self: A = target
      override val typeClassInstance: Statements[A] = tc
    }
  }

  def apply[A](implicit instance: Statements[A]): Statements[A] = instance
}
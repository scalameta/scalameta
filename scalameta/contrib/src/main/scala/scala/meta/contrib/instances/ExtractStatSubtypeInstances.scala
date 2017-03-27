package scala.meta.contrib.instances

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib.Extract

// These could be optimised if necessary. As they currently build an intermediate list
// TODO: Consider completely covering all stat subtypes here. We may actually just be able to macro this
trait ExtractStatSubtypeInstances {
  implicit def extractDefsFromStats[A](
      implicit ev: Extract[A, Seq[Stat]]): Extract[A, Seq[Defn.Def]] =
    new Extract[A, Seq[Defn.Def]] {
      override def extract(a: A): Seq[Defn.Def] =
        ev.extract(a).collect { case d: Defn.Def => d }
    }

  implicit def extractValsFromStats[A](
      implicit ev: Extract[A, Seq[Stat]]): Extract[A, Seq[Defn.Val]] =
    new Extract[A, Seq[Defn.Val]] {
      override def extract(a: A): Seq[Defn.Val] =
        ev.extract(a).collect { case v: Defn.Val => v }
    }

  implicit def extractVarsFromStats[A](
      implicit ev: Extract[A, Seq[Stat]]): Extract[A, Seq[Defn.Var]] =
    new Extract[A, Seq[Defn.Var]] {
      override def extract(a: A): Seq[Defn.Var] =
        ev.extract(a).collect { case v: Defn.Var => v }
    }

  implicit def extractTraitsFromStats[A](
      implicit ev: Extract[A, Seq[Stat]]): Extract[A, Seq[Defn.Trait]] =
    new Extract[A, Seq[Defn.Trait]] {
      override def extract(a: A): Seq[Defn.Trait] =
        ev.extract(a).collect { case t: Defn.Trait => t }
    }

  implicit def extractObjectsFromStats[A](
      implicit ev: Extract[A, Seq[Stat]]): Extract[A, Seq[Defn.Object]] =
    new Extract[A, Seq[Defn.Object]] {
      override def extract(a: A): Seq[Defn.Object] =
        ev.extract(a).collect { case o: Defn.Object => o }
    }

  implicit def extractClassesFromStats[A](
      implicit ev: Extract[A, Seq[Stat]]): Extract[A, Seq[Defn.Class]] =
    new Extract[A, Seq[Defn.Class]] {
      override def extract(a: A): Seq[Defn.Class] =
        ev.extract(a).collect { case c: Defn.Class => c }
    }

  implicit def extractTypesFromStats[A](
      implicit ev: Extract[A, Seq[Stat]]): Extract[A, Seq[Defn.Type]] =
    new Extract[A, Seq[Defn.Type]] {
      override def extract(a: A): Seq[Defn.Type] =
        ev.extract(a).collect { case t: Defn.Type => t }
    }
}

object ExtractStatSubtypeInstances

package scala.meta.contrib.instances

import scala.meta._
import scala.meta.contrib._

trait ExtractStatSubtypeInstances {
  implicit def extractDefsFromStats[A](implicit ev: Extract[A, Stats]): Extract[A, Defs] =
    Extract(a => ev.extract(a).collect { case d: Defn.Def => d })

  implicit def extractValsFromStats[A](implicit ev: Extract[A, Stats]): Extract[A, Vals] =
    Extract(a => ev.extract(a).collect { case v: Defn.Val => v })

  implicit def extractVarsFromStats[A](implicit ev: Extract[A, Stats]): Extract[A, Vars] =
    Extract(a => ev.extract(a).collect { case v: Defn.Var => v })

  implicit def extractTraitsFromStats[A](implicit ev: Extract[A, Stats]): Extract[A, Traits] =
    Extract(a => ev.extract(a).collect { case t: Defn.Trait => t })

  implicit def extractObjectsFromStats[A](implicit ev: Extract[A, Stats]): Extract[A, Objects] =
    Extract(a => ev.extract(a).collect { case o: Defn.Object => o })

  implicit def extractClassesFromStats[A](implicit ev: Extract[A, Stats]): Extract[A, Classes] =
    Extract(a => ev.extract(a).collect { case c: Defn.Class => c })

  implicit def extractTypesFromStats[A](implicit ev: Extract[A, Stats]): Extract[A, Types] =
    Extract(a => ev.extract(a).collect { case d: Defn.Type => d })

}

object ExtractStatSubtypeInstances

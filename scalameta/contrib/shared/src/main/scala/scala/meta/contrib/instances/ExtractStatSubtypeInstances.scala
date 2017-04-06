package scala.meta.contrib.instances

import scala.meta._
import scala.meta.contrib._

trait ExtractStatSubtypeInstances {
  implicit def extractDefsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Defn.Def] =
    Extract(a => ev.extract(a).collect { case d: Defn.Def => d })

  implicit def extractValsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Defn.Val] =
    Extract(a => ev.extract(a).collect { case v: Defn.Val => v })

  implicit def extractVarsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Defn.Var] =
    Extract(a => ev.extract(a).collect { case v: Defn.Var => v })

  implicit def extractTraitsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Defn.Trait] =
    Extract(a => ev.extract(a).collect { case t: Defn.Trait => t })

  implicit def extractObjectsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Defn.Object] =
    Extract(a => ev.extract(a).collect { case o: Defn.Object => o })

  implicit def extractClassesFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Defn.Class] =
    Extract(a => ev.extract(a).collect { case c: Defn.Class => c })

  implicit def extractTypesFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Defn.Type] =
    Extract(a => ev.extract(a).collect { case t: Defn.Type => t })

  implicit def extractTermsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Term] =
    Extract(a => ev.extract(a).collect { case t: Term => t })

  implicit def extractMembersFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Member] =
    Extract(a => ev.extract(a).collect { case d: Member => d })

  implicit def extractDefnsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Defn] =
    Extract(a => ev.extract(a).collect { case d: Defn => d })

  implicit def extractDeclsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Decl] =
    Extract(a => ev.extract(a).collect { case d: Decl => d })

  implicit def extractDeclDefsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Decl.Def] =
    Extract(a => ev.extract(a).collect { case d: Decl.Def => d })

  implicit def extractDeclVarsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Decl.Var] =
    Extract(a => ev.extract(a).collect { case v: Decl.Var => v })

  implicit def extractDeclValsFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Decl.Val] =
    Extract(a => ev.extract(a).collect { case v: Decl.Val => v })

  implicit def extractDeclTypesFromStats[A](implicit ev: Extract[A, Stat]): Extract[A, Decl.Type] =
    Extract(a => ev.extract(a).collect { case v: Decl.Type => v })
}

object ExtractStatSubtypeInstances extends ExtractStatSubtypeInstances

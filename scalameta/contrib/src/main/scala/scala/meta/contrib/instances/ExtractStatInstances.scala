package scala.meta.contrib.instances

import scala.meta._
import scala.meta.contrib._
import scala.collection.immutable.Seq

trait ExtractStatInstances {

  implicit val extractTemplateStats: Extract[Template, Seq[Stat]] =
    new Extract[Template, Seq[Stat]] {
      override def extract(t: Template): Seq[Stat] =
        t.stats.getOrElse(Nil)
    }

  implicit val extractClassStats: Extract[Defn.Class, Seq[Stat]] =
    new Extract[Defn.Class, Seq[Stat]] {
      override def extract(c: Defn.Class): Seq[Stat] =
        c.templ.stats.getOrElse(Nil)
    }

  implicit val extractTraitStats: Extract[Defn.Trait, Seq[Stat]] =
    new Extract[Defn.Trait, Seq[Stat]] {
      override def extract(t: Defn.Trait): Seq[Stat] =
        t.templ.stats.getOrElse(Nil)
    }

  implicit val extractObjectStats: Extract[Defn.Object, Seq[Stat]] =
    new Extract[Defn.Object, Seq[Stat]] {
      override def extract(o: Defn.Object): Seq[Stat] =
        o.templ.stats.getOrElse(Nil)
    }

  implicit val extractPkgStats: Extract[Pkg, Seq[Stat]] = new Extract[Pkg, Seq[Stat]] {
    override def extract(p: Pkg): Seq[Stat] =
      p.stats
  }

  implicit val extractSourcetats: Extract[Source, Seq[Stat]] = new Extract[Source, Seq[Stat]] {
    override def extract(s: Source): Seq[Stat] =
      s.stats
  }

  implicit val extractDefStats: Extract[Defn.Def, Seq[Stat]] = new Extract[Defn.Def, Seq[Stat]] {
    override def extract(d: Defn.Def): Seq[Stat] =
      extractStatsFromTerm(d.body)
  }

  implicit val extractValStats: Extract[Defn.Val, Seq[Stat]] = new Extract[Defn.Val, Seq[Stat]] {
    override def extract(v: Defn.Val): Seq[Stat] =
      extractStatsFromTerm(v.rhs)
  }

  implicit val extractVarStats: Extract[Defn.Var, Seq[Stat]] = new Extract[Defn.Var, Seq[Stat]] {
    override def extract(v: Defn.Var): Seq[Stat] =
      v.rhs
        .map(extractStatsFromTerm)
        .getOrElse(Nil)
  }

  private def extractStatsFromTerm(term: Term): Seq[Stat] =
    term match {
      case Term.Block(stats) => stats
      case s => s :: Nil
    }
}

object ExtractStatInstances extends ExtractStatInstances

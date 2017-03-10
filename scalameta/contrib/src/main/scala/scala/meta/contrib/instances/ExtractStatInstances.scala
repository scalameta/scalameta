package scala.meta.contrib.instances

import scala.meta._
import scala.meta.contrib._

trait ExtractStatInstances {

  implicit val extractTemplateStats: Extract[Template, Stats] =
    Extract(_.stats.getOrElse(Nil))

  implicit val extractClassStats: Extract[Defn.Class, Stats] =
    Extract(_.templ.stats.getOrElse(Nil))

  implicit val extractTraitStats: Extract[Defn.Trait, Stats] =
    Extract(_.templ.stats.getOrElse(Nil))

  implicit val extractObjectStats: Extract[Defn.Object, Stats] =
    Extract(_.templ.stats.getOrElse(Nil))

  implicit val extractPkgStats: Extract[Pkg, Stats] =
    Extract(_.stats)

  implicit val extractSourceStats: Extract[Source, Stats] =
    Extract(_.stats)

  implicit val extractDefStats: Extract[Defn.Def, Stats] =
    Extract(d => extractStatsFromTerm(d.body))

  implicit val extractValStats: Extract[Defn.Val, Stats] =
    Extract(v => extractStatsFromTerm(v.rhs))

  implicit val extractVarStats: Extract[Defn.Var, Stats] =
    Extract(
      _.rhs
        .map(extractStatsFromTerm)
        .getOrElse(Nil))

  private def extractStatsFromTerm(term: Term): Stats =
    term match {
      case Term.Block(stats) => stats
      case s => s :: Nil
    }
}

object ExtractStatInstances extends ExtractStatInstances

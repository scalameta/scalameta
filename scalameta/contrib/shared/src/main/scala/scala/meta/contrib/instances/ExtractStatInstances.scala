package scala.meta.contrib.instances

import scala.meta._
import scala.meta.contrib._

trait ExtractStatInstances {
  implicit val extractTemplateStats: Extract[Template, Stat] =
    Extract(_.stats)

  implicit val extractClassStats: Extract[Defn.Class, Stat] =
    Extract(_.templ.stats)

  implicit val extractTraitStats: Extract[Defn.Trait, Stat] =
    Extract(_.templ.stats)

  implicit val extractObjectStats: Extract[Defn.Object, Stat] =
    Extract(_.templ.stats)

  implicit val extractPkgStats: Extract[Pkg, Stat] =
    Extract(_.stats)

  implicit val extractSourceStats: Extract[Source, Stat] =
    Extract(_.stats)

  implicit val extractDefStats: Extract[Defn.Def, Stat] =
    Extract(d => extractStatsFromTerm(d.body))

  implicit val extractValStats: Extract[Defn.Val, Stat] =
    Extract(v => extractStatsFromTerm(v.rhs))

  implicit val extractVarStats: Extract[Defn.Var, Stat] =
    Extract(
      _.rhs
        .map(extractStatsFromTerm)
        .getOrElse(Nil))

  private def extractStatsFromTerm(term: Term): List[Stat] =
    term match {
      case Term.Block(stats) => stats
      case s => s :: Nil
    }
}

object ExtractStatInstances extends ExtractStatInstances

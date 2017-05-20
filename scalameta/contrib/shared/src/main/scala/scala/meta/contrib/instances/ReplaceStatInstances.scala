package scala.meta.contrib.instances

import scala.meta._
import scala.meta.contrib._

trait ReplaceStatInstances {

  // NOTE: We presume when using replace that the
  // stat block is always generated, even if
  // supplied with an empty stat list
  // `new Foo` vs `new Foo {}` (We use the latter)
  implicit val replaceTemplateStats: Replace[Template, Stat] =
    Replace((a, bs) => a.copy(stats = bs))

  implicit val replaceClassStats: Replace[Defn.Class, Stat] =
    Replace((a, bs) => a.copy(templ = a.templ.copy(stats = bs)))

  implicit val replaceTraitStats: Replace[Defn.Trait, Stat] =
    Replace((a, bs) => a.copy(templ = a.templ.copy(stats = bs)))

  implicit val replaceObjectStats: Replace[Defn.Object, Stat] =
    Replace((a, bs) => a.copy(templ = a.templ.copy(stats = bs)))

  implicit val replaceDefStats: Replace[Defn.Def, Stat] =
    Replace((a, bs) => a.copy(body = statsToTerm(bs)))

  implicit val replaceValStats: Replace[Defn.Val, Stat] =
    Replace((a, bs) => a.copy(rhs = statsToTerm(bs)))

  // NOTE: We do not cover the None case for rhs,
  // we presume a block is required.
  implicit val replaceVarStats: Replace[Defn.Var, Stat] =
    Replace((a, bs) => a.copy(rhs = Some(statsToTerm(bs))))

  private def statsToTerm(bs: List[Stat]): Term = {
    bs match {
      case (head: Term) :: Nil => head
      case _ => Term.Block(bs)
    }
  }
}

object ReplaceStatInstances extends ReplaceStatInstances

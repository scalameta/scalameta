package scala.meta.contrib.instances

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.implicits.Statements

trait StatementsInstances {
  implicit val termBlockStats: Statements[Term.Block] = new Statements[Term.Block] {
    override def getStats(block: Term.Block): Seq[Stat] = block.stats

    override def replaceStats(block: Term.Block, newStats: Seq[Stat]): Term.Block =
      block.copy(stats = newStats)
  }

  implicit val packageStats: Statements[Pkg] = new Statements[Pkg] {
    override def getStats(pkg: Pkg): Seq[Stat] = pkg.stats

    // TODO: Packages have restrictions that all others dont.
    // aka top level stats only
    override def replaceStats(pkg: Pkg, newStats: Seq[Stat]): Pkg =
      pkg.copy(stats = newStats)
  }

  implicit val templStats: Statements[Template] = new Statements[Template] {
    override def getStats(templ: Template): Seq[Stat] =
      templ.stats match {
        case Some(stats) => stats
        case None => Nil
      }

    override def replaceStats(templ: Template, newStats: Seq[Stat]): Template = {
      newStats match {
        case Nil => templ.copy(stats = None)
        case s => templ.copy(stats = Some(s))
      }
    }
  }

  implicit val classStats: Statements[Defn.Class] = new Statements[Defn.Class] {
    override def getStats(clazz: Defn.Class): Seq[Stat] =
      clazz.templ.getStats

    override def replaceStats(clazz: Defn.Class, newStats: Seq[Stat]): Defn.Class = {
      clazz.copy(templ = clazz.templ.replaceStats(newStats))
    }
  }

  implicit val objectStats: Statements[Defn.Object] = new Statements[Defn.Object] {
    override def getStats(obj: Defn.Object): Seq[Stat] =
      obj.templ.getStats

    override def replaceStats(obj: Defn.Object, newStats: Seq[Stat]): Defn.Object =
      obj.copy(templ = obj.templ.replaceStats(newStats))
  }

  implicit val traitStats: Statements[Defn.Trait] = new Statements[Defn.Trait] {
    override def getStats(trat: Defn.Trait): Seq[Stat] =
      trat.templ.getStats

    override def replaceStats(trat: Defn.Trait, newStats: Seq[Stat]): Defn.Trait =
      trat.copy(templ = trat.templ.replaceStats(newStats))
  }

  // All terms are stats, but not all stats are terms
  // The RHS must be a term, or Seq[Stat] wrapped in a Term.Block
  implicit val varStats: Statements[Defn.Var] = new Statements[Defn.Var] {
    override def getStats(varr: Defn.Var): Seq[Stat] = {
      varr.rhs match {
        case None => Nil
        case Some(term) => extractStatsFromTerm(term)

      }
    }

    override def replaceStats(varr: Defn.Var, newStats: Seq[Stat]): Defn.Var = {
      val maybeTerm = newStats match {
        case Nil => None
        case s => Some(createTermFromStats(s))
      }
      varr.copy(rhs = maybeTerm)
    }
  }

  implicit val valStats: Statements[Defn.Val] = new Statements[Defn.Val] {
    override def getStats(vall: Defn.Val): Seq[Stat] =
      extractStatsFromTerm(vall.rhs)

    override def replaceStats(vall: Defn.Val, newStats: Seq[Stat]): Defn.Val =
      vall.copy(rhs = createTermFromStats(newStats))
  }

  implicit val defStats: Statements[Defn.Def] = new Statements[Defn.Def] {
    override def getStats(deff: Defn.Def): Seq[Stat] =
      extractStatsFromTerm(deff.body)

    override def replaceStats(deff: Defn.Def, newStats: Seq[Stat]): Defn.Def =
      deff.copy(body = createTermFromStats(newStats))
  }

  // TODO: Should this be another typeclass instance?
  // Does it make sense to call getStats on an arbitrary Term???
  private def extractStatsFromTerm(term: Term): Seq[Stat] =
    term match {
      case Term.Block(stats) => stats
      case s => s :: Nil
    }

  // TODO: The isInstanceof seems ugly. consider better options
  private def createTermFromStats(stats: Seq[Stat]): Term =
    stats match {
      case head :: Nil if head.isInstanceOf[Term] => head.asInstanceOf[Term]
      case s => Term.Block(s)
    }
}



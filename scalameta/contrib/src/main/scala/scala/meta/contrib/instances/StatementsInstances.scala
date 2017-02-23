package scala.meta.contrib.instances

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._
import scala.meta.contrib.implicits.Statements

trait StatementsInstances {
  implicit val termBlockStats: Statements[Term.Block] = new Statements[Term.Block] {
    override def getStats(block: Term.Block): Seq[Stat] = block.stats

    override def withStats(block: Term.Block, newStats: Seq[Stat]): Term.Block =
      block.copy(stats = newStats)
  }

  implicit val sourceStats: Statements[Source] = new Statements[Source] {
    override def getStats(source: Source): Seq[Stat] = source.stats

    override def withStats(source: Source, newStats: Seq[Stat]): Source =
      source.copy(stats = newStats)
  }

  implicit val packageStats: Statements[Pkg] = new Statements[Pkg] {
    override def getStats(pkg: Pkg): Seq[Stat] =
      pkg.stats match {
        case (p @ Pkg(_, _)) :: Nil =>
          p.stats
        case s =>
          s
      }

    // TODO: Packages have restrictions that all others dont.
    // aka top level stats only. But do we care if people break this??
    override def withStats(pkg: Pkg, newStats: Seq[Stat]): Pkg =
      pkg.copy(stats = newStats)
  }

  implicit val templStats: Statements[Template] = new Statements[Template] {
    override def getStats(templ: Template): Seq[Stat] =
      templ.stats.getOrElse(Nil)

    override def withStats(templ: Template, newStats: Seq[Stat]): Template = {
      newStats match {
        case Nil => templ.copy(stats = None)
        case s => templ.copy(stats = Some(s))
      }
    }
  }

  implicit val classStats: Statements[Defn.Class] = new Statements[Defn.Class] {
    override def getStats(clazz: Defn.Class): Seq[Stat] =
      clazz.templ.getStats

    override def withStats(clazz: Defn.Class, newStats: Seq[Stat]): Defn.Class = {
      clazz.copy(templ = clazz.templ.withStats(newStats))
    }
  }

  implicit val objectStats: Statements[Defn.Object] = new Statements[Defn.Object] {
    override def getStats(obj: Defn.Object): Seq[Stat] =
      obj.templ.getStats

    override def withStats(obj: Defn.Object, newStats: Seq[Stat]): Defn.Object =
      obj.copy(templ = obj.templ.withStats(newStats))
  }

  implicit val traitStats: Statements[Defn.Trait] = new Statements[Defn.Trait] {
    override def getStats(trat: Defn.Trait): Seq[Stat] =
      trat.templ.getStats

    override def withStats(trat: Defn.Trait, newStats: Seq[Stat]): Defn.Trait =
      trat.copy(templ = trat.templ.withStats(newStats))
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

    override def withStats(varr: Defn.Var, newStats: Seq[Stat]): Defn.Var = {
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

    override def withStats(vall: Defn.Val, newStats: Seq[Stat]): Defn.Val =
      vall.copy(rhs = createTermFromStats(newStats))
  }

  implicit val defStats: Statements[Defn.Def] = new Statements[Defn.Def] {
    override def getStats(deff: Defn.Def): Seq[Stat] =
      extractStatsFromTerm(deff.body)

    override def withStats(deff: Defn.Def, newStats: Seq[Stat]): Defn.Def =
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



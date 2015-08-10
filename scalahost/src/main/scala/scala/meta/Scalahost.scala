package scala.meta

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.Global
import scala.meta.hosts.{scalac => impl}
import scala.meta.taxonomic.{Context => TaxonomicContext}

object Mirror {
  def apply(modules: Module*)(implicit taxonomy: TaxonomicContext): impl.Mirror = {
    impl.Mirror(modules: _*)
  }
}

object Toolbox {
  def apply(modules: Module*)(implicit taxonomy: TaxonomicContext): impl.Toolbox = {
    impl.Toolbox(modules: _*)
  }

  def apply(options: String, modules: Module*)(implicit taxonomy: TaxonomicContext): impl.Toolbox = {
    impl.Toolbox(options, modules: _*)
  }
}

object Proxy {
  def apply[G <: Global](global: G): impl.Proxy[G] = {
    impl.Proxy[G](global)
  }
}

package scala.meta.internal
package scalahost

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.typechecker.ScalahostAnalyzer
import scala.meta.internal.scalahost.v1.LocationOps

class ScalahostPlugin(val global: Global)
    extends Plugin
    with HijackAnalyzer
    with LocationOps
    with ReflectionToolkit
    with ScalahostAnalyzer
    with ScalahostPipeline {
  val name = "scalahost"
  val description = "scala.meta's connector to the Scala compiler"
  hijackAnalyzer()
  val components = List[PluginComponent](ScalahostComponent)
}

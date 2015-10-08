// NOTE: has to be this package or otherwise we won't be able to access Context.issue
package scala.tools.nsc.typechecker

import scala.tools.nsc.Global
import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}
import org.scalameta.invariants._
import org.scalameta.reflection._
import scala.meta.internal.hosts.scalac.reflect._
import scala.reflect.internal.Mode
import scala.reflect.internal.Mode._
import scala.reflect.internal.util.{Statistics, ListOfNil}
import scala.tools.nsc.typechecker.TypersStats._
import scala.reflect.internal.Flags
import scala.reflect.internal.Flags._
import scala.collection.mutable

trait ScalahostAnalyzer extends NscAnalyzer with GlobalToolkit {
  val global: Global
  import global._
  import definitions._

  val stableCurrentRun = global.currentRun
  import stableCurrentRun.runDefinitions._

  override def newTyper(context: Context) = new ScalahostTyper(context)
  class ScalahostTyper(context0: Context) extends Typer(context0)
}
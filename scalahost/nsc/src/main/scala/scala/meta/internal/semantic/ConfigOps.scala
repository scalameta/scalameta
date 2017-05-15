package scala.meta.internal
package semantic

import scala.meta.io._
import scala.meta.internal.scalahost.ScalahostPlugin.name

case class ScalahostConfig(sourcepath: Sourcepath, semanticdb: SemanticdbMode) {
  def syntax = s"-P:$name:sourcepath:$sourcepath -P:$name:semanticdb:$sourcepath"
}
object ScalahostConfig {
  def default = ScalahostConfig(new Sourcepath(sys.props("user.dir")), SemanticdbMode.Fat)
}
sealed abstract class SemanticdbMode {
  import SemanticdbMode._
  def isSlim: Boolean = this == Slim
  def isFat: Boolean = this == Fat
  def isDisabled: Boolean = this == Disabled
}
object SemanticdbMode {
  def unapply(arg: String): Option[SemanticdbMode] =
    all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(Fat, Slim, Disabled)
  case object Fat extends SemanticdbMode
  case object Slim extends SemanticdbMode
  case object Disabled extends SemanticdbMode
}
trait ConfigOps { self: DatabaseOps =>
  val SetSemanticdb = "semanticdb:(.*)".r
  val SetSourcepath = "sourcepath:(.*)".r
  var config: ScalahostConfig = ScalahostConfig.default
}

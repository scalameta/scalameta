package scala.meta.internal
package semanticdb

import scala.meta.internal.io.PathIO
import scala.meta.internal.SemanticdbPlugin
import scala.meta.io._

case class SemanticdbConfig(
    sourceroot: AbsolutePath,
    mode: SemanticdbMode,
    failures: FailureMode,
    members: MemberMode) {
  def syntax: String =
    s"-P:${SemanticdbPlugin.name}:sourceroot:$sourceroot " +
      s"-P:${SemanticdbPlugin.name}:mode:$mode" +
      s"-P:${SemanticdbPlugin.name}:failures:${failures.name} "
}
object SemanticdbConfig {
  def default =
    SemanticdbConfig(
      PathIO.workingDirectory,
      SemanticdbMode.Fat,
      FailureMode.Warning,
      MemberMode.None)
}

sealed abstract class FailureMode {
  def name: String = toString.toLowerCase
}
object FailureMode {
  def unapply(arg: String): Option[FailureMode] =
    all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(Error, Warning, Info, Ignore)
  case object Error extends FailureMode
  case object Warning extends FailureMode
  case object Info extends FailureMode
  case object Ignore extends FailureMode
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

sealed abstract class MemberMode {
  import MemberMode._
  def isAll: Boolean = this == All
}
object MemberMode {
  def unapply(arg: String): Option[MemberMode] =
    all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(All, None)
  case object All extends MemberMode
  case object None extends MemberMode
}

trait ConfigOps { self: DatabaseOps =>
  val SetMode = "mode:(.*)".r
  val SetSourceroot = "sourceroot:(.*)".r
  val SetFailures = "failures:(.*)".r
  val SetMembers = "members:(.*)".r

  var config: SemanticdbConfig = SemanticdbConfig.default
  implicit class XtensionSemanticdbConfig(ignored: SemanticdbConfig) {
    def setSourceroot(sourceroot: AbsolutePath): Unit =
      config = config.copy(sourceroot = sourceroot)
    def setMode(mode: SemanticdbMode): Unit =
      config = config.copy(mode = mode)
    def setFailures(severity: FailureMode): Unit =
      config = config.copy(failures = severity)
    def setMembers(members: MemberMode): Unit =
      config = config.copy(members = members)
  }
}

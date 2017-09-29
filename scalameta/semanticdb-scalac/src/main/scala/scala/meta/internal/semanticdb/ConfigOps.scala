package scala.meta.internal
package semanticdb

import scala.meta.internal.io.PathIO
import scala.meta.internal.SemanticdbPlugin
import scala.meta.io._

case class SemanticdbConfig(
    sourceroot: AbsolutePath,
    mode: SemanticdbMode,
    failures: FailureMode,
    members: MemberMode,
    denotations: DenotationMode,
    profiling: ProfilingMode
    ) {
  def syntax: String =
    s"-P:${SemanticdbPlugin.name}:sourceroot:$sourceroot " +
      s"-P:${SemanticdbPlugin.name}:mode:${mode.name}" +
      s"-P:${SemanticdbPlugin.name}:failures:${failures.name} " +
      s"-P:${SemanticdbPlugin.name}:denotations:${denotations.name} " +
      s"-P:${SemanticdbPlugin.name}:profiling:${profiling.name} "
}
object SemanticdbConfig {
  def default = SemanticdbConfig(
    PathIO.workingDirectory,
    SemanticdbMode.Fat,
    FailureMode.Warning,
    MemberMode.None,
    DenotationMode.All,
    ProfilingMode.Off
  )
}

sealed abstract class SemanticdbMode {
  def name: String = toString.toLowerCase
  import SemanticdbMode._
  def isSlim: Boolean = this == Slim
  def isFat: Boolean = this == Fat
  def isDisabled: Boolean = this == Disabled
}
object SemanticdbMode {
  def unapply(arg: String): Option[SemanticdbMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(Fat, Slim, Disabled)
  case object Fat extends SemanticdbMode
  case object Slim extends SemanticdbMode
  case object Disabled extends SemanticdbMode
}

sealed abstract class FailureMode {
  def name: String = toString.toLowerCase
}
object FailureMode {
  def unapply(arg: String): Option[FailureMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(Error, Warning, Info, Ignore)
  case object Error extends FailureMode
  case object Warning extends FailureMode
  case object Info extends FailureMode
  case object Ignore extends FailureMode
}

sealed abstract class DenotationMode {
  def name: String = toString.toLowerCase
  import DenotationMode._
  def saveDefinitions: Boolean = this == All || this == Definitions
  def saveReferences: Boolean = this == All
}
object DenotationMode {
  def unapply(arg: String): Option[DenotationMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(All, Definitions, None)
  case object All extends DenotationMode
  case object Definitions extends DenotationMode
  case object None extends DenotationMode
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

sealed abstract class ProfilingMode {
  def name: String = toString.toLowerCase
  import ProfilingMode._
  def isConsole: Boolean = this == Console
  def isOff: Boolean = this == Off
}
object ProfilingMode {
  def unapply(arg: String): Option[ProfilingMode] = all.find(_.toString.equalsIgnoreCase(arg))
  def all = List(Console, Off)
  case object Console extends ProfilingMode
  case object Off extends ProfilingMode
}

trait ConfigOps { self: DatabaseOps =>
  val SetSourceroot = "sourceroot:(.*)".r
  val SetMode = "mode:(.*)".r
  val SetFailures = "failures:(.*)".r
  val SetMembers = "members:(.*)".r
  val SetDenotations = "denotations:(.*)".r
  val SetProfiling = "profiling:(.*)".r

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
    def setDenotations(denotations: DenotationMode): Unit =
      config = config.copy(denotations = denotations)
    def setProfiling(profiling: ProfilingMode): Unit =
      config = config.copy(profiling = profiling)
  }
}

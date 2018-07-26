package org.scalameta
package build

import sbt._
import Keys._
import sbt.complete.Parser

// Helper to execute command from tasks.
object RunSbtCommand {

  // copied from https://github.com/sbt/sbt-release/blob/0d79966dfe65a60ac97d8084691ce396bbaaede7/src/main/scala-sbt-1.0/Compat.scala
  type Command = sbt.Exec
  implicit def command2String(command: Command) = command.commandLine
  implicit def string2Exex(s: String): Command = sbt.Exec(s, None, None)
  private val FailureCommand: Exec = "--failure--"

  // copied from https://github.com/sbt/sbt-release/blob/e6b1574b00cbbf71dd24b500520f4c56e8cc492b/src/main/scala/ReleasePlugin.scala#L102-L115
  def apply(command: String): State => State = { initState: State =>
    @annotation.tailrec
    def runCommand(command: Exec, state: State): State = {
      val nextState = Parser.parse(command.commandLine, state.combinedParser) match {
        case Right(cmd) => cmd()
        case Left(msg) => throw sys.error(s"Invalid programmatic input:\n$msg")
      }
      nextState.remainingCommands.toList match {
        case Nil => nextState.copy(remainingCommands = initState.remainingCommands)
        case FailureCommand :: tail => nextState.copy(remainingCommands = FailureCommand +: initState.remainingCommands)
        case head :: tail => runCommand(head, nextState.copy(remainingCommands = tail))
      }
    }
    runCommand(command, initState.copy(remainingCommands = Nil))
  }
}

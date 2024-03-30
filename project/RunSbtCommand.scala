package org.scalameta
package build

import sbt._
import sbt.complete.DefaultParsers._
import sbt.complete.Parser

import Keys._

// Helper to execute command from tasks.
// Note: This was copied from https://github.com/sbt/sbt-release/blob/663cfd426361484228a21a1244b2e6b0f7656bdf/src/main/scala/ReleasePlugin.scala#L99-L115
object RunSbtCommand {
  final val FailureCommand = Exec("--failure--", None, None)

  def apply(command: String): State => State = { initState: State =>
    @annotation.tailrec
    def runCommand(command: Exec, state: State): State = {
      val nextState = Parser.parse(command.commandLine, state.combinedParser) match {
        case Right(cmd) => cmd()
        case Left(msg) => throw sys.error(s"Invalid programmatic input:\n$msg")
      }
      nextState.remainingCommands match {
        case Nil => nextState.copy(remainingCommands = initState.remainingCommands)
        case FailureCommand :: tail => nextState
            .copy(remainingCommands = FailureCommand +: initState.remainingCommands)
        case head :: tail => runCommand(head, nextState.copy(remainingCommands = tail))
      }
    }
    runCommand(Exec(command, None, None), initState.copy(remainingCommands = Nil))
  }
}

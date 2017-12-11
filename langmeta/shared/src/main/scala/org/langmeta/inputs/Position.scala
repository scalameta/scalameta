package org.langmeta
package inputs

import scala.io.AnsiColor._

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Position.end can point to the last character plus one.
//
// NOTE: All numbers here are zero-based, namely:
// * Offset 0 is the first character in the input.
// * Line 0 is the first line in the input.
// * Column 0 is the first character in the line.
// -1 is the sentinel value used in Position.None.
sealed trait Position {
  def input: Input
  def start: Int
  def startLine: Int
  def startColumn: Int
  def end: Int
  def endLine: Int
  def endColumn: Int
  def text: String
}

object Position {
  case object None extends Position {
    def input = Input.None
    def start = -1
    def startLine = -1
    def startColumn = -1
    def end = -1
    def endLine = -1
    def endColumn = -1
    def text = ""
    override def toString = "Position.None"
  }

  final case class Range(input: Input, start: Int, end: Int) extends Position {
    def startLine: Int = input.offsetToLine(start)
    def startColumn: Int = start - input.lineToOffset(startLine)
    def endLine: Int = input.offsetToLine(end)
    def endColumn: Int = end - input.lineToOffset(endLine)
    override def text = new String(input.chars, start, end - start)

    private def highlightLine(text: String, start: Int, end: Int): String = {
      if(startLine == endLine) {
        val startLineColumn = input.lineToOffset(startLine)
        val endLineColumn = input.lineToOffset(endLine)
        val highlight = UNDERLINED + B_BLACK + WHITE + text.slice(start, end) + RESET
        text.slice(startLineColumn, start) + highlight + text.slice(end, endLineColumn)
      } else {
        ""
      }
    }

    private def inputSyntax(input: Input): String = {
      val path =
        input match {
          case Input.VirtualFile(path, _) => path
          case Input.File(path, _) => path




          // private def inputSyntax(input: Input)
          // 
          case Input.Synthetic(value, input, start2, end2) => 
            Position.Range(input, start2, end2))


          case Input.Slice(input, start2, end2) => 
            Position.Range(input, start2, end2))

          case Input.Denotation(_, symbol) => symbol.syntax
          case _: Input.String => "<string>"
          case _: Input.Stream => "<stream>"
          case _: Input.None => "<none>"
        }

      s"${path}:${startLine}:${startColumn}"
    }

    override def toString = {
      input match {
        case Input.VirtualFile(path, _) =>
        case Input.File(path, _) =>
        case Input.Synthetic(_, input, _, _) =>
        case Input.Slice(input, _, _) =>
        case _: Input.String => ""
        case _: Input.Denotation => ""
        case _: Input.Stream => default
        case _: Input.None => ""
      }

      // def default = highlightLine(text, start, end)

      s"${highlight}"
    }
  }
}

// <denotation> _root_.toString
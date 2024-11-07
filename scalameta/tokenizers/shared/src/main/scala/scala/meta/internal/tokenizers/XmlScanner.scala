package scala.meta.internal.tokenizers

import scala.meta.Dialect
import scala.meta.inputs.Input
import scala.meta.internal.tokens.Chars

import scala.annotation.switch
import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import fastparse.NoWhitespace._
import fastparse._

/**
 * This is a simplified scanner, no validation will be done except checking for proper tag ending.
 */
class XmlScanner(dialect: Dialect, inputText: String, startIndex: Int) {

  private val _splicePositions = List.newBuilder[XmlScanner.XmlTokenRange]

  private val xmlParsedTag = "xml:unparsed"
  private val cdataStart = "<![CDATA["
  private val cdataEnd = "]]>"
  private val commentStart = "<!--"
  private val commentEnd = "-->"
  private val whitespaceChars = Set(' ', '\t', '\n', '\r')
  def splicePositions: List[XmlScanner.XmlTokenRange] = _splicePositions.result()

  private var currIndex = startIndex
  def lastIndex() = currIndex

  def scan(): Try[Int] = Try {
    var openedTags = 0
    var prevChar: Char = ' '
    var inString = false
    var ignoreProc = false
    var ignoreCDATA = false
    var ignoreComment = false
    var firstLoop = true

    def startsString(name: String) = currIndex + name.size <= inputText.size &&
      inputText.substring(currIndex, currIndex + name.size) == name

    def skipUntilClosingAngleBracket() = {
      while (currIndex < inputText.size && inputText(currIndex) != '>') currIndex += 1
      currIndex += 1
    }

    while ((openedTags > 0 || firstLoop) && currIndex < inputText.size) {
      firstLoop = false

      inputText(currIndex) match {
        // STOP Ignoring <? ?> <- end processing instructions
        case '>' if ignoreProc && prevChar == '?' =>
          ignoreProc = false
          openedTags -= 1
        // STOP Ignoring </xml:unparsed>
        case '/' if ignoreProc && prevChar == '<' && startsString(s"/$xmlParsedTag") =>
          // we opened a new tag on `<`, which is actually a closing one
          openedTags -= 1
          ignoreProc = false
          skipUntilClosingAngleBracket()
        // STOP ignoring [CDATA[
        case ']' if startsString(cdataEnd) && ignoreCDATA =>
          openedTags -= 1
          ignoreCDATA = false
          currIndex += cdataEnd.size
        // Inside ignored block
        case _ if ignoreProc || ignoreCDATA => currIndex += 1
        // START Ignoring <? ?> <-  processing instructions, ignore everything until the end
        case '?' if prevChar == '<' =>
          ignoreProc = true
          currIndex += 1
        // START Ignoring <xml:unparsed> <- ignore everything until the end tag
        case 'x' if prevChar == '<' && startsString(xmlParsedTag) =>
          ignoreProc = true
          currIndex += 1
        // START Ignoring `<![CDATA[` <- ignore everything until the end `]]>`
        case '<' if startsString(cdataStart) =>
          ignoreCDATA = true
          openedTags += 1
          currIndex += cdataStart.size
        // START ignoring comment
        case '<' if startsString(commentStart) =>
          ignoreComment = true
          openedTags += 1
          currIndex += commentStart.size
        case '<' if !startsString("</") =>
          openedTags += 1
          currIndex += 1
        case '>' if prevChar == '/' =>
          openedTags -= 1
          currIndex += 1
        case '/' if prevChar == '<' =>
          // we opened a new tag on `<`, which is actually a closing one
          openedTags -= 1
          currIndex += 1
          skipUntilClosingAngleBracket()
        // End comment <--! -->
        case '-' if ignoreComment && startsString(commentEnd) =>
          openedTags -= 1
          ignoreComment = false
          currIndex += commentEnd.size
        case '"' =>
          inString = !inString
          currIndex += 1
        // escape `{{`
        case '{' if startsString("{{") => currIndex += 2
        case '{' if !inString =>
          val newIndex = scalaExpression(currIndex + 1, inputText)
          currIndex = newIndex
        case _ => currIndex += 1
      }
      prevChar = inputText(currIndex - 1)
    }

    if (currIndex > inputText.size) throw new Exception("Unexpected end of file")
    if (openedTags > 0) throw new Exception("No end tag found")
    currIndex
  }.flatMap { result =>
    var tmpIndex = result
    // if we have another open tag continue scanning
    while (tmpIndex < inputText.size && whitespaceChars(inputText(tmpIndex))) tmpIndex += 1
    if (tmpIndex < inputText.size && inputText(tmpIndex) == '<') {
      currIndex = tmpIndex
      scan()
    } else Success(result)
  }

  def scalaExpression(index: Int, text: String) = {
    val input = Input.String(text.slice(index, text.length))
    val scanner = new LegacyScanner(input, dialect)
    scanner.initialize()

    def getNextIndex(ltd: LegacyTokenData) = index + ltd.offset

    @tailrec
    def rec(curlyBraceCount: Int): Int = {
      val ltd = scanner.nextToken()
      (ltd.token: @switch) match {
        case LegacyToken.RBRACE if curlyBraceCount == 0 =>
          val nextIndex = getNextIndex(ltd)
          _splicePositions += XmlScanner.XmlTokenRange(index, nextIndex)
          nextIndex
        case LegacyToken.EOF => throw new Exception("Unexpected end of file")
        case LegacyToken.LBRACE => rec(curlyBraceCount + 1)
        case LegacyToken.RBRACE => rec(curlyBraceCount - 1)
        case _ => rec(curlyBraceCount)
      }
    }
    rec(0)

  }
}

object XmlScanner {
  case class XmlTokenRange(from: Int, to: Int) // from is inclusive, to is exclusive
}

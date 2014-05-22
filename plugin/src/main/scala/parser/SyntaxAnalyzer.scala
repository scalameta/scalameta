package scala.reflect.internal.hosts
package scalacompiler
package parser

import scala.tools.nsc.ast.parser.{SyntaxAnalyzer => NscSyntaxAnalyzer}
import scala.tools.nsc.ast.parser.BracePatch
import scala.tools.nsc.ast.parser.Tokens._
import scala.tools.nsc.{Global, Phase, SubComponent}
import scala.reflect.internal.Flags
import scala.collection.mutable.ListBuffer
import macros.Common

abstract class SyntaxAnalyzer extends NscSyntaxAnalyzer with Common {
  import global._

  val runsAfter = List[String]()
  val runsRightAfter = None
  override val initial = true

  private def initialUnitBody(unit: CompilationUnit): Tree = {
    if (unit.isJava) new JavaUnitParser(unit).parse()
    else if (global.reporter.incompleteHandled) new PalladiumUnitParser(unit).parse()
    else new PalladiumUnitParser(unit).smartParse()
  }

  def newUnitScanner(unit: CompilationUnit): UnitScanner = new PalladiumUnitScanner(unit)
  private class PalladiumUnitScanner(unit: CompilationUnit, patches: List[BracePatch]) extends UnitScanner(unit, patches) {
    def this(unit: CompilationUnit) = this(unit, List())

    val allowIdentSetter = classOf[scala.tools.nsc.ast.parser.SyntaxAnalyzer#Scanner].getDeclaredMethods().filter(_.getName == "allowIdent_$eq").head
    allowIdentSetter.setAccessible(true)
    allowIdentSetter.invoke(this, nme.MACROkw)

    override def inFirstOfStat(token: Token) = {
      if (token == IDENTIFIER && name == nme.MACROkw) true
      else super.inFirstOfStat(token)
    }

    override def nextTokenAllow(name: Name) = {
      if (name != nme.MACROkw) abort(s"unexpected keyword $name")
      nextToken()
    }
  }

  def newUnitParser(unit: CompilationUnit): UnitParser = new PalladiumUnitParser(unit)
  private class PalladiumUnitParser(unit: global.CompilationUnit, patches: List[BracePatch]) extends UnitParser(unit, patches) {
    def this(unit: global.CompilationUnit) = this(unit, Nil)
    override def withPatches(patches: List[BracePatch]): UnitParser = new UnitParser(unit, patches)
    override def newScanner() = new PalladiumUnitScanner(unit, patches)

    override def isExprIntroToken(token: Token) = !isMacro && super.isExprIntroToken(token)
    override def isDclIntro: Boolean = isMacro || super.isDclIntro

    override def defOrDcl(pos: Offset, mods: Modifiers): List[Tree] = {
      def tokenRange(token: TokenData) = r2p(token.offset, token.offset, token.offset + token.name.length - 1)
      if (isMacro) List(macroDef(pos, (mods | Flags.MACRO) withPosition(MACRO, tokenRange(in))))
      else super.defOrDcl(pos, mods)
    }

    def macroDef(start: Offset, mods: Modifiers): DefDef = {
      in.nextToken()
      val nameOffset = in.offset
      val name = ident()
      val result = atPos(start, if (name.toTermName == nme.ERROR) start else nameOffset) {
        val contextBoundBuf = new ListBuffer[Tree]
        val tparams = typeParamClauseOpt(name, contextBoundBuf)
        val vparamss = paramClauses(name, contextBoundBuf.toList, ofCaseClass = false)
        val (isBlackbox, restype) = {
          if (in.token == SUBTYPE) { in.nextToken(); (false, typ()) }
          else { accept(COLON); (true, typ()) }
        }
        accept(EQUALS)
        PalladiumMacro(mods, name.toTermName, tparams, vparamss, isBlackbox, restype, expr())
      }
      signalParseProgress(result.pos)
      result
    }
  }

  override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
    override val checkable = false
    override val keepsTypeParams = false

    def apply(unit: CompilationUnit) {
      informProgress("parsing " + unit)
      // if the body is already filled in, don't overwrite it
      // otherwise compileLate is going to overwrite bodies of synthetic source files
      if (unit.body == EmptyTree)
        unit.body = initialUnitBody(unit)

      if (settings.Yrangepos && !reporter.hasErrors)
        validatePositions(unit.body)

      if (settings.Ymemberpos.isSetByUser)
        new MemberPosReporter(unit) show (style = settings.Ymemberpos.value)
    }
  }
}

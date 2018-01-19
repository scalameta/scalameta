package scala.meta.internal
package semanticdb

import java.util
import scala.collection.mutable
import scala.reflect.internal.util._
import scala.reflect.internal.{Flags => gf}
import scala.{meta => m}
import org.langmeta.internal.semanticdb.{schema => s}
import scala.meta.internal.inputs._
import scala.meta.io.AbsolutePath
import org.langmeta.io.RelativePath

trait DocumentOps { self: DatabaseOps =>
  def validateCompilerState(): Unit = {
    if (!g.settings.Yrangepos.value) {
      sys.error("the compiler instance must have -Yrangepos enabled")
    }
    if (g.useOffsetPositions) {
      sys.error("The compiler instance must use range positions")
    }
    if (!g.settings.plugin.value.exists(_.contains("semanticdb"))) {
      sys.error("the compiler instance must use the semanticdb plugin")
    }
    if (!g.analyzer.getClass.getName.contains("HijackAnalyzer")) {
      println(g.analyzer.getClass.getName)
      sys.error("the compiler instance must use a hijacked analyzer")
    }
    if (g.phase.id < g.currentRun.phaseNamed("typer").id) {
      sys.error("the compiler phase must be not earlier than typer")
    }
    if (g.phase.id > g.currentRun.phaseNamed("patmat").id) {
      sys.error("the compiler phase must be not later than patmat")
    }
  }

  import scala.tools.nsc.ast.parser.Tokens.isIdentifier
  case class Iddent(start: Int, end: Int)
  object Iddent {
    import scala.collection.JavaConverters._
    def apply(unit: global.CompilationUnit): mutable.Map[Int, s.Position] = {
      val tokens = new util.TreeMap[Int, s.Position]()
      class Scan extends global.syntaxAnalyzer.UnitScanner(unit) {
        override def deprecationWarning(off: Int, msg: String, since: String) {}
        override def error(off: Int, msg: String) {}
        override def incompleteInputError(off: Int, msg: String) {}

        override def nextToken() {
          val offset0 = offset
          val code = token

          super.nextToken()

          if (isIdentifier(code)) {
            val length = (lastOffset - offset0).max(1)
            tokens.put(offset0, s.Position(offset0, offset0 + length))
          }
        }
      }
      val parser = new global.syntaxAnalyzer.UnitParser(unit) {
        override def newScanner = new Scan
      }
      parser.parse()
      tokens.asScala
    }
  }

  implicit class XtensionCompilationUnitDocument(unit: g.CompilationUnit) {
    def toRelpath: RelativePath = AbsolutePath(unit.source.file.file).toRelative(config.sourceroot)
    def toDocument: s.Document = {
      val names = mutable.Map[s.Position, s.ResolvedName]()
      type sSymbol = String
      val denotations = mutable.Map[sSymbol, s.Denotation]()
      val members = mutable.Map[sSymbol, List[m.Signature]]()
      val inferred = mutable.Map[s.Position, Inferred]().withDefaultValue(Inferred())
      val isVisited = mutable.Set.empty[g.Tree] // macro expandees can have cycles, keep tracks of visited nodes.
      val mstarts = Iddent(unit)
      val mends = mutable.Map[Int, s.Position]() // end offset -> tree

      locally {
        object traverser extends g.Traverser {
          private def tryFindMtree(gtree: g.Tree): Unit = {
            def success(spos: s.Position, gsym0: g.Symbol): Unit = {
              // We cannot be guaranteed that all symbols have a position, see
              // https://github.com/scalameta/scalameta/issues/665
              // Instead of crashing with "unsupported file", we ignore these cases.
              if (gsym0 == null) return
              if (gsym0.isAnonymousClass) return
//              if (spos.pos == m.Position.None) return
              if (names.contains(spos)) return // NOTE: in the future, we may decide to preempt preexisting db entries

              val gsym = {
                def isClassRefInCtorCall = gsym0.isConstructor // && spos.isNot[m.Name.Anonymous]
                if (gsym0 != null && isClassRefInCtorCall) gsym0.owner
                else gsym0 // TODO: fix this in callers of `success`
              }
              val symbol = gsym.toSemantic
              val ssymbol = symbol.syntax
              if (symbol == m.Symbol.None) return

              val isDefinition = gtree.isDef

              names(spos) =
                s.ResolvedName(position = Some(spos), symbol = ssymbol, isDefinition = isDefinition)

              def saveDenotation(): Unit = {
                if (!gsym.isOverloaded && gsym != g.definitions.RepeatedParamClass) {
                  denotations(ssymbol) = gsym.toDenotation
                }
                if (gsym.isClass && !gsym.isTrait) {
                  val gprim = gsym.primaryConstructor
                  if (gprim != g.NoSymbol) {
                    denotations(gprim.toSemantic.syntax) = gprim.toDenotation
                  }
                }
              }
              if (isDefinition && config.denotations.saveDefinitions) saveDenotation()
              if (!isDefinition && config.denotations.saveReferences) saveDenotation()
            }
            def tryMstart(start: Int): Boolean = {
              if (!mstarts.contains(start)) return false
              success(mstarts(start), gtree.symbol)
              return true
            }
            def tryMend(end: Int): Boolean = {
              if (!mends.contains(end)) return false
              success(mends(end), gtree.symbol)
              return true
            }
            def tryMpos(start: Int, end: Int): Boolean = {
              if (!mstarts.contains(start)) return false
              val mtree = mstarts(start)
              if (mtree.end != end) return false
              success(mtree, gtree.symbol)
              return true
            }

            if (gtree.pos == null || gtree.pos == NoPosition) return
            val gstart = gtree.pos.start
            val gpoint = gtree.pos.point
            val gend = gtree.pos.end

            if (config.members.isAll) {
              gtree match {
                case gtree: g.Template =>
                  gtree.parents.foreach { parent =>
                    members(parent.symbol.toSemantic.syntax) = parent.tpe.lookupMembers
                  }
                case _ =>
              }
            }

            // Ideally, we'd like a perfect match when gtree.pos == mtree.pos.
            // Unfortunately, this is often not the case as demonstrated by a bunch of cases above and below.
            if (tryMpos(gstart, gend)) return

            gtree match {
              case gtree: g.ValDef if gtree.symbol == gtree.symbol.owner.thisSym =>
                tryMstart(gstart)
              case gtree: g.MemberDef if gtree.symbol.isSynthetic || gtree.symbol.isArtifact =>
              // NOTE: never interested in synthetics except for the ones above
              case gtree: g.PackageDef =>
                if (config.members.isAll) {
                  members(gtree.symbol.toSemantic.syntax) = gtree.pid.tpe.lookupMembers
                }
              // NOTE: capture PackageDef.pid instead
              case gtree: g.ModuleDef if gtree.name == g.nme.PACKAGE =>
                // TODO: if a package object comes first in the compilation unit
                // then its positions are completely mental, so we just hack around
                tryMstart(gpoint + 7)
                tryMstart(gpoint)
              case gtree: g.ValDef =>
                tryMstart(gstart)
                tryMstart(gpoint)
              case gtree: g.MemberDef =>
                tryMstart(gpoint)
              case gtree: g.DefTree =>
                tryMstart(gpoint)
              case gtree: g.This =>
                tryMstart(gpoint)
              case gtree: g.Super =>
                // TODO: change the delta to account for whitespace and comments
                tryMend(gend - 1)
              case gtree: g.Select if gtree.symbol == g.definitions.NilModule =>
                // NOTE: List() gets desugared into mkAttributedRef(NilModule)
                tryMstart(gstart)
              case gtree: g.RefTree =>
                def prohibited(name: String) = {
                  name.contains(g.nme.DEFAULT_GETTER_STRING)
                }
                if (prohibited(gtree.name.decoded)) return
                tryMstart(gpoint)
              case gtree: g.Import =>
                val sels = gtree.selectors.flatMap { sel =>
                  if (sel.name == g.nme.WILDCARD && config.members.isAll) {
                    members(gtree.expr.symbol.toSemantic.syntax) = gtree.expr.tpe.lookupMembers
                    Nil
                  } else {
                    mstarts.get(sel.namePos).map(mname => (sel.name, mname))
                  }
                }
                sels.foreach {
                  case (gname, mname) =>
                    val import1 = gtree.expr.tpe.member(gname.toTermName)
                    val import2 = gtree.expr.tpe.member(gname.toTypeName)
                    success(
                      mname,
                      wrapAlternatives(
                        "<import " + gtree.expr + "." + gname + ">",
                        import1,
                        import2))
                }
              case _ =>
            }
          }

          private def tryFindInferred(gtree: g.Tree): Unit = {
            if (!config.synthetics.saveSynthetics) return

            import scala.meta.internal.semanticdb.{AttributedSynthetic => S}
            def success(pos: s.Position, f: Inferred => Inferred): Unit = {
              inferred(pos) = f(inferred(pos))
            }

            if (!gtree.pos.isRange) return

            object ApplySelect {
              def unapply(tree: g.Tree): Option[g.Select] = Option(tree).collect {
                case g.Apply(select: g.Select, _) => select
                case select: g.Select => select
              }
            }

            object ForComprehensionImplicitArg {
              private def isForComprehensionSyntheticName(select: g.Select): Boolean = {
                select.pos == select.qualifier.pos && (select.name == g.nme.map ||
                select.name == g.nme.withFilter ||
                select.name == g.nme.flatMap ||
                select.name == g.nme.foreach)
              }

              private def findSelect(t: g.Tree): Option[g.Tree] = t match {
                case g.Apply(fun, _) => findSelect(fun)
                case g.TypeApply(fun, _) => findSelect(fun)
                case s @ g.Select(qual, _) if isForComprehensionSyntheticName(s) => Some(qual)
                case _ => None
              }

              def unapply(gfun: g.Apply): Option[g.Tree] = findSelect(gfun)
            }

            gtree match {
              case gview: g.ApplyImplicitView =>
                val pos = gtree.pos.toSchema
                val syntax = showSynthetic(gview.fun) + "(" + S.star + ")"
                success(pos, _.copy(conversion = Some(syntax)))
                isVisited += gview.fun
              case gimpl: g.ApplyToImplicitArgs =>
                val args = S.mkString(gimpl.args.map(showSynthetic), ", ")
                gimpl.fun match {
                  case gview: g.ApplyImplicitView =>
                    isVisited += gview
                    val pos = gtree.pos.toSchema
                    val syntax = showSynthetic(gview.fun) + "(" + S.star + ")(" + args + ")"
                    success(pos, _.copy(conversion = Some(syntax)))
                  case ForComprehensionImplicitArg(qual) =>
                    val morePrecisePos = qual.pos.withStart(qual.pos.end).toSchema
                    val syntax = S("(") + S.star + ")" + "(" + args + ")"
                    success(morePrecisePos, _.copy(args = Some(syntax)))
                  case gfun =>
                    val morePrecisePos = gimpl.pos.withStart(gimpl.pos.end).toSchema
                    val syntax = S("(") + args + ")"
                    success(morePrecisePos, _.copy(args = Some(syntax)))
                }
              case g.TypeApply(fun, targs @ List(targ, _*)) =>
                if (targ.pos.isRange) return
                val morePrecisePos = fun.pos.withStart(fun.pos.end).toSchema
                val args = S.mkString(targs.map(showSynthetic), ", ")
                val syntax = S("[") + args + "]"
                success(morePrecisePos, _.copy(targs = Some(syntax)))
              case ApplySelect(select @ g.Select(qual, nme)) if isSyntheticName(select) =>
                val pos = qual.pos.withStart(qual.pos.end).toSchema
                val symbol = select.symbol.toSemantic.syntax
                val name = nme.decoded
                val names = List(SyntheticRange(0, name.length, symbol))
                val syntax = S(".") + S(nme.decoded, names)
                success(pos, _.copy(select = Some(syntax)))
              case _ =>
              // do nothing
            }
          }

          override def traverse(gtree: g.Tree): Unit = {
            if (isVisited(gtree)) return else isVisited += gtree
            gtree.attachments.all.foreach {
              case att: g.analyzer.MacroExpansionAttachment =>
                traverse(att.expandee)
              case _ =>
            }
            gtree match {
              case ConstfoldOf(original) =>
                traverse(original)
              case ClassOf(original) =>
                traverse(original)
              case NewArrayOf(original) =>
                traverse(original)
              case SingletonTypeTreeOf(original) =>
                traverse(original)
              case CompoundTypeTreeOf(original) =>
                traverse(original)
              case ExistentialTypeTreeOf(original) =>
                traverse(original)
              case AnnotatedOf(original) =>
                traverse(original)
              case SelfTypeOf(original) =>
                traverse(original)
              case SelectOf(original) =>
                traverse(original)
              case g.Function(params, body) if params.exists(_.name.decoded.startsWith("x$")) =>
                traverse(body)
              case gtree: g.TypeTree if gtree.original != null =>
                traverse(gtree.original)
              case gtree: g.TypeTreeWithDeferredRefCheck =>
                traverse(gtree.check())
              case gtree: g.MemberDef =>
                gtree.symbol.annotations.map(ann => traverse(ann.original))
                tryFindMtree(gtree)
              case _: g.Apply | _: g.TypeApply =>
                tryFindInferred(gtree)
              case select: g.Select if isSyntheticName(select) =>
                tryFindMtree(select.qualifier)
                tryFindInferred(select)
              case _ =>
                tryFindMtree(gtree)
            }
            super.traverse(gtree)
          }
        }
        traverser.traverse(unit.body)
      }

      val input = unit.source.toInput

      val synthetics = inferred.toIterator.map {
        case (pos, inferred) => inferred.toSynthetic(input, pos)
      }
      val symbols = denotations.map {
        case (sym, denot) =>
          val denotationWithMembers = members.get(sym).fold(denot) { members =>
            s.Denotation(
              denot.flags,
              denot.name,
              denot.signature,
              denot.names,
              members.map {
                // HACK(olafur) we don't encode names here to avoid double backtick ``c_=``
                case m.Signature.Term(value) => value + "."
                case m.Signature.Type(value) => value + "#"
                case els => els.syntax
              }
            )
          }
          s.ResolvedSymbol(sym, Some(denotationWithMembers))
      }.toList

      s.Document(
        filename = input.syntax,
        contents = input.text,
        language = language,
        names = names.values.toSeq,
        symbols = symbols,
        synthetics = synthetics.toList,
        messages = unit.reportedMessages(mstarts)
//        names.map { case (pos, sym) => m.ResolvedName(pos, sym, binders(pos)) }.toList,
      )
    }
  }

  private def isSyntheticName(select: g.Select): Boolean =
    select.pos == select.qualifier.pos &&
      (select.name == g.nme.apply ||
        select.name == g.nme.foreach ||
        select.name == g.nme.withFilter ||
        select.name == g.nme.flatMap ||
        select.name == g.nme.map ||
        select.name == g.nme.unapplySeq ||
        select.name == g.nme.unapply)

  private def syntaxAndPos(gtree: g.Tree): String = {
    if (gtree == g.EmptyTree) "\u001b[1;31mEmptyTree\u001b[0m"
    else {
      val text =
        gtree.toString.substring(0, Math.min(45, gtree.toString.length)).replace("\n", " ")
      s"$text [${gtree.pos.start}..${gtree.pos.end})"
    }
  }

  private def syntaxAndPos(mtree: m.Tree): String = {
    s"${mtree.pos.syntax} $mtree"
  }

  private def wrapAlternatives(name: String, alts: g.Symbol*): g.Symbol = {
    val normalizedAlts = {
      val alts1 = alts.toList.filter(_ != g.NoSymbol)
      val alts2 = alts1.map(alt => if (alt.isModuleClass) alt.asClass.module else alt)
      alts2.distinct
    }
    normalizedAlts match {
      case List(sym) =>
        sym
      case normalizedAlts =>
        val wrapper = g.NoSymbol.newTermSymbol(g.TermName(name))
        wrapper.setFlag(gf.OVERLOADED)
        wrapper.setInfo(g.OverloadedType(g.NoType, normalizedAlts))
    }
  }
}

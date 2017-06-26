package scala.meta.internal
package semantic

import org.scalameta.unreachable
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.reflect.internal.util._
import scala.reflect.internal.{Flags => gf}
import scala.{meta => m}
import scala.{meta => mf}
import scala.meta.internal.ast.Helpers._
import scala.meta.internal.inputs._

trait AttributesOps { self: DatabaseOps =>

  implicit class XtensionCompilationUnitAttributes(unit: g.CompilationUnit) {
    def toAttributes: m.Attributes = {
      unit.cache.getOrElse("attributes", {
        if (!g.settings.Yrangepos.value)
          sys.error("the compiler instance must have -Yrangepos enabled")
        if (g.useOffsetPositions) sys.error("The compiler instance must use range positions")
        if (!g.settings.plugin.value.exists(_.contains("scalahost")))
          sys.error("the compiler instance must use the scalahost plugin")
        if (!g.analyzer.getClass.getName.contains("scalahost")) {
          println(g.analyzer.getClass.getName)
          sys.error("the compiler instance must use a hijacked analyzer")
        }
        if (g.phase.id < g.currentRun.phaseNamed("typer").id)
          sys.error("the compiler phase must be not earlier than typer")
        if (g.phase.id > g.currentRun.phaseNamed("patmat").id)
          sys.error("the compiler phase must be not later than patmat")

        val names = mutable.Map[m.Position, m.Symbol]()
        val denotations = mutable.Map[m.Symbol, m.Denotation]()
        val inferred = mutable.Map[m.Position, String]()
        val todo = mutable.Set[m.Name]() // names to map to global trees
        val mstarts = mutable.Map[Int, m.Name]() // start offset -> tree
        val mends = mutable.Map[Int, m.Name]() // end offset -> tree
        val margnames = mutable.Map[Int, List[m.Name]]() // start offset of enclosing apply -> its arg names
        val mwithins = mutable.Map[m.Tree, m.Name]() // name of enclosing member -> name of private/protected within
        val mwithinctors = mutable.Map[m.Tree, m.Name]() // name of enclosing class -> name of private/protected within for primary ctor

        locally {
          object traverser extends m.Traverser {
            private def indexName(mname: m.Name): Unit = {
              todo += mname
              // TODO: also drop trivia (no idea how to formulate this concisely)
              val tok = mname.tokens.dropWhile(_.is[m.Token.LeftParen]).headOption
              val mstart1 = tok.map(_.start).getOrElse(-1)
              val mend1 = tok.map(_.end).getOrElse(-1)
              if (mstarts.contains(mstart1))
                sys.error(
                  s"ambiguous mstart ${syntaxAndPos(mname)} ${syntaxAndPos(mstarts(mstart1))}")
              if (mends.contains(mend1))
                sys.error(s"ambiguous mend ${syntaxAndPos(mname)} ${syntaxAndPos(mends(mend1))}")
              mstarts(mstart1) = mname
              mends(mend1) = mname
            }
            private def indexArgNames(mapp: m.Tree, mnames: List[m.Name]): Unit = {
              if (mnames.isEmpty) return
              todo ++= mnames
              // TODO: also drop trivia (no idea how to formulate this concisely)
              val mstart1 = mapp.tokens
                .dropWhile(_.is[m.Token.LeftParen])
                .headOption
                .map(_.start)
                .getOrElse(-1)
              // only add names for the top-level term.apply of a curried function application.
              if (!margnames.contains(mstart1))
                margnames(mstart1) = mnames
            }
            private def indexWithin(mname: m.Name.Indeterminate): Unit = {
              todo += mname
              val menclosing = mname.parent.flatMap(_.parent).get
              menclosing match {
                case menclosing: m.Ctor.Primary =>
                  val menclosingDefn = menclosing.parent.get.asInstanceOf[m.Member]
                  val menclosingName = menclosingDefn.name
                  if (mwithinctors.contains(menclosingName))
                    sys.error(
                      s"ambiguous mwithinctors ${syntaxAndPos(mname)} ${syntaxAndPos(mwithinctors(menclosingName))}")
                  mwithinctors(menclosingName) = mname
                case _ =>
                  def findBinder(pat: m.Pat) =
                    pat.collect { case m.Pat.Var.Term(name) => name }.head
                  val menclosingName = menclosing match {
                    case mtree: m.Member => mtree.name
                    case m.Decl.Val(_, pat :: Nil, _) => findBinder(pat)
                    case m.Decl.Var(_, pat :: Nil, _) => findBinder(pat)
                    case m.Defn.Val(_, pat :: Nil, _, _) => findBinder(pat)
                    case m.Defn.Var(_, pat :: Nil, _, _) => findBinder(pat)
                  }
                  if (mwithins.contains(menclosingName))
                    sys.error(
                      s"ambiguous mwithins ${syntaxAndPos(mname)} ${syntaxAndPos(mwithins(menclosingName))}")
                  mwithins(menclosingName) = mname
              }
            }
            override def apply(mtree: m.Tree): Unit = {
              val mstart = mtree.pos.start.offset
              val mend = mtree.pos.end.offset
              if (mstart != mend) {
                mtree match {
                  case mtree @ m.Term.Apply(_, margs) =>
                    def loop(term: m.Term): List[m.Term.Name] = term match {
                      case m.Term.Apply(mfn, margs) =>
                        margs.toList.collect {
                          case m.Term.Assign(mname: m.Term.Name, _) => mname
                        } ++ loop(mfn)
                      case _ => Nil
                    }
                    indexArgNames(mtree, loop(mtree))
                  case mtree @ m.Mod.Private(mname: m.Name.Indeterminate) =>
                    indexWithin(mname)
                  case mtree @ m.Mod.Protected(mname: m.Name.Indeterminate) =>
                    indexWithin(mname)
                  case mtree @ m.Importee.Rename(mname, mrename) =>
                    indexName(mname)
                    return // NOTE: ignore mrename for now, we may decide to make it a binder
                  case mtree @ m.Name.Anonymous() =>
                  // do nothing
                  case mtree: m.Name =>
                    indexName(mtree)
                  case _ =>
                  // do nothing
                }
              }
              super.apply(mtree)
            }
          }
          traverser(unit.toSource)
        }

        locally {
          object traverser extends g.Traverser {
            private def tryFindMtree(gtree: g.Tree): Unit = {
              def success(mtree: m.Name, gsym: g.Symbol): Unit = {
                // We cannot be guaranteed that all symbols have a position, see
                // https://github.com/scalameta/scalameta/issues/665
                // Instead of crashing with "unsupported file", we ignore these cases.
                if (mtree.pos == m.Position.None) return
                if (names.contains(mtree.pos)) return // NOTE: in the future, we may decide to preempt preexisting db entries

                val symbol = gsym.toSemantic
                if (symbol == m.Symbol.None) return

                names(mtree.pos) = symbol
                denotations(symbol) = gsym.toDenotation
                if (gsym.isClass && !gsym.isTrait) {
                  val gprim = gsym.primaryConstructor
                  denotations(gprim.toSemantic) = gprim.toDenotation
                }
                todo -= mtree

                def tryWithin(map: mutable.Map[m.Tree, m.Name], gsym0: g.Symbol): Unit = {
                  if (map.contains(mtree)) {
                    val gsym = gsym0.getterIn(gsym0.owner).orElse(gsym0)
                    if (!gsym.hasAccessBoundary) return
                    val within1 = gsym.privateWithin
                    val within2 = within1.owner.info.member(
                      if (within1.name.isTermName) within1.name.toTypeName
                      else within1.name.toTermName)
                    success(map(mtree),
                            wrapAlternatives("<within " + symbol + ">", within1, within2))
                  }
                }
                tryWithin(mwithins, gsym)
                tryWithin(mwithinctors, gsym.primaryConstructor)
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
                if (mtree.pos.end.offset != end) return false
                success(mtree, gtree.symbol)
                return true
              }

              if (gtree.pos == null || gtree.pos == NoPosition) return
              val gstart = gtree.pos.start
              val gpoint = gtree.pos.point
              val gend = gtree.pos.end

              if (margnames.contains(gstart) || margnames.contains(gpoint)) {
                (margnames.get(gstart) ++ margnames.get(gpoint)).flatten.foreach(margname => {
                  if (gtree.symbol != null && gtree.symbol.isMethod) {
                    val gsym = gtree.symbol.paramss.flatten.find(_.name.decoded == margname.value)
                    gsym.foreach(success(margname, _))
                  }
                })
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
                  val sels = gtree.selectors.flatMap(sel =>
                    mstarts.get(sel.namePos).map(mname => (sel.name, mname)))
                  sels.foreach {
                    case (gname, mname) =>
                      val import1 = gtree.expr.tpe.member(gname.toTermName)
                      val import2 = gtree.expr.tpe.member(gname.toTypeName)
                      success(mname,
                              wrapAlternatives("<import " + gtree.expr + "." + gname + ">",
                                               import1,
                                               import2))
                  }
                case _ =>
              }
            }

            private def tryFindInferred(gtree: g.Tree): Unit = {
              def success(pos: m.Position, syntax: String): Unit = {
                if (inferred.contains(pos)) return
                inferred(pos) = syntax
              }

              if (!gtree.pos.isRange) return
              gtree match {
                case gview: g.ApplyImplicitView =>
                  val pos = gtree.pos.toMeta
                  val syntax = gview.fun + "(*)"
                  success(pos, syntax)
                case gimpl: g.ApplyToImplicitArgs =>
                  gimpl.fun match {
                    case gview: g.ApplyImplicitView =>
                      val pos = gtree.pos.toMeta
                      val syntax = gview.fun + "(*)(" + gimpl.args.mkString(", ") + ")"
                      success(pos, syntax)
                    case gfun =>
                      val fullyQualifiedArgs = gimpl.args.map(g.showCode(_))
                      val morePrecisePos = gimpl.pos.withStart(gimpl.pos.end).toMeta
                      val syntax = "(" + fullyQualifiedArgs.mkString(", ") + ")"
                      success(morePrecisePos, syntax)
                  }
                case g.TypeApply(fun, targs @ List(targ, _ *)) =>
                  if (targ.pos.isRange) return
                  val morePrecisePos = fun.pos.withStart(fun.pos.end).toMeta
                  val syntax = "[" + targs.mkString(", ") + "]"
                  success(morePrecisePos, syntax)
                case _ =>
                // do nothing
              }
            }

            override def traverse(gtree: g.Tree): Unit = {
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
                case select: g.Select if isSyntheticApply(select) =>
                  tryFindMtree(select.qualifier)
                case _ =>
                  tryFindMtree(gtree)
              }
              super.traverse(gtree)
            }
          }
          traverser.traverse(unit.body)
        }

        val messages = unit.hijackedMessages.map {
          case (gpos, gseverity, msg) =>
            val mpos = {
              // NOTE: The caret in unused import warnings points to Importee.pos, but
              // the message position start/end point to the enclosing Import.pos.
              // See https://github.com/scalameta/scalameta/issues/839
              if (msg == "Unused import" && mstarts.contains(gpos.point)) mstarts(gpos.point).pos
              else gpos.toMeta
            }
            val mseverity = gseverity match {
              case 0 => m.Severity.Info
              case 1 => m.Severity.Warning
              case 2 => m.Severity.Error
              case _ => unreachable
            }
            m.Message(mpos, mseverity, msg)
        }
        m.Attributes(dialect, names.toList, messages.toList, denotations.toList, inferred.toList)
      })
    }
  }

  private def isSyntheticApply(select: g.Select): Boolean =
    select.pos == select.qualifier.pos &&
      select.name == g.nme.apply

  private def syntaxAndPos(gtree: g.Tree): String = {
    if (gtree == g.EmptyTree) "\u001b[1;31mEmptyTree\u001b[0m"
    else
      s"${gtree.toString
        .substring(0, Math.min(45, gtree.toString.length))
        .replace("\n", " ")} [${gtree.pos.start}..${gtree.pos.end})"
  }

  private def syntaxAndPos(mtree: m.Tree): String = {
    s"${mtree.pos.syntax} $mtree"
  }

  private def wrapAlternatives(name: String, alts: g.Symbol*): g.Symbol = {
    val normalizedAlts = {
      val alts1 = alts.toList.filter(_.exists)
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

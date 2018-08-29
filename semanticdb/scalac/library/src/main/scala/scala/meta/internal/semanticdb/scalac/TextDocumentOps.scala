package scala.meta.internal.semanticdb.scalac

import scala.collection.mutable
import scala.meta.internal.inputs._
import scala.meta.internal.io.PathIO
import scala.meta.internal.scalacp._
import scala.meta.internal.{semanticdb => s}
import scala.reflect.internal._
import scala.reflect.internal.util._
import scala.reflect.internal.{Flags => gf}
import scala.reflect.io.{PlainFile => GPlainFile}
import scala.{meta => m}
import scala.meta.internal.semanticdb.Scala._

trait TextDocumentOps { self: SemanticdbOps =>
  def validateCompilerState(): Unit = {
    if (!g.settings.Yrangepos.value) {
      sys.error("the compiler instance must have -Yrangepos enabled")
    }
    if (g.useOffsetPositions) {
      sys.error("the compiler instance must use range positions")
    }
    if (!g.settings.plugin.value.exists(_.contains("semanticdb"))) {
      sys.error("the compiler instance must use the semanticdb plugin")
    }
    val analyzerClassName = g.analyzer.getClass.getName
    if (!analyzerClassName.contains("HijackAnalyzer")) {
      sys.error(
        s"the compiler instance must use a hijacked analyzer, instead of $analyzerClassName")
    }
    if (g.currentRun.phaseNamed("typer") != NoPhase) {
      if (g.phase.id < g.currentRun.phaseNamed("typer").id) {
        sys.error("the compiler phase must be not earlier than typer")
      }
    } else {
      sys.error("the compiler instance does not have a typer phase")
    }
    if (g.currentRun.phaseNamed("patmat") != NoPhase) {
      if (g.phase.id > g.currentRun.phaseNamed("patmat").id) {
        sys.error("the compiler phase must be not later than patmat")
      }
    } else {
      // do nothing
    }
  }

  implicit class XtensionCompilationUnitDocument(unit: g.CompilationUnit) {
    def toTextDocument: s.TextDocument = {
      val binders = mutable.Set[m.Position]()
      val occurrences = mutable.Map[m.Position, String]()
      val symbols = mutable.Map[String, s.SymbolInformation]()
      val synthetics = mutable.ListBuffer[s.Synthetic]()
      val isVisited = mutable.Set.empty[g.Tree] // macro expandees can have cycles, keep tracks of visited nodes.
      val isVisitedParent = mutable.Set.empty[g.Tree] // synthetics we have already visited the parents of
      val todo = mutable.Set[m.Name]() // names to map to global trees
      val mstarts = mutable.Map[Int, m.Name]() // start offset -> tree
      val mends = mutable.Map[Int, m.Name]() // end offset -> tree
      val margnames = mutable.Map[Int, List[m.Name]]() // start offset of enclosing apply -> its arg names
      val mwithins = mutable.Map[m.Tree, m.Name]() // name of enclosing member -> name of private/protected within
      val mwithinctors = mutable.Map[m.Tree, m.Name]() // name of enclosing class -> name of private/protected within for primary ctor
      val mctordefs = mutable.Map[Int, m.Name]() // start offset of ctor -> ctor's anonymous name
      val mctorrefs = mutable.Map[Int, m.Name]() // start offset of new/init -> new's anonymous name

      locally {
        object traverser extends m.Traverser {
          private def indexName(mname: m.Name): Unit = {
            todo += mname
            val tok = mname.tokens.dropWhile(_.is[m.Token.LeftParen]).headOption
            val mstart1 = tok.map(_.start).getOrElse(mname.pos.start)
            val mend1 = tok.map(_.end).getOrElse(mname.pos.end)
            if (mstarts.contains(mstart1)) {
              val details = syntaxAndPos(mname) + " " + syntaxAndPos(mstarts(mstart1))
              sys.error(s"ambiguous mstart $details")
            }
            if (mends.contains(mend1)) {
              val details = syntaxAndPos(mname) + " " + syntaxAndPos(mends(mend1))
              sys.error(s"ambiguous mend $details")
            }
            mstarts(mstart1) = mname
            mends(mend1) = mname
          }
          private def indexArgNames(mapp: m.Tree, mnames: List[m.Name]): Unit = {
            if (mnames.isEmpty) return
            todo ++= mnames
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
            val mencl = mname.parent.flatMap(_.parent).get
            mencl match {
              case mencl: m.Ctor.Primary =>
                val menclDefn = mencl.parent.get.asInstanceOf[m.Member]
                val menclName = menclDefn.name
                if (mwithinctors.contains(menclName)) {
                  val details = syntaxAndPos(mname) + " " + syntaxAndPos(mwithinctors(menclName))
                  sys.error(s"ambiguous mwithinctors $details")
                }
                mwithinctors(menclName) = mname
              case _ =>
                def findBinder(pat: m.Pat) =
                  pat.collect { case m.Pat.Var(name) => name }.head
                val menclName = mencl match {
                  case mtree: m.Member => mtree.name
                  case m.Decl.Val(_, pat :: Nil, _) => findBinder(pat)
                  case m.Decl.Var(_, pat :: Nil, _) => findBinder(pat)
                  case m.Defn.Val(_, pat :: Nil, _, _) => findBinder(pat)
                  case m.Defn.Var(_, pat :: Nil, _, _) => findBinder(pat)
                }
                if (mwithins.contains(menclName)) {
                  val details = syntaxAndPos(mname) + " " + syntaxAndPos(mwithins(menclName))
                  sys.error(s"ambiguous mwithins $details")
                }
                mwithins(menclName) = mname
            }
          }
          override def apply(mtree: m.Tree): Unit = {
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
                ()
              case mtree: m.Ctor =>
                mctordefs(mtree.pos.start) = mtree.name
              case mtree: m.Term.New =>
                mctorrefs(mtree.pos.start) = mtree.init.name
              case mtree: m.Init =>
                mctorrefs(mtree.pos.start) = mtree.name
              case mtree: m.Name =>
                indexName(mtree)
              case _ =>
                ()
            }
            super.apply(mtree)
          }
        }
        traverser(unit.toSource)
      }

      locally {
        object traverser extends g.Traverser {
          private def trySymbolDefinition(gsym: g.Symbol): Unit = {
            if (config.symbols.isNone) return
            if (gsym == null) return
            if (gsym.hasPackageFlag) return
            if (gsym.isUselessSymbolInformation) return
            val symbol = gsym.toSemantic
            if (symbol == Symbols.None) return
            if (config.symbols.isLocalOnly && !symbol.isLocal) return

            def saveSymbol(gs: g.Symbol): Unit = {
              if (gs.isUseful) {
                symbols(gs.toSemantic) = gs.toSymbolInformation(SymlinkChildren)
              }
            }

            saveSymbol(gsym)
            if (gsym.isClass && !gsym.isTrait) {
              val gprim = gsym.primaryConstructor
              saveSymbol(gprim)
              gprim.info.paramss.flatten.foreach(saveSymbol)
            }
            if (gsym.isGetter) {
              val gsetter = gsym.setterIn(gsym.owner)
              saveSymbol(gsetter)
              gsetter.info.paramss.flatten.foreach(saveSymbol)
            }
            if (gsym.isUsefulField && gsym.isMutable) {
              val getterInfo = symbols(symbol)
              val setterInfos = Synthetics.setterInfos(getterInfo, SymlinkChildren)
              setterInfos.foreach { info =>
                val msymbol = info.symbol
                symbols(msymbol) = info
              }
            }
          }
          private def tryFindMtree(gtree: g.Tree): Unit = {
            def success(mtree: m.Name, gsym0: g.Symbol): Unit = {
              // We cannot be guaranteed that all symbols have a position, see
              // https://github.com/scalameta/scalameta/issues/665
              // Instead of crashing with "unsupported file", we ignore these cases.
              if (gsym0 == null) return
              if (gsym0.isUselessOccurrence) return
              if (mtree.pos == m.Position.None) return
              if (occurrences.contains(mtree.pos)) return

              val gsym = {
                def isClassRefInCtorCall = gsym0.isConstructor && mtree.isNot[m.Name.Anonymous]
                if (gsym0 != null && isClassRefInCtorCall) gsym0.owner
                else gsym0
              }
              val symbol = gsym.toSemantic
              if (symbol == Symbols.None) return

              todo -= mtree

              if (mtree.isDefinition) {
                binders += mtree.pos
                occurrences(mtree.pos) = symbol
              } else {
                val selectionFromStructuralType = gsym.owner.isRefinementClass
                if (!selectionFromStructuralType) occurrences(mtree.pos) = symbol
              }

              def tryWithin(map: mutable.Map[m.Tree, m.Name], gsym0: g.Symbol): Unit = {
                if (map.contains(mtree)) {
                  val gsym = gsym0.getterIn(gsym0.owner).orElse(gsym0)
                  if (!gsym.hasAccessBoundary) return
                  val within1 = gsym.privateWithin
                  val within2 = within1.owner.info.member({
                    if (within1.name.isTermName) within1.name.toTypeName
                    else within1.name.toTermName
                  })
                  success(map(mtree), wrapAlternatives("<within " + symbol + ">", within1, within2))
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
              if (mtree.pos.end != end) return false
              success(mtree, gtree.symbol)
              return true
            }

            gtree match {
              case _: g.DefTree => trySymbolDefinition(gtree.symbol)
              case _ =>
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

            if (mctordefs.contains(gstart)) {
              val mname = mctordefs(gstart)
              gtree match {
                case gtree: g.Template =>
                  val gctor =
                    gtree.body.find(x => Option(x.symbol).exists(_.isPrimaryConstructor))
                  success(mname, gctor.map(_.symbol).getOrElse(g.NoSymbol))
                case gtree: g.DefDef if gtree.symbol.isConstructor =>
                  success(mname, gtree.symbol)
                case _ =>
              }
            }

            if (mctorrefs.contains(gpoint)) {
              val mname = mctorrefs(gpoint)
              gtree match {
                case g.Select(_, g.nme.CONSTRUCTOR) => success(mname, gtree.symbol)
                case _ =>
              }
            }

            // Ideally, we'd like a perfect match when gtree.pos == mtree.pos.
            // Unfortunately, this is often not the case as demonstrated by a bunch of cases above and below.
            if (tryMpos(gstart, gend)) return

            val gsym = gtree.symbol
            gtree match {
              case gtree: g.ValDef if gsym.isSelfParameter =>
                tryMstart(gstart)
              case gtree: g.MemberDef if gtree.symbol.isSynthetic || gtree.symbol.isArtifact =>
                if (!gsym.isSemanticdbLocal && !gsym.isUseless) {
                  symbols(gsym.toSemantic) = gsym.toSymbolInformation(SymlinkChildren)
                }
              case gtree: g.PackageDef =>
                // NOTE: capture PackageDef.pid instead
                ()
              case gtree: g.ModuleDef if gtree.name == g.nme.PACKAGE =>
                // NOTE: if a package object comes first in the compilation unit
                // then its positions are completely mental, so we just hack around
                tryMstart(gpoint + 7)
                tryMstart(gpoint)
              case gtree: g.ValDef =>
                val gsym = gtree.symbol
                if (!gsym.isMethod && gsym.getterIn(gsym.owner) != g.NoSymbol) {
                  // FIXME: https://github.com/scalameta/scalameta/issues/1538
                  // Skip the field definition in favor of the associated getter.
                  // This will make sure that val/var class parameters are consistently
                  // resolved to getter symbols both as definition and references.
                } else {
                  tryMstart(gstart)
                  tryMstart(gpoint)
                }
              case gtree: g.MemberDef =>
                tryMstart(gpoint)
              case gtree: g.DefTree =>
                tryMstart(gpoint)
              case gtree: g.This =>
                tryMstart(gpoint)
              case gtree: g.Super =>
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
                  if (sel.name == g.nme.WILDCARD) {
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

          private def tryFindSynthetic(gtree: g.Tree): Unit = {
            if (!config.synthetics.isOn) return

            if (!gtree.pos.isRange) return

            object ApplySelect {
              def unapply(tree: g.Tree): Option[g.Select] = Option(tree).collect {
                case g.Apply(select: g.Select, _) => select
                case select: g.Select => select
              }
            }

            def isForSynthetic(gtree: g.Tree): Boolean = {
              def isForComprehensionSyntheticName(select: g.Select): Boolean = {
                select.pos == select.qualifier.pos && (select.name == g.nme.map ||
                select.name == g.nme.withFilter ||
                select.name == g.nme.flatMap ||
                select.name == g.nme.foreach)
              }
              gtree match {
                case g.Apply(fun, List(arg: g.Function)) => isForSynthetic(fun)
                case g.TypeApply(fun, _) => isForSynthetic(fun)
                case gtree: g.Select if isForComprehensionSyntheticName(gtree) => true
                case _ => false
              }
            }

            def forMethodSelect(gtree: g.Tree): s.Tree = {
              isVisitedParent += gtree
              gtree match {
                case gtree: g.TypeApply =>
                  val typeArguments = gtree.args.map(_.tpe.toSemanticTpe)
                  val innerTree = forMethodSelect(gtree.fun)
                  s.TypeApplyTree(
                    function = innerTree,
                    typeArguments = typeArguments
                  )
                case gtree: g.Select if isForSynthetic(gtree) =>
                  val qualifier = forSyntheticOrOrig(gtree.qualifier)
                  s.SelectTree(
                    qualifier = qualifier,
                    id = Some(gtree.toSemanticId)
                  )
              }
            }

            def forMethodBody(gtree: g.Tree): s.Tree = gtree match {
              case gtree: g.Function =>
                val names = gtree.vparams.map(_.toSemanticId)
                val bodyTree = forSyntheticOrOrig(gtree.body)
                s.FunctionTree(names, bodyTree)
              case _ =>
                gtree.toSemanticOriginal
            }

            def forSyntheticOrOrig(gtree: g.Tree): s.Tree = {
              isVisitedParent += gtree
              gtree match {
                case gtree: g.ApplyToImplicitArgs =>
                  val implicitArgs = gtree.args.map(_.toSemanticTree)
                  val innerTree = forSyntheticOrOrig(gtree.fun)
                  s.ApplyTree(
                    function = innerTree,
                    arguments = implicitArgs
                  )
                case gtree: g.Apply if isForSynthetic(gtree) =>
                  val fun = forMethodSelect(gtree.fun)
                  val body = forMethodBody(gtree.args.head)
                  s.ApplyTree(
                    function = fun,
                    arguments = List(body)
                  )
                case gtree => gtree.toSemanticOriginal
              }
            }

            if (!isVisitedParent(gtree)) {
              gtree match {
                case gview: g.ApplyImplicitView =>
                  val pos = gtree.pos.toMeta
                  synthetics += s.Synthetic(
                    range = Some(pos.toRange),
                    tree = s.ApplyTree(
                      function = gview.fun.toSemanticTree,
                      arguments = List(
                        s.OriginalTree(
                          range = Some(pos.toRange)
                        ))
                    )
                  )
                  isVisited += gview.fun
                case gimpl: g.ApplyToImplicitArgs =>
                  gimpl.fun match {
                    case gview: g.ApplyImplicitView =>
                      isVisitedParent += gview
                      val range = gtree.pos.toMeta.toRange
                      synthetics += s.Synthetic(
                        range = Some(range),
                        tree = s.ApplyTree(
                          function = s.ApplyTree(
                            function = gview.fun.toSemanticTree,
                            arguments = List(
                              s.OriginalTree(
                                range = Some(range)
                              ))
                          ),
                          arguments = gimpl.args.map(_.toSemanticTree)
                        )
                      )
                    case gfun if isForSynthetic(gfun) =>
                      val range = gimpl.pos.toMeta.toRange
                      val synthTree = forSyntheticOrOrig(gimpl)
                      synthetics += s.Synthetic(
                        range = Some(range),
                        tree = synthTree
                      )
                    case gfun =>
                      synthetics += s.Synthetic(
                        range = Some(gfun.pos.toMeta.toRange),
                        tree = s.ApplyTree(
                          function = s.OriginalTree(
                            range = Some(gfun.pos.toMeta.toRange)
                          ),
                          arguments = gimpl.args.map(_.toSemanticTree)
                        )
                      )
                  }
                case g.TypeApply(fun, targs @ List(targ, _*)) =>
                  if (targ.pos.isRange) return
                  // for loops
                  val fnTree = fun match {
                    case ApplySelect(select @ g.Select(qual, nme)) if isSyntheticName(select) =>
                      isVisitedParent += select
                      isVisitedParent += fun
                      val symbol = select.symbol.toSemantic
                      s.SelectTree(
                        qualifier = s.OriginalTree(range = Some(qual.pos.toMeta.toRange)),
                        id = Some(s.IdTree(symbol = symbol))
                      )
                    case _ =>
                      s.OriginalTree(
                        range = Some(fun.pos.toMeta.toRange)
                      )
                  }
                  synthetics += s.Synthetic(
                    range = Some(fun.pos.toMeta.toRange),
                    tree = s.TypeApplyTree(
                      function = fnTree,
                      typeArguments = targs.map(_.tpe.toSemanticTpe)
                    )
                  )
                case ApplySelect(select @ g.Select(qual, nme)) if isSyntheticName(select) =>
                  isVisitedParent += select
                  val symbol = select.symbol.toSemantic
                  synthetics += s.Synthetic(
                    range = Some(qual.pos.toMeta.toRange),
                    tree = s.SelectTree(
                      qualifier = s.OriginalTree(range = Some(qual.pos.toMeta.toRange)),
                      id = Some(s.IdTree(symbol = symbol))
                    )
                  )
                case gtree if isForSynthetic(gtree) =>
                  val range = gtree.pos.toMeta.toRange
                  val synthTree = forSyntheticOrOrig(gtree)
                  synthetics += s.Synthetic(
                    range = Some(range),
                    tree = synthTree
                  )
                case _ =>
                // do nothing
              }
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
                tryFindSynthetic(gtree)
              case select: g.Select if isSyntheticName(select) =>
                tryFindMtree(select.qualifier)
                tryFindSynthetic(select)
              case _ =>
                tryFindMtree(gtree)
            }
            super.traverse(gtree)
          }
        }
        traverser.traverse(unit.body)
      }

      val input = unit.source.toInput

      val finalSymbols = symbols.values.toList

      val finalOccurrences = {
        occurrences.flatMap {
          case (pos, sym) =>
            sym.asMulti.map { flatSym =>
              val role =
                if (binders.contains(pos)) s.SymbolOccurrence.Role.DEFINITION
                else s.SymbolOccurrence.Role.REFERENCE
              s.SymbolOccurrence(Some(pos.toRange), flatSym, role)
            }
        }.toList
      }

      val diagnostics = unit.reportedDiagnostics(mstarts)

      s.TextDocument(
        schema = s.Schema.SEMANTICDB4,
        uri = unit.source.toUri,
        text = unit.source.toText,
        md5 = unit.source.toMD5,
        language = s.Language.SCALA,
        symbols = finalSymbols,
        occurrences = finalOccurrences,
        diagnostics = diagnostics,
        synthetics = synthetics
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

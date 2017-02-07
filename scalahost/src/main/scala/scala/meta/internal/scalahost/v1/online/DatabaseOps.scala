package scala.meta.internal
package scalahost
package v1
package online

import scala.collection.mutable
import scala.reflect.internal.util._
import scala.reflect.internal.Flags._
import scala.tools.nsc.Global
import scala.{meta => m}
import scala.meta.semantic.v1.{Location, Database}
import scala.compat.Platform.EOL

trait DatabaseOps { self: Mirror =>

  implicit class XtensionCompilationUnitDatabase(unit: g.CompilationUnit) {
    def toDatabase: Database = {
      unit.cache.getOrElse("database", {
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

        val symbols = mutable.Map[Location, m.Symbol]()
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
              if (margnames.contains(mstart1))
                sys.error(
                  s"ambiguous margnames ${mnames.map(syntaxAndPos)} ${margnames(mstart1).map(syntaxAndPos)}")
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
                        (margs.toList
                          .collect { case m.Term.Arg.Named(mname, _) => mname }) ++ loop(mfn)
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
                val loc = mtree.pos.toSemantic
                if (symbols.contains(loc)) return // NOTE: in the future, we may decide to preempt preexisting db entries

                val symbol = gsym.toSemantic
                if (symbol == m.Symbol.None) return

                symbols(loc) = symbol
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
                case gtree: g.MemberDef if gtree.symbol.isSynthetic =>
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
                case g.Function(params, body) if params.exists(_.name.decoded.startsWith("x$")) =>
                  traverse(body)
                case gtree: g.TypeTree if gtree.original != null =>
                  traverse(gtree.original)
                case gtree: g.TypeTreeWithDeferredRefCheck =>
                  traverse(gtree.check())
                case gtree: g.MemberDef =>
                  gtree.symbol.annotations.map(ann => traverse(ann.original))
                  tryFindMtree(gtree)
                case _ =>
                  tryFindMtree(gtree)
              }
              super.traverse(gtree)
            }
          }
          traverser.traverse(unit.body)
        }

        if (todo.nonEmpty && Configuration.strictMode) {
          val buf = new StringBuilder
          buf ++= ("Unmapped names in " + unit.source.file.file + EOL)
          todo.toList
            .sortBy(_.pos.start.offset)
            .foreach(mtree => buf ++= (syntaxAndPos(mtree) + EOL))
          sys.error(buf.toString)
        }

        Database(symbols.toMap)
      })
    }
  }

  private def syntaxAndPos(gtree: g.Tree): String = {
    if (gtree == g.EmptyTree) "\u001b[1;31mEmptyTree\u001b[0m"
    else
      s"${gtree.toString
        .substring(0, Math.min(45, gtree.toString.length))
        .replace("\n", " ")} [${gtree.pos.start}..${gtree.pos.end})"
  }

  private def syntaxAndPos(mtree: m.Tree): String = {
    s"$mtree [${mtree.pos.start.offset}..${mtree.pos.end.offset})"
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
        wrapper.setFlag(OVERLOADED)
        wrapper.setInfo(g.OverloadedType(g.NoType, normalizedAlts))
    }
  }
}

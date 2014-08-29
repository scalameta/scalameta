package scala.meta
package internal.hosts.scalacompiler
package typechecker

import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}
import org.scalameta.reflection.Metadata
import scala.reflect.internal.Mode
import scala.reflect.internal.Mode._
import scala.reflect.internal.util.Statistics
import scala.tools.nsc.typechecker.TypersStats._

trait Analyzer extends NscAnalyzer with Metadata {
  import global._
  import definitions._
  val stableCurrentRun = global.currentRun
  import stableCurrentRun.runDefinitions._

  override def newTyper(context: Context) = new ParadiseTyper(context)
  class ParadiseTyper(context: Context) extends Typer(context) {
    import infer._
    import TyperErrorGen._
    private def makeAccessible(tree: Tree, sym: Symbol, pre: Type, site: Tree): (Tree, Type) = {
      if (context.isInPackageObject(sym, pre.typeSymbol)) {
        if (pre.typeSymbol == ScalaPackageClass && sym.isTerm) {
          // short cut some aliases. It seems pattern matching needs this
          // to notice exhaustiveness and to generate good code when
          // List extractors are mixed with :: patterns. See Test5 in lists.scala.
          //
          // TODO SI-6609 Eliminate this special case once the old pattern matcher is removed.
          def dealias(sym: Symbol) =
            (atPos(tree.pos.makeTransparent) {gen.mkAttributedRef(sym)} setPos tree.pos, sym.owner.thisType)
          sym.name match {
            case nme.List => return dealias(ListModule)
            case nme.Seq  => return dealias(SeqModule)
            case nme.Nil  => return dealias(NilModule)
            case _ =>
          }
        }
        val qual = typedQualifier { atPos(tree.pos.makeTransparent) {
          tree match {
            case Ident(_) => Ident(rootMirror.getPackageObjectWithMember(pre, sym))
            case Select(qual, _) => Select(qual, nme.PACKAGEkw)
            case SelectFromTypeTree(qual, _) => Select(qual, nme.PACKAGEkw)
          }
        }}
        val tree1 = atPos(tree.pos) {
          tree match {
            case Ident(name) => Select(qual, name)
            case Select(_, name) => Select(qual, name)
            case SelectFromTypeTree(_, name) => SelectFromTypeTree(qual, name)
          }
        }
        (checkAccessible(tree1, sym, qual.tpe, qual), qual.tpe)
      } else {
        (checkAccessible(tree, sym, pre, site), pre)
      }
    }
    private def typedSelectOrSuperQualifier(qual: Tree) =
      context withinSuperInit typed(qual, PolyQualifierModes)
    override def typed1(tree: Tree, mode: Mode, pt: Type): Tree = {
      def lookupInOwner(owner: Symbol, name: Name): Symbol = if (mode.inQualMode) rootMirror.missingHook(owner, name) else NoSymbol
      def lookupInRoot(name: Name): Symbol  = lookupInOwner(rootMirror.RootClass, name)
      def lookupInEmpty(name: Name): Symbol = rootMirror.EmptyPackageClass.info member name
      def lookupInQualifier(qual: Tree, name: Name): Symbol = (
        if (name == nme.ERROR || qual.tpe.widen.isErroneous)
          NoSymbol
        else lookupInOwner(qual.tpe.typeSymbol, name) orElse {
          NotAMemberError(tree, qual, name)
          NoSymbol
        }
      )
      def typedSelect(tree: Tree, qual: Tree, name: Name): Tree = {
        val t = typedSelectInternal(tree, qual, name)
        // Checking for OverloadedTypes being handed out after overloading
        // resolution has already happened.
        if (isPastTyper) t.tpe match {
          case OverloadedType(pre, alts) =>
            if (alts forall (s => (s.owner == ObjectClass) || (s.owner == AnyClass) || isPrimitiveValueClass(s.owner))) ()
            else if (settings.debug) printCaller(
              s"""|Select received overloaded type during $phase, but typer is over.
                  |If this type reaches the backend, we are likely doomed to crash.
                  |$t has these overloads:
                  |${alts map (s => "  " + s.defStringSeenAs(pre memberType s)) mkString "\n"}
                  |""".stripMargin
            )("")
          case _ =>
        }
        // NOTE: this is a meaningful difference from the code in Typers.scala
        //-t
        if (t.metadata.contains("originalQual") || t.metadata.contains("originalName")) t
        else t.appendMetadata("originalQual" -> qual, "originalName" -> name)
      }
      def typedSelectInternal(tree: Tree, qual: Tree, name: Name): Tree = {
        def asDynamicCall = dyna.mkInvoke(context, tree, qual, name) map { t =>
          dyna.wrapErrors(t, (_.typed1(t, mode, pt)))
        }

        val sym = tree.symbol orElse member(qual, name) orElse {
          // symbol not found? --> try to convert implicitly to a type that does have the required
          // member.  Added `| PATTERNmode` to allow enrichment in patterns (so we can add e.g., an
          // xml member to StringContext, which in turn has an unapply[Seq] method)
          if (name != nme.CONSTRUCTOR && mode.inAny(EXPRmode | PATTERNmode)) {
            val qual1 = adaptToMemberWithArgs(tree, qual, name, mode, reportAmbiguous = true, saveErrors = true)
            if ((qual1 ne qual) && !qual1.isErrorTyped)
              // NOTE: this is a meaningful difference from the code in Typers.scala
              //-return typed(treeCopy.Select(tree, qual1, name), mode, pt)
              return typed(treeCopy.Select(tree, qual1, name).appendMetadata("originalQual" -> qual, "originalName" -> name), mode, pt)
          }
          NoSymbol
        }
        if (phase.erasedTypes && qual.isInstanceOf[Super] && tree.symbol != NoSymbol)
          qual setType tree.symbol.owner.tpe

        if (!reallyExists(sym)) {
          def handleMissing: Tree = {
            def errorTree = missingSelectErrorTree(tree, qual, name)
            def asTypeSelection = (
              if (context.unit.isJava && name.isTypeName) {
                // SI-3120 Java uses the same syntax, A.B, to express selection from the
                // value A and from the type A. We have to try both.
                atPos(tree.pos)(gen.convertToSelectFromType(qual, name)) match {
                  case EmptyTree => None
                  case tree1     => Some(typed1(tree1, mode, pt))
                }
              }
              else None
            )
            debuglog(s"""
              |qual=$qual:${qual.tpe}
              |symbol=${qual.tpe.termSymbol.defString}
              |scope-id=${qual.tpe.termSymbol.info.decls.hashCode}
              |members=${qual.tpe.members mkString ", "}
              |name=$name
              |found=$sym
              |owner=${context.enclClass.owner}
              """.stripMargin)

            // 1) Try converting a term selection on a java class into a type selection.
            // 2) Try expanding according to Dynamic rules.
            // 3) Try looking up the name in the qualifier.
            asTypeSelection orElse asDynamicCall getOrElse (lookupInQualifier(qual, name) match {
              case NoSymbol => setError(errorTree)
              case found    => typed1(tree setSymbol found, mode, pt)
            })
          }
          handleMissing
        }
        else {
          val tree1 = tree match {
            case Select(_, _) => treeCopy.Select(tree, qual, name)
            case SelectFromTypeTree(_, _) => treeCopy.SelectFromTypeTree(tree, qual, name)
          }
          val (result, accessibleError) = silent(_.asInstanceOf[ParadiseTyper].makeAccessible(tree1, sym, qual.tpe, qual)) match {
            case SilentTypeError(err: AccessTypeError) =>
              (tree1, Some(err))
            case SilentTypeError(err) =>
              SelectWithUnderlyingError(tree, err)
              return tree
            case SilentResultValue(treeAndPre) =>
              (stabilize(treeAndPre._1, treeAndPre._2, mode, pt), None)
          }

          result match {
            // could checkAccessible (called by makeAccessible) potentially have skipped checking a type application in qual?
            case SelectFromTypeTree(qual@TypeTree(), name) if qual.tpe.typeArgs.nonEmpty => // TODO: somehow the new qual is not checked in refchecks
              treeCopy.SelectFromTypeTree(
                result,
                (TypeTreeWithDeferredRefCheck(){ () => val tp = qual.tpe; val sym = tp.typeSymbolDirect
                  // will execute during refchecks -- TODO: make private checkTypeRef in refchecks public and call that one?
                  checkBounds(qual, tp.prefix, sym.owner, sym.typeParams, tp.typeArgs, "")
                  qual // you only get to see the wrapped tree after running this check :-p
                }) setType qual.tpe setPos qual.pos,
                name)
            case _ if accessibleError.isDefined =>
              // don't adapt constructor, SI-6074
              val qual1 = if (name == nme.CONSTRUCTOR) qual
                          else adaptToMemberWithArgs(tree, qual, name, mode, reportAmbiguous = false, saveErrors = false)
              if (!qual1.isErrorTyped && (qual1 ne qual))
                typed(Select(qual1, name) setPos tree.pos, mode, pt)
              else
                // before failing due to access, try a dynamic call.
                asDynamicCall getOrElse {
                  issue(accessibleError.get)
                  setError(tree)
                }
            case _ =>
              result
          }
        }
      }
      def tryWithFilterAndFilter(tree: Select, qual: Tree): Tree = {
        def warn(sym: Symbol) = context.deprecationWarning(tree.pos, sym, s"`withFilter' method does not yet exist on ${qual.tpe.widen}, using `filter' method instead")
        silent(_ => typedSelect(tree, qual, nme.withFilter)) orElse { _ =>
          silent(_ => typed1(Select(qual, nme.filter) setPos tree.pos, mode, pt)) match {
            case SilentResultValue(res) => warn(res.symbol) ; res
            case SilentTypeError(err)   => WithFilterError(tree, err)
          }
        }
      }
      def typedSelectOrSuperCall(tree: Select) = tree match {
        case Select(qual @ Super(_, _), nme.CONSTRUCTOR) =>
          // the qualifier type of a supercall constructor is its first parent class
          typedSelect(tree, typedSelectOrSuperQualifier(qual), nme.CONSTRUCTOR)
        case Select(qual, name) =>
          if (Statistics.canEnable) Statistics.incCounter(typedSelectCount)
          val qualTyped = checkDead(typedQualifier(qual, mode))
          val qualStableOrError = (
            if (qualTyped.isErrorTyped || !name.isTypeName || treeInfo.admitsTypeSelection(qualTyped))
              qualTyped
            else
              UnstableTreeError(qualTyped)
          )
          val tree1 = name match {
            case nme.withFilter if !settings.future => tryWithFilterAndFilter(tree, qualStableOrError)
            case _              => typedSelect(tree, qualStableOrError, name)
          }
          def sym = tree1.symbol
          if (tree.isInstanceOf[PostfixSelect])
            checkFeature(tree.pos, PostfixOpsFeature, name.decode)
          if (sym != null && sym.isOnlyRefinementMember && !sym.isMacro)
            checkFeature(tree1.pos, ReflectiveCallsFeature, sym.toString)

          qualStableOrError.symbol match {
            case s: Symbol if s.isRootPackage => treeCopy.Ident(tree1, name)
            case _                            => tree1
          }
      }
      // ========================
      // NOTE: The code above is almost completely copy/pasted from Typers.scala.
      // The changes there are mostly mechanical (indentation), but those, which are non-trivial (e.g. appending metadata to trees)
      // are denoted with //- and //+ comments that designate diffs from the original code.
      // I would gladly do away with the copy/paste, which is not plain ugly, but also imposes high maintainability tax,
      // but I can't do that, because `typedSelect` needs to remember the original qualifier, and I can't override it, because it's a local method.
      // ========================
      if (isPastTyper) super.typed1(tree, mode, pt)
      else {
        val result = tree match {
          case tree @ Select(qual, name) => typedSelectOrSuperCall(tree)
          case _ => super.typed1(tree, mode, pt)
        }
        // TODO: wat do these methods even mean, and how do they differ?
        if (result.isErroneous || result.isErrorTyped) result
        else tree match {
          case Ident(_) => result.appendMetadata("originalIdent" -> tree)
          case _ => result
        }
      }
    }
  }
}
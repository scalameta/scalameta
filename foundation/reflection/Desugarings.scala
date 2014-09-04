package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.org.scalameta.reflection.Helpers

// NOTE: this file undoes the desugarings applied by Scala's parser and typechecker to the extent possible with scala.reflect trees
// for instance, insertions of implicit argument lists are undone, because it's a simple Apply => Tree transformation
// however, we can't undo ClassDef desugarings or while-loop desugarings, because scala.reflect lacks the trees to represent those in their original form
// some of the undesugarings can be done automatically, some of them require https://github.com/scalameta/scalahost/blob/master/plugin/typechecker/Analyzer.scala
trait Desugarings extends Metadata with Helpers { self =>
  val global: Global
  import global._
  import definitions._
  val currentRun = global.currentRun
  import currentRun.runDefinitions._

  import org.scalameta.invariants.Implication
  def require[T](x: T): Unit = macro org.scalameta.invariants.Macros.require

  def undoDesugarings[Input <: Tree, Output <: Tree](tree: Input)(implicit ev: UndoDesugaringsSignature[Input, Output]): Output = {
    object transformer extends Transformer {
      // NOTE: this is the newly established desugaring protocol
      // if a transformer wants to be friendly to us, they can use this protocol to simplify our lives
      object DesugaringProtocol {
        def unapply(tree: Tree): Option[Tree] = tree.metadata.get("original").map(_.asInstanceOf[Tree])
      }

      // NOTE: partially copy/pasted from Reshape.scala in scalac
      object MacroExpansion {
        def unapply(tree: Tree): Option[Tree] = {
          def postprocess(original: Tree): Tree = {
            // TODO: remember macro expansions here, because the host will need to convert and attach them to expandee's attrs
            def mkImplicitly(tp: Type) = gen.mkNullaryCall(Predef_implicitly, List(tp)).setType(tp)
            val sym = original.symbol
            original match {
              // this hack is necessary until I fix implicit macros
              // so far tag materialization is implemented by sneaky macros hidden in scala-compiler.jar
              // hence we cannot reify references to them, because noone will be able to see them later
              // when implicit macros are fixed, these sneaky macros will move to corresponding companion objects
              // of, say, ClassTag or TypeTag
              case Apply(TypeApply(_, List(tt)), _) if sym == materializeClassTag            => mkImplicitly(appliedType(ClassTagClass, tt.tpe))
              case Apply(TypeApply(_, List(tt)), List(pre)) if sym == materializeWeakTypeTag => mkImplicitly(typeRef(pre.tpe, WeakTypeTagClass, List(tt.tpe)))
              case Apply(TypeApply(_, List(tt)), List(pre)) if sym == materializeTypeTag     => mkImplicitly(typeRef(pre.tpe, TypeTagClass, List(tt.tpe)))
              case _                                                                         => original
            }
          }
          def strip(tree: Tree): Tree = {
            // TODO: current approaches to macro expansion metadata is a bit weird
            // because both expandees and expansions are attached with the same piece of metadata
            // we need to debug recursive macro expansions to see how they behave in the current system
            duplicateAndKeepPositions(tree).removeMetadata("expandeeTree").removeAttachment[analyzer.MacroExpansionAttachment]
          }
          // NOTE: the expandeeTree metadata is attached by scala.meta macro expansion
          // the MacroExpansionAttachment attachment is attached by scala.reflect macro expansion
          (tree.metadata.get("expandeeTree").map(_.asInstanceOf[Tree]), tree.attachments.get[analyzer.MacroExpansionAttachment]) match {
            case (Some(original), _) => Some(strip(postprocess(original)))
            case (None, Some(analyzer.MacroExpansionAttachment(original, _))) => Some(strip(postprocess(original)))
            case _ => None
          }
        }
      }

      object TypeTreeWithOriginal {
        def unapply(tree: Tree): Option[Tree] = tree match {
          case tree @ TypeTree() if tree.original == null =>
            // NOTE: would be nice to disallow TypeTree(tpe) without originals, but allow TypeTree()
            // because that's what one can write syntactically
            // however we have scala.reflect macros, which can generate TypeTree(tpe) trees
            // so for the sake of compatibility we have to remain conservative
            // TODO: however, you know, let's ban synthetic TypeTrees for now and see where it leads
            require(tree.tpe == null)
            None
          case tree @ TypeTree() if tree.original != null =>
            val original = tree.original match {
              case tree: SingletonTypeTree =>
                treeCopy.SingletonTypeTree(tree, tree.metadata("originalRef").asInstanceOf[Tree])
              case tree @ CompoundTypeTree(templ) =>
                // NOTE: this attachment is only going to work past typer
                // but since we're not yet going to implement whitebox macros, that's not yet a problem
                require(templ.self == noSelfType)
                val Some(CompoundTypeTreeOriginalAttachment(parents1, stats1)) = templ.attachments.get[CompoundTypeTreeOriginalAttachment]
                val templ1 = treeCopy.Template(templ, parents1, noSelfType, stats1).setType(NoType).setSymbol(NoSymbol)
                treeCopy.CompoundTypeTree(tree, templ1)
              case tree @ TypeBoundsTree(lo, hi) =>
                // TODO: infer which of the bounds were specified explicitly by the user
                val lo1 = if (lo.tpe =:= NothingTpe) EmptyTree else lo
                val hi1 = if (hi.tpe =:= AnyTpe) EmptyTree else hi
                treeCopy.TypeBoundsTree(tree, lo1, hi1)
              case tree =>
                tree
            }
            Some(original.setType(tree.tpe))
          case in @ TypeTreeWithDeferredRefCheck() =>
            // NOTE: I guess, we can do deferred checks here as the converter isn't supposed to run in the middle of typer
            // we will have to revisit this in case we decide to support whitebox macros in Palladium
            // TODO: in the future, when we'll have moved the validating part of refchecks before the macro expansion phase,
            // there won't be any necessity to support TypeTreeWithDeferredRefCheck trees
            unapply(in.check())
          case _ =>
            None
        }
      }

      private def isInferred(tree: Tree): Boolean = tree match {
        case tt @ TypeTree() => tt.nonEmpty && tt.original == null
        case _ => false
      }

      // TODO: wasEmpty is not really working well here and checking nullness of originals is too optimistic
      // however, the former produces much uglier results, so I'm going for the latter
      object MemberDefWithInferredReturnType {
        def unapply(tree: Tree): Option[Tree] = tree match {
          case tree @ ValDef(_, _, tt @ TypeTree(), _) if isInferred(tt) => Some(copyValDef(tree)(tpt = EmptyTree))
          case tree @ DefDef(_, _, _, _, tt @ TypeTree(), _) if isInferred(tt) => Some(copyDefDef(tree)(tpt = EmptyTree))
          case _ => None
        }
      }

      object TypeApplicationWithInferredTypeArguments {
        def unapply(tree: Tree): Option[Tree] = tree match {
          case TypeApply(fn, targs) if targs.exists(isInferred) => Some(fn)
          case _ => None
        }
      }

      object PartialFunction {
        def unapply(tree: Tree): Option[Tree] = tree match {
          case Typed(Block((gcdef @ ClassDef(_, tpnme.ANON_FUN_NAME, _, _)) :: Nil, q"new ${Ident(tpnme.ANON_FUN_NAME)}()"), tpt)
          if tpt.tpe.typeSymbol == definitions.PartialFunctionClass =>
            val (m, cases) :: Nil = gcdef.impl.body.collect { case DefDef(_, nme.applyOrElse, _, _, _, m @ Match(_, cases :+ _)) => (m, cases) }
            Some(treeCopy.Match(m, EmptyTree, cases))
          case _ =>
            None
        }
      }

      // TODO: infer whether implicit arguments were provided explicitly and don't remove them if so
      object ApplicationWithInferredImplicitArguments {
        def unapply(tree: Tree): Option[Tree] = tree match {
          case Apply(fn, args) if isImplicitMethodType(fn.tpe) => Some(fn)
          case _ => None
        }
      }

      // TODO: test the situation when tree.symbol is a package object
      object RefTreeWithOriginal {
        def unapply(tree: Tree): Option[Tree] = {
          object OriginalIdent { def unapply(tree: Tree): Option[Ident] = tree.metadata.get("originalIdent").map(_.asInstanceOf[Ident]) }
          object OriginalQual { def unapply(tree: Tree): Option[Tree] = tree.metadata.get("originalQual").map(_.asInstanceOf[Tree]) }
          object OriginalName { def unapply(tree: Tree): Option[Name] = tree.metadata.get("originalName").map(_.asInstanceOf[Name]) }
          object OriginalSelect { def unapply(tree: Tree): Option[(Tree, Name)] = OriginalQual.unapply(tree).flatMap(qual => OriginalName.unapply(tree).flatMap(name => Some((qual, name)))) }
          (tree, tree) match {
            // Ident => This is a very uncommon situation, which happens when we typecheck a self reference
            // unfortunately, this self reference can't have a symbol, because self doesn't have a symbol, so we have to do some encoding
            case (This(_), OriginalIdent(orig)) => Some(orig.copyAttrs(tree).removeMetadata("originalIdent"))
            case (Select(_, _), OriginalIdent(orig)) => Some(orig.copyAttrs(tree).removeMetadata("originalIdent"))
            case (Select(_, _), OriginalSelect(origQual, origName)) => Some(treeCopy.Select(tree, origQual, origName).removeMetadata("originalQual", "originalName"))
            case (SelectFromTypeTree(_, _), OriginalSelect(origQual, origName)) => Some(treeCopy.SelectFromTypeTree(tree, origQual, origName).removeMetadata("originalQual", "originalName"))
            case _ => None
          }
        }
      }

      override def transform(tree: Tree): Tree = {
        def logFailure() = {
          def summary(x: Any) = x match { case x: Product => x.productPrefix; case null => "null"; case _ => x.getClass }
          var details = tree.toString.replace("\n", "")
          if (details.length > 60) details = details.take(60) + "..."
          println("(" + summary(tree) + ") " + details)
        }
        try {
          object Desugared {
            def unapply(tree: Tree): Option[Tree] = tree match {
              case DesugaringProtocol(original) => Some(original)
              case MacroExpansion(expandee) => Some(expandee)
              case TypeTreeWithOriginal(original) => Some(original)
              case MemberDefWithInferredReturnType(original) => Some(original)
              case TypeApplicationWithInferredTypeArguments(original) => Some(original)
              case PartialFunction(original) => Some(original)
              case ApplicationWithInferredImplicitArguments(original) => Some(original)
              case RefTreeWithOriginal(original) => Some(original)
              case _ => None
            }
          }
          tree match {
            case Desugared(original) => transform(original.appendScratchpad(tree))
            case _ => super.transform(tree)
          }
        } catch {
          case err: _root_.java.lang.AssertionError => logFailure(); throw err
          case err: _root_.org.scalameta.UnreachableError.type => logFailure(); throw err
          case ex: _root_.scala.Exception => logFailure(); throw ex
        }
      }
    }
    transformer.transform(tree).asInstanceOf[Output]
  }
}

trait UndoDesugaringsSignature[Input, Output]
object UndoDesugaringsSignature {
  // NOTE: can't be bothered with contravariant implicits, so writing a macro instead
  implicit def materialize[Input, Output]: UndoDesugaringsSignature[Input, Output] = macro impl[Input]
  def impl[Input: c.WeakTypeTag](c: Context) = {
    import c.universe._
    import c.internal._
    val input = c.weakTypeOf[Input]
    val output = input match {
      case TypeRef(pre, sym, args) => typeRef(pre, {
        val explicitMapping = Map(
          "TermTree" -> "Tree", // macro expansion
          "SymTree" -> "Tree", // macro expansion
          "NameTree" -> "Tree", // macro expansion
          "RefTree" -> "Tree", // macro expansion
          "UnApply" -> "GenericApply", // extractor-based unapplication
          "AssignOrNamedArg" -> "AssignOrNamedArg", // macros can't expand into these
          "Super" -> "Super", // macros can't expand into these
          "TypeTree" -> "Tree" // not all originals are TypTrees, some are just Trees
                               // the rest of the tree types are mapped to themselves (see below)
        )
        implicit class StringlyTyped(x1: String) {
          def toType: Type = typeRef(pre, pre.member(TypeName(x1)), Nil)
          def toSymbol: Symbol = pre.member(TypeName(x1))
        }
        val matches = explicitMapping.keys.filter(k => input <:< k.toType).toList
        val disambiguated = matches.filter(m1 => matches.forall(m2 => !(m1 != m2 && (m2.toType <:< m1.toType))))
        disambiguated match {
          case m :: Nil => explicitMapping(m).toSymbol
          case Nil => sym
          case other => c.abort(c.enclosingPosition, s"$input => ?: derivation failed, because of unexpected match list $other")
        }
      }, args)
      case tpe => tpe
    }
    q"new _root_.org.scalameta.reflection.UndoDesugaringsSignature[$input, $output] {}"
  }
}

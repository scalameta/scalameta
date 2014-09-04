package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

trait Desugarings { self =>
  val global: Global
  import global._
  import definitions._
  val currentRun = global.currentRun
  import currentRun.runDefinitions._

  def undoDesugarings[Input <: Tree, Output <: Tree](tree: Input)(implicit ev: UndoDesugaringsSignature[Input, Output]): Output = {
    // NOTE: partially copy/pasted from Reshape.scala in scalac
    // NOTE: we can't undo macro expansions in annotation arguments, because after typechecking those are stored on symbols and we can't change those
    // therefore undoDesugarings for annotation arguments is called from within a host, when it takes the contents of symbols and converts them to Palladium trees
    object transformer extends Transformer with Metadata {
      lazy val global: self.global.type = self.global
      override def transform(tree: Tree): Tree = {
        tree.metadata.get("originalWhitebox").map(_.asInstanceOf[Tree]) match {
          case Some(original) => transform(original)
          case _ =>
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
            (tree.metadata.get("expandeeTree").map(_.asInstanceOf[Tree]), tree.attachments.get[analyzer.MacroExpansionAttachment]) match {
              case (Some(original), _) => super.transform(postprocess(original))
              case (None, Some(analyzer.MacroExpansionAttachment(original, _))) => super.transform(postprocess(original))
              case _ => super.transform(tree)
            }
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

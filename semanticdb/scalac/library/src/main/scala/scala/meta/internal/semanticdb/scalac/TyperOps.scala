package scala.meta.internal.semanticdb.scalac

import scala.collection.mutable
import scala.reflect.classTag
import scala.reflect.internal.Mode
import scala.util.control.NonFatal

trait TyperOps {
  self: SemanticdbOps =>

  import g._
  import g.analyzer._

  // used to record RHS of a ValDef; will add flags to distinguish if necessary
  private[meta] final class ValDefRhs(val tree: Tree)
  private[scalac] val ValDefRhsTag = classTag[ValDefRhs]

  /** the typed copy of a tree in a Java annotation's arguments, kept because scalac discards it */
  private[meta] final class TypedAnnotArg(val tree: Tree)
  private[scalac] val TypedAnnotArgTag = classTag[TypedAnnotArg]

  /** the argument trees of the Java annotation `ann` by range, the outermost at each range */
  private final class AnnotArgs private (private val byRange: mutable.LongMap[Tree]) {
    def record(typed: Tree): Unit = {
      val pos = typed.pos
      val sym = typed.symbol
      // skip an overloaded symbol: scalac resolves it in a copy
      if (pos.isRange && ((sym eq null) || !sym.isOverloaded)) {
        val arg = byRange.getOrNull(key(pos))
        if ((arg ne null) && (arg ne typed) && (arg.tpe eq null) &&
          !arg.hasAttachment(TypedAnnotArgTag)) arg
          .updateAttachment(new TypedAnnotArg(typed))(TypedAnnotArgTag)
      }
    }
  }
  private object AnnotArgs {
    def apply(ann: Apply): AnnotArgs = {
      val byRange = new mutable.LongMap[Tree]
      val traverser = new Traverser {
        override def traverse(tree: Tree): Unit = {
          val pos = tree.pos
          if (pos.isRange) {
            val k = key(pos)
            if (!byRange.contains(k)) byRange.update(k, tree)
          }
          super.traverse(tree)
        }
      }
      traverser.traverseTrees(ann.args)
      new AnnotArgs(byRange)
    }
  }
  private val AnnotArgsTag = classTag[AnnotArgs]
  private def key(pos: Position): Long = pos.start.toLong << 32 | pos.end

  private[scalac] object TyperRecorder extends AnalyzerPlugin {
    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      try tree match {
          case t: ValDef if (null ne currentRun) && !isPastTyper => // we just need the typer phase
            val gsym = t.symbol
            if (gsym.isSynthetic && !gsym.isParameter && gsym.isLocalToBlock) gsym
              .updateAttachment(new ValDefRhs(t.rhs))
          // mark a Java annotation once the typer has typed its constructor
          case t: Select if t.name eq nme.CONSTRUCTOR =>
            typer.context.tree match {
              case ann: Apply if ann.args.nonEmpty && (null ne currentRun) && !isPastTyper =>
                val ctor = t.symbol
                if (ctor.isConstructor && isClassfileAnnotation(ctor.owner)) {
                  val pos = t.pos
                  val fpos = ann.fun.pos
                  if (pos.isRange && fpos.isRange && key(pos) == key(fpos) &&
                    !ann.hasAttachment(AnnotArgsTag)) ann
                    .updateAttachment(AnnotArgs(ann))(AnnotArgsTag)
                }
              case _ =>
            }
          case _ => typer.context.tree match {
              case ann: Apply if !ann.attachments.isEmpty && (null ne currentRun) && !isPastTyper =>
                ann.attachments.get(AnnotArgsTag) match {
                  case Some(args) => args.record(tree)
                  case _ =>
                }
              case _ =>
            }
        }
      catch { case ex if NonFatal(ex) => }
      tpe
    }
  }

}

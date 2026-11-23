package scala.meta.internal.semanticdb.scalac

import scala.annotation.tailrec
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

  /**
   * the names that the imports of a unit rename to. `byStart` maps the start of each identifier
   * with one of these names to the symbol that the typer bound it to
   */
  private[meta] final class RenameBindings(val names: Set[Name]) {
    val byStart = new mutable.LongMap[Symbol]
  }
  private[scalac] val RenameBindingsTag = classTag[RenameBindings]
  private[scalac] val NoRenames = new RenameBindings(Set.empty)
  private def scanRenames(body: Tree): RenameBindings = {
    val names = Set.newBuilder[Name]
    body.foreach {
      case t: Import => t.selectors.foreach { sel =>
          val rename = sel.rename
          if ((rename ne null) && rename != sel.name && rename != nme.WILDCARD) names += rename
        }
      case _ =>
    }
    val result = names.result()
    if (result.isEmpty) NoRenames else new RenameBindings(result)
  }

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
      if ((null eq currentRun) || isPastTyper) return tpe // we just need the typer phase

      recordBinding(typer, tree)
      try tree match {
          case t: ValDef =>
            val gsym = t.symbol
            if (gsym.isSynthetic && !gsym.isParameter && gsym.isLocalToBlock) gsym
              .updateAttachment(new ValDefRhs(t.rhs))
          // mark a Java annotation once the typer has typed its constructor
          case t: Select if t.name eq nme.CONSTRUCTOR =>
            typer.context.tree match {
              case ann: Apply if ann.args.nonEmpty =>
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
              case ann: Apply if !ann.attachments.isEmpty =>
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

    /**
     * the body of the last unit that the typer typed. The typer calls `pluginsTyped` for every
     * tree, so `updateLastBody` looks up the attachment only when the typer moves to another unit
     */
    private var lastBody: Tree = null

    /** the bindings of `lastBody`, or `NoRenames` if its unit has no rename */
    private var bindings: RenameBindings = NoRenames

    private def updateLastBody(body: Tree): Unit = if (body ne lastBody) {
      bindings = // do this first, in case it throws
        if (body eq EmptyTree) NoRenames // never attach to a shared tree
        else body.attachments.get(RenameBindingsTag) match {
          case Some(b) => b
          case _ =>
            val b =
              try scanRenames(body)
              catch { case ex if NonFatal(ex) => NoRenames }
            body.updateAttachment(b)(RenameBindingsTag)
            b
        }
      lastBody = body
    }

    /**
     * records the symbol of a written identifier in the unit's `RenameBindings`, if an import of
     * the unit renames to the identifier's name
     */
    private def recordBinding(typer: Typer, tree: Tree): Unit =
      try { // keep the compile going if `exists` throws while it loads a class file
        val unit = typer.context.unit
        updateLastBody(unit.body)
        if (bindings ne NoRenames) tree match {
          case t: RefTree =>
            val pos = t.pos
            /* skip a tree in a method that scalac adds to the companion, such as a constructor's
             * default getter: it has no range. Skip a tree of another source: its offsets are in
             * another text */
            if (t.name.isTermName && pos.isRange && hasNoWrittenQualifier(t) &&
              (pos.source eq unit.source) && bindings.names(t.name)) {
              val sym = t.symbol
              if ((sym ne null) && sym.exists && !sym.isError) bindings.byStart(pos.start) = sym
            }
          case _ =>
        }
      } catch { case ex if NonFatal(ex) => }

    /**
     * scalac built every qualifier of the tree, as it does for an imported name or for a member of
     * a package object: none has an opaque range
     */
    @tailrec
    private def hasNoWrittenQualifier(tree: Tree): Boolean = tree match {
      case t: Select => !t.qualifier.pos.isOpaqueRange && hasNoWrittenQualifier(t.qualifier)
      case _ => true
    }
  }

}

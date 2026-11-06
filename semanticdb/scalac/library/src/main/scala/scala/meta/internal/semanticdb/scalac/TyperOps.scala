package scala.meta.internal.semanticdb.scalac

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

  private[scalac] object TyperRecorder extends AnalyzerPlugin {
    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      try tree match {
          case t: ValDef if (null ne currentRun) && !isPastTyper => // we just need the typer phase
            val gsym = t.symbol
            if (gsym.isSynthetic && !gsym.isParameter && gsym.isLocalToBlock) gsym
              .updateAttachment(new ValDefRhs(t.rhs))
          case _ =>
        }
      catch { case ex if NonFatal(ex) => }
      tpe
    }
  }
}

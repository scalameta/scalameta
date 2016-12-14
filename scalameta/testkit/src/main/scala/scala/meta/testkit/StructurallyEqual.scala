package scala.meta.testkit

import scala.meta.Tree
object StructurallyEqual {
  private def clsName(a: Any) = a.getClass.getName

  case class TreeDiff(a: Any, b: Any) extends Exception {
    def lineNumber: Int =
      1 + (a match {
        case e: Tree => e.pos.start.line
        case Some(t: Tree) => t.pos.start.line
        case lst: Seq[_] =>
          lst match {
            case (head: Tree) :: tail => head.pos.start.line
            case _ => -2
          }
        case _ => -2
      })
    def mismatchClass: String =
      if (clsName(a) != clsName(b)) s"(${clsName(a)} != ${clsName(b)})"
      else s"same class ${clsName(a)}"
    override def toString: String =
      s"""|$a != $b $mismatchClass""".stripMargin
    private def compare(a: Any, b: Any): String =
      (a, b) match {
        case (t1: Tree, t2: Tree) =>
          s"""$toString
             |Syntax diff:
             |${t1.syntax}
             |${t2.syntax}
             |
             |Structure diff:
             |${t1.structure}
             |${t2.structure}
           """.stripMargin
        case (t1: Seq[_], t2: Seq[_]) =>
          t1.zip(t2).map { case (a, b) => compare(a, b) }.mkString
        case _ => toString
      }
    def detailed: String = compare(a, b)
  }

  /** Test if two trees are structurally equal.
    * @return Left(errorMessage with minimal diff) if trees are not structurally
    *         different, otherwise Right(Unit). To convert into exception with
    *         meaningful error message,
    *         val Right(_) = StructurallyEqual(a, b)
    **/
  def apply(a: Tree, b: Tree): Either[TreeDiff, Unit] = {
    def loop(x: Any, y: Any): Boolean = {
      val ok: Boolean = (x, y) match {
        case (x, y) if x == null || y == null => x == null && y == null
        case (x: Some[_], y: Some[_]) => loop(x.get, y.get)
        case (x: None.type, y: None.type) => true
        case (xs: Seq[_], ys: Seq[_]) =>
          xs.length == ys.length &&
            xs.zip(ys).forall {
              case (x, y) => loop(x, y)
            }
        case (x: Tree, y: Tree) =>
          def sameStructure =
            x.productPrefix == y.productPrefix &&
              loop(x.productIterator.toList, y.productIterator.toList)
          sameStructure
        case _ => x == y
      }
      if (!ok) throw TreeDiff(x, y)
      else true
    }
    try {
      loop(a, b)
      Right(Unit)
    } catch {
      case t: TreeDiff => Left(t)
    }
  }
}

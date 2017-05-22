package scala.meta.testkit

import scala.meta.Tree

/** Helper class to create textual diff between two objects */
case class AnyDiff(a: Any, b: Any) extends Exception {
  override def toString: String = s"""$a != $b $mismatchClass"""
  def detailed: String = compare(a, b)

  /** Best effort attempt to find a line number for scala.meta.Tree */
  def lineNumber: Int =
    1 + (a match {
      case e: Tree => e.pos.startLine
      case Some(t: Tree) => t.pos.startLine
      case lst: List[_] =>
        lst match {
          case (head: Tree) :: tail => head.pos.startLine
          case _ => -2
        }
      case _ => -2
    })
  def mismatchClass: String =
    if (clsName(a) != clsName(b)) s"(${clsName(a)} != ${clsName(b)})"
    else s"same class ${clsName(a)}"

  private def clsName(a: Any) = a.getClass.getName

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
      case (t1: List[_], t2: List[_]) =>
        t1.zip(t2).map { case (a, b) => compare(a, b) }.mkString
      case _ => toString
    }
}
